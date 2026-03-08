;;; workset.el --- Coordinated git worktree + vterm workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vterm "0.0.2") (transient "0.4.0"))
;; Keywords: tools, processes, vc
;; URL: https://github.com/eric/workset
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; workset is a workflow tool for AI-assisted parallel development.
;; It coordinates git worktrees, vterm sessions, and agent-friendly layouts
;; so each task gets an isolated branch, filesystem, and terminal.
;;
;; Usage:
;;   M-x workset          - Open the transient menu
;;   M-x workset-create   - Create a new workset
;;   M-x workset-load     - Load an existing branch into a workset
;;   M-x workset-load-pr  - Load a GitHub PR into a workset
;;   M-x workset-open     - Switch to an existing workset
;;   M-x workset-vterm    - Open another terminal in a workset
;;   M-x workset-list     - List active worksets
;;   M-x workset-remove   - Remove a workset

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(let ((workset--dir (file-name-directory (or load-file-name buffer-file-name))))
  (when workset--dir
    (add-to-list 'load-path workset--dir)))

;;;; Customization

(defgroup workset nil
  "Coordinated git worktree + vterm workspaces."
  :group 'tools
  :prefix "workset-")

(defcustom workset-base-directory (expand-file-name "~/.workset")
  "Base directory for workset worktrees.
Worktrees are stored under BASE/worktrees/REPO/TASK."
  :type 'directory
  :group 'workset)

(defcustom workset-superset-directory (expand-file-name "~/.superset")
  "Base directory for superset worktrees.
Worktrees are stored under SUPERSET/worktrees/[ORG/][OWNER/]TASK."
  :type 'directory
  :group 'workset)

(defcustom workset-create-directory 'superset
  "Where to create new worktrees.
`superset' creates worktrees under `workset-superset-directory'.
`workset' creates worktrees under `workset-base-directory'."
  :type '(choice (const :tag "Superset directory" superset)
                 (const :tag "Workset base directory" workset))
  :group 'workset)

(defcustom workset-default-organization ""
  "Default organization name for superset-style worktree paths.
When non-empty, worktrees are placed under
SUPERSET/worktrees/ORG/[OWNER/]TASK.  Example: \"internal\"."
  :type 'string
  :group 'workset)

(defcustom workset-default-owner ""
  "Default owner name for superset-style worktree paths.
When non-empty, worktrees are placed under
SUPERSET/worktrees/[ORG/]OWNER/TASK.  Example: \"eric-larson\"."
  :type 'string
  :group 'workset)

(defcustom workset-project-backend 'auto
  "Project backend for selecting the source repository.
`auto' uses projectile if loaded, otherwise project.el."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "project.el" project)
                 (const :tag "Projectile" projectile))
  :group 'workset)

(defcustom workset-copy-patterns
  '(".env" ".envrc" ".env.local"
    "docker-compose.yml" "docker-compose.yaml"
    ".tool-versions" ".node-version" ".python-version" ".ruby-version")
  "Files or glob patterns to copy from the source repo into new worktrees."
  :type '(repeat string)
  :group 'workset)

(defcustom workset-vterm-buffer-name-format "*workset: %r/%t<%n>*"
  "Format string for vterm buffer names.
%r is replaced with the repo name, %t with the task name,
and %n with the terminal number."
  :type 'string
  :group 'workset)

(defcustom workset-branch-prefix ""
  "Optional prefix for new branch names (e.g. \"eric/\")."
  :type 'string
  :group 'workset)

(defcustom workset-start-point "HEAD"
  "Start point for new worktree branches."
  :type 'string
  :group 'workset)

;;;; Internal state

(defvar workset--active-worksets nil
  "Alist of active worksets.
Each entry is (KEY . PLIST) where KEY is \"repo/task\" and PLIST
contains :repo-root, :worktree-path, :branch, :vterm-buffers.")

(defvar workset-prefix-map (make-sparse-keymap)
  "Keymap for Workset commands.")

(defvar workset--keymap-installed nil
  "Key prefix currently used for `workset-prefix-map'.")

(defun workset--install-keymap-prefix (prefix)
  "Install Workset keymap under PREFIX."
  (let ((key (kbd prefix)))
    (when workset--keymap-installed
      (global-unset-key (kbd workset--keymap-installed)))
    (global-set-key key workset-prefix-map)
    (setq workset--keymap-installed prefix)))

(defun workset--set-keymap-prefix (symbol value)
  "Set SYMBOL to VALUE and update the Workset keymap binding."
  (set-default symbol value)
  (when (and value (stringp value) (not (string-empty-p value)))
    (workset--install-keymap-prefix value)))

(defcustom workset-keymap-prefix "C-c w"
  "Global key prefix for Workset commands."
  :type 'string
  :group 'workset
  :set #'workset--set-keymap-prefix)

;;;; Internal helpers

(defun workset--repo-name (repo-root)
  "Extract the repository name from REPO-ROOT path."
  (file-name-nondirectory (directory-file-name repo-root)))

(defun workset--discovery-directories ()
  "Return list of directories to scan for worktrees."
  (list (expand-file-name "worktrees" workset-base-directory)
        (expand-file-name "worktrees" workset-superset-directory)))

(defun workset--worktree-directory (repo-name task)
  "Return the worktree directory for REPO-NAME and TASK.
When `workset-create-directory' is `superset', the path is
SUPERSET/worktrees/[ORG/][OWNER/]TASK, omitting ORG or OWNER levels
when their defcustoms are empty strings.
When `workset-create-directory' is `workset', the path is
BASE/worktrees/REPO/TASK."
  (if (eq workset-create-directory 'superset)
      (let* ((segments (list "worktrees"))
             (segments (if (string-empty-p workset-default-organization)
                           segments
                         (append segments (list workset-default-organization))))
             (segments (if (string-empty-p workset-default-owner)
                           segments
                         (append segments (list workset-default-owner))))
             (segments (append segments (list task)))
             (rel-path (mapconcat #'identity segments "/")))
        (expand-file-name rel-path workset-superset-directory))
    (expand-file-name (concat "worktrees/" repo-name "/" task)
                      workset-base-directory)))

(defun workset--key (repo-name task)
  "Return the workset key for REPO-NAME and TASK."
  (concat repo-name "/" task))

(defun workset--make-key (repo-name task)
  "Return the workset key for REPO-NAME and TASK based on current mode.
In superset mode (`workset-create-directory' is `superset'), the key is
\[ORG/][OWNER/]TASK using `workset-default-organization' and
`workset-default-owner'.
In workset mode, the key is REPO/TASK."
  (if (eq workset-create-directory 'superset)
      (let ((parts nil))
        (unless (string-empty-p workset-default-organization)
          (push workset-default-organization parts))
        (unless (string-empty-p workset-default-owner)
          (push workset-default-owner parts))
        (push task parts)
        (mapconcat #'identity (nreverse parts) "/"))
    (workset--key repo-name task)))

(defun workset--ws-repo-name (key ws)
  "Return the repo name for workset KEY with plist WS.
Prefers the stored :repo-name, falls back to first component of KEY."
  (or (plist-get ws :repo-name)
      (car (split-string key "/"))))

(defun workset--ws-task (key ws)
  "Return the task name for workset KEY with plist WS.
Prefers the stored :task, falls back to remainder of KEY after first /."
  (or (plist-get ws :task)
      (let ((parts (split-string key "/")))
        (mapconcat #'identity (cdr parts) "/"))))

(defun workset--get (key)
  "Return the plist for workset KEY, or nil."
  (cdr (assoc key workset--active-worksets)))

(defun workset--put (key plist)
  "Store PLIST under workset KEY."
  (if-let ((cell (assoc key workset--active-worksets)))
      (setcdr cell plist)
    (push (cons key plist) workset--active-worksets)))

(defun workset--remove (key)
  "Remove workset KEY from the active list."
  (setq workset--active-worksets
        (assoc-delete-all key workset--active-worksets)))

(defun workset--active-keys ()
  "Return list of active workset keys."
  (mapcar #'car workset--active-worksets))

;;;; Sub-modules

(require 'workset-project)
(require 'workset-worktree)
(require 'workset-vterm)
(require 'workset-notify)
(require 'workset-list-mode)

;;;; Interactive commands

(define-key workset-prefix-map (kbd "w") #'workset)
(define-key workset-prefix-map (kbd "c") #'workset-create)
(define-key workset-prefix-map (kbd "o") #'workset-open)
(define-key workset-prefix-map (kbd "t") #'workset-vterm)
(define-key workset-prefix-map (kbd "l") #'workset-list)
(define-key workset-prefix-map (kbd "r") #'workset-remove)
(define-key workset-prefix-map (kbd "b") #'workset-load)
(define-key workset-prefix-map (kbd "p") #'workset-load-pr)

;;;###autoload
(defun workset-create ()
  "Create a new workset: select project, name task, create worktree, open vterm."
  (interactive)
  (let* ((repo-root (workset-project-select))
         (repo-name (workset--repo-name repo-root))
         (task (read-string (format "Task name for %s: " repo-name)))
         (key (workset--make-key repo-name task))
         (branch (concat workset-branch-prefix task))
         (wt-path (workset--worktree-directory repo-name task)))
    (when (string-empty-p task)
      (user-error "Task name cannot be empty"))
    (when (workset--get key)
      (user-error "Workset %s already exists" key))
    (if (file-directory-p wt-path)
        (unless (yes-or-no-p (format "Worktree directory %s already exists.  Use it? " wt-path))
          (user-error "Aborted"))
      (workset-worktree-create repo-root wt-path branch workset-start-point))
    (workset-worktree-copy-files repo-root wt-path workset-copy-patterns)
    (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
      (workset--put key
                    (list :repo-root repo-root
                          :worktree-path wt-path
                          :branch branch
                          :repo-name repo-name
                          :task task
                          :vterm-buffers (list buf)))
      (message "Created workset %s" key))))

;;;###autoload
(defun workset-open ()
  "Switch to an existing workset's vterm, creating one if all are dead.
Also discovers on-disk worktrees from both the current repo and all
configured discovery directories."
  (interactive)
  (let* ((active-keys (workset--active-keys))
         (repo-root (workset--git-repo-root))
         (repo-worktrees (workset--discover-worktrees repo-root))
         (all-worktrees (workset--discover-all-worktrees))
         ;; Merge both discovery sources, deduplicating by key
         (disk-worktrees (append repo-worktrees
                                 (cl-remove-if (lambda (entry)
                                                 (assoc (car entry) repo-worktrees))
                                               all-worktrees)))
         (all-keys (delete-dups (append active-keys (mapcar #'car disk-worktrees))))
         (_ (unless all-keys (user-error "No active worksets or discoverable worktrees")))
         (key (completing-read "Open workset: " all-keys nil t))
         (ws (workset--get key)))
    ;; If not already active, register from on-disk worktree
    (unless ws
      (let ((disk-entry (assoc key disk-worktrees)))
        (unless disk-entry
          (user-error "Workset %s not found" key))
        (setq ws (cdr disk-entry))
        (workset--put key ws)))
    (let ((wt-path (plist-get ws :worktree-path)))
      (unless (file-directory-p wt-path)
        (workset--remove key)
        (user-error "Worktree %s no longer exists; workset removed" wt-path))
      (let* ((repo-name (workset--ws-repo-name key ws))
             (task (workset--ws-task key ws))
             (live-bufs (workset-vterm-list workset-vterm-buffer-name-format repo-name task)))
        (if live-bufs
            (progn
              (pop-to-buffer-same-window (car live-bufs))
              (setq ws (plist-put ws :vterm-buffers live-bufs)))
          ;; All vterms killed; create a fresh one
          (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
            (setq ws (plist-put ws :vterm-buffers (list buf)))))
        (workset--put key ws)))))

;;;###autoload
(defun workset-vterm ()
  "Open an additional numbered terminal in an existing workset."
  (interactive)
  (let* ((keys (workset--active-keys))
         (_ (unless keys (user-error "No active worksets")))
         (key (completing-read "Add terminal to workset: " keys nil t))
         (ws (workset--get key))
         (wt-path (plist-get ws :worktree-path))
         (repo-name (workset--ws-repo-name key ws))
         (task (workset--ws-task key ws)))
    (unless (file-directory-p wt-path)
      (workset--remove key)
      (user-error "Worktree %s no longer exists; workset removed" wt-path))
    (let* ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task))
           (bufs (append (plist-get ws :vterm-buffers) (list buf))))
      (setq ws (plist-put ws :vterm-buffers bufs))
      (workset--put key ws))))

;;;###autoload
(defun workset-list ()
  "Display worksets in a tabulated list buffer."
  (interactive)
  (workset-list-buffer))

(defun workset--git-repo-root ()
  "Return the git repository root for `default-directory', or nil."
  (let ((default-directory default-directory))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "rev-parse" "--show-toplevel"))
        (string-trim (buffer-string))))))

(defun workset--discover-worktrees (repo-root)
  "Return an alist of (KEY . PLIST) for on-disk worktrees in REPO-ROOT.
Excludes the main worktree (REPO-ROOT itself).  Returns nil if
REPO-ROOT is nil or has no linked worktrees."
  (when repo-root
    (let ((repo-name (workset--repo-name repo-root))
          (repo-truename (file-truename repo-root))
          (result nil))
      (dolist (wt (workset-worktree-list repo-root))
        (let ((wt-path (plist-get wt :path)))
          ;; Skip the main worktree
          (unless (equal (file-truename wt-path) repo-truename)
            (let* ((branch-ref (plist-get wt :branch))
                   (branch (if branch-ref
                               (replace-regexp-in-string
                                "\\`refs/heads/" "" branch-ref)
                             ""))
                   (task (workset-worktree--task-from-branch
                          branch workset-branch-prefix))
                   (key (workset--key repo-name task)))
              (push (cons key (list :repo-root repo-root
                                    :worktree-path wt-path
                                    :branch branch
                                    :repo-name repo-name
                                    :task task))
                    result)))))
      (nreverse result))))

(defun workset--discover-all-worktrees ()
  "Discover worktrees from all configured discovery directories.
Returns an alist of (KEY . PLIST) where KEY is derived from the
worktree's relative path under its base directory."
  (let ((result nil))
    (dolist (base-dir (workset--discovery-directories))
      (when (file-directory-p base-dir)
        (dolist (wt (workset-worktree-discover-in-directory base-dir))
          (let* ((wt-path (plist-get wt :path))
                 (branch (plist-get wt :branch))
                 (repo-root (plist-get wt :repo-root))
                 ;; Key is the relative path under the base directory
                 (key (file-relative-name wt-path base-dir))
                 ;; Use the last path component as the task name
                 (task (file-name-nondirectory (directory-file-name wt-path)))
                 ;; Use the repo directory name as repo-name
                 (repo-name (when repo-root
                              (file-name-nondirectory
                               (directory-file-name repo-root)))))
            (unless (assoc key result)  ;; dedup
              (push (cons key (list :repo-root repo-root
                                    :worktree-path wt-path
                                    :branch (or branch "")
                                    :repo-name (or repo-name key)
                                    :task task))
                    result))))))
    (nreverse result)))

;;;###autoload
(defun workset-remove ()
  "Remove a workset: kill vterms, optionally remove worktree."
  (interactive)
  (let* ((keys (workset--active-keys))
         (_ (unless keys (user-error "No active worksets")))
         (key (completing-read "Remove workset: " keys nil t))
         (ws (workset--get key))
         (wt-path (plist-get ws :worktree-path))
         (repo-root (plist-get ws :repo-root))
         (repo-name (workset--ws-repo-name key ws))
         (task (workset--ws-task key ws)))
    ;; Kill vterm buffers
    (dolist (buf (workset-vterm-list workset-vterm-buffer-name-format repo-name task))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Optionally remove worktree
    (when (and (file-directory-p wt-path)
               (yes-or-no-p (format "Also remove worktree at %s? " wt-path)))
      (workset-worktree-remove repo-root wt-path))
    (workset--remove key)
    (message "Removed workset %s" key)))

;;;; Private helpers for loading branches

(defun workset--load-branch (repo-root branch task)
  "Load BRANCH into a workset for REPO-ROOT with task name TASK.
Handles remote-tracking refs by creating a local tracking branch."
  (let* ((repo-name (workset--repo-name repo-root))
         (key (workset--make-key repo-name task))
         (wt-path (workset--worktree-directory repo-name task)))
    (when (string-empty-p task)
      (user-error "Task name cannot be empty"))
    (when (workset--get key)
      (user-error "Workset %s already exists" key))
    (if (file-directory-p wt-path)
        (unless (yes-or-no-p (format "Worktree directory %s already exists.  Use it? " wt-path))
          (user-error "Aborted"))
      (let ((default-directory repo-root))
        (make-directory (file-name-directory wt-path) t)
        (if (string-match-p "/" branch)
            ;; Remote-tracking branch: create local tracking branch
            (let* ((local-name (workset-worktree--task-from-branch branch))
                   (local-branch (concat workset-branch-prefix local-name))
                   (exit-code (call-process "git" nil nil nil
                                            "worktree" "add" "--track"
                                            "-b" local-branch wt-path branch)))
              (unless (zerop exit-code)
                ;; Branch may already exist locally; try without -b
                (let ((exit-code2 (call-process "git" nil nil nil
                                                "worktree" "add" wt-path local-branch)))
                  (unless (zerop exit-code2)
                    (error "Failed to create worktree at %s for branch %s" wt-path branch)))))
          ;; Local branch
          (let ((exit-code (call-process "git" nil nil nil
                                         "worktree" "add" wt-path branch)))
            (unless (zerop exit-code)
              (error "Failed to create worktree at %s for branch %s" wt-path branch))))))
    (workset-worktree-copy-files repo-root wt-path workset-copy-patterns)
    (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
      (workset--put key
                    (list :repo-root repo-root
                          :worktree-path wt-path
                          :branch branch
                          :repo-name repo-name
                          :task task
                          :vterm-buffers (list buf)))
      (message "Loaded workset %s" key))))

;;;; GitHub helpers

(defun workset--gh-list-prs (repo-root)
  "List open PRs in REPO-ROOT using `gh'.
Returns an alist of (\"#N: title\" . \"N\")."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code (call-process "gh" nil t nil
                                     "pr" "list" "--state" "open"
                                     "--json" "number,title"
                                     "--jq" ".[] | \"\\(.number)\\t\\(.title)\"")))
        (unless (zerop exit-code)
          (error "Failed to list PRs (is `gh' installed and authenticated?)"))
        (let ((result nil))
          (dolist (line (split-string (buffer-string) "\n" t))
            (when (string-match "\\`\\([0-9]+\\)\t\\(.*\\)" line)
              (let ((num (match-string 1 line))
                    (title (match-string 2 line)))
                (push (cons (format "#%s: %s" num title) num) result))))
          (nreverse result))))))

(defun workset--gh-pr-branch (repo-root pr-number)
  "Get the head branch name for PR-NUMBER in REPO-ROOT."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code (call-process "gh" nil t nil
                                     "pr" "view" pr-number
                                     "--json" "headRefName"
                                     "--jq" ".headRefName")))
        (unless (zerop exit-code)
          (error "Failed to get branch for PR #%s" pr-number))
        (string-trim (buffer-string))))))

(defun workset--git-fetch-branch (repo-root branch)
  "Fetch BRANCH from origin in REPO-ROOT.  Non-fatal on failure."
  (let ((default-directory repo-root))
    (call-process "git" nil nil nil "fetch" "origin" branch)))

;;;; Load commands

;;;###autoload
(defun workset-load ()
  "Load an existing branch into a new workset."
  (interactive)
  (let* ((repo-root (workset-project-select))
         (branches (workset-worktree-list-branches repo-root))
         (branch (completing-read "Branch: " branches nil t))
         (task (workset-worktree--task-from-branch branch workset-branch-prefix)))
    (workset--load-branch repo-root branch task)))

;;;###autoload
(defun workset-load-pr ()
  "Load a GitHub pull request into a new workset."
  (interactive)
  (let* ((repo-root (workset-project-select))
         (prs (workset--gh-list-prs repo-root))
         (_ (unless prs (user-error "No open pull requests found")))
         (choice (completing-read "Pull request: " prs nil t))
         (pr-number (cdr (assoc choice prs)))
         (branch (workset--gh-pr-branch repo-root pr-number)))
    (workset--git-fetch-branch repo-root branch)
    (let ((task (workset-worktree--task-from-branch branch workset-branch-prefix)))
      (workset--load-branch repo-root (concat "origin/" branch) task))))

;;;; Transient menu

;;;###autoload (autoload 'workset "workset" nil t)
(transient-define-prefix workset ()
  "Workset: coordinated worktree + terminal workspaces."
  ["Create & Open"
   ("c" "Create workset"     workset-create)
   ("o" "Open workset"       workset-open)
   ("b" "Load branch"        workset-load)
   ("p" "Load pull request"  workset-load-pr)]
  ["Manage"
   ("l" "List worksets"   workset-list)
   ("t" "Open terminal"   workset-vterm)
   ("r" "Remove workset"  workset-remove)])

(workset--install-keymap-prefix workset-keymap-prefix)

(provide 'workset)
;;; workset.el ends here
