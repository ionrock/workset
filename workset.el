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
;;   M-x workset-vterm-here - Open a terminal for the current workset directory
;;   M-x workset-switch-to-buffer - Switch to a buffer in the current workset
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

(defcustom workset-vterm-buffer-name-format "*workset: %r/%t<%n>*"
  "Format string for vterm buffer names.
%r is replaced with the repo name, %t with the task name,
and %n with the terminal number."
  :type 'string
  :group 'workset)

(defcustom workset-repos nil
  "List of git repository root paths to track in the workset listing."
  :type '(repeat directory)
  :group 'workset)

(defcustom workset-branch-prefix ""
  "Optional prefix for new branch names (e.g. \"eric/\")."
  :type 'string
  :group 'workset)

;;;; Internal state

(defvar workset--active-worksets nil
  "Alist of active worksets.
Each entry is (KEY . PLIST) where KEY is \"repo/task\" and PLIST
contains :repo-root, :worktree-path, :branch, :vterm-buffers.")

(defvar workset-prefix-map (make-sparse-keymap)
  "Keymap for Workset commands.")

(defvar workset-x-map (make-sparse-keymap)
  "Keymap for Workset project commands.")

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

(defun workset--directory-contains-p (directory file)
  "Return non-nil if FILE is inside DIRECTORY.
Both DIRECTORY and FILE may name files or directories.  Missing
paths are compared by their expanded names instead of truenames."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (target (expand-file-name file))
         (true-dir (if (file-exists-p dir) (file-truename dir) dir))
         (true-target (if (file-exists-p target) (file-truename target) target)))
    (file-in-directory-p true-target true-dir)))

(defun workset--main-entry-for-repo (repo-root)
  "Return a workset entry for the main worktree at REPO-ROOT."
  (let* ((repo-name (workset--repo-name repo-root))
         (key (workset--key repo-name "main")))
    (cons key (list :repo-root repo-root
                    :worktree-path repo-root
                    :branch "main"
                    :repo-name repo-name
                    :task "main"))))

(defun workset--merge-workset-entry (entry entries)
  "Return ENTRIES with ENTRY added unless its key is already present."
  (if (assoc (car entry) entries)
      entries
    (append entries (list entry))))

(defun workset--workset-candidates (&optional repo-root)
  "Return active and discoverable workset candidates.
When REPO-ROOT is non-nil, include its main worktree and linked
worktrees.  Also includes the main worktree for each path in
`workset-repos' and worktrees discovered under configured workset
storage directories."
  (let ((entries nil))
    (dolist (entry workset--active-worksets)
      (setq entries (workset--merge-workset-entry entry entries)))
    (when repo-root
      (setq entries (workset--merge-workset-entry
                     (workset--main-entry-for-repo repo-root) entries))
      (dolist (entry (workset--discover-worktrees repo-root))
        (setq entries (workset--merge-workset-entry entry entries))))
    (dolist (repo workset-repos)
      (when (file-directory-p repo)
        (setq entries (workset--merge-workset-entry
                       (workset--main-entry-for-repo repo) entries))))
    (dolist (entry (workset--discover-all-worktrees))
      (setq entries (workset--merge-workset-entry entry entries)))
    entries))

(defun workset--current-workset-entry ()
  "Return the workset entry containing `default-directory', or nil.
If multiple candidates contain `default-directory', prefer the one
with the longest worktree path."
  (let* ((repo-root (workset--git-repo-root))
         (candidates (workset--workset-candidates repo-root))
         (dir (expand-file-name default-directory))
         (matches (cl-remove-if-not
                   (lambda (entry)
                     (when-let ((path (plist-get (cdr entry) :worktree-path)))
                       (workset--directory-contains-p path dir)))
                   candidates)))
    (car (sort matches
               (lambda (left right)
                 (> (length (or (plist-get (cdr left) :worktree-path) ""))
                    (length (or (plist-get (cdr right) :worktree-path) ""))))))))

(defun workset--buffer-workset-directory (buffer)
  "Return BUFFER's associated directory, or nil."
  (with-current-buffer buffer
    (cond
     (buffer-file-name (file-name-directory buffer-file-name))
     ((and default-directory (file-directory-p default-directory))
      default-directory))))

(defun workset--buffers-in-directory (directory)
  "Return live buffers whose file or `default-directory' is under DIRECTORY."
  (cl-remove-if-not
   (lambda (buffer)
     (when-let ((buffer-dir (workset--buffer-workset-directory buffer)))
       (workset--directory-contains-p directory buffer-dir)))
   (buffer-list)))

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
(define-key workset-prefix-map (kbd "a") #'workset-add-repo)
(define-key workset-prefix-map (kbd "R") #'workset-remove-repo)
(define-key workset-prefix-map (kbd "x") workset-x-map)
(define-key workset-x-map (kbd "b") #'workset-switch-to-buffer)
(define-key workset-x-map (kbd "v") #'workset-vterm-here)

;;;###autoload
(defun workset-create (&optional repo-root)
  "Create a new workset: select project, name task, create worktree, open vterm.
When REPO-ROOT is non-nil, use it instead of prompting for a project."
  (interactive)
  (let* ((repo-root (or repo-root (workset-project-select)))
         (repo-name (workset--repo-name repo-root))
         (task (read-string (format "Task name for %s: " repo-name)))
         (key (workset--make-key repo-name task))
         (branch (concat workset-branch-prefix task)))
    (when (string-empty-p task)
      (user-error "Task name cannot be empty"))
    (when (workset--get key)
      (user-error "Workset %s already exists" key))
    (let ((wt-path (workset-worktree-create repo-root branch)))
      (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
        (workset--put key
                      (list :repo-root repo-root
                            :worktree-path wt-path
                            :branch branch
                            :repo-name repo-name
                            :task task
                            :vterm-buffers (list buf)))
        (message "Created workset %s" key)))))

;;;###autoload
(defun workset-open ()
  "Switch to an existing workset's vterm, creating one if all are dead.
Also discovers on-disk worktrees from both the current repo and all
configured discovery directories.  Includes the main repo directory."
  (interactive)
  (let* ((active-keys (workset--active-keys))
         (repo-root (workset--git-repo-root))
         (repo-worktrees (workset--discover-worktrees repo-root))
         (all-worktrees (workset--discover-all-worktrees))
         ;; Include main repo directory
         (main-entry (when repo-root
                       (workset--main-entry-for-repo repo-root)))
         ;; Merge both discovery sources, deduplicating by key
         (disk-worktrees (append (when main-entry (list main-entry))
                                 repo-worktrees
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
(defun workset-vterm-here ()
  "Create a new numbered vterm for the current workset directory.
The current workset is inferred from `default-directory'.  This
command does not create, switch, or otherwise manage git worktrees;
when invoked from a normal repository directory it creates a vterm
for that repository's main workset."
  (interactive)
  (let* ((entry (or (workset--current-workset-entry)
                    (user-error "Current buffer is not in a workset or git repository")))
         (key (car entry))
         (ws (cdr entry))
         (wt-path (plist-get ws :worktree-path))
         (repo-name (workset--ws-repo-name key ws))
         (task (workset--ws-task key ws)))
    (unless (file-directory-p wt-path)
      (workset--remove key)
      (user-error "Workset directory %s no longer exists; workset removed" wt-path))
    (let* ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task))
           (active-ws (or (workset--get key) ws))
           (bufs (append (plist-get active-ws :vterm-buffers) (list buf))))
      (setq active-ws (plist-put active-ws :vterm-buffers bufs))
      (workset--put key active-ws))))

;;;###autoload
(defun workset-switch-to-buffer ()
  "Switch to a buffer associated with the current workset directory.
A related buffer is any live buffer whose file or `default-directory'
is under the current workset's worktree path."
  (interactive)
  (let* ((entry (or (workset--current-workset-entry)
                    (user-error "Current buffer is not in a workset or git repository")))
         (ws (cdr entry))
         (wt-path (plist-get ws :worktree-path))
         (buffers (workset--buffers-in-directory wt-path)))
    (unless buffers
      (user-error "No buffers found for workset directory %s" wt-path))
    (let* ((current (current-buffer))
           (candidates (or (delq current (copy-sequence buffers)) buffers))
           (names (mapcar #'buffer-name candidates))
           (default (car names))
           (choice (completing-read "Switch to workset buffer: " names nil t nil nil default)))
      (switch-to-buffer choice))))

;;;###autoload
(defun workset-list ()
  "Display worksets in a tabulated list buffer."
  (interactive)
  (workset-list-buffer))

;;;###autoload
(defun workset-add-repo ()
  "Add a git repository to `workset-repos'.
Prompts for a directory (defaulting to the project root), adds it
to the list, and persists via `customize-save-variable'."
  (interactive)
  (let* ((default (workset-project-select))
         (repo-root (expand-file-name
                     (directory-file-name default))))
    (if (member repo-root workset-repos)
        (message "Repo %s is already tracked" repo-root)
      (customize-save-variable 'workset-repos
                               (append workset-repos (list repo-root)))
      (message "Added repo %s" repo-root))))

;;;###autoload
(defun workset-remove-repo ()
  "Remove a git repository from `workset-repos'."
  (interactive)
  (unless workset-repos
    (user-error "No repos registered"))
  (let ((repo (completing-read "Remove repo: " workset-repos nil t)))
    (customize-save-variable 'workset-repos
                             (delete repo workset-repos))
    (message "Removed repo %s" repo)))

(defun workset--git-repo-root ()
  "Return the git repository root for `default-directory', or nil."
  (let ((default-directory default-directory))
    (with-temp-buffer
      (when (zerop (workset--call-process "git" t
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
            (let* ((branch (or (plist-get wt :branch) ""))
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
         (branch (plist-get ws :branch))
         (repo-name (workset--ws-repo-name key ws))
         (task (workset--ws-task key ws)))
    ;; Kill vterm buffers
    (dolist (buf (workset-vterm-list workset-vterm-buffer-name-format repo-name task))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Optionally remove worktree
    (when (and (file-directory-p wt-path)
               (yes-or-no-p (format "Also remove worktree at %s? " wt-path)))
      (workset-worktree-remove repo-root branch))
    (workset--remove key)
    (message "Removed workset %s" key)))

;;;; Private helpers for loading branches

(defun workset-worktree-switch (repo-root branch)
  "Switch to an existing BRANCH worktree in REPO-ROOT using wt.
Runs `wt switch BRANCH --no-cd -y' and resolves the worktree path
via `wt list --format=json'.  Returns the worktree path string."
  (let ((default-directory repo-root))
    (let ((exit-code (call-process "wt" nil nil nil
                                   "switch" branch "--no-cd" "-y")))
      (unless (zerop exit-code)
        (error "Wt switch failed for branch %s in %s" branch repo-root)))
    (with-temp-buffer
      (let ((exit-code (workset--call-process "wt" t "list" "--format=json")))
        (unless (zerop exit-code)
          (error "Wt list failed in %s: %s" repo-root (string-trim (buffer-string))))
        (goto-char (point-min))
        (condition-case err
            (let* ((entries (json-parse-buffer :object-type 'hash-table
                                               :array-type 'list))
                   (match (cl-find-if (lambda (entry)
                                        (equal (gethash "branch" entry) branch))
                                      entries)))
              (unless match
                (error "Could not find worktree for branch %s in wt list output" branch))
              (gethash "path" match))
          (error
           (error "Failed to parse wt list JSON in %s: %s\nBuffer contents: %s"
                  repo-root (error-message-string err)
                  (string-trim (buffer-string)))))))))

(defun workset--load-branch (repo-root branch task)
  "Load BRANCH into a workset for REPO-ROOT with task name TASK.
Handles remote-tracking refs by deriving a local branch name."
  (let* ((repo-name (workset--repo-name repo-root))
         (key (workset--make-key repo-name task))
         ;; For remote-tracking refs, derive a local branch name
         (local-branch (if (string-match-p "/" branch)
                           (concat workset-branch-prefix
                                   (workset-worktree--task-from-branch branch))
                         branch)))
    (when (string-empty-p task)
      (user-error "Task name cannot be empty"))
    (when (workset--get key)
      (user-error "Workset %s already exists" key))
    (let ((wt-path (workset-worktree-create repo-root local-branch)))
      (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
        (workset--put key
                      (list :repo-root repo-root
                            :worktree-path wt-path
                            :branch local-branch
                            :repo-name repo-name
                            :task task
                            :vterm-buffers (list buf)))
        (message "Loaded workset %s" key)))))

;;;; GitHub helpers

(defun workset--gh-list-prs (repo-root)
  "List open PRs in REPO-ROOT using `gh'.
Returns an alist of (\"#N: title\" . \"N\")."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code (workset--call-process "gh" t
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
      (let ((exit-code (workset--call-process "gh" t
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

;;;; Migration helpers

(defun workset-migrate--user-config-toml ()
  "Return the TOML string for the Worktrunk user config.
Builds a worktree-path template based on `workset-create-directory'."
  (let ((worktree-path
         (if (eq workset-create-directory 'superset)
             (let* ((base (expand-file-name "worktrees"
                                            workset-superset-directory))
                    (parts (list base)))
               (unless (string-empty-p workset-default-organization)
                 (setq parts (append parts (list workset-default-organization))))
               (unless (string-empty-p workset-default-owner)
                 (setq parts (append parts (list workset-default-owner))))
               (setq parts (append parts (list "{{ branch | sanitize }}")))
               (mapconcat #'identity parts "/"))
           (concat workset-base-directory
                   "/worktrees/{{ repo }}/{{ branch | sanitize }}"))))
    (format "worktree-path = \"%s\"\n" worktree-path)))

(defun workset-migrate--project-config-toml (repo-root)
  "Return the TOML string for the Worktrunk project config in REPO-ROOT.
Reads `.superset/config.json' if it exists for setup/teardown hooks."
  (let* ((config-file (expand-file-name ".superset/config.json" repo-root))
         (setup-cmds nil)
         (teardown-cmds nil))
    ;; Parse .superset/config.json if present
    (when (file-readable-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (condition-case _
            (let ((json (json-parse-buffer :object-type 'hash-table
                                           :array-type 'list)))
              (setq setup-cmds (gethash "setup" json nil))
              (setq teardown-cmds (gethash "teardown" json nil)))
          (error nil))))
    ;; Build [post-create] section
    (let ((lines nil))
      (push "[post-create]" lines)
      (let ((i 1))
        (dolist (cmd (or setup-cmds nil))
          (push (format "setup-%d = \"%s\"" i (string-replace "\"" "\\\"" cmd)) lines)
          (setq i (1+ i))))
      (push "copy = \"wt step copy-ignored\"" lines)
      ;; Build [pre-remove] section if teardown commands exist
      (when teardown-cmds
        (push "" lines)
        (push "[pre-remove]" lines)
        (let ((i 1))
          (dolist (cmd teardown-cmds)
            (push (format "teardown-%d = \"%s\"" i (string-replace "\"" "\\\"" cmd)) lines)
            (setq i (1+ i)))))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun workset-migrate--register-worktrees (repo-root)
  "Discover and register untracked worktrees for REPO-ROOT with wt.
Returns a list of branch names that were registered."
  (let* ((known-worktrees (workset-worktree-list repo-root))
         (known-branches (delq nil (mapcar (lambda (wt) (plist-get wt :branch))
                                           known-worktrees)))
         (registered nil))
    (dolist (base-dir (workset--discovery-directories))
      (when (file-directory-p base-dir)
        (dolist (wt (workset-worktree-discover-in-directory base-dir))
          (let ((wt-repo-root (plist-get wt :repo-root))
                (branch (plist-get wt :branch)))
            (when (and branch
                       (not (string-empty-p branch))
                       wt-repo-root
                       (equal (file-truename wt-repo-root)
                              (file-truename repo-root))
                       (not (member branch known-branches)))
              ;; Register this worktree with wt
              (let ((default-directory repo-root))
                (let ((exit-code (call-process "wt" nil nil nil
                                               "switch" branch "--no-cd" "-y")))
                  (when (zerop exit-code)
                    (push branch registered)
                    ;; Add to known list to avoid double-registering
                    (push branch known-branches)))))))))
    (nreverse registered)))

;;;###autoload
(defun workset-migrate ()
  "Migrate existing workset configuration to Worktrunk (wt).
Generates wt config files and registers existing worktrees with wt.
Results are displayed in the *workset-migrate* buffer."
  (interactive)
  (let ((output-lines nil)
        (user-config-path (expand-file-name "~/.config/worktrunk/config.toml")))
    ;; --- Step 1: Generate user config ---
    (if (file-exists-p user-config-path)
        (push (format "SKIP user config (already exists): %s" user-config-path)
              output-lines)
      (let ((toml (workset-migrate--user-config-toml))
            (dir (file-name-directory user-config-path)))
        (make-directory dir t)
        (with-temp-file user-config-path
          (insert toml))
        (push (format "WROTE user config: %s" user-config-path) output-lines)
        (push (format "  %s" (string-trim toml)) output-lines)))
    ;; --- Step 2: Generate project configs and register worktrees ---
    (dolist (repo-root workset-repos)
      (push (format "\nRepo: %s" repo-root) output-lines)
      ;; Project config
      (let ((wt-config (expand-file-name ".config/wt.toml" repo-root)))
        (if (file-exists-p wt-config)
            (push (format "  SKIP project config (already exists): %s" wt-config)
                  output-lines)
          (let ((toml (workset-migrate--project-config-toml repo-root))
                (dir (file-name-directory wt-config)))
            (make-directory dir t)
            (with-temp-file wt-config
              (insert toml "\n"))
            (push (format "  WROTE project config: %s" wt-config) output-lines))))
      ;; Register worktrees
      (condition-case err
          (let ((registered (workset-migrate--register-worktrees repo-root)))
            (if registered
                (dolist (branch registered)
                  (push (format "  REGISTERED worktree: %s" branch) output-lines))
              (push "  No unregistered worktrees found" output-lines)))
        (error
         (push (format "  ERROR registering worktrees: %s" (error-message-string err))
               output-lines))))
    ;; Display results
    (let ((buf (get-buffer-create "*workset-migrate*")))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert "Workset Migration Results\n")
        (insert "=========================\n\n")
        (dolist (line (nreverse output-lines))
          (insert line "\n"))
        (read-only-mode 1)
        (goto-char (point-min)))
      (pop-to-buffer buf))))

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
   ("b" "Switch buffer"   workset-switch-to-buffer)
   ("v" "Vterm here"      workset-vterm-here)
   ("r" "Remove workset"  workset-remove)]
  ["Repos"
   ("a" "Add repo"        workset-add-repo)
   ("R" "Remove repo"     workset-remove-repo)])

(workset--install-keymap-prefix workset-keymap-prefix)

(provide 'workset)
;;; workset.el ends here
