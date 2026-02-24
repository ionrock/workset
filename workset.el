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
;;   M-x workset-open     - Switch to an existing workset
;;   M-x workset-vterm    - Open another terminal in a workset
;;   M-x workset-list     - List active worksets
;;   M-x workset-remove   - Remove a workset

;;; Code:

(require 'subr-x)
(require 'transient)

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

(defun workset--worktree-directory (repo-name task)
  "Return the worktree directory for REPO-NAME and TASK."
  (expand-file-name (concat "worktrees/" repo-name "/" task)
                    workset-base-directory))

(defun workset--key (repo-name task)
  "Return the workset key for REPO-NAME and TASK."
  (concat repo-name "/" task))

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

;;;; Interactive commands

(define-key workset-prefix-map (kbd "w") #'workset)
(define-key workset-prefix-map (kbd "c") #'workset-create)
(define-key workset-prefix-map (kbd "o") #'workset-open)
(define-key workset-prefix-map (kbd "t") #'workset-vterm)
(define-key workset-prefix-map (kbd "l") #'workset-list)
(define-key workset-prefix-map (kbd "r") #'workset-remove)

;;;###autoload
(defun workset-create ()
  "Create a new workset: select project, name task, create worktree, open vterm."
  (interactive)
  (let* ((repo-root (workset-project-select))
         (repo-name (workset--repo-name repo-root))
         (task (read-string (format "Task name for %s: " repo-name)))
         (key (workset--key repo-name task))
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
                          :vterm-buffers (list buf)))
      (message "Created workset %s" key))))

;;;###autoload
(defun workset-open ()
  "Switch to an existing workset's vterm, creating one if all are dead."
  (interactive)
  (let* ((keys (workset--active-keys))
         (_ (unless keys (user-error "No active worksets")))
         (key (completing-read "Open workset: " keys nil t))
         (ws (workset--get key))
         (wt-path (plist-get ws :worktree-path)))
    (unless (file-directory-p wt-path)
      (workset--remove key)
      (user-error "Worktree %s no longer exists; workset removed" wt-path))
    (let* ((parts (split-string key "/"))
           (repo-name (car parts))
           (task (mapconcat #'identity (cdr parts) "/"))
           (live-bufs (workset-vterm-list workset-vterm-buffer-name-format repo-name task)))
      (if live-bufs
          (progn
            (pop-to-buffer-same-window (car live-bufs))
            (setq ws (plist-put ws :vterm-buffers live-bufs)))
        ;; All vterms killed; create a fresh one
        (let ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task)))
          (setq ws (plist-put ws :vterm-buffers (list buf)))))
      (workset--put key ws))))

;;;###autoload
(defun workset-vterm ()
  "Open an additional numbered terminal in an existing workset."
  (interactive)
  (let* ((keys (workset--active-keys))
         (_ (unless keys (user-error "No active worksets")))
         (key (completing-read "Add terminal to workset: " keys nil t))
         (ws (workset--get key))
         (wt-path (plist-get ws :worktree-path))
         (parts (split-string key "/"))
         (repo-name (car parts))
         (task (mapconcat #'identity (cdr parts) "/")))
    (unless (file-directory-p wt-path)
      (workset--remove key)
      (user-error "Worktree %s no longer exists; workset removed" wt-path))
    (let* ((buf (workset-vterm-create wt-path workset-vterm-buffer-name-format repo-name task))
           (bufs (append (plist-get ws :vterm-buffers) (list buf))))
      (setq ws (plist-put ws :vterm-buffers bufs))
      (workset--put key ws))))

;;;###autoload
(defun workset-list ()
  "Display active worksets in a temporary buffer."
  (interactive)
  (let ((keys (workset--active-keys)))
    (if (not keys)
        (message "No active worksets")
      (with-output-to-temp-buffer "*workset-list*"
        (dolist (key keys)
          (let* ((ws (workset--get key))
                 (wt-path (plist-get ws :worktree-path))
                 (branch (plist-get ws :branch))
                 (parts (split-string key "/"))
                 (repo-name (car parts))
                 (task (mapconcat #'identity (cdr parts) "/"))
                 (live-bufs (workset-vterm-list workset-vterm-buffer-name-format repo-name task))
                 (alive (file-directory-p wt-path)))
            (princ (format "%s\n  branch:   %s\n  worktree: %s%s\n  terminals: %d\n\n"
                           key branch wt-path
                           (if alive "" " [STALE]")
                           (length live-bufs)))))))))

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
         (parts (split-string key "/"))
         (repo-name (car parts))
         (task (mapconcat #'identity (cdr parts) "/")))
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

;;;; Transient menu

;;;###autoload (autoload 'workset "workset" nil t)
(transient-define-prefix workset ()
  "Workset: coordinated worktree + terminal workspaces."
  ["Create & Open"
   ("c" "Create workset"  workset-create)
   ("o" "Open workset"    workset-open)]
  ["Manage"
   ("l" "List worksets"   workset-list)
   ("t" "Open terminal"   workset-vterm)
   ("r" "Remove workset"  workset-remove)])

(workset--install-keymap-prefix workset-keymap-prefix)

(provide 'workset)
;;; workset.el ends here
