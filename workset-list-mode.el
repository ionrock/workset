;;; workset-list-mode.el --- Tabulated-list based workset listing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Interactive workset listing buffer using tabulated-list-mode for
;; a sortable, navigable UI with action keybindings.

;;; Code:

(require 'cl-lib)

;;;; Custom faces

(defgroup workset-list nil
  "Faces for the workset listing buffer."
  :group 'workset)

(defface workset-list-heading
  '((t :inherit bold))
  "Face for group headings."
  :group 'workset-list)

(defface workset-list-key
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for workset keys."
  :group 'workset-list)

(defface workset-list-branch
  '((t :inherit font-lock-keyword-face))
  "Face for branch names."
  :group 'workset-list)

(defface workset-list-path
  '((t :inherit shadow))
  "Face for file paths."
  :group 'workset-list)

(defface workset-list-stale
  '((t :inherit warning))
  "Face for stale worktree indicators."
  :group 'workset-list)

(defface workset-list-separator
  '((t :inherit bold :underline t))
  "Face for separator/group rows."
  :group 'workset-list)

;;;; Forward declarations

(defvar workset--active-worksets)
(defvar workset-vterm-buffer-name-format)

(declare-function workset--active-keys "workset")
(declare-function workset--get "workset")
(declare-function workset--discovery-directories "workset")
(declare-function workset--git-repo-root "workset")
(declare-function workset--ws-repo-name "workset")
(declare-function workset--ws-task "workset")
(declare-function workset--repo-name "workset")
(declare-function workset--put "workset")
(declare-function workset--remove "workset")
(declare-function workset-vterm-list "workset-vterm")
(declare-function workset-vterm-create "workset-vterm")
(declare-function workset-worktree-list "workset-worktree")
(declare-function workset-worktree-discover-in-directory "workset-worktree")
(declare-function workset-worktree-remove "workset-worktree")
(declare-function workset-create "workset")
(declare-function workset-load "workset")
(declare-function workset-load-pr "workset")
(declare-function projectile-known-projects "projectile")

;;;; Data gathering

(defun workset-list--gather-entries ()
  "Gather workset entries from all sources, deduplicated by path.
Returns a list of plists sorted by `:repo-name' with separator
plists inserted between groups."
  (let ((seen (make-hash-table :test #'equal))
        (entries nil))
    ;; 1. Active worksets
    (dolist (key (workset--active-keys))
      (let* ((ws (workset--get key))
             (path (plist-get ws :worktree-path))
             (repo-root (plist-get ws :repo-root))
             (branch (plist-get ws :branch))
             (repo-name (workset--ws-repo-name key ws))
             (alive (file-directory-p path))
             (status (if alive "active" "stale")))
        (unless (gethash (file-truename path) seen)
          (puthash (file-truename path) t seen)
          (push (list :type 'active
                      :key key
                      :path path
                      :repo-root repo-root
                      :repo-name (or repo-name "")
                      :branch (or branch "")
                      :status status)
                entries))))
    ;; 2. Discovered worktrees
    (dolist (base-dir (workset--discovery-directories))
      (when (file-directory-p base-dir)
        (dolist (wt (workset-worktree-discover-in-directory base-dir))
          (let* ((path (plist-get wt :path))
                 (truepath (file-truename path)))
            (unless (gethash truepath seen)
              (puthash truepath t seen)
              (let* ((branch (plist-get wt :branch))
                     (repo-root (plist-get wt :repo-root))
                     (repo-name (if repo-root
                                    (workset--repo-name repo-root)
                                  (file-name-nondirectory
                                   (directory-file-name path))))
                     (key (file-relative-name path base-dir)))
                (push (list :type 'discovered
                            :key key
                            :path path
                            :repo-root repo-root
                            :repo-name (or repo-name "")
                            :branch (or branch "")
                            :status "disc")
                      entries)))))))
    ;; 3. Git worktrees from current repo
    (let ((repo-root (workset--git-repo-root)))
      (when repo-root
        (let ((repo-truename (file-truename repo-root))
              (repo-name (workset--repo-name repo-root)))
          (dolist (wt (workset-worktree-list repo-root))
            (let* ((path (plist-get wt :path))
                   (truepath (file-truename path)))
              ;; Skip main worktree
              (unless (or (equal truepath repo-truename)
                          (gethash truepath seen))
                (puthash truepath t seen)
                (let* ((branch-ref (or (plist-get wt :branch) ""))
                       (branch (replace-regexp-in-string
                                "\\`refs/heads/" "" branch-ref))
                       (key (or branch
                                (file-name-nondirectory
                                 (directory-file-name path)))))
                  (push (list :type 'git-worktree
                              :key key
                              :path path
                              :repo-root repo-root
                              :repo-name (or repo-name "")
                              :branch branch
                              :status "wt")
                        entries))))))))
    ;; 4. Projectile projects (when available)
    (when (featurep 'projectile)
      (dolist (proj (projectile-known-projects))
        (let* ((path (directory-file-name (expand-file-name proj)))
               (truepath (file-truename path)))
          (unless (gethash truepath seen)
            (puthash truepath t seen)
            (let ((repo-name (file-name-nondirectory path)))
              (push (list :type 'project
                          :key repo-name
                          :path path
                          :repo-root path
                          :repo-name (or repo-name "")
                          :branch ""
                          :status "proj")
                    entries))))))
    ;; Sort by repo-name (case-insensitive), then insert separators
    (setq entries (nreverse entries))
    (setq entries (sort entries
                        (lambda (a b)
                          (string-lessp
                           (downcase (or (plist-get a :repo-name) ""))
                           (downcase (or (plist-get b :repo-name) ""))))))
    ;; Insert separator plists between groups
    (let ((result nil)
          (last-group nil))
      (dolist (entry entries)
        (let ((group (downcase (or (plist-get entry :repo-name) ""))))
          (unless (equal group last-group)
            (push (list :type 'separator
                        :key nil
                        :path nil
                        :repo-root nil
                        :repo-name (plist-get entry :repo-name)
                        :branch ""
                        :status "")
                  result)
            (setq last-group group)))
        (push entry result))
      (nreverse result))))

;;;; Entry formatting

(defun workset-list--format-entry (entry)
  "Format ENTRY plist into a tabulated-list entry.
Returns (ID . [VECTOR]) where ID is the plist."
  (let ((type (plist-get entry :type)))
    (if (eq type 'separator)
        (list entry
              (vector (propertize (or (plist-get entry :repo-name) "")
                                  'face 'workset-list-separator)
                      "" "" "" ""))
      (let* ((key (or (plist-get entry :key) ""))
             (branch (or (plist-get entry :branch) ""))
             (path (or (plist-get entry :path) ""))
             (status (or (plist-get entry :status) ""))
             (type-str (symbol-name type))
             (name-face (if (string= status "stale")
                            'workset-list-stale
                          'workset-list-key)))
        (list entry
              (vector (propertize key 'face name-face)
                      type-str
                      (propertize branch 'face 'workset-list-branch)
                      status
                      (propertize (abbreviate-file-name path)
                                  'face 'workset-list-path)))))))

;;;; Mode definition

(defvar workset-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'workset-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'workset-create)
    (define-key map (kbd "b") #'workset-load)
    (define-key map (kbd "p") #'workset-load-pr)
    (define-key map (kbd "RET") #'workset-list-open)
    (define-key map (kbd "t") #'workset-list-vterm)
    (define-key map (kbd "r") #'workset-list-remove)
    (define-key map (kbd "d") #'workset-list-dired)
    map)
  "Keymap for `workset-list-mode'.")

(define-derived-mode workset-list-mode tabulated-list-mode "Workset"
  "Major mode for the workset listing buffer."
  :group 'workset
  (setq tabulated-list-format [("Name" 30 t)
                                ("Type" 12 t)
                                ("Branch" 25 t)
                                ("Status" 8 t)
                                ("Path" 0 t)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;; Refresh and buffer entry point

(defun workset-list-refresh ()
  "Rebuild the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*")))
    (with-current-buffer buffer
      (let ((entries (workset-list--gather-entries)))
        (setq tabulated-list-entries
              (mapcar #'workset-list--format-entry entries))
        (tabulated-list-print t)
        (goto-char (point-min))))))

(defun workset-list-buffer ()
  "Display the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'workset-list-mode)
        (workset-list-mode)))
    (workset-list-refresh)
    (pop-to-buffer-same-window buffer)))

;;;; Helper functions

(defun workset-list--entry-at-point ()
  "Return the plist for the entry at point, or nil if separator."
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (eq (plist-get id :type) 'separator)))
      id)))

(defun workset-list--ensure-active (path key repo-root branch)
  "Ensure the worktree at PATH is registered as an active workset.
KEY, REPO-ROOT, and BRANCH describe the worktree.  Return the key."
  (let ((existing-key (or key (file-name-nondirectory (directory-file-name path)))))
    (unless (workset--get existing-key)
      (let* ((repo-name (if repo-root
                            (file-name-nondirectory (directory-file-name repo-root))
                          existing-key))
             (task (file-name-nondirectory (directory-file-name path))))
        (workset--put existing-key
                      (list :repo-root (or repo-root path)
                            :worktree-path path
                            :branch (or branch "")
                            :repo-name repo-name
                            :task task
                            :vterm-buffers nil))))
    existing-key))

;;;; Action commands

(defun workset-list-open ()
  "Open the workset at point: switch to its vterm."
  (interactive)
  (let ((entry (workset-list--entry-at-point)))
    (unless entry
      (user-error "No workset entry at point"))
    (let* ((path (plist-get entry :path))
           (type (plist-get entry :type)))
      (unless path
        (user-error "Cannot determine worktree path"))
      (unless (file-directory-p path)
        (user-error "Worktree %s no longer exists" path))
      (let* ((key (plist-get entry :key))
             (repo-root (plist-get entry :repo-root))
             (branch (plist-get entry :branch))
             (active-key (if (eq type 'active)
                             key
                           (workset-list--ensure-active path key repo-root branch)))
             (ws (workset--get active-key))
             (repo-name (workset--ws-repo-name active-key ws))
             (task (workset--ws-task active-key ws))
             (live-bufs (workset-vterm-list
                         workset-vterm-buffer-name-format repo-name task)))
        (if live-bufs
            (pop-to-buffer-same-window (car live-bufs))
          (let ((buf (workset-vterm-create path workset-vterm-buffer-name-format repo-name task)))
            (setq ws (plist-put ws :vterm-buffers (list buf)))
            (workset--put active-key ws)))))))

(defun workset-list-vterm ()
  "Open an additional terminal for the workset at point."
  (interactive)
  (let ((entry (workset-list--entry-at-point)))
    (unless entry
      (user-error "No workset entry at point"))
    (let* ((path (plist-get entry :path))
           (type (plist-get entry :type)))
      (unless path
        (user-error "Cannot determine worktree path"))
      (unless (file-directory-p path)
        (user-error "Worktree %s no longer exists" path))
      (let* ((key (plist-get entry :key))
             (repo-root (plist-get entry :repo-root))
             (branch (plist-get entry :branch))
             (active-key (if (eq type 'active)
                             key
                           (workset-list--ensure-active path key repo-root branch)))
             (ws (workset--get active-key))
             (repo-name (workset--ws-repo-name active-key ws))
             (task (workset--ws-task active-key ws)))
        (let* ((buf (workset-vterm-create path workset-vterm-buffer-name-format repo-name task))
               (bufs (append (plist-get ws :vterm-buffers) (list buf))))
          (setq ws (plist-put ws :vterm-buffers bufs))
          (workset--put active-key ws))))))

(defun workset-list-remove ()
  "Remove the workset at point."
  (interactive)
  (let ((entry (workset-list--entry-at-point)))
    (unless entry
      (user-error "No workset entry at point"))
    (unless (eq (plist-get entry :type) 'active)
      (user-error "Can only remove active worksets"))
    (let* ((key (plist-get entry :key))
           (ws (workset--get key))
           (wt-path (plist-get ws :worktree-path))
           (repo-root (plist-get ws :repo-root))
           (repo-name (workset--ws-repo-name key ws))
           (task (workset--ws-task key ws)))
      (unless (yes-or-no-p (format "Remove workset %s? " key))
        (user-error "Aborted"))
      ;; Kill vterm buffers
      (dolist (buf (workset-vterm-list workset-vterm-buffer-name-format repo-name task))
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      ;; Optionally remove worktree
      (when (and (file-directory-p wt-path)
                 (yes-or-no-p (format "Also remove worktree at %s? " wt-path)))
        (workset-worktree-remove repo-root wt-path))
      (workset--remove key)
      (message "Removed workset %s" key)
      (workset-list-refresh))))

(defun workset-list-dired ()
  "Open dired at the worktree path of the entry at point."
  (interactive)
  (let ((entry (workset-list--entry-at-point)))
    (unless entry
      (user-error "No workset entry at point"))
    (let ((path (plist-get entry :path)))
      (unless path
        (user-error "No path at point"))
      (unless (file-directory-p path)
        (user-error "Worktree %s no longer exists" path))
      (dired path))))

(provide 'workset-list-mode)
;;; workset-list-mode.el ends here
