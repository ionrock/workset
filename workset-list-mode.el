;;; workset-list-mode.el --- Tabular workset listing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Interactive workset listing buffer with repo headers and indented
;; worktree entries.  Repos with no worktrees show as a single line.

;;; Code:

(require 'cl-lib)

;;;; Custom faces

(defgroup workset-list nil
  "Faces for the workset listing buffer."
  :group 'workset)

(defface workset-list-repo
  '((t :inherit bold))
  "Face for repo name headers."
  :group 'workset-list)

(defface workset-list-repo-path
  '((t :inherit shadow))
  "Face for repo path in header line."
  :group 'workset-list)

(defface workset-list-border
  '((t :inherit shadow))
  "Face for box-drawing border characters."
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

(defface workset-list-type
  '((t :inherit font-lock-comment-face))
  "Face for entry type labels."
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
Returns an alist of (REPO-NAME . ENTRIES) sorted by repo name.
Each entry is a plist with :type, :key, :path, :repo-root,
:repo-name, :branch, and :status."
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
                            :status "discovered")
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
                              :status "worktree")
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
                          :status "project")
                    entries))))))
    ;; Group by repo-name
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (entry (nreverse entries))
        (let ((repo (or (plist-get entry :repo-name) "")))
          (puthash repo (append (gethash repo groups) (list entry)) groups)))
      ;; Sort groups by name, return alist
      (let ((result nil))
        (maphash (lambda (k v) (push (cons k v) result)) groups)
        (sort result (lambda (a b)
                       (string-lessp (downcase (car a))
                                     (downcase (car b)))))))))

;;;; Buffer rendering

(defun workset-list--insert-border (prefix char width)
  "Insert a border line: PREFIX + WIDTH copies of CHAR."
  (insert (propertize (concat prefix (make-string width char))
                      'face 'workset-list-border))
  (insert "\n"))

(defun workset-list--repo-path (entries)
  "Get the repo-root path from ENTRIES to display in repo header."
  (let ((repo-root (cl-some (lambda (e) (plist-get e :repo-root)) entries)))
    (when repo-root
      (abbreviate-file-name repo-root))))

(defun workset-list--insert-repo-header (repo-name entries)
  "Insert a repo header for REPO-NAME with ENTRIES count info."
  (let* ((repo-path (workset-list--repo-path entries))
         (count (length entries))
         (header (concat (propertize repo-name 'face 'workset-list-repo)
                         (when repo-path
                           (concat "  "
                                   (propertize repo-path
                                               'face 'workset-list-repo-path)))
                         (propertize (format "  (%d)" count)
                                    'face 'workset-list-type))))
    (workset-list--insert-border "┌─" ?─ 78)
    (insert (propertize "│ " 'face 'workset-list-border) header "\n")
    (when entries
      (workset-list--insert-border "├─" ?─ 78))))

(defun workset-list--insert-entry (entry last-p)
  "Insert a single worktree ENTRY line.  LAST-P non-nil for the last entry."
  (let* ((key (or (plist-get entry :key) ""))
         (branch (or (plist-get entry :branch) ""))
         (path (abbreviate-file-name (or (plist-get entry :path) "")))
         (status (or (plist-get entry :status) ""))
         (connector (if last-p "└── " "├── "))
         (stale-p (string= status "stale"))
         (name-face (if stale-p 'workset-list-stale 'workset-list-key))
         ;; Derive a short display name from the key
         (display-name (file-name-nondirectory (directory-file-name key)))
         (beg (point)))
    (insert (propertize "│ " 'face 'workset-list-border)
            (propertize connector 'face 'workset-list-border)
            (propertize (workset-list--pad display-name 25) 'face name-face)
            " "
            (propertize (workset-list--pad status 12) 'face 'workset-list-type)
            (propertize (workset-list--pad branch 30) 'face 'workset-list-branch)
            (propertize path 'face 'workset-list-path)
            "\n")
    ;; Store the entry plist as a text property on the line
    (put-text-property beg (point) 'workset-entry entry)))

(defun workset-list--insert-repo-solo (repo-name entries)
  "Insert a single-line repo entry for REPO-NAME with no children."
  (let* ((repo-path (workset-list--repo-path entries))
         (beg (point)))
    (insert (propertize "─── " 'face 'workset-list-border)
            (propertize repo-name 'face 'workset-list-repo)
            (when repo-path
              (concat "  "
                      (propertize repo-path 'face 'workset-list-repo-path)))
            "\n")
    ;; Make it actionable if there's a single project entry
    (when (and entries (= (length entries) 1))
      (put-text-property beg (point) 'workset-entry (car entries)))))

(defun workset-list--insert-group (repo-name entries)
  "Insert a group for REPO-NAME with its ENTRIES."
  (if (null entries)
      (workset-list--insert-repo-solo repo-name nil)
    (workset-list--insert-repo-header repo-name entries)
    (let ((remaining entries))
      (while remaining
        (workset-list--insert-entry (car remaining) (null (cdr remaining)))
        (setq remaining (cdr remaining))))
    (workset-list--insert-border "└─" ?─ 78)
    (insert "\n")))

(defun workset-list--pad (str width)
  "Pad or truncate STR to WIDTH characters."
  (if (>= (length str) width)
      (substring str 0 width)
    (concat str (make-string (- width (length str)) ?\s))))

;;;; Mode definition

(defvar workset-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'workset-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "c") #'workset-create)
    (define-key map (kbd "b") #'workset-load)
    (define-key map (kbd "p") #'workset-load-pr)
    (define-key map (kbd "RET") #'workset-list-open)
    (define-key map (kbd "t") #'workset-list-vterm)
    (define-key map (kbd "r") #'workset-list-remove)
    (define-key map (kbd "d") #'workset-list-dired)
    (define-key map (kbd "n") #'workset-list-next-entry)
    (define-key map (kbd "p") #'workset-list-prev-entry)
    map)
  "Keymap for `workset-list-mode'.")

(define-derived-mode workset-list-mode special-mode "Workset"
  "Major mode for the workset listing buffer."
  :group 'workset
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;;; Refresh and buffer entry point

(defun workset-list-refresh ()
  "Rebuild the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*"))
        (pos (point)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((groups (workset-list--gather-entries)))
          (dolist (group groups)
            (workset-list--insert-group (car group) (cdr group)))))
      (goto-char (min pos (point-max)))
      ;; Move to the first actionable entry
      (when (= pos 1)
        (workset-list-next-entry)))))

(defun workset-list-buffer ()
  "Display the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'workset-list-mode)
        (workset-list-mode)))
    (workset-list-refresh)
    (pop-to-buffer-same-window buffer)))

;;;; Navigation

(defun workset-list-next-entry ()
  "Move to the next worktree entry."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'workset-entry)))
      (forward-line 1))
    (when (eobp)
      (goto-char pos))))

(defun workset-list-prev-entry ()
  "Move to the previous worktree entry."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'workset-entry)))
      (forward-line -1))
    (when (bobp)
      (unless (get-text-property (point) 'workset-entry)
        (goto-char pos)))))

;;;; Helper functions

(defun workset-list--entry-at-point ()
  "Return the entry plist at point, or nil."
  (get-text-property (point) 'workset-entry))

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
