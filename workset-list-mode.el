;;; Workset-List-mode.el --- Tabular workset listing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Interactive workset listing buffer with repo headers and indented
;; worktree entries.  Repos with no worktrees show as a single line.

;;; Code:

(require 'cl-lib)
(require 'transient)

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

(defface workset-list-terminal-idle
  '((t :inherit success))
  "Face for terminal indicator when all terminals are idle."
  :group 'workset-list)

(defface workset-list-terminal-busy
  '((t :inherit warning))
  "Face for terminal indicator when some terminals are busy."
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
(declare-function vterm--at-prompt-p "vterm")

(defvar vterm--prompt-tracking-enabled-p)
(declare-function workset-worktree-list "workset-worktree")
(declare-function workset-worktree-list-full "workset-worktree")
(declare-function workset-worktree-discover-in-directory "workset-worktree")
(declare-function workset-worktree-remove "workset-worktree")
(declare-function workset-create "workset")
(declare-function workset-load "workset")
(declare-function workset-load-pr "workset")
(declare-function workset-add-repo "workset")
(declare-function workset-remove-repo "workset")

(defvar workset-repos)

;;;; Data gathering

(defun workset-list--gather-entries ()
  "Gather workset entries from all sources, deduplicated by path.
Returns an alist of (REPO-NAME . ENTRIES) sorted by repo name.
Each entry is a plist with :type, :key, :path, :repo-root,
:repo-name, :branch, and :status."
  (let ((seen (make-hash-table :test #'equal))
        (wt-by-truepath (make-hash-table :test #'equal))
        (entries nil))
    ;; 1. Active worksets
    (dolist (key (workset--active-keys))
      (let* ((ws (workset--get key))
             (path (plist-get ws :worktree-path))
             (repo-root (plist-get ws :repo-root))
             (branch (plist-get ws :branch))
             (repo-name (workset--ws-repo-name key ws))
             (alive (and path (file-directory-p path)))
             (status (if alive "active" "stale"))
             (true-path (when path (file-truename path))))
        (unless (and true-path (gethash true-path seen))
          (when true-path (puthash true-path t seen))
          (push (list :type 'active
                      :key key
                      :path (or path "")
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
    ;; 3. Worktrees from registered repos
    (dolist (repo-root workset-repos)
      (when (file-directory-p repo-root)
        (let ((repo-name (workset--repo-name repo-root)))
          (dolist (wt (workset-worktree-list-full repo-root t))
            (let* ((is-main (plist-get wt :is-main))
                   (kind (plist-get wt :kind))
                   (path (plist-get wt :path))
                   (branch (or (plist-get wt :branch) "")))
              ;; Skip main worktree
              (unless is-main
                (if (equal kind "branch")
                    ;; Branch-only entry (no worktree on disk)
                    (let ((dedup-key (concat repo-name "/" branch)))
                      (unless (gethash dedup-key seen)
                        (puthash dedup-key t seen)
                        (push (list :type 'wt-branch
                                    :key branch
                                    :path ""
                                    :repo-root repo-root
                                    :repo-name (or repo-name "")
                                    :branch branch
                                    :status "branch"
                                    :symbols (plist-get wt :symbols)
                                    :main-state (plist-get wt :main-state)
                                    :main-ahead (plist-get wt :main-ahead)
                                    :main-behind (plist-get wt :main-behind)
                                    :remote-ahead (plist-get wt :remote-ahead)
                                    :remote-behind (plist-get wt :remote-behind)
                                    :commit-short-sha (plist-get wt :commit-short-sha)
                                    :commit-message (plist-get wt :commit-message)
                                    :working-tree (plist-get wt :working-tree)
                                    :is-current (plist-get wt :is-current)
                                    :kind kind)
                              entries)))
                  ;; Worktree entry (has a path on disk)
                  (let ((truepath (when path (file-truename path))))
                    (unless (or (null truepath) (gethash truepath seen))
                      (puthash truepath t seen)
                      (puthash truepath wt wt-by-truepath)
                      (let ((key (or (and (not (string-empty-p branch)) branch)
                                     (file-name-nondirectory
                                      (directory-file-name path)))))
                        (push (list :type 'git-worktree
                                    :key key
                                    :path (or path "")
                                    :repo-root repo-root
                                    :repo-name (or repo-name "")
                                    :branch branch
                                    :status "worktree"
                                    :symbols (plist-get wt :symbols)
                                    :main-state (plist-get wt :main-state)
                                    :main-ahead (plist-get wt :main-ahead)
                                    :main-behind (plist-get wt :main-behind)
                                    :remote-ahead (plist-get wt :remote-ahead)
                                    :remote-behind (plist-get wt :remote-behind)
                                    :commit-short-sha (plist-get wt :commit-short-sha)
                                    :commit-message (plist-get wt :commit-message)
                                    :working-tree (plist-get wt :working-tree)
                                    :is-current (plist-get wt :is-current)
                                    :kind kind)
                              entries)))))))))))
    ;; Enrich active entries with wt metadata
    (dolist (entry entries)
      (when (and (eq (plist-get entry :type) 'active)
                 (not (plist-get entry :symbols)))
        (let* ((path (plist-get entry :path))
               (tp (when (and path (not (string-empty-p path)))
                     (file-truename path)))
               (wt-data (when tp (gethash tp wt-by-truepath))))
          (when wt-data
            (plist-put entry :symbols (plist-get wt-data :symbols))
            (plist-put entry :main-state (plist-get wt-data :main-state))
            (plist-put entry :main-ahead (plist-get wt-data :main-ahead))
            (plist-put entry :main-behind (plist-get wt-data :main-behind))
            (plist-put entry :remote-ahead (plist-get wt-data :remote-ahead))
            (plist-put entry :remote-behind (plist-get wt-data :remote-behind))
            (plist-put entry :commit-short-sha (plist-get wt-data :commit-short-sha))
            (plist-put entry :commit-message (plist-get wt-data :commit-message))))))
    ;; Group by repo-name
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (entry (nreverse entries))
        (let ((repo (or (plist-get entry :repo-name) "")))
          (puthash repo (append (gethash repo groups) (list entry)) groups)))
      ;; Ensure registered repos always appear as headers even with no worktrees
      (dolist (repo-root workset-repos)
        (let ((repo-name (workset--repo-name repo-root)))
          (unless (gethash repo-name groups)
            (puthash repo-name nil groups))))
      ;; Sort groups by name, return alist
      (let ((result nil))
        (maphash (lambda (k v) (push (cons k v) result)) groups)
        (sort result (lambda (a b)
                       (string-lessp (downcase (car a))
                                     (downcase (car b)))))))))

;;;; Terminal status helpers

(defun workset-list--vterm-status (buffers)
  "Classify BUFFERS into a status plist (:count N :state STATE).
STATE is `idle' if all are at prompt, `busy' if any is running a
command, or `unknown' if prompt tracking is not available."
  (let ((count (length buffers))
        (unknown 0)
        (busy 0))
    (dolist (buf buffers)
      (with-current-buffer buf
        (if (and (boundp 'vterm--prompt-tracking-enabled-p)
                 vterm--prompt-tracking-enabled-p)
            (unless (vterm--at-prompt-p)
              (cl-incf busy))
          (cl-incf unknown))))
    (list :count count
          :state (cond
                  ((> unknown 0) 'unknown)
                  ((> busy 0)    'busy)
                  (t             'idle)))))

(defun workset-list--format-terminal-indicator (status)
  "Format STATUS plist into a propertized terminal indicator string.
Returns empty string when count is 0."
  (let ((count (plist-get status :count))
        (state (plist-get status :state)))
    (if (zerop count)
        ""
      (let* ((suffix (pcase state
                       ('busy    "*")
                       ('unknown "?")
                       (_        "")))
             (face (pcase state
                     ('idle    'workset-list-terminal-idle)
                     ('busy    'workset-list-terminal-busy)
                     ('unknown 'workset-list-type)))
             (text (format " [T:%d%s]" count suffix)))
        (propertize text 'face face)))))

(defun workset-list--entry-vterm-buffers (entry)
  "Return live vterm buffers for ENTRY plist."
  (let* ((key (plist-get entry :key))
         (type (plist-get entry :type)))
    (when key
      (let (repo-name task)
        (if (eq type 'active)
            (let ((ws (workset--get key)))
              (setq repo-name (workset--ws-repo-name key ws))
              (setq task (workset--ws-task key ws)))
          ;; For discovered/git-worktree, derive from entry
          (setq repo-name (or (plist-get entry :repo-name) ""))
          (setq task (file-name-nondirectory
                      (directory-file-name (or (plist-get entry :path) key)))))
        (workset-vterm-list workset-vterm-buffer-name-format
                            repo-name task)))))

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

(defun workset-list--repo-root-from-entries (entries)
  "Get the repo-root path from ENTRIES."
  (cl-some (lambda (e) (plist-get e :repo-root)) entries))

(defun workset-list--insert-repo-header (repo-name entries)
  "Insert a repo header for REPO-NAME with ENTRIES count info."
  (let* ((repo-path (workset-list--repo-path entries))
         (repo-root (workset-list--repo-root-from-entries entries))
         (count (length entries))
         (term-indicator
          (if repo-root
              (let ((bufs (workset-vterm-list
                           workset-vterm-buffer-name-format
                           (workset--repo-name repo-root) "main")))
                (if bufs
                    (workset-list--format-terminal-indicator
                     (workset-list--vterm-status bufs))
                  ""))
            ""))
         (header (concat (propertize repo-name 'face 'workset-list-repo)
                         (when repo-path
                           (concat "  "
                                   (propertize repo-path
                                               'face 'workset-list-repo-path)))
                         (propertize (format "  (%d)" count)
                                    'face 'workset-list-type)
                         term-indicator))
         (beg (point)))
    (workset-list--insert-border "┌─" ?─ 78)
    (insert (propertize "│ " 'face 'workset-list-border) header "\n")
    (when repo-root
      (put-text-property beg (point) 'workset-repo-root repo-root))
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
    (let ((term-indicator
           (let ((bufs (workset-list--entry-vterm-buffers entry)))
             (if bufs
                 (workset-list--format-terminal-indicator
                  (workset-list--vterm-status bufs))
               ""))))
      (insert (propertize "│ " 'face 'workset-list-border)
              (propertize connector 'face 'workset-list-border)
              (propertize (workset-list--pad display-name 25) 'face name-face)
              " "
              (propertize (workset-list--pad status 12) 'face 'workset-list-type)
              (propertize (workset-list--pad branch 30) 'face 'workset-list-branch)
              (propertize path 'face 'workset-list-path)
              term-indicator
              "\n"))
    ;; Store the entry plist as a text property on the line
    (put-text-property beg (point) 'workset-entry entry)))

(defun workset-list--insert-repo-solo (repo-name entries)
  "Insert a single-line repo entry for REPO-NAME with no children."
  (let* ((repo-root (or (workset-list--repo-root-from-entries entries)
                        (cl-find-if (lambda (r)
                                      (string= (workset--repo-name r) repo-name))
                                    workset-repos)))
         (repo-path (or (workset-list--repo-path entries)
                        (when repo-root
                          (abbreviate-file-name repo-root))))
         (beg (point)))
    (let ((term-indicator
           (if repo-root
               (let ((bufs (workset-vterm-list
                            workset-vterm-buffer-name-format
                            (workset--repo-name repo-root) "main")))
                 (if bufs
                     (workset-list--format-terminal-indicator
                      (workset-list--vterm-status bufs))
                   ""))
             "")))
      (insert (propertize "─── " 'face 'workset-list-border)
              (propertize repo-name 'face 'workset-list-repo)
              (if repo-path
                  (concat "  "
                          (propertize repo-path 'face 'workset-list-repo-path))
                "")
              term-indicator
              "\n"))
    ;; Make it actionable if there's a single project entry
    (when (and entries (= (length entries) 1))
      (put-text-property beg (point) 'workset-entry (car entries)))
    ;; Always store repo-root for terminal/dired access
    (when repo-root
      (put-text-property beg (point) 'workset-repo-root repo-root))))

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
    (define-key map (kbd "c") #'workset-list-create)
    (define-key map (kbd "b") #'workset-load)
    (define-key map (kbd "P") #'workset-load-pr)
    (define-key map (kbd "RET") #'workset-list-open)
    (define-key map (kbd "t") #'workset-list-vterm)
    (define-key map (kbd "r") #'workset-list-remove)
    (define-key map (kbd "d") #'workset-list-dired)
    (define-key map (kbd "a") #'workset-add-repo)
    (define-key map (kbd "R") #'workset-remove-repo)
    (define-key map (kbd "n") #'workset-list-next-entry)
    (define-key map (kbd "p") #'workset-list-prev-entry)
    (define-key map "?" #'workset-list-dispatch)
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

(defun workset-list--actionable-line-p ()
  "Return non-nil if the current line is actionable."
  (or (get-text-property (point) 'workset-entry)
      (get-text-property (point) 'workset-repo-root)))

(defun workset-list-next-entry ()
  "Move to the next worktree entry."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (workset-list--actionable-line-p)))
      (forward-line 1))
    (when (eobp)
      (goto-char pos))))

(defun workset-list-prev-entry ()
  "Move to the previous worktree entry."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (workset-list--actionable-line-p)))
      (forward-line -1))
    (when (bobp)
      (unless (workset-list--actionable-line-p)
        (goto-char pos)))))

;;;; Helper functions

(defun workset-list--entry-at-point ()
  "Return the entry plist at point, or nil."
  (get-text-property (point) 'workset-entry))

(defun workset-list--repo-root-at-point ()
  "Return the repo-root path at point, or nil."
  (get-text-property (point) 'workset-repo-root))

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
  "Open the workset or repo at point: switch to its vterm."
  (interactive)
  (let ((entry (workset-list--entry-at-point))
        (repo-root (workset-list--repo-root-at-point)))
    (cond
     (entry
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
              (workset--put active-key ws))))))
     (repo-root
      (unless (file-directory-p repo-root)
        (user-error "Repo %s no longer exists" repo-root))
      (let* ((repo-name (workset--repo-name repo-root))
             (live-bufs (workset-vterm-list
                         workset-vterm-buffer-name-format repo-name "main")))
        (if live-bufs
            (pop-to-buffer-same-window (car live-bufs))
          (let ((buf (workset-vterm-create repo-root workset-vterm-buffer-name-format repo-name "main")))
            (pop-to-buffer-same-window buf)))))
     (t
      (user-error "No workset entry or repo at point")))))

(defun workset-list-vterm ()
  "Open a terminal for the workset or repo at point."
  (interactive)
  (let ((entry (workset-list--entry-at-point))
        (repo-root (workset-list--repo-root-at-point)))
    (cond
     (entry
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
            (workset--put active-key ws)))))
     (repo-root
      (unless (file-directory-p repo-root)
        (user-error "Repo %s no longer exists" repo-root))
      (let* ((repo-name (workset--repo-name repo-root))
             (buf (workset-vterm-create repo-root workset-vterm-buffer-name-format repo-name "main")))
        (pop-to-buffer-same-window buf)))
     (t
      (user-error "No workset entry or repo at point")))))

(defun workset-list-remove ()
  "Remove the workset at point.
For active entries, kill vterm buffers and optionally remove the worktree.
For discovered and git-worktree entries, remove the worktree from disk.
Use `workset-remove-repo' (R) to unregister a repo."
  (interactive)
  (let* ((entry (workset-list--entry-at-point))
         (type (and entry (plist-get entry :type))))
    (unless entry
      (user-error "No worktree entry at point (use R to remove a repo)"))
    (pcase type
      ('active
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
           (workset-worktree-remove repo-root (plist-get ws :branch)))
         (workset--remove key)
         (message "Removed workset %s" key)))
      ((or 'discovered 'git-worktree)
       (let ((path (plist-get entry :path))
             (branch (plist-get entry :branch))
             (repo-root (plist-get entry :repo-root)))
         (unless (yes-or-no-p (format "Remove worktree at %s? " path))
           (user-error "Aborted"))
         (workset-worktree-remove repo-root branch)
         (message "Removed worktree %s" path)))
      (_
       (user-error "Unknown entry type: %s" type)))
    (workset-list-refresh)))

(defun workset-list-dired ()
  "Open dired at the worktree path or repo root at point."
  (interactive)
  (let ((entry (workset-list--entry-at-point))
        (repo-root (workset-list--repo-root-at-point)))
    (cond
     (entry
      (let ((path (plist-get entry :path)))
        (unless path
          (user-error "No path at point"))
        (unless (file-directory-p path)
          (user-error "Worktree %s no longer exists" path))
        (dired path)))
     (repo-root
      (unless (file-directory-p repo-root)
        (user-error "Repo %s no longer exists" repo-root))
      (dired repo-root))
     (t
      (user-error "No workset entry or repo at point")))))

(defun workset-list-create ()
  "Create a workset, using the repo at point if available."
  (interactive)
  (let ((repo-root (workset-list--repo-root-at-point)))
    (workset-create repo-root)))

;;;; Transient help menu

(transient-define-prefix workset-list-dispatch ()
  "Actions for the workset listing buffer."
  ["Navigation"
   ("n" "Next entry"        workset-list-next-entry :transient t)
   ("p" "Previous entry"    workset-list-prev-entry :transient t)]
  ["Open"
   ("RET" "Open workset/repo"  workset-list-open)
   ("t"   "Open terminal"      workset-list-vterm)
   ("d"   "Dired"              workset-list-dired)]
  ["Create"
   ("c" "Create workset"    workset-list-create)
   ("b" "Load branch"       workset-load)
   ("P" "Load pull request" workset-load-pr)]
  ["Manage"
   ("r" "Remove"            workset-list-remove)
   ("a" "Add repo"          workset-add-repo)
   ("R" "Remove repo"       workset-remove-repo)]
  ["Buffer"
   ("g" "Refresh"           workset-list-refresh :transient t)
   ("q" "Quit"              quit-window)])

(provide 'workset-list-mode)
;;; workset-list-mode.el ends here
