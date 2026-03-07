;;; workset-list-mode.el --- Magit-section based workset listing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Interactive workset listing buffer using magit-section for
;; collapsible, navigable UI with action keybindings.

;;; Code:

(require 'magit-section)
(require 'eieio)

;;;; Custom faces

(defgroup workset-list nil
  "Faces for the workset listing buffer."
  :group 'workset)

(defface workset-list-heading
  '((t :inherit magit-section-heading))
  "Face for group headings."
  :group 'workset-list)

(defface workset-list-key
  '((t :inherit magit-section-highlight :weight bold))
  "Face for workset keys."
  :group 'workset-list)

(defface workset-list-branch
  '((t :inherit magit-branch-local))
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

;;;; Section classes

(defclass workset-group-section (magit-section) ()
  "Section for group headings.")

(defclass workset-active-section (magit-section) ()
  "Section for active workset entries.")

(defclass workset-discovered-section (magit-section) ()
  "Section for discovered worktree entries.")

(defclass workset-git-worktree-section (magit-section) ()
  "Section for git worktree entries.")

;;;; Forward declarations

(defvar workset--active-worksets)
(defvar workset-vterm-buffer-name-format)

(declare-function workset--active-keys "workset")
(declare-function workset--get "workset")
(declare-function workset--ws-repo-name "workset")
(declare-function workset--ws-task "workset")
(declare-function workset--discovery-directories "workset")
(declare-function workset--git-repo-root "workset")
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

;;;; Mode definition

(defvar workset-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
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

(define-derived-mode workset-list-mode magit-section-mode "Workset"
  "Major mode for the workset listing buffer."
  :group 'workset)

;;;; Section inserter functions

(defun workset-list--insert-active-worksets ()
  "Insert Active Worksets group section."
  (let ((keys (workset--active-keys)))
    (when keys
      (magit-insert-section (workset-group-section 'active)
        (magit-insert-heading
          (propertize "Active Worksets" 'font-lock-face 'workset-list-heading))
        (dolist (key keys)
          (let* ((ws (workset--get key))
                 (wt-path (plist-get ws :worktree-path))
                 (branch (plist-get ws :branch))
                 (repo-root (plist-get ws :repo-root))
                 (repo-name (workset--ws-repo-name key ws))
                 (task (workset--ws-task key ws))
                 (alive (file-directory-p wt-path))
                 (live-bufs (workset-vterm-list
                             workset-vterm-buffer-name-format repo-name task)))
            (magit-insert-section (workset-active-section (cons key ws))
              (magit-insert-heading
                (concat (propertize key 'font-lock-face 'workset-list-key)
                        (unless alive
                          (propertize " [STALE]" 'font-lock-face 'workset-list-stale))
                        "\n"))
              (insert (format "  branch:    %s\n"
                              (propertize (or branch "") 'font-lock-face 'workset-list-branch)))
              (insert (format "  worktree:  %s\n"
                              (propertize (abbreviate-file-name wt-path) 'font-lock-face 'workset-list-path)))
              (when repo-root
                (insert (format "  repo:      %s\n"
                                (propertize (abbreviate-file-name repo-root) 'font-lock-face 'workset-list-path))))
              (insert (format "  terminals: %d\n" (length live-bufs)))
              (insert "\n"))))
        (insert "\n")))))

(defun workset-list--insert-discovered-worktrees ()
  "Insert Discovered Worktrees sections per discovery directory."
  (dolist (base-dir (workset--discovery-directories))
    (when (file-directory-p base-dir)
      (let ((worktrees (workset-worktree-discover-in-directory base-dir)))
        (when worktrees
          (magit-insert-section (workset-group-section base-dir)
            (magit-insert-heading
              (propertize (format "Discovered (%s)" (abbreviate-file-name base-dir))
                          'font-lock-face 'workset-list-heading))
            (dolist (wt worktrees)
              (let* ((wt-path (plist-get wt :path))
                     (branch (plist-get wt :branch))
                     (repo-root (plist-get wt :repo-root))
                     (rel-path (file-relative-name wt-path base-dir)))
                (magit-insert-section (workset-discovered-section
                                       (list :path wt-path
                                             :branch branch
                                             :repo-root repo-root
                                             :base-dir base-dir
                                             :key rel-path))
                  (magit-insert-heading
                    (concat (propertize rel-path 'font-lock-face 'workset-list-key)
                            "\n"))
                  (insert (format "  branch: %s\n"
                                  (propertize (or branch "(detached)")
                                              'font-lock-face 'workset-list-branch)))
                  (insert (format "  path:   %s\n"
                                  (propertize (abbreviate-file-name wt-path)
                                              'font-lock-face 'workset-list-path)))
                  (when repo-root
                    (insert (format "  repo:   %s\n"
                                    (propertize (abbreviate-file-name repo-root)
                                                'font-lock-face 'workset-list-path))))
                  (insert "\n"))))
            (insert "\n")))))))

(defun workset-list--insert-git-worktrees (repo-root)
  "Insert Git Worktrees section for REPO-ROOT."
  (when repo-root
    (let ((worktrees (workset-worktree-list repo-root))
          (repo-name (workset--repo-name repo-root)))
      (when worktrees
        (magit-insert-section (workset-group-section repo-root)
          (magit-insert-heading
            (propertize (format "Git Worktrees (%s)" repo-name)
                        'font-lock-face 'workset-list-heading))
          (dolist (wt worktrees)
            (let* ((path (plist-get wt :path))
                   (branch (or (plist-get wt :branch) "(detached)"))
                   (branch-short (replace-regexp-in-string "\\`refs/heads/" "" branch))
                   (head (plist-get wt :head))
                   (short-head (if head (substring head 0 (min 8 (length head))) "?")))
              (magit-insert-section (workset-git-worktree-section wt)
                (magit-insert-heading
                  (concat short-head "  "
                          (propertize branch-short 'font-lock-face 'workset-list-branch)
                          "  "
                          (propertize (abbreviate-file-name path) 'font-lock-face 'workset-list-path)
                          "\n")))))
          (insert "\n"))))))

;;;; Refresh and buffer entry point

(defun workset-list-refresh ()
  "Rebuild the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (magit-insert-section (magit-section)
          (workset-list--insert-active-worksets)
          (workset-list--insert-discovered-worktrees)
          (workset-list--insert-git-worktrees (workset--git-repo-root))))
      (goto-char (point-min)))))

(defun workset-list-buffer ()
  "Display the workset listing buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*workset*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'workset-list-mode)
        (workset-list-mode)))
    (workset-list-refresh)
    (pop-to-buffer-same-window buffer)))

;;;; Placeholder action commands

(defun workset-list-open ()
  "Open the workset at point."
  (interactive)
  (user-error "Not yet implemented"))

(defun workset-list-vterm ()
  "Open a terminal for the workset at point."
  (interactive)
  (user-error "Not yet implemented"))

(defun workset-list-remove ()
  "Remove the workset at point."
  (interactive)
  (user-error "Not yet implemented"))

(defun workset-list-dired ()
  "Open dired at the worktree path of the section at point."
  (interactive)
  (user-error "Not yet implemented"))

(provide 'workset-list-mode)
;;; workset-list-mode.el ends here
