;;; workset-worktree.el --- Git worktree operations for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Git worktree create/remove/list operations using call-process directly.
;; No magit dependency required.

;;; Code:

(defun workset-worktree-create (repo-root worktree-path branch &optional start-point)
  "Create a git worktree at WORKTREE-PATH with BRANCH from REPO-ROOT.
START-POINT defaults to HEAD.  If BRANCH already exists, checks it out
into the worktree instead of creating a new branch."
  (let ((default-directory repo-root)
        (start (or start-point "HEAD")))
    (make-directory (file-name-directory worktree-path) t)
    (let ((exit-code
           (call-process "git" nil nil nil
                         "worktree" "add" "-b" branch worktree-path start)))
      (unless (zerop exit-code)
        ;; Branch may already exist; try without -b
        (let ((exit-code2
               (call-process "git" nil nil nil
                             "worktree" "add" worktree-path branch)))
          (unless (zerop exit-code2)
            (error "Failed to create worktree at %s for branch %s" worktree-path branch)))))))

(defun workset-worktree-remove (repo-root worktree-path)
  "Remove git worktree at WORKTREE-PATH from REPO-ROOT.
Offers force removal on failure, then prunes stale worktrees."
  (let ((default-directory repo-root))
    (let ((exit-code
           (call-process "git" nil nil nil "worktree" "remove" worktree-path)))
      (unless (zerop exit-code)
        (if (yes-or-no-p (format "Worktree removal failed.  Force remove %s? " worktree-path))
            (let ((exit-code2
                   (call-process "git" nil nil nil
                                 "worktree" "remove" "--force" worktree-path)))
              (unless (zerop exit-code2)
                (error "Force removal of worktree %s failed" worktree-path)))
          (error "Worktree removal aborted"))))
    (call-process "git" nil nil nil "worktree" "prune")))

(defun workset-worktree-list (repo-root)
  "List git worktrees for REPO-ROOT.
Returns a list of plists with :path, :head, and :branch keys."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (call-process "git" nil t nil "worktree" "list" "--porcelain")
      (workset-worktree--parse-porcelain (buffer-string)))))

(defun workset-worktree--parse-porcelain (output)
  "Parse porcelain OUTPUT from `git worktree list' into plists."
  (let ((entries nil)
        (current nil))
    (dolist (line (split-string output "\n" t))
      (cond
       ((string-prefix-p "worktree " line)
        (when current
          (push (nreverse current) entries))
        (setq current (list :path (substring line 9))))
       ((string-prefix-p "HEAD " line)
        (setq current (plist-put current :head (substring line 5))))
       ((string-prefix-p "branch " line)
        (setq current (plist-put current :branch (substring line 7))))))
    (when current
      (push (nreverse current) entries))
    (nreverse entries)))

(defun workset-worktree-copy-files (source-dir target-dir patterns)
  "Copy files matching PATTERNS from SOURCE-DIR to TARGET-DIR.
PATTERNS is a list of filenames or glob patterns.
Only copies files that don't already exist in TARGET-DIR."
  (dolist (pattern patterns)
    (let ((matches (if (workset-worktree--glob-pattern-p pattern)
                       (file-expand-wildcards
                        (expand-file-name pattern source-dir) t)
                     (let ((f (expand-file-name pattern source-dir)))
                       (when (file-exists-p f) (list f))))))
      (dolist (source-file matches)
        (let* ((relative (file-relative-name source-file source-dir))
               (target-file (expand-file-name relative target-dir)))
          (unless (file-exists-p target-file)
            (make-directory (file-name-directory target-file) t)
            (copy-file source-file target-file)))))))

(defun workset-worktree--glob-pattern-p (pattern)
  "Return non-nil if PATTERN contains glob characters."
  (string-match-p "[*?\\[]" pattern))

(provide 'workset-worktree)
;;; workset-worktree.el ends here
