;;; workset-worktree.el --- Git worktree operations for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Git worktree create/remove/list operations using call-process directly.
;; No magit dependency required.

;;; Code:

(require 'cl-lib)

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
          (push current entries))
        (setq current (list :path (substring line 9))))
       ((string-prefix-p "HEAD " line)
        (setq current (plist-put current :head (substring line 5))))
       ((string-prefix-p "branch " line)
        (setq current (plist-put current :branch (substring line 7))))))
    (when current
      (push current entries))
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

(defun workset-worktree-list-branches (repo-root)
  "Return a deduplicated list of branch names for REPO-ROOT.
Runs `git branch --all --format=%(refname:short)' and strips
remote prefixes for deduplication."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code (call-process "git" nil t nil
                                     "branch" "--all"
                                     "--format=%(refname:short)")))
        (unless (zerop exit-code)
          (error "Failed to list branches in %s" repo-root))
        (let ((branches nil))
          (dolist (line (split-string (buffer-string) "\n" t))
            (let ((name (string-trim line)))
              (unless (string-suffix-p "/HEAD" name)
                (push name branches))))
          (delete-dups (nreverse branches)))))))

(defun workset-worktree--task-from-branch (branch &optional branch-prefix)
  "Derive a task name from BRANCH by stripping remote and BRANCH-PREFIX.
Strips leading `origin/', `remotes/origin/' then BRANCH-PREFIX."
  (let ((name branch))
    (when (string-prefix-p "remotes/" name)
      (setq name (replace-regexp-in-string "\\`remotes/[^/]+/" "" name)))
    (when (string-prefix-p "origin/" name)
      (setq name (substring name (length "origin/"))))
    (when (and branch-prefix
               (not (string-empty-p branch-prefix))
               (string-prefix-p branch-prefix name))
      (setq name (substring name (length branch-prefix))))
    name))

(defun workset-worktree--read-branch-from-head (head-file)
  "Read HEAD-FILE and return branch name, or nil if detached."
  (when (file-readable-p head-file)
    (with-temp-buffer
      (insert-file-contents head-file)
      (let ((content (string-trim (buffer-string))))
        (when (string-prefix-p "ref: refs/heads/" content)
          (substring content (length "ref: refs/heads/")))))))

(defun workset-worktree--resolve-gitdir (dot-git-file)
  "Read a .git file and return the gitdir path it points to."
  (when (file-readable-p dot-git-file)
    (with-temp-buffer
      (insert-file-contents dot-git-file)
      (let ((content (string-trim (buffer-string))))
        (when (string-prefix-p "gitdir: " content)
          (let ((gitdir (substring content (length "gitdir: "))))
            ;; Resolve relative paths relative to the .git file's directory
            (expand-file-name gitdir (file-name-directory dot-git-file))))))))

(defun workset-worktree--repo-root-from-gitdir (gitdir)
  "Derive the main repo root from a linked worktree's GITDIR path.
GITDIR is typically /path/to/repo/.git/worktrees/NAME."
  ;; Go up from .git/worktrees/NAME to .git, then to repo root
  (let ((git-dir (file-name-directory (directory-file-name
                   (file-name-directory (directory-file-name gitdir))))))
    (file-name-directory (directory-file-name git-dir))))

(defun workset-worktree-discover-in-directory (base-dir &optional max-depth)
  "Discover git worktrees under BASE-DIR up to MAX-DEPTH levels deep.
Returns a list of plists with :path, :branch, :repo-root, and :type keys.
TYPE is either `linked' (linked worktree) or `main' (standalone repo)."
  (let ((depth (or max-depth 4))
        (result nil)
        (skip-dirs '(".git" "node_modules" ".cache" "elpa" ".venv" ".tox")))
    (cl-labels ((walk (dir level)
      (when (and (file-directory-p dir) (<= level depth))
        (let ((dot-git (expand-file-name ".git" dir)))
          (cond
           ((file-regular-p dot-git)  ;; linked worktree
            (let* ((gitdir (workset-worktree--resolve-gitdir dot-git))
                   (repo-root (when gitdir (workset-worktree--repo-root-from-gitdir gitdir)))
                   (head-file (when gitdir (expand-file-name "HEAD" gitdir)))
                   (branch (when head-file (workset-worktree--read-branch-from-head head-file))))
              (push (list :path dir :branch branch :repo-root repo-root :type 'linked) result)))
           ((file-directory-p dot-git)  ;; main repo
            (let* ((head-file (expand-file-name "HEAD" dot-git))
                   (branch (workset-worktree--read-branch-from-head head-file)))
              (push (list :path dir :branch branch :repo-root dir :type 'main) result)))
           (t  ;; No .git here, keep walking
            (dolist (entry (directory-files dir t nil t))
              (when (and (file-directory-p entry)
                         (not (member (file-name-nondirectory entry) (append '("." "..") skip-dirs))))
                (walk entry (1+ level))))))))))
      (walk (expand-file-name base-dir) 0)
      (nreverse result))))

(provide 'workset-worktree)
;;; workset-worktree.el ends here
