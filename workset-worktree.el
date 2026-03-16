;;; workset-worktree.el --- Git worktree operations for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Git worktree create/remove/list operations using the wt CLI.
;; No magit dependency required.

;;; Code:

(require 'cl-lib)

(defun workset-worktree-create (repo-root branch)
  "Create a worktree for BRANCH from REPO-ROOT using wt CLI.
Calls `wt switch -c BRANCH --no-cd -y' from REPO-ROOT, then resolves
the new worktree path from `wt list --format=json'.
Returns the path to the created worktree."
  (let ((default-directory repo-root))
    (let ((exit-code
           (call-process "wt" nil nil nil
                         "switch" "-c" branch "--no-cd" "-y")))
      (unless (zerop exit-code)
        (error "Wt switch -c %s failed (exit %d)" branch exit-code)))
    ;; Resolve the worktree path by listing all worktrees
    (let ((worktrees (workset-worktree-list repo-root)))
      (let ((entry (cl-find-if (lambda (wt)
                                 (equal (plist-get wt :branch) branch))
                               worktrees)))
        (unless entry
          (error "Could not find worktree for branch %s after creation" branch))
        (plist-get entry :path)))))

(defun workset-worktree-remove (repo-root branch)
  "Remove the worktree for BRANCH from REPO-ROOT using wt CLI.
Calls `wt remove BRANCH -y --force' from REPO-ROOT."
  (let ((default-directory repo-root))
    (let ((exit-code
           (call-process "wt" nil nil nil
                         "remove" branch "-y" "--force")))
      (unless (zerop exit-code)
        (error "Wt remove %s failed (exit %d)" branch exit-code)))))

(defun workset-worktree-list (repo-root)
  "List git worktrees for REPO-ROOT using wt CLI.
Calls `wt list --format=json' and parses the output.
Returns a list of plists with :path, :head, and :branch keys.
Branch values are bare names with no refs/heads/ prefix."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code
             (call-process "wt" nil t nil "list" "--format=json")))
        (unless (zerop exit-code)
          (error "Wt list --format=json failed (exit %d)" exit-code)))
      (goto-char (point-min))
      (condition-case err
          (let ((json (json-parse-buffer :object-type 'hash-table
                                         :array-type 'list)))
            (mapcar (lambda (entry)
                      (let* ((raw-branch (gethash "branch" entry))
                             (branch (when (and raw-branch
                                                (not (equal raw-branch "")))
                                       (replace-regexp-in-string
                                        "\\`refs/heads/" "" raw-branch))))
                        (list :path   (gethash "path" entry)
                              :head   (gethash "head" entry)
                              :branch branch)))
                    json))
        (error
         (error "Failed to parse wt list JSON output: %s" (error-message-string err)))))))

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
