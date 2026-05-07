;;; workset-worktree.el --- Git worktree operations for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Git worktree create/remove/list operations using the wt CLI.
;; No magit dependency required.

;;; Code:

(require 'cl-lib)

(defcustom workset-process-timeout 30
  "Timeout in seconds for external process calls (wt, git, gh).
Set to nil to disable the timeout."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "No timeout" nil))
  :group 'workset)

(defun workset--call-process (program buffer &rest args)
  "Run PROGRAM with ARGS, inserting stdout into BUFFER.
BUFFER follows `call-process' convention: t means current buffer,
nil means discard.  Stderr is always discarded.
Respects `workset-process-timeout'; signals an error on timeout."
  (if (null workset-process-timeout)
      ;; No timeout — use synchronous call
      (apply #'call-process program nil
             (if buffer '(t nil) nil) nil args)
    ;; Async process with timeout so Emacs stays responsive
    (let* ((stderr-buf (generate-new-buffer " *workset-stderr*"))
           (out-buf (if (eq buffer t) (current-buffer) buffer))
           (proc (make-process
                  :name "workset-proc"
                  :buffer out-buf
                  :stderr stderr-buf
                  :command (cons program args)
                  :connection-type 'pipe
                  :sentinel #'ignore))
           (deadline (+ (float-time) workset-process-timeout)))
      (unwind-protect
          (progn
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (when (process-live-p proc)
              (kill-process proc)
              ;; Wait briefly for process to die before signaling
              (accept-process-output proc 0.1)
              (error "%s timed out after %ds in %s"
                     program workset-process-timeout default-directory))
            (process-exit-status proc))
        ;; Clean up stderr buffer without prompting
        (when (buffer-live-p stderr-buf)
          (let ((stderr-proc (get-buffer-process stderr-buf)))
            (when (and stderr-proc (process-live-p stderr-proc))
              (delete-process stderr-proc)))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer stderr-buf)))))))

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

(defun workset-worktree--parse-wt-entry (entry)
  "Convert a single hash-table ENTRY from `wt list --format=json' into a plist.
Returns a flat plist with keys:
  :branch :path :kind
  :commit-sha :commit-short-sha :commit-message :commit-timestamp
  :working-tree :main-state :main-ahead :main-behind
  :remote-name :remote-branch :remote-ahead :remote-behind
  :symbols :is-main :is-current :is-previous"
  (let* ((raw-branch (gethash "branch" entry))
         (branch (when (and raw-branch (not (equal raw-branch "")))
                   (replace-regexp-in-string "\\`refs/heads/" "" raw-branch)))
         (kind (gethash "kind" entry))
         (path (when (equal kind "worktree") (gethash "path" entry)))
         (commit (gethash "commit" entry))
         (main   (gethash "main" entry))
         (remote (gethash "remote" entry)))
    (list :branch            branch
          :path              path
          :kind              kind
          :commit-sha        (when commit (gethash "sha" commit))
          :commit-short-sha  (when commit (gethash "short_sha" commit))
          :commit-message    (when commit (gethash "message" commit))
          :commit-timestamp  (when commit (gethash "timestamp" commit))
          :working-tree      (gethash "working_tree" entry)
          :main-state        (gethash "main_state" entry)
          :main-ahead        (when main (gethash "ahead" main))
          :main-behind       (when main (gethash "behind" main))
          :remote-name       (when remote (gethash "name" remote))
          :remote-branch     (when remote (gethash "branch" remote))
          :remote-ahead      (when remote (gethash "ahead" remote))
          :remote-behind     (when remote (gethash "behind" remote))
          :symbols           (gethash "symbols" entry)
          :is-main           (gethash "is_main" entry)
          :is-current        (gethash "is_current" entry)
          :is-previous       (gethash "is_previous" entry))))

(defun workset-worktree-list-full (repo-root &optional include-branches)
  "List all wt entries for REPO-ROOT with full metadata.
When INCLUDE-BRANCHES is non-nil, passes --branches to wt list.
Calls `wt list --format=json [--branches]' and parses the output.
Returns a list of plists as produced by `workset-worktree--parse-wt-entry'."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let* ((args (append '("list" "--format=json")
                           (when include-branches '("--branches"))))
             (exit-code (apply #'workset--call-process "wt" t args)))
        (unless (zerop exit-code)
          (error "Wt list --format=json failed (exit %d): %s"
                 exit-code (string-trim (buffer-string)))))
      (goto-char (point-min))
      (condition-case err
          (let ((json (json-parse-buffer :object-type 'hash-table
                                         :array-type 'list)))
            (mapcar #'workset-worktree--parse-wt-entry json))
        (error
         (error "Failed to parse wt list JSON output: %s\nBuffer contents: %s"
                (error-message-string err)
                (string-trim (buffer-string))))))))

(defun workset-worktree-list (repo-root)
  "List git worktrees for REPO-ROOT using wt CLI.
Calls `wt list --format=json' and parses the output.
Returns a list of plists with :path, :head, and :branch keys.
Branch values are bare names with no refs/heads/ prefix."
  (let ((full (workset-worktree-list-full repo-root)))
    (mapcar (lambda (entry)
              (list :path   (plist-get entry :path)
                    :head   (plist-get entry :commit-sha)
                    :branch (plist-get entry :branch)))
            (cl-remove-if (lambda (entry)
                            (equal (plist-get entry :kind) "branch"))
                          full))))


(defun workset-worktree-list-branches (repo-root)
  "Return a deduplicated list of branch names for REPO-ROOT.
Runs `git branch --all --format=%(refname:short)' and strips
remote prefixes for deduplication."
  (let ((default-directory repo-root))
    (with-temp-buffer
      (let ((exit-code (workset--call-process "git" t
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

(defun workset-worktree-merge (repo-root branch &optional target)
  "Merge worktree for BRANCH from REPO-ROOT using wt CLI.
Looks up the worktree path for BRANCH via `workset-worktree-list-full',
then runs `wt merge -y [TARGET]' from that worktree directory.
Returns non-nil on success, signals an error on failure."
  (let* ((entries (workset-worktree-list-full repo-root))
         (entry (cl-find-if (lambda (e)
                              (equal (plist-get e :branch) branch))
                            entries)))
    (unless entry
      (error "No worktree found for branch %s" branch))
    (let ((worktree-path (plist-get entry :path)))
      (unless worktree-path
        (error "Worktree for branch %s has no path" branch))
      (let ((default-directory worktree-path))
        (with-temp-buffer
          (let* ((args (append '("merge" "-y")
                               (when target (list target))))
                 (exit-code (apply #'workset--call-process "wt" t args)))
            (unless (zerop exit-code)
              (error "Wt merge failed for branch %s: %s"
                     branch (string-trim (buffer-string))))
            t))))))

(provide 'workset-worktree)
;;; workset-worktree.el ends here
