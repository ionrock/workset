;;; workset-test.el --- Tests for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for workset.  Covers pure functions (no git/vterm side effects)
;; and integration tests that use temporary git repos.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'workset)

;;;; Unit tests — pure functions

(ert-deftest workset-test-repo-name ()
  "Test extracting repo name from paths."
  (should (equal (workset--repo-name "/home/user/projects/myrepo")
                 "myrepo"))
  (should (equal (workset--repo-name "/home/user/projects/myrepo/")
                 "myrepo"))
  (should (equal (workset--repo-name "/repo")
                 "repo")))

(ert-deftest workset-test-worktree-directory ()
  "Test worktree directory construction."
  (let ((workset-base-directory "/tmp/workset-test-base"))
    (should (equal (workset--worktree-directory "myrepo" "fix-bug")
                   "/tmp/workset-test-base/worktrees/myrepo/fix-bug"))))

(ert-deftest workset-test-key ()
  "Test workset key formatting."
  (should (equal (workset--key "myrepo" "fix-bug")
                 "myrepo/fix-bug")))

(ert-deftest workset-test-put-get-remove ()
  "Test alist operations for active worksets."
  (let ((workset--active-worksets nil))
    ;; Put and get
    (workset--put "repo/task1" '(:repo-root "/r" :branch "task1"))
    (should (equal (workset--get "repo/task1")
                   '(:repo-root "/r" :branch "task1")))
    ;; Overwrite
    (workset--put "repo/task1" '(:repo-root "/r2" :branch "task1b"))
    (should (equal (plist-get (workset--get "repo/task1") :repo-root) "/r2"))
    ;; Second entry
    (workset--put "repo/task2" '(:repo-root "/r" :branch "task2"))
    (should (equal (length workset--active-worksets) 2))
    ;; Remove
    (workset--remove "repo/task1")
    (should-not (workset--get "repo/task1"))
    (should (workset--get "repo/task2"))
    ;; Remove last
    (workset--remove "repo/task2")
    (should (null workset--active-worksets))))

(ert-deftest workset-test-active-keys ()
  "Test listing active workset keys."
  (let ((workset--active-worksets nil))
    (workset--put "r/a" '(:branch "a"))
    (workset--put "r/b" '(:branch "b"))
    (should (equal (sort (workset--active-keys) #'string<)
                   '("r/a" "r/b")))))

(ert-deftest workset-test-notify-match-state ()
  "Test notification pattern matching."
  (let ((workset-notify-input-patterns '("input please"))
        (workset-notify-done-patterns '("all done")))
    (should (eq (workset-notify--match-state "input please") 'needs-input))
    (should (eq (workset-notify--match-state "all done") 'done))
    (should-not (workset-notify--match-state "still working"))))

(ert-deftest workset-test-notify-match-priority ()
  "Test that input-needed has priority over done."
  (let ((workset-notify-input-patterns '("ready"))
        (workset-notify-done-patterns '("ready")))
    (should (eq (workset-notify--match-state "ready") 'needs-input))))

(ert-deftest workset-test-notify-trim-output ()
  "Test trimming recent output window."
  (let ((workset-notify-max-output 5))
    (should (equal (workset-notify--trim-output "abc") "abc"))
    (should (equal (workset-notify--trim-output "abcdef") "bcdef"))))

(ert-deftest workset-test-notify-play-sound-disabled ()
  "Sound is not played when workset-notify-sound-enabled is nil."
  (let ((workset-notify-sound-enabled nil)
        (called nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (setq called t) nil)))
      (workset-notify--play-sound "Glass" 'done))
    (should-not called)))

(ert-deftest workset-test-notify-play-sound-non-darwin ()
  "Sound is not played on non-macOS systems."
  (let ((workset-notify-sound-enabled t)
        (called nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (setq called t) nil)))
      (let ((system-type 'gnu/linux))
        (workset-notify--play-sound "Glass" 'done)))
    (should-not called)))

(ert-deftest workset-test-notify-play-sound-throttle ()
  "Sound is throttled when called too rapidly for the same state."
  (let ((workset-notify-sound-enabled t)
        (workset-notify-sound-throttle-seconds 60)
        (workset-notify-sound-command "afplay")
        (workset-notify--last-sound-time nil)
        (call-count 0))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (let ((system-type 'darwin))
        ;; First call should play
        (workset-notify--play-sound "Glass" 'done)
        (should (= call-count 1))
        ;; Second call within throttle window should NOT play
        (workset-notify--play-sound "Glass" 'done)
        (should (= call-count 1))))))

(ert-deftest workset-test-notify-play-sound-different-states ()
  "Sound throttle is per-state: different states are not throttled together."
  (let ((workset-notify-sound-enabled t)
        (workset-notify-sound-throttle-seconds 60)
        (workset-notify-sound-command "afplay")
        (workset-notify--last-sound-time nil)
        (call-count 0))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (let ((system-type 'darwin))
        (workset-notify--play-sound "Glass" 'done)
        (should (= call-count 1))
        ;; Different state should still play immediately
        (workset-notify--play-sound "Sosumi" 'needs-input)
        (should (= call-count 2))))))

(ert-deftest workset-test-notify-play-sound-missing-file ()
  "A warning message is emitted when the sound file does not exist."
  (let ((workset-notify-sound-enabled t)
        (workset-notify-sound-throttle-seconds 0)
        (workset-notify-sound-command "afplay")
        (workset-notify--last-sound-time nil)
        (warned nil))
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "sound file not found" (apply #'format fmt args))
                   (setq warned t)))))
      (let ((system-type 'darwin))
        (workset-notify--play-sound "NoSuchSound" 'done)))
    (should warned)))

(ert-deftest workset-test-notify-claude-code-input-patterns ()
  "Test that default input patterns detect Claude Code prompt markers."
  ;; Claude Code prompt marker
  (should (workset-notify--matches-any workset-notify-input-patterns "> "))
  ;; Y/n prompts
  (should (workset-notify--matches-any workset-notify-input-patterns "Do you want to proceed? [Y/n]"))
  (should (workset-notify--matches-any workset-notify-input-patterns "(y/n)"))
  (should (workset-notify--matches-any workset-notify-input-patterns "(Y/n)"))
  (should (workset-notify--matches-any workset-notify-input-patterns "Press enter"))
  ;; Legacy generic patterns still work
  (should (workset-notify--matches-any workset-notify-input-patterns "awaiting your input")))

(ert-deftest workset-test-notify-claude-code-done-patterns ()
  "Test that default done patterns detect Claude Code completion indicators."
  (should (workset-notify--matches-any workset-notify-done-patterns "✓ Completed"))
  (should (workset-notify--matches-any workset-notify-done-patterns "✓ Done"))
  (should (workset-notify--matches-any workset-notify-done-patterns "Task completed"))
  (should (workset-notify--matches-any workset-notify-done-patterns "Changes applied"))
  ;; Legacy patterns still work
  (should (workset-notify--matches-any workset-notify-done-patterns "all done")))

(ert-deftest workset-test-notify-working-patterns ()
  "Test that default working patterns detect common progress indicators."
  (should (workset-notify--matches-any workset-notify-working-patterns "Thinking..."))
  (should (workset-notify--matches-any workset-notify-working-patterns "Analyzing..."))
  (should (workset-notify--matches-any workset-notify-working-patterns "Reading file"))
  (should (workset-notify--matches-any workset-notify-working-patterns "Writing file"))
  (should (workset-notify--matches-any workset-notify-working-patterns "Searching")))

(ert-deftest workset-test-notify-use-preset-merges ()
  "Test that use-preset merges patterns without overwriting existing ones."
  (let ((workset-notify-input-patterns '("existing-input"))
        (workset-notify-done-patterns '("existing-done"))
        (workset-notify-working-patterns '("existing-working"))
        (workset-notify-agent-presets
         '((test-agent
            :input ("preset-input-1" "preset-input-2")
            :done ("preset-done-1")
            :working ("preset-working-1")))))
    (workset-notify-use-preset 'test-agent)
    ;; Original patterns preserved
    (should (member "existing-input" workset-notify-input-patterns))
    (should (member "existing-done" workset-notify-done-patterns))
    (should (member "existing-working" workset-notify-working-patterns))
    ;; Preset patterns appended
    (should (member "preset-input-1" workset-notify-input-patterns))
    (should (member "preset-input-2" workset-notify-input-patterns))
    (should (member "preset-done-1" workset-notify-done-patterns))
    (should (member "preset-working-1" workset-notify-working-patterns))))

(ert-deftest workset-test-notify-use-preset-no-duplicates ()
  "Test that use-preset does not add duplicate patterns."
  (let ((workset-notify-input-patterns '("shared-pattern"))
        (workset-notify-done-patterns nil)
        (workset-notify-working-patterns nil)
        (workset-notify-agent-presets
         '((test-agent
            :input ("shared-pattern" "new-pattern")
            :done nil
            :working nil))))
    (workset-notify-use-preset 'test-agent)
    ;; shared-pattern appears only once
    (should (= 1 (cl-count "shared-pattern" workset-notify-input-patterns :test #'equal)))
    ;; new-pattern was added
    (should (member "new-pattern" workset-notify-input-patterns))))

(ert-deftest workset-test-notify-use-preset-unknown-agent ()
  "Test that use-preset signals an error for unknown agents."
  (let ((workset-notify-agent-presets '((known-agent :input nil :done nil :working nil))))
    (should-error (workset-notify-use-preset 'unknown-agent) :type 'user-error)))

(ert-deftest workset-test-notify-preset-claude-code-exists ()
  "Test that the claude-code preset is defined with expected keys."
  (let ((preset (alist-get 'claude-code workset-notify-agent-presets)))
    (should preset)
    (should (plist-get preset :input))
    (should (plist-get preset :done))
    (should (plist-get preset :working))))

;;;; vterm buffer naming tests

(ert-deftest workset-test-format-buffer-name ()
  "Test vterm buffer name formatting."
  (should (equal (workset-vterm--format-buffer-name "*workset: %r/%t<%n>*" "myrepo" "fix-bug" 1)
                 "*workset: myrepo/fix-bug<1>*"))
  (should (equal (workset-vterm--format-buffer-name "*%t@%r[%n]*" "repo" "task" 3)
                 "*task@repo[3]*")))

(ert-deftest workset-test-next-index ()
  "Test finding next unused buffer index."
  (let ((fmt "*test-ws-%r-%t-%n*"))
    ;; No buffers exist, should return 1
    (should (= (workset-vterm--next-index fmt "r" "t") 1))
    ;; Create buffer at index 1
    (let ((buf (get-buffer-create (workset-vterm--format-buffer-name fmt "r" "t" 1))))
      (unwind-protect
          (progn
            (should (= (workset-vterm--next-index fmt "r" "t") 2))
            ;; Create buffer at index 2
            (let ((buf2 (get-buffer-create (workset-vterm--format-buffer-name fmt "r" "t" 2))))
              (unwind-protect
                  (should (= (workset-vterm--next-index fmt "r" "t") 3))
                (kill-buffer buf2))))
        (kill-buffer buf)))))

(ert-deftest workset-test-next-index-gap ()
  "Test that next-index fills gaps."
  (let ((fmt "*test-ws-gap-%r-%t-%n*"))
    ;; Create buffer at index 2 only (gap at 1)
    (let ((buf (get-buffer-create (workset-vterm--format-buffer-name fmt "r" "t" 2))))
      (unwind-protect
          ;; Should return 1 since that slot is free
          (should (= (workset-vterm--next-index fmt "r" "t") 1))
        (kill-buffer buf)))))

;;;; Project backend tests

(ert-deftest workset-test-project-backend-auto ()
  "Test auto backend resolution."
  (let ((workset-project-backend 'auto))
    ;; project.el is always available in Emacs 29+
    ;; If projectile is not loaded, should return 'project
    (unless (featurep 'projectile)
      (should (eq (workset-project--backend) 'project)))))

(ert-deftest workset-test-project-backend-explicit ()
  "Test explicit backend selection."
  (let ((workset-project-backend 'project))
    (should (eq (workset-project--backend) 'project)))
  (let ((workset-project-backend 'projectile))
    (should (eq (workset-project--backend) 'projectile))))

;;;; Worktree helper tests

(ert-deftest workset-test-glob-pattern-p ()
  "Test glob pattern detection."
  (should (workset-worktree--glob-pattern-p "*.env"))
  (should (workset-worktree--glob-pattern-p "file[0-9]"))
  (should (workset-worktree--glob-pattern-p "dir/?.txt"))
  (should-not (workset-worktree--glob-pattern-p ".env"))
  (should-not (workset-worktree--glob-pattern-p "docker-compose.yml")))

(ert-deftest workset-test-parse-porcelain ()
  "Test parsing git worktree list --porcelain output."
  (let ((output "worktree /home/user/repo\nHEAD abc123\nbranch refs/heads/main\n\nworktree /home/user/repo-wt\nHEAD def456\nbranch refs/heads/feature\n"))
    (let ((result (workset-worktree--parse-porcelain output)))
      (should (= (length result) 2))
      (should (equal (plist-get (car result) :path) "/home/user/repo"))
      (should (equal (plist-get (car result) :head) "abc123"))
      (should (equal (plist-get (car result) :branch) "refs/heads/main"))
      (should (equal (plist-get (cadr result) :path) "/home/user/repo-wt"))
      (should (equal (plist-get (cadr result) :branch) "refs/heads/feature")))))

;;;; Integration tests — require temp git repo

(ert-deftest workset-test-worktree-create-remove ()
  "Integration test: create and remove a worktree."
  (let* ((tmpdir (make-temp-file "workset-test-" t))
         (repo-dir (expand-file-name "repo" tmpdir))
         (wt-dir (expand-file-name "worktree" tmpdir)))
    (unwind-protect
        (progn
          ;; Set up a git repo with an initial commit
          (make-directory repo-dir t)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README" repo-dir)
              (insert "test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "init"))
          ;; Create worktree
          (workset-worktree-create repo-dir wt-dir "test-branch")
          (should (file-directory-p wt-dir))
          (should (file-exists-p (expand-file-name "README" wt-dir)))
          ;; Remove worktree
          ;; Use cl-letf to stub yes-or-no-p in case of force prompt
          (workset-worktree-remove repo-dir wt-dir)
          (should-not (file-directory-p wt-dir)))
      (delete-directory tmpdir t))))

(ert-deftest workset-test-worktree-copy-files ()
  "Integration test: copy files matching patterns."
  (let* ((tmpdir (make-temp-file "workset-test-copy-" t))
         (source (expand-file-name "source" tmpdir))
         (target (expand-file-name "target" tmpdir)))
    (unwind-protect
        (progn
          (make-directory source t)
          (make-directory target t)
          ;; Create source files
          (with-temp-file (expand-file-name ".env" source)
            (insert "SECRET=foo\n"))
          (with-temp-file (expand-file-name ".envrc" source)
            (insert "use nix\n"))
          (with-temp-file (expand-file-name "unrelated.txt" source)
            (insert "nope\n"))
          ;; Copy with patterns
          (workset-worktree-copy-files source target '(".env" ".envrc" ".missing"))
          ;; .env and .envrc should be copied
          (should (file-exists-p (expand-file-name ".env" target)))
          (should (file-exists-p (expand-file-name ".envrc" target)))
          ;; unrelated.txt should not
          (should-not (file-exists-p (expand-file-name "unrelated.txt" target)))
          ;; Already-existing files should not be overwritten
          (with-temp-file (expand-file-name ".env" target)
            (insert "ORIGINAL\n"))
          (workset-worktree-copy-files source target '(".env"))
          (with-temp-buffer
            (insert-file-contents (expand-file-name ".env" target))
            (should (equal (buffer-string) "ORIGINAL\n"))))
      (delete-directory tmpdir t))))

(ert-deftest workset-test-worktree-list ()
  "Integration test: list worktrees."
  (let* ((tmpdir (make-temp-file "workset-test-list-" t))
         (repo-dir (expand-file-name "repo" tmpdir))
         (wt-dir (expand-file-name "worktree" tmpdir)))
    (unwind-protect
        (progn
          (make-directory repo-dir t)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README" repo-dir)
              (insert "test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "init"))
          ;; Create a worktree
          (workset-worktree-create repo-dir wt-dir "list-test-branch")
          ;; List should contain both the main repo and the worktree
          (let ((trees (workset-worktree-list repo-dir)))
            (should (>= (length trees) 2))
            (let ((wt-true (file-truename wt-dir)))
              (should (cl-some (lambda (wt)
                                 (equal (file-truename (plist-get wt :path))
                                        wt-true))
                               trees))))
          ;; Clean up worktree
          (workset-worktree-remove repo-dir wt-dir))
      (delete-directory tmpdir t))))

;;;; Branch helper tests

(ert-deftest workset-test-task-from-branch ()
  "Test deriving task name from branch."
  ;; Local branch, no prefix
  (should (equal (workset-worktree--task-from-branch "fix-bug") "fix-bug"))
  ;; Remote branch
  (should (equal (workset-worktree--task-from-branch "origin/fix-bug") "fix-bug"))
  ;; remotes/origin/ prefix
  (should (equal (workset-worktree--task-from-branch "remotes/origin/fix-bug") "fix-bug"))
  ;; With branch prefix
  (should (equal (workset-worktree--task-from-branch "eric/fix-bug" "eric/") "fix-bug"))
  ;; Remote + branch prefix
  (should (equal (workset-worktree--task-from-branch "origin/eric/fix-bug" "eric/") "fix-bug"))
  ;; No match for prefix — keep as-is
  (should (equal (workset-worktree--task-from-branch "other/fix-bug" "eric/") "other/fix-bug")))

(ert-deftest workset-test-gh-list-prs-parse ()
  "Test PR list parsing with stubbed call-process."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer &rest _args)
               (when buffer
                 (with-current-buffer (if (eq buffer t) (current-buffer) buffer)
                   (insert "42\tFix login bug\n7\tAdd dark mode\n")))
               0)))
    (let ((result (workset--gh-list-prs "/tmp/fake-repo")))
      (should (equal (length result) 2))
      (should (equal (car (nth 0 result)) "#42: Fix login bug"))
      (should (equal (cdr (nth 0 result)) "42"))
      (should (equal (car (nth 1 result)) "#7: Add dark mode"))
      (should (equal (cdr (nth 1 result)) "7")))))

(ert-deftest workset-test-gh-pr-branch ()
  "Test PR branch lookup with stubbed call-process."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer &rest _args)
               (when buffer
                 (with-current-buffer (if (eq buffer t) (current-buffer) buffer)
                   (insert "feature/cool-thing\n")))
               0)))
    (should (equal (workset--gh-pr-branch "/tmp/fake-repo" "42")
                   "feature/cool-thing"))))

(ert-deftest workset-test-list-branches ()
  "Integration test: list branches in a temp git repo."
  (let* ((tmpdir (make-temp-file "workset-test-branches-" t))
         (repo-dir (expand-file-name "repo" tmpdir)))
    (unwind-protect
        (progn
          (make-directory repo-dir t)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README" repo-dir)
              (insert "test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "init")
            (call-process "git" nil nil nil "branch" "feature-a")
            (call-process "git" nil nil nil "branch" "feature-b"))
          (let ((branches (workset-worktree-list-branches repo-dir)))
            (should (member "feature-a" branches))
            (should (member "feature-b" branches))
            ;; main or master should be present
            (should (cl-some (lambda (b) (member b '("main" "master"))) branches))))
      (delete-directory tmpdir t))))

(provide 'workset-test)
;;; workset-test.el ends here
