;;; workset-notify.el --- Vterm notifications for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Detect agent output in vterm buffers and notify via modeline, messages,
;; and/or sounds.  Supports macOS system sounds via `afplay' when
;; `workset-notify-method' includes sound (e.g. `modeline-and-sound').
;; Agent-specific detection patterns can be loaded via
;; `workset-notify-use-preset'.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup workset-notify nil
  "Notifications for workset vterm buffers."
  :group 'workset
  :prefix "workset-notify-")

(defcustom workset-notify-enabled t
  "Whether to enable notifications in workset vterm buffers."
  :type 'boolean
  :group 'workset-notify)

(defcustom workset-notify-input-patterns
  '(;; Claude Code: prompt marker / waiting state
    "^> $"
    "\\? \\[Y/n\\]"
    "\\? \\[y/N\\]"
    ;; Generic input/confirmation prompts
    "\\bDo you want to proceed\\b"
    "\\bawaiting your input\\b"
    "\\bneed your input\\b"
    "\\bplease respond\\b"
    "\\benter to continue\\b"
    "\\bpress \\(enter\\|return\\) to continue\\b"
    "\\[Y/n\\]"
    "\\[y/N\\]"
    "(y/n)"
    "(Y/n)"
    "Press enter")
  "Regex patterns that indicate an agent is waiting for user input."
  :type '(repeat string)
  :group 'workset-notify)

(defcustom workset-notify-done-patterns
  '(;; Claude Code: idle/done state indicator
    "✓ Completed"
    "✓ Done"
    "⎿ .* completed"
    ;; Generic done patterns
    "\\bTask completed\\b"
    "\\bChanges applied\\b"
    "\\ball done\\b"
    "\\btask complete\\b"
    "\\bfinished\\b"
    "\\bcompleted\\b"
    "\\bdone\\b")
  "Regex patterns that indicate an agent is done."
  :type '(repeat string)
  :group 'workset-notify)

(defcustom workset-notify-working-patterns
  '(;; Claude Code / generic progress indicators
    "\\bThinking\\.\\.\\.\\b"
    "\\bAnalyzing\\.\\.\\.\\b"
    "\\bReading file\\b"
    "\\bWriting file\\b"
    "\\bRunning\\b"
    "\\bSearching\\b")
  "Optional regex patterns that indicate an agent is actively working.
If nil, any output counts as working."
  :type '(choice (const :tag "Any output" nil)
                 (repeat string))
  :group 'workset-notify)

(defcustom workset-notify-agent-presets
  '((claude-code
     :input ("^> $"
             "\\? \\[Y/n\\]"
             "\\? \\[y/N\\]"
             "\\bDo you want to proceed\\b"
             "\\bPress enter\\b")
     :done ("✓ Completed"
            "✓ Done"
            "⎿ .* completed"
            "\\bTask completed\\b"
            "\\bChanges applied\\b")
     :working ("\\bThinking\\.\\.\\.\\b"
               "\\bAnalyzing\\.\\.\\.\\b"
               "\\bReading file\\b"
               "\\bWriting file\\b"))
    (cursor
     :input ("\\bDo you want to proceed\\b"
             "\\[Y/n\\]"
             "(y/n)"
             "\\bPress enter\\b")
     :done ("\\bTask completed\\b"
            "\\bChanges applied\\b"
            "\\bDone\\b")
     :working ("\\bThinking\\.\\.\\.\\b"
               "\\bAnalyzing\\.\\.\\.\\b"
               "\\bApplying changes\\b"))
    (aider
     :input ("\\bDo you want to proceed\\b"
             "^aider> $"
             "\\[Y/n\\]"
             "(y/n)")
     :done ("\\bFiles? \\(created\\|edited\\)\\b"
            "\\bChanges applied\\b"
            "\\bTask completed\\b")
     :working ("\\bSearching\\b"
               "\\bAnalyzing\\.\\.\\.\\b"
               "\\bReading file\\b"
               "\\bWriting file\\b")))
  "Alist mapping agent name symbols to pattern lists.
Each entry has the form (AGENT-NAME :input PATS :done PATS :working PATS).
Use `workset-notify-use-preset' to load a preset."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type (repeat string)))
  :group 'workset-notify)

(defcustom workset-notify-notify-states '(needs-input done)
  "States that should trigger Emacs notifications."
  :type '(repeat symbol)
  :group 'workset-notify)

(defcustom workset-notify-method 'modeline
  "Notification method to use when state changes.

Available methods:
- `modeline': update the modeline indicator only.
- `modeline-and-message': modeline plus an echo-area message.
- `modeline-and-warning': modeline plus a `display-warning' popup.
- `modeline-and-sound': modeline plus a macOS system sound when the
  buffer is not currently visible.
- `sound': play a macOS system sound (no modeline update).
- `modeline-message-and-sound': modeline, echo-area message, and sound.

Sound methods require macOS and use `workset-notify-sound-command' to
play files from /System/Library/Sounds/.  See also
`workset-notify-sound-done', `workset-notify-sound-needs-input', and
`workset-notify-sound-throttle-seconds'."
  :type '(choice (const :tag "Modeline only" modeline)
                 (const :tag "Modeline + message" modeline-and-message)
                 (const :tag "Modeline + warning" modeline-and-warning)
                 (const :tag "Modeline + sound" modeline-and-sound)
                 (const :tag "Sound only" sound)
                 (const :tag "Modeline + message + sound" modeline-message-and-sound))
  :group 'workset-notify)

(defcustom workset-notify-max-output 2000
  "Max chars of recent output to keep for pattern matching."
  :type 'integer
  :group 'workset-notify)

(defcustom workset-notify-debounce-seconds 0.5
  "Minimum seconds between state changes (except input-needed)."
  :type 'number
  :group 'workset-notify)

(defcustom workset-notify-idle-seconds 10
  "Seconds of no output before marking the buffer idle.
Set to nil to disable idle detection."
  :type '(choice (const :tag "Disable" nil)
                 number)
  :group 'workset-notify)

(defcustom workset-notify-clear-commands
  '(vterm-self-insert
    vterm-send-return
    vterm-send-tab
    vterm-send-backspace
    vterm-send-C-c
    vterm-send-C-d
    vterm-send-C-u
    vterm-send-C-w)
  "Commands that clear a pending input notification."
  :type '(repeat symbol)
  :group 'workset-notify)

(defcustom workset-notify-sound-enabled t
  "Whether to play sounds on state notifications.
Has effect only when `workset-notify-method' is a sound-enabled method
such as `modeline-and-sound', `sound', or `modeline-message-and-sound'.
Sound playback is always a no-op on non-macOS systems regardless of
this setting."
  :type 'boolean
  :group 'workset-notify)

(defcustom workset-notify-sound-done "Glass"
  "Sound name to play when an agent finishes.
Must be the base name of a file in /System/Library/Sounds/ (without .aiff)."
  :type 'string
  :group 'workset-notify)

(defcustom workset-notify-sound-needs-input "Sosumi"
  "Sound name to play when an agent needs input.
Must be the base name of a file in /System/Library/Sounds/ (without .aiff)."
  :type 'string
  :group 'workset-notify)

(defcustom workset-notify-sound-command "afplay"
  "Shell command used to play a sound file on macOS.
The command is invoked as an asynchronous subprocess with the absolute
path to an AIFF file as its sole argument.  The default `afplay' is
available on all macOS systems.  Change this only if you need a
custom audio player."
  :type 'string
  :group 'workset-notify)

(defcustom workset-notify-sound-throttle-seconds 5
  "Minimum seconds between repeated sounds for the same state.
Prevents rapid repeated sound notifications."
  :type 'number
  :group 'workset-notify)

(defface workset-notify-needs-input-face
  '((t :weight bold :foreground "orange"))
  "Face for input-needed modeline indicator."
  :group 'workset-notify)

(defface workset-notify-done-face
  '((t :weight bold :foreground "green"))
  "Face for done modeline indicator."
  :group 'workset-notify)

(defface workset-notify-working-face
  '((t :foreground "deep sky blue"))
  "Face for working modeline indicator."
  :group 'workset-notify)

(defface workset-notify-idle-face
  '((t :foreground "gray50"))
  "Face for idle modeline indicator."
  :group 'workset-notify)

(defvar workset-notify--window-hook-installed nil
  "Non-nil when the global window-selection-change hook has been installed.")

(defvar-local workset-notify--state nil
  "Current notification state for this buffer.
One of nil, `working', `needs-input', `done', or `idle'.")
(defvar-local workset-notify--recent-output ""
  "Accumulated recent vterm output used for pattern matching.")
(defvar-local workset-notify--mode-line-cell nil
  "The modeline (:eval ...) cell added to `mode-line-format'.")
(defvar-local workset-notify--last-change-time 0
  "Float time of the last state change, used for debouncing.")
(defvar-local workset-notify--idle-timer nil
  "Timer that fires to set the buffer state to `idle'.")
(defvar-local workset-notify--last-sound-time nil
  "Alist mapping state symbol to the `float-time' when that sound was last played.")

(defun workset-notify--play-sound (sound-name state)
  "Play SOUND-NAME asynchronously for STATE if conditions allow.
SOUND-NAME is the base name of a file under /System/Library/Sounds/.
STATE is the notification state symbol used for throttle tracking.
Does nothing when `workset-notify-sound-enabled' is nil, when not on
macOS, or when the sound was played too recently (see
`workset-notify-sound-throttle-seconds')."
  (when (and workset-notify-sound-enabled
             (eq system-type 'darwin))
    (let* ((now (float-time))
           (last (cdr (assq state workset-notify--last-sound-time)))
           (throttle workset-notify-sound-throttle-seconds))
      (when (or (null last)
                (<= throttle 0)
                (>= (- now last) throttle))
        (let ((path (format "/System/Library/Sounds/%s.aiff" sound-name)))
          (if (file-exists-p path)
              (progn
                (setq workset-notify--last-sound-time
                      (cons (cons state now)
                            (assq-delete-all state workset-notify--last-sound-time)))
                (start-process "workset-notify-sound" nil
                               workset-notify-sound-command path))
            (message "workset-notify: sound file not found: %s" path)))))))

(defun workset-notify--matches-any (patterns text)
  "Return non-nil if any regex in PATTERNS matches TEXT."
  (seq-some (lambda (re) (string-match-p re text)) patterns))

(defun workset-notify--match-state (text)
  "Return state symbol for TEXT based on pattern matches."
  (cond
   ((workset-notify--matches-any workset-notify-input-patterns text) 'needs-input)
   ((workset-notify--matches-any workset-notify-done-patterns text) 'done)
   (t nil)))

(defun workset-notify--trim-output (text)
  "Trim TEXT to the last `workset-notify-max-output' chars."
  (let ((len (length text)))
    (if (<= len workset-notify-max-output)
        text
      (substring text (- len workset-notify-max-output)))))

(defun workset-notify--mode-line ()
  "Return modeline segment based on `workset-notify--state'."
  (pcase workset-notify--state
    ('needs-input (propertize " [Input]" 'face 'workset-notify-needs-input-face))
    ('done (propertize " [Done]" 'face 'workset-notify-done-face))
    ('working (propertize " [Working]" 'face 'workset-notify-working-face))
    ('idle (propertize " [Idle]" 'face 'workset-notify-idle-face))
    (_ "")))

(defun workset-notify--ensure-modeline ()
  "Ensure the modeline segment is present in the current buffer."
  (unless workset-notify--mode-line-cell
    (setq workset-notify--mode-line-cell '(:eval (workset-notify--mode-line))))
  (unless (member workset-notify--mode-line-cell mode-line-format)
    (setq-local mode-line-format
                (append mode-line-format (list workset-notify--mode-line-cell)))))

(defun workset-notify--buffer-visible-p ()
  "Return non-nil if the current buffer is visible in any window."
  (get-buffer-window (current-buffer)))

(defun workset-notify--emit (state)
  "Emit Emacs notifications for STATE if configured."
  (when (and workset-notify-enabled (memq state workset-notify-notify-states))
    (let ((label (pcase state
                   ('needs-input "needs input")
                   ('done "is done")
                   (_ nil))))
      (when label
        (pcase workset-notify-method
          ('modeline nil)
          ('modeline-and-message
           (message "Workset: %s %s" (buffer-name) label))
          ('modeline-and-warning
           (display-warning 'workset (format "Workset: %s %s" (buffer-name) label)
                            :warning))
          ('modeline-and-sound
           (when (not (workset-notify--buffer-visible-p))
             (let ((sound (pcase state
                            ('done workset-notify-sound-done)
                            ('needs-input workset-notify-sound-needs-input)
                            (_ nil))))
               (when sound
                 (workset-notify--play-sound sound state)))))
          ('sound
           (when (not (workset-notify--buffer-visible-p))
             (let ((sound (pcase state
                            ('done workset-notify-sound-done)
                            ('needs-input workset-notify-sound-needs-input)
                            (_ nil))))
               (when sound
                 (workset-notify--play-sound sound state)))))
          ('modeline-message-and-sound
           (message "Workset: %s %s" (buffer-name) label)
           (when (not (workset-notify--buffer-visible-p))
             (let ((sound (pcase state
                            ('done workset-notify-sound-done)
                            ('needs-input workset-notify-sound-needs-input)
                            (_ nil))))
               (when sound
                 (workset-notify--play-sound sound state))))))))))


(defun workset-notify--set-state (state)
  "Set notification STATE and update UI."
  (let ((now (float-time))
        (debounce workset-notify-debounce-seconds))
    (when (or (not (eq workset-notify--state state))
              (eq state 'needs-input))
      (when (or (eq state 'needs-input)
                (<= debounce 0)
                (>= (- now workset-notify--last-change-time) debounce))
        (setq workset-notify--state state)
        (setq workset-notify--last-change-time now)
        (force-mode-line-update)
        (workset-notify--emit state)))))

(defun workset-notify--schedule-idle ()
  "Schedule idle state update for the current buffer."
  (when workset-notify--idle-timer
    (cancel-timer workset-notify--idle-timer))
  (when (and workset-notify-idle-seconds
             (numberp workset-notify-idle-seconds)
             (> workset-notify-idle-seconds 0))
    (setq workset-notify--idle-timer
          (run-at-time workset-notify-idle-seconds nil
                       (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (when workset-notify-mode
                               (workset-notify--set-state 'idle)))))
                       (current-buffer)))))

(defun workset-notify--output-filter (output)
  "Filter vterm OUTPUT to detect agent state."
  (when workset-notify-mode
    (setq workset-notify--recent-output
          (workset-notify--trim-output
           (concat workset-notify--recent-output output)))
    (workset-notify--schedule-idle)
    (let ((matched (workset-notify--match-state workset-notify--recent-output)))
      (cond
       (matched (workset-notify--set-state matched))
       (workset-notify-working-patterns
        (when (workset-notify--matches-any workset-notify-working-patterns
                                           workset-notify--recent-output)
          (workset-notify--set-state 'working)))
       ((eq workset-notify--state 'done)
        (workset-notify--set-state 'working))
       ((and (not (eq workset-notify--state 'needs-input))
             (not (eq workset-notify--state 'working)))
        (workset-notify--set-state 'working)))))
  output)

(defun workset-notify--maybe-clear-on-command ()
  "Clear pending input state on user commands."
  (when (and (eq workset-notify--state 'needs-input)
             (memq this-command workset-notify-clear-commands))
    (workset-notify--set-state 'working)))

(defun workset-notify--window-selection-change (_window)
  "Clear pending input state when a notified buffer is focused."
  (let ((buf (window-buffer (selected-window))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and workset-notify-mode
                   (eq workset-notify--state 'needs-input))
          (workset-notify--set-state 'working))))))

(defun workset-notify--ensure-window-hook ()
  "Install the global window selection hook once."
  (unless workset-notify--window-hook-installed
    (add-hook 'window-selection-change-functions
              #'workset-notify--window-selection-change)
    (setq workset-notify--window-hook-installed t)))

;;;###autoload
(define-minor-mode workset-notify-mode
  "Minor mode to notify on agent output in vterm buffers."
  :init-value nil
  :lighter " WN"
  (if workset-notify-mode
      (progn
        (setq-local workset-notify--state nil)
        (setq-local workset-notify--recent-output "")
        (setq-local workset-notify--last-change-time 0)
        (setq-local workset-notify--last-sound-time nil)
        (workset-notify--ensure-modeline)
        (workset-notify--ensure-window-hook)
        (add-hook 'vterm-output-filter-functions #'workset-notify--output-filter nil t)
        (add-hook 'post-command-hook #'workset-notify--maybe-clear-on-command nil t))
    (remove-hook 'vterm-output-filter-functions #'workset-notify--output-filter t)
    (remove-hook 'post-command-hook #'workset-notify--maybe-clear-on-command t)
    (when workset-notify--idle-timer
      (cancel-timer workset-notify--idle-timer)
      (setq workset-notify--idle-timer nil))))

;;;###autoload
(defun workset-notify-set-state (state)
  "Manually set notification STATE in the current buffer."
  (interactive
   (list (intern (completing-read "State: "
                                  '(idle working needs-input done)
                                  nil t))))
  (workset-notify--set-state state))

;;;###autoload
(defun workset-notify-attach ()
  "Enable `workset-notify-mode' in the current buffer if appropriate."
  (when (and workset-notify-enabled (derived-mode-p 'vterm-mode))
    (workset-notify-mode 1)))

;;;###autoload
(defun workset-notify-use-preset (agent)
  "Merge patterns from AGENT preset into the current pattern variables.
AGENT must be a key in `workset-notify-agent-presets'.
Existing custom patterns are preserved; preset patterns are appended
if not already present."
  (interactive
   (list (intern (completing-read "Agent preset: "
                                  (mapcar #'car workset-notify-agent-presets)
                                  nil t))))
  (let ((preset (alist-get agent workset-notify-agent-presets)))
    (unless preset
      (user-error "Unknown agent preset: %s" agent))
    (let ((input-pats (plist-get preset :input))
          (done-pats (plist-get preset :done))
          (working-pats (plist-get preset :working)))
      (dolist (pat input-pats)
        (unless (member pat workset-notify-input-patterns)
          (setq workset-notify-input-patterns
                (append workset-notify-input-patterns (list pat)))))
      (dolist (pat done-pats)
        (unless (member pat workset-notify-done-patterns)
          (setq workset-notify-done-patterns
                (append workset-notify-done-patterns (list pat)))))
      (dolist (pat working-pats)
        (unless (member pat workset-notify-working-patterns)
          (setq workset-notify-working-patterns
                (append workset-notify-working-patterns (list pat)))))
      (message "workset-notify: loaded preset %s" agent))))

(provide 'workset-notify)
;;; workset-notify.el ends here
