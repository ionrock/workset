;;; workset-vterm.el --- Terminal management for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Numbered vterm buffer creation and management for workset.
;; Each workset can have multiple numbered terminals.

;;; Code:

(require 'subr-x)

(declare-function vterm-mode "vterm")

(defun workset-vterm--format-buffer-name (format-string repo task index)
  "Format a vterm buffer name from FORMAT-STRING.
Substitutes %r with REPO, %t with TASK, and %n with INDEX."
  (let ((name format-string))
    (setq name (string-replace "%r" repo name))
    (setq name (string-replace "%t" task name))
    (setq name (string-replace "%n" (number-to-string index) name))
    name))

(defun workset-vterm--next-index (format-string repo task)
  "Find the first unused buffer index for REPO/TASK.
Starts at 1 and finds the first index where no live buffer exists."
  (let ((index 1))
    (while (get-buffer (workset-vterm--format-buffer-name format-string repo task index))
      (setq index (1+ index)))
    index))

(defun workset-vterm-create (directory format-string repo task)
  "Create a new vterm buffer in DIRECTORY.
FORMAT-STRING, REPO, and TASK are used for buffer naming.
Returns the created buffer."
  (unless (require 'vterm nil t)
    (error "vterm is not installed"))
  (let* ((index (workset-vterm--next-index format-string repo task))
         (buf-name (workset-vterm--format-buffer-name format-string repo task index))
         (default-directory directory)
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    (let ((proc (get-buffer-process buf)))
      (when proc
        (set-process-sentinel
         proc
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (let ((pbuf (process-buffer process)))
               (when (buffer-live-p pbuf)
                 (kill-buffer pbuf))))))))
    (pop-to-buffer-same-window buf)
    buf))

(defun workset-vterm-list (format-string repo task)
  "Return live vterm buffers for REPO/TASK.
Checks indices starting at 1 until a gap of 100 unused indices is found."
  (let ((index 1)
        (gap 0)
        (buffers nil))
    (while (< gap 100)
      (let ((buf (get-buffer
                  (workset-vterm--format-buffer-name format-string repo task index))))
        (if (and buf (buffer-live-p buf))
            (progn
              (push buf buffers)
              (setq gap 0))
          (setq gap (1+ gap))))
      (setq index (1+ index)))
    (nreverse buffers)))

(provide 'workset-vterm)
;;; workset-vterm.el ends here
