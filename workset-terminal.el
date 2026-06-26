;;; workset-terminal.el --- Terminal management for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Numbered terminal buffer creation and management for workset.
;; Each workset can have multiple numbered terminals.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function vterm-mode "vterm")
(declare-function ghostel "ghostel")
(declare-function workset-notify-attach "workset-notify")

(defvar workset-terminal-backend)
(defvar ghostel-buffer-name)
(defvar ghostel-buffer-name-function)
(defvar ghostel--buffer-identity)

(defun workset-vterm--format-buffer-name (format-string repo task index)
  "Format a terminal buffer name from FORMAT-STRING.
Substitutes %r with REPO, %t with TASK, and %n with INDEX."
  (let ((name format-string))
    (setq name (string-replace "%r" repo name))
    (setq name (string-replace "%t" task name))
    (setq name (string-replace "%n" (number-to-string index) name))
    name))

(defun workset-vterm--buffer-matches-name-p (buffer name)
  "Return non-nil when BUFFER is the workset terminal named NAME."
  (or (equal (buffer-name buffer) name)
      (and (boundp 'ghostel--buffer-identity)
           (buffer-local-boundp 'ghostel--buffer-identity buffer)
           (equal (buffer-local-value 'ghostel--buffer-identity buffer) name))))

(defun workset-vterm--get-buffer (name)
  "Return the live terminal buffer named NAME, or nil.
Also matches Ghostel buffers by their creation-time identity, because
Ghostel can rename buffers after shell title or directory changes."
  (or (get-buffer name)
      (seq-find (lambda (buffer)
                  (and (buffer-live-p buffer)
                       (workset-vterm--buffer-matches-name-p buffer name)))
                (buffer-list))))

(defun workset-vterm--next-index (format-string repo task)
  "Find the first unused buffer index for REPO/TASK.
Starts at 1 and finds the first index where no live buffer exists."
  (let ((index 1))
    (while (workset-vterm--get-buffer
            (workset-vterm--format-buffer-name format-string repo task index))
      (setq index (1+ index)))
    index))

(defun workset-vterm--create-vterm (directory buf-name)
  "Create a vterm buffer named BUF-NAME in DIRECTORY."
  (unless (require 'vterm nil t)
    (error "vterm is not installed"))
  (let ((default-directory directory)
        (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (when (fboundp 'workset-notify-attach)
        (workset-notify-attach)))
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

(defun workset-vterm--create-ghostel (directory buf-name)
  "Create a Ghostel buffer named BUF-NAME in DIRECTORY."
  (unless (require 'ghostel nil t)
    (error "ghostel is not installed"))
  (let ((default-directory directory)
        (ghostel-buffer-name buf-name)
        ;; Workset owns the stable buffer naming scheme; Ghostel's default
        ;; title/directory tracking would otherwise rename these buffers.
        (ghostel-buffer-name-function nil))
    (let ((buf (ghostel)))
      (with-current-buffer buf
        (setq-local ghostel-buffer-name-function nil))
      buf)))

(defun workset-vterm-create (directory format-string repo task)
  "Create a new terminal buffer in DIRECTORY.
FORMAT-STRING, REPO, and TASK are used for buffer naming.
Returns the created buffer."
  (let* ((index (workset-vterm--next-index format-string repo task))
         (buf-name (workset-vterm--format-buffer-name format-string repo task index)))
    (pcase workset-terminal-backend
      ('vterm (workset-vterm--create-vterm directory buf-name))
      ('ghostel (workset-vterm--create-ghostel directory buf-name))
      (_ (error "Unsupported workset terminal backend: %S" workset-terminal-backend)))))

(defun workset-vterm-list (format-string repo task)
  "Return live terminal buffers for REPO/TASK.
Checks indices starting at 1 until a gap of 100 unused indices is found."
  (let ((index 1)
        (gap 0)
        (buffers nil))
    (while (< gap 100)
      (let ((buf (workset-vterm--get-buffer
                  (workset-vterm--format-buffer-name format-string repo task index))))
        (if (and buf (buffer-live-p buf))
            (progn
              (push buf buffers)
              (setq gap 0))
          (setq gap (1+ gap))))
      (setq index (1+ index)))
    (nreverse buffers)))

(provide 'workset-terminal)
(provide 'workset-vterm) ; Backward compatibility for existing require forms.
;;; workset-terminal.el ends here
