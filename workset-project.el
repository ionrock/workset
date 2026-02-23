;;; workset-project.el --- Project backend for workset  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Eric

;; Author: Eric
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Project selection abstraction that dispatches to project.el or projectile
;; depending on user configuration and available packages.

;;; Code:

(require 'project)

(declare-function projectile-known-projects "projectile")
(declare-function projectile-project-root "projectile")

(defun workset-project--backend ()
  "Resolve the effective project backend.
Returns `project' or `projectile' based on `workset-project-backend'."
  (pcase workset-project-backend
    ('project 'project)
    ('projectile 'projectile)
    ('auto (if (featurep 'projectile) 'projectile 'project))
    (_ (error "Unknown workset-project-backend: %s" workset-project-backend))))

(defun workset-project-select ()
  "Prompt the user to select a project root directory.
Returns the absolute path to the selected project root."
  (pcase (workset-project--backend)
    ('project (workset-project--select-project-el))
    ('projectile (workset-project--select-projectile))))

(defun workset-project--select-project-el ()
  "Select a project using project.el.
Returns the project root directory."
  (project-root (project-current t)))

(defun workset-project--select-projectile ()
  "Select a project using projectile.
Returns the project root directory."
  (unless (featurep 'projectile)
    (error "Projectile is not loaded but workset-project-backend is set to projectile"))
  (let ((projects (projectile-known-projects)))
    (unless projects
      (error "No known projectile projects"))
    (completing-read "Project: " projects nil t)))

(provide 'workset-project)
;;; workset-project.el ends here
