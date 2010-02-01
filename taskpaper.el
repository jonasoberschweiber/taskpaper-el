;;; taskpaper.el --- Taskpaper implementation for Emacs

;; Copyright (C) 2008 Kentaro Kuribayashi

;; Author: kentaro <kentarok@gmail.com>
;; Keywords: tools, task

;; Modified Feb 2010 by Jonas Oberschweiber <jonas@oberschweiber.com>
;; Changed handling of "done" tasks: uses TaskPaper's @done notation
;; instead of +/- at the beginning of the line
;; Changed the indentation function to automatically indent tasks that
;; appear below projects (only one level supported).
;; I don't know if the other functions work (didn't test them as I
;; don't use them (yet)).

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Install
;;
;; After download this file, just put the code below into your .emacs
;;
;;   (require 'taskpaper)
;;
;; * Usage
;;
;; (1) Create a Taskpaper file
;;
;;   M-x find-file RET 2008-02-18.taskpaper
;;
;; (2) Create a new project
;;
;;   `C-c C-p' or just write as follows:
;;
;;   Project 1:
;;
;; (2) List tasks as follows:
;;
;;   `C-c C-t' or just write as follows:
;;
;;   Project 1:
;;
;;   + task 1
;;   + task 2
;;
;;   Project 2:
;;
;;   + task 3
;;   + task 4
;;
;; (3) Mark task as done
;;
;;   `C-c C-d' on the task you have done.
;;

;;; TODO:

;;   + Norrowing by project and tag with simple interaction

;;; Code:

;; Hook
(defvar taskpaper-mode-hook nil
  "*Hooks for Taskpaper major mode")

;; Keymap
(defvar taskpaper-mode-map (make-keymap)
  "*Keymap for Taskpaper major mode")

(defvar taskpaper-indent-amount 4)

(define-key taskpaper-mode-map "\C-c\C-p" 'taskpaper-create-new-project)
(define-key taskpaper-mode-map "\C-c\C-t" 'taskpaper-create-new-task)
(define-key taskpaper-mode-map "\C-c\C-d" 'taskpaper-toggle-task)
(define-key taskpaper-mode-map "+"        'taskpaper-electric-mark)
(define-key taskpaper-mode-map "-"        'taskpaper-electric-mark)

;; Face
(defface taskpaper-project-face
  '((((class color) (background light))
     (:foreground "white" :underline "red" :weight bold))
    (((class color) (background dark))
     (:foreground "white" :underline "red" :weight bold)))
  "Face definition for project name")

(defface taskpaper-task-face
  '((((class color) (background light))
     (:foreground "yellow"))
    (((class color) (background dark))
     (:foreground "yellow")))
  "Face definition for task")

(defface taskpaper-task-marked-as-done-face
  '((((class color) (background light))
     (:foreground "dim grey" :weight light :strike-through t))
    (((class color) (background dark))
     (:foreground "dim grey" :weight light :strike-through t)))
  "Face definition for task marked as done")

(defface taskpaper-done-mark-face
  '((((class color) (background light))
     (:foreground "dim grey"))
    (((class color) (background dark))
     (:foreground "dim grey")))
  "Face definition for done mark")

(defface taskpaper-undone-mark-face
  '((((class color) (background light))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "blue")))
  "Face definition for undone mark")

(defvar taskpaper-project-face 'taskpaper-project-face)
(defvar taskpaper-task-face 'taskpaper-task-face)
(defvar taskpaper-task-marked-as-done-face 'taskpaper-task-marked-as-done-face)
(defvar taskpaper-done-mark-face 'taskpaper-done-mark-face)
(defvar taskpaper-undone-mark-face 'taskpaper-undone-mark-face)
(defvar taskpaper-font-lock-keywords
  '(("^.+:[ \t]*$" 0 taskpaper-project-face)
    ("^[ \t]*\\(-\\)\\(.*\\)\\@done\\(.*\\)$"
     (1 taskpaper-done-mark-face t)
     (2 taskpaper-task-marked-as-done-face))
    ("^[ \t]*\\(-\\)\\(.*\\)$"
     (1 taskpaper-undone-mark-face t)
     (2 taskpaper-task-face))))

;; Taskpaper major mode
(define-derived-mode taskpaper-mode fundamental-mode "Taskpaper"
  "Major mode to manage tasks easily"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'taskpaper-mode)
  (setq mode-name "Taskpaper")
  (use-local-map taskpaper-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(taskpaper-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'taskpaper-indent-line)
  (run-hooks 'taskpaper-mode-hook))

(add-to-list 'auto-mode-alist (cons "\\.taskpaper$" 'taskpaper-mode))

;; Commands
(defun taskpaper-create-new-project (name)
  "Creates new project"
  (interactive "sProject Name: ")
  (insert (concat name ":\n\n")))

(defun taskpaper-create-new-task (task)
  "Creates new task"
  (interactive "sNew Task: ")
  (insert (concat "+ " task))
  (newline-and-indent))

;; I don't use it, so I didn't touch it.
(defun taskpaper-toggle-task ()
  "Marks task as done"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[-+]")
      (let ((mark (if (equal (match-string 0) "+") "-" "+")))
        (delete-char 1)
        (insert mark)))))

(defun taskpaper-indent-line ()
  "Detects if list mark is needed when indented"
  (interactive)
  (let ((mark-flag nil)
        (in-project nil)
        (old-tabs indent-tabs-mode))
    ;; TaskPaper won't recognize the indents otherwise.
    (setq indent-tabs-mode t)
    (save-excursion
      (while (and (not in-project) (not (bobp)))
        (forward-line -1)
        (when (looking-at "^.+:[ \t]*$") (setq in-project t))
        (when (looking-at "[+-]") (setq mark-flag t))))
    (when mark-flag (insert "- "))    
    (when in-project (indent-line-to taskpaper-indent-amount))
    (setq indent-tabs-mode old-tabs)))

(defun taskpaper-electric-mark (arg)
  "Inserts a list mark"
  (interactive "*p")
  (if (zerop (current-column))
      (progn
        (taskpaper-indent-line)
        (self-insert-command arg)      
        (insert " "))
    (self-insert-command arg)))

(provide 'taskpaper)
;;; taskpaper.el ends here
