;;; auto-complete-ya-gtags.el --- a source for auto-complete

;; Copyright (C) 2011  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: auto-complete
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(defvar ac-ya-gtags-dbpath nil
  "A pathname to the currently using GTAGS.
nil means no GTAGS found.")

(defvar ac-ya-gtags-completion-table nil
  "A completion table for the currently using GTAGS.")

(defface ac-ya-gtags-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for ya-gtags candidates."
  :group 'auto-complete)

(defface ac-ya-gtags-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the ya-gtags selected candidate."
  :group 'auto-complete)

(defun ac-ya-gtags-require-update ()
  "Return t if `ac-ya-gtags-completion-table' needs to be updated."
  (let ((path (car (split-string (shell-command-to-string "global -p") "\n"))))
    (cond
     ((string= path "global: GTAGS not found.")
      ;; We need to disable ac-source-ya-gtags.
      (setq ac-ya-gtags-dbpath nil)
      (setq ac-ya-gtags-completion-table nil)
      nil)
     ((or (not ac-ya-gtags-dbpath) (not (string= path ac-ya-gtags-dbpath)))
      ;; This is a new GTAGS
      (setq ac-ya-gtags-dbpath path)
      t)
     (t nil))))

(defun ac-ya-gtags-init ()
  "Check and update completeion table.

If GTAGS is not found in the curernt directory hierarchy, then we
set both `ac-ya-gtags-dbpath' and `ac-ya-gtags-completion-table'
to nil. If it's found and it's the new one, we need to create a
new completion table. If it is the same one as previous, we do
nothing."
  (when (ac-ya-gtags-require-update)
    ;; We have to create a new completion table.
    (setq ac-ya-gtags-completion-table
          (split-string (shell-command-to-string (format "global -c")) "\n"))))

(defun ac-ya-gtags-candidate ()
  (ac-ya-gtags-init)
  (when ac-ya-gtags-dbpath
    (all-completions ac-target ac-ya-gtags-completion-table)))

(ac-define-source ya-gtags
  '((candidates . ac-ya-gtags-candidate)
    (candidate-face . ac-ya-gtags-candidate-face)
    (selection-face . ac-ya-gtags-selection-face)
    (requires . 3)
    (symbol . "s")))
