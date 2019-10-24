;;; shell-synopsis.el --- Provide synopsis information for command with eldoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: xFA25E
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show shell command synopsis in minibuffer with eldoc.

;; To enable add shell-synopsis-setup to shell-mode-hook

;;; Code:

(require 'man)
(require 'subr-x)
(require 'comint)

(defun shell-synopsis-current-command ()
  "Return the command on current line."
  (let ((line-start (save-excursion (comint-bol) (point)))
        (line-end   (save-excursion (move-end-of-line nil) (point))))
    (when (<= line-start (point))
      (save-excursion
        (if (re-search-backward "|" line-start t)
            (forward-char)
          (comint-bol))
        (when-let ((match-point (re-search-forward "[^\s]+" line-end t)))
          (goto-char match-point)
          (current-word))))))

(defun shell-synopsis-command-exists-p (command)
  "Check whether the `COMMAND' exists."
  (not
   (string-empty-p
    (shell-command-to-string
     (concat "command -v " command)))))

(defvar shell-synopsis-man-cache (make-hash-table :test #'equal)
  "Cache of command synopses.")

(defun shell-synopsis-clear-man-cache ()
  "Clear the `shell-synopsis-man-cache'."
  (interactive)
  (setq shell-synopsis-man-cache (make-hash-table :test #'equal)))

(defun shell-synopsis-man (command)
  "Return man page for `COMMAND'."
  (let ((lang (getenv "LANG")))
    (prog2 (setenv "LANG" "C")
        (shell-command-to-string
         (format "%s %s | col -b" manual-program command))
      (setenv "LANG" lang))))

(defun shell-synopsis-parse-synopsis (manual-page)
  "Parse synopsis from `MANUAL-PAGE'."
  (cl-loop for skip-line on (split-string manual-page "\n")
           if (string-match-p "^SYNOPSIS$" (car skip-line))
           return (cl-loop for line in (cdr skip-line)
                           if (not (string-match-p "^[[:alnum:]]+" line))
                           concat (concat (string-trim line) "\n") into result
                           else
                           return (string-trim-right result)
                           finally
                           return (string-trim-right result))))

(defun shell-synopsis-make-synopsis (command)
  "Make synopsis for `COMMAND'."
  (puthash command
           (shell-synopsis-parse-synopsis (shell-synopsis-man command))
           shell-synopsis-man-cache))

(defun shell-synopsis-get-command-synopsis (command)
  "Get synopsis for `COMMAND'."
  (let ((synopsis (gethash command shell-synopsis-man-cache 'not-found)))
    (if (not (eql synopsis 'not-found))
        synopsis
      (shell-synopsis-make-synopsis command))))

(defun shell-synopsis-eldoc-command ()
  "Return eldoc string for the pointed symbol."
  (when-let ((command (shell-synopsis-current-command)))
    (cond
     ((string-match-p "^[/.]" command) nil)
     ((string-match-p "^\\*." command)
      (shell-synopsis-get-command-synopsis (substring command 1)))
     ((shell-synopsis-command-exists-p command)
      (shell-synopsis-get-command-synopsis command)))))

;;;###autoload
(defun shell-synopsis-setup ()
  "Setup eldoc function for Shell."
  (interactive)
  (set (make-local-variable 'eldoc-documentation-function)
       #'shell-synopsis-eldoc-command))

(provide 'shell-synopsis)
;;; shell-synopsis.el ends here
