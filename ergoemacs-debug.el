;;; ergoemacs-debug.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-debug.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;; Commentary:
;;
;; Debugging messages for ergoemacs-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
;; (require 'guide-key nil t)
(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(defvar ergoemacs-debug ""
  "Debugging for `ergoemacs-mode'.")

(defvar ergoemacs-debug-buffer " *ErgoEmacs-mode Debug Log*"
  "Variable for ergoemacs debugging buffer.")

(defvar ergoemacs-debug-keymap--temp-map nil)
(defun ergoemacs-debug-keymap (keymap)
  "Print keymap bindings."
  (if (not (ignore-errors (symbolp (symbol-name keymap))))
      (progn
        (setq ergoemacs-debug-keymap--temp-map keymap)
        (ergoemacs-debug "%s" (substitute-command-keys "\\{ergoemacs-debug-keymap--temp-map}")))
    (ergoemacs-debug-heading "%s"
                             (format "Keymap Description: %s" (symbol-name keymap)))
    (ergoemacs-debug "%s" (substitute-command-keys (format "\\{%s}" (symbol-name keymap))))))

(defvar ergoemacs-debug-heading-start-time (float-time))
(defvar ergoemacs-debug-heading-last-time (float-time))

(defun ergoemacs-debug-heading (&rest arg)
  "Ergoemacs debugging heading."
  (ergoemacs-debug (concat "** "
                           (condition-case err
                               (apply 'format arg)
                             (error (format "Bad format string: %s (%s)" arg err)))))
  (ergoemacs-debug "Time Since Start ergoemacs-mode: %1f sec" (- (float-time) ergoemacs-debug-heading-start-time))
  (ergoemacs-debug "Time Since Last Heading: %1f sec" (- (float-time) ergoemacs-debug-heading-last-time))
  (setq ergoemacs-debug-heading-last-time (float-time)))

(defun ergoemacs-debug (&rest arg)
  "Ergoemacs debugging facility."
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (ergoemacs-debug-flush)
        (switch-to-buffer-other-window (get-buffer-create ergoemacs-debug-buffer))
        (setq ergoemacs-debug-buffer (replace-regexp-in-string "^ +" "" ergoemacs-debug-buffer))
        (rename-buffer ergoemacs-debug-buffer)
        (unless (eq major-mode 'org-mode)
          (call-interactively 'org-mode)))
    (setq ergoemacs-debug
          (format "%s\n%s"
                  ergoemacs-debug
                  (condition-case err
                      (apply 'format arg)
                    (error (format "Bad Format String: %s (%s)" arg err)))))))

(defun ergoemacs-debug-clear ()
  "Clears the variable `ergoemacs-debug' and `ergoemacs-debug-buffer'"
  (setq ergoemacs-debug "")
  (save-excursion
    (with-current-buffer (get-buffer-create ergoemacs-debug-buffer) 
      (delete-region (point-min) (point-max)))))

(defun ergoemacs-debug-flush ()
  "Flushes ergoemacs debug to `ergoemacs-debug-buffer'"
  (save-excursion
    (with-current-buffer (get-buffer-create ergoemacs-debug-buffer) 
      (goto-char (point-max))
      (unless (looking-back "\n" nil)
        (insert "\n"))
      (insert ergoemacs-debug)
      (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point))))
  (setq ergoemacs-debug ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-debug.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
