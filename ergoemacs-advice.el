;;; ergoemacs-advice.el --- Ergoemacs advices -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-advice.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;; Commentary:
;;  Advices for `ergoemacs-mode'.
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
(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(require 'mouse)

(defvar ergoemacs-mode)
(defvar ergoemacs-keymap)
(defvar ergoemacs-map--unbound-keys)
(defvar ergoemacs-user-keymap)

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-properties--hook-define-key "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--installed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original-user "ergoemacs-map-properties")

(declare-function ergoemacs-key-description--substitute-command-keys "ergoemacs-key-description")

(declare-function ergoemacs-translate--define-key "ergoemacs-translate")
(declare-function ergoemacs-translate--apply-key "ergoemacs-translate")
(declare-function ergoemacs-major-mode-menu-map "ergoemacs-lib")
(declare-function ergoemacs-translate--get "ergoemacs-translate")
(declare-function ergoemacs-translate--keymap "ergoemacs-translate")
(declare-function ergoemacs-command-loop--modal-p "ergoemacs-command-loop")
(declare-function ergoemacs-translation-struct-keymap-modal "ergoemacs-translate")
(declare-function ergoemacs-command-loop--internal "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--temp-message "ergoemacs-command-loop")
(declare-function ergoemacs-key-description "ergoemacs-key-description")

(defvar ergoemacs-advice--temp-replace-functions nil
  "List of `ergoemacs-mode' temporary replacement functions.

These replacement functions are are turned on when
`ergoemacs-mode' is turned on.")

(defvar ergoemacs-advice--permanent-replace-functions nil
  "List of `ergoemacs-mode' permanent replacement functions.
 
These replacement functinos are turned on after `ergoemacs-mode'
is loaded, but not turned off.")

(defun ergoemacs-advice--enable-replacement (ad &optional disable)
  "Enable ergoemacs-c advice AD (or optionally DISABLE)."
  (cond
   (disable
    (when (fboundp (intern (concat "ergoemacs-advice--real-" (symbol-name ad))))
      (defalias ad (intern (concat "ergoemacs-advice--real-" (symbol-name ad)))
        (documentation (intern (concat "ergoemacs-advice--real-" (symbol-name ad)))))))
   (t
    (when (fboundp (intern (concat "ergoemacs-advice--" (symbol-name ad))))
      (defalias ad (intern (concat "ergoemacs-advice--" (symbol-name ad)))
        (documentation (intern (concat "ergoemacs-advice--" (symbol-name ad)))))))))

(defun ergoemacs-advice--enable-replacements (&optional disable permanent)
  "Enable the function replacements.

When DISABLE is non-nil, disable the replacements.

When PERMANENT is non-nil, these replacements are permanent, not temporary."
  (dolist (ad (or (and permanent ergoemacs-advice--permanent-replace-functions)
                  ergoemacs-advice--temp-replace-functions))
    (ergoemacs-advice--enable-replacement ad disable)))

(add-hook 'ergoemacs-mode-startup-hook 'ergoemacs-advice--enable-replacements)

(defun ergoemacs-advice--disable-replacements ()
  "Disable the function replacements."
  (ergoemacs-advice--enable-replacements t))

(add-hook 'ergoemacs-mode-shutdown-hook 'ergoemacs-advice--disable-replacements)

(defun ergoemacs-advice--enable-permanent-replacements ()
  "Enable permanent replacements."
  (ergoemacs-advice--enable-replacements nil t))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-advice--enable-permanent-replacements)

(defvar ergoemacs--original-local-map nil
  "Original keymap used with `use-local-map'.")

;; FIXME for emacs 25
(ergoemacs-advice substitute-command-keys (string)
  "Use `ergoemacs-substitute-command-keys' when `ergoemacs-mode' is enabled"
  :type :replace
  (if ergoemacs-mode
      (ergoemacs-key-description--substitute-command-keys string)
    (ergoemacs-advice--real-substitute-command-keys string)))


(defun ergoemacs-mode--undefined-advice (&optional type)
  "Advice for undefined.

TYPE is the type of translation installed."
  (let* ((keys (this-single-command-keys))
	     (type (or type :normal))
	     (translation (ergoemacs-translate--get type))
	     (local-keymap (ergoemacs-translate--keymap translation))
	     (local-key (substring keys -1))
	     modal-p)
    (when (setq modal-p (ergoemacs :modal-p))
      (setq local-keymap (ergoemacs-translation-struct-keymap-modal modal-p)))
    ;; This starts the command loop when DEL or MENU is replaced in the proper place.
    (if (lookup-key local-keymap local-key)
	    (let ((i 1)) ;; Setup history
	      (setq ergoemacs-command-loop--history nil)
	      (while (<= i (- (length keys) 1))
	        (push (list (substring keys 0 i) :normal nil
			            current-prefix-arg (aref (substring keys (- i 1) i) 0))
		          ergoemacs-command-loop--history)
	        (setq i (+ 1 i)))
	      (ergoemacs-command-loop--internal keys nil nil nil ergoemacs-command-loop--history))
      (ding)
      (ergoemacs-command-loop--temp-message "%s does not do anything!"
                                            (ergoemacs-key-description (this-single-command-keys)))
      (setq defining-kbd-macro nil)
      (force-mode-line-update)
      ;; If this is a down-mouse event, don't reset prefix-arg;
      ;; pass it to the command run by the up event.
      (setq prefix-arg
            (when (memq 'down (event-modifiers last-command-event))
              current-prefix-arg)))))

(ergoemacs-advice undefined ()
  "Allow `ergoemacs-mode' to display keys, and intercept ending <apps> keys."
  :type :around
  (if (not ergoemacs-mode)
      ad-do-it
    (ergoemacs-mode--undefined-advice)))
 
(ergoemacs-advice handle-shift-selection ()
  "Allow `ergoemacs-mode' to do shift selection on keys like Alt+# to Alt+3."
  :type :before
  (when (eq 'ergoemacs-command-loop--shift-translate (key-binding (this-single-command-keys)))
    (setq this-command-keys-shift-translated t)))

(ergoemacs-advice read-key (&optional prompt)
  "Drop single command keys for read-key." ; For compataiblity with emacs 25.5
  :type :before
  (setq ergoemacs-command-loop--single-command-keys nil))


(provide 'ergoemacs-advice)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advice.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
