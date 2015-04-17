;;; ergoemacs-unbind.el --- unbind keys -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Keywords: convenience

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library define keys that we want to set/unset because they are
;; already defined by ergoemacs minor mode

;; Todo:

;; 

;;; Code:
(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(require 'edmacro)

(defvar ergoemacs-single-command-keys)
(defvar ergoemacs-shortcut-keymap)
(defvar ergoemacs-no-shortcut-keymap)
(defvar ergoemacs-keymap)
(defvar keyfreq-mode)
(defvar keyfreq-table)
(defvar ergoemacs-describe-key)
(declare-function ergoemacs-debug "ergoemacs-mode.el")
(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
(defun ergoemacs-undefined ()
  "Ergoemacs Undefined key, echo new key for old action."
  (interactive)
  (let* ((key-kbd (this-single-command-keys))
         tmp
         (local-fn nil))
    ;; Lookup local key, if present and then issue that
    ;; command instead...
    ;;
    ;; This way the unbound keys are just above the global
    ;; map and doesn't actually change it.
    (cond
     ((progn
        ;; See if this is present in the `ergoemacs-shortcut-keymap'
        (setq local-fn (lookup-key ergoemacs-shortcut-keymap key-kbd))
        (unless (functionp local-fn)
          ;; Lookup in ergoemacs-keymap
          (setq local-fn (lookup-key ergoemacs-keymap key-kbd)))
        (functionp local-fn))
      (ergoemacs-debug "WARNING: The command %s is undefined when if shouldn't be..." local-fn)
      (setq tmp (ergoemacs-real-key-binding key-kbd))
      (when (and tmp (not (equal tmp 'ergoemacs-undefined)))
        (setq local-fn tmp))
      (when (featurep 'keyfreq)
        (when keyfreq-mode
          (let ((command 'ergoemacs-undefined) count)
            (setq count (gethash (cons major-mode command) keyfreq-table))
            (cond
             ((not count))
             ((= count 1)
              (remhash (cons major-mode command) keyfreq-table))
             (count
              (puthash (cons major-mode command) (- count 1)
                       keyfreq-table)))
            ;; Add local-fn to counter.
            (setq command local-fn)
            (setq count (gethash (cons major-mode command) keyfreq-table))
            (puthash (cons major-mode command) (if count (+ count 1) 1)
                     keyfreq-table))))
      (setq this-command local-fn)
      (condition-case err
          (call-interactively local-fn)
        (error (beep) (message "%s" err))))
     ((progn
        ;; Local map present.  Use it, if there is a key
        ;; defined there.
        (setq local-fn (get-char-property (point) 'local-map))
        (if (and local-fn
                 (ignore-errors (keymapp local-fn)))
            (setq local-fn (lookup-key local-fn key-kbd))
          (if (current-local-map)
              (setq local-fn (lookup-key (current-local-map) key-kbd))
            (setq local-fn nil)))
        (functionp local-fn))
      (setq this-command local-fn) ; Don't record this command.
      (when (featurep 'keyfreq)
        (when keyfreq-mode
          (let ((command 'ergoemacs-undefined) count)
            (setq count (gethash (cons major-mode command) keyfreq-table))
            (cond
             ((not count))
             ((= count 1)
              (remhash (cons major-mode command) keyfreq-table))
             (count
              (puthash (cons major-mode command) (- count 1)
                       keyfreq-table)))
            ;; Add local-fn to counter.
            (setq command local-fn)
            (setq count (gethash (cons major-mode command) keyfreq-table))
            (puthash (cons major-mode command) (if count (+ count 1) 1)
                     keyfreq-table))))
      (condition-case err
          (call-interactively local-fn)
        (error (beep) (message "%s" err))))
     (t
      ;; Not locally defined, complain.
      (beep)
      (ergoemacs-where-is-old-binding key-kbd))))
  (setq ergoemacs-describe-key nil))

;;;###autoload
(defun ergoemacs-translate-current-key (key)
  "Translate the current KEY."
  (cond
   ((string= (key-description key) "<backspace>")
    (read-kbd-macro "DEL" t))
   (t key)))

(defun ergoemacs-translate-current-function (curr-fn)
  "Translate the current function CURR-FN."
  (cond
   ((eq 'delete-horizontal-space curr-fn)
    'ergoemacs-shrink-whitespaces)
   ((eq 'left-char curr-fn) 'backward-char)
   ((eq 'right-char curr-fn) 'forward-char)
   (t curr-fn)))

;; Based on describe-key-briefly
(declare-function ergoemacs-key-fn-lookup "ergoemacs-translate.el")
(declare-function ergoemacs-describe-key-kbd "ergoemacs-translate.el")
(defvar yank-menu)
(defun ergoemacs-where-is-old-binding (&optional key only-new-key)
  "Print the name of the function KEY invoked before to start ErgoEmacs minor mode."
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
	 (cursor-in-echo-area t)
	 saved-yank-menu)
     (unwind-protect
	 (let (key)
	   ;; If yank-menu is empty, populate it temporarily, so that
	   ;; "Select and Paste" menu can generate a complete event.
	   (when (null (cdr yank-menu))
	     (setq saved-yank-menu (copy-sequence yank-menu))
	     (menu-bar-update-yank-menu "(any string)" nil))
	   (setq key (read-key-sequence "Describe old key (or click or menu item): "))
	   ;; If KEY is a down-event, read and discard the
	   ;; corresponding up-event.  Note that there are also
	   ;; down-events on scroll bars and mode lines: the actual
	   ;; event then is in the second element of the vector.
	   (and (vectorp key)
		(let ((last-idx (1- (length key))))
		  (and (eventp (aref key last-idx))
		       (memq 'down (event-modifiers (aref key last-idx)))))
		(read-event))
	   (list key))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
	 (setq yank-menu (copy-sequence saved-yank-menu))
	 (fset 'yank-menu (cons 'keymap yank-menu))))))
  
  (let* ((old-cmd (lookup-key (current-global-map) key))
         message-log-max
         (key-desc (key-description key))
         (new-key (key-description (ergoemacs-key-fn-lookup old-cmd))))
    (unless old-cmd
      (setq old-cmd (lookup-key
                     (current-global-map)
                     (ergoemacs-translate-current-key key)))
      (setq new-key (key-description (ergoemacs-key-fn-lookup old-cmd))))
    (cond
     ((and new-key only-new-key)
      (read-kbd-macro new-key t))
     (only-new-key
      nil)
     ((and old-cmd new-key)
      (message "%s keybinding%s%s (%s)"
               (ergoemacs-describe-key-kbd key-desc)
               (if (called-interactively-p  'any)
                   " is changed to "
                 " is disabled! Use ")
               (ergoemacs-describe-key-kbd new-key)
               old-cmd))
     (old-cmd
      (message "Key %s was bound to `%s' which is not bound any longer"
               (ergoemacs-describe-key-kbd key-desc)
               old-cmd))
     (t
      (message "Key %s was not bound to any command (%s)"
               (ergoemacs-describe-key-kbd key-desc)
               old-cmd)))))

(provide 'ergoemacs-unbind)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-unbind.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
