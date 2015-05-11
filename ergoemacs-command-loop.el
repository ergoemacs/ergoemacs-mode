;;; ergoemacs-command-loop.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-command-loop.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:08:09 2013 (-0500)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
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

(defun ergoemacs-command-loop--redefine-quit-key (&optional key)
  "Redefines the quit-key in emacs.

Typically the emacs quit key is Ctrl+g, but it can be redefined
with this function."
  (let ((cur-input (current-input-mode))
        (new-key (listify-key-sequence (or key [7]))))
    (when (> 1 (length new-key))
      (error "Will not set a key sequence to the emacs key sequence"))
    (setq cur-input (reverse (append new-key (cdr (reverse cur-input)))))
    (apply 'set-input-mode cur-input)))

(defun ergoemacs-command-loop--universal-argument (&optional type)
  "Ergoemacs universal argument.
This is called through `ergoemacs-command-loop'"
  (interactive)
  (setq current-prefix-arg '(4))
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-universal-argument 'ergoemacs-command-loop--universal-argument)


(defun ergoemacs-command-loop--digit-argument (&optional type)
  "Ergoemacs digit argument.
 This is called through `ergoemacs-command-loop'"
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq current-prefix-arg digit))
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-digit-argument 'ergoemacs-command-loop--digit-argument)

(defun ergoemacs-command-loop--negative-argument (&optional type)
  "Ergoemacs negative argument.
This is called through `ergoemacs-command-loop'"
  (setq current-prefix-arg '-)
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-negative-argument 'ergoemacs-command-loop--negative-argument)

;; Command Loop

;; (1) Read Key sequence

(defun ergoemacs-command-loop--read-key-help-text-prefix-argument (&optional blink-on universal)
  "Display prefix argument portion of the `ergoemacs-mode' help text."
  (or (and (not current-prefix-arg)
           (concat (or
                    (and (not universal) "")
                    (and ergoemacs-command-loop-blink-character
                         (or (and blink-on (ergoemacs-key-description--unicode-char ergoemacs-read-blink "-"))
                             " "))
                    " ")
                   (or
                    (and (not universal) "")
                    (ergoemacs-key-description--unicode-char "▸" ">"))))
      (format
       "%s%s%s %s "
       (cond
        ((listp current-prefix-arg)
         (make-string (round (log (nth 0 current-prefix-arg) 4)) ?u))
        (t current-prefix-arg))
       (or (and (not universal) "")
           (and ergoemacs-command-loop-blink-character
                (or (and blink-on (ergoemacs-key-description--unicode-char ergoemacs-read-blink "-"))
                    " "))
           " ")
       (or (and (listp current-prefix-arg)
                (format "%s" current-prefix-arg))
           "")
       (ergoemacs-key-description--unicode-char "▸" ">"))))

(defun ergoemacs-command-loop--read-key (current-key &optional type universal)
  (let* ((universal universal)
         (type (or type :normal))
         (translation (ergoemacs-translate--get type))
         (local-keymap (ergoemacs-translation-struct-keymap translation))
         (text (ergoemacs-translation-struct-text translation))
         (unchorded (ergoemacs-translation-struct-unchorded translation))
         (trans (ergoemacs-translation-struct-translation translation))
         (blink-on nil)
         msg)
    ;; (ergoemacs-command-loop--read-key (read-kbd-macro "C-x" t) :unchorded-ctl)
    (when (functionp text)
      (setq text (funcall text)))
    (setq trans (or (and trans (concat "\nTranslations: "
                                       (mapconcat
                                        (lambda(elt)
                                          (format "%s%s%s"
                                                  (mapconcat #'ergoemacs-key-description--modifier (nth 0 elt) "")
                                                  (ergoemacs-key-description--unicode-char "→" "->")
                                                  (mapconcat #'ergoemacs-key-description--modifier (nth 1 elt) "")))
                                        trans ", "))) ""))
    (setq unchorded (or (and unchorded (concat " " (mapconcat #'ergoemacs-key-description--modifier unchorded ""))) ""))
    (ergoemacs-command-loop--message
     "%s" (concat
           (ergoemacs-command-loop--read-key-help-text-prefix-argument blink-on universal)
           text
           (ergoemacs-key-description current-key)
           unchorded
           ;; Cursor
           trans))
    ))

(defun ergoemacs-command-loop (&optional key type initial-key-type universal)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal.

INITIAL-KEY-TYPE represents the translation type for the initial KEY.

UNIVERSAL allows ergoemacs-read-key to start with universal
argument prompt."
  (let ((type (or type :normal))
        )
    ))

(defun ergoemacs-command-loop--message (&rest args)
  "Message facility for `ergoemacs-mode' command loop"
  (cond
   ;; FIXME, message somewhere when you are in the minibuffer ((minibufferp))
   (t
    (let (message-log-max)
      (apply 'message args)))))


;; (2) Key sequence translated to command
(defun ergoemacs-command-loop--message-binding (key &optional lookup translated-key)
  "Optionally messages information about the translation.
KEY is the original key,
TRANSLATED-KEY is what the assumed key is actually bound.
LOOKUP is what will be run"
  (cond
   ((and lookup (ergoemacs-keymapp lookup)))
   (lookup
    (ergoemacs-command-loop--message "%s%s%s%s"
                                     (ergoemacs-key-description key)
                                     (ergoemacs-key-description--unicode-char "→" "->")
                                     lookup
                                     (or (and translated-key
                                              (format " (from %s)" (ergoemacs-key-description translated-key)))
                                         "")))
   (t
    (ergoemacs-command-loop--message "%s is undefined!"
                                     (ergoemacs-key-description key)))))

(defun ergoemacs-command-loop--key-lookup (key)
  "Find the KEY's function based on current bindings.

If `ergoemacs-mode' has translated this, make emacs think you
pressed the translated key by changing
`ergoemacs-command-loop--single-command-keys'."
  (let ((trials (ergoemacs-translate--trials key))
        ret)
    (catch 'found-command
      (dolist (cur-key trials)
        (when (and cur-key (setq ret (key-binding key)))
          (cond
           ((equal cur-key (nth 0 trials))) ;; Actual key
           ((equal cur-key (nth 1 trials)) ;; `ergoemacs-mode' shift translation
            (setq ergoemacs-command-loop--shift-translated t
                  ergoemacs-command-loop--single-command-keys cur-key))
           (t
            (ergoemacs-command-loop--message-binding key ret cur-key)
            (setq ergoemacs-command-loop--single-command-keys cur-key)))
          (throw 'found-command ret))))
    ret))

(defvar ergoemacs-command-loop--execute-modify-command-list
  '(last-repeatable-command
    this-command
    this-original-command
    mc--this-command)
  "Commands that will be set to what `ergoemacs-command-loop' executes.")

(defvar ergoemacs-command-loop--shift-translated nil
  "This command is forced to be shift translated.")

(defvar ergoemacs-command-loop--mark-active nil
  "Determines if mark was active before ergoemacs command loop.")

(defvar ergoemacs-command-loop--pre-command-hook nil
  "Pre-command hook for `ergoemacs-mode' command loop.")

(defun ergoemacs-command-loop--pre-command-hook ()
  "Command loop `pre-command-hook.'"
  (setq ergoemacs-command-loop--mark-active mark-active)
  ;; Defer pre-command hook for functions that induce the
  ;; `ergoemacs-mode' command loop.
  (unless (ergoemacs :command-loop-p this-command)
    (run-hooks 'ergoemacs-command-loop--pre-command-hook)))

(add-hook 'ergoemacs-pre-command-hook #'ergoemacs-command-loop--pre-command-hook)

(defvar ergoemacs-command-loop--deferred-functions
  '(delete-selection-pre-hook ac-handle-pre-command cua--pre-command-handler mc/make-a-note-of-the-command-being-run)
  "Hooks that are moved to `ergoemacs-pre-command-hook'.
These hooks are deferred to make sure `this-command' is set appropriately.")

(defun ergoemacs-command-loop--depopulate-pre-command-hook ()
  "Depopulate `ergoemacs-command-loop--pre-command-hook' with `pre-command-hook' values."
  (ergoemacs-command-loop--populate-pre-command-hook t))

(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--depopulate-pre-command-hook)

(defun ergoemacs-command-loop--populate-pre-command-hook (&optional depopulate)
  "Populate `ergoemacs-command-loop--pre-command-hook' with `pre-command-hook' values."
  (let ((from-hook (or (and depopulate 'ergoemacs-command-loop--pre-command-hook)
                       'pre-command-hook))
        do-append ergoemacs-mode)
    (dolist (item (default-value from-hook))
      (if (eq item t)
          (setq do-append t)
        (unless (or depopulate (not (memq item ergoemacs-command-loop--deferred-functions)))
          (add-hook 'ergoemacs-command-loop--pre-command-hook item do-append nil)
          (remove-hook 'pre-command-hook item nil))
        (when depopulate
          (add-hook 'pre-command-hook item do-append nil)
          (remove-hook 'ergoemacs-command-loop--pre-command-hook item do-append))))
    (save-excursion
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (unless (equal (default-value from-hook)
                         (ergoemacs-sv from-hook))
            (setq do-append nil)
            (dolist (item (ergoemacs-sv from-hook))
              (if (eq item t)
                  (setq do-append t)
                (unless (or depopulate (not (memq item ergoemacs-command-loop--deferred-functions)))
                  (add-hook 'ergoemacs-command-loop--pre-command-hook item do-append t)
                  (remove-hook 'pre-command-hook item t))
                (when depopulate
                  (add-hook 'pre-command-hook item do-append t)
                  (remove-hook 'ergoemacs-command-loop--pre-command-hook item t))))))))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--populate-pre-command-hook)

(defun ergoemacs-command-loop--execute-handle-shift-selection ()
  "Allow `ergoemacs-mode' command loop to handle shift selection.
This allows shift-selection of non-letter keys.
For instance in QWERTY M-> is shift translated to M-."
  (let ((this-command-keys-shift-translated
         (or this-command-keys-shift-translated
             (if ergoemacs-command-loop--shift-translated t nil))))

    (when (and ergoemacs-command-loop--shift-translated
               (ergoemacs :movement-p function))
      (cond
       ((and shift-select-mode
             ergoemacs-command-loop--mark-active
             (not mark-active))
        ;; Mark was active, then it was deactivated, now activate
        ;; again.
        (setq transient-mark-mode
              (cons 'only
                    (unless (eq transient-mark-mode 'lambda)
                      transient-mark-mode))
              mark-active t))
       (t ;; Mark was not active, activate mark.
        (handle-shift-selection))))))

(defun ergoemacs-command-loop--execute-rm-keyfreq (command)
  "Remove COMMAND from `keyfreq-mode' counts."
  (when (featurep 'keyfreq)
    (when keyfreq-mode
      (let (count)
        (setq count (gethash (cons major-mode command) keyfreq-table))
        (cond
         ((not count))
         ((= count 1)
          (remhash (cons major-mode command) keyfreq-table))
         (count
          (puthash (cons major-mode command) (- count 1)
                   keyfreq-table)))
        ;; Add local-fn to counter.
        (setq count (gethash (cons major-mode function) keyfreq-table))
        (puthash (cons major-mode function) (if count (+ count 1) 1)
                 keyfreq-table)))))

;; (3) execute command
(defvar ergoemacs-command-loop--single-command-keys nil)
(defun ergoemacs-command-loop--execute (command &optional keys)
  "Execute COMMAND pretending that KEYS were pressed."
  ;; The emacs command loop should preserve the undo command by
  ;; running `undo-boundary'.

  (let ((keys (or keys ergoemacs-command-loop--single-command-keys)))
    (cond
     ((or (stringp command) (vectorp command))
      ;; If the command is a keyboard macro (string/vector) then execute
      ;; it though `execute-kbd-macro'
      (execute-kbd-macro command))
     (t
      ;; This should be a regular command.
      
      ;; Remove counting of `this-command' in `keyfreq-mode'
      (ergoemacs-command-loop--execute-rm-keyfreq this-command)
      
      ;; This command execute should modify the following variables:
      ;; - `last-repeatable-command'
      ;; - `this-command'
      ;; - `this-original-command'
      
      ;; In addition, other minor modes may store the command, so these
      ;; should be modified as well.
      
      ;; These are stored in `ergoemacs-command-loop--execute-modify-command-list'
      
      (dolist (var ergoemacs-command-loop--execute-modify-command-list)
        (set var command))
      
      ;; Handle Shift Selection
      (ergoemacs-command-loop--execute-handle-shift-selection)
      (when keys
        ;; Modify the output for these functions when `keys' is not nil.
        ;; - `this-command-keys'
        ;; - `this-command-keys-vector'
        ;; - `this-single-command-keys'

        ;; Also when `keys' are non-nil, modify `last-command-event' and
        ;; `last-nonmenu-event'
        
        ;; This should allow `self-insert-command' to insert the correct key.
        (setq ergoemacs-command-loop--single-command-keys keys
              last-command-event (elt keys (- (length keys) 1))
              last-nonmenu-event last-command-event))
      

      (unwind-protect
          (progn
            ;; Run deferred pre-command hook.
            (remove-hook 'ergoemacs-pre-command-hook #'ergoemacs-pre-command-hook)
            (remove-hook 'ergoemacs-pre-command-hook #'ergoemacs-pre-command-hook t)
            (run-hooks 'ergoemacs-pre-command-hook)
            
            (call-interactively this-command t))
        (setq ergoemacs-command-loop--single-command-keys nil
              ergoemacs-command-loop--shift-translated nil)))))
  
  ;; I think these should be correct from the command loop:
  ;; - `last-event-frame' -- (should be correct from emacs command loop)
  
  ;; After executing, the emacs loop should copy `this-command' into
  ;; `last-command'.
  ;; It should also change `last-prefix-arg'

  ;; The `post-command-hook' should be run by emacs
  )

(provide 'ergoemacs-command-loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-command-loop.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
