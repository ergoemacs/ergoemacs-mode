;;; ergoemacs-command-loop.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2018  Free Software Foundation, Inc.

;; Filename: ergoemacs-command-loop.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:08:09 2013 (-0500)
;;
;;; Commentary:
;;  This is the functions for the `ergoemacs-mode' command loop.
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


(declare-function ergoemacs-warn "ergoemacs-lib")

(declare-function guide-key/close-guide-buffer "guide-key")
(declare-function guide-key/popup-function "guide-key")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")

(declare-function ergoemacs-mode-line "ergoemacs-mode")

(declare-function ergoemacs-layout--regexp "ergoemacs-layouts")
(declare-function ergoemacs-layouts--list "ergoemacs-layouts")

(declare-function ergoemacs-map-properties--movement-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")

(declare-function ergoemacs-translate--define-key "ergoemacs-translate")
(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")
(declare-function ergoemacs-translate--event-basic-type "ergoemacs-translate")
(declare-function ergoemacs-translate--event-convert-list "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifiers "ergoemacs-translate")
(declare-function ergoemacs-translate--event-mods "ergoemacs-translate")
(declare-function ergoemacs-translate--get "ergoemacs-translate")
(declare-function ergoemacs-translate--keymap "ergoemacs-translate")
(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")
(declare-function ergoemacs-translate--trials "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-key "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-keymap "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-keymap-modal "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-modal-always "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-modal-color "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-p "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-text "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-translation "ergoemacs-translate")
(declare-function ergoemacs-translation-struct-unchorded "ergoemacs-translate")

(declare-function ergoemacs-key-description--modifier "ergoemacs-key-description")

(declare-function ergoemacs-posnp "ergoemacs-command-loop")

(if (fboundp 'posnp)
    (defalias 'ergoemacs-posnp #'posnp)
  ;; For emacs 24.1
  (defun ergoemacs-posnp (obj)
    "Return non-nil if OBJ appears to be a valid `posn' object."
    (and (windowp (car-safe obj))
         (atom (car-safe (setq obj (cdr obj))))                ;AREA-OR-POS.
         (integerp (car-safe (car-safe (setq obj (cdr obj))))) ;XOFFSET.
         (integerp (car-safe (cdr obj))))))

(defvar ergoemacs-command-loop-echo-keystrokes)
(defvar ergoemacs-default-cursor-color)
(defvar ergoemacs-echo-function)
(defvar ergoemacs-map--quit-map)
(defvar ergoemacs-modal-emacs-state-modes)
(defvar ergoemacs-modal-ignored-buffers)
(defvar ergoemacs-modal-ignored-keymap)
(defvar ergoemacs-mode-started-p)
(defvar guide-key/guide-key-sequence)
(defvar keyfreq-mode)
(defvar keyfreq-table)


(defvar universal-argument-num-events) ;; Not in Emacs 24.5



(defvar ergoemacs-command-loop--mark-active nil
  "Determines if mark was active before ergoemacs command loop.")


(defvar ergoemacs-command-loop--universal-functions '(universal-argument ergoemacs-universal-argument ergoemacs-command-loop--universal-argument)
  "List of `ergoemacs-mode' recognized functions.")

(define-obsolete-variable-alias 'ergoemacs-universal-fns 'ergoemacs-command-loop--universal-functions "Ergoemacs-v5.16")

(defvar ergoemacs-command-loop--next-key-hash
  (let ((hash (make-hash-table)))
    (puthash 'event-apply-shift-modifier (list '(shift) :force) hash)
    (puthash 'event-apply-alt-modifier (list '(alt) :force) hash)
    (puthash 'event-apply-control-modifier (list '(control) :force) hash)
    (puthash 'event-apply-hyper-modifier (list '(hyper) :force) hash)
    (puthash 'event-apply-meta-modifier (list '(meta) :force) hash)
    (puthash 'event-apply-super-modifier (list '(super) :force) hash)
    hash)
  "Hash table of how functions force the unchorded next key translation to behave.")

(defvar ergoemacs-command-loop--undo-functions
  '(ergoemacs-read-key-undo-last
    ergoemacs-command-loop--undo-last ergoemacs-read-key-force-undo-last ergoemacs-command-loop--force-undo-last)
  "Undo functions recognized by `ergoemacs-mode'.")

(defvar ergoemacs-command-loop--help-last-key nil)

(defvar ergoemacs-command-loop--decode-event-delay 0.01
  "Timeout for `ergoemacs-command-loop--decode-event'.
This is to distinguish events in a terminal, like xterm.

It needs to be less than `ergoemacs-command-loop-blink-rate'.")

(define-obsolete-variable-alias 'ergoemacs-read-key-delay 'ergoemacs-command-loop--decode-event-delay "Ergoemacs-v5.16")

(defvar ergoemacs-command-loop--history nil
  "History of command loop locations.")

(defvar ergoemacs-command-loop--last-event-time nil
  "Stores the last event time.")

(defvar ergoemacs-command-loop--first-type nil
  "This is the first type for the `ergoemacs-mode' command loop.")
(defvar ergoemacs-command-loop--current-type nil
  "This is the current translation type for the `ergoemacs-mode' command loop.")
(defvar ergoemacs-command-loop--universal nil
  "This determines if `ergoemacs-mode' will start editing/completing universal arguments.")
(defvar ergoemacs-command-loop--self-insert-command-count 0
  "The count of `self-insert-command' to allow inserting correct undo boundaries.")
;; (defvar ergoemacs-command-loop--mouse-event nil)
(defvar ergoemacs-command-loop--exit nil
  "External variable controlling if the `ergoemacs-command-loop' will exit.

:ignore-post-command-hook means that the command will exit, and
ignore the post-command hooks.")

(defvar ergoemacs-command-loop--execute-modify-command-list
  '(last-repeatable-command
    this-command
    this-original-command
    mc--this-command)
  "Commands that will be set to what `ergoemacs-command-loop' executes.")

(defun ergoemacs-command-loop--execute-modify-command-list (command)
  "Set variables in `ergoemacs-command-loop--execute-modify-command-list' to COMMAND."
  (ergoemacs-save-buffer-state
   (dolist (var ergoemacs-command-loop--execute-modify-command-list)
     (set var command))))

(defvar ergoemacs-command-loop--single-command-keys nil
  "If defined, a vector of the command keys pressed in the `ergoemacs-command-loop'.")

(defvar ergoemacs-command-loop--echo-keystrokes-complete nil
  "Echoed keystrokes, keep echoing active.")

(defvar ergoemacs-command-loop--modal-stack '()
  "The Modal Stack.")

(defvar ergoemacs-command-loop-swap-translation)
(defvar ergoemacs-command-loop-time-before-blink)
(defvar ergoemacs-command-loop-blink-character)
(defvar ergoemacs-command-loop-blink-rate)
(defvar ergoemacs-command-loop-hide-shift-translations)
(defvar ergoemacs-mode)
(defvar ergoemacs-command-loop-type)
(defvar ergoemacs-keymap)
(defvar ergoemacs-handle-ctl-c-or-ctl-x)
(defvar ergoemacs-ctl-c-or-ctl-x-delay)


(defun ergoemacs-command-loop--modal-show ()
  "Show modal translation.
Returns the mode-line text."
  (let (tmp color text)
    (ergoemacs-save-buffer-state
     (cond
      ((setq tmp (ergoemacs :modal-p))
       (setq color (ergoemacs-translation-struct-modal-color tmp))
       (if color
           (set-cursor-color color)
         (when ergoemacs-default-cursor-color
           (set-cursor-color ergoemacs-default-cursor-color)))
       (setq text (ergoemacs-translation-struct-text tmp))
       (when (functionp text)
         (setq text (funcall text)))
       (if text
           (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
            text)
         (ergoemacs-mode-line))
       (or text "Unnamed"))
      (t
       (when ergoemacs-default-cursor-color
         (set-cursor-color ergoemacs-default-cursor-color))
       (ergoemacs-mode-line)
       nil)))))

(defun ergoemacs-command-loop--modal-p ()
  "Determine if the command should be modal.
If so return the translation."
  (if (not ergoemacs-command-loop--modal-stack) nil
    (let* ((translation (nth 0 ergoemacs-command-loop--modal-stack))
           (always)
           ret)
      (when (ergoemacs-translation-struct-p translation)
        (setq always (ergoemacs-translation-struct-modal-always translation))
        (cond
         ((and (minibufferp)
               (not always)))
         ((and (not always)
               (memq major-mode ergoemacs-modal-emacs-state-modes)))
         ((and (not always)
               (catch 'match-modal
                 (dolist (reg ergoemacs-modal-ignored-buffers)
                   (when (string-match reg (buffer-name))
                     (throw 'match-modal t)))
                 nil)))
         (t
          (setq ret translation))))
      ret)))

(defun ergoemacs-command-loop--modal-pop ()
  "Turn off the last ergoemacs modal in the modal-stack."
  (when ergoemacs-command-loop--modal-stack
    (ergoemacs-command-loop--modal (ergoemacs-translation-struct-key (nth 0 ergoemacs-command-loop--modal-stack)))))

(defun ergoemacs-command-loop--modal (type)
  "Toggle ergoemacs command modes.

The TYPE is the type of command translation/modal keymaps that are installed."
  (cond
   ((or (not ergoemacs-command-loop--modal-stack) ;; First time to turn on
        (not (eq (ergoemacs-translation-struct-key (nth 0 ergoemacs-command-loop--modal-stack)) type)) ;; New modal 
        )
    (push (ergoemacs-translate--get type) ergoemacs-command-loop--modal-stack)
    (unless ergoemacs-default-cursor-color
      (setq ergoemacs-default-cursor-color
            (or (frame-parameter nil 'cursor-color) "black")))
    (ergoemacs-command-loop--message "%s command mode installed" (ergoemacs-command-loop--modal-show)))
   
   (t ;; Turn off.
    (setq ergoemacs-command-loop--modal-stack (cdr ergoemacs-command-loop--modal-stack))
    (if (ergoemacs :modal-p)
        (ergoemacs-command-loop--message "%s command mode resumed." (ergoemacs-command-loop--modal-show))
      (ergoemacs-command-loop--modal-show)
      (ergoemacs-command-loop--message "Resume regular ergoemacs-mode")))))

(defun ergoemacs-command-loop--redefine-quit-key (&optional key)
  "Redefines the quit-key in Emacs to KEY or Ctrl+g.

Typically the Emacs quit key is Ctrl+g, but it can be redefined
with this function."
  (let ((cur-input (current-input-mode))
        (new-key (listify-key-sequence (or key [7]))))
    (when (> 1 (length new-key))
      (error "Will not set a key sequence to the emacs key sequence"))
    (setf (nth 3 cur-input) new-key)
    (and (ignore-errors (apply 'set-input-mode cur-input))
         (message "Redefined Emacs quit key to %s"
                  (ergoemacs-key-description (or key [7])))
         t)))

(defun ergoemacs-command-loop--setup-quit-key ()
  "Setup the `ergoemacs-mode' quit key."
  (let (quit-keys vect-key)
    (dolist (key (reverse (append (where-is-internal 'keyboard-quit)
                                  (where-is-internal 'ergoemacs-keyboard-quit))))
      (setq vect-key (vconcat key))
      (unless (or (symbolp (aref vect-key 0))
                  (not (= 1 (length vect-key)))
                  (member key quit-keys))
        (push vect-key quit-keys)))
    (when quit-keys
      (catch 'found-quit
        (dolist (key quit-keys)
          (when (ergoemacs-command-loop--redefine-quit-key key)
            (throw 'found-quit t)))
        nil))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--setup-quit-key)
(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--redefine-quit-key)

(defun ergoemacs-command-loop--universal-argument (&rest _ignore)
  "`ergoemacs-mode' universal argument.
This is called through `ergoemacs-command-loop'"
  (interactive)
  (cond
   ;; ((not (ergoemacs :command-loop-p))
   ;;  ;; Command loop hasn't started.
   ;;  (setq current-prefix-arg '(4))
   ;;  (ergoemacs-command-loop nil type nil t))
   ((not current-prefix-arg)
    (setq current-prefix-arg '(4)
          ergoemacs-command-loop--universal t
          ergoemacs-command-loop--exit :ignore-post-command-hook))
   ((listp current-prefix-arg)
    ;; Change current prefix argument
    (setq current-prefix-arg (list (* (nth 0 current-prefix-arg) 4))
          ergoemacs-command-loop--universal t
          ergoemacs-command-loop--exit :ignore-post-command-hook))
   (t
    (setq ergoemacs-command-loop--universal t
          ergoemacs-command-loop--exit :ignore-post-command-hook))))

(defalias 'ergoemacs-read-key--universal-argument 'ergoemacs-command-loop--universal-argument)

(defalias 'ergoemacs-universal-argument 'ergoemacs-command-loop--universal-argument)

(defun ergoemacs-command-loop--digit-argument (&optional type)
  "Ergoemacs digit argument.

This is called through `ergoemacs-command-loop'.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'.
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal."
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

This is called through `ergoemacs-command-loop'.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal."
  (setq current-prefix-arg '-)
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-negative-argument 'ergoemacs-command-loop--negative-argument)

(dolist (arg '((next-key-is-alt (meta))
               (next-key-is-meta (meta))
               (next-key-is-ctl (control))
               (next-key-is-control (control))
               (next-key-is-alt-ctl (control meta))
               (next-key-is-ctl-alt (control meta))
               (next-key-is-control-meta (control meta))
               (next-key-is-meta-control (control meta))
               (next-key-is-quoted nil)))
  (eval (macroexpand-all
         `(progn
            (defun ,(intern (concat "ergoemacs-command-loop--" (symbol-name (nth 0 arg)))) ()
              ,(format "Ergoemacs function to allow %s to be the emacs modifiers" (nth 1 arg))
              (interactive)
              (message "Dummy Function for %s" (ergoemacs :modifier-desc ,(nth 1 arg))))
            (defalias ',(intern (concat "ergoemacs-read-key-" (symbol-name (nth 0 arg)))) ',(intern (concat "ergoemacs-command-loop--" (symbol-name (nth 0 arg)))))
            (puthash ',(intern (concat "ergoemacs-command-loop--" (symbol-name (nth 0 arg)))) '(,(nth 1 arg) nil) ergoemacs-command-loop--next-key-hash)
            (puthash ',(intern (concat "ergoemacs-read-key-" (symbol-name (nth 0 arg)))) '(,(nth 1 arg) nil) ergoemacs-command-loop--next-key-hash)
            (defun ,(intern (concat "ergoemacs-command-loop--force-" (symbol-name (nth 0 arg)))) ()
              ,(format "Ergoemacs function to allow %s to be the emacs modifiers" (nth 1 arg))
              (interactive)
              (message "Dummy Function for %s" (ergoemacs :modifier-desc ,(nth 1 arg))))
            (defalias ',(intern (concat "ergoemacs-read-key-force-" (symbol-name (nth 0 arg)))) ',(intern (concat "ergoemacs-command-loop--force-" (symbol-name (nth 0 arg)))))
            (puthash ',(intern (concat "ergoemacs-command-loop--force-" (symbol-name (nth 0 arg)))) '(,(nth 1 arg) :force) ergoemacs-command-loop--next-key-hash)
            (puthash ',(intern (concat "ergoemacs-read-key-force-" (symbol-name (nth 0 arg)))) '(,(nth 1 arg) :force) ergoemacs-command-loop--next-key-hash)))))

(defvar ergoemacs-last-command-event nil
  "`ergoemacs-mode' command loop last read command.")

(defun ergoemacs-command-loop--undo-last ()
  "Function to undo the last key-press.

Uses the `ergoemacs-command-loop--history' variable/function."
  (interactive)
  (if ergoemacs-command-loop--history
      (let ((tmp (pop ergoemacs-command-loop--history)))
        (setq ergoemacs-command-loop--single-command-keys (nth 0 tmp)
              ergoemacs-command-loop--current-type (nth 1 tmp)
              ergoemacs-command-loop--universal (nth 2 tmp)
              current-prefix-arg (nth 3 tmp)
              last-command-event (nth 4 tmp)
              last-input-event last-command-event
              ergoemacs-last-command-event last-command-event))
    ;; Nothing to undo, exit the command loop.
    (setq ergoemacs-command-loop--exit t)))

(defalias 'ergoemacs-read-key-undo-last 'ergoemacs-command-loop--undo-last)

(defun ergoemacs-command-loop--force-undo-last ()
  "Function to undo the last key-press.

Unlike `ergoemacs-command-loop--undo-last', this ignores any bindings like \\[backward-kill-sentence]
This is actually a dummy function.  The actual work is done in `ergoemacs-command-loop'"
  (interactive)
  (call-interactively 'ergoemacs-command-loop--undo-last))
(put 'ergoemacs-command-loop--force-undo-last :ergoemacs-local :force)

(defalias 'ergoemacs-read-key-force-undo-last 'ergoemacs-command-loop--force-undo-last)
(put 'ergoemacs-read-key-force-undo-last :ergoemacs-local :force)

(defun ergoemacs-command-loop--swap-translation ()
  "Function to swap translations.

Uses the `ergoemacs-command-loop-swap-translation' variable."
  (interactive)
  (let ((next-swap (assoc (list ergoemacs-command-loop--first-type ergoemacs-command-loop--current-type) ergoemacs-command-loop-swap-translation)))
    (setq ergoemacs-command-loop--current-type
          (if next-swap
              (nth 1 next-swap)
            :normal))))

(defalias 'ergoemacs-read-key-swap 'ergoemacs-command-loop--swap-translation)

(defun ergoemacs-command-loop--help ()
  "Show help for the current sequence KEY."
  (interactive)
  ;; Eventually...
  (if (not ergoemacs-command-loop--single-command-keys) nil
    (cond
     ;; TEST:
     ;; ((and (boundp 'icicle-mode) icicle-mode)
     ;;  (let ((key (vconcat ergoemacs-command-loop--single-command-keys [ergoemacs-ignore])))
     ;;    (ergoemacs-read-key-call 'icicle-complete-keys nil key)))
     ;; FIXME:
     ((and (boundp 'guide-key-mode) guide-key-mode)
      (let ((key ergoemacs-command-loop--single-command-keys))
        (cond
         ((equal ergoemacs-command-loop--help-last-key ergoemacs-command-loop--single-command-keys)
          (setq ergoemacs-command-loop--help-last-key nil
                guide-key/guide-key-sequence (delete (key-description ergoemacs-command-loop--single-command-keys) guide-key/guide-key-sequence))
          (guide-key/close-guide-buffer))
         (t
          ;; Not using pushnew because the test is equal and
          ;; guide-key/guide-key-sequence is a global variable.
          (setq ergoemacs-command-loop--help-last-key ergoemacs-command-loop--single-command-keys)
          (unless (member (key-description ergoemacs-command-loop--single-command-keys) guide-key/guide-key-sequence)
            (push (key-description ergoemacs-command-loop--single-command-keys) guide-key/guide-key-sequence))
          (guide-key/popup-function key)))))
     (t (let ((cb (current-buffer))
              (key ergoemacs-command-loop--single-command-keys))
          (save-excursion
            (with-help-window (help-buffer)
              (set-buffer (help-buffer))
              (describe-buffer-bindings cb key)))
          (setq ergoemacs-command-loop--exit t))))))

(defalias 'ergoemacs-read-key-help 'ergoemacs-command-loop--help)

;; Command Loop

;; (1) Read Key sequence

(defvar ergoemacs-command-loop--read-key-prompt ""
  "Extra prompt for `ergoemacs-command-loop--read-key'.")
(defun ergoemacs-command-loop--read-key-help-text-prefix-argument (&optional blink-on universal)
  "Display prefix argument portion of the `ergoemacs-mode' help text.

BLINK-ON

UNIVERSAL"
  (or (and (not current-prefix-arg)
           (concat (or
                    (and (not universal) "")
                    (or (and (string= ergoemacs-command-loop--read-key-prompt "") "") " ")
                    (and ergoemacs-command-loop-blink-character
                         (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-command-loop-blink-character "-"))
                             " "))
                    " ")
                   (or
                    (and (not universal) "")
                    (ergoemacs :unicode-or-alt "▸" ">"))))
      (format
       "%s%s%s %s "
       (cond
        ((listp current-prefix-arg)
         (make-string (round (log (nth 0 current-prefix-arg) 4)) ?u))
        (t current-prefix-arg))
       (or (and (not universal) "")
           (and ergoemacs-command-loop-blink-character
                (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-command-loop-blink-character "-"))
                    " "))
           " ")
       (or (and (listp current-prefix-arg)
                (format "%s" current-prefix-arg))
           "")
       (ergoemacs :unicode-or-alt "▸" ">"))))

(defun ergoemacs-command-loop--ensure-sane-variables ()
  "Make sure that certain variables won't lock up Emacs.

Currently this ensures:

`ergoemacs-command-loop--decode-event-delay' is less than `ergoemacs-command-loop-blink-rate'."
  (when (>= ergoemacs-command-loop--decode-event-delay ergoemacs-command-loop-blink-rate)
    (ergoemacs-warn "ergoemacs-command-loop--decode-event-delay >= ergoemacs-command-loop-blink-rate; Reset to ergoemacs-command-loop-blink-rate / 1000")
    (setq ergoemacs-command-loop--decode-event-delay (/ ergoemacs-command-loop-blink-rate 1000))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--ensure-sane-variables)

(defun ergoemacs-command-loop--combine (current-key next-event)
  "Combine CURRENT-KEY and NEXT-EVENT into a vector."
  (let (tmp)
    (cond
     ((and (vectorp current-key)
	   (eventp (setq tmp (aref current-key 0)))
	   (consp tmp)
	   (memq (event-basic-type (car tmp))
		 '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7 mouse-8 mouse-9)))
      (push next-event unread-command-events))
     (t (vconcat current-key (vector next-event))))))

(defvar ergoemacs-comand-loop--untranslated-event nil)

(declare-function ergoemacs--real-read-key-sequence "ergoemacs-command-loop")
(fset 'ergoemacs--real-read-key-sequence (symbol-function #'read-key-sequence))
(declare-function ergoemacs--real-describe-key "ergoemacs-command-loop")

(fset 'ergoemacs--real-describe-key (symbol-function #'describe-key))

(defun ergoemacs-command-loop--input-method (event)
  "Call `input-method-function' on EVENT.
Ensure that `read-key-sequence' is the original function (not
`ergoemacs-command-loop--read-key-sequence')."
  (ergoemacs-no-specials
   (ignore-errors (funcall input-method-function event))))

(defun ergoemacs-command-loop--history (&optional prompt seconds current-key)
  "Read event and add to event history.

PROMPT is the prompt that will be displayed.

SECONDS is the number of seconds between cursor blink.

CURRENT-KEY is the current key being read.  This is used
inconjunction with `input-method-function' to translate keys if
`set-input-method' is using a different keyboard layout.

Also add to `last-command-event' to allow `self-insert-character'
to work appropriately.  I'm not sure the purpose of
`last-event-frame', but this is modified as well.

This is not done when the event is [ergoemacs-ignore]"
  (or (let ((event (pop unread-command-events))
	    translate)
	(setq ergoemacs-comand-loop--untranslated-event event)
	(when (and current-input-method (not current-key)
		   (not overriding-local-map) (not overriding-terminal-local-map)
		   (setq translate (ergoemacs-command-loop--input-method event)))
	  (setq event (pop translate))
	  (when translate
	    (setq unread-command-events (append translate unread-command-events))))
	(unless (eq event 'ergoemacs-ignore)
	  (setq last-command-event event
		last-input-event last-command-event
		ergoemacs-last-command-event last-command-event
		last-event-frame (selected-frame)))
	event)
      (let* ((last-event-time (or (and ergoemacs-command-loop--last-event-time
				       (- (float-time) ergoemacs-command-loop--last-event-time))
				  (and (setq ergoemacs-command-loop--last-event-time (float-time)) 0)))
	     (prompt (cond
		      ((not prompt) nil)
		      ((not (string= "" ergoemacs-command-loop--read-key-prompt)) prompt)
		      ((or (string= prompt " ")
			   (string-match-p prompt (concat " *" (ergoemacs :unicode-or-alt ergoemacs-command-loop-blink-character "-") " *"))) nil)
		      (ergoemacs-command-loop--universal prompt)
		      (ergoemacs-command-loop--echo-keystrokes-complete prompt)
		      ((not (numberp ergoemacs-command-loop-echo-keystrokes)) prompt)
		      ((= 0 ergoemacs-command-loop-echo-keystrokes) prompt)
		      ((< last-event-time ergoemacs-command-loop-echo-keystrokes) nil)
		      ;; ((and (not ergoemacs-command-loop--echo-keystrokes-complete)
		      ;;       (numberp ergoemacs-command-loop-echo-keystrokes)
		      ;;       (or (= 0 ergoemacs-command-loop-echo-keystrokes)
		      ;;           (< last-event-time ergoemacs-command-loop-echo-keystrokes))) nil)
		      ;; ((and (< last-event-time ergoemacs-command-loop-time-before-blink) (string= prompt "")) nil)
		      ;; ((and (< last-event-time ergoemacs-command-loop-time-before-blink) ) nil)
		      (t
		       (setq ergoemacs-command-loop--echo-keystrokes-complete t)
		       prompt)))
	     (echo-keystrokes 0)
	     ;; Run (with-timeout) so that idle timers will work.
	     (event (cond
		     (prompt (with-timeout (seconds nil)
			       (progn
				 (ergoemacs-command-loop--message prompt)
				 (ignore-errors (read-event)))))
		     ((and (not ergoemacs-command-loop--echo-keystrokes-complete)
			   ergoemacs-command-loop--single-command-keys)
		      (with-timeout (ergoemacs-command-loop-echo-keystrokes nil)
			(ignore-errors (read-event))))
		     (t (ignore-errors (read-event)))))
	     translate)
	(when (eventp event)
	  (setq ergoemacs-comand-loop--untranslated-event event)
	  (unless (consp event) ;; Don't record mouse events
	    (when (and current-input-method (not current-key)
		       (not overriding-local-map) (not overriding-terminal-local-map)
		       (setq translate (ergoemacs-command-loop--input-method event)))
	      (setq event (pop translate))
	      (when translate
		(setq unread-command-events (append translate unread-command-events))))
	    (push (list ergoemacs-command-loop--single-command-keys 
			ergoemacs-command-loop--current-type 
			ergoemacs-command-loop--universal
			current-prefix-arg
			last-command-event)
		  ergoemacs-command-loop--history))
	  (unless (eq event 'ergoemacs-ignore)
	    (setq ergoemacs-command-loop--last-event-time (float-time)
		  last-command-event event
		  last-input-event last-command-event
		  ergoemacs-last-command-event last-command-event
		  last-event-frame (selected-frame))))
	event)))

(defvar ergoemacs-command-loop--decode-event-timeout-p nil
  "Determines if `ergoemacs-command-loop--decode-event' timed out.")

(defun ergoemacs-command-loop--decode-event (event keymap &optional current-key)
  "Change EVENT based on KEYMAP.

Used to help with translation keymaps like `input-decode-map'.

CURRENT-KEY is the current key being read.  This is used
inconjunction with `input-method-function' to translate keys if
`set-input-method' is using a different keyboard layout."
  (let* ((new-event event)
         (old-ergoemacs-input unread-command-events)
         new-ergoemacs-input
         (current-test-key (or (and (listp event)
				    (vector (ergoemacs-translate--event-convert-list
					     (append (ergoemacs-translate--event-modifiers event)
						     (list (ergoemacs-translate--event-basic-type event))))))
			       (vector event)))
         (test-ret (lookup-key keymap current-test-key))
	 (timeout-key (key-binding (vconcat current-test-key [ergoemacs-timeout])))
         next-key)
    (while (and current-test-key
                (ergoemacs-keymapp test-ret))
      ;; The translation needs more keys...
      (if timeout-key
	  (setq next-key (with-timeout (ergoemacs-ctl-c-or-ctl-x-delay
					(progn
					  (setq ergoemacs-command-loop--decode-event-timeout-p t)
					  nil))
			   (ergoemacs-command-loop--history nil ergoemacs-command-loop--decode-event-delay current-key)))
	(setq next-key (ergoemacs-command-loop--history nil ergoemacs-command-loop--decode-event-delay current-key)))
      (when next-key ;; Since a key was read, save it to be read later.
        (push last-command-event new-ergoemacs-input))
      (if next-key
          (setq current-test-key (ergoemacs :combine current-test-key next-key)
		timeout-key (key-binding (vconcat current-test-key [ergoemacs-timeout]))
                test-ret (lookup-key keymap current-test-key))
        (setq current-test-key nil)))
    ;; Change strings to emacs keys.
    (when (stringp test-ret)
      ;; Should it be read-kbd-macro?
      (setq test-ret (vconcat test-ret)))
    (when (functionp test-ret)
      (when (memq test-ret '(xterm-mouse-translate xterm-mouse-translate-extended))
	(message "xterm-mouse-translate: %s->%s" current-test-key (funcall test-ret nil)))
      (setq last-input-event event
	    test-ret (if (or (eq keymap input-decode-map)
			     (eq keymap key-translation-map)
			     (eq keymap local-function-key-map))
			 (funcall test-ret nil) ;; Pretend emacs called this from command loop.
		       (funcall test-ret)))
      (when (not (equal unread-command-events old-ergoemacs-input))
      	(push (pop unread-command-events) new-ergoemacs-input)))
    (if (and (vectorp test-ret)
             (= (length test-ret) 1))
        (progn
          (setq new-event (elt test-ret 0)))
      ;; Not a new event, restore anything that was popped off the
      ;; unread command events.
      (when old-ergoemacs-input
        (setq unread-command-events old-ergoemacs-input))
      ;; Add anything read to the
      ;; unread-command-events
      (when new-ergoemacs-input
        (setq unread-command-events (append new-ergoemacs-input unread-command-events))))
    new-event))

(defun ergoemacs-command-loop--read-event (prompt &optional current-key)
  "Read a single event.

PROMPT is the prompt used when reading an event.

CURRENT-KEY is the current key sequence that has alerady been
read.

This respects `input-decode-map', `local-function-key-map' and
`key-translation-map'.

It also inputs real read events into the history with
`ergoemacs-command-loop--history'

It will timeout after `ergoemacs-command-loop-blink-rate' and
return nil."
  (let ((input (ergoemacs-command-loop--history prompt ergoemacs-command-loop-blink-rate current-key))
        last-input
        basic mods
        binding gui)
    ;; Fix issues with `input-decode-map'
    (when input
      ;; Fix input as if you defined C-i -> <C-i> on `input-decode-map'
      ;; http://emacs.stackexchange.com/questions/10271/how-to-bind-c-for-real-seriously-for-real-this-time/15174
      (if (and (display-graphic-p)
               (setq basic (event-basic-type input))
               (memq basic (list 'i 'm '\[ ?i ?m ?\[))
               (setq mods (event-modifiers input))
               (memq 'control mods)
               (setq gui (ergoemacs-translate--event-convert-list (append (list 'ergoemacs-gui) mods (list basic))))
               (setq binding (key-binding (ergoemacs :combine current-key input) t)))
          (setq input gui)
        (setq input (ergoemacs-command-loop--decode-event input input-decode-map current-key)
              binding (key-binding (ergoemacs :combine current-key input) t)))
      ;; These should only be replaced if they are not bound.
      (unless binding
        (setq last-input input
              input (ergoemacs-command-loop--decode-event input local-function-key-map current-key))
        (unless (eq last-input input)
          (setq binding (key-binding (ergoemacs :combine current-key input) t))))
      (setq last-input input
	    input (ergoemacs-command-loop--decode-event input key-translation-map current-key))
      (unless (eq last-input input)
          (setq binding (key-binding (ergoemacs :combine current-key input) t))))
    input))

(defun ergoemacs-command-loop--read-key (&optional current-key type universal)
  "Read a key for the `ergoemacs-mode' command loop.

This uses `ergoemacs-command-loop--read-event'.

CURRENT-KEY is the current key that is being read, the next key
read will be appended to this key.

TYPE is the type of translation being applied.  By default,
the :normal traslation is used.

UNIVERSAL flag telss if this is a univeral argument that is being
read."
  (let* ((universal universal)
         (type (or type :normal))
         (translation (ergoemacs-translate--get type))
         (local-keymap (ergoemacs-translate--keymap translation))
         (text (ergoemacs-translation-struct-text translation))
         (unchorded (ergoemacs-translation-struct-unchorded translation))
         (trans (ergoemacs-translation-struct-translation translation))
         (modal (ergoemacs :modal-p))
         (keys nil)
         (blink-on nil)
         input
         raw-input
         mod-keys tmp
         reset-key-p
         double)
    ;; Setup modal translation
    (when (and (eq type :normal) modal)
      (setq type (ergoemacs-translation-struct-key modal)
            local-keymap (ergoemacs-translation-struct-keymap-modal modal)
            text (ergoemacs-translation-struct-text modal)
            unchorded (ergoemacs-translation-struct-unchorded modal)
            trans (ergoemacs-translation-struct-translation modal)
            tmp translation
            translation modal
            modal tmp
            tmp nil))
    
    ;; (ergoemacs-command-loop--read-key (read-kbd-macro "C-x" t) :unchorded-ctl)
    (when (functionp text)
      (setq text (funcall text)))

    (when trans
      ;; Don't echo the uncommon hyper/super/alt translations (alt is
      ;; not the alt key...)
      (dolist (tr trans)
        (unless (or (memq 'hyper (nth 0  tr)) (memq 'super (nth 0 tr)) (memq 'alt (nth 0 tr))
                    (and ergoemacs-command-loop-hide-shift-translations (memq 'shift (nth 0  tr))))
          (if (member (list (nth 1 tr) (nth 0 tr)) trans)
              (when (not (member (list (nth 1 tr) (nth 0 tr)) double))
                (push tr double))
            (push tr tmp))))
      (setq trans tmp))
    
    (setq trans (or (and (or trans double)
                         (concat "\nTranslations: "
                                 (or (and double
                                          (mapconcat
                                           (lambda(elt)
      ;; (and (setq tmp (elt current-key 0))
      ;;      (or (and (consp tmp) (symbolp (setq tmp (car tmp)))))
      ;;      (stringp tmp)
      ;;      (string-match-p "\\<mouse\\>" tmp))
                                             (format "%s%s%s"
                                                     (ergoemacs :modifier-desc (nth 0 elt))
                                                     (ergoemacs :unicode-or-alt "↔" "<->")
                                                     (ergoemacs :modifier-desc (nth 1 elt))))
                                           double ", "))
                                     "")
                                 (or (and double trans ", ") "")
                                 (mapconcat
                                  (lambda(elt)
                                    (format "%s%s%s"
                                            (ergoemacs :modifier-desc (nth 0 elt))
                                            (ergoemacs :unicode-or-alt "→" "->")
                                            (ergoemacs :modifier-desc (nth 1 elt))))
                                  trans ", "))) ""))
    (maphash
     (lambda(key item)
       (let ((local-key (where-is-internal key local-keymap t))
             tmp)
         (when local-key
           (setq tmp (format "%s%s%s"
                             (ergoemacs-key-description local-key)
                             (if (eq (nth 1 item) :force)
                                 (ergoemacs :unicode-or-alt "⇒" "=>")
                               (ergoemacs :unicode-or-alt "→" "->"))
                             (ergoemacs :modifier-desc (nth 0 item))))
           (push (elt local-key 0) mod-keys)
           (setq keys (or (and (not keys) tmp)
                          (and keys (concat keys ", " tmp)))))))
     ergoemacs-command-loop--next-key-hash)

    (setq keys (or (and keys (concat "\nKeys: " keys)) ""))
    (setq unchorded (or (and unchorded (concat " " (ergoemacs :modifier-desc unchorded))) ""))
    
    (while (not input)
      (while (not input)
        (setq blink-on (not blink-on)
              input (ergoemacs-command-loop--read-event
                     (format
                      "%s" (concat
                            ergoemacs-command-loop--read-key-prompt
                            (ergoemacs-command-loop--read-key-help-text-prefix-argument blink-on universal)
                            text
                            (ergoemacs-key-description current-key)
                            unchorded
                            ;; Cursor
                            (or (and (string= ergoemacs-command-loop--read-key-prompt "") "") " ")
                            (or (and universal "")
                                (and ergoemacs-command-loop-blink-character
                                     (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-command-loop-blink-character "-"))
                                         " "))
                                " ")
                            trans
                            keys))
                     current-key)))
      (cond
       ((and (setq trans (or (and (memq input mod-keys)
                                  (ergoemacs-gethash (lookup-key local-keymap (vector input)) ergoemacs-command-loop--next-key-hash))
                             (setq reset-key-p (ergoemacs-gethash (lookup-key local-function-key-map (ergoemacs :combine current-key input)) ergoemacs-command-loop--next-key-hash))))
             (or (eq :force (nth 1 trans)) ;; Override any keys
                 (not (key-binding (vconcat current-key (ergoemacs-translate--event-mods input trans)) t)) ;; Don't use if bound.
                 ))
        (setq trans (nth 0 trans)
              unchorded (concat " " (ergoemacs :modifier-desc trans))
              input nil)
        ;; Changed behavior.
        (while (not input)
          (setq blink-on (not blink-on)
                input (ergoemacs-command-loop--read-event
                       (format
                        "%s" (concat
                              ergoemacs-command-loop--read-key-prompt
                              (ergoemacs-command-loop--read-key-help-text-prefix-argument blink-on universal)
                              text
                              (or (and reset-key-p "") (ergoemacs-key-description current-key))
                              unchorded
                              ;; Cursor
                              (or (and (string= ergoemacs-command-loop--read-key-prompt "") "") " ")
                              (or (and universal "")
                                  (and ergoemacs-command-loop-blink-character
                                       (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-command-loop-blink-character "-"))
                                           " "))
                                  " ")
                              "\n"
                              "\n"))
                       current-key)))
        (setq raw-input input
              input (ergoemacs-translate--event-mods input trans)
              last-command-event input
              last-input-event input
              ergoemacs-last-command-event last-command-event))
       (t
        ;; Translate the key appropriately.
        (when (and modal (lookup-key ergoemacs-modal-ignored-keymap (vector input)))
          ;; Swap back, or ignore the modal translation.
          (setq type (ergoemacs-translation-struct-key modal)
                local-keymap (ergoemacs-translation-struct-keymap-modal modal)
                text (ergoemacs-translation-struct-text modal)
                unchorded (ergoemacs-translation-struct-unchorded modal)
                trans (ergoemacs-translation-struct-translation modal)
                tmp translation
                translation modal
                modal tmp
                tmp nil))
        (setq raw-input input
              input (ergoemacs-translate--event-mods input type)
              last-command-event input
              last-input-event input
              ergoemacs-last-command-event last-command-event)))
      (cond
       ((and input (not universal)
             (not (key-binding (ergoemacs :combine current-key raw-input)))
             (and local-keymap
                  (memq (lookup-key local-keymap (vector raw-input))
                        ergoemacs-command-loop--universal-functions)))
        (setq universal t
              raw-input nil
              input nil
              ergoemacs-command-loop--echo-keystrokes-complete t))
       ((and raw-input universal) ;; Handle universal arguments.
        (setq ergoemacs-command-loop--echo-keystrokes-complete t)
        (cond
         ((eq raw-input 45) ;; Negative argument
          (cond
           ((integerp current-prefix-arg)
            (setq current-prefix-arg (- current-prefix-arg)))
           ((eq current-prefix-arg '-)
            (setq current-prefix-arg nil))
           (t
            (setq current-prefix-arg '-)))
          (setq raw-input nil
                input nil))
         ((memq raw-input (number-sequence 48 57)) ;; Number
          (setq raw-input (- raw-input 48)) ;; Actual Number.
          (cond
           ((and (integerp current-prefix-arg) (< 0 current-prefix-arg))
            (setq current-prefix-arg (+ raw-input (* current-prefix-arg 10))))
           ((and (integerp current-prefix-arg) (> 0 current-prefix-arg))
            (setq current-prefix-arg (+ (- raw-input) (* current-prefix-arg 10))))
           ((and (eq current-prefix-arg '-) (> raw-input 0))
            (setq current-prefix-arg (- raw-input)))
           (t
            (setq current-prefix-arg raw-input)))
          (setq input nil
                raw-input nil))
         ((and local-keymap
               (memq (lookup-key local-keymap (vector raw-input))
                     ergoemacs-command-loop--universal-functions)) ;; Toggle to key-sequence.
          (setq raw-input nil
                universal nil))
         ((or (memq (key-binding (ergoemacs :combine current-key input) t) ergoemacs-command-loop--universal-functions)
              (not (key-binding (ergoemacs :combine current-key raw-input) t))
              (and local-keymap (memq (lookup-key local-keymap (vector raw-input)) ergoemacs-command-loop--universal-functions)))
          ;; Universal argument called.
          (cond
           ((not current-prefix-arg)
            (setq current-prefix-arg '(4)
                  raw-input nil
                  input nil))
           ((listp current-prefix-arg)
            (setq current-prefix-arg (list (* (nth 0 current-prefix-arg) 4))
                  raw-input nil
                  input nil))
           (t
            (setq universal nil
                  input nil
                  raw-input nil))))
         ((and local-keymap
               (memq (lookup-key local-keymap (vector raw-input))
                     ergoemacs-command-loop--undo-functions))
          ;; Allow backspace to edit universal arguments.
          (cond
           ((not current-prefix-arg)) ;; Exit  universal argument
           ((and (integerp current-prefix-arg)
                 (= 0 (truncate current-prefix-arg 10))
                 (< 0 current-prefix-arg))
            (setq current-prefix-arg nil
                  input nil
                  raw-input nil))
           ((and (integerp current-prefix-arg)
                 (= 0 (truncate current-prefix-arg 10))
                 (> 0 current-prefix-arg))
            (setq current-prefix-arg '-
                  input nil
                  raw-input nil))
           ((integerp current-prefix-arg)
            (setq current-prefix-arg (truncate current-prefix-arg 10)
                  input nil
                  raw-input nil))
           ((listp current-prefix-arg)
            (setq current-prefix-arg
                  (list (expt 4 (- (round (log (nth 0 current-prefix-arg) 4)) 1))))
            (when (equal current-prefix-arg '(1))
              (setq current-prefix-arg nil))
            (setq input nil
                  raw-input nil))
           ((eq current-prefix-arg '-)
            (setq current-prefix-arg nil
                  input nil
                  raw-input nil))))))))
    ;; Return list of raw key, and translated current key
    (list (vector raw-input) (ergoemacs :combine (if reset-key-p nil current-key) input))))

(defun ergoemacs-command-loop--listify-key-sequence (key &optional type)
  "Return a key sequence from KEY.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'.

This sequence is compatible with `listify-key-sequence'."
  (let (input
        (type (or type :normal)))
    (cond
     ((not key)) ;; Not specified.
     ((vectorp key) ;; Actual key sequence
      (setq input (listify-key-sequence key)))
     ((consp key) ;; Listified key sequence
      (setq input key))
     ((stringp key) ;; Kbd code
      (setq input (listify-key-sequence (read-kbd-macro key t)))))
    (setq input (mapcar
                 (lambda(elt)
                   (ergoemacs-translate--event-mods elt type))
                 input))
    input))

(defun ergoemacs-command-loop-p ()
  "Determine if `ergoemacs-mode' is running its command loop.
This is done by looking at the current `backtrace' and making
sure that `ergoemacs-command-loop--internal' hasn't been called."
  (eq ergoemacs-last-command-event last-command-event))

(defvar ergoemacs-command-loop-start nil)
(defun ergoemacs-command-loop (&optional key type initial-key-type universal)
  "Process `ergoemacs-command-loop'.

KEY is the key being read, or sequence being read.

TYPE is the translation being used.

INITIAL-KEY-TYPE ist he key type that is used fot the initial
translation.

UNIVERSAL is if the function will be calling a universal
argument.

The true work is done in `ergoemacs-command-loop--internal'."
  (interactive)
  (cond
   ((and ergoemacs-command-loop-start (not (ergoemacs-command-loop-p)))
    ;; (ergoemacs-command-loop--message "Start ergoemacs-mode command loop." )
    (ergoemacs-command-loop--internal key type initial-key-type universal))
   (t
    (setq ergoemacs-command-loop--exit :ignore-post-command-hook
          prefix-arg current-prefix-arg
          ergoemacs-command-loop--single-command-keys (or (and key (read-kbd-macro key t))
                                                          ergoemacs-command-loop--single-command-keys)
          unread-command-events (or (and key (ergoemacs-command-loop--listify-key-sequence key initial-key-type))
                                    unread-command-events)
          ergoemacs-command-loop--universal (if (and ergoemacs-command-loop--universal (not universal)) nil
                                              universal)
          ergoemacs-command-loop--current-type (or type ergoemacs-command-loop--current-type)))))

(defvar ergoemacs-command-loop--running-pre-command-hook-p nil
  "Variable to tell if ergoemacs-command loop is running the `pre-command-hook'.")

(defvar ergoemacs-command-loop--excluded-variables
  '(defining-kbd-macro executing-kbd-macro)
  "List of variables stopping the command loop.

While these variables are non-nil, the `ergoemacs-command-loop'
will stop and not be started agin.")

(defvar ergoemacs-command-loop--excluded-major-modes
  '(calc-mode calc-trail-mode calc-edit-mode)
  "List of major modes where the command loop is incompatible.")


(defvar ergoemacs-command-loop--minibuffer-unsupported-p nil)
(defun ergoemacs-command-loop--minibuffer-supported-p (&optional command)
  "Determine if the current minibuffer supports the full command loop.
When COMMAND is non-nil, set
`ergoemacs-command-loop--minibuffer-unsupported-p' to the
appropriate value based on the COMMAND."
  (if (not command)
      (or (not (minibufferp))
	  (not ergoemacs-command-loop--minibuffer-unsupported-p))
    (when (or (and command (symbolp command) (string-match-p "^\\(calc\\|math\\)" (symbol-name command)))
	      (and (stringp command) (string-match-p "^[^:]*:\\(calc\\|math\\)" command))) 
      (set (make-local-variable 'ergoemacs-command-loop--minibuffer-unsupported-p) t))
    (ergoemacs-command-loop--minibuffer-supported-p)))

(defun ergoemacs-command-loop-full-p ()
  "Determines if the full command loop should be run."
  (and
   (eq ergoemacs-command-loop-type :full)
   (ergoemacs-command-loop--minibuffer-supported-p)
   (catch 'excluded-variables
     (dolist (var ergoemacs-command-loop--excluded-variables)
       (when (and var (ergoemacs-sv var))
         (throw 'excluded-variables nil)))
     t)
   (not (memq major-mode ergoemacs-command-loop--excluded-major-modes))))

(defun ergoemacs-command-loop--start-with-pre-command-hook ()
  "Start ergoemacs command loop.

This is done by replacing `this-command' with
`ergoemacs-command-loop-start' and then running `this-command'
from within the ergoemacs-mode command loop."
  (when (and (not ergoemacs-command-loop--running-pre-command-hook-p)
             (ergoemacs-command-loop-full-p)
             (not unread-command-events)
             (not (ergoemacs-command-loop-p)))
    (setq ergoemacs-command-loop-start this-command
          ergoemacs-command-loop--single-command-keys (this-single-command-keys)
          this-command 'ergoemacs-command-loop-start)))

(add-hook 'ergoemacs-pre-command-hook #'ergoemacs-command-loop--start-with-pre-command-hook)


(defvar ergoemacs-command-loop--internal-end-command-p nil)

(defvar ergoemacs-last-command-was-ergoemacs-ignore-p nil
  "Last command was `ergoemacs-ignore'.")

(defun ergoemacs-command-loop--start-with-post-command-hook ()
  "Start ergoemacs command loop.

This is done by pushing the key [ergoemacs-ignore] on the
`unread-command-events' stack.  This then forces `ergoemacs-mode'
to start with
`ergoemacs-command-loop--start-with-pre-command-hook'."
  (when (and (not ergoemacs-command-loop--internal-end-command-p)
             (ergoemacs-command-loop-full-p))
    (if ergoemacs-last-command-was-ergoemacs-ignore-p
	(unless (eq ergoemacs-last-command-was-ergoemacs-ignore-p :idle)
	  (run-with-idle-timer 0.05 nil (lambda()
					  (setq ergoemacs-last-command-was-ergoemacs-ignore-p :idle)
					  (ergoemacs-command-loop-start))))
      (push 'ergoemacs-ignore unread-command-events))))

(add-hook 'ergoemacs-post-command-hook #'ergoemacs-command-loop--start-with-post-command-hook)

(defvar ergoemacs-command-loop--point-motion-last-point nil
  "Record the last point.")

(defun ergoemacs-command-loop--point-motion-hooks ()
  "Emlulate Emacs' command-loop portion of the point-motion hooks.
The properties `point-entered' and `point-left' are handled by C internals."
  (unless (or disable-point-adjustment global-disable-point-adjustment inhibit-point-motion-hooks)
    ;; Only the adjustment of the point in fishy areas is done in the
    ;; command loop.
    (let* ((props '(intangible composition display invisible))
	   (last-point (or ergoemacs-command-loop--point-motion-last-point (point)))
	   (cur-point (point))
	   (found-prop (catch 'found-prop
			 (dolist (p props)
			   (when (get-char-property (point) p)
			     (throw 'found-prop p)))
			 nil)))
      ;; Adjust the point in fishy areas.
      (when found-prop
	(goto-char (or (and (>= cur-point last-point)
			    (next-single-char-property-change cur-point found-prop))
		       (previous-single-char-property-change cur-point found-prop)))
	(setq last-point cur-point
	      cur-point (point)))))
  (setq disable-point-adjustment nil)
  (set (make-local-variable 'ergoemacs-command-loop--point-motion-last-point) (point)))

(defun ergoemacs-command-loop--sync-point ()
  "Sometimes the window buffer and selected buffer are out of sync.
Fix this issue."
  (unless (eq (current-buffer) (window-buffer))
    (ignore-errors (switch-to-buffer (window-buffer) t t))
    (goto-char (window-point))))

(defun ergoemacs-command-loop--update-primary-selection ()
  "Update primary clipboard in X based systems."
  (when (and mouse-drag-copy-region
	     (eventp last-command-event)
	     (consp last-command-event)
	     (memq (event-basic-type (car last-command-event))
			'(mouse-1))
	     (region-active-p))
    (ergoemacs :set-selection 'PRIMARY (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun ergoemacs-command-loop--internal-end-command ()
  "Simulates the end of a command."
  ;; Simulate the end of an emacs command, since we are not
  ;; exiting the loop.
  (setq ergoemacs-command-loop--internal-end-command-p t)
  (unwind-protect
      (run-hooks 'post-command-hook)
    (setq ergoemacs-command-loop--internal-end-command-p nil))
  
  ;; Deactivate mark.
  (when deactivate-mark
    (deactivate-mark)
    (setq deactivate-mark nil))
  
  ;; Create undo-boundary like emacs does.

  ;; The undo boundary is created every 20 characters.
  (when (eq this-command 'self-insert-command)
    ;; Adapted from `org-self-insert-command'
    (if (not (eq last-command 'self-insert-command))
        (setq ergoemacs-command-loop--self-insert-command-count 1)
      (if (>= ergoemacs-command-loop--self-insert-command-count 20)
          (setq ergoemacs-command-loop--self-insert-command-count 1)
        (and (> ergoemacs-command-loop--self-insert-command-count 0)
             buffer-undo-list (listp buffer-undo-list)
             (not (cadr buffer-undo-list)) ; remove nil entry
             (setcdr buffer-undo-list (cddr buffer-undo-list)))
        (setq ergoemacs-command-loop--self-insert-command-count
              (1+ ergoemacs-command-loop--self-insert-command-count))))
    ;; See: http://stackoverflow.com/questions/6590889/how-emacs-determines-a-unit-of-work-to-undo
    
    ;; FIXME:
    ;; Certain "hairy" insertions (as determined by
    ;; internal_self_insert) cause an an undo boundary to be added
    ;; immediately, and the character count to be reset. Reading the
    ;; code, it looks as though these are: (1) in overwrite-mode, if you
    ;; overwrote a character with one that has a different width,
    ;; e.g. typing over a tab; (2) if the character you inserted caused
    ;; an abbreviation to be expanded; (3) if the character you typed
    ;; caused auto-fill-mode to insert indentation.
    )
  
  ;; After executing, the emacs loop should copy `this-command' into
  ;; `last-command'.
  ;; It should also change `last-prefix-arg'
  (setq last-command this-command
        real-last-command this-command ;; Hopefully doesn't throw an error.
        last-prefix-arg prefix-arg
        current-prefix-arg prefix-arg
        prefix-arg nil
        this-command nil
        deactivate-mark nil
        ergoemacs-command-loop--echo-keystrokes-complete nil)
  
  (undo-boundary)
  ;;  This (sort of) fixes `this-command-keys'
  ;; But it doesn't fix it for keyboard macros.
  (clear-this-command-keys t)
  (setq ergoemacs-command-loop--decode-event-timeout-p nil)
  (ergoemacs-command-loop--sync-point)
  (ergoemacs-command-loop--point-motion-hooks)
  (ergoemacs-command-loop--update-primary-selection))

(defun ergoemacs-command-loop--mouse-command-drop-first (args &optional fn-arg-p)
  "Internal function for processing mouse commands.

This function drops the first argument of a function, which is
usually an event for mouse functions.

ARGS are the function's arguments.
FN-ARG-P can be nil, :drop-rest or :rest"
  (let (ret)
    (cond
     ((eq fn-arg-p :drop-rest)
      (if (and (<= 2 (length args))
               (eq (nth (- (length args) 2) args) '&rest))
          (if (= (length args) 2)
              nil
            (ergoemacs-command-loop--mouse-command-drop-first (butlast args 2)))
        (ergoemacs-command-loop--mouse-command-drop-first args)))
     ((eq fn-arg-p :rest)
      (if (and (<= 2 (length args))
               (eq (nth (- (length args) 2) args) '&rest))
          (nth (- (length args) 1) args)
        nil))
     ((eq (car args) '&rest)
      ;;(&rest arg)
      (if fn-arg-p args 
        (cdr args)))
     ((eq (car args) '&optional)
      ;;(&optional ...)
      (if fn-arg-p
          (cond
           ((not (cdr (cdr args)))
            ;; (&optional arg) -> nil
            nil)
           ((eq '&rest (car (cdr (cdr args))))
            ;; (&optional arg &rest rest) -> (&rest rest)
            (cdr (cdr args)))
           (t
            ;; (&optional arg1 arg2 &rest rest)-> (&optional arg2 &rest rest)
            ;; (&optional arg1 arg2 arg3)-> (&optional arg2 arg3)
            `(&optional ,@(cdr (cdr args)))))
        (dolist (a (cdr (cdr args)))
          (unless (eq a '&rest)
            (push a ret)))
        (reverse ret)))
     (t
      (if fn-arg-p
          (cdr args)
        (dolist (a (cdr args))
          (unless (memq a '(&rest &optional))
            (push a ret)))
        (reverse ret))))))

(defun ergoemacs-command-loop--modify-mouse-command (command)
  "Modify mouse COMMAND to work with ergoemacs command loop."
  (let* ((iform (interactive-form command))
         (form (and iform (consp iform) (= 2 (length iform)) (stringp (nth 1 iform)) (nth 1 iform)))
         (args (help-function-arglist command t))
         (fn-args (ergoemacs-command-loop--mouse-command-drop-first args t))
         (strip-args (ergoemacs-command-loop--mouse-command-drop-first args))
         (rest-p (ergoemacs-command-loop--mouse-command-drop-first args :rest))
         (drop-rest (ergoemacs-command-loop--mouse-command-drop-first args :drop-rest))
         (select-window-p (and form (string-match-p "^[*^]*[@]" form)))
         (event-p (and form (string-match-p "^[*@^]*e" form)))
         (new-form (and form
                        (or (and (not event-p) form)
                            (and event-p (replace-regexp-in-string "^\\([*@^]*\\)e\n*\\(.*\\)" "\\1\\2" form))))))
    (when (and new-form (string= new-form ""))
      (setq new-form nil))
    (cond
     ((not event-p)
      command)
     (rest-p
      `(lambda ,fn-args
         ,(if new-form
              `(interactive ,new-form)
            `(interactive))
         ,(when select-window-p
            '(select-window (posn-window (event-start last-command-event))))
         (if ,rest-p
             (apply ',command last-command-event ,@strip-args)
           (,command last-command-event ,@drop-rest))
	 (ergoemacs-command-loop--execute-modify-command-list ',command)))
     
     ((not rest-p)
      `(lambda ,fn-args
         ,(if new-form
              `(interactive ,new-form)
            `(interactive))
         ,(when select-window-p
            '(select-window (posn-window (event-start last-command-event))))
         (,command last-command-event ,@strip-args)
	 (ergoemacs-command-loop--execute-modify-command-list ',command))))))

(defun ergoemacs-command-loop--call-mouse-command (command &optional record-flag keys)
  "Call a possible mouse COMMAND.

The COMMAND is modified to take out any event information and
replace it with `last-event-command' information.  This
modifciation isd one by
`ergoemacs-command-loop--modify-mouse-command'.

Mouse commands are also wrapped in `ignore-errors'.  This takes
care of `window-live-p' errors that occur when running the
Emacs detects keys when outside of Emacs.

The RECORD-FLAG and KEYS arguments are passed to
`ergoemacs-command-loop--grow-interactive' for the mouse command."
  (cond
   ((ergoemacs-keymapp command)
    (popup-menu command nil current-prefix-arg))
   (t
    (ignore-errors
      (ergoemacs-command-loop--grow-interactive (ergoemacs-command-loop--modify-mouse-command command) record-flag keys)))))

(defvar ergoemacs-command-loop-describe-key-functions
  '(describe-key describe-function)
  "Functions like `describe-key'.
These functions will:
- Replace `key-description' with `ergoemacs-key-description'.
- Replace `read-key-sequence' with `ergoemacs-command-loop--read-key-sequence'.")

(defcustom ergoemacs-comand-loop-grow-max-sizes-p t
  "Grow the max sizes if needed.
This grows `max-specpdl-size' and `max-lisp-eval-depth' if
`ergoemacs-command-loop--call-interactively' throws an error
about `max-specpdl-size' or `max-lisp-eval-depth'.

The overall maximum that these are set to are controlled by
`ergoemacs-max-specpdl-size' and
`ergoemacs-max-lisp-eval-depth.'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-command-loop--grow-command nil)
(defvar ergoemacs-command-loop--grow-record nil)
(defvar ergoemacs-command-loop--grow-keys nil)
(defvar ergoemacs-command-loop--grow-special nil)


(defcustom ergoemacs-max-specpdl-size (* 8 max-specpdl-size)
  "Maximum `max-specpdl-size' that `ergoemacs-mode' increases to..."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-max-lisp-eval-depth (* 8 max-lisp-eval-depth)
  "Maximum `max-lisp-eval-depth' that `ergoemacs-mode' increases to..."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-command-loop-dont-grow-commands
  '(org-agenda)
  "List of commands where the command loop will not adjust sizes."
  :type '(repeat (sexp :tag "Command"))
  :group 'ergoemacs-mode)

(defun ergoemacs-command-loop--grow-interactive (command &optional record-flag keys)
  "Call the COMMAND interactively.
The RECORD-FLAG and KEYS are sent to `ergoemacs--real-call-interactively'.

This will grow `max-lisp-eval-depth' and `max-specpdl-size' if
needed (and resotre them to the original values)."
  (setq ergoemacs-command-loop--grow-command nil
	  ergoemacs-command-loop--grow-record nil
	  ergoemacs-command-loop--grow-keys nil
	  ergoemacs-command-loop--grow-special nil)
  (if (memq command ergoemacs-command-loop-dont-grow-commands)
      (call-interactively command record-flag keys)
    (let ((grow-max-lisp-p t)
	  (orig-max-specpdl-size max-specpdl-size)
	  (orig-max-lisp-eval-depth max-lisp-eval-depth))
      (while grow-max-lisp-p
	(condition-case err
	    (cond
	     (ergoemacs-command-loop--grow-command
	      (command-execute ergoemacs-command-loop--grow-command
			       ergoemacs-command-loop--grow-record
			       ergoemacs-command-loop--grow-keys
			       ergoemacs-command-loop--grow-special)
	      (setq grow-max-lisp-p nil))
	     (t
	      (call-interactively command record-flag keys)
	      (setq grow-max-lisp-p nil)))
	  (error
	   (if (and (consp err)
		    (eq (car err) 'error)
		    (stringp (nth 1 err))
		    (string-match "max-specpdl-size\\|max-lisp-eval-depth"
				  (nth 1 err))
		    ergoemacs-comand-loop-grow-max-sizes-p
		    (<= max-specpdl-size ergoemacs-max-specpdl-size)
		    (<= max-lisp-eval-depth ergoemacs-max-lisp-eval-depth))
	       (progn
		 (setq max-specpdl-size (* 2 max-specpdl-size)
		       max-lisp-eval-depth (* 2 max-lisp-eval-depth))
		 (ergoemacs-warn "Increased max-specpdl-size to %s and max-lisp-eval-depth to %s for %s"
				 max-specpdl-size max-lisp-eval-depth command))
	     (setq grow-max-lisp-p nil
		   max-specpdl-size orig-max-specpdl-size
		   max-lisp-eval-depth orig-max-lisp-eval-depth)
	     (if (and err (consp err))
		 (signal (car err) (cdr err))
	       (signal err "Unknown error"))))))
      (setq max-specpdl-size orig-max-specpdl-size
	    max-lisp-eval-depth orig-max-lisp-eval-depth))))


(defun ergoemacs-command-loop--call-interactively (command &optional record-flag keys)
  "Call the COMMAND interactively.  Also handle mouse events (if possible.)
The RECORD-FLAG and KEYS are sent to `ergoemacs-command-loop--grow-interactive'."
  (ergoemacs-command-loop--sync-point)
  (setq ergoemacs-last-command-was-ergoemacs-ignore-p nil)
  (cond
   ((and (eventp last-command-event)
         (consp last-command-event)
	 (memq (event-basic-type (car last-command-event))
		   '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7 mouse-8 mouse-9)))
    (ergoemacs-command-loop--call-mouse-command command record-flag keys))
   ((and (symbolp command) (not (fboundp command)))
    (ergoemacs-command-loop--message "Command `%s' is not found" command))
   ((and (symbolp command) (not (commandp command)))
    (ergoemacs-command-loop--message "Command `%s' cannot be called from a key" command))
   ((and (consp ergoemacs-command-loop-describe-key-functions)
	 (memq command ergoemacs-command-loop-describe-key-functions))
    (ergoemacs-specials
     (ergoemacs-command-loop--grow-interactive command record-flag keys)))
   (t
    (ergoemacs-command-loop--grow-interactive command record-flag keys))))


(defun ergoemacs-command-loop-start ()
  "Start `ergoemacs-command-loop'."
  (interactive)
  ;; Should work...
  (unless ergoemacs-command-loop-start
    (setq ergoemacs-command-loop-start t))
  (ergoemacs-command-loop))

(defvar ergoemacs-command-loop--spinner nil)
(defvar ergoemacs-command-loop--spinner-i nil)
(defvar ergoemacs-command-loop--spinner-list nil)
(defvar ergoemacs-command-loop-spinner)
(defvar ergoemacs-command-loop-spinners)

(defvar ergoemacs-command-loop--spinner-display-message nil
  "Use spinner messages with history.")

(defvar ergoemacs-command-loop--message-log-max nil
  "Determine `message-log-max' for `ergoemacs-command-loop--message'.")

(defcustom ergoemacs-message-level :start
  "Message Level for `ergoemacs-mode'."
  :type '(choice
	  (const :tag "No ergoemacs-mode messages" nil)
	  (const :tag "New ergoemacs-mode messages" :new)
	  (const :tag "Mesages on startup" :start)
	  (const :tag "Maximum debugging messages" :max))
  :group 'ergoemacs-mode)

(defvar ergoemacs-command-loop--spinner-display :max
  "Variable to control level of spinner display.")

(defun ergoemacs-command-loop--spinner-display (&optional string &rest args)
  "Spinner display.

Display STRING with a spinner pre-pended.  Additional
arguments (ARGS) will be applied with `format'.

STRING can also be a list of strings.  The string selected for
use with `format' will be selecting using
`ergoemacs-key-description--unicode-char'.

STRING can also be a symbol representing the level of message to
be displayed.  This is used in conjunction with
`ergoemacs-message-level' to only display messages that should be
displayed.  When the message should be displayed addional ARGS
are then passed to `ergoemacs-command-loop--spinner-display'
instead of `format'."
  (prog1 t
    (if (symbolp string)
	(cond
	 ((eq ergoemacs-message-level :max)
	  (apply #'ergoemacs-command-loop--spinner-display args))
	 ((and (eq string :start) ergoemacs-mode-started-p))
	 ((and (eq string :start) (not ergoemacs-mode-started-p))
	  (setq ergoemacs-message-level :max)
	  (unwind-protect
	      (apply #'ergoemacs-command-loop--spinner-display args))
	  (setq ergoemacs-message-level :start))
	 ((and (eq string :new)
	       (memq ergoemacs-message-level '(:new)))
	  (setq ergoemacs-command-loop--spinner-display :new)
	  (unwind-protect
	      (apply #'ergoemacs-command-loop--spinner-display args))
	  (setq ergoemacs-command-loop--spinner-display :max)))
      (when (eq ergoemacs-message-level ergoemacs-command-loop--spinner-display)      
	(let* ((string (or (and (listp string)
				(eq (car string) 'quote)
				(eval string))
			   string))
	       (rest (or (and (listp string)
			      (concat " " (apply #'format (apply #'ergoemacs-key-description--unicode-char string) args)))
			 (and (not string) "")
			 (concat " " (apply #'format string args))))
	       (ergoemacs-command-loop--message-log-max (and ergoemacs-command-loop--spinner-display-message message-log-max)))
	  (when (not ergoemacs-command-loop--spinner-list)
	    (setq ergoemacs-command-loop--spinner-list (nth 1 (assoc ergoemacs-command-loop-spinner ergoemacs-command-loop-spinners))
		  ergoemacs-command-loop--spinner-i 0))
	  (ergoemacs-command-loop--message "%s%s" (nth (mod (setq ergoemacs-command-loop--spinner-i (+ 1 ergoemacs-command-loop--spinner-i))
							    (length ergoemacs-command-loop--spinner-list)) ergoemacs-command-loop--spinner-list)
					   rest))))))

(defun ergoemacs-command-loop--spinner-end ()
  "Cancel the `ergoemacs-command-loop--spinner' timer."
  (when ergoemacs-command-loop--spinner
    (cancel-timer ergoemacs-command-loop--spinner)
    (setq ergoemacs-command-loop--spinner-list nil
          ergoemacs-command-loop--spinner nil
          ergoemacs-command-loop--spinner-i nil)))

(defvar ergoemacs-command-loop--this-command-keys (symbol-function 'this-command-keys))
(defun ergoemacs-command-loop--this-command-keys ()
  "Return `ergoemacs-command-loop--single-command-keys'.
Used to replace:

- `this-command-keys-vector'
- `this-command-keys'
- `this-single-command-keys'
- `this-single-command-raw-keys'

Currently these are all vectors and all ingore prefix arguments.
They don't exactly behave like their Emacs equivalents."
  (or (and ergoemacs-mode ergoemacs-command-loop--single-command-keys)
      (funcall ergoemacs-command-loop--this-command-keys)))

(defvar ergoemacs-command-loop--timer nil
  "Timer to startup `ergoemacs-mode' command loop.")
(defun ergoemacs-command-loop--timer ()
  "Start `ergoemacs-command-loop--internal' if not currently running."
  (unless (and (ergoemacs-command-loop-full-p)
               (ergoemacs-command-loop-p))
    (ergoemacs-command-loop--internal)))

(defun ergoemacs-command-loop--install-timer ()
  "Install the `ergoemacs-command-loop--timer'."
  (setq ergoemacs-command-loop--timer
        (run-with-idle-timer 0.05 nil #'ergoemacs-command-loop--timer)))

(defun ergoemacs-command-loop--remove-timer ()
  "Remove `ergoemacs-command-loop--timer'."
  (when ergoemacs-command-loop--timer
    (cancel-timer ergoemacs-command-loop--timer)
    (setq ergoemacs-command-loop--timer nil)))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--install-timer)
(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--remove-timer)

(defun ergoemacs-command-loop--ignore (&rest _ignore)
  "Do nothing and return nil.

This function accepts any number of arguments, but ignores them.

Unlike `ignore', this command pretends `ergoemacs-command-loop--ignore' command was never
run, by changing `this-command' to `last-command'"
  (interactive)
  (setq ergoemacs-last-command-was-ergoemacs-ignore-p t)
  (ergoemacs-command-loop--execute-modify-command-list last-command)
  ;; FIXME: Somehow change the output of `this-single-command-raw-keys'
  nil)

(defun ergoemacs-command-loop--read-key-sequence (prompt &rest _ignore)
  "Read key sequence in ergoemacs-mode with PROMPT.

Ignore all the other options."
  (let ((old ergoemacs-command-loop-type)
        (old-prompt ergoemacs-command-loop--read-key-prompt)
        ret)
    (setq ergoemacs-command-loop-type :read-key-sequence
          ergoemacs-command-loop--read-key-prompt prompt)
    (unwind-protect
        (setq ret (ergoemacs-command-loop--internal))
      (setq ergoemacs-command-loop-type old
            ergoemacs-command-loop--read-key-prompt old-prompt))
    ret))

(defun ergoemacs-command-loop--internal (&optional key type initial-key-type universal)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal.

INITIAL-KEY-TYPE represents the translation type for the initial KEY.

UNIVERSAL allows ergoemacs-read-key to start with universal
argument prompt.

While in the loop, every command resets the keys typed every time
a command is completed (by `clear-this-command-keys')

Also in the loop, `universal-argument-num-events' is set to
0. (Allows commands like `isearch' to work correctly in older
Emacs versions)."
  (interactive)
  (when ergoemacs-mode
    (ergoemacs-command-loop--execute-rm-keyfreq 'ergoemacs-command-loop)
    ;; Call the startup command
    (when (commandp ergoemacs-command-loop-start)
      (ergoemacs-command-loop--call-interactively ergoemacs-command-loop-start)
      (ergoemacs-command-loop--internal-end-command))
    ;; Replace functions temporarily
    (cl-letf (((symbol-function 'this-command-keys) #'ergoemacs-command-loop--this-command-keys)
	      ((symbol-function 'this-single-command-keys) #'ergoemacs-command-loop--this-command-keys)
	      ((symbol-function 'this-command-keys-vector) #'ergoemacs-command-loop--this-command-keys)
	      ((symbol-function 'this-single-command-raw-keys) #'ergoemacs-command-loop--this-command-keys)
              ;; ((symbol-function 'read-key-sequence) #'ergoemacs-command-loop--read-key-sequence)
	      )
      (let* ((type (or type :normal))
             (from-start-p ergoemacs-command-loop-start)
             (continue-read t)
             (first-type type)
             raw-key current-key last-current-key
             (translation (ergoemacs-translate--get type))
             (local-keymap (ergoemacs-translate--keymap translation))
             modal-p
             tmp command)
        (unwind-protect
            (progn
	      ;; Set these to nil when entering the command loop;
	      ;;
	      ;; For some reason `inhibit-point-motion-hooks' on emacs
	      ;; 25.1 is t when the command loop is entered.
	      ;;
	      ;; To allow the point motion hooks to work as
	      ;; advertised, set these on starting the command loop.
	      (setq inhibit-point-motion-hooks      nil
		    disable-point-adjustment        nil
		    global-disable-point-adjustment nil)
              ;; Setup initial unread command events, first type and history
              (setq tmp (ergoemacs-command-loop--listify-key-sequence key initial-key-type)
                    unread-command-events (or (and unread-command-events tmp (append tmp unread-command-events)) tmp)
                    ergoemacs-command-loop--first-type first-type
                    ergoemacs-command-loop--history nil
                    ergoemacs-command-loop-start nil)
              (while continue-read
		(setq ergoemacs-last-command-was-ergoemacs-ignore-p nil)
                (unless (eq ergoemacs-command-loop-type :read-key-sequence)
                  (setq inhibit-quit t))
                (while continue-read
		  (setq ergoemacs-last-command-was-ergoemacs-ignore-p nil)
                  ;; Read key
                  (setq ergoemacs-command-loop--single-command-keys current-key
                        ergoemacs-command-loop--current-type type
                        ergoemacs-command-loop--universal universal
                        raw-key (ergoemacs-command-loop--read-key
                                 current-key
                                 (or (and unread-command-events :normal) type)
                                 (and (not unread-command-events) universal))
                        ergoemacs-command-loop--single-command-keys nil
                        universal-argument-num-events 0
                        last-current-key current-key
                        current-key (nth 1 raw-key)
                        raw-key (nth 0 raw-key)
                        continue-read nil)
		  (when (setq modal-p (ergoemacs :modal-p))
		    (setq local-keymap (ergoemacs-translation-struct-keymap-modal modal-p)))
                  (cond
                   ;; Handle quit commands
                   ((and last-current-key
                         (or (lookup-key ergoemacs-map--quit-map raw-key)
                             (and (equal raw-key [27])
                                  (lookup-key ergoemacs-map--quit-map [escape]))))
                    (ergoemacs-command-loop--message
                     "Key sequence %s aborted by %s"
                     (ergoemacs-key-description last-current-key)
                     (ergoemacs-key-description raw-key))
                    (setq quit-flag t))
                   ;; Handle local commands.
                   ((and (or modal-p
                             (not (equal current-key raw-key)))
                         (setq command (lookup-key local-keymap raw-key))
                         (not (ergoemacs-keymapp command)) ;; Ignore locally
                         ;; Already handled by `ergoemacs-command-loop--read-key'
                         (not (ergoemacs-gethash command ergoemacs-command-loop--next-key-hash))
                         ;; If a command has :ergoemacs-local property of :force, don't
                         ;; worry about looking up a key, just run the function.
                         (or modal-p
                             (and (symbolp command) (eq (get command :ergoemacs-local) :force))
                             (not (key-binding current-key t))))
                    (pop ergoemacs-command-loop--history) ;; Don't recored local events
                    (setq ergoemacs-command-loop--single-command-keys last-current-key
                          universal-argument-num-events 0
                          ergoemacs-command-loop--current-type type
                          ergoemacs-command-loop--universal universal
                          ergoemacs-command-loop--exit nil)
                    
                    (unless (eq ergoemacs-command-loop-type :test)
                      (setq tmp this-command
                            this-command command)
                      (ergoemacs-command-loop--call-interactively this-command)
                      (setq command this-command
                            this-command tmp))
                    
                    ;; If the command changed anything, fix it here.
                    (unless (equal type ergoemacs-command-loop--current-type)
                      (setq type ergoemacs-command-loop--current-type
                            translation (ergoemacs-translate--get type)
                            local-keymap (ergoemacs-translate--keymap translation)))
                    
                    (setq current-key ergoemacs-command-loop--single-command-keys
                          universal ergoemacs-command-loop--universal
                          ergoemacs-command-loop--single-command-keys nil
                          continue-read (not ergoemacs-command-loop--exit)))
                   
                   ;; Handle any keys that are bound in some translatable way.
                   ((setq command (ergoemacs-command-loop--key-lookup current-key))
                    ;; Setup external indicators of how the loop currently behaves.
                    (setq ergoemacs-command-loop--single-command-keys current-key
                          universal-argument-num-events 0
                          ergoemacs-command-loop--current-type type
                          ergoemacs-command-loop--universal nil
                          ergoemacs-command-loop--exit t)
                    (if (setq continue-read (and (not (and (consp (aref current-key 0))
							   (memq (event-basic-type (car (aref current-key 0)))
								 '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7 mouse-8 mouse-9))))
						 (ergoemacs-keymapp command)))
                        (setq universal nil)
                      (unless (memq ergoemacs-command-loop-type '(:test :read-key-sequence))
                        (with-local-quit
                          (ergoemacs-command-loop--execute command)))
                      
                      (when quit-flag
                        (ergoemacs-command-loop--message "Quit!"))
                      
                      ;; Change any information (if needed)
                      (unless (equal type ergoemacs-command-loop--current-type)
                        (setq type ergoemacs-command-loop--current-type
                              translation (ergoemacs-translate--get type)
                              local-keymap (ergoemacs-translate--keymap translation)))

                      (when (eq ergoemacs-command-loop-type :read-key-sequence)
                        (setq ergoemacs-command-loop--exit t
                              continue-read nil
                              command current-key))
                      
                      (setq current-key ergoemacs-command-loop--single-command-keys 
                            universal ergoemacs-command-loop--universal
                            ergoemacs-command-loop--single-command-keys nil
                            continue-read (not ergoemacs-command-loop--exit)
                            current-prefix-arg (if ergoemacs-command-loop--universal current-prefix-arg prefix-arg))
                      
                      (when (and (not continue-read)
                                 (eq ergoemacs-command-loop--exit :ignore-post-command-hook))
                        (setq continue-read t)))
                    
                    (when (or (not ergoemacs-command-loop--exit)
                              (and (not continue-read) (setq continue-read unread-command-events)))
                      (ergoemacs-command-loop--internal-end-command)))
                   (quit-flag
                    (ergoemacs-command-loop--message "Quit!")
                    (setq quit-flag nil
                          type :normal
                          first-type :normal
                          raw-key nil
                          current-key nil
                          translation (ergoemacs-translate--get type)
                          local-keymap (ergoemacs-translate--keymap translation)
                          ergoemacs-command-loop--first-type first-type
                          ergoemacs-command-loop--history nil))
		   ((consp (aref current-key 0))) ;; don't complain about mouse keys
                   (t ;; Command not found exit.
                    (ergoemacs-command-loop--message "Key %s doesn't do anything." (ergoemacs-key-description current-key)))))
                (unless quit-flag
                  (ergoemacs-command-loop--internal-end-command))
                (setq quit-flag nil
                      type :normal
                      continue-read (or unread-command-events (and from-start-p (ergoemacs-command-loop-full-p)))
                      first-type :normal
                      raw-key nil
                      current-key nil
                      translation (ergoemacs-translate--get type)
                      local-keymap (ergoemacs-translate--keymap translation)
                      ergoemacs-command-loop--first-type first-type
                      ergoemacs-command-loop--history nil)
                (when (or (not ergoemacs-mode) (eq :read-key-sequence ergoemacs-command-loop-type))
                  (setq continue-read nil)))
              (setq inhibit-quit nil)))
        command))))

(defcustom ergoemacs-message-in-mode-line t
  "Display ergoemacs information in mode-line."
  :type 'boolean
  :group 'ergoemacs-mode)

(defconst ergoemacs-command-loop--mode-line-format
  '(:eval (ergoemacs-command-loop--update-mode-line)))

(defvar ergoemacs-command-loop--update-mode-line nil
  "Message to display in mode-line.")

(defun ergoemacs-command-loop--update-mode-line ()
  "Update mode line.

This displays `ergoemacs-command-loop--update-mode-line' in the mode line."
  (or (and ergoemacs-command-loop--update-mode-line
           (concat ergoemacs-command-loop--update-mode-line " ")) ""))

(defun ergoemacs-command-loop--mode-line-not-set-p ()
  "Is the `ergoemacs-mode' mode line present?"
  (and (listp mode-line-format)
       (member ergoemacs-command-loop--mode-line-format mode-line-format)))

(defun ergoemacs-command-loop--reset-mode-line ()
  "Reset the mode line."
  (when (and ergoemacs-message-in-mode-line (ergoemacs-command-loop--mode-line-not-set-p))
    (setq mode-line-format (delete ergoemacs-command-loop--mode-line-format mode-line-format))
    (force-mode-line-update)))

(defun ergoemacs-command-loop--refresh-mode-line ()
  "Update mode line."
  (when (and ergoemacs-message-in-mode-line
             (not (ergoemacs-command-loop--mode-line-not-set-p)))
    (setq mode-line-format (cons ergoemacs-command-loop--mode-line-format mode-line-format)))
  (when ergoemacs-message-in-mode-line
    (force-mode-line-update)))

(defun ergoemacs-command-loop--mode-line-message (&rest args)
  "Message in mode-line.
ARGS are applied with `format'."
  (setq ergoemacs-command-loop--update-mode-line
        (apply #'format args))
  (ergoemacs-command-loop--refresh-mode-line)
  (run-with-timer minibuffer-message-timeout nil
                  #'ergoemacs-command-loop--reset-mode-line))

(defvar ergoemacs-command-loop-message-sit-for 3
  "Command loop message sit for.")

(defun ergoemacs-command-loop--message (str &rest args)
  "Message facility for `ergoemacs-mode' command loop.

STR is the formatting string and ARGS are the arguments applied
to the `format' like: (format str args)."
  (setq ergoemacs-command-loop--last-event-time (float-time))
  (cond
   ((string= str ""))
   ((or (minibufferp) isearch-mode)
    (apply #'ergoemacs-command-loop--mode-line-message
           (append (list str) args)))
   (t
    (let ((message-log-max ergoemacs-command-loop--message-log-max))
      (apply #'message (append (list str) args))))))

(defvar ergoemacs-command-loop--temp-message-timer-secs 0.5
  "Timer to ensure minibuffer isn't active.")

(defvar ergoemacs-command-loop--temp-message-timer nil
  "Timer to ensure minibuffer isn't active.")

(defvar ergoemacs-command-loop--temp-message-timer-str nil
  "Message string.")

(defun ergoemacs-command-loop--temp-message-timer-echo ()
  "Echos `ergoemacs-command-loop--temp-message-timer-str' if minibuffer isn't active."
  (if (or (minibufferp) isearch-mode)
      (setq ergoemacs-command-loop--temp-message-timer
	    (run-with-idle-timer ergoemacs-command-loop--temp-message-timer-secs
				 nil #'ergoemacs-command-loop--temp-message-timer-echo))
    (cancel-timer ergoemacs-command-loop--temp-message-timer)
    (let (message-log-max)
      (with-temp-message ergoemacs-command-loop--temp-message-timer-str
	(sit-for (or (and (numberp ergoemacs-command-loop-message-sit-for) ergoemacs-command-loop-message-sit-for) 2))))))

(defun ergoemacs-command-loop--temp-message (str &rest args)
  "Message facility for `ergoemacs-mode' command loop.

STR is the format string
ARGS is the format arguments
These are passed to `format' as (format str args)."
  (setq ergoemacs-command-loop--last-event-time (float-time))
  (cond
   ((string= str ""))
   ((or (minibufferp) isearch-mode)
    (apply #'ergoemacs-command-loop--mode-line-message
           (append (list str) args)))
   (t
    (setq ergoemacs-command-loop--temp-message-timer-str (apply #'format (append (list str) args))
	  ergoemacs-command-loop--temp-message-timer
	  (run-with-idle-timer ergoemacs-command-loop--temp-message-timer-secs
			       nil #'ergoemacs-command-loop--temp-message-timer-echo)))))

;; (2) Key sequence translated to command
(defun ergoemacs-command-loop--message-binding (key &optional lookup translated-key)
  "Optionally messages information about the translation.

KEY is the original key.

LOOKUP is what will be run.

TRANSLATED-KEY is what the assumed key is actually bound."
  (cond
   ((and lookup (ergoemacs-keymapp lookup)))
   ((consp (elt key 0))) ;; Don't message mouse translations
   ((and (or (eq ergoemacs-echo-function :multi-key)
             (not (and translated-key (eq ergoemacs-echo-function :on-translation)))
             (not (eq ergoemacs-echo-function t)))
         (vectorp key) (or (= (length key) 1) ;; Don't message single keys
                           (and (eq 27 (elt key 0)) (= (length key) 2)))))
   ((and lookup
         (or (eq ergoemacs-echo-function t)
             (and translated-key (eq ergoemacs-echo-function :on-translation))
             (eq ergoemacs-echo-function :multi-key)))
    (ergoemacs-command-loop--temp-message "%s%s%s%s"
                                     (ergoemacs-key-description key)
                                     (ergoemacs :unicode-or-alt "→" "->")
                                     lookup
                                     (or (and translated-key
                                              (format " (from %s)" (ergoemacs-key-description translated-key)))
                                         "")))
   ((not lookup)
    (ergoemacs-command-loop--temp-message "%s is undefined!"
                                     (ergoemacs-key-description key)))
   ((and ergoemacs-echo-function
	 (not (or (= (length key) 1) ;; Clear command completing message
		  (and (eq 27 (elt key 0)) (= (length key) 2)))))
    (ergoemacs-command-loop--message ""))))

(defun ergoemacs-command-loop--key-lookup (key)
  "Find the KEY's function based on current bindings.

If `ergoemacs-mode' has translated this, make Emacs think you
pressed the translated key by changing
`ergoemacs-command-loop--single-command-keys'."
  (if (and (vectorp key)
	   (consp (aref key 0))
	   (memq (event-basic-type (car (aref key 0)))
		 '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5 mouse-6 mouse-7 mouse-8 mouse-9)))
      (let* ((event (aref key 0))
	     (posn (car (cdr last-command-event)))
	     (area (and posn (ergoemacs-posnp posn) (posn-area posn)))
	     (obj (and posn (ergoemacs-posnp posn) (posn-object posn)))
	     (original-command (key-binding key t))
	     command tmp)
	;; From `read-key-sequence':
	;; /* Clicks in non-text areas get prefixed by the symbol
	;; in their CHAR-ADDRESS field.  For example, a click on
	;; the mode line is prefixed by the symbol `mode-line'.
	;; Furthermore, key sequences beginning with mouse clicks
	;; are read using the keymaps of the buffer clicked on, not
	;; the current buffer.  So we may have to switch the buffer
	;; here.
	;; When we turn one event into two events, we must make sure
	;; that neither of the two looks like the original--so that,
	;; if we replay the events, they won't be expanded again.
	;; If not for this, such reexpansion could happen either here
	;; or when user programs play with this-command-keys.  */

	;;
	;; /* Arrange to go back to the original buffer once we're
	;; done reading the key sequence.  Note that we can't
	;; use save_excursion_{save,ore} here, because they
	;; save point as well as the current buffer; we don't
	;; want to save point, because redisplay may change it,
	;; to accommodate a Fset_window_start or something.  We
	;; don't want to do this at the top of the function,
	;; because we may get input from a subprocess which
	;; wants to change the selected window and stuff (say,
	;; emacsclient).  */
	(when area
	  (setq command (key-binding (vconcat (list area event)) t))
	  (when (and obj (consp obj)
		     (setq tmp (ignore-errors (get-text-property (cdr obj)  'local-map (car obj))))
		     (setq tmp (or (and (symbolp tmp) (ergoemacs-sv tmp)) tmp))
		     (ergoemacs-keymapp tmp)
		     (setq tmp (lookup-key tmp (vconcat (list area event)))))
	    (setq command tmp)))
	(unless command
	  (setq command original-command))
	;; (ergoemacs-command-loop--call-mouse-command command record-flag keys)
	
	command)
    ;; Make sure to lookup the keys in the selected buffer
    (ergoemacs-command-loop--sync-point)
    (let ((trials (ergoemacs-translate--trials key))
        tmp tmp2 ret)
    (setq this-command-keys-shift-translated nil)
    (catch 'found-command
      (dolist (cur-key trials)
        (when cur-key
          (let* ((orig-key cur-key)
                 (bind (key-binding orig-key t))
                 (meta-key (ergoemacs-translate--meta-to-escape cur-key))
                 (esc-key (ergoemacs-translate--escape-to-meta cur-key))
                 (new-key (or meta-key esc-key))
                 (new-binding (and new-key (key-binding new-key)))
                 (global (and new-key
                              (list (lookup-key ergoemacs-keymap orig-key t)
                                    (lookup-key ergoemacs-keymap new-key t)))))
            ;; Prefer non-global keys.
	    (when (eq bind 'undefined)
	      (setq bind nil))
	    (when (eq new-binding 'undefined)
	      (setq new-binding nil))
            (cond
             ((not new-key)
              (setq new-key orig-key))
             ((not (memq bind global))
              (setq new-key orig-key))
             ((and new-binding (not (memq new-binding global)))
              (setq bind new-binding)))
            (unless bind
              (cond
               ((or (ergoemacs-keymapp (setq tmp (lookup-key input-decode-map orig-key)))
                    (and (not (integerp tmp)) (commandp tmp)))
                (setq bind tmp))
               ((or (ergoemacs-keymapp (setq tmp (lookup-key local-function-key-map orig-key)))
                    (and (not (integerp tmp)) (commandp tmp)))
                (setq bind tmp))
               ((or (ergoemacs-keymapp (setq tmp (lookup-key key-translation-map orig-key)))
                    (and (not (integerp tmp)) (commandp tmp)))
                (setq bind tmp))))
            (when (and orig-key
                       (setq ret bind
                             ret (if (and (eq ret 'ergoemacs-map-undefined)
                                          (equal orig-key (nth 0 trials))
                                          (nth 1 trials)) nil ret)))
              (cond
               ((equal orig-key (nth 0 trials))
                (setq ergoemacs-command-loop--single-command-keys new-key)
		;; (message "History %s" (length ergoemacs-command-loop--history))
                (when (and (not (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
			   (ergoemacs-keymapp ret)
                           (setq tmp (lookup-key ret [ergoemacs-timeout])))
                  (cond
                   ((eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                    (setq ret tmp))
		   ((< 1  (length ergoemacs-command-loop--history)))
                   ((not (region-active-p))) ;; its a key sequence.
                   
                   ((and this-command-keys-shift-translated
                         (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)))

                   ;; Immediate
                   ((and (not ergoemacs-ctl-c-or-ctl-x-delay)
			 (eq ergoemacs-handle-ctl-c-or-ctl-x 'both))
                    (setq ret tmp))
                   
                   (t ;; with delay
		    (if ergoemacs-command-loop--decode-event-timeout-p
			(setq tmp2 nil
			      ergoemacs-command-loop--decode-event-timeout-p nil))
                    (setq tmp2 (with-timeout (ergoemacs-ctl-c-or-ctl-x-delay nil)
                                 (ergoemacs-command-loop--read-event nil key)))
                    (if (not tmp2)
                        (setq ret tmp) ;; timeout, use copy/cut
                      ;; Actual key
                      (setq ret (ergoemacs-command-loop--key-lookup (vconcat key (vector tmp2))))))))
                (ergoemacs-command-loop--message-binding new-key ret))
               ((equal orig-key (nth 1 trials)) ;; `ergoemacs-mode' shift translation
                (setq this-command-keys-shift-translated t
                      ergoemacs-command-loop--single-command-keys (nth 0 trials))
                
                ;; Shift+Control+c
                (when (and (ergoemacs-keymapp ret)
                           (setq tmp (lookup-key ret [ergoemacs-timeout]))
                           (eq ergoemacs-handle-ctl-c-or-ctl-x 'both))
                  (setq ret tmp))
                (ergoemacs-command-loop--message-binding new-key ret key))
               (t
                (ergoemacs-command-loop--message-binding new-key ret key)
                (setq ergoemacs-command-loop--single-command-keys new-key)))
              (throw 'found-command ret))))))
    ret)))

(defun ergoemacs-command-loop--execute-handle-shift-selection (function)
  "Allow `ergoemacs-mode' command loop to handle shift selection.

This will apply `handle-shift-selection' when FUNCTION is
considered a shift-selection compatible function.

This allows shift-selection of non-letter keys.
For instance in QWERTY M-> is shift translated to M-."
  (when (ergoemacs :movement-p function)
    (handle-shift-selection)))

(defun ergoemacs-command-loop--execute-rm-keyfreq (command)
  "Remove COMMAND from `keyfreq-mode' counts."
  (when (featurep 'keyfreq)
    (when keyfreq-mode
      (let (count)
        (setq count (ergoemacs-gethash (cons major-mode command) keyfreq-table))
        (cond
         ((not count))
         ((= count 1)
          (remhash (cons major-mode command) keyfreq-table))
         (count
          (puthash (cons major-mode command) (- count 1)
                   keyfreq-table)))
        ;; Add local-fn to counter.
        (setq count (ergoemacs-gethash (cons major-mode command) keyfreq-table))
        (puthash (cons major-mode command) (if count (+ count 1) 1)
                 keyfreq-table)))))

;; (3) execute command
(defun ergoemacs-command-loop--execute (command &optional keys)
  "Execute COMMAND pretending that KEYS were pressed."
  (unwind-protect
      (let ((keys (or keys ergoemacs-command-loop--single-command-keys)))
        ;; (ergoemacs-command-loop--spinner)
        (cond
         ((or (stringp command) (vectorp command))
          ;; If the command is a keyboard macro (string/vector) then execute
          ;; it by adding it to `unread-command-events'
          (let ((tmp (prefix-numeric-value current-prefix-arg)))
            (cond
             ((<= tmp 0) ;; Unsure what to do here.
              (ergoemacs-command-loop--message "The %s keyboard macro was not run %s times" (ergoemacs-key-description (vconcat command)) tmp))
             (t
              (dotimes (_i tmp unread-command-events)
                (setq unread-command-events
                      (append (listify-key-sequence command)
                              unread-command-events))))))
          (setq ergoemacs-command-loop--single-command-keys nil))
         (t
          ;; This should be a regular command.
          
          ;; Remove counting of `this-command' in `keyfreq-mode'
          ;; Shouldn't be needed any more...
          ;; (ergoemacs-command-loop--execute-rm-keyfreq this-command)
          
          ;; This command execute should modify the following variables:
          ;; - `last-repeatable-command'
          ;; - `this-command'
          ;; - `this-original-command'
          
          ;; In addition, other minor modes may store the command, so these
          ;; should be modified as well.
          
          ;; These are stored in `ergoemacs-command-loop--execute-modify-command-list'

	  (ergoemacs-command-loop--execute-modify-command-list command)
          
          ;; Handle Shift Selection
          (ergoemacs-command-loop--execute-handle-shift-selection this-command)
          (when keys
            (setq ergoemacs-command-loop--single-command-keys keys)
            
            ;; Modify the output for these functions when `keys' is not nil.
            
            ;; Assume this is a nonmenu event if it isn't a mouse event
            (unless (consp last-command-event)
              (setq last-nonmenu-event last-command-event)))
          (unwind-protect
              (progn
                (setq ergoemacs-command-loop--running-pre-command-hook-p t)
                (run-hooks 'pre-command-hook))
            (setq ergoemacs-command-loop--running-pre-command-hook-p nil))
          (unwind-protect
              (ergoemacs-command-loop--call-interactively this-command t)
            (setq ergoemacs-command-loop--single-command-keys nil)))))
    ;; (ergoemacs-command-loop--spinner-end)
    ))

(provide 'ergoemacs-command-loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-command-loop.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
