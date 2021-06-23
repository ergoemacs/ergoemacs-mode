;;; ergoemacs-command-loop.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2016  Free Software Foundation, Inc.

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


(declare-function guide-key/close-guide-buffer "guide-key")
(declare-function guide-key/popup-function "guide-key")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")

(declare-function ergoemacs-mode-line "ergoemacs-mode")

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
(defvar ergoemacs-mode-started-p)
(defvar guide-key/guide-key-sequence)
(defvar keyfreq-mode)
(defvar keyfreq-table)


(defvar universal-argument-num-events) ;; Not in Emacs 24.5



(defvar ergoemacs-command-loop--mark-active nil
  "Determines if mark was active before ergoemacs command loop.")


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

(defvar ergoemacs-command-loop-swap-translation)
(defvar ergoemacs-command-loop-time-before-blink)
(defvar ergoemacs-command-loop-blink-character)
(defvar ergoemacs-command-loop-blink-rate)
(defvar ergoemacs-mode)
(defvar ergoemacs-command-loop-type)
(defvar ergoemacs-keymap)


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
                         (or (and blink-on ergoemacs-command-loop-blink-character)
                             " "))
                    " ")
                   (or
                    (and (not universal) "")
                    "▸")))
      (format
       "%s%s%s %s "
       (cond
        ((listp current-prefix-arg)
         (make-string (round (log (nth 0 current-prefix-arg) 4)) ?u))
        (t current-prefix-arg))
       (or (and (not universal) "")
           (and ergoemacs-command-loop-blink-character
                (or (and blink-on ergoemacs-command-loop-blink-character)
                    " "))
           " ")
       (or (and (listp current-prefix-arg)
                (format "%s" current-prefix-arg))
           "")
       "▸")))

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
		      ((not (stringp prompt)))
		      ((not (string= "" ergoemacs-command-loop--read-key-prompt)) prompt)
		      ((or (string= prompt " ")
			   (string-match-p prompt (concat " *" ergoemacs-command-loop-blink-character " *")))
		       nil)
		      (ergoemacs-command-loop--universal prompt)
		      (ergoemacs-command-loop--echo-keystrokes-complete prompt)
		      ((not (numberp ergoemacs-command-loop-echo-keystrokes)) prompt)
		      ((= 0 ergoemacs-command-loop-echo-keystrokes) prompt)
		      ((< last-event-time ergoemacs-command-loop-echo-keystrokes) nil)
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

(defun ergoemacs-command-loop--key-msg (blink-on universal text current-key unchorded trans keys)
  "Key message.

BLINK-ON is the flag for if the blink is on

UNIVERSAL is if the prompt is in the universal argument.

TEXT for prompting.

CURRENT-KEY Current key.

UNCHORDED is this a unchorded key?

TRANS translation information.

KEYS is the keys information"
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
                  (or (and blink-on ergoemacs-command-loop-blink-character)
                      " "))
             " ")
         trans
         keys)))

(defvar erogemacs-command--echo-timer nil)
(defvar ergoemacs-command--blink-on nil)
(defvar ergoemacs-orig-echo-keystrokes nil)

(defvar ergoemacs-command--timeout-timer nil)
(defvar ergoemacs-command--timeout-keys nil)

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

(defvar ergoemacs-command-loop--minibuffer-unsupported-p nil)

(defvar ergoemacs-last-command-was-ergoemacs-ignore-p nil
  "Last command was `ergoemacs-ignore'.")

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
  (ergoemacs-save-buffer-state
   (set (make-local-variable 'ergoemacs-command-loop--point-motion-last-point) (point))))

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

(defvar ergoemacs-command-loop-describe-key-functions
  '(describe-key describe-function)
  "Functions like `describe-key'.
These functions will:
- Replace `key-description' with `ergoemacs-key-description'.
- Replace `read-key-sequence' with `ergoemacs-command-loop--read-key-sequence'.")

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
							    (length ergoemacs-command-loop--spinner-list))
						       ergoemacs-command-loop--spinner-list)
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

(provide 'ergoemacs-command-loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-command-loop.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
