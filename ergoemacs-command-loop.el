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


(defvar ergoemacs-command-loop--universal-functions '(universal-argument ergoemacs-universal-argument ergoemacs-command-loop--universal-argument)
  "List of `ergoemacs-mode' recognized functions.")

(define-obsolete-variable-alias 'ergoemacs-universal-fns 'ergoemacs-command-loop--universal-functions)

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

(defvar ergoemacs-command-loop--next-key-hash (make-hash-table)
  "Hash table of how functions force the unchorded next key translation to behave.")

(dolist (arg '((next-key-is-alt (meta))
               (next-key-is-meta (meta))
               (next-key-is-ctl (control))
               (next-key-is-control (control))
               (next-key-is-alt-ctl (control meta))
               (next-key-is-ctl-alt (control meta))
               (next-key-is-control-meta (control meta))
               (next-key-is-meta-control (control meta))
               (next-key-is-quoted nil)))
  (eval (macroexpand
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

(defun ergoemacs-command-loop--undo-last ()
  "Function to undo the last key-press.
This is actually a dummy function.  The actual work is done in `ergoemacs-command-loop--undo-last'"
  (interactive)
  (warn "This is a dummy function called by `ergoemacs-command-loop'"))

(defalias 'ergoemacs-read-key-undo-last 'ergoemacs-command-loop--undo-last)

(defun ergoemacs-command-loop--force-undo-last ()
  "Function to undo the last key-press.
Unlike `ergoemacs-command-loop--undo-last', this ignores any bindings like \\[backward-kill-sentence]
This is actually a dummy function.  The actual work is done in `ergoemacs-command-loop'"
  (interactive)
  (warn "This is a dummy function called by `ergoemacs-command-loop'"))

(defalias 'ergoemacs-read-key-force-undo-last 'ergoemacs-command-loop--force-undo-last)

(defvar ergoemacs-command-loop--undo-functions
  '(ergoemacs-read-key-undo-last
    ergoemacs-command-loop--undo-last ergoemacs-read-key-force-undo-last ergoemacs-command-loop--force-undo-last)
  "Undo functions recognized by `ergoemacs-mode'")

;; Command Loop

;; (1) Read Key sequence

(defun ergoemacs-command-loop--read-key-help-text-prefix-argument (&optional blink-on universal)
  "Display prefix argument portion of the `ergoemacs-mode' help text."
  (or (and (not current-prefix-arg)
           (concat (or
                    (and (not universal) "")
                    (and ergoemacs-command-loop-blink-character
                         (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-read-blink "-"))
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
                (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-read-blink "-"))
                    " "))
           " ")
       (or (and (listp current-prefix-arg)
                (format "%s" current-prefix-arg))
           "")
       (ergoemacs :unicode-or-alt "▸" ">"))))


(defun ergoemacs-command-loop--read-key-universal (current-key &optional type)
  )

(defvar ergoemacs-command-loop--decode-event-delay 0.01
  "Timeout for `ergoemacs-command-loop--decode-event'.
This is to distinguish events in a terminal, like xterm.

It needs to be less than `ergoemacs-command-loop-blink-rate'.")

(defun ergoemacs-command-loop--ensure-sane-variables ()
  "Makes sure that certain variables won't lock up emacs.

Currently this ensures:
 `ergoemacs-command-loop--decode-event-delay' is less than `ergoemacs-command-loop-blink-rate'."
  (when (>= ergoemacs-command-loop--decode-event-delay ergoemacs-command-loop-blink-rate)
    (warn "ergoemacs-command-loop--decode-event-delay >= ergoemacs-command-loop-blink-rate; Reset to ergoemacs-command-loop-blink-rate / 1000")
    (setq ergoemacs-command-loop--decode-event-delay (/ ergoemacs-command-loop-blink-rate 1000))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--ensure-sane-variables)

(defun ergoemacs-command-loop--decode-event (event keymap)
  "Change EVENT based on KEYMAP.
Used to help with translation keymaps like `input-decode-map'"
  (let* ((new-event event)
         (old-ergoemacs-input ergoemacs-command-loop--unread-command-events)
         new-ergoemacs-input
         (current-key (vector event))
         (test-ret (lookup-key keymap current-key))
         next-key)
    (while (and current-key
                (keymapp test-ret))
      ;; The translation needs more keys...
      (setq tmp nil
            next-key (with-timeout (ergoemacs-command-loop--decode-event-delay nil)
                       (or (pop ergoemacs-command-loop--unread-command-events)
                           (setq last-command-event (read-event)))))
      (when last-command-event ;; Since a key was read, save it to be read later.
        (push last-command-event new-ergoemacs-input))
      (if next-key
          (setq current-key (vconcat current-key (vector next-key))
                test-ret (lookup-key keymap current-key))
        (setq current-key nil)))
    (when (stringp test-ret) 
      (setq test-ret (read-kbd-macro test-ret t)))
    (if (and (vectorp test-ret)
             (= (length test-ret)))
        (setq new-event (elt test-ret 0))
      ;; Not a new event, restore anything that was popped off the
      ;; unread command events.
      (when old-ergoemacs-input
        (setq ergoemacs-command-loop--unread-command-events old-ergoemacs-input)))
    ;; Add anything read to the
    ;; ergoemacs-command-loop--unread-command-events
    (when new-ergoemacs-input
      (setq ergoemacs-command-loop--unread-command-events (append new-ergoemacs-input ergoemacs-command-loop--unread-command-events)))
    new-event))

(defun ergoemacs-command-loop--read-event (&optional current-key)
  "Reads a single event.
This respects `input-decode-map', `local-function-key-map' and `key-translation-map'.

It will timeout after `ergoemacs-command-loop-blink-rate' and return nil."
  (let ((input (with-timeout (ergoemacs-command-loop-blink-rate nil)
                 (or (pop ergoemacs-command-loop--unread-command-events)
                     (setq last-command-event (read-event)))))
        last-input
        binding)
    ;; Fix issues with `input-decode-map'
    (when input
      (setq input (ergoemacs-command-loop--decode-event input input-decode-map)
            binding (key-binding (vconcat current-key (vector input))))
      ;; These should only be replaced if they are not bound.
      (unless binding
        (setq last-input input
              input (ergoemacs-command-loop--decode-event input local-function-key-map))
        (unless (= last-input input)
          (setq binding (key-binding (vconcat current-key (vector input))))))
      (unless binding
        (setq input (ergoemacs-command-loop--decode-event input key-translation-map))))
    input))

(defun ergoemacs-command-loop--read-key (&optional current-key type universal)
  (let* ((universal universal)
         (type (or type :normal))
         (translation (ergoemacs-translate--get type))
         (local-keymap (ergoemacs-translation-struct-keymap translation))
         (text (ergoemacs-translation-struct-text translation))
         (unchorded (ergoemacs-translation-struct-unchorded translation))
         (trans (ergoemacs-translation-struct-translation translation))
         (keys nil)
         (blink-on nil)
         input
         raw-input
         mod-keys)
    ;; (ergoemacs-command-loop--read-key (read-kbd-macro "C-x" t) :unchorded-ctl)
    (when (functionp text)
      (setq text (funcall text)))
    
    (setq trans (or (and trans (concat "\nTranslations: "
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
                             (ergoemacs :unicode-or-alt "→" "->")
                             (ergoemacs :modifier-desc item)))
           (push (elt local-key 0) mod-keys)
           (setq keys (or (and (not keys) tmp)
                          (and keys (concat keys ", " tmp)))))))
     ergoemacs-command-loop--next-key-hash)

    (setq keys (or (and keys (concat "\nKeys: " keys)) ""))
    (setq unchorded (or (and unchorded (concat " " (ergoemacs :modifier-desc unchorded))) ""))
    (while (not input)
      (while (not input)
        (ergoemacs-command-loop--message
         "%s" (concat
               (ergoemacs-command-loop--read-key-help-text-prefix-argument blink-on universal)
               text
               (ergoemacs-key-description current-key)
               unchorded
               ;; Cursor
               (or (and universal "")
                   (and ergoemacs-command-loop-blink-character
                        (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-read-blink "-"))
                            " "))
                   " ")
               trans
               keys))
        (setq blink-on (not blink-on)
              input (ergoemacs-command-loop--read-event current-key)))
      (cond
       ((and (memq input mod-keys)
             (setq trans (gethash (lookup-key local-keymap (vector input)) ergoemacs-command-loop--next-key-hash))
             (or (eq :force (nth 1 trans)) ;; Override any keys
                 (not (key-binding (vconcat current-key (ergoemacs-translate--event-mods input trans)))) ;; Don't use if bound.
                 ))
        (setq trans (nth 0 trans)
              unchorded (concat " " (ergoemacs :modifier-desc trans))
              input nil)
        ;; Changed behavior.
        (while (not input)
          (ergoemacs-command-loop--message
           "%s" (concat
                 (format "#%s" input)
                 (ergoemacs-command-loop--read-key-help-text-prefix-argument blink-on universal)
                 text
                 (ergoemacs-key-description current-key)
                 unchorded
                 ;; Cursor
                 (or (and universal "")
                     (and ergoemacs-command-loop-blink-character
                          (or (and blink-on (ergoemacs :unicode-or-alt ergoemacs-read-blink "-"))
                              " "))
                     " ")
                 "\n"
                 "\n"))
          (setq blink-on (not blink-on)
                input (ergoemacs-command-loop--read-event current-key)))
        (setq raw-input input
              input (ergoemacs-translate--event-mods input trans)))
       (t
        ;; Translate the key appropriately.
        (setq raw-input input
              input (ergoemacs-translate--event-mods input type))))
      (cond
       ((and input (not universal)
             (not (commandp (key-binding (vconcat current-key (vector raw-input))) t))
             (and local-keymap
                  (memq (lookup-key local-keymap (vector raw-input))
                        ergoemacs-command-loop--universal-functions)))
        (setq universal t
              raw-input nil
              input nil))
       ((and raw-input universal) ;; Handle universal arguments.
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
         ((or (memq (key-binding (vconcat current-key (vector input))) ergoemacs-universal-fns)
              (not (commandp (key-binding (vconcat current-key (vector raw-input)))))
              (and local-keymap (memq (lookup-key local-keymap (vector raw-input)) ergoemacs-universal-fns)))
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
    (list (vector raw-input) (vconcat current-key (vector input)))))

(defvar ergoemacs-command-loop--unread-command-events nil
  "List of events that `ergoemacs-command-loop' hasn't read.")


(defun ergoemacs-command-loop--listify-key-sequence (key &optional type)
  "Returns a key sequence from KEY.

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

(defvar ergoemacs-command-loop--current-type nil)
(defvar ergoemacs-command-loop--universal nil)
(defun ergoemacs-command-loop (&optional key type initial-key-type universal)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal.

INITIAL-KEY-TYPE represents the translation type for the initial KEY.

UNIVERSAL allows ergoemacs-read-key to start with universal
argument prompt."
  (let* ((type (or type :normal))
         (continue-read t)
         (first-type type)
         raw-key current-key
         (translation (ergoemacs-translate--get type))
         (local-keymap (ergoemacs-translation-struct-keymap translation))
         command)
    ;; Setup initial unread command events
    (setq ergoemacs-command-loop--unread-command-events (ergoemacs-command-loop--listify-key-sequence key initial-key-type))
    (when unread-command-events
      (setq ergoemacs-command-loop--unread-command-events (append ergoemacs-command-loop--unread-command-events unread-command-events)
            unread-command-events nil))
    
    (while continue-read
      ;; Read key
      (setq raw-key (ergoemacs-command-loop--read-key
                     current-key
                     (or (and ergoemacs-command-loop--unread-command-events :normal) type)
                     (and (not ergoemacs-command-loop--unread-command-events) universal))
            current-key (nth 1 raw-key)
            raw-key (nth 0 raw-key)
            continue-read nil)
      (cond
       ;; Handle local commands.
       ((and (setq command (lookup-key local-keymap raw-key))
             ;; Already handled by `ergoemacs-command-loop--read-key'
             (not (gethash command ergoemacs-command-loop--next-key-hash)) 
             ;; If a command has :ergoemacs-local property of :force, don't
             ;; worry about looking up a key, just run the function.
             (or (eq (get command :ergoemacs-local) :force)
                 (not (key-binding current-key))))

        (setq ergoemacs-command-loop--single-command-keys current-key
              ergoemacs-command-loop--current-type type
              ergoemacs-command-loop--universal universal
              ergoemacs-command-loop--exit nil)
        
        (call-interactively command)
        
        ;; If the command changed anything, fix it here.
        (when (not (equal type ergoemacs-command-loop--current-type))
          (setq type ergoemacs-command-loop--current-type
                translation (ergoemacs-translate--get type)
                local-keymap (ergoemacs-translation-struct-keymap translation)))
        (setq current-key ergoemacs-command-loop--single-command-keys 
              universal ergoemacs-command-loop--universal
              ergoemacs-command-loop--single-command-keys nil
              continue-read (not ergoemacs-command-loop--exit)))
       
       ;; Handle any keys that are bound in some translatable way.
       ((setq command (ergoemacs-command-loop--key-lookup current-key))
        (unless (setq continue-read (ergoemacs-keymapp command))
          (ergoemacs-command-loop--execute command))
        (when (and (not continue-read) (setq continue-read ergoemacs-command-loop--unread-command-events))
          ;; Simulate the end of an emacs command, since we are not
          ;; exiting the loop.
          (run-hooks 'post-command-hook)
          ;; Deactivate mark.
          (when deactivate-mark
            (deactivate-mark))
          ;; After executing, the emacs loop should copy `this-command' into
          ;; `last-command'.
          ;; It should also change `last-prefix-arg'
          (setq last-command this-command
                last-prefix-arg prefix-arg
                this-command nil
                prefix-arg nil
                deactivate-mark nil)))
       (t ;; Command not found exit.
        )))))

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
TRANSLATED-KEY is what the assumed key is actually bound.
KEY is the original key,
LOOKUP is what will be run"
  (cond
   ((and lookup (ergoemacs-keymapp lookup)))
   (lookup
    (ergoemacs-command-loop--message "%s%s%s%s"
                                     (ergoemacs-key-description key)
                                     (ergoemacs :unicode-or-alt "→" "->")
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
              ;; last-command-event (elt keys (- (length keys) 1))
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
