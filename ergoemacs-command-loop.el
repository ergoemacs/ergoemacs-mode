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

(defun ergoemacs-command-loop--setup-quit-key ()
  "Setup the `ergoemacs-mode' quit key."
  (ergoemacs-command-loop--redefine-quit-key (nth 0 (where-is-internal 'keyboard-quit))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--setup-quit-key)
(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--redefine-quit-key)

(defun ergoemacs-command-loop--universal-argument (&optional type)
  "Ergoemacs universal argument.
This is called through `ergoemacs-command-loop'"
  (interactive)
  (cond
   ((not ergoemacs---ergoemacs-command-loop)
    ;; Command loop hasn't started.
    (setq current-prefix-arg '(4))
    (ergoemacs-command-loop nil type nil t))
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
Uses the `ergoemacs-command-loop--history' variable/function."
  (interactive)
  (if ergoemacs-command-loop--history
      (let ((tmp (pop ergoemacs-command-loop--history)))
        (setq ergoemacs-command-loop--single-command-keys (nth 0 tmp)
              ergoemacs-command-loop--current-type (nth 1 tmp)
              ergoemacs-command-loop--universal (nth 2 tmp)
              current-prefix-arg (nth 3 tmp)
              last-command-event (nth 4 tmp)))
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

(defvar ergoemacs-command-loop--undo-functions
  '(ergoemacs-read-key-undo-last
    ergoemacs-command-loop--undo-last ergoemacs-read-key-force-undo-last ergoemacs-command-loop--force-undo-last)
  "Undo functions recognized by `ergoemacs-mode'")


(defun ergoemacs-command-loop--swap-translation ()
  "Function to swap translations.
Uses the `ergoemacs-command-loop-swap-translation' variable."
  (interactive)
  (let ((next-swap (assoc (list first-type current-type) ergoemacs-command-loop-swap-translation)))
    (setq ergoemacs-command-loop--current-type
          (if next-swap
              (nth 1 next-swap)
            :normal))))

(defalias 'ergoemacs-read-key-swap 'ergoemacs-command-loop--swap-translation)

(defvar ergoemacs-command-loop--help-last-key nil)
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
     ;; ((and (boundp 'guide-key-mode) guide-key-mode)
     ;;  (let ((key ergoemacs-command-loop--single-command-keys))
     ;;    (if (equal ergoemacs-command-loop--help-last-key ergoemacs-command-loop--single-command-keys)
     ;;        (progn
     ;;          (setq ergoemacs-command-loop--help-last-key nil
     ;;                guide-key/guide-key-sequence (delete (key-description ergoemacs-command-loop--single-command-keys) guide-key/guide-key-sequence))
     ;;          (guide-key/close-guide-buffer))
     ;;      ;; Not using pushnew because the test is equal and
     ;;      ;; guide-key/guide-key-sequence is a global variable.
     ;;      (add-to-list 'guide-key/guide-key-sequence (key-description ergoemacs-command-loop--single-command-keys)
     ;;                   ergoemacs-command-loop--help-last-key ergoemacs-read-key)
     ;;      (guide-key/popup-function key))
     ;;    t))
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

(define-obsolete-variable-alias 'ergoemacs-read-key-delay 'ergoemacs-command-loop--decode-event-delay)


(defun ergoemacs-command-loop--ensure-sane-variables ()
  "Makes sure that certain variables won't lock up emacs.

Currently this ensures:
 `ergoemacs-command-loop--decode-event-delay' is less than `ergoemacs-command-loop-blink-rate'."
  (when (>= ergoemacs-command-loop--decode-event-delay ergoemacs-command-loop-blink-rate)
    (warn "ergoemacs-command-loop--decode-event-delay >= ergoemacs-command-loop-blink-rate; Reset to ergoemacs-command-loop-blink-rate / 1000")
    (setq ergoemacs-command-loop--decode-event-delay (/ ergoemacs-command-loop-blink-rate 1000))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--ensure-sane-variables)

(defvar ergoemacs-command-loop--history nil
  "History of command loop locations.")
(defvar ergoemacs-command-loop-time-before-blink 2.5)
(defvar ergoemacs-command-loop--last-event-time nil)

(defun ergoemacs-command-loop--combine (current-key next-event)
  "Combine two key and next event."
  (let (tmp)
    (cond
     ((and (setq tmp (elt current-key 0))
           (or (and (consp tmp) (symbolp (setq tmp (car tmp))) (setq tmp (symbol-name tmp)))
               (and (symbolp tmp) (setq tmp (symbol-name tmp))))
           (string-match-p "\\<mouse\\>" tmp))
      (warn "Dropping events %s" current-key)
      (vector next-event))
     (t (vconcat current-key (vector next-event))))))

(defun ergoemacs-command-loop--history (&optional prompt seconds)
  "Read event and add to event history.
Also add to `last-command-event' to allow `self-insert-character' to work appropriately.
I'm not sure the purpose of `last-event-frame', but this is modified as well"
  (or (let ((event (pop unread-command-events)))
        (setq last-command-event event
              last-event-frame (selected-frame))
        event)
      (let* ((last-event-time (or (and ergoemacs-command-loop--last-event-time
                                       (- (float-time) ergoemacs-command-loop--last-event-time))
                                  (and (setq ergoemacs-command-loop--last-event-time (float-time)) 0)))
             (prompt (cond
                      ((or (minibufferp) isearch-mode) nil)
                      ((and (< last-event-time ergoemacs-command-loop-time-before-blink) (string= prompt "")) nil)
                      ((and (< last-event-time ergoemacs-command-loop-time-before-blink) (string= prompt (ergoemacs :unicode-or-alt ergoemacs-read-blink "-"))) nil)
                      (t prompt)))
             ;; Run (with-timeout) so that idle timers will work.
             (event (with-timeout (seconds nil)
                      (read-event prompt)))
             click-event)
        (when (eventp event)
          ;; (setq event (ergoemacs-command-loop--decode-mouse event))
          (unless (consp event) ;; Don't record mouse events
            (push (list ergoemacs-command-loop--single-command-keys 
                        ergoemacs-command-loop--current-type 
                        ergoemacs-command-loop--universal
                        current-prefix-arg
                        last-command-event)
                  ergoemacs-command-loop--history))
          (setq ergoemacs-command-loop--last-event-time (float-time)
                last-command-event event
                last-event-frame (selected-frame)))
        event)))

(defun ergoemacs-command-loop--decode-event (event keymap)
  "Change EVENT based on KEYMAP.
Used to help with translation keymaps like `input-decode-map'"
  (let* ((new-event event)
         (old-ergoemacs-input unread-command-events)
         new-ergoemacs-input
         (current-key (vector event))
         (test-ret (lookup-key keymap current-key))
         next-key)
    (while (and current-key
                (keymapp test-ret))
      ;; The translation needs more keys...
      (setq tmp nil
            next-key (ergoemacs-command-loop--history nil ergoemacs-command-loop--decode-event-delay))
      (when next-key ;; Since a key was read, save it to be read later.
        (push last-command-event new-ergoemacs-input))
      (if next-key
          (setq current-key (ergoemacs :combine current-key next-key)
                test-ret (lookup-key keymap current-key))
        (setq current-key nil)))
    (when (stringp test-ret) 
      (setq test-ret (read-kbd-macro test-ret t)))
    (if (and (vectorp test-ret)
             (= (length test-ret) 1))
        (setq new-event (elt test-ret 0))
      ;; Not a new event, restore anything that was popped off the
      ;; unread command events.
      (when old-ergoemacs-input
        (setq unread-command-events old-ergoemacs-input)))
    ;; Add anything read to the
    ;; unread-command-events
    (when new-ergoemacs-input
      (setq unread-command-events (append new-ergoemacs-input unread-command-events)))
    new-event))

(defun ergoemacs-command-loop--read-event (prompt &optional current-key)
  "Reads a single event.
This respects `input-decode-map', `local-function-key-map' and `key-translation-map'.

It also inputs real read events into the history with `ergoemacs-command-loop--history'

It will timeout after `ergoemacs-command-loop-blink-rate' and return nil."
  (let ((input (ergoemacs-command-loop--history prompt ergoemacs-command-loop-blink-rate))
        last-input
        binding)
    ;; Fix issues with `input-decode-map'
    (when input
      (setq input (ergoemacs-command-loop--decode-event input input-decode-map)
            binding (key-binding (ergoemacs :combine current-key input) t))
      ;; These should only be replaced if they are not bound.
      (unless binding
        (setq last-input input
              input (ergoemacs-command-loop--decode-event input local-function-key-map))
        (unless (eq last-input input)
          (setq binding (key-binding (ergoemacs :combine current-key input) t))))
      (unless binding
        (setq input (ergoemacs-command-loop--decode-event input key-translation-map))))
    input))

(defun ergoemacs-command-loop--read-key (&optional current-key type universal)
  "Read a key for the `ergoemacs-mode' command loop.

This uses `ergoemacs-command-loop--read-event'."
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
        (setq blink-on (not blink-on)
              input (ergoemacs-command-loop--read-event
                     (format
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
                     current-key)))
      (cond
       ((and (memq input mod-keys)
             (setq trans (gethash (lookup-key local-keymap (vector input)) ergoemacs-command-loop--next-key-hash))
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
                       current-key)))
        (setq raw-input input
              input (ergoemacs-translate--event-mods input trans)))
       (t
        ;; Translate the key appropriately.
        (setq raw-input input
              input (ergoemacs-translate--event-mods input type))))
      (cond
       ((and input (not universal)
             (not (commandp (key-binding (ergoemacs :combine current-key raw-input)) t))
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
         ((or (memq (key-binding (ergoemacs :combine current-key input) t) ergoemacs-universal-fns)
              (not (commandp (key-binding (ergoemacs :combine current-key raw-input) t)))
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
    (list (vector raw-input) (ergoemacs :combine current-key input))))

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

(defvar ergoemacs-command-loop--first-type nil)
(defvar ergoemacs-command-loop--current-type nil)
(defvar ergoemacs-command-loop--universal nil)

(defun ergoemacs-command-loop--internal (&optional key type initial-key-type universal)
  "Process `ergoemacs-command-loop' while `ergoemacs-command-loop' is running."
  (interactive)
  (if (and ergoemacs---ergoemacs-command-loop
           (called-interactively-p 'interactive))
      (progn ;; Somehow this wasn't reset correctly...
        (message "Abnormal `ergoemacs-command-loop' exit, reseting `ergoemacs-command-loop'.")
        (ergoemacs-command-loop--reset-functions)
        (ergoemacs-command-loop key type initial-key-type universal))
    (setq ergoemacs-command-loop--exit :ignore-post-command-hook
          ergoemacs---this-single-command-keys (or (and key (read-kbd-macro key t))
                                                   ergoemacs---this-single-command-keys)
          ergoemacs-command-loop--universal (if (and ergoemacs-command-loop--universal (not universal)) nil
                                              universal)
          ergoemacs-command-loop--current-type (or type ergoemacs-command-loop--current-type))))

(defun ergoemacs-command-loop--reset-functions ()
  "Reset functions"
  (interactive)
  (fset 'this-command-keys-vector ergoemacs---this-command-keys-vector)
  (fset 'this-command-keys ergoemacs---this-command-keys)
  (fset 'this-single-command-keys ergoemacs---this-single-command-keys)
  (when ergoemacs---ergoemacs-command-loop
    (fset 'ergoemacs-command-loop ergoemacs---ergoemacs-command-loop)
    (setq ergoemacs---ergoemacs-command-loop nil)))

(add-hook 'pre-command-hook #'ergoemacs-command-loop--reset-functions)

(defvar ergoemacs-command-loop--self-insert-command-count 0)
(defun ergoemacs-command-loop--internal-end-command ()
  "Simulates the end of a command."
  ;; Simulate the end of an emacs command, since we are not
  ;; exiting the loop.
  (run-hooks 'post-command-hook)

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
        last-prefix-arg prefix-arg
        this-command nil
        prefix-arg nil
        deactivate-mark nil)
  
  (undo-boundary))


(defvar ergoemacs-command-loop--start-timer nil
  "Timer to start `ergoemacs-command-loop'")

(defun ergoemacs-command-loop--start-if-necessary ()
  "Starts `ergoemacs-command-loop' if necessary."
  (unless ergoemacs---ergoemacs-command-loop
    (call-interactively 'ergoemacs-command-loop)))

(defun ergoemacs-command-loop--enable-start-timer ()
  "Enable `ergoemacs-command-loop--start-if-necessary' with idle timer."
  (setq ergoemacs-command-loop--start-timer
        (run-with-idle-timer 0.2 nil #'ergoemacs-command-loop--start-if-necessary)))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--enable-start-timer)

(defun ergoemacs-command-loop--disable-start-timer ()
  "Disable `ergoemacs-command-loop'."
  (when ergoemacs-command-loop--start-timer
    (cancel-timer ergoemacs-command-loop--start-timer)
    (setq ergoemacs-command-loop--start-timer nil)))

(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--disable-start-timer)

(defvar ergoemacs-command-loop--mouse-event nil)
(defun ergoemacs-command-loop--call-interactively (command &optional record-flag keys)
  "Call the command interactively.  Also handle mouse events (if possible.)"
  (cond
   ((and (eventp last-command-event)
         (consp last-command-event))
    (let* ((mods (event-modifiers last-command-event))
           (l-event (length last-command-event))
           (posn (ignore-errors (car (cdr last-command-event))))
           (area (and posn (posnp posn) (posn-area posn)))
           (command command)
           (obj (and posn (posnp posn) (posn-object posn)))
           form tmp)
      (when area
        (setq command (key-binding (vconcat (list area last-command-event)) t))
        (when (and obj (setq tmp (get-text-property (cdr obj)  'local-map (car obj)))
                   (setq tmp (lookup-key tmp (vconcat (list area last-command-event)))))
          (setq command tmp)))
      (setq form (and (commandp command t) (interactive-form command)))
      ;; (message "Area: %s; Command: %s; Event: %s" area command last-command-event)
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
      ;; use save_excursion_{save,restore} here, because they
      ;; save point as well as the current buffer; we don't
      ;; want to save point, because redisplay may change it,
      ;; to accommodate a Fset_window_start or something.  We
      ;; don't want to do this at the top of the function,
      ;; because we may get input from a subprocess which
      ;; wants to change the selected window and stuff (say,
      ;; emacsclient).  */
      (cond
       ((keymapp command)
        (popup-menu command nil current-prefix-arg))
       ((not (nth 1 form))
        (call-interactively command record-flag keys))
       
       ((and (stringp (nth 1 form)) (string= "e" (nth 1 form)))
        (call-interactively `(lambda,(cdr (help-function-arglist command t)) (interactive) (funcall ',command last-command-event)) record-flag keys))
       
       ((and (stringp (nth 1 form)) (string= "@e" (nth 1 form)))
        (call-interactively `(lambda,(cdr (help-function-arglist command t)) (interactive) (select-window (posn-window (event-start last-command-event))) (funcall ',command last-command-event)) record-flag keys))
       
       ((and (stringp (nth 1 form)) (string= "@" (nth 1 form)))
        (call-interactively `(lambda,(cdr (help-function-arglist command t)) (interactive) (select-window (posn-window (event-start last-command-event))) (funcall ',command)) record-flag keys))
       
       ((ignore-errors (and (stringp (nth 1 form)) (string= "e" (substring (nth 1 form) 0 1))))
        (call-interactively `(lambda,(cdr (help-function-arglist command t))
                               (interactive ,(substring (nth 1 form) 1) ,@(cdr (cdr form)))
                               (funcall ',command last-command-event ,@(remove-if (lambda(elt) (eq '&optional elt)) (help-function-arglist command t))))
                            record-flag keys))

       ((ignore-errors (and (stringp (nth 1 form)) (string= "@e" (substring (nth 1 form) 0 2))))
        (call-interactively `(lambda,(cdr (help-function-arglist command t))
                               (interactive ,(substring (nth 1 form) 2) ,@(cdr (cdr form)))
                               (funcall ',command last-command-event ,@(remove-if (lambda(elt) (eq '&optional elt)) (help-function-arglist command t))))
                            record-flag keys))
       (t ;; Assume that the "e" or "@e" specifications are not present.
        (call-interactively command record-flag keys)))))
   (t
    (call-interactively command record-flag keys))))


(defvar ergoemacs---this-command-keys-vector (symbol-function 'this-command-keys-vector))
(defvar ergoemacs---this-command-keys (symbol-function 'this-command-keys))
(defvar ergoemacs---this-single-command-keys (symbol-function 'this-single-command-keys))
(defvar ergoemacs---ergoemacs-command-loop nil)

(defun ergoemacs-command-loop--this-command-keys-vector ()
  "`ergoemacs-mode' command loop single command keys"
  ergoemacs-command-loop--single-command-keys)

(defalias 'ergoemacs-command-loop--this-command-keys 'ergoemacs-command-loop--this-command-keys-vector)
(defalias 'ergoemacs-command-loop--this-single-command-keys 'ergoemacs-command-loop--this-command-keys-vector)

(defun ergoemacs-command-loop-persistent-p ()
  "Is the `ergoemacs-mode' command loop persistent?"
  (and ergoemacs-mode (eq ergoemacs-command-loop-type :full)))

(defun ergoemacs-command-loop (&optional key type initial-key-type universal)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: :ctl-to-alt :unchorded :normal.

INITIAL-KEY-TYPE represents the translation type for the initial KEY.

UNIVERSAL allows ergoemacs-read-key to start with universal
argument prompt.

While in the loop, the following functions are modified to
return `ergoemacs-command-loop--single-command-keys':

- `this-command-keys-vector'
- `this-command-keys'
- `this-single-command-keys'

Also in the loop, `universal-argument-num-events' is set to
0. (Allows commands like `isearch' to work correctly).
"
  (interactive)
  (setq ergoemacs---ergoemacs-command-loop (symbol-function 'ergoemacs-command-loop))
  (let* ((type (or type :normal))
         (continue-read t)
         (first-type type)
         raw-key current-key
         (translation (ergoemacs-translate--get type))
         (local-keymap (ergoemacs-translation-struct-keymap translation))
         tmp tmp2 local-quit command)
    (unwind-protect
        (progn
          ;; Replace functions temporarily
          (fset 'ergoemacs-command-loop 'ergoemacs-command-loop--internal)
          (fset 'this-command-keys-vector 'ergoemacs-command-loop--this-command-keys-vector)
          (fset 'this-command-keys 'ergoemacs-command-loop--this-command-keys-vector)
          (fset 'this-single-command-keys 'ergoemacs-command-loop--this-command-keys-vector)
          
          ;; Setup initial unread command events, first type and history
          (setq tmp (ergoemacs-command-loop--listify-key-sequence key initial-key-type)
                unread-command-events (or (and unread-command-events tmp (append tmp unread-command-events))
                                          tmp)
                ergoemacs-command-loop--first-type first-type
                ergoemacs-command-loop--history nil)
          (while (ergoemacs-command-loop-persistent-p)
            (setq inhibit-quit t)
            (while continue-read
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
                    current-key (nth 1 raw-key)
                    raw-key (nth 0 raw-key)
                    continue-read nil)
              (cond
               ;; Handle local commands.
               ((and (setq command (lookup-key local-keymap raw-key t))
                     ;; Already handled by `ergoemacs-command-loop--read-key'
                     (not (gethash command ergoemacs-command-loop--next-key-hash)) 
                     ;; If a command has :ergoemacs-local property of :force, don't
                     ;; worry about looking up a key, just run the function.
                     (or (eq (get command :ergoemacs-local) :force)
                         (not (key-binding current-key t))))
                
                (unless (eq ergoemacs-command-loop-type :test)
                  (ergoemacs-command-loop--call-interactively command))

                (setq ergoemacs-command-loop--single-command-keys current-key
                      universal-argument-num-events 0
                      ergoemacs-command-loop--current-type type
                      ergoemacs-command-loop--universal universal
                      ergoemacs-command-loop--exit nil)
                
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
                ;; Setup external indicators of how the loop currently behaves.
                (setq ergoemacs-command-loop--single-command-keys current-key
                      universal-argument-num-events 0
                      ergoemacs-command-loop--current-type type
                      ergoemacs-command-loop--universal nil
                      ergoemacs-command-loop--exit t)

                (unless (setq continue-read (ergoemacs-keymapp command))
                  (unless (eq ergoemacs-command-loop-type :test)
                    (ergoemacs-command-loop--enable-start-timer)
                    (with-local-quit
                      (ergoemacs-command-loop--execute command))
                    (ergoemacs-command-loop--disable-start-timer))
                  
                  (when quit-flag
                    (ergoemacs-command-loop--message "Quit!"))
                  ;; Change any information (if needed)

                  (when (not (equal type ergoemacs-command-loop--current-type))
                    (setq type ergoemacs-command-loop--current-type
                          translation (ergoemacs-translate--get type)
                          local-keymap (ergoemacs-translation-struct-keymap translation)))
                  (setq current-key ergoemacs-command-loop--single-command-keys 
                        universal ergoemacs-command-loop--universal
                        ergoemacs-command-loop--single-command-keys nil
                        continue-read (not ergoemacs-command-loop--exit)
                        current-prefix-arg (if ergoemacs-command-loop--universal current-prefix-arg nil))
                  (when (and (not continue-read)
                             (eq ergoemacs-command-loop--exit :ignore-post-command-hook))
                    (setq continue-read t)))
                
                (when (or (not ergoemacs-command-loop--exit)
                          (and (not continue-read) (setq continue-read unread-command-events)))
                  (ergoemacs-command-loop--internal-end-command)))
               (t ;; Command not found exit.
                (ergoemacs-command-loop--message "Key %s doesn't do anything." (ergoemacs-key-description current-key)))))
            (unless quit-flag
              (ergoemacs-command-loop--internal-end-command))
            (setq quit-flag nil
                  type :normal
                  continue-read t
                  first-type :normal
                  raw-key nil
                  current-key nil
                  next-key nil
                  translation (ergoemacs-translate--get type)
                  local-keymap (ergoemacs-translation-struct-keymap translation)
                  ergoemacs-command-loop--first-type first-type
                  ergoemacs-command-loop--history nil))
          (setq inhibit-quit nil)))
    (ergoemacs-command-loop--reset-functions)
    command))

(defun ergoemacs-command-loop--message (str &rest args)
  "Message facility for `ergoemacs-mode' command loop"
  (setq ergoemacs-command-loop--last-event-time (float-time))
  (cond
   ;; FIXME, message somewhere when you are in the minibuffer
   ;; ((minibufferp))
   ((string= str ""))
   ((or (minibufferp) isearch-mode))
   (t
    (let (message-log-max)
      (apply 'message (append (list str) args)))
    )))


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
        tmp tmp2 ret)
    (setq ergoemacs-command-loop--shift-translated nil
          this-command-keys-shift-translated nil)
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
            (cond
             ((not new-key)
              (setq new-key orig-key))
             ((not (memq bind global))
              (setq new-key orig-key))
             ((and new-binding (not (memq new-binding global)))
              (setq bind new-binding)))
            (when (and orig-key
                       (setq ret bind
                             ret (if (and (eq ret 'ergoemacs-map-undefined)
                                          (not (equal orig-key (nth 0 trials)))) nil ret)))
              (cond
               ((equal orig-key (nth 0 trials))
                (setq ergoemacs-command-loop--single-command-keys new-key)
                (when (and (not (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
                           (ergoemacs-keymapp ret)
                           (setq tmp (lookup-key ret [ergoemacs-timeout])))
                  (cond
                   ((eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                    (setq ret tmp))
                   ((not (region-active-p))) ;; its a key sequence.
                   ;; Shift+Control+c
                   ((and this-command-keys-shift-translated
                         (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)))

                   ;; Immediate
                   ((and (not ergoemacs-ctl-c-or-ctl-x-delay)
                         (eq ergoemacs-handle-ctl-c-or-ctl-x 'both))
                    (setq command tmp))
                   
                   (t ;; with delay
                    (setq tmp2 (with-timeout (ergoemacs-ctl-c-or-ctl-x-delay nil)
                                 (ergoemacs-command-loop--read-event nil key)))
                    (if (not tmp2)
                        (setq ret tmp) ;; timeout, use copy/cut
                      (setq ret (ergoemacs-command-loop--key-lookup (vconcat key (vector tmp2))))))))
                ) ;; Actual key
               ((equal orig-key (nth 1 trials)) ;; `ergoemacs-mode' shift translation
                (setq ergoemacs-command-loop--shift-translated t
                      this-command-keys-shift-translated t
                      ergoemacs-command-loop--single-command-keys (nth 0 trials)))
               (t
                (ergoemacs-command-loop--message-binding key ret new-key)
                (setq ergoemacs-command-loop--single-command-keys new-key)))
              (throw 'found-command ret))))))
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

(defun ergoemacs-command-loop--execute-handle-shift-selection (function)
  "Allow `ergoemacs-mode' command loop to handle shift selection.
This allows shift-selection of non-letter keys.
For instance in QWERTY M-> is shift translated to M-."
  (let ((this-command-keys-shift-translated
         (or this-command-keys-shift-translated
             (if ergoemacs-command-loop--shift-translated t nil))))
    (when (ergoemacs :movement-p function)
      (handle-shift-selection))))

;; (defun ergoemacs-command-loop--execute-rm-keyfreq (command)
;;   "Remove COMMAND from `keyfreq-mode' counts."
;;   (when (featurep 'keyfreq)
;;     (when keyfreq-mode
;;       (let (count)
;;         (setq count (gethash (cons major-mode command) keyfreq-table))
;;         (cond
;;          ((not count))
;;          ((= count 1)
;;           (remhash (cons major-mode command) keyfreq-table))
;;          (count
;;           (puthash (cons major-mode command) (- count 1)
;;                    keyfreq-table)))
;;         ;; Add local-fn to counter.
;;         (setq count (gethash (cons major-mode function) keyfreq-table))
;;         (puthash (cons major-mode function) (if count (+ count 1) 1)
;;                  keyfreq-table)))))

;; (3) execute command
(defvar ergoemacs-command-loop--single-command-keys nil)
(defun ergoemacs-command-loop--execute (command &optional keys)
  "Execute COMMAND pretending that KEYS were pressed."
  (let ((keys (or keys ergoemacs-command-loop--single-command-keys)))
    (cond
     ((or (stringp command) (vectorp command))
      ;; If the command is a keyboard macro (string/vector) then execute
      ;; it though `execute-kbd-macro'
      (execute-kbd-macro command))
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
      
      (dolist (var ergoemacs-command-loop--execute-modify-command-list)
        (set var command))
      
      ;; Handle Shift Selection
      (ergoemacs-command-loop--execute-handle-shift-selection this-command)
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
            (unwind-protect
                (progn
                  (remove-hook 'ergoemacs-pre-command-hook #'ergoemacs-pre-command-hook)
                  (remove-hook 'ergoemacs-pre-command-hook #'ergoemacs-pre-command-hook t)
                  (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
                  (remove-hook 'pre-command-hook #'ergoemacs-pre-command-hook t)
                  (remove-hook 'pre-command-hook #'ergoemacs-command-loop--reset-functions)
                  (remove-hook 'pre-command-hook #'ergoemacs-command-loop--reset-functions t)
                  (run-hooks 'pre-command-hook)
                  (run-hooks 'ergoemacs-pre-command-hook))
              (add-hook 'pre-command-hook #'ergoemacs-pre-command-hook)
              (add-hook 'pre-command-hook #'ergoemacs-command-loop--reset-functions))
            (ergoemacs-command-loop--call-interactively command t))
        (setq ergoemacs-command-loop--single-command-keys nil)))))
  
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
