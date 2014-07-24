;;; ergoemacs-shortcuts.el --- Ergoemacs shortcuts interface -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

;; Filename: ergoemacs-shortcuts.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
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
;; (require 'guide-key nil t)

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(unless (fboundp 'ergoemacs-pretty-key)
  (require 'ergoemacs-translate))


(defgroup ergoemacs-read nil
  "Options for ergoemacs-read-key."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-translate-keys t
  "When translation is enabled, when a command is not defined
look for the command with or without modifiers."
  :type 'boolean
  :group 'ergoemacs-read)

(defcustom ergoemacs-translate-emacs-keys t
  "When key is undefined, translate to an emacish key.

For example in `org-mode' C-c C-n performs
`outline-next-visible-heading'.  A QWERTY `ergoemacs-mode' key
equivalent is <apps> f M-k.  When enabled, pressing this should also perfomr `outline-next-visible-heading'"
  :type 'boolean
  :group 'ergoemacs-read)

(defvar ergoemacs-first-variant nil
  "First variant of `ergoemacs-read' key.")
(defvar ergoemacs-describe-key nil)
(defun ergoemacs-describe-key ()
  "Ergoemacs replacement for `describe-key'
Uses `ergoemacs-read-key'"
  (interactive)
  (setq ergoemacs-describe-key t)
  (ergoemacs-read-key nil 'normal))

(defvar ergoemacs-single-command-keys nil)
(defvar ergoemacs-mark-active nil)

(defun ergoemacs-to-sequence (key)
  "Returns a key sequence from KEY.
This sequence is compatible with `listify-key-sequence'."
  (let (input)
    (cond
     ((not key)) ;; Not specified.
     ((vectorp key) ;; Actual key sequence
      (setq input (listify-key-sequence key)))
     ((consp key) ;; Listified key sequence
      (setq input key))
     ((stringp key) ;; Kbd code
      (setq input (listify-key-sequence (read-kbd-macro key t)))))
    input))

(defun ergoemacs-universal-argument (&optional type)
  "Ergoemacs universal argument.
This is called through `ergoemacs-read-key'"
  (interactive)
  (setq current-prefix-arg '(4))
  (ergoemacs-read-key nil type nil t))

(defun ergoemacs-digit-argument (&optional type)
  "Ergoemacs digit argument.
 This is called through `ergoemacs-read-key'"
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq current-prefix-arg digit))
  (ergoemacs-read-key nil type nil t))

(defun ergoemacs-negative-argument (&optional type)
  "Ergoemacs negative argument.
This is called through `ergoemacs-read-key'"
  (setq current-prefix-arg '-)
  (ergoemacs-read-key nil type nil t))

(defcustom ergoemacs-read-key-delay 0.01
  "Timeout for `ergoemacs-read-event'.
This is to distinguish events in a terminal, like PuTTy."
  :type 'number
  :group 'ergoemacs-read)

(defvar ergoemacs--input)

(defun ergoemacs-read-event-change (event keymap)
  "Change EVENT based on KEYMAP.
Used to help with translation keymaps like `input-decode-map'"
  (let ((ret event)
        current-key next-key
        test-ret
        (old-ergoemacs-input ergoemacs--input))
    (setq current-key (vector ret))
    (setq test-ret (lookup-key keymap current-key))
    (while (and current-key
                (keymapp test-ret))
      (setq next-key
            (with-timeout (ergoemacs-read-key-delay nil)
              (or (pop ergoemacs--input)
                  (read-key))))
      (if (not next-key)
          (setq current-key nil)
        (setq current-key (vconcat current-key (vector next-key)))
        (setq test-ret (lookup-key keymap current-key))))
    (if (not current-key)
        (when old-ergoemacs-input
          (setq ergoemacs--input old-ergoemacs-input))
      (if (and (vectorp test-ret)
                 (= 1 (length test-ret)))
          (progn
            (setq ret (elt test-ret 0)))
        (when old-ergoemacs-input
          (setq ergoemacs--input old-ergoemacs-input))))
    ret))

(defcustom ergoemacs-read-swaps
  '(((normal normal) unchorded)
    ((normal unchorded) ctl-to-alt)
    ((normal unchorded) normal)
    ((ctl-to-alt ctl-to-alt) unchorded)
    ((ctl-to-alt unchorded) ctl-to-alt)
    ((unchorded unchorded) ctl-to-alt)
    ((unchorded ctl-to-alt) unchorded))
  "How the translation will be swapped."
  :type '(repeat
          (list
           (list
            (sexp :tag "First Type")
            (sexp :tag "Current Type"))
           (sexp :tag "Translated Type")))
  :group 'ergoemacs-read)

(defcustom ergoemacs-echo-function 'on-translation
  "Shows the function evaluated with a key."
  :type '(choice
          (const :tag "Always echo" t)
          (const :tag "Echo on translations" on-translation)
          (const :tag "Don't Echo"))
  :group 'ergoemacs-read)

(defcustom ergoemacs-read-blink "•"
  "Blink character."
  :type '(choice
          (string :tag "Cursor")
          (const :tag "No cursor" nil))
  :group 'ergoemacs-read)

(defcustom ergoemacs-read-blink-timeout 0.4
  "Timeout for `ergoemacs-read' blinking cursor."
  :type 'number
  :group 'ergoemacs-read)

(defcustom ergoemacs-backspace-will-undo-swap-translation t
  "Backspace will undo a swapped keyboard translation."
  :type 'boolean
  :group 'ergoemacs-read)

(defvar ergoemacs-translation-text)
(defvar ergoemacs-use-ergoemacs-key-descriptions)
(defvar ergoemacs-modal)
(defvar ergoemacs-universal-fns)
(defvar ergoemacs-read-input-keys)
(defvar ergoemacs-read-local-emulation-mode-map-alist)
(declare-function ergoemacs-unicode-char "ergoemacs-translate.el")
(declare-function ergoemacs-translate "ergoemacs-translate.el")
(declare-function ergoemacs-local-map "ergoemacs-translate.el")
(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
(defun ergoemacs-read-event (type &optional pretty-key extra-txt universal)
  "Reads a single event of TYPE.

PRETTY-KEY represents the previous keys pressed in
`ergoemacs-pretty-key' format.

EXTRA-TXT changes the text before the prompt.

UNIVERSAL tells that a universal argument has been pressed, or a
universal argument can be entered.
"
  (let* ((universal universal)
         (type (or type 'normal))
         (local-keymap (ergoemacs-local-map type))
         (key-tag (intern (concat ":" (symbol-name type) "-key")))
         ret (blink-on nil)
         (help-list (gethash type ergoemacs-translation-text))
         help-text)
    (when help-list
      (let ((off (if ergoemacs-use-ergoemacs-key-descriptions 1 0)))
          (setq help-text
                (concat (if (nth off help-list)
                            (concat "\nTranslations:"
                                    (mapconcat
                                     (lambda (x) x)
                                     (nth off help-list) ", ")) "")
                        (if (nth (+ 2 off) help-list)
                            (concat "\nKeys:"
                                    (mapconcat
                                     (lambda (x) x)
                                     (nth (+ 2 off) help-list) ", ")) "")))))
    (while (not ret)
      (unless (or (minibufferp) (and ergoemacs-modal (not pretty-key)))
        (let (message-log-max)
          (message "%s%s%s%s%s%s\t%s"
                   (if ergoemacs-describe-key
                       "Describe key: " "")
                   (if current-prefix-arg
                       (format
                        "%s%s%s %s "
                        (cond
                         ((listp current-prefix-arg)
                          (make-string (round (log (nth 0 current-prefix-arg) 4)) ?u))
                         (t current-prefix-arg))
                        (if universal
                            (if blink-on
                                (if ergoemacs-read-blink
                                    (ergoemacs-unicode-char
                                     ergoemacs-read-blink "-")
                                  " ") " ") "")
                        (if (listp current-prefix-arg)
                            (format "%s" 
                                    current-prefix-arg)
                          "")
                        (ergoemacs-unicode-char "▸" ">"))
                     (if universal
                         (format "%s %s "
                                 (if universal
                                     (if blink-on
                                         (if ergoemacs-read-blink
                                             (ergoemacs-unicode-char
                                              ergoemacs-read-blink "-")
                                           " ") " ") "")
                                 (ergoemacs-unicode-char "▸" ">"))
                       ""))
                   (if help-list (nth 5 help-list) "")
                   (or pretty-key "")
                   (or extra-txt (if help-list
                                     (nth
                                      (if ergoemacs-use-ergoemacs-key-descriptions
                                          1 0) (nth 4 help-list)) ""))
                   (if universal ""
                     (if blink-on
                         (if ergoemacs-read-blink
                             (ergoemacs-unicode-char
                              ergoemacs-read-blink "-")
                           "") ""))
                   (if (and help-text (not universal))
                       help-text ""))))
      
      (setq blink-on (not blink-on))
      (setq ret (with-timeout (ergoemacs-read-blink-timeout nil)
                  (or (pop ergoemacs--input)
                      (read-key))))
      ;; Now try to fix issues with `input-decode-map'
      (when ret
        (setq ret (ergoemacs-read-event-change ret input-decode-map))
        ;; These should only be replaced if they are not bound.
        (unless (commandp (ergoemacs-real-key-binding (vector ret)) t)
          (setq ret (ergoemacs-read-event-change ret local-function-key-map)))
        (unless (commandp (ergoemacs-real-key-binding (vector ret)) t)
          (setq ret (ergoemacs-read-event-change ret key-translation-map))))
      (cond
       ((and ret (not universal)
             (and local-keymap
                  (memq (lookup-key local-keymap (vector ret))
                        ergoemacs-universal-fns)))
        (setq universal t)
        (setq ret nil))
       ((and ret universal) ;; Handle universal arguments.
        (cond
         ((eq ret 45) ;; Negative argument
          (cond
           ((integerp current-prefix-arg)
            (setq current-prefix-arg (- current-prefix-arg)))
           ((eq current-prefix-arg '-)
            (setq current-prefix-arg nil))
           (t
            (setq current-prefix-arg '-)))
          (setq ret nil))
         ((memq ret (number-sequence 48 57)) ;; Number
          (setq ret (- ret 48)) ;; Actual Number.
          (cond
           ((and (integerp current-prefix-arg) (< 0 current-prefix-arg))
            (setq current-prefix-arg (+ ret (* current-prefix-arg 10))))
           ((and (integerp current-prefix-arg) (> 0 current-prefix-arg))
            (setq current-prefix-arg (+ (- ret) (* current-prefix-arg 10))))
           ((and (eq current-prefix-arg '-) (> ret 0))
            (setq current-prefix-arg (- ret))
            (setq ret nil))
           (t
            (setq current-prefix-arg ret)))
          (setq ret nil))
         ((and local-keymap
               (memq (lookup-key local-keymap (vector ret))
                     ergoemacs-universal-fns)) ;; Toggle to key-sequence.
          (setq ret nil)
          (setq universal nil))
         ((let (ergoemacs-read-input-keys)
            (or (memq (ergoemacs-real-key-binding
                       (plist-get
                        (ergoemacs-translate (vector ret))
                        key-tag))
                      ergoemacs-universal-fns)
                (and local-keymap
                     (memq (lookup-key local-keymap (vector ret))
                           ergoemacs-universal-fns))))
          ;; Universal argument called.
          (cond
           ((not current-prefix-arg)
            (setq current-prefix-arg '(4))
            (setq ret nil))
           ((listp current-prefix-arg)
            (setq current-prefix-arg (list (* (nth 0 current-prefix-arg) 4)))
            (setq ret nil))
           (t
            (setq universal nil)
            (setq ret nil))))
         ((member (vector ret)
                  (where-is-internal
                   'ergoemacs-read-key-undo-last
                   local-keymap))
          ;; Allow backspace to edit universal arguments.
          (cond
           ((not current-prefix-arg)) ;; Exit  universal argument
           ((and (integerp current-prefix-arg)
                 (= 0 (truncate current-prefix-arg 10))
                 (< 0 current-prefix-arg))
            (setq current-prefix-arg nil)
            (setq ret nil))
           ((and (integerp current-prefix-arg)
                 (= 0 (truncate current-prefix-arg 10))
                 (> 0 current-prefix-arg))
            (setq current-prefix-arg '-)
            (setq ret nil))
           ((integerp current-prefix-arg)
            (setq current-prefix-arg (truncate current-prefix-arg 10))
            (setq ret nil))
           ((listp current-prefix-arg)
            (setq current-prefix-arg
                  (list (expt 4 (- (round (log (nth 0 current-prefix-arg) 4)) 1))))
            (if (equal current-prefix-arg '(1))
                (setq current-prefix-arg nil))
            (setq ret nil))
           ((eq current-prefix-arg '-)
            (setq current-prefix-arg nil)
            (setq ret nil))))))))
    ret))

(defun ergoemacs-read-key-swap (&optional first-type current-type)
  "Function to swap key translation.

This function looks up the FIRST-TYPE and CURRENT-TYPE in
`ergoemacs-read-swaps' to figure out the next translation.

If the next translation is not found, default to the normal
translation."
(interactive)
  (let ((next-swap (assoc (list first-type current-type) ergoemacs-read-swaps)))
    (if next-swap
        (nth 1 next-swap)
      'normal)))

(defun ergoemacs-read-key-undo-last ()
  "Function to undo the last key-press.
This is actually a dummy function.  The actual work is done in `ergoemacs-read-key'"
  (interactive))

(defvar ergoemacs-translations)
(defun ergoemacs-read-key-install-next-key (next-key key pretty kbd)
  "Installs KEY PRETTY-KEY and KBD into NEXT-KEY plist.
It will replace anything defined by `ergoemacs-translation'"
  (let ((next-key next-key))
    (maphash 
     (lambda(_ignore var-plist)
       (let* ((variant (concat ":" (symbol-name (plist-get var-plist ':name))))
              (var-kbd (intern variant))
              (var-key (intern (concat variant "-key")))
              (var-pretty (intern (concat variant "-pretty")))
              (var-s-kbd (intern (concat variant "-shift")))
              (var-s-key (intern (concat variant "-shift-key")))
              (var-s-pretty (intern (concat variant "-shift-pretty"))))
         (setq next-key (plist-put next-key var-kbd kbd))
         (setq next-key (plist-put next-key var-s-kbd kbd))
         (setq next-key (plist-put next-key var-key key))
         (setq next-key (plist-put next-key var-s-key key))
         (setq next-key (plist-put next-key var-pretty pretty))
         (setq next-key (plist-put next-key var-s-pretty pretty))))
     ergoemacs-translations)
    next-key))

(declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
(defvar ergoemacs-alt-text "M-")

(defvar ergoemacs-ctl-text "C-")

(defvar ergoemacs-alt-ctl-text "M-C-")

(defun ergoemacs-read-key-next-key-is-alt (&optional type pretty-key)
  "The next key read is an Alt+ key. (or M- )"
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate
                      (vector
                       (ergoemacs-read-event nil pretty-key ergoemacs-alt-text))))
           (key (plist-get next-key ':alt-key))
           (pretty (plist-get next-key ':alt-pretty))
           (kbd (plist-get next-key ':alt)))
      (ergoemacs-read-key-install-next-key next-key key pretty kbd))))

(defun ergoemacs-read-key-next-key-is-ctl (&optional type pretty-key)
  "The next key read is an Ctrl+ key. (or C- )"
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate
                      (vector
                       (ergoemacs-read-event nil pretty-key ergoemacs-ctl-text))))
           (key (plist-get next-key ':ctl-key))
           (pretty (plist-get next-key ':ctl-pretty))
           (kbd (plist-get next-key ':ctl)))    
      (ergoemacs-read-key-install-next-key next-key key pretty kbd))))

(defun ergoemacs-read-key-next-key-is-alt-ctl (&optional type pretty-key)
  "The next key read is an Alt+Ctrl+ key. (or C-M- )"
  (interactive)
  (if (or type pretty-key)
      (when (and type pretty-key)
        (let* ((next-key (ergoemacs-translate
                          (vector
                           (ergoemacs-read-event nil pretty-key ergoemacs-alt-ctl-text))))
               (key (plist-get next-key ':alt-ctl-key))
               (pretty (plist-get next-key ':alt-ctl-pretty))
               (kbd (plist-get next-key ':alt-ctl)))
          (ergoemacs-read-key-install-next-key next-key key pretty kbd)))
    (warn "This should be called from ergoemacs read key sequence only.")))

(defun ergoemacs-read-key-next-key-is-quoted (&optional type pretty-key)
  "The next key read is quoted."
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate (vector (ergoemacs-read-event nil pretty-key ""))))
          (key (plist-get next-key ':normal-key))
          (pretty (plist-get next-key ':normal-pretty))
          (kbd (plist-get next-key ':normal)))
      (ergoemacs-read-key-install-next-key next-key key pretty kbd))))

(defvar guide-key-mode)
(defvar guide-key/guide-key-sequence)
(declare-function guide-key/popup-function "guide-key.el")
(declare-function guide-key/close-guide-buffer "guide-key.el")
(declare-function guide-key/popup-guide-buffer-p "guide-key.el")
(defvar ergoemacs-read-key-last-help nil)
(defvar ergoemacs-read-key nil
  "Current key for `ergoemacs-read-key'")
(defun ergoemacs-read-key-help ()
  "Show help for the current sequence KEY."
  (interactive)
  ;; Eventually...
  (if (not ergoemacs-read-key) nil
    (if (and (boundp 'guide-key-mode) guide-key-mode)
        (let ((key ergoemacs-read-key))
          (if (equal ergoemacs-read-key-last-help ergoemacs-read-key)
              (progn
                (setq ergoemacs-read-key-last-help nil)
                (setq guide-key/guide-key-sequence (delete (key-description ergoemacs-read-key) guide-key/guide-key-sequence))
                (guide-key/close-guide-buffer))
            ;; Not using pushnew because the test is equal and
            ;; guide-key/guide-key-sequence is a global variable.
            (add-to-list 'guide-key/guide-key-sequence (key-description ergoemacs-read-key))
            (setq ergoemacs-read-key-last-help ergoemacs-read-key)
            (guide-key/popup-function key))
          t)
      (let ((cb (current-buffer))
            (key ergoemacs-read-key))
        (save-excursion
          (with-help-window (help-buffer)
            (set-buffer (help-buffer))
            (describe-buffer-bindings cb key)))
        nil))))

(defvar ergoemacs-modal-list)
(declare-function minibuffer-keyboard-quit "delsel.el")
(declare-function ergoemacs-key-fn-lookup "ergoemacs-translate.el")
(declare-function ergoemacs-modal-toggle "ergoemacs-modal.el")
(declare-function cua-clear-rectangle-mark "cua-rect.el")

(defvar ergoemacs-keyboard-quit-modes
  '((wdired-mode wdired-exit))
  "Escape key for various modes.")
(defvar ergoemacs-mode)
(defvar ergoemacs-no-shortcut-keys)
(defvar ergoemacs-shortcut-keys)
(defvar ergoemacs-no-shorcut-keys)
(defvar ergoemacs-unbind-keys)
(defvar ergoemacs-repeat-keys)
(defun ergoemacs-keyboard-quit ()
  "Replacement for `keyboard-quit' and `minibuffer-keyboard-quit'.

- In a minibuffer, do `minibuffer-keyboard-quit'.  When a
- `cua-mode' rectangle is active, clear the selected rectangle.
- If the 【q】 key is bound to a non self-insert function, exit
  by this function. (By convention, the 【q】 key is often quit)
- If the 【Ctrl+G】 key is bound to something other than
  `keyboard-quit' use that.
- If the major mode has a function defined in
  `ergoemacs-keyboard-quit', use that.
- If `ergoemacs-mode' knows of the quit function, use that
- If an `ergoemacs-mode' modal translation is active, deactivate it.
- Otherwise issue `keyboard-quit'
"
  (let (tmp)
    (cond
     ((minibufferp)
      (minibuffer-keyboard-quit))
     ((and (boundp 'cua--rectangle) cua--rectangle
           (boundp 'cua-mode) cua-mode
           (fboundp 'cua-clear-rectangle-mark))
      (cua-clear-rectangle-mark))
     ((and (not (region-active-p))
           (and (progn
                  (setq tmp (key-binding "q"))
                  (commandp tmp t))
               (not (string-match "self-insert" (symbol-name tmp)))))
      (call-interactively tmp))
     ((and (not (region-active-p))
           (and (progn
                  (setq tmp (key-binding "C-g"))
                  (commandp tmp t))
                (not (eq 'keyboard-quit tmp))))
      (call-interactively tmp))
     ((and (not (region-active-p))
           (and (progn
                  (setq tmp (assoc major-mode ergoemacs-keyboard-quit-modes))
                  (if (not tmp) nil
                    (setq tmp (car (cdr tmp)))
                    (commandp tmp t)))))
      (call-interactively tmp))
     (t
      (let (defined-fn
             ergoemacs-shortcut-keys
             ergoemacs-no-shortcut-keys
             ergoemacs-read-input-keys
             ergoemacs-mode)
        (setq defined-fn (ergoemacs-key-fn-lookup 'keyboard-quit))
        (setq defined-fn (ignore-errors (ergoemacs-real-key-binding defined-fn)))
        (cond
         (defined-fn
           (ergoemacs-read-key-call defined-fn))
         ((and ergoemacs-modal
               (let ((hash (gethash (nth 0 ergoemacs-modal-list) ergoemacs-translations)))
                 (and hash
                      (not (plist-get hash ':modal-always))))) ;; Exit modal 
          (ergoemacs-modal-toggle (nth 0 ergoemacs-modal-list)))
         (t
          (keyboard-quit)))))))
  (setq ergoemacs-describe-key nil))


(defvar ergoemacs-defer-post-command-hook nil)
(defun ergoemacs-defer-post-command-hook ()
  "Defers `post-command-hook'."
  (set-default 'ergoemacs-defer-post-command-hook (default-value 'post-command-hook))
  (set (make-local-variable 'ergoemacs-defer-post-command-hook) post-command-hook)
  (set (make-local-variable 'post-command-hook) nil)
  (set-default 'post-command-hook nil))

(defun ergoemacs-restore-post-command-hook ()
  (when (or (default-value 'ergoemacs-defer-post-command-hook)
            ergoemacs-defer-post-command-hook)
    (set-default 'post-command-hook (default-value 'ergoemacs-defer-post-command-hook))
    (set (make-local-variable 'post-command-hook) ergoemacs-defer-post-command-hook)
    (set (make-local-variable 'ergoemacs-defer-post-command-hook) nil)
    (set-default 'ergoemacs-defer-post-command-hook nil)))

(defvar ergoemacs-deactivate-mark nil)
(defvar ergoemacs-shift-translated nil)
(defvar ergoemacs-mode)
(defvar ergoemacs-original-keys-to-shortcut-keys)
(defvar ergoemacs-this-command-fake)
(defvar keyfreq-mode)
(defvar ergoemacs-this-command)
(defvar keyfreq-table)
(declare-function ergoemacs-emulations "ergoemacs-mode.el")
(declare-function ergoemacs-smart-function-p "ergoemacs-model.el")
(defvar ergoemacs-test-fn nil
  "Flag to have `ergoemacs-read-key-call' assign the function to
  this variable.")
(defun ergoemacs-read-key-call (function &optional record-flag keys)
  "`ergoemacs-mode' replacement for `call-interactively'.

This will call the function with the standard `call-interactively' unless:

- `ergoemacs-describe-key' is non-nil.  In this case,
   `describe-function' is called instead.

-  The function name matches \"self-insert\", then it will send the keys
   by `unread-command-events'.

In addition, when the function is called:

- Add the function count to `keyfreq-table' when `keyfreq-mode'
  is enabled.

- Remove `ergoemacs-this-command' count from `keyfreq-table' when
  `keyfreq-mode' is enabled.

- set `this-command' to the function called.

"
  (setq ergoemacs-deactivate-mark nil)
  (when (and ergoemacs-read-key-last-help (boundp 'guide-key-mode) guide-key-mode)
    (setq ergoemacs-read-key-last-help nil)
    (guide-key/close-guide-buffer))
  (cond
   (ergoemacs-test-fn
    (setq ergoemacs-test-fn function))
   (ergoemacs-describe-key
    (let ((pt (point))
          (buf (current-buffer))
          (keys '())
          test)
      (unwind-protect
          (save-excursion
            (describe-function function)
            (set-buffer (help-buffer))
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert (format "%s runs the command "
                              (ergoemacs-pretty-key (key-description keys))))
              (when (search-forward " is" nil t)
                (replace-match ", which is"))
              (fill-paragraph)
              (when (search-forward "bound to" nil t)
                (delete-region
                 (point)
                 (if (re-search-forward "\\.\n\n") (point) (point)))
                (with-current-buffer buf
                  (goto-char pt)
                  (ergoemacs-with-global
                   (dolist (global-key (where-is-internal function))
                     (setq test (gethash global-key ergoemacs-original-keys-to-shortcut-keys))
                     (when test
                       (dolist (global-key test)
                         (unless (eq (elt global-key 0) 'menu-bar)
                           (push (ergoemacs-pretty-key (key-description global-key))
                                 keys))))))
                  (let (ergoemacs-modal ergoemacs-repeat-keys ergoemacs-read-input-keys
                                        ergoemacs-shortcut-keys ergoemacs-no-shortcut-keys)
                    (dolist (global-key (where-is-internal function))
                      (unless (eq (elt global-key 0) 'menu-bar)
                        (push (ergoemacs-pretty-key (key-description global-key))
                            keys)))))
                (insert (mapconcat (lambda(x) x) keys ", "))
                (insert ".\n\n"))))
        (setq ergoemacs-describe-key nil))))
   ((ignore-errors (string-match "self-insert" (symbol-name function)))
    (setq ergoemacs-single-command-keys keys)
    (setq last-input-event keys)
    (setq prefix-arg current-prefix-arg)
    (setq unread-command-events (append (listify-key-sequence keys) unread-command-events))
    (ergoemacs-defer-post-command-hook)
    (reset-this-command-lengths))
   (t
    (dolist (var ergoemacs-this-command-fake)
      ;; should include `this-command' and `this-original-command'
      (when (ergoemacs-smart-function-p var)
        (set var function)))
    (let ((this-command-keys-shift-translated
           (or this-command-keys-shift-translated
               (if ergoemacs-shift-translated t nil))))
      (when (featurep 'keyfreq)
        (when keyfreq-mode
          (let ((command ergoemacs-this-command) count)
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
                     keyfreq-table))))
      (let (deactivate-mark)
	(remove-hook 'ergoemacs-pre-command-hook 'ergoemacs-pre-command-hook)
	(remove-hook 'ergoemacs-pre-command-hook 'ergoemacs-pre-command-hook t)
        (run-hooks 'ergoemacs-pre-command-hook)
        (call-interactively function record-flag keys)
        (setq ergoemacs-deactivate-mark deactivate-mark)
        (when deactivate-mark
          (setq ergoemacs-mark-active nil))))))
  (when ergoemacs-deactivate-mark
    (setq deactivate-mark ergoemacs-deactivate-mark
          ergoemacs-mark-active nil)
    (setq ergoemacs-deactivate-mark nil)))

(defvar ergoemacs-read-key-overriding-terminal-local-save nil)
(defvar ergoemacs-read-key-overriding-overlay-save nil)
(defun ergoemacs-read-key-lookup-get-ret---universal (fn)
  "Setup type and first-type when bound and set ret to universal"
  (let (ret tmp)
    (setq ret 'universal)
    (setq tmp (symbol-name fn))
    ;; Now change `type' and `first-type' when bound.
    (when (and (boundp 'type)
               (boundp 'first-type))
      (if (not (string-match "^ergoemacs-\\(.*\\)-universal-argument" tmp))
          (setq type 'normal
                first-type 'normal)
        (setq tmp (intern (match-string 1 tmp)))
        (setq type tmp
              first-type tmp)))
    ret))

(defun ergoemacs-read-key-lookup-get-ret (fn)
  "Get ret type for FN.
Returns 'keymap for FN that is a keymap.
If FN is a member of `ergoemacs-universal-fns', return 'universal

If universal is returned, and type first-type is bound, set these
to the appropriate values for `ergoemacs-read-key'.
"
  (let (ret)
    (when (ignore-errors (keymapp fn))
      ;; If keymap, continue.
      (setq ret 'keymap))
    (when (memq fn ergoemacs-universal-fns)
      (setq ret (ergoemacs-read-key-lookup-get-ret---universal fn)))
    ret))

(defun ergoemacs-read-key--echo-command (pretty-key command)
  "Echo that PRETTY-KEY is COMMAND"
  (when (and ergoemacs-echo-function
             (boundp 'pretty-key-undefined)
             (not (or this-command-keys-shift-translated
                      ergoemacs-shift-translated)))
    (let (message-log-max)
      (if (string= pretty-key-undefined pretty-key)
          (when (eq ergoemacs-echo-function t)
            (message "%s%s%s" pretty-key
                     (ergoemacs-unicode-char "→" "->")
                     command))
        (message "%s%s%s (from %s)"
                 pretty-key
                 (ergoemacs-unicode-char "→" "->")
                 command
                 pretty-key-undefined)))))

(defun ergoemacs-read-key--send-unread (unread-vector
                                        lookup
                                        use-override
                                        pretty-key)
  "Send `unread-command-events' inside `ergoemacs-read-key-lookup'"
  (setq ergoemacs-mark-active
        (or (and mark-active transient-mark-mode) mark-active))
  (setq ergoemacs-single-command-keys unread-vector)
  (setq last-input-event unread-vector)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events (append (listify-key-sequence unread-vector) unread-command-events))
  (ergoemacs-defer-post-command-hook)
  (reset-this-command-lengths)
  (ergoemacs-read-key--echo-command
   pretty-key (ergoemacs-pretty-key (key-description unread-vector)))
  (when lookup
    (define-key lookup [ergoemacs-single-command-keys] 'ignore)
    (if (not use-override)
        (setq ergoemacs-read-key-overriding-terminal-local-save overriding-terminal-local-map)
      (setq overriding-terminal-local-map lookup))))

(defvar ergoemacs-command-shortcuts-hash)
(defvar ergoemacs-extract-map-hash)
(defvar refix-keys)
(defvar ergoemacs-unbind-keymap)
(defvar pretty-key-undefined)
(defvar ergoemacs-modal-save)
(defvar ergoemacs-shortcut-prefix-keys)
(defun ergoemacs-read-key-lookup (key pretty-key)
  "Lookup KEY and run if necessary.

PRETTY-KEY is the ergoemacs-mode pretty representation of the key.
"
  (prog1
      (let* (ergoemacs-read-input-keys
             ergoemacs-modal
             ergoemacs-shortcut-keys
             ergoemacs-no-shortcut-keys
             (hash (and key (gethash key ergoemacs-command-shortcuts-hash)))
             lookup
             tmp-overlay use-override
             tmp ret fn)
        (unwind-protect
            (progn
              ;; Install `overriding-terminal-local-map' without
              ;; `ergoemacs-read-key' The composed map with ergoemacs-read-key
              ;; will be installed on the `ergoemacs-post-command-hook'
              (when overriding-terminal-local-map
                (setq lookup (gethash
                              (md5
                               (format "override-terminal-read:%s"
                                       overriding-terminal-local-map))
                              ergoemacs-extract-map-hash))
                (when lookup
                  (setq use-override t)
                  (setq overriding-terminal-local-map lookup)))
              
              ;; Install overriding-local-map
              (when overriding-local-map 
                (setq lookup (gethash
                              (md5
                               (format "override-local-read:%s"
                                       overriding-local-map))
                              ergoemacs-extract-map-hash))
                (when lookup
                  (setq use-override t)
                  (setq overriding-local-map lookup)))
              (when (get-char-property (point) 'keymap)
                (setq lookup (gethash
                              (md5
                               (format "char-map-read:%s" (get-char-property (point) 'keymap)))
                              ergoemacs-extract-map-hash))
                (let (deactivate-mark)
                  (when lookup
                    (setq tmp-overlay (make-overlay (max (- (point) 1) (point-min))
                                                    (min (+ (point) 1) (point-max))))
                    (overlay-put tmp-overlay 'keymap lookup)
                    (overlay-put tmp-overlay 'priority 536870910))))
              (cond
               ;; Apply shortcuts (even though masked).
               ((and key (member key ergoemacs-shortcut-prefix-keys))
                ;; Shortcut prefix key, wait for more.
                (setq ret 'keymap))
               ((and hash (commandp (nth 0 hash) t))
                ;; Shorcut command
                (ergoemacs-read-key--echo-command
                 pretty-key (or (and (symbolp (nth 0 hash)) (symbol-name (nth 0 hash)))
                                (ergoemacs-unicode-char "λ" "lambda")))
                (ergoemacs-shortcut-remap (nth 0 hash))
                (setq ergoemacs-single-command-keys nil)
                (setq ret 'function-remap))
               ((and hash (stringp (nth 0 (nth 0 hash))) (not (nth 1 (nth 0 hash))))
                ;; Shortcut character.
                (ergoemacs-read-key--send-unread
                 (read-kbd-macro (nth 0 (nth 0 hash)) t) lookup use-override
                 pretty-key)
                (setq ret 'unread))
               ((and hash (stringp (nth 0 (nth 0 hash))))
                ;; Shorcut to another key combination/translation
                ;; Reset `ergoemacs-read-key'
                (setq ret (list (nth 0 (nth 0 hash)) (nth 1 (nth 0 hash)) (nth 1 (nth 0 hash)))))
               ((progn ;; key translations first, just like emacs.
                  (setq tmp (lookup-key local-function-key-map key))
                  (when (and tmp (integerp tmp))
                    (setq tmp nil))
                  (unless tmp
                    (setq tmp (lookup-key key-translation-map key))
                    (when (and tmp (integerp tmp))
                      (setq tmp nil)))
                  tmp)
                ;; Should use emacs key translation.
                (cond
                 ((or (keymapp tmp))
                  (setq ret 'keymap))
                 ((and ergoemacs-describe-key (vectorp tmp))
                  (setq ergoemacs-single-command-keys nil)
                  (message "%s translates to %s" pretty-key
                           (ergoemacs-pretty-key (key-description tmp)))
                  (setq ergoemacs-describe-key nil)
                  (setq ret 'translate))
                 ((and (vectorp tmp)
                       (progn
                         (setq fn (ergoemacs-real-key-binding tmp))
                         (when (and (symbolp fn) (string-match "self-insert" (symbol-name fn)))
                           (setq fn nil))
                         (commandp fn t)))
                  (setq fn (or (command-remapping fn (point)) fn))
                  (setq ergoemacs-single-command-keys key)
                  (ergoemacs-read-key--echo-command
                   pretty-key (or (and (symbolp fn) (symbol-name fn))
                                  (ergoemacs-unicode-char "λ" "lambda")))
                  (ergoemacs-read-key-call fn nil key)
                  (setq ergoemacs-single-command-keys nil)
                  (setq ret 'translate-fn))
                 ((vectorp tmp)
                  (ergoemacs-read-key--send-unread tmp lookup use-override pretty-key)
                  (setq ret 'translate))))
               ;; Is there an local override function?
               ((progn
                  (setq fn (ergoemacs-get-override-function key))
                  (setq ret (ergoemacs-read-key-lookup-get-ret fn))
                  (or ret (commandp fn t)))
                (unless ret
                  (ergoemacs-read-key-call fn nil key)
                  (setq ret 'local-function-override)))
               ;; Does this call a function?
               ((progn
                  (setq fn (ergoemacs-real-key-binding key))
                  (setq ret (ergoemacs-read-key-lookup-get-ret fn))
                  (or ret (commandp fn t)))
                (unless ret
                  (setq fn (or (command-remapping fn (point)) fn))
                  (when (memq fn ergoemacs-universal-fns)
                    (setq ret (ergoemacs-read-key-lookup-get-ret---universal fn)))
                  (unless ret
                    (setq ergoemacs-single-command-keys key)
                    (ergoemacs-read-key--echo-command
                     pretty-key (or (and (symbolp fn) (symbol-name fn))
                                    (ergoemacs-unicode-char "λ" "lambda")))
                    (ergoemacs-read-key-call fn nil key)
                    (setq ergoemacs-single-command-keys nil)
                    (setq ret 'function))))
               ;; Does this call an override or major/minor mode function?
               ((progn
                  (setq fn (or
                            ;; Call major/minor mode key?
                            (ergoemacs-with-major-and-minor-modes 
                             (ergoemacs-real-key-binding key))
                            ;; Call unbound or global key?
                            (if (eq (lookup-key ergoemacs-unbind-keymap key) 'ergoemacs-undefined) 'ergoemacs-undefined
                              (let (ergoemacs-read-input-keys)
                                (if (keymapp (ergoemacs-real-key-binding key))
                                    (setq ret 'keymap)
                                  (ergoemacs-with-global
                                   (ergoemacs-real-key-binding key)))))))
                  (setq ret (ergoemacs-read-key-lookup-get-ret fn))
                  (or ret (commandp fn t)))
                (unless ret
                  (setq fn (or (command-remapping fn (point)) fn))
                  (setq ergoemacs-single-command-keys key)
                  (let (message-log-max)
                    (if (string= pretty-key-undefined pretty-key)
                        (message "%s%s%s" pretty-key
                                 (ergoemacs-unicode-char "→" "->")
                                 fn)
                      (message "%s%s%s (from %s)"
                               pretty-key
                               (ergoemacs-unicode-char "→" "->")
                               fn
                               pretty-key-undefined)))
                  (ergoemacs-read-key-call fn nil key)
                  (setq ergoemacs-single-command-keys nil)
                  (setq ret 'function-global-or-override)))))
          ;; Fix tempoary overlay
          (when (and tmp-overlay (not ergoemacs-read-key-overriding-overlay-save))
            (delete-overlay tmp-overlay)))
        ret)
    ;; Turn off read-input-keys for shortcuts
    (when unread-command-events
      (when ergoemacs-modal
        (setq ergoemacs-modal-save ergoemacs-modal))
      (setq ergoemacs-modal nil)
      (set-default 'ergoemacs-modal nil))
    (when ergoemacs-single-command-keys
      (setq ergoemacs-read-input-keys nil))))

(defvar ergoemacs-translation-keymap)
(defun ergoemacs-read-key-add-translation (key-plist trans)
  "Adds `ergoemacs-translation-keymap' to KEY-PLIST for TRANS translation.
Otherwise add new translation to key-plist and return it."
  (let* ((key (plist-get key-plist (intern (concat trans "-key"))))
         (new-key (if key (lookup-key ergoemacs-translation-keymap key) nil))
         kd
         (new-trans (concat trans "-et"))
         (key-plist key-plist))
    (if (or (not new-key) (integerp new-key))
        (setq key-plist (setq key-plist (plist-put key-plist (intern new-trans) nil)))
      (setq key-plist (plist-put key-plist (intern (concat new-trans "-key")) new-key))
      (setq kd (key-description new-key))
      (setq key-plist (plist-put key-plist (intern new-trans) kd))
      (setq kd (ergoemacs-pretty-key kd))
      (setq key-plist (plist-put key-plist (intern (concat new-trans "-pretty")) kd)))
    key-plist))

(defvar ergoemacs-command-shortcuts-hash)
(defvar guide-key/recursive-key-sequence-flag)
(defun ergoemacs-read-key (&optional key type initial-key-type universal)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type, defined by `ergoemacs-translate'
Ergoemacs-mode sets up: 'ctl-to-alt 'unchorded 'normal.

INITIAL-KEY-TYPE represents the translation type for the initial KEY.

UNIVERSAL allows ergoemacs-read-key to start with universal
argument prompt.
"
  (setq ergoemacs-deactivate-mark nil)
  (let ((continue-read t)
        (guide-key/guide-key-sequence '())
        (guide-key/recursive-key-sequence-flag t)
        (real-type (or type 'normal))
        (first-type (or type 'normal))
        deactivate-mark
        pretty-key
        pretty-key-undefined
        next-key
        (ergoemacs-read-key key)
        key-trial
        pretty-key-trial
        (type (or initial-key-type 'normal))
        base
        local-keymap
        local-fn
        key-trials
        real-read
        (first-universal universal)
        (curr-universal nil)
        ergoemacs--input tmp
        history)
    (setq ergoemacs--input (ergoemacs-to-sequence ergoemacs-read-key)
          ergoemacs-read-key nil)
    (while continue-read
      (setq continue-read nil)
      (when (and (not ergoemacs--input) real-type)
        (setq type real-type)
        (setq curr-universal first-universal)
        (setq real-type nil))
      (when (and ergoemacs-read-key (boundp 'guide-key-mode) guide-key-mode
                 (not (equal ergoemacs-read-key-last-help ergoemacs-read-key))
                 (guide-key/popup-guide-buffer-p ergoemacs-read-key))
        (setq ergoemacs-read-key-last-help ergoemacs-read-key)
        (guide-key/popup-function ergoemacs-read-key))
      (setq real-read (not ergoemacs--input))
      (setq base (concat ":" (symbol-name type))
            next-key (vector
                      (ergoemacs-read-event type pretty-key nil curr-universal)))
      (setq next-key (ergoemacs-translate next-key))
      (setq tmp (plist-get next-key ':normal))
      (cond
       ((string= tmp "ESC")
        (setq tmp "<escape>"))
       ((string= tmp "RET")
        (setq tmp "<return>")))
      (if (string= tmp (key-description
                        (ergoemacs-key-fn-lookup 'keyboard-quit)))
          (cond
           ((and (not ergoemacs-read-key))
            (ergoemacs-keyboard-quit))
           ((and ergoemacs-read-key-last-help (boundp 'guide-key-mode) guide-key-mode)
            (setq ergoemacs-read-key-last-help nil
                  guide-key/guide-key-sequence (delete (key-description ergoemacs-read-key) guide-key/guide-key-sequence)
                  continue-read t)
            (guide-key/close-guide-buffer))
           (t (unless (minibufferp)
                (let (message-log-max)
                  (setq tmp (gethash type ergoemacs-translation-text))
                  (message "%s%s%s Canceled with %s"
                           (if ergoemacs-describe-key
                               "Help for: " "")
                           (if tmp (nth 5 tmp) "")
                           (or pretty-key "") 
                           (if ergoemacs-use-ergoemacs-key-descriptions
                               (plist-get next-key ':normal-pretty)
                             (concat (plist-get next-key ':normal) " "))))))
            (setq ergoemacs-describe-key nil))
        (setq tmp (plist-get next-key ':normal-key))
        ;; See if there is a local equivalent of this...
        (setq local-keymap (ergoemacs-local-map type))
        (if (and (or real-read
                     (and (boundp 'modal-default) modal-default))
                 local-keymap)
            (setq local-fn (lookup-key local-keymap tmp))
          (setq local-fn nil))
        (if (eq local-fn 'ergoemacs-read-key-undo-last)
            (if (= 0 (length history))
                (setq continue-read nil) ;; Exit read-key
              (setq tmp (pop history))
              (setq continue-read t ;; Undo last key
                    ergoemacs--input (nth 1 tmp)
                    real-read nil
                    real-type (nth 0 tmp))
              (setq ergoemacs-read-key nil
                    pretty-key nil
                    type 'normal
                    key-trial nil
                    key-trials nil
                    pretty-key-trial nil
                    pretty-key nil))
          (if (and (eq local-fn 'ergoemacs-read-key-swap)
                   (or (not curr-universal) ergoemacs-read-key))
              (progn
                ;; Swap translation
                (when (and real-read ergoemacs-backspace-will-undo-swap-translation)
                  (push (list type
                              (listify-key-sequence ergoemacs-read-key))
                        history))
                (setq type (ergoemacs-read-key-swap first-type type)
                      continue-read t))
            (setq curr-universal nil)
            (when (or
                   (not
                    (ignore-errors (interactive-form local-fn)))
                   (eq local-fn 'ergoemacs-read-key-swap))
              ;; Either the `ergoemacs-read-key-swap' is not applicable,
              ;; or not specified correctly.  Therefore set local-fn to
              ;; nil.
              (setq local-fn nil))
            ;; Change ergoemacs--input type for next key press.
            (when (memq local-fn '(ergoemacs-read-key-next-key-is-alt
                                   ergoemacs-read-key-next-key-is-ctl
                                   ergoemacs-read-key-next-key-is-alt-ctl
                                   ergoemacs-read-key-next-key-is-quoted))
              (setq next-key (funcall local-fn type pretty-key))
              (setq local-fn nil))
            (if (eq local-fn 'ergoemacs-read-key-help)
                (setq continue-read (ergoemacs-read-key-help))
              (if local-fn
                  (ergoemacs-read-key-call local-fn)
                (setq pretty-key-undefined nil)
                ;; Now we have the 'next-key, try to find a function/keymap
                ;; completion.
                (setq key-trials nil)
                ;; This is the order that ergoemacs-read-key tries keys:
                (push base key-trials)
                (setq next-key  (ergoemacs-read-key-add-translation next-key base))
                (push (concat base "-et") key-trials)
                (push (concat base "-shift-translated") key-trials)
                (setq next-key (ergoemacs-read-key-add-translation next-key (concat base "-shift-translated")))
                (push (concat base "-shift-translated-et") key-trials)
                
                (when (and ergoemacs-read-key ergoemacs-translate-emacs-keys)
                  (setq tmp (gethash (plist-get next-key
                                                (intern (concat base "-key")))
                                     ergoemacs-command-shortcuts-hash))
                  (when (and tmp (commandp (nth 0 tmp) t))
                    (dolist (ergoemacs-read-key (ergoemacs-shortcut-function-binding (nth 0 tmp)))
                      (let ((key-base (concat ":" (md5 (format "%s" ergoemacs-read-key))))
                            (ergoemacs-use-ergoemacs-key-descriptions t))
                        ;; First add translation to next-key
                        (setq next-key
                              (plist-put next-key
                                         (intern (concat key-base "-key"))
                                         ergoemacs-read-key))
                        (setq next-key
                              (plist-put next-key
                                         (intern key-base)
                                         (key-description ergoemacs-read-key)))
                        (setq next-key
                              (plist-put next-key
                                         (intern (concat key-base "-pretty"))
                                         (ergoemacs-pretty-key
                                          (plist-get next-key (intern key-base)))))
                        ;; Now add to list to check.
                        (push key-base key-trials)
                        ;; Add ergoemacs translation
                        (setq next-key (ergoemacs-read-key-add-translation next-key key-base))
                        (push (concat key-base "-et") key-trials)))))
                (when ergoemacs-translate-keys
                  (dolist (trial '(":raw" ":ctl" ":alt" ":alt-ctl" ":raw-shift" ":ctl-shift" ":alt-shift" ":alt-ctl-shift"))
                    (push trial key-trials)
                    (setq next-key (ergoemacs-read-key-add-translation next-key trial))
                    (push (concat trial "-et") key-trials)))
                (setq key-trials (reverse key-trials))
                (unless
                    (catch 'ergoemacs-key-trials
                      (while key-trials
                        (setq key-trial nil)
                        (while (and key-trials (not key-trial))
                          (setq tmp (pop key-trials))
                          ;; If :shift-translated is nil, go to next option.
                          (when (plist-get next-key (intern (concat tmp "-key")))
                            (setq key-trial
                                  (if ergoemacs-read-key
                                      (vconcat ergoemacs-read-key (plist-get next-key (intern (concat tmp "-key"))))
                                    (plist-get next-key (intern (concat tmp "-key"))))
                                  pretty-key-trial
                                  (if pretty-key
                                      (concat pretty-key
                                              (concat
                                               (plist-get next-key
                                                          (intern (concat tmp (if ergoemacs-use-ergoemacs-key-descriptions
                                                                                  "-pretty" ""))))
                                               (if ergoemacs-use-ergoemacs-key-descriptions "" " ")))
                                    (concat
                                     (plist-get next-key
                                                (intern (concat tmp (if ergoemacs-use-ergoemacs-key-descriptions
                                                                        "-pretty" ""))))
                                     (if ergoemacs-use-ergoemacs-key-descriptions "" " "))))))
                        (unless pretty-key-undefined
                          (setq pretty-key-undefined pretty-key-trial))
                        (setq ergoemacs-shift-translated (string-match "-shift-translated" tmp))
                        (setq deactivate-mark nil)
                        (setq local-fn
                              (if key-trial
                                  (ergoemacs-read-key-lookup
                                   key-trial pretty-key-trial) nil))
                        (setq ergoemacs-deactivate-mark deactivate-mark)
                        (cond
                         ((eq local-fn 'keymap)
                          ;; Test to see if major/minor modes have an
                          ;; override for this keymap, see Issue 243.
                          (let ((new-fn (and ergoemacs-read-key (ergoemacs-with-major-and-minor-modes (ergoemacs-real-key-binding key-trial)))))
                            (if (ignore-errors (commandp new-fn t))
                                (progn
                                  (setq local-fn 'major-minor-override-fn)
                                  (ergoemacs-read-key-call new-fn nil ergoemacs-read-key))
                              (when real-read
                                (push (list type
                                            (listify-key-sequence ergoemacs-read-key))
                                      history))
                              (setq continue-read t
                                    ergoemacs-read-key key-trial
                                    pretty-key pretty-key-trial)))
                          ;; Found, exit
                          (throw 'ergoemacs-key-trials t))
                         ((eq (type-of local-fn) 'cons)
                          (when real-read
                            (push (list type
                                        (listify-key-sequence ergoemacs-read-key))
                                  history))
                          ;; ergoemacs-shortcut reset ergoemacs-read-key
                          (setq continue-read t
                                ergoemacs--input (ergoemacs-to-sequence (nth 0 local-fn))
                                ergoemacs-read-key nil
                                pretty-key nil
                                type 'normal
                                real-type (nth 1 local-fn)
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                first-type (nth 2 local-fn))
                          ;; Found, exit
                          (throw 'ergoemacs-key-trials t))
                         ((and (eq local-fn 'universal)
                               (not current-prefix-arg))
                          (setq curr-universal t
                                continue-read t
                                ergoemacs-read-key nil
                                pretty-key nil
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                current-prefix-arg '(4))
                          (throw 'ergoemacs-key-trials t))
                         ((and (eq local-fn 'universal)
                               (listp current-prefix-arg))
                          (setq curr-universal t
                                continue-read t
                                ergoemacs-read-key nil
                                pretty-key nil
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                current-prefix-arg (list (* 4 (nth 0 current-prefix-arg))))
                          (throw 'ergoemacs-key-trials t))
                         (local-fn
                          ;; Found exit
                          (throw 'ergoemacs-key-trials t)))
                        (cond
                         ((eq local-fn 'keymap)
                          (when real-read
                            (push (list type
                                        (listify-key-sequence ergoemacs-read-key))
                                  history))
                          (setq continue-read t
                                ergoemacs-read-key key-trial
                                pretty-key pretty-key-trial)
                          ;; Found, exit
                          (throw 'ergoemacs-key-trials t))
                         ((eq (type-of local-fn) 'cons)
                          (when real-read
                            (push (list type
                                        (listify-key-sequence ergoemacs-read-key))
                                  history))
                          ;; ergoemacs-shortcut reset ergoemacs-read-key
                          (setq continue-read t
                                ergoemacs--input (ergoemacs-to-sequence (nth 0 local-fn))
                                ergoemacs-read-key nil
                                pretty-key nil
                                type 'normal
                                real-type (nth 1 local-fn)
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                first-type (nth 2 local-fn))
                          ;; Found, exit
                          (throw 'ergoemacs-key-trials t))
                         ((and (eq local-fn 'universal)
                               (not current-prefix-arg))
                          (setq curr-universal t
                                continue-read t
                                ergoemacs-read-key nil
                                pretty-key nil
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                current-prefix-arg '(4))
                          (throw 'ergoemacs-key-trials t))
                         ((and (eq local-fn 'universal)
                               (listp current-prefix-arg))
                          (setq curr-universal t
                                continue-read t
                                ergoemacs-read-key nil
                                pretty-key nil
                                key-trial nil
                                key-trials nil
                                pretty-key-trial nil
                                current-prefix-arg (list (* 4 (nth 0 current-prefix-arg))))
                          (throw 'ergoemacs-key-trials t))
                         (local-fn
                          ;; Found exit
                          (throw 'ergoemacs-key-trials t)))
                        ;; Not found, try the next in the trial
                        (unless key-trials ;; exit
                          (setq key-trial nil)))
                      nil)
                  ;; Could not find the key.
                  (beep)
                  (unless (minibufferp)
                    (let (message-log-max)
                      (message "%s is undefined!" pretty-key-undefined)))))))))))
  (when ergoemacs-deactivate-mark
    (setq deactivate-mark ergoemacs-deactivate-mark
          ergoemacs-mark-active nil))
  (setq ergoemacs-describe-key nil
        ergoemacs-read-key-last-help nil))

(defun ergoemacs-read-key-default ()
  "The default command for `ergoemacs-mode' read-key.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
no translation listed."
  (interactive "^")
  (ergoemacs-read-key
   (or ergoemacs-single-command-keys (this-single-command-keys))))



(defvar ergoemacs-ignored-prefixes '(;; "C-h" "<f1>"
                                     "C-x" "C-c" "ESC" "<escape>"
                                     "<remap>"))

(defvar ergoemacs-extract-map-hash (make-hash-table :test 'equal))

(defvar ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal)
  "List of command shortcuts.")

(defcustom ergoemacs-shortcut-ignored-functions
  '(undo-tree-visualize)
  "Ignored functions for `ergoemacs-shortcut'."
  :group 'ergoemacs-mode
  :type '(repeat
          (symbol :tag "Function to ignore:")))

(defun ergoemacs-get-override-function (keys)
  "See if KEYS has an overriding function.
If so, return the function, otherwise return nil.

First it looks up ergoemacs user commands.  These are defined in the key
<ergoemacs-user> KEYS

If there is no ergoemacs user commands, look in override map.
This is done by looking up the function for KEYS with
`ergoemacs-with-overrides'.

If the overriding function is found make sure it isn't the key
defined in the major/minor modes (by
`ergoemacs-with-major-and-minor-modes'). "
  (let ((override (ergoemacs-real-key-binding (read-kbd-macro (format "<ergoemacs-user> %s" (key-description keys)))))
        cmd1 cmd2)
    (unless (commandp override)
      (setq override nil))
    (unless override
      (setq cmd1 (ergoemacs-with-overrides
                  (ergoemacs-real-key-binding keys)))
      (when (commandp cmd1 t)
        (setq cmd2 (ergoemacs-with-major-and-minor-modes
                    (ergoemacs-real-key-binding keys)))
        (unless (eq cmd1 cmd2)
          (setq override cmd1))))
    override))

(defun ergoemacs-shortcut---internal ()
  "Real shortcut function.
This is used for the following functions: `ergoemacs-shortcut-movement'
`ergoemacs-shortcut-movement-no-shift-select' and `ergoemacs-shortcut'.

Basically, this gets the keys called and passes the arguments to`ergoemacs-read-key'."
  (let* ((keys (or ergoemacs-single-command-keys (this-single-command-keys)))
         (args (gethash keys ergoemacs-command-shortcuts-hash)))
    (unless args
      (setq keys (read-kbd-macro (key-description keys) t))
      (setq args (gethash keys ergoemacs-command-shortcuts-hash)))
    (ergoemacs-read-key keys)
    (setq ergoemacs-single-command-keys nil)))


(defvar ergoemacs-repeat-keymap nil)
(defvar ergoemacs-repeat-emulation-mode-map-alist)
(defvar ergoemacs-repeat-keys)
(declare-function ergoemacs-mode-line "ergoemacs-mode.el")
(defun ergoemacs-install-repeat-keymap (keymap &optional mode-line)
  "Installs repeat KEYMAP."
  (setq ergoemacs-repeat-keymap keymap)
  (setq ergoemacs-repeat-emulation-mode-map-alist
        (list (cons 'ergoemacs-repeat-keys ergoemacs-repeat-keymap)))
  (setq ergoemacs-repeat-keys t)
  (when mode-line
    (ergoemacs-mode-line mode-line)))

(defvar ergoemacs-repeat-movement-commands)
(defun ergoemacs-repeat-movement-full-keymap (&optional cmds)
  "Allow movement commands to be repeated without pressing the ALT key"
  (let (ergoemacs-modal
        ergoemacs-repeat-keys
        ergoemacs-read-input-keys
        (keymap (make-sparse-keymap)))
    (dolist (key (apply 'append
                        (mapcar
                         (lambda (cmd)
                           (where-is-internal cmd))
                         (or cmds '(ergoemacs-shortcut-movement ergoemacs-shortcut-movement-no-shift-select)))))
      (when (= 1 (length key))
        (let ((mods (event-modifiers (elt key 0))))
          (when (memq 'meta mods)
            (define-key keymap
              (vector
               (event-convert-list
                (append (delete 'meta mods)
                        (list (event-basic-type (elt key 0))))))
              `(lambda() (interactive) (ergoemacs-read-key ,(key-description key))))))))
    keymap))

(defun ergoemacs-shortcut-movement ()
  "Shortcut for other key/function for movement keys.

This function is `cua-mode' aware for movement and supports
`shift-select-mode'.

Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for
`ergoemacs-single-command-keys' or `this-single-command-keys'."
  (interactive "^")
  (ergoemacs-shortcut-movement-no-shift-select))
(put 'ergoemacs-shortcut-movement 'CUA 'move)


(defun ergoemacs-shortcut-movement-no-shift-select ()
  "Shortcut for other key/function in movement keys without shift-selection support.

Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for
`ergoemacs-single-command-keys' or `this-single-command-keys'.
"
  (interactive)
  (let ((ck (this-single-command-keys)))
    (ergoemacs-shortcut---internal)
    ;; Now optionally install the repeatable movements.
    (cond
     ((and (eq ergoemacs-repeat-movement-commands 'single) (= (length ck) 1))
      (ergoemacs-install-repeat-keymap
       (let ((map (make-sparse-keymap)))
         (define-key map (vector (event-basic-type (elt ck 0))) this-command)
         map)
       (format " %sSingle" (ergoemacs-unicode-char "↔" "<->"))))
     ((eq ergoemacs-repeat-movement-commands 'all)
      (ergoemacs-install-repeat-keymap
       (ergoemacs-repeat-movement-full-keymap)
       (format " %sFull" (ergoemacs-unicode-char "↔" "<->")))))))

(defun ergoemacs-shortcut ()
  "Shortcut for other key/function for non-movement keys.
Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for `ergoemacs-single-command-keys' or `this-single-command-keys'."
  (interactive)
  (ergoemacs-shortcut---internal))

(defvar ergoemacs-shortcut-send-key nil)
(defvar ergoemacs-shortcut-send-fn nil)
(defvar ergoemacs-shortcut-send-timer nil)

(defvar ergoemacs-debug-shortcuts nil)
(defvar ergoemacs-shortcut-function-binding-hash (make-hash-table :test 'equal))
(defvar ergoemacs-where-is-global-hash)
(defun ergoemacs-shortcut-function-binding (function &optional dont-ignore-menu)
  "Determine the global bindings for FUNCTION.

This also considers archaic emacs bindings by looking at
`ergoemacs-where-is-global-hash' (ie bindings that are no longer
in effect)."
  (let ((ret (gethash (list function dont-ignore-menu) ergoemacs-shortcut-function-binding-hash)))
    (if ret
        ret
      (if dont-ignore-menu
          (where-is-internal function (current-global-map))
        (dolist (x (where-is-internal function (current-global-map)))
          (unless (or (eq 'menu-bar (elt x 0)))
            (push x ret)))
        (setq ret (reverse ret)))
      (puthash (list function dont-ignore-menu) ret ergoemacs-shortcut-function-binding-hash)
      ret)))

(defcustom ergoemacs-use-function-remapping t
  "Uses function remapping.
For example in `org-agenda-mode' the standard key for
save-buffer (C-x C-s) `org-save-all-org-buffers'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-shortcut-remap-list
  (function
   &optional keymap
   ignore-desc dont-swap-for-ergoemacs-functions dont-ignore-commands)
  "Determine the possible current remapping of FUNCTION.

It based on the key bindings bound to the default emacs keys
For the corresponding FUNCTION.

It returns a list of (remapped-function key key-description).

If KEYMAP is non-nil, lookup the binding based on that keymap
instead of the keymap with ergoemacs disabled.

For example, in `org-agenda-mode' C-x C-s runs
`org-save-all-org-buffers' instead of `save-buffer'.

Therefore:

 (ergoemacs-shortcut-remap-list 'save-buffer)

returns

 ((org-save-all-org-buffers [24 19] \"C-x C-s\"))

in `org-agenda-mode'.

If you wish to disable this remapping, you can set
`ergoemacs-use-function-remapping' to nil.

When IGNORE-DESC is t, then ignore key description filters.  When
IGNORE-DESC is nil, then ignore keys that match \"[sAH]-\".  When
IGNORE-DESC is a regular expression, then ignore keys that match
the regular expression.

By default, if an interactive function ergoemacs-X exists, use
that instead of the remapped function.  This can be turned off by
changing DONT-SWAP-FOR-ERGOEMACS-FUNCTIONS.

This command also ignores anything that remaps to FUNCTION,
`ergoemacs-this-command' and
`ergoemacs-shortcut-ignored-functions'.  This can be turned off
by setting changed by setting DONT-IGNORE-COMMANDS to t.

When KEYMAP is defined, `ergoemacs-this-command' is not included
in the ignored commands.i

Also this ignores anything that is changed in the global keymap.

If <ergoemacs-user> key is defined, ignore this functional remap.

<ergoemacs-user> key is defined with the `define-key' advice when
running `run-mode-hooks'.  It is a work-around to try to respect
user-defined keys.
"
  (if (not ergoemacs-use-function-remapping) nil
    (let ((key-bindings-lst (ergoemacs-shortcut-function-binding function))
          case-fold-search
          (old-global-map (current-global-map))
          (new-global-map (make-sparse-keymap))
          ret ret2)
      (unwind-protect
          (progn
            (use-global-map new-global-map)
            (when key-bindings-lst
              (dolist (key key-bindings-lst)
                (let* (fn
                       (key-desc (key-description key))
                       (user-key
                        (read-kbd-macro
                         (concat "<ergoemacs-user> " key-desc)))
                       fn2)
                  (cond
                   (keymap
                    (setq fn (lookup-key keymap key t))
                    (if (eq fn (lookup-key keymap user-key))
                        (setq fn nil)
                      (unless (commandp fn t)
                        (setq fn nil))))
                   (t
                    (ergoemacs-with-global
                     (setq fn (ergoemacs-real-key-binding key t nil (point)))
                     (if (eq fn (ergoemacs-real-key-binding user-key t nil (point)))
                         (setq fn nil)
                       (if (keymapp fn)
                           (setq fn nil))))))
                  (when fn
                    (cond
                     ((eq ignore-desc t))
                     ((eq (type-of ignore-desc) 'string)
                      (when (string-match ignore-desc key-desc)
                        (setq fn nil)))
                     (t
                      (when (string-match "[sAH]-" key-desc)
                        (setq fn nil))))
                    (when fn
                      (unless dont-swap-for-ergoemacs-functions
                        (setq fn2 (ignore-errors (intern-soft (concat "ergoemacs-" (symbol-name fn)))))
                        (when (and fn2 (not (interactive-form fn2)))
                          (setq fn2 nil)))
                      (when (memq fn (append
                                      `(,function ,(if keymap nil ergoemacs-this-command))
                                      (if dont-ignore-commands '() ergoemacs-shortcut-ignored-functions)))
                        (setq fn nil))
                      (when (and fn2
                                 (memq fn2 (append
                                            `(,function ,(if keymap nil ergoemacs-this-command))
                                            (if dont-ignore-commands '() ergoemacs-shortcut-ignored-functions))))
                        (setq fn2 nil))
                      (cond
                       (fn2
                        (push (list fn2 key key-desc) ret2))
                       (fn (push (list fn key key-desc) ret)))))))))
        (use-global-map old-global-map))
      (when ret2
        (setq ret (append ret2 ret)))
      ret)))

(defun ergoemacs-shortcut-remap (function)
  "Runs the FUNCTION or whatever `ergoemacs-shortcut-remap-list' returns.
Will use KEYS or `this-single-command-keys', if cannot find the
original key binding.
"
  (let ((fn-lst (ergoemacs-shortcut-remap-list function))
        (fn function)
        send-fn)
    (when fn-lst
      (setq fn (nth 0 (nth 0 fn-lst))))
    (setq send-fn (or (command-remapping fn (point)) fn))
    (unless (commandp send-fn t)
      (setq send-fn fn))
    (ergoemacs-read-key-call send-fn)))

(declare-function ergoemacs-theme--install-shortcuts-list "ergoemacs-theme-engine.el")
(defun ergoemacs-install-shortcuts-map (&optional map dont-complete)
  "Returns a keymap with shortcuts installed.
If MAP is defined, use a copy of that keymap as a basis for the shortcuts.
If MAP is nil, base this on a sparse keymap."
  (let ((ergoemacs-shortcut-override-keymap
         (or map
             (make-sparse-keymap)))
        (ergoemacs-orig-keymap
         (if map
             (copy-keymap map) nil))
        shortcut-list)
    (maphash
     (lambda (key item)
       (push (list key item) shortcut-list))
     ergoemacs-command-shortcuts-hash)
    (ergoemacs-theme--install-shortcuts-list
     shortcut-list ergoemacs-shortcut-override-keymap 
     ergoemacs-orig-keymap (not dont-complete))
    ergoemacs-shortcut-override-keymap))

(defvar ergoemacs-describe-keybindings-functions
  '(describe-package
    describe-bindings
    describe-key
    where-is
    describe-key-briefly
    describe-function
    describe-variable
    ergoemacs-describe-major-mode)
  "Functions that describe keys.
Setup C-c and C-x keys to be described properly.")

(defvar ergoemacs-show-true-bindings nil
  "Show the true bindings.  Otherwise, show what the bindings translate to...")

(defun ergoemacs-remove-shortcuts (&optional create-overlay)
  "Removes ergoemacs shortcuts from keymaps."
  (let ((inhibit-read-only t)
        deactivate-mark
        hashkey lookup override-text-map
        tmp-overlay)
    (cond
     ((and overriding-terminal-local-map nil) ;; Don't do this for now...
      (when (or
             (eq (lookup-key
                  overriding-terminal-local-map [ergoemacs]) 'ignore)
             (eq (lookup-key
                  overriding-terminal-local-map [ergoemacs-read]) 'ignore))
        (setq hashkey (md5 (format "override-terminal-orig%s:%s"
                                   (if ergoemacs-modal
                                       (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                     "")
                                   overriding-terminal-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          (setq overriding-terminal-local-map lookup)
          ;; (ergoemacs-debug-heading "Remove ergoemacs from `overriding-terminal-local-map'")
          ;; Save old map.
          ;; (ergoemacs-debug-keymap 'overriding-terminal-local-map)
          )))
     (overriding-local-map
      (when (or (eq (lookup-key overriding-local-map [ergoemacs]) 'ignore)
                (eq (lookup-key overriding-local-map [ergoemacs-read]) 'ignore))
        (setq hashkey (md5 (format "override-local-orig%s:%s"
                                   (if ergoemacs-modal
                                       (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                     "")
                                   overriding-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          ;; (ergoemacs-debug-heading "Remove ergoemacs from `overriding-local-map'")
          (setq overriding-local-map lookup)
          ;; (ergoemacs-debug-keymap 'overriding-local-map)
          )))
     ((progn
        (setq override-text-map (get-char-property (point) 'keymap))
        (and (keymapp override-text-map)
             (or (eq (lookup-key override-text-map [ergoemacs]) 'ignore)
                 (eq (lookup-key override-text-map [ergoemacs-read]) 'ignore))))
      (let ((overlays (overlays-at (point)))
            found)
        (while (and overlays (not create-overlay))
          (let* ((overlay (car overlays))
                 (overlay-keymap (overlay-get overlay 'keymap)))
            (if (not (equal overlay-keymap override-text-map))
                (setq overlays (cdr overlays))
              (setq found overlay)
              (setq overlays nil))))
        (setq hashkey (md5 (format "char-map-orig%s:%s"
                                   (if ergoemacs-modal
                                       (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                     "")
                                   override-text-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          ;; (ergoemacs-debug-heading "Remove ergoemacs from (get-char-property (point) 'keymap)")
          (setq override-text-map lookup)
          (if (not create-overlay)
              (if found
                  (overlay-put found 'keymap override-text-map)
                (when (and (previous-single-property-change (point) 'keymap)
                           (next-single-property-change (point) 'keymap))
                  ;; (ergoemacs-debug "Put into text properties")
                  (ergoemacs-save-buffer-state
                   (put-text-property
                    (or (previous-single-property-change (point) 'keymap (current-buffer) (point-min)) (point-min))
                    (or (next-single-property-change (point) 'keymap (current-buffer) (point-max)) (point-max))
                    'keymap override-text-map))))
            (setq tmp-overlay (make-overlay (max (- (point) 1) (point-min))
                                            (min (+ (point) 1) (point-max))))
            (overlay-put tmp-overlay 'keymap lookup)
            (overlay-put tmp-overlay 'priority 536870911))
          ;; (ergoemacs-debug-keymap 'override-text-map)
          ))))
    tmp-overlay))

(defvar ergoemacs-read-input-keymap)
(defvar ergoemacs-modal-keymap)
(declare-function ergoemacs-flatten-composed-keymap "ergoemacs-mode.el")
(defun ergoemacs-install-shortcut-up--internal (text keymap &optional dont-complete)
  (let* ((keymap keymap)
         read-map
         (hashkey (md5 (format "%s%s:%s"
                               (if ergoemacs-modal
                                   (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                 "")
                               text keymap)))
         (orig-map (copy-keymap keymap))
         (lookup (gethash hashkey ergoemacs-extract-map-hash)))
    (if lookup
        (progn
          (setq keymap lookup))
      (ergoemacs-install-shortcuts-map keymap dont-complete)
      (setq read-map (copy-keymap keymap))
      (define-key read-map [ergoemacs-read] 'ignore)
      (if ergoemacs-modal
          (setq keymap
                (let ((map (make-sparse-keymap)))
                  (setq map ergoemacs-modal-keymap)
                  (setq map
                        (ergoemacs-flatten-composed-keymap
                         (make-composed-keymap
                          (list
                           (ergoemacs-local-map
                            (nth 0 ergoemacs-modal-list)
                            t)
                           map))))
                  map))
        (setq keymap
              (copy-keymap
               (make-composed-keymap
                (list ergoemacs-read-input-keymap keymap)))))
      (define-key keymap [ergoemacs] 'ignore)
      (puthash hashkey keymap ergoemacs-extract-map-hash)
      (puthash (md5 (format "%s%s:%s" text
                            (if ergoemacs-modal
                                (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                              "")
                            read-map)) keymap ergoemacs-extract-map-hash)
      ;; Save old map.
      ;; Lookup map on either the composed or non-composed map
      ;; gives the same original map
      (setq hashkey (md5 (format "%s-orig%s:%s" text
                                 (if ergoemacs-modal
                                     (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                   "")
                                 keymap)))
      (puthash hashkey orig-map ergoemacs-extract-map-hash)
      (setq hashkey (md5 (format "%s-orig%s:%s" text
                                 (if ergoemacs-modal
                                     (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                   "")
                                 read-map)))
      (puthash hashkey orig-map ergoemacs-extract-map-hash)
      (setq hashkey (md5 (format "%s-read%s:%s" text
                                 (if ergoemacs-modal
                                     (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                   "")
                                 keymap)))
      (puthash hashkey read-map ergoemacs-extract-map-hash)
      (setq hashkey (md5 (format "%s-read%s:%s" text
                                 (if ergoemacs-modal
                                     (concat "-modal-" (symbol-name (nth 0 ergoemacs-modal-list)))
                                   "")
                                 read-map)))
      (puthash hashkey read-map ergoemacs-extract-map-hash))
    keymap))

(defun ergoemacs-install-shortcuts-up ()
  "Installs ergoemacs shortcuts into overriding keymaps.
The keymaps are:
- `overriding-terminal-local-map'
- `overriding-local-map'
- Overlays with :keymap property
- text property with :keymap property."
  (let ((inhibit-read-only t)
        deactivate-mark
        override-text-map)
    (cond
     ((and overriding-terminal-local-map nil) ;; Don't do this for now
      (when (not
             (eq (lookup-key
                  overriding-terminal-local-map [ergoemacs])
                 'ignore))
        ;; (ergoemacs-debug-heading "Install shortcuts into overriding-terminal-local-map")
        (setq overriding-terminal-local-map 
              (ergoemacs-install-shortcut-up--internal
               "override-terminal" overriding-terminal-local-map)))
      (let (overriding-terminal-local-map)
        (ergoemacs-install-shortcuts-up)))
     (overriding-local-map
      (when  (not (eq (lookup-key overriding-local-map [ergoemacs])
                      'ignore))
        ;; (ergoemacs-debug-heading "Install shortcuts into overriding-local-map")
        (setq overriding-local-map
              (ergoemacs-install-shortcut-up--internal
               "override-local" overriding-local-map)))
      (let (overriding-local-map)
        (ergoemacs-install-shortcuts-up)))
     ((progn
        (setq override-text-map (get-char-property (point) 'keymap))
        (and (keymapp override-text-map)
             (not (eq (lookup-key override-text-map [ergoemacs])
                      'ignore))))
      ;; (ergoemacs-debug-heading "Install shortcuts into (get-char-property (point) 'keymap)")
      (let ((overlays (overlays-at (point)))
            found)
        (while overlays
          (let* ((overlay (car overlays))
                 (overlay-keymap (overlay-get overlay 'keymap)))
            (if (not (equal overlay-keymap override-text-map))
                (setq overlays (cdr overlays))
              (setq found overlay)
              (setq overlays nil))))
        (setq override-text-map
              (ergoemacs-install-shortcut-up--internal
               "char-map" override-text-map t))
        (if found
            (overlay-put found 'keymap override-text-map)
          ;; Overlay not found; change text property
          ;; (ergoemacs-debug "Put into text properties")
          (ergoemacs-save-buffer-state
           (put-text-property
            (or (previous-single-property-change (point) 'keymap (current-buffer) (point-min)) (point-min))
            (or (next-single-property-change (point) 'keymap (current-buffer) (point-max)) (point-max))
            'keymap override-text-map)))
        ;; (ergoemacs-debug-keymap 'override-text-map)
        )))))

(defvar ergoemacs-debug-keymap--temp-map)
(declare-function ergoemacs-real-substitute-command-keys "ergoemacs-advice.el")
(defun ergoemacs-extract-prefixes (keymap)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'."
  (save-match-data
    (let ((string (or (ignore-errors
                        (ergoemacs-real-substitute-command-keys (format "\\{%s}" (symbol-name keymap))))
                      (progn
                        (setq ergoemacs-debug-keymap--temp-map keymap)
                        (ergoemacs-real-substitute-command-keys "\\{ergoemacs-debug-keymap--temp-map}"))))
          (pt 0)
          (ret '()))
      (while (string-match "^\\([^ \n][^ \n\t]*?\\) [^.].+*" string pt)
        (unless (or (string-match-p "\\(--\\|key\\)" (match-string 1 string))
                    (member (match-string 1 string) ergoemacs-ignored-prefixes)
                    (member (match-string 1 string) ret))
          (when (string-match-p (format "%s [A-Za-z]" (regexp-quote (match-string 1 string)))
                                (match-string 0 string))
            (push (match-string 1 string) ret)))
        (setq pt (match-end 0)))
      ret)))

(defvar ergoemacs-ignore-advice)
(defun ergoemacs-setup-local-prefixes ()
  "Setup local prefixes to use `ergoemacs-read-key'.
Do not do anything if you are:
- In the minibuffer (determined by `minibufferp')
- If overriding text maps are active, like `overriding-terminal-local-map' and `overriding-local-map'"
  (unless (or (minibufferp) overriding-terminal-local-map overriding-local-map)
    (let* ((map (current-local-map))
           (ergoemacs-ignore-advice t)
           key-vector
           (prefixes (ergoemacs-extract-prefixes map))
           (read-map (make-sparse-keymap)))
      (when prefixes
        (dolist (prefix prefixes)
          (setq key-vector (read-kbd-macro prefix prefix))
          (ignore-errors 
            (define-key read-map key-vector 'ergoemacs-read-key-default)))
        (set (make-local-variable 'ergoemacs-read-local-emulation-mode-map-alist)
             (list (cons 'ergoemacs-read-input-keys read-map)))))))

(provide 'ergoemacs-shortcuts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-shortcuts.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
