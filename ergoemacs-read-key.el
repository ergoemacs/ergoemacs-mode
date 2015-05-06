;;; ergoemacs-read-key.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-read-key.el
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

(defun ergoemacs-read-key--universal-argument (&optional type)
  "Ergoemacs universal argument.
This is called through `ergoemacs-read-key'"
  (interactive)
  (setq current-prefix-arg '(4))
  (ergoemacs-read-key nil type nil t))

(defalias 'ergoemacs-universal-argument 'ergoemacs-read-key--universal-argument)


(defun ergoemacs-read-key--digit-argument (&optional type)
  "Ergoemacs digit argument.
 This is called through `ergoemacs-read-key'"
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq current-prefix-arg digit))
  (ergoemacs-read-key nil type nil t))

(defalias 'ergoemacs-digit-argument 'ergoemacs-read-key--digit-argument)

(defun ergoemacs-read-key--negative-argument (&optional type)
  "Ergoemacs negative argument.
This is called through `ergoemacs-read-key'"
  (setq current-prefix-arg '-)
  (ergoemacs-read-key nil type nil t))

(defalias 'ergoemacs-negative-argument 'ergoemacs-read-key--negative-argument)

(defvar ergoemacs-read-key--input nil
  "Read key input")

(defun ergoemacs-read-key--event-change (event keymap)
  "Change EVENT based on KEYMAP.
Used to help `ergoemacs-read-key' with translation keymaps like
`input-decode-map'"
  (let ((ret event)
        current-key next-key
        test-ret
        (old-ergoemacs-input ergoemacs-read-key--input))
    (setq current-key (vector ret))
    (setq test-ret (lookup-key keymap current-key))
    (while (and current-key
                (keymapp test-ret))
      (setq next-key
            (with-timeout (ergoemacs-read-key-delay nil)
              (or (pop ergoemacs-read-key--input)
                  (read-key))))
      (if (not next-key)
          (setq current-key nil)
        (setq current-key (vconcat current-key (vector next-key)))
        (setq test-ret (lookup-key keymap current-key))))
    (if (not current-key)
        (when old-ergoemacs-input
          (setq ergoemacs-read-key--input old-ergoemacs-input))
      (if (and (vectorp test-ret)
               (= 1 (length test-ret)))
          (progn
            (setq ret (elt test-ret 0)))
        (when old-ergoemacs-input
          (setq ergoemacs-read-key--input old-ergoemacs-input))))
    ret))

(defvar ergoemacs-read-key nil
  "Current key for `ergoemacs-read-key'")

(defvar ergoemacs-read-key--describe-key nil)

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
          (message "%s%s%s%s%s\t%s"
                   (if ergoemacs-read-key--describe-key
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
                   (ergoemacs--pretty-key-concat
                    pretty-key
                    (or extra-txt
                        (if help-list
                            (nth
                             (if ergoemacs-use-ergoemacs-key-descriptions
                                 1 0) (nth 4 help-list)) "")))
                   
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
                  (or (pop ergoemacs-read-key--input)
                      (read-key))))
      ;; Now try to fix issues with `input-decode-map'
      (when ret
        (setq ret (ergoemacs-read-key--event-change ret input-decode-map))
        ;; These should only be replaced if they are not bound.
        (unless (commandp (key-binding (vector ret)) t)
          (setq ret (ergoemacs-read-key--event-change ret local-function-key-map)))
        (unless (commandp (key-binding (vector ret)) t)
          (setq ret (ergoemacs-read-key--event-change ret key-translation-map))))
      (cond
       ((and ret (not universal)
             (not (key-binding
                   (or (and ergoemacs-read-key (vconcat ergoemacs-read-key (vector ret)))
                       (vector ret))))
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
            (or (memq (key-binding
                       (plist-get
                        (ergoemacs-translate (vector ret))
                        key-tag))
                      ergoemacs-universal-fns)
                (not (key-binding
                      (or (and ergoemacs-read-key (vconcat ergoemacs-read-key (vector ret)))
                          (vector ret))))
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
                   'ergoemacs-read-key--undo-last
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

(defun ergoemacs-read-key--swap (&optional first-type current-type)
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


(defun ergoemacs-read-key--undo-last ()
  "Function to undo the last key-press.
This is actually a dummy function.  The actual work is done in `ergoemacs-read-key'"
  (interactive)
  (warn "This is a dummy function called by `ergoemacs-read-key'"))

(defun ergoemacs-read-key--force-undo-last ()
  "Function to undo the last key-press.
Unlike `ergoemacs-read-key-undo-last', this ignores any bindings like \\[backward-kill-sentence]
This is actually a dummy function.  The actual work is done in `ergoemacs-read-key'"
  (interactive)
  (warn "This is a dummy function called by `ergoemacs-read-key'"))

(defun ergoemacs-read-key--install-next-key (next-key key pretty kbd)
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

(defvar ergoemacs-read-key--alt-text "M-"
  "Alt+ text")

(defvar ergoemacs-read-key--ctl-text "C-"
  "Ctrl+ text")

(defvar ergoemacs-read-key--alt-ctl-text "M-C-"
  "Alt+Ctl+ text")

(defun ergoemacs-read-key--next-key-is-alt (&optional type pretty-key
                                                     next-key-vector)
  "The next key read is an Alt+ key. (or M- )"
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate
                      (or next-key-vector
                          (vector
                           (ergoemacs-read-event nil pretty-key ergoemacs-alt-text)))))
           (key (plist-get next-key :alt-key))
           (pretty (plist-get next-key :alt-pretty))
           (kbd (plist-get next-key :alt)))
      (ergoemacs-read-key--install-next-key next-key key pretty kbd))))

(defalias 'ergoemacs-read-key--force-next-key-is-alt 'ergoemacs-read-key--next-key-is-alt)


(defun ergoemacs-read-key--next-key-is-ctl (&optional type pretty-key
                                                     next-key-vector)
  "The next key read is an Ctrl+ key. (or C- )"
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate
                      (or next-key-vector
                          (vector
                           (ergoemacs-read-event nil pretty-key ergoemacs-ctl-text)))))
           (key (plist-get next-key :ctl-key))
           (pretty (plist-get next-key :ctl-pretty))
           (kbd (plist-get next-key :ctl)))    
      (ergoemacs-read-key--install-next-key next-key key pretty kbd))))

(defalias 'ergoemacs-read-key--force-next-key-is-ctl 'ergoemacs-read-key--next-key-is-ctl)


(defun ergoemacs-read-key--next-key-is-alt-ctl (&optional type pretty-key
                                                         next-key-vector)
  "The next key read is an Alt+Ctrl+ key. (or C-M-)"
  (interactive)
  (if (or type pretty-key)
      (when (and type pretty-key)
        (let* ((next-key (ergoemacs-translate
                          (or next-key-vector
                              (vector
                               (ergoemacs-read-event nil pretty-key ergoemacs-alt-ctl-text)))))
               (key (plist-get next-key ':alt-ctl-key))
               (pretty (plist-get next-key ':alt-ctl-pretty))
               (kbd (plist-get next-key ':alt-ctl)))
          (ergoemacs-read-key--install-next-key next-key key pretty kbd)))
    (warn "This should be called from ergoemacs read key sequence only.")))

(defalias 'ergoemacs-read-key--force-next-key-is-alt-ctl 'ergoemacs-read-key--next-key-is-alt-ctl)


(defalias 'ergoemacs-read-key--force-next-key-is-quoted 'ergoemacs-read-key--next-key-is-quoted)
(defun ergoemacs-read-key--next-key-is-quoted (&optional type pretty-key
                                                        next-key-vector)
  "The next key read is quoted."
  (interactive)
  (when (and type pretty-key)
    (let* ((next-key (ergoemacs-translate (or next-key-vector
                                              (vector (ergoemacs-read-event nil pretty-key "")))))
           (key (plist-get next-key :normal-key))
           (pretty (plist-get next-key :normal-pretty))
           (kbd (plist-get next-key :normal)))
      (ergoemacs-read-key--install-next-key next-key key pretty kbd))))

(defvar ergoemacs-read-key--last-help nil)
(defun ergoemacs-read-key--help ()
  "Show help for the current sequence KEY."
  (interactive)
  ;; Eventually...
  (if (not ergoemacs-read-key) nil
    (cond
     ((and (boundp 'icicle-mode) icicle-mode)
      (let ((key (vconcat ergoemacs-read-key [ergoemacs-ignore])))
        (ergoemacs-read-key--call 'icicle-complete-keys nil key)
        nil))
     ((and (boundp 'guide-key-mode) guide-key-mode)
      (let ((key ergoemacs-read-key))
        (if (equal ergoemacs-read-key--last-help ergoemacs-read-key)
            (progn
              (setq ergoemacs-read-key--last-help nil)
              (setq guide-key/guide-key-sequence (delete (key-description ergoemacs-read-key) guide-key/guide-key-sequence))
              (guide-key/close-guide-buffer))
          ;; Not using pushnew because the test is equal and
          ;; guide-key/guide-key-sequence is a global variable.
          (add-to-list 'guide-key/guide-key-sequence (key-description ergoemacs-read-key))
          (setq ergoemacs-read-key--last-help ergoemacs-read-key)
          (guide-key/popup-function key))
        t))
     (t (let ((cb (current-buffer))
              (key ergoemacs-read-key))
          (save-excursion
            (with-help-window (help-buffer)
              (set-buffer (help-buffer))
              (describe-buffer-bindings cb key)))
          nil)))))

(defvar ergoemacs-read-key--keyboard-quit-modes
  '((wdired-mode wdired-exit))
  "Escape key for various modes.")

(defun ergoemacs-read-key--keyboard-quit ()
  "Replacement for `keyboard-quit' and `minibuffer-keyboard-quit'.
- In a minibuffer, do `minibuffer-keyboard-quit'.  When a
- `cua-mode' rectangle is active, clear the selected rectangle.
- If the 【q】 key is bound to a non self-insert function, exit
  by this function. (By convention, the 【q】 key is often quit)
- If the 【Ctrl+G】 key is bound to something other than
  `keyboard-quit' use that.
- If the major mode has a function defined in
  `ergoemacs-read-key--keyboard-quit-modes', use that.
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
                (not (ignore-errors (string-match "self-insert" (symbol-name tmp))))))
      (call-interactively tmp))
     ((and (not (region-active-p))
           (and (progn
                  (setq tmp (key-binding "C-g"))
                  (commandp tmp t))
                (not (eq 'keyboard-quit tmp))))
      (call-interactively tmp))
     ((and (not (region-active-p))
           (and (progn
                  (setq tmp (assoc major-mode ergoemacs-read-key--keyboard-quit-modes))
                  (if (not tmp) nil
                    (setq tmp (car (cdr tmp)))
                    (commandp tmp t)))))
      (call-interactively tmp))
     (t
      (let ((defined-fn (key-binding [ergoemacs-remap keyboard-quit])))
        (cond
         (defined-fn
           (ergoemacs-read-key--call defined-fn))
         ((and ergoemacs-modal
               (let ((hash (gethash (nth 0 ergoemacs-modal-list) ergoemacs-translations)))
                 (and hash
                      (not (plist-get hash ':modal-always))))) ;; Exit modal 
          (ergoemacs-modal-toggle (nth 0 ergoemacs-modal-list)))
         (t
          (keyboard-quit)))))))
  (setq ergoemacs-read-key--describe-key nil))

(defvar ergoemacs-read-key--defer-post-command-hook nil)
(defun ergoemacs-read-key--defer-post-command-hook ()
  "Defers `post-command-hook'."
  (set-default 'ergoemacs-read-key--defer-post-command-hook
               (default-value 'post-command-hook))
  (set (make-local-variable 'ergoemacs-read-key--defer-post-command-hook)
       post-command-hook)
  (set (make-local-variable 'post-command-hook) nil)
  (set-default 'post-command-hook nil))

(defun ergoemacs-read-key--restore-post-command-hook ()
  (when (or (default-value 'ergoemacs-read-key--defer-post-command-hook)
            ergoemacs-read-key--defer-post-command-hook)
    (set-default 'post-command-hook (default-value 'ergoemacs-read-key--defer-post-command-hook))
    (set (make-local-variable 'post-command-hook) ergoemacs-read-key--defer-post-command-hook)
    (set (make-local-variable 'ergoemacs-read-key--defer-post-command-hook) nil)
    (set-default 'ergoemacs-read-key--defer-post-command-hook nil)))

(defvar ergoemacs-read-key--deactivate-mark nil)
(defvar ergoemacs-read-key--shift-translated nil)

(defvar ergoemacs-read-key--test-fn nil
  "Flag to have `ergoemacs-read-key--call' assign the function to
  this variable.")

(defun ergoemacs-read-key--call (function &optional record-flag keys)
  "`ergoemacs-mode' replacement for `call-interactively'.
This will call the function with the standard `call-interactively' unless:
- `ergoemacs-read-key--describe-key' is non-nil.  In this case,
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
  (setq ergoemacs-read-key--deactivate-mark nil)
  (when (and ergoemacs-read-key-last-help (boundp 'guide-key-mode) guide-key-mode)
    (setq ergoemacs-read-key-last-help nil)
    (guide-key/close-guide-buffer))
  (cond
   ((ergoemacs-smart-function-p function)
    (error "Ergoemacs-mode is confused, and exiting out of an infinite loop (refused to call %s)" function))
   (ergoemacs-read-key--test-fn
    (setq ergoemacs-read-key--test-fn function))
   (ergoemacs-read-key--describe-key
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
        (setq ergoemacs-read-key--describe-key nil))))
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
      (set var function))
    (let ((this-command-keys-shift-translated
           (or this-command-keys-shift-translated
               (if ergoemacs-read-key--shift-translated t nil))))
      ;; Try to maintain shift-selection.
      (when (and ergoemacs-read-key--shift-translated
                 (ergoemacs-is-movement-command-p function))
        (cond
         ((and shift-select-mode ergoemacs-force-shift-select-mark-active
               (not mark-active))
          ;; Mark was active, then it was deactivated, now activate again.
          (unless (and mark-active
                       (eq (car-safe transient-mark-mode) 'only))
            (setq transient-mark-mode
                  (cons 'only
                        (unless (eq transient-mark-mode 'lambda)
                          transient-mark-mode))
                  mark-active t)))
         (t ;; Mark was not active, activate mark.
          (handle-shift-selection))))
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
      (let (deactivate-mark
            (ergoemacs-single-command-keys keys))
        (remove-hook 'ergoemacs-pre-command-hook 'ergoemacs-pre-command-hook)
        (remove-hook 'ergoemacs-pre-command-hook 'ergoemacs-pre-command-hook t)
        (run-hooks 'ergoemacs-pre-command-hook)
        (call-interactively this-command record-flag keys)
        (setq ergoemacs-read-key--deactivate-mark deactivate-mark)
        (when deactivate-mark
          (setq ergoemacs-mark-active nil))))))
  (when ergoemacs-read-key--deactivate-mark
    (setq deactivate-mark ergoemacs-read-key--deactivate-mark
          ergoemacs-mark-active nil)
    (setq ergoemacs-read-key--deactivate-mark nil)))




(provide 'ergoemacs-read-key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-read-key.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
