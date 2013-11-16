;;; ergoemacs-mode.el --- Emacs mode based on common modern software interface and ergonomics.

;; Copyright © 2007, 2008, 2009 by Xah Lee
;; Copyright © 2009, 2010 by David Capello
;; Copyright © 2012, 2013 by Matthew Fidler

;; Author: Xah Lee <xah@xahlee.org>
;;         David Capello <davidcapello@gmail.com>
;;         Matthew L. Fidler <matthew.fidler@gmail.com>
;; Maintainer: Matthew L. Fidler <matthew.fidler@gmail.com>
;; Created: August 01 2007
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

;; This keybinding set puts the most frequently used Emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see:
;; http://ergoemacs.github.io/ergoemacs-mode/

;; Todo:

;; 

;;; Acknowledgment:
;; Thanks to Shahin Azad for persian layout (fa) ishahinism at g
;; mail.com
;; Thanks to Thomas Rikl workhorse.t at googlemail.com for german layout
;; Thanks to Baptiste Fouques  bateast at bat.fr.eu.org for bepo layout
;; Thanks to Andrey Kotlarski (aka m00naticus) for a patch on 2012-12-08
;; Thanks to Nikolaj Schumacher for his implementation of extend-selection.
;; Thanks to Andreas Politz and Nikolaj Schumacher for correcting/improving implementation of toggle-letter-case.
;; Thanks to Lennart Borgman for several suggestions on code to prevent shortcuts involving shift key to start select text when CUA-mode is on.
;; Thanks to marciomazza for spotting several default bindings that
;; should have been unbound.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to maddin for ergoemacs-global/local-set-key functions and ergoemacs-hook-modes improvements.
;; Thanks to many users who send in comments and appreciations on this.
;; Layout contributors:
;; Danish layout “da”.  Contributors: Michael Budde
;; UK QWERTY layout “gb”.  Contributor: Jorge Dias (aka theturingmachine)
;; UK Dvorak layout “gb-dv”.  Contributor: Phillip Wood
;; French AZERTY layout “fr”.  Contributor: Alexander Doe
;; Italian QWERTY layout “it”.  Contributor: David Capello, Francesco Biccari


;;; Code:

;; (eval-when-compile (require 'cl))
;; FIXME: Use cl-lib when available.
(require 'cl)
(require 'easymenu)
(require 'cua-base)
(require 'cua-rect)

(defvar ergoemacs-debug ""
  "Debugging for `ergoemacs-mode'.")

(defvar ergoemacs-debug-buffer " *ErgoEmacs-mode Debug Log*"
  "Variable for ergoemacs debugging buffer.")

(defun ergoemacs-debug-keymap (keymap)
  "Print keymap bindings."
  (ergoemacs-debug-heading (format "Keymap Description: %s" (symbol-name keymap)))
  (ergoemacs-debug
   (substitute-command-keys (format "\\{%s}" (symbol-name keymap)))))

(defvar ergoemacs-debug-heading-start-time (float-time))
(defvar ergoemacs-debug-heading-last-time (float-time))

(defun ergoemacs-debug-heading (&rest arg)
  "Ergoemacs debugging heading."
  (ergoemacs-debug (make-string 80 ?=))
  (ergoemacs-debug (condition-case err
                       (apply 'format arg)
                     (error (format "Bad format string: %s" arg))))
  (ergoemacs-debug "Time Since Start ergoemacs-mode: %1f sec" (- (float-time) ergoemacs-debug-heading-start-time))
  (ergoemacs-debug "Time Since Last Heading: %1f sec" (- (float-time) ergoemacs-debug-heading-last-time))
  (ergoemacs-debug (make-string 80 ?=))
  (setq ergoemacs-debug-heading-last-time (float-time)))

(defun ergoemacs-debug (&rest arg)
  "Ergoemacs debugging facility."
  (interactive)
  (if (interactive-p)
      (progn
        (ergoemacs-debug-flush)
        (switch-to-buffer-other-window (get-buffer-create ergoemacs-debug-buffer))
        (setq ergoemacs-debug-buffer (replace-regexp-in-string "^ +" "" ergoemacs-debug-buffer))
        (rename-buffer ergoemacs-debug-buffer))
    (setq ergoemacs-debug
          (format "%s\n%s"
                  ergoemacs-debug
                  (condition-case err
                      (apply 'format arg)
                    (error (format "Bad Format String: %s" arg)))))))

(defun ergoemacs-debug-flush ()
  "Flushes ergoemacs debug to `ergoemacs-debug-buffer'"
  (save-excursion
    (with-current-buffer (get-buffer-create ergoemacs-debug-buffer) 
      (goto-char (point-max))
      (insert ergoemacs-debug)
      (delete-region (save-excursion (skip-chars-backward "\n\t ") (point)) (point))
      (insert "\n")))
  (setq ergoemacs-debug ""))

;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)


(load "ergoemacs-layouts")

;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.8.0"
  "Ergoemacs-keybindings minor mode version number.")

(defconst ergoemacs-mode-changes "Delete window Alt+0 changed to Alt+2.
Added beginning-of-buffer Alt+n (QWERTY notation) and end-of-buffer Alt+Shift+n")

(defgroup ergoemacs-mode nil
  "Emacs mode based on common modern software interface and ergonomics."
  :group 'editing-basics
  :group 'convenience
  :group 'emulations)

(defcustom ergoemacs-mode-used nil
  "Ergoemacs-keybindings minor mode version number used."
  :type 'string
  :group 'ergoemacs-mode)

(defvar ergoemacs-movement-functions
  '(scroll-down
    move-beginning-of-line move-end-of-line scroll-up
    scroll-down forward-block backward-block
    forward-word backward-word next-line previous-line
    forward-char backward-char ergoemacs-backward-block
    ergoemacs-forward-block ergoemacs-backward-open-bracket
    ergoemacs-forward-close-bracket move-end-of-line
    move-beginning-of-line backward-word forward-word
    subword-backward subword-forward
    beginning-of-buffer end-of-buffer)
  "Movement functions.")

(defvar ergoemacs-deletion-functions
  '(delete-backward-char
    delete-char backward-kill-word kill-word kill-line
    ergoemacs-shrink-whitespaces ergoemacs-kill-line-backward)
  "Deletion functions.")

(defvar ergoemacs-undo-redo-functions
  '(undo
    redo
    undo-tree-undo
    undo-tree-redo)
  "Undo and redo functions that ErgoEmacs is aware of...")

(defun ergoemacs-set-default (symbol new-value)
  "Ergoemacs equivalent to set-default.
Will reload `ergoemacs-mode' after setting the values."
  (set-default symbol new-value)
  (when (and (or (not (boundp 'ergoemacs-fixed-layout-tmp))
                 (save-match-data (string-match "ergoemacs-redundant-keys-" (symbol-name symbol))))
             (boundp 'ergoemacs-mode) ergoemacs-mode)
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)))

(defcustom ergoemacs-keyboard-layout (or (getenv "ERGOEMACS_KEYBOARD_LAYOUT") "us")
  (concat "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT.

Valid values are:

" (ergoemacs-get-layouts-doc))
  :type (ergoemacs-get-layouts-type)
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-change-smex-meta-x t
  "Changes the `smex-prompt-string' to match the `execute-extended-command'"
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-cua-rect-modifier-orig cua--rectangle-modifier-key)

(defcustom ergoemacs-cua-rect-modifier 'super
  "Change the CUA rectangle modifier to this key."
  :type '(choice
          (const :tag "Do not modify the cua-rectangle modifier" nil)
          (const :tag "Meta Modifier" 'meta)
          (const :tag "Super Modifier" 'super)
          (const :tag "Hyper Modifier" 'hyper)
          (const :tag "Alt Modifier" 'alt))
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-repeat-movement-commands nil
  "Allow movement commands to be repeated without pressing the ALT key."
  :group 'ergoemacs-mode
  :type '(choice
          (const :tag "Do not allow fast repeat commands." nil)
          (const :tag "Allow fast repeat command of the current movement command" 'single)
          (const :tag "Allow fast repeat of all movement commands" 'all)))

(defcustom ergoemacs-repeat-undo-commands 'apps
  "Allow undo commands to be repeated without pressing the entire key.  For example if <apps> z is undo, then <apps> z z sould be undo twice if enabled."
  :group 'ergoemacs-mode
  :type '(choce
          (const :tag "Do not allow fast repeat commands." nil)
          (const :tag "Allow fast repeat for <apps> menu." 'apps)))


(when (not (fboundp 'set-temporary-overlay-map))
  ;; Backport this function from newer emacs versions
  (defun set-temporary-overlay-map (map &optional keep-pred)
    "Set a new keymap that will only exist for a short period of time.
The new keymap to use must be given in the MAP variable. When to
remove the keymap depends on user input and KEEP-PRED:

- if KEEP-PRED is nil (the default), the keymap disappears as
  soon as any key is pressed, whether or not the key is in MAP;

- if KEEP-PRED is t, the keymap disappears as soon as a key *not*
  in MAP is pressed;

- otherwise, KEEP-PRED must be a 0-arguments predicate that will
  decide if the keymap should be removed (if predicate returns
  nil) or kept (otherwise). The predicate will be called after
  each key sequence."    
    (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
           (overlaysym (make-symbol "t"))
           (alist (list (cons overlaysym map)))
           (clearfun
            `(lambda ()
               (unless ,(cond ((null keep-pred) nil)
                              ((eq t keep-pred)
                               `(eq this-command
                                    (lookup-key ',map
                                                (this-command-keys-vector))))
                              (t `(funcall ',keep-pred)))
                 (remove-hook 'pre-command-hook ',clearfunsym)
                 (setq emulation-mode-map-alists
                       (delq ',alist emulation-mode-map-alists))))))
      (set overlaysym overlaysym)
      (fset clearfunsym clearfun)
      (add-hook 'pre-command-hook clearfunsym)
      
      (push alist emulation-mode-map-alists))))

(defvar ergoemacs-undo-apps-keymap nil
  "Keymap for repeating undo/redo commands in apps menu.")

(defun ergoemacs-undo-apps-text nil
  "Text for repeat undo/redo commands in apps menu.")

(defun ergoemacs-create-undo-apps-keymap ()
  "Create `ergoemacs-undo-apps-keymap', based on current ergoemacs keybindings."
  (let ((ergoemacs-undo-key
         (replace-regexp-in-string "<\\(apps\\|menu\\)> " "" (key-description (ergoemacs-key-fn-lookup 'undo t))))
        (ergoemacs-redo-key
         (replace-regexp-in-string "<\\(apps\\|menu\\)> " "" (key-description (ergoemacs-key-fn-lookup 'redo t)))))
    (setq ergoemacs-undo-apps-text (format "Undo repeat key `%s'; Redo repeat key `%s'"
                                           ergoemacs-undo-key ergoemacs-redo-key))
    (setq ergoemacs-undo-apps-keymap (make-keymap))
    (define-key ergoemacs-undo-apps-keymap (read-kbd-macro ergoemacs-undo-key) 'undo)
    (define-key ergoemacs-undo-apps-keymap (read-kbd-macro ergoemacs-redo-key) 'redo)))

(defmacro ergoemacs-create-undo-advices (command)
  "Creates repeat advices for undo/redo commands defined in `ergoemacs-undo-redo-functions'. The repeat behavior is defined by `ergoemacs-repeat-undo-commands'.ergoemacs-repeat-undo-commands"
  `(defadvice ,(intern (symbol-name command)) (around ergoemacs-undo-redo-advice activate)
     ,(format "ErgoEmacs fast keymap for `%s'" (symbol-name command))
     ad-do-it
     (when (and ergoemacs-mode (eq ergoemacs-repeat-undo-commands 'apps))
       (message
        "%s" ergoemacs-undo-apps-text)
         (set-temporary-overlay-map ergoemacs-undo-apps-keymap t))))

(mapc
 (lambda(x)
   (eval `(ergoemacs-create-undo-advices ,x)))
 ergoemacs-undo-redo-functions)

(defvar ergoemacs-check-mode-line-change nil)
(defmacro ergoemacs-create-movement-commands (command)
  "Creates a shifted and repeat advices and isearch commands."
  `(progn
     ,(if (eq 'backward-char command)
          `(defun ,(intern (concat "ergoemacs-isearch-" (symbol-name command))) (&optional arg)
             ,(format "Ergoemacs isearch movement command for `%s'.  Behviour controlled with `ergoemacs-isearch-backward-char-to-edit'.  A prefix command will temporarily toggle if the keyboard will edit the item." (symbol-name command))
             (interactive "^P")
             (if (or (and arg (not ergoemacs-isearch-backward-char-to-edit))
                     (and (not arg) ergoemacs-isearch-backward-char-to-edit))
                 (isearch-edit-string)
               (isearch-exit)
               (condition-case err
                   (call-interactively (or (command-remapping ',command (point)) ',command))
                 (error
                  (beep)
                  (message "%s" err)))
               
               (setq this-command ',command))))
     (defvar ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")) (make-sparse-keymap)
       ,(format "Ergoemacs fast keymap for `%s'." (symbol-name command)))
     ;; Change to advices
     (defadvice ,(intern (symbol-name command)) (around ergoemacs-movement-advice activate)
       ,(format "Ergoemacs advice for command for `%s'.
May install a fast repeat key based on `ergoemacs-repeat-movement-commands',  `ergoemacs-full-fast-keys-keymap' and `ergoemacs-fast-%s-keymap'.
" (symbol-name command) (symbol-name command))
       ad-do-it
       (when (and ergoemacs-mode ergoemacs-repeat-movement-commands
                  (called-interactively-p 'any)
                  (not cua--rectangle-overlays)) ;; Don't add overlays to rectangles
         (set-temporary-overlay-map (cond
                                     ((eq ergoemacs-repeat-movement-commands 'single)
                                      (unless (where-is-internal last-command (list ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap"))) t)
                                        (setq ergoemacs-check-mode-line-change (list ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap"))))
                                        (message ,(format "Repeat last movement(%s) key: %%s" (symbol-name command))
                                                 (replace-regexp-in-string
                                                  "M-" "" (key-description (this-single-command-keys))))
                                        (ergoemacs-mode-line (format " %sSingle" (ergoemacs-unicode-char "↔" "<->"))))
                                      ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")))
                                     ((eq ergoemacs-repeat-movement-commands 'all)
                                      (unless (where-is-internal last-command (list ergoemacs-full-fast-keys-keymap) t)
                                        (setq ergoemacs-check-mode-line-change (list ergoemacs-full-fast-keys-keymap))
                                        ;; (message "Repeating movement keys installed")
                                        (ergoemacs-mode-line (format " %sFull" (ergoemacs-unicode-char "↔" "<->"))))
                                      ergoemacs-full-fast-keys-keymap)
                                     (t ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")))) t)))))
(mapc
 (lambda(x)
   (eval `(ergoemacs-create-movement-commands ,x)))
 ergoemacs-movement-functions)

(defvar ergoemacs-M-O-keymap (make-sparse-keymap)
  "M-O translation map.")
(define-key ergoemacs-M-O-keymap [exit] 'ignore)

(defvar ergoemacs-M-o-keymap (make-sparse-keymap)
  "M-o translation map.")
(define-key ergoemacs-M-o-keymap [exit] 'ignore)

(defun ergoemacs-cancel-M-O ()
  "Cancels M-O [timeout] key."
  (setq ergoemacs-push-M-O-timeout nil)
  (when (timerp ergoemacs-M-O-timer)
    (cancel-timer ergoemacs-M-O-timer)))

(defvar ergoemacs-push-M-O-timeout nil
  "Should the M-O [timeout] key be canceled?")

(defvar ergoemacs-curr-prefix-arg nil)

(defvar ergoemacs-fix-M-O t
  "Fixes the ergoemacs M-O console translation.")

(defvar ergoemacs-M-O-delay 0.01
  "Number of seconds before sending the M-O event instead of sending the terminal's arrow key equivalent.")

(defvar ergoemacs-M-O-timer nil
  "Timer for the M-O")

(defun ergoemacs-exit-M-O-keymap ()
  "Exit M-O keymap and cancel the `ergoemacs-M-O-timer'"
  (setq ergoemacs-push-M-O-timeout nil)
  (when (timerp ergoemacs-M-O-timer)
    (cancel-timer ergoemacs-M-O-timer))
  nil)

(defvar ergoemacs-shortcut-keys nil)
(defvar ergoemacs-unbind-keys nil)

(defvar ergoemacs-M-O-prefix-keys nil)
(defvar ergoemacs-M-O-function nil)
(defun ergoemacs-M-O-timeout ()
  "Push timeout on unread command events."
  (when ergoemacs-push-M-O-timeout
    (setq ergoemacs-push-M-O-timeout nil)
    (cond
     ((and (condition-case err
               (interactive-form ergoemacs-M-O-function)
             (error nil))
           ergoemacs-M-O-prefix-keys
           (equal 'ignore
               (key-binding
                (read-kbd-macro
                 (format "%s <exit>"
                         ergoemacs-M-O-prefix-keys)))))
      (ergoemacs-send-fn ergoemacs-M-O-prefix-keys ergoemacs-M-O-function)
      (setq unread-command-events (cons 'exit unread-command-events)))
     (ergoemacs-M-O-prefix-keys
      (let (fn)
        (let (ergoemacs-shortcut-keys)
          (setq fn (key-binding
                    (read-kbd-macro
                     (format "%s <timeout>"
                             ergoemacs-M-O-prefix-keys)))))
        (unless fn
          (let (message-log-max
                ergoemacs-mode
                ergoemacs-unbind-keys
                nk)
            (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
            (unwind-protect
                (setq nk (key-binding (read-kbd-macro prefix-keys) t nil (point)))
              (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist))
            (if nk
                (setq fn nk)
              (beep)
              (message "Nothing is assigned to %s" ergoemacs-M-O-prefix-keys))))
        ;; Lookup keys, and then send <exit> event.
        (when fn
          (condition-case err
              (ergoemacs-send-fn ergoemacs-M-O-prefix-keys fn)
            (error
             (beep)
             (message "%s" err))))
        (reset-this-command-lengths)
        ))
     (t ;; Prefix arg required when sending unread command events 
      (setq prefix-arg ergoemacs-curr-prefix-arg)
      (reset-this-command-lengths)
      (setq unread-command-events (cons 'timeout unread-command-events))))
    (setq ergoemacs-M-O-function nil)
    (setq ergoemacs-M-O-prefix-keys nil)))

(defun ergoemacs-M-o (&optional arg use-map)
  "Ergoemacs M-o function.
Allows arrow keys and the to work in the terminal. Call the true
function immediately when `window-system' is true."
  (interactive "P")
  (setq ergoemacs-curr-prefix-arg current-prefix-arg)
  (let ((map (or use-map ergoemacs-M-o-keymap))
        (prefix-keys (if use-map "M-O" "M-o")))
    (if window-system
        (let ((fn (lookup-key map [timeout] t)))
          (if fn
              (condition-case err
                  (call-interactively
                   (or (command-remapping fn (point)) fn))
                (error
                 (beep)
                 (let (message-log-max)
                   (message "%s" err))))
            (let (message-log-max
                  ergoemacs-mode
                  ergoemacs-unbind-keys
                  nk)
              (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
              (unwind-protect
                  (setq nk (key-binding (read-kbd-macro prefix-keys) t nil (point)))
                (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist))s
              
              (if nk
                  (call-interactively nk)
                (beep)
                (message "Nothing is assigned to %s" prefix-keys)))))
      (when (timerp ergoemacs-M-O-timer)
        (cancel-timer ergoemacs-M-O-timer)
        ;; Issue correct command.
        (let ((window-system t))
          (ergoemacs-M-o arg use-map)))
      (setq ergoemacs-push-M-O-timeout t)
      (setq ergoemacs-M-O-prefix-keys prefix-keys)
      (set-temporary-overlay-map map 'ergoemacs-exit-M-O-keymap)
      (setq ergoemacs-M-O-timer (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-M-O-timeout)))))

(defun ergoemacs-M-O (&optional arg)
  "Ergoemacs M-O function to allow arrow keys and the like to
work in the terminal."
  (interactive "P")
  (ergoemacs-M-o arg ergoemacs-M-O-keymap))

(load "ergoemacs-themes")
(load "ergoemacs-unbind")

;;; ergoemacs-keymap


(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

(defvar ergoemacs-shortcut-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode shortcut keymap")

(defvar ergoemacs-shortcut-override-keymap (make-sparse-keymap)
  "Keymap for overriding keymap.")

(load "ergoemacs-modal")
(load "ergoemacs-functions")
(load "ergoemacs-translate")
(load "ergoemacs-shortcuts")


(defvar ergoemacs-backward-compatability-variables
  '((ergoemacs-backward-paragraph-key            backward-block)
    (ergoemacs-forward-paragraph-key             forward-block)
    (ergoemacs-recenter-key                      recenter-top-bottom)
    (ergoemacs-kill-region-key                   cut-line-or-region)
    (ergoemacs-kill-ring-save-key                copy-line-or-region))
  "Backward compatible variables that do not follow the convention ergoemacs-FUNCTION-key")

(defun ergoemacs-setup-backward-compatability ()
  "Set up backward-compatible variables"
  (mapc
   (lambda(var)
     (condition-case err
         (eval `(setq ,(intern (concat "ergoemacs-" (symbol-name (nth 1 var)) "-key")) (ergoemacs-kbd (nth 0 var))))
       (error (ergoemacs-debug "Ignored backward compatability for %s" (nth 1 var)))))
   (symbol-value (ergoemacs-get-variable-layout)))
  (mapc
   (lambda(var)
     (let ((saved-var (intern-soft (concat "ergoemacs-" (symbol-name (nth 1 var)) "-key"))))
       (when saved-var
         (set (nth 0 var) (symbol-value saved-var)))))
   ergoemacs-backward-compatability-variables))



(defun ergoemacs-mode-line (&optional text)
  "Set ergoemacs-mode-line"
  ;; (ergoemacs-debug-heading "Set Mode Line to %s" (or text "Default"))
  (if text
      (setq minor-mode-alist
            (mapcar (lambda(x)
                      (if (not (eq 'ergoemacs-mode (nth 0 x)))
                          x
                        `(ergoemacs-mode ,text)))
                    minor-mode-alist))
    (setq minor-mode-alist
          (mapcar (lambda(x)
                    (if (not (eq 'ergoemacs-mode (nth 0 x)))
                        x
                      `(ergoemacs-mode ,(concat
                                         (if (not ergoemacs-theme)
                                             " ErgoEmacs"
                                           (concat " Ergo"
                                                   (upcase (substring ergoemacs-theme 0 1))
                                                   (substring ergoemacs-theme 1)))
                                         "[" ergoemacs-keyboard-layout "]"))))
                  minor-mode-alist)))
  (ergoemacs-debug-flush))

(require 'lookup-word-on-internet nil "NOERROR")
(load "ergoemacs-extras")

;; ErgoEmacs hooks


(defun ergoemacs-hook-define-key (keymap key-def definition translate)
  "Ergoemacs `define-key' in hook."
  (if (or (not (condition-case err
                   (keymapp keymap)
                 (error nil)))
          (not key-def)) nil
    (let ((fn definition))
      (when (stringp definition)
        (setq fn `(lambda(&optional arg)
                    (interactive "P")
                    (setq this-command last-command) ; Don't record this command.
                    ;; (setq prefix-arg current-prefix-arg)
                    (ergoemacs-shortcut-internal ,definition))))
      (if (and (eq translate 'remap)
               (functionp key-def)
               (condition-case err
                   (interactive-form fn)
                 (error nil)))
          (let ((no-ergoemacs-advice t))
            (define-key keymap
              (eval (macroexpand `[remap ,(intern (symbol-name key-def))]))
              fn))
        (let* ((no-ergoemacs-advice t)
               (key-code
                (cond
                 ((and (or translate ergoemacs-change-fixed-layout-to-variable-layout)
                       (eq 'string (type-of key-def)))
                  (ergoemacs-kbd key-def))
                 ((eq 'string (type-of key-def))
                  (condition-case err
                      (read-kbd-macro key-def)
                    (error (read-kbd-macro
                            (encode-coding-string key-def locale-coding-system)))))
                 ((ergoemacs-key-fn-lookup key-def)
                  ;; Also define <apps> key
                  (when (and (ergoemacs-key-fn-lookup key-def t)
                             (condition-case err
                                 (interactive-form fn)
                               (error nil)))
                    (define-key keymap (ergoemacs-key-fn-lookup key-def t) fn))
                  (ergoemacs-key-fn-lookup key-def))
                 ;; Define <apps>  key
                 ((ergoemacs-key-fn-lookup key-def t)
                  (ergoemacs-key-fn-lookup key-def t)
                  nil)
                 (t
                  (if (and (functionp key-def)
                           (condition-case err
                               (interactive-form fn)
                             (error nil)))
                      (eval
                       (macroexpand `[remap ,(intern (symbol-name key-def))]))
                    nil)))))
          (ergoemacs-debug "hook: %s->%s %s %s"
                           key-def key-code
                           fn translate)
          (when (and key-code
                     (condition-case err
                         (interactive-form fn)
                       (error nil)))
            (define-key keymap key-code fn)))))))

(defmacro ergoemacs-create-hook-function (hook keys &optional global)
  "Creates a hook function based on the HOOK and the list of KEYS defined."
  (let ((is-emulation-p (make-symbol "is-emulation-p"))
        (is-major-mode-p (make-symbol "is-major-mode-p"))
        (old-keymap (make-symbol "old-keymap"))
        (override-keymap (make-symbol "override-keymap")))
    (if (not (listp keys))
        (progn
          `(progn
             (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap")) nil
               ,(concat "Old keymap for `" (symbol-name hook) "'."))
             (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
               ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-create-hook-function'.")
               ;; Only generate keymap if it hasn't previously been
               ;; generated.
               (ergoemacs-vars-sync)
               (if (not ergoemacs-mode)
                   (progn
                     (ergoemacs-debug ,(format "WARNING: %s not removed."
                                               (intern (concat "ergoemacs-" (symbol-name hook))))))
                 (unless ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))
                   (ergoemacs-debug-heading ,(concat "Run ergoemacs-" (symbol-name hook)))
                   (setq ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))
                         (copy-keymap ,keys))
                   (ergoemacs-install-shortcuts-map ,keys (memq ',keys ergoemacs-maps-only-install-shortcuts))
                   (define-key ,keys
                     (read-kbd-macro  "<ergoemacs>") 'ignore)
                   (ergoemacs-debug-keymap ',keys)
                   (ergoemacs-vars-sync)
                   (ergoemacs-debug-flush)))
               t)
             (ergoemacs-add-hook ',hook
                                 ',(intern (concat "ergoemacs-" (symbol-name hook)))
                                 ',(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))
                                 nil)))
      (setq is-emulation-p (or (not (nth 2 (nth 0 keys)))
                               (eq 'emulation-mode-map-alists (nth 2 (nth 0 keys)))))
      (when is-emulation-p
        (setq is-major-mode-p
              (intern-soft
               (replace-regexp-in-string
                "-hook$" "" (symbol-name hook)))))
      `(progn
         (ergoemacs-debug-heading ,(format "ergoemacs-create-hook-function for %s" (symbol-name hook)))
         (ergoemacs-debug ,(format "Emulation: %s" is-emulation-p))
         ,(if is-emulation-p
              (progn
                (setq old-keymap nil)
                `(progn
                   (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) nil
                     ,(concat "Ergoemacs overriding keymap for `" (symbol-name hook) "'"))))
            (setq old-keymap t)
            (setq override-keymap (nth 2 (nth 0 keys)))
            `(defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap")) nil
               ,(concat "Old keymap for `" (symbol-name hook) "'.")))

         (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
           ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-create-hook-function'.")
           ;; Only generate keymap if it hasn't previously been
           ;; generated.
           (ergoemacs-vars-sync)
           (if  (not ergoemacs-mode)
               (progn
                 (ergoemacs-debug ,(format "WARNING: %s not removed."
                                           (intern (concat "ergoemacs-" (symbol-name hook))))))
             (unless ,(if is-emulation-p
                          `(assq ,(if is-major-mode-p
                                      '(intern (format "ergoemacs--emulation-for-%s" major-mode))
                                    `(intern ,(concat "ergoemacs--emulation-for-" (symbol-name hook))))
                                 ergoemacs-emulation-mode-map-alist)
                        (intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap")))
               (ergoemacs-debug-heading ,(concat "Run ergoemacs-" (symbol-name hook)))
               ,(if  is-emulation-p
                    `(setq ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) (make-sparse-keymap))
                  `(progn
                     (setq ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))
                           (copy-keymap ,(nth 2 (nth 0 keys))))
                     (ergoemacs-install-shortcuts-map ,(nth 2 (nth 0 keys)) (memq ',(nth 2 (nth 0 keys)) ergoemacs-maps-only-install-shortcuts))
                     (define-key ,(nth 2 (nth 0 keys))
                       (read-kbd-macro  "<ergoemacs>") 'ignore)))
               ,@(mapcar
                  (lambda(def)
                    `(ergoemacs-hook-define-key
                      ,(if (and is-emulation-p
                                (or (not (nth 2 (nth 0 keys)))
                                    (equal (nth 2 def)
                                           'emulation-mode-map-alists)))
                           (intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))
                         (nth 2 def))
                      ,(if (eq (type-of (nth 0 def)) 'string)
                           `,(nth 0 def)
                         `(quote ,(nth 0 def)))
                      ',(nth 1 def)
                      ',(nth 3 def)))
                  keys)
               ,(if (not  is-emulation-p)
                    `(ergoemacs-debug-keymap ',(nth 2 (nth 0 keys))))
               ;;(set-default ',(nth 2 (nth 0 keys)) ,(nth 2 (nth 0 keys)))
               ,(when is-emulation-p
                  `(progn
                     (let ((name ,(if is-major-mode-p
                                      '(intern (format "ergoemacs--emulation-for-%s" major-mode))
                                    `(intern ,(concat "ergoemacs--emulation-for-" (symbol-name hook))))))
                       (ergoemacs-debug-keymap ',(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                       (let ((x (assq name ergoemacs-emulation-mode-map-alist)))
                         ;; Delete keymap.
                         (if x
                             (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
                         ;; Put at the end of the list
                         (setq ergoemacs-emulation-mode-map-alist
                               (append ergoemacs-emulation-mode-map-alist
                                       (list (cons name ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))))))
                       (ergoemacs-debug-keymap ',(intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))))))
               (ergoemacs-debug-heading ,(concat "Finish ergoemacs-" (symbol-name hook)))
               (ergoemacs-vars-sync)
               (ergoemacs-debug-flush)
               t)
             ,(if is-emulation-p
                 `(let ((name ,(if is-major-mode-p
                                   '(intern (format "ergoemacs--emulation-for-%s" major-mode))
                                 `(intern ,(concat "ergoemacs--emulation-for-" (symbol-name hook))))))
                    (set-default name nil)
                    (set (make-local-variable name) t)))))
         (ergoemacs-add-hook ',hook ',(intern (concat "ergoemacs-" (symbol-name hook))) ',(if old-keymap (intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))) ',override-keymap)))))

(defun ergoemacs-pre-command-install-minor-mode-overriding-map-alist ()
  "Install `minor-mode-overriding-map-alist' if it didn't get installed (like in some `org-mode')."
  (let ((hook (intern-soft (format "ergoemacs-%s-hook" major-mode))))
    (when hook
      (funcall hook))))

(defvar ergoemacs-hook-list (list)
  "List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function old-keymap keymap-name)
  "Adds a pair of HOOK and HOOK-FUNCTION to the list `ergoemacs-hook-list'."
  (add-to-list 'ergoemacs-hook-list (list hook hook-function old-keymap keymap-name)))

(defvar ergoemacs-advices '()
  "List of advices to enable and disable when ergoemacs is running.")

(defun ergoemacs-hook-modes ()
  "Installs/Removes ergoemacs minor mode hooks from major modes
depending the state of `ergoemacs-mode' variable."
  (let ((modify-advice (if (and (boundp 'ergoemacs-mode) ergoemacs-mode) 'ad-enable-advice 'ad-disable-advice)))    
    ;; when ergoemacs-mode is on, activate hooks and unset global keys, else do inverse
    (if (and (boundp 'ergoemacs-mode) ergoemacs-mode (not (equal ergoemacs-mode 0)))
        (progn
          ;; alt+n is the new "Quit" in query-replace-map
          (when (ergoemacs-key-fn-lookup 'keyboard-quit)
            ;; Not sure if this is 
            ;; (ergoemacs-unset-global-key query-replace-map "\e")
            (define-key query-replace-map (ergoemacs-key-fn-lookup 'keyboard-quit) 'exit-prefix)))
      ;; if ergoemacs was disabled: restore original keys
      (define-key query-replace-map (read-kbd-macro "C-g") 'exit-prefix))
    
    ;; install the mode-hooks
    (dolist (hook ergoemacs-hook-list)
      (if (and (boundp 'ergoemacs-mode) ergoemacs-mode)
          (add-hook (nth 0 hook) (nth 1 hook) t)
        (remove-hook (nth 0 hook) (nth 1 hook)))
      ;; Restore original keymap
      (when (and (not (and (boundp 'ergoemacs-mode) ergoemacs-mode))
                 (nth 2 hook)
                 (nth 3 hook)
                 (symbol-value (nth 2 hook))
                 (symbol-value (nth 3 hook)))
        (set (nth 3 hook)
             (copy-keymap (symbol-value (nth 2 hook))))
        (set (nth 2 hook) nil)))
    
    ;; enable advices
    (mapc
     (lambda(advice)
       (condition-case err
           (let ((fn (intern (replace-regexp-in-string "\\(^ergoemacs-\\|-advice$\\)" "" (symbol-name advice)))))
             (funcall modify-advice fn 'around advice)
             (ad-activate fn))
         (error "Error modifying advice %s" (symbol-name advice))))
     ergoemacs-advices)))

(defun ergoemacs-create-hooks ()
  "Creates Ergoemacs Hooks from `ergoemacs-get-minor-mode-layout'."
  (let ((ergoemacs-mode))
    (ergoemacs-hook-modes))
  (setq ergoemacs-hook-list nil)
  (mapc
   (lambda(x)
     (cond
      ((string-match "-hook$" (symbol-name (car x)))
       (let ((f (macroexpand `(ergoemacs-create-hook-function ,(car x) ,(car (cdr x))))))
         (eval f)))
      (t ;; not a hook, assume it is a variable
       (let ((var (car x))
             (keys (car (cdr x)))
             (map (make-sparse-keymap)))
         (mapc
          (lambda(args)
            (ergoemacs-hook-define-key
             map (nth 0 args) (nth 1 args) (nth 3 args)))
	  keys)
         (let ((x (assq var ergoemacs-emulation-mode-map-alist)))
           (when x
             (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
           (push (cons var map) ergoemacs-emulation-mode-map-alist))))))
   (symbol-value (ergoemacs-get-minor-mode-layout)))
  (ergoemacs-hook-modes))

(defun ergoemacs-setup-keys (&optional no-check)
  "Setups keys based on a particular layout. Based on `ergoemacs-keyboard-layout'."
  (interactive)
  (ergoemacs-debug "Ergoemacs layout: %s" ergoemacs-keyboard-layout)
  (ergoemacs-debug "Ergoemacs theme: %s" ergoemacs-theme)
  (ergoemacs-debug "Emacs Version: %s" (emacs-version))
  (let ((ergoemacs-state (if (boundp 'ergoemacs-mode) ergoemacs-mode nil))
        (layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode -1))))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))
    (ergoemacs-unbind-setup-keymap)
    (ergoemacs-create-hooks)
    ;; Add ergoemacs-shortcut
    (let ((x (assq 'ergoemacs-shortcut-keys ergoemacs-emulation-mode-map-alist)))
      (when x
        (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
      (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist))
    ;; Add ergoemacs-unbind
    (let ((x (assq 'ergoemacs-unbind-keys minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      ;; Put at the END of the list.
      (setq minor-mode-map-alist
            (append minor-mode-map-alist
                    (list (cons 'ergoemacs-unbind-keys ergoemacs-unbind-keymap)))))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode 1))))))

(defun ergoemacs-lookup-execute-extended-command ()
  "Lookup the execute-extended-command"
  (key-description
   (or (ergoemacs-key-fn-lookup 'execute-extended-command)
       (ergoemacs-key-fn-lookup 'smex)
       (ergoemacs-key-fn-lookup 'helm-M-x))))




(require 'cus-edit)

(defun ergoemacs-check-for-new-version ()
  "This allows the user to keep an old-version of keybindings if they change."
  (condition-case err
      (progn
        (when ergoemacs-mode
          ;; Apply any settings...
          (ergoemacs-debug "Reset ergoemacs-mode.")
          (ergoemacs-mode -1)
          (ergoemacs-mode 1))
        (when (and
               (custom-file t) ;; Make sure a custom file exists.
               (not ergoemacs-theme) ;; Ergoemacs default used.
               (or (not ergoemacs-mode-used)
                   (not (string= ergoemacs-mode-used ergoemacs-mode-version))))
          (if (yes-or-no-p
               (format "Ergoemacs keybindings changed, %s; Would you like to change as well?"
                       ergoemacs-mode-changes))
              (progn
                (setq ergoemacs-mode-used ergoemacs-mode-version)
                (customize-save-variable 'ergoemacs-mode-used (symbol-value 'ergoemacs-mode-used))
                (customize-save-variable 'ergoemacs-theme (symbol-value 'ergoemacs-theme))
                (customize-save-customized))
            (when (not ergoemacs-mode-used)
              (setq ergoemacs-mode-used "5.7.5"))
            (setq ergoemacs-theme ergoemacs-mode-used)
            (customize-save-variable 'ergoemacs-mode-used (symbol-value 'ergoemacs-mode-used))
            (customize-save-variable 'ergoemacs-theme (symbol-value 'ergoemacs-theme))
            (customize-save-customized))))
    (error nil)))

(add-hook 'emacs-startup-hook 'ergoemacs-check-for-new-version)
(defvar ergoemacs-old-ns-command-modifier nil)
(defvar ergoemacs-old-ns-alternate-modifier nil)

(defcustom ergoemacs-use-mac-command-as-meta t
  "Use Mac's command/apple key as emacs meta-key when enabled."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-menus t
  "Use ergoemacs menus"
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-save-variables
  '((org-CUA-compatible t)
    (shift-select-mode t)
    (delete-selection-mode 1)
    (set-mark-command-repeat-pop t)
    (org-special-ctrl-a/e t)
    (ido-vertical-define-keys C-n-C-p-up-down-left-right C-n-and-C-p-only)
    (scroll-error-top-bottom t))
  "Variables/Modes that `ergoemacs-mode' changes, and restores.
Initial state is what `ergoemacs-mode' toggles to when it is turned on.
When the state is 1 or -1 it turns on/off the corresponding mode."
  :type '(repeat
          (list 
           (symbol :tag "Variable/Mode")
           (choice
            (const :tag "Set Variable to t" t)
            (const :tag "Set Variable to nil" nil)
            (const :tag "Turn ON mode" 1)
            (const :tag "Turn OFF mode" -1)
            (sexp :tag "Value to set"))
           (choce
            (const :tag "Restore opposite behavior" nil)
            (const :tag "Value to revert"))))
  :group 'ergoemacs-mode)
(defvar ergoemacs-save-variables-actual nil)
(defvar ergoemacs-save-variables-state nil)

(defvar ergoemacs-emulation-mode-map-alist nil
  "Override keys in ergoemacs-mode for `emulation-mode-map-alist'")

;; ErgoEmacs minor mode
;;;###autoload
(define-minor-mode ergoemacs-mode
  "Toggle ergoemacs keybinding minor mode.
This minor mode changes your emacs keybinding.

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

Home page URL `http://ergoemacs.github.io/ergoemacs-mode/'

For the standard layout, with A QWERTY keyboard the `execute-extended-command' M-x is now M-a.

The layout and theme changes the bindings.  For the current
bindings the keymap is:

\\{ergoemacs-keymap}
"
  nil
  :lighter " ErgoEmacs"
  :global t
  :group 'ergoemacs-mode
  :keymap ergoemacs-keymap
  
  ;; Try to turn on only rectangle support, global mark mode, and
  ;; other features of CUA mode.  Let ergoemacs handle C-c and C-v.
  ;; This will possibly allow swapping of C-c and M-c.
  (if ergoemacs-mode
      (progn
        (setq ergoemacs-debug-heading-start-time (float-time))
        (setq ergoemacs-debug-heading-last-time (float-time))
        (ergoemacs-debug-heading "Ergoemacs-mode is turning ON.")
        (when cua-mode
          (cua-mode -1)
          (cua-selection-mode 1))
        ;; (if (boundp 'org-CUA-compatible)
        ;;     (setq ergoemacs-org-CUA-compatible nil)
        ;;   (setq ergoemacs-org-CUA-compatible org-CUA-compatible))
        (unless ergoemacs-save-variables-state
          (setq ergoemacs-save-variables-actual ergoemacs-save-variables)
          (ergoemacs-debug-heading "Update variables and modes")
          (ergoemacs-debug "Old ergoemacs-save-variables: %s" ergoemacs-save-variables)
          (setq ergoemacs-save-variables-actual
                (mapcar
                 (lambda(x)
                   (let (val val2)
                     (if (condition-case err (or (= 1 (nth 1 x)) (= -1 (nth 1 x))) (error nil))
                         (progn
                           (ergoemacs-debug "Call (%s %s)"
                                            (symbol-name (nth 0 x)) (nth 1 x))
                           (funcall (nth 0 x) (nth 1 x))
                           (setq val (if (= (nth 1 x) 1) -1 1)))
                       (ergoemacs-debug "Set %s to %s" (symbol-name (nth 0 x)) (nth 1 x))
                       (set (nth 0 x) (nth 1 x))
                       (set-default (nth 0 x) (nth 1 x))
                       (if (not (nth 2 x))
                           (setq val (not (nth 1 x)))
                         (setq val (nth 2 x))
                         (setq val2 (nth 1 x))))
                     `(,(nth 0 x) ,val ,val2)))
                 ergoemacs-save-variables-actual))
          (ergoemacs-debug "New ergoemacs-save-variables: %s" ergoemacs-save-variables)
          (setq ergoemacs-save-variables-state t))
        ;; From yasnippet:
        ;; Install the direct keymaps in `emulation-mode-map-alists'
        ;; (we use `add-hook' even though it's not technically a hook,
        ;; but it works). Then define variables named after modes to
        ;; index `ergoemacs-emulation-mode-map-alist'.
        (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
        ;; Setup keys
        (setq ergoemacs-shortcut-keymap (make-sparse-keymap))
        (ergoemacs-setup-keys t)
        (ergoemacs-debug-heading "Ergoemacs Keys have loaded.")
        (when (eq system-type 'darwin)
          (let ((cm (or (intern-soft "ns-command-modifier")
                        (intern-soft "mac-command-modifier")))
                (am (or (intern-soft "ns-alternate-modifier")
                        (intern-soft "mac-command-modifier"))))
            (when cm
              (setq ergoemacs-old-ns-command-modifier (symbol-value cm))
              (set cm 'meta))
            (when am
              (setq ergoemacs-old-ns-alternate-modifier ns-alternate-modifier)
              (set am nil))))
        ;; Turn on menu
        (if ergoemacs-use-menus
            (progn
              (require 'ergoemacs-menus)
              (ergoemacs-menus-on))
          (when (featurep 'ergoemacs-menus)
            (ergoemacs-menus-off)))
        (ergoemacs-debug-heading "Ergoemacs Menus have loaded.")
        (setq cua--rectangle-modifier-key ergoemacs-cua-rect-modifier)
        (setq cua--rectangle-keymap (make-sparse-keymap))
        (setq cua--rectangle-initialized nil)
        (cua--init-rectangles)
        (setq cua--keymap-alist
              `((cua--ena-prefix-override-keymap . ,cua--prefix-override-keymap)
                (cua--ena-prefix-repeat-keymap . ,cua--prefix-repeat-keymap)
                (cua--ena-cua-keys-keymap . ,cua--cua-keys-keymap)
                (cua--ena-global-mark-keymap . ,cua--global-mark-keymap)
                (cua--rectangle . ,cua--rectangle-keymap)
                (cua--ena-region-keymap . ,cua--region-keymap)
                (cua-mode . ,cua-global-keymap)))
        (mapc ;; Now install hooks.
         (lambda(buf)
           (with-current-buffer buf
             (when (and (intern-soft (format "ergoemacs-%s-hook" major-mode)))
               (funcall (intern-soft (format "ergoemacs-%s-hook" major-mode))))))
         (buffer-list))
        (setq ergoemacs-shortcut-keys t)
        (setq ergoemacs-unbind-keys t)
        (add-hook 'pre-command-hook 'ergoemacs-pre-command-hook)
        (add-hook 'minibuffer-setup-hook #'ergoemacs-minibuffer-setup)
        (ergoemacs-debug-heading "Ergoemacs-mode turned ON."))
    ;; turn off ergoemacs-mode
    (ergoemacs-debug-heading "Ergoemacs-mode is turning OFF.")
    (when ergoemacs-save-variables-state
      (ergoemacs-debug-heading "Revert variables and modes")
      (ergoemacs-debug "Old ergoemacs-save-variables: %s" ergoemacs-save-variables)
      (setq ergoemacs-save-variables-actual
            (mapcar
             (lambda(x)
               (let (val val2)
                 (if (condition-case err (or (= 1 (nth 1 x)) (= -1 (nth 1 x))) (error nil))
                     (progn
                       (ergoemacs-debug "Call (%s %s)"
                                        (symbol-name (nth 0 x)) (nth 1 x))
                       (funcall (nth 0 x) (nth 1 x))
		       (setq val (if (= 1 (nth 1 x)) -1 1)))
                   (ergoemacs-debug "Set %s to %s" (symbol-name (nth 0 x)) (nth 1 x))
                   (set (nth 0 x) (nth 1 x))
		   (if (not (nth 2 x))
                       (setq val (not (nth 1 x)))
                     (setq val (nth 2 x))
                     (setq val2 (nth 1 x)))
                   (set-default (nth 0 x) (nth 1 x)))
                 `(,(nth 0 x) ,val ,val2)))
             ergoemacs-save-variables-actual))
      (ergoemacs-debug "Old ergoemacs-save-variables: %s" ergoemacs-save-variables-actual)
      (setq ergoemacs-save-variables-actual nil)
      (setq ergoemacs-save-variables-state nil))    
    (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
    (when (featurep 'ergoemacs-menus)
      (ergoemacs-menus-off))
    (when (and (eq system-type 'darwin))
      (let ((cm (or (intern-soft "ns-command-modifier")
                    (intern-soft "mac-command-modifier")))
            (am (or (intern-soft "ns-alternate-modifier")
                    (intern-soft "mac-alternate-modifier"))))
        (when cm
          (set cm ergoemacs-old-ns-command-modifier))
        (when am
          (set am ergoemacs-old-ns-alternate-modifier))))
    ;; Change retangle modifier back.    
    (setq cua--rectangle-modifier-key ergoemacs-cua-rect-modifier-orig)
    (setq cua--rectangle-modifier-key ergoemacs-cua-rect-modifier)
    (setq cua--rectangle-keymap (make-sparse-keymap))
    (setq cua--rectangle-initialized nil)
    (cua--init-rectangles)
    (setq cua--keymap-alist
          `((cua--ena-prefix-override-keymap . ,cua--prefix-override-keymap)
            (cua--ena-prefix-repeat-keymap . ,cua--prefix-repeat-keymap)
            (cua--ena-cua-keys-keymap . ,cua--cua-keys-keymap)
            (cua--ena-global-mark-keymap . ,cua--global-mark-keymap)
            (cua--rectangle . ,cua--rectangle-keymap)
            (cua--ena-region-keymap . ,cua--region-keymap)
            (cua-mode . ,cua-global-keymap)))
    (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
      ;; Remove keymap
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      (setq x (assq 'ergoemacs-unbind-keys minor-mode-map-alist))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist))))
    (setq ergoemacs-shortcut-keys nil)
    (setq ergoemacs-unbind-keys nil)
    (setq ergoemacs-emulation-mode-map-alist nil)
    (ergoemacs-hook-modes) ;; Remove hooks and advices.
    (remove-hook 'minibuffer-setup-hook #'ergoemacs-minibuffer-setup)
    (remove-hook 'pre-command-hook 'ergoemacs-pre-command-hook)
    (ergoemacs-debug-heading "Ergoemacs-mode turned OFF."))
  ;; Always have `ergoemacs-post-command-hook' on so that it will
  ;; uninstall ergoemacs keymaps that were installed to overlays and
  ;; text-properties and anything above `emulation-mode-map-alists'.
  (add-hook 'post-command-hook 'ergoemacs-post-command-hook) 
  (ergoemacs-debug "post-command-hook: %s" post-command-hook)
  (ergoemacs-debug "pre-command-hook: %s" pre-command-hook)
  (ergoemacs-debug "ergoemacs-shortcut-keys: %s" ergoemacs-shortcut-keys)
  (ergoemacs-debug "ergoemacs-unbind-keys: %s" ergoemacs-unbind-keys)
  (ergoemacs-debug "ergoemacs-mode %s" ergoemacs-mode)
  (ergoemacs-debug "ergoemacs-save-variables-state %s" ergoemacs-save-variables-state)
  (ergoemacs-debug "emulation-mode-map-alists: %s" emulation-mode-map-alists)
  (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    ergoemacs-emulation-mode-map-alist))
  (ergoemacs-debug "minor-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    minor-mode-map-alist))
  (ergoemacs-debug-heading "ErgoEmacs Hooks")
  (ergoemacs-debug "%s" (mapconcat (lambda(x) (format "%s: %s" (nth 0 x) (symbol-value (nth 0 x)))) ergoemacs-hook-list "\n"))
  (unless ergoemacs-mode
    (setq ergoemacs-hook-list nil))
  (ergoemacs-debug-flush))


;; ErgoEmacs replacements for local-set-key

(defvar ergoemacs-local-keymap nil
  "Local ergoemacs keymap")

(defun ergoemacs-local-set-key (key command)
  "Set a key in the ergoemacs local map."
  ;; install keymap if not already installed
  (interactive)
  (let (major ergoemacs-local-keymap)
    (eval (macroexpand `(setq major ',(intern (format "ergoemacs--emulation-for-%s-local" major-mode)))))
    (set (make-local-variable major) t)
    (progn
      (unless ergoemacs-local-keymap
        (set (make-local-variable 'ergoemacs-local-keymap) (make-sparse-keymap)))
      ;; add key
      (define-key ergoemacs-local-keymap key command)
      (let ((x (assq major ergoemacs-emulation-mode-map-alist)))
        ;; Delete keymap.
        (if x
            (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
        ;; Put at the top of the list
        (setq ergoemacs-emulation-mode-map-alist
              (append ergoemacs-emulation-mode-map-alist
                      (list (cons major ergoemacs-local-keymap))))
        (ergoemacs-shuffle-keys)))))

(defun ergoemacs-local-unset-key (key)
  "Unset a key in the ergoemacs local map."
  (ergoemacs-local-set-key key nil))



(load "ergoemacs-advices")

(defcustom ergoemacs-ignore-prev-global t
  "If non-nil, the ergoemacs-mode will ignore previously defined global keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

(if ergoemacs-ignore-prev-global
    (ergoemacs-ignore-prev-global)
  (ergoemacs-reset-global-where-is))



;;; Frequently used commands as aliases

(defcustom ergoemacs-use-aliases t
  "Use aliases defined by `ergoemacs-aliases' to abbreviate commonly used commands.
Depending on how you use the completion engines, this may or may not be useful.
However instead of using M-a `eval-buffer', you could use M-a `eb'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-aliases
  '((ar    align-regexp)
    (c     toggle-case-fold-search)
    (cc    calc)
    (dml   delete-matching-lines)
    (dnml  delete-non-matching-lines)
    (dtw   delete-trailing-whitespace)
    (eb    eval-buffer)
    (ed    eval-defun)
    (eis   elisp-index-search)
    (er    eval-region)
    (fb    flyspell-buffer)
    (fd    find-dired)
    (g     grep)
    (gf    grep-find)
    (lcd   list-colors-display)
    (lf    load-file)
    (lml   list-matching-lines)
    (ps    powershell)
    (qrr   query-replace-regexp)
    (rb    revert-buffer)
    (rof   recentf-open-files)
    (rr    reverse-region)
    (rs    replace-string)
    (sbc   set-background-color)
    (sh    shell)
    (sl    sort-lines)
    (ws    whitespace-mode))
  "List of aliases defined by `ergoemacs-mode'."
  :type '(repeat
          (list
           (sexp :tag "alias")
           (symbol :tag "actual function")))
  :group 'ergoemacs-mode)

(defun ergoemacs-load-aliases ()
  "Loads aliases defined in `ergoemacs-aliases'."
  (mapc
   (lambda(x)
     (eval (macroexpand `(defalias ',(nth 0 x) ',(nth 1 x)))))
   ergoemacs-aliases))

(when ergoemacs-use-aliases
  (ergoemacs-load-aliases))

(defun ergoemacs-vars-sync ()
  "Sync variables. `ergoemacs-mode' `ergoemacs-shortcut-keys', `ergoemacs-unbind-keys'."
  (if (assq 'ergoemacs-mode minor-mode-map-alist)
      (when (or ergoemacs-mode ergoemacs-shortcut-keys ergoemacs-unbind-keys
                ergoemacs-save-variables)
        (unless ergoemacs-mode
          (setq ergoemacs-mode t)
          (ergoemacs-debug "WARNING: ergoemacs-mode was turned off; Turning on."))
        (unless ergoemacs-shortcut-keys
          (if ergoemacs-shortcut-override-mode
              (ergoemacs-debug "WARNING: ergoemacs-shortcut-keys was turned off, but ergoemacs-shortcut-override-mode is on, keeping off.")
            (setq ergoemacs-shortcut-keys t)
            (ergoemacs-debug "WARNING: ergoemacs-shortcut-keys was turned off; Turning on.")))
        (unless ergoemacs-unbind-keys
          (setq ergoemacs-unbind-keys t)
          (ergoemacs-debug "WARNING: ergoemacs-unbind-keys was turned off; Turning on.")))
    (when ergoemacs-mode      
      (setq ergoemacs-mode nil)
      (ergoemacs-debug "WARNING: ergoemacs-mode was turned on; Turning off."))
    (unless ergoemacs-unbind-keys
      (setq ergoemacs-unbind-keys nil)
      (ergoemacs-debug "WARNING: ergoemacs-unbind-keys was turned on; Turning off."))
    (unless ergoemacs-shortcut-keys
      (setq ergoemacs-shortcut-keys nil)
      (ergoemacs-debug "WARNING: ergoemacs-shortcut-keys was turned on; Turning off."))
    (unless ergoemacs-shortcut-override-mode
      (setq ergoemacs-shortcut-override-mode nil)
      (ergoemacs-debug "WARNING: ergoemacs-shortcut-override-mode was turned on; Turning off."))))

(defun ergoemacs-shuffle-keys ()
  "Shuffle ergoemacs keymaps"
  ;; Promotes keymaps in `ergoemacs-emulation-mode-map-alist'
  (mapc
   (lambda(what)
     (let ((x (assq what ergoemacs-emulation-mode-map-alist)))
       (and x (setq ergoemacs-emulation-mode-map-alist
                    (cons x (delq x ergoemacs-emulation-mode-map-alist ))))))
   ;; Promoted from least to most important
   '(ergoemacs-shortcut-keys ergoemacs-shortcut-override-mode ergoemacs-modal))
  ;; Demote
  (let ((x (assq 'ergoemacs-unbind-keys minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete x minor-mode-map-alist) (list x)))))

(defvar ergoemacs-this-command nil)
(defun ergoemacs-pre-command-hook ()
  "Ergoemacs pre-command-hook."
  (let (deactivate-mark)
    (condition-case err
        (progn
          (ergoemacs-vars-sync)
          (setq ergoemacs-this-command this-command)
          (when ergoemacs-mode
            ;; Raise shortcuts and modal modes.
            (ergoemacs-shuffle-keys)
            (let ((key-binding
                   (read-kbd-macro
                    (format
                     "<override> %s" (key-description (this-single-command-keys))))))
              (cond
               ((condition-case err
                    (interactive-form key-binding)
                  (error nil))
                (setq this-command key-binding))))
            (unless ergoemacs-modal
              (ergoemacs-install-shortcuts-up))
            (when (and (not ergoemacs-show-true-bindings)
                       (memq this-command ergoemacs-describe-keybindings-functions))
              (ergoemacs-shortcut-override-mode 1))))
      (error nil)))
  t)

(defun ergoemacs-post-command-hook ()
  "Ergoemacs post-command-hook"
  (let (deactivate-mark)
    (condition-case err
        (progn
          (when ergoemacs-mode
            (ergoemacs-shuffle-keys)
            ;; Reset mode-line
            (when ergoemacs-check-mode-line-change
              (unless (where-is-internal this-command  ergoemacs-check-mode-line-change)
                ;; (message "Restored normal ergoemacs keys.")
                (ergoemacs-mode-line)
                (setq ergoemacs-check-mode-line-change nil)))
            (when (and (not ergoemacs-show-true-bindings)
                       (memq this-command ergoemacs-describe-keybindings-functions))
              (ergoemacs-shortcut-override-mode -1))
            (when (and ergoemacs-modal
                       (memq major-mode ergoemacs-modal-emacs-state-modes))
              (set (make-local-variable 'ergoemacs-modal)
                   nil))
            (when ergoemacs-modal
              (mapc
               (lambda(reg)
                 (when (and ergoemacs-modal
                            (string-match reg (buffer-name)))
                   (set (make-local-variable 'ergoemacs-modal)
                        nil)))
               ergoemacs-modal-ignored-buffers))
            (if ergoemacs-modal
                (progn
                  (unless ergoemacs-default-cursor
                    (setq ergoemacs-default-cursor
                          (or (frame-parameter nil 'cursor-color) "black"))
                    (set-cursor-color ergoemacs-modal-cursor)
                    (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
                     (concat " " ergoemacs-modal))))
              (when ergoemacs-default-cursor
                (set-cursor-color ergoemacs-default-cursor)
                (setq ergoemacs-default-cursor nil))
              (ergoemacs-mode-line))
            (unless ergoemacs-modal
              (ergoemacs-install-shortcuts-up)
              (ergoemacs-vars-sync)))
          (when (not ergoemacs-mode)
            (ergoemacs-remove-shortcuts)))
      (error (message "Error %s" err))))
  t)

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
