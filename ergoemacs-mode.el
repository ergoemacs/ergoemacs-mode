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

(defun ergoemacs-debug (&rest arg)
  "Ergoemacs debugging facility."
  (interactive)
  (if (interactive-p)
      (progn
        (ergoemacs-debug-flush)
        (switch-to-buffer-other-window (get-buffer-create " *ergoemacs-debug*")))
    (setq ergoemacs-debug
          (format "%s\n%s"
                  ergoemacs-debug
                  (apply 'format arg)))))

(defun ergoemacs-debug-flush ()
  "Flushes ergoemacs debug to *ergoemacs-debug*"
  (save-excursion
    (with-current-buffer (get-buffer-create " *ergoemacs-debug*") ;; Should be hidden.
      (insert ergoemacs-debug "\n")))
  (setq ergoemacs-debug ""))

;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)


(require 'ergoemacs-layouts)

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

(defvar ergoemacs-window-tab-switching
  '(ergoemacs-switch-to-previous-frame
    ergoemacs-switch-to-next-frame
    ergoemacs-previous-user-buffer
    split-window-horizontally
    delete-window
    delete-other-windows
    split-window-vertically
    ergoemacs-next-user-buffer)
  "Window/Tab switching functions.")

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

;; FIXME: maybe a better name is `ergoemacs-change-smex-meta-x', since
;; Customization displays it as "Ergoemacs Change Smex M X".
(defcustom ergoemacs-change-smex-M-x t
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
       (message "%s" ergoemacs-undo-apps-text)
         (set-temporary-overlay-map ergoemacs-undo-apps-keymap t))))

(mapc
 (lambda(x)
   (eval `(ergoemacs-create-undo-advices ,x)))
 ergoemacs-undo-redo-functions)


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
               (call-interactively ',command t)
               (setq this-command ',command)))
        `(defun ,(intern (concat "ergoemacs-isearch-" (symbol-name command))) (&optional arg)
           ,(format "Ergoemacs isearch movement command for `%s'." (symbol-name command))
           (interactive "^P")
           (isearch-exit)
           (call-interactively ',command t)
           (setq this-command ',command)))
     (defvar ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")) (make-sparse-keymap)
       ,(format "Ergoemacs fast keymap for `%s'." (symbol-name command)))
     ;; Change to advices
     (defadvice ,(intern (symbol-name command)) (around ergoemacs-movement-advice activate)
       ,(format "Ergoemacs advice for command for `%s'.
May install a fast repeat key based on `ergoemacs-repeat-movement-commands',  `ergoemacs-full-fast-keys-keymap' and `ergoemacs-fast-%s-keymap'.
" (symbol-name command) (symbol-name command))
       ad-do-it
       (when (and ergoemacs-mode ergoemacs-repeat-movement-commands
                  (called-interactively-p 'interactive) (not cua--rectangle-overlays)) ;; Don't add overlays to rectangles
         (set-temporary-overlay-map (cond
                                     ((eq ergoemacs-repeat-movement-commands 'single)
                                      ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")))
                                     ((eq ergoemacs-repeat-movement-commands 'all)
                                      ergoemacs-full-fast-keys-keymap)
                                     (t ,(intern (concat "ergoemacs-fast-" (symbol-name command) "-keymap")))) t)))))
(mapc
 (lambda(x)
   (eval `(ergoemacs-create-movement-commands ,x)))
 ergoemacs-movement-functions)

(defvar ergoemacs-M-O-keymap (make-keymap)
  "M-O translation map.")
(define-key ergoemacs-M-O-keymap [exit] 'ignore)

(defvar ergoemacs-M-o-keymap (make-keymap)
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

(defvar ergoemacs-M-O-prefix-keys nil)


(defun ergoemacs-M-O-timeout ()
  "Push timeout on unread command events."
  (when ergoemacs-push-M-O-timeout
    (setq ergoemacs-push-M-O-timeout nil)
    (if ergoemacs-M-O-prefix-keys
        (let (fn)
          (let (ergoemacs-shortcut-mode)
            (setq fn (key-binding
                      (read-kbd-macro
                       (format "%s <timeout>"
                               ergoemacs-M-O-prefix-keys)))))
          ;; Lookup keys, and then send <exit> event.
          (setq prefix-arg ergoemacs-curr-prefix-arg)
          (setq this-command fn)
          (call-interactively fn t)
          (reset-this-command-lengths)
          (setq unread-command-events (cons 'exit unread-command-events)))
      (setq prefix-arg ergoemacs-curr-prefix-arg)
      (reset-this-command-lengths)
      (setq unread-command-events (cons 'timeout unread-command-events)))
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
          (call-interactively fn t))
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

(require 'ergoemacs-themes)
(require 'ergoemacs-unbind)


(defvar ergoemacs-needs-translation nil
  "Tells if ergoemacs keybindings need a translation")

(defvar ergoemacs-translation-from nil
  "Translation from keyboard layout")

(defvar ergoemacs-translation-to nil
  "Translation to keyboard layout")

(defvar ergoemacs-translation-assoc nil
  "Translation alist")

(defvar ergoemacs-translation-regexp nil
  "Translation regular expression")

;;; ergoemacs-keymap


(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

(defvar ergoemacs-shortcut-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode shortcut keymap")

(defvar ergoemacs-shortcut-override-keymap (make-sparse-keymap)
  "Keymap for overriding keymap.")

(defvar ergoemacs-full-fast-keys-keymap (make-sparse-keymap)
  "Ergoemacs full fast keys keymap")

(defvar ergoemacs-full-alt-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+ keymap.  Alt is removed from all these keys so that no key chord is necessary.")

(defvar ergoemacs-full-alt-shift-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+Shift+ keymap.
Alt+shift is removed from all these keys so that no key chord is
necessary.  Unshifted keys are changed to shifted keys.")

(defun ergoemacs-exit-dummy ()
  "Dummy function for exiting keymaps."
  (interactive))

(defun ergoemacs-setup-fast-keys ()
  "Setup an array listing the fast keys."
  (interactive)
  (ergoemacs-create-undo-apps-keymap)
  (setq ergoemacs-full-fast-keys-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-shift-keymap (make-sparse-keymap))
  (define-key ergoemacs-full-alt-keymap (read-kbd-macro
                                         (if (eq system-type 'windows-nt)
                                             "<apps>"
                                           "<menu>"))
    'ergoemacs-exit-dummy)
  (define-key ergoemacs-full-alt-shift-keymap (read-kbd-macro
                                               (if (eq system-type 'windows-nt)
                                                   "<apps>"
                                                 "<menu>"))
    'ergoemacs-exit-dummy)
  (ergoemacs-debug (make-string 80 ?=))
  (ergoemacs-debug "Setup Fast Keys")
  (ergoemacs-debug (make-string 80 ?=))
  (mapc
   (lambda(var)
     (let* ((key (ergoemacs-kbd (nth 0 var) t))
            (cmd (nth 1 var))
            (stripped-key (replace-regexp-in-string
                           (format "\\<[%s]-"
                                   (if ergoemacs-swap-alt-and-control
                                       "C"
                                     "M"))
                           "" key))
            (new-cmd (nth 1 var)))
       (ergoemacs-debug "Key:%s stripped-key: %s" key stripped-key)
       (when (string-match "^[A-Za-z]$" stripped-key)
         ;;(message "Stripped key: %s" stripped-key)
         (if (string= (downcase stripped-key) stripped-key)
             (progn
               (define-key ergoemacs-full-alt-keymap (edmacro-parse-keys stripped-key) new-cmd)
               (define-key ergoemacs-full-alt-shift-keymap (edmacro-parse-keys (upcase stripped-key)) new-cmd))
           (define-key ergoemacs-full-alt-shift-keymap (edmacro-parse-keys (downcase stripped-key)) new-cmd)
           (define-key ergoemacs-full-alt-keymap (edmacro-parse-keys stripped-key) new-cmd)))
       (when (member cmd ergoemacs-movement-functions)
         (set (intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
              (make-sparse-keymap))
         (eval `(define-key ,(intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
                  ,(edmacro-parse-keys stripped-key) new-cmd))
         (define-key ergoemacs-full-fast-keys-keymap
           (edmacro-parse-keys stripped-key)
           new-cmd))))
   (symbol-value (ergoemacs-get-variable-layout))))

(defvar ergoemacs-exit-temp-map-var nil)

(defun ergoemacs-minibuffer-exit-maps ()
  "Exit temporary overlay maps."
  (setq ergoemacs-exit-temp-map-var t))

(add-hook 'minibuffer-setup-hook #'ergoemacs-minibuffer-exit-maps)

(defun ergoemacs-exit-alt-keys ()
  "Exit alt keys predicate."
  (let (ret cmd)
    (condition-case err
        (progn
          (setq cmd (lookup-key ergoemacs-full-alt-keymap
                                (this-command-keys-vector)))
          (when cmd
            (setq ret t))
          (when (eq cmd 'ergoemacs-exit-dummy)
            (setq ret nil))
          (when ergoemacs-exit-temp-map-var
            (setq ret nil)
            (setq ergoemacs-exit-temp-map-var nil)))
      (error (message "Err %s" err)))
    (symbol-value 'ret)))

(defun ergoemacs-alt-keys ()
  "Install the alt keymap temporarily"
  (interactive)
  (setq ergoemacs-exit-temp-map-var nil)
  (set-temporary-overlay-map  ergoemacs-full-alt-keymap
                              'ergoemacs-exit-alt-keys)
  (message "[Alt+] keys installed to keymap. Press [Menu], [Esc], to exit"))

(defun ergoemacs-exit-alt-shift-keys ()
  "Exit alt-shift keys predicate"
  (let (ret cmd)
    (condition-case err
        (progn
          (setq cmd (lookup-key ergoemacs-full-alt-shift-keymap
                                (this-command-keys-vector)))
          (when cmd
            (setq ret t))
          (when (eq cmd 'ergoemacs-exit-dummy)
            (setq ret nil))
          (when ergoemacs-exit-temp-map-var
            (setq ret nil)
            (setq ergoemacs-exit-temp-map-var nil)))
      (error (message "Err %s" err)))
    (symbol-value 'ret)))

(defun ergoemacs-alt-shift-keys ()
  "Install the alt-shift keymap temporarily"
  (interactive)
  (setq ergoemacs-exit-temp-map-var nil)
  (set-temporary-overlay-map ergoemacs-full-alt-shift-keymap
                             'ergoemacs-exit-alt-shift-keys)
  (message "[Alt+Shift+] keys installed to keymap. Press [Menu], [Esc], to exit"))

(require 'ergoemacs-functions)

(defun ergoemacs-setup-translation (layout &optional base-layout)
  "Setup translation from BASE-LAYOUT to LAYOUT."
  (let ((base (or base-layout "us"))
        lay
        len i)
    (unless (and (string= layout ergoemacs-translation-to)
                 (string= base ergoemacs-translation-from))
      (if (equal layout base)
          (progn
            (setq ergoemacs-translation-from base)
            (setq ergoemacs-translation-to layout)
            (setq ergoemacs-needs-translation nil)
            (setq ergoemacs-translation-assoc nil)
            (setq ergoemacs-translation-regexp nil))
        (setq ergoemacs-translation-from base)
        (setq ergoemacs-translation-to layout)
        (setq lay (symbol-value (intern (concat "ergoemacs-layout-" layout))))
        (setq base (symbol-value (intern (concat "ergoemacs-layout-" base))))
        (setq ergoemacs-needs-translation t)
        (setq ergoemacs-translation-assoc nil)
        (setq len (length base))
        (setq i 0)
        (while (< i len)
          (unless (or (string= "" (nth i base))
                      (string= "" (nth i lay)))
            (add-to-list 'ergoemacs-translation-assoc
                         `(,(nth i base) . ,(nth i lay))))
          (setq i (+ i 1)))
        (setq ergoemacs-translation-regexp
              (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                      (regexp-opt (mapcar (lambda(x) (nth 0 x))
                                          ergoemacs-translation-assoc) nil)))))))

(defvar ergoemacs-kbd-hash (make-hash-table :test 'equal))
;; This is called so frequently make a hash-table of the results.

(defun ergoemacs-kbd (key &optional just-translate only-first)
  "Translates kbd code KEY for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'.
If JUST-TRANSLATE is non-nil, just return the KBD code, not the actual emacs key sequence.
"
  (save-match-data
    (if (not key)
        nil
      (let ((new-key (gethash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to)
                              ergoemacs-kbd-hash)))
        (if new-key
            (symbol-value 'new-key)
          (setq new-key key)
          (cond
           ((eq system-type 'windows-nt)
            (setq new-key (replace-regexp-in-string "<menu>" "<apps>" new-key)))
           (t
            (setq new-key (replace-regexp-in-string "<apps>" "<menu>" new-key))))
          ;; Translate Alt+ Ctl+ or Ctrl+ to M- and C-
          (setq new-key (replace-regexp-in-string "[Aa][Ll][Tt][+]" "M-" new-key))
          (setq new-key (replace-regexp-in-string "[Cc][Tt][Rr]?[Ll][+]" "C-" new-key))
          (when ergoemacs-needs-translation
            (setq new-key
                  (with-temp-buffer
                    (insert new-key)
                    (goto-char (point-min))
                    (when (re-search-forward ergoemacs-translation-regexp nil t)
                      (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                      (skip-chars-backward " "))
                    (when (not only-first)
                      (while (re-search-forward ergoemacs-translation-regexp nil t)
                        (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                        (skip-chars-backward " ")))
                    (buffer-string))))
          (if (not just-translate)
              (condition-case err
                  (read-kbd-macro new-key)
                (error
                 (read-kbd-macro (encode-coding-string new-key locale-coding-system))))
            (puthash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to) new-key
                     ergoemacs-kbd-hash)
            new-key))))))

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

(defcustom ergoemacs-swap-alt-and-control nil
  "Swaps Alt and Ctrl keys"
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-change-fixed-layout-to-variable-layout nil
  "Change the fixed layout to variable layout keys.
For example, on dvorak, change C-j to C-c (copy/command)."
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defun ergoemacs-get-kbd-translation (pre-kbd-code &optional dont-swap)
  "This allows a translation from the listed kbd-code and the true kbd code."
  (let ((ret (replace-regexp-in-string
              "[Cc]\\(?:on\\)?tro?l[+-]" "C-"
              (replace-regexp-in-string
               "[Aa]lt[+-]" "M-" pre-kbd-code))))
    (when (and ergoemacs-swap-alt-and-control (not dont-swap))
      (setq ret
            (replace-regexp-in-string
             "\\^-" "M-"
             (replace-regexp-in-string
              "M-" "C-"
              (replace-regexp-in-string
               "C-" "^-" ret)))))
    (symbol-value 'ret)))

(defvar ergoemacs-prefer-shortcuts nil ;; Prefer shortcuts.
  "Prefer shortcuts")

(defvar ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal)
  "List of command shortcuts.")

(defun ergoemacs-setup-keys-for-keymap---internal (keymap key def)
  "Defines KEY in KEYMAP to be DEF"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (if (condition-case err
              (stringp (nth 0 def))
            (error nil))
          (if (and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
              (progn
                (puthash (read-kbd-macro (key-description key) t)
                     `(,(nth 0 def) ,(nth 1 def))
                     ergoemacs-command-shortcuts-hash)
                (define-key ergoemacs-shortcut-keymap key
                  'ergoemacs-shortcut))
            (define-key keymap key
              `(lambda(&optional arg)
                 (interactive "P")
                 (setq this-command last-command) ; Don't record this command.
                 (setq prefix-arg current-prefix-arg)
                 (ergoemacs-shortcut-internal ,(nth 0 def) ',(nth 1 def)))))
        (mapc
         (lambda(new-def)
           (unless found
             (setq found
                   (ergoemacs-setup-keys-for-keymap---internal keymap key new-def))))
         def))
      (symbol-value 'found)))
   ((condition-case err
        (interactive-form def)
      (error nil))
    (cond
     ((memq def '(ergoemacs-ctl-c ergoemacs-ctl-x))
      (define-key ergoemacs-shortcut-keymap key def))
     ((and ergoemacs-prefer-shortcuts
           (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap
           (or (remove-if '(lambda(x) (eq 'menu-bar (elt x 0))) ; Ignore
                                                           ; menu-bar
                                                           ; functions
                          (where-is-internal def (current-global-map)))
               (gethash def ergoemacs-where-is-global-hash)))
      
      (puthash (read-kbd-macro (key-description key) t)
               (list def 'global) ergoemacs-command-shortcuts-hash)
      (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut))
     (t
      (define-key keymap key def)))
    t)
   ((condition-case err
        (keymapp (symbol-value def))
      (error nil))
    (define-key keymap key (symbol-value def))
    t)
   ((condition-case err
	(stringp def)
      (error nil))
    (if (and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
        (progn
          (puthash (read-kbd-macro (key-description key) t)
                   `(,def nil)
                   ergoemacs-command-shortcuts-hash)
          (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut))
      (define-key keymap key
        `(lambda(&optional arg)
           (interactive "P")
           (setq this-command last-command) ; Don't record this command.
           (setq prefix-arg current-prefix-arg)
           (ergoemacs-shortcut-internal ,def))))
    
    t)
   (t nil)))

(defmacro ergoemacs-setup-keys-for-keymap (keymap)
  "Setups ergoemacs keys for a specific keymap"
  `(condition-case err
       (let ((no-ergoemacs-advice t)
             (case-fold-search t)
             key
             trans-key
             cmd cmd-tmp)
         (setq ,keymap (make-sparse-keymap))
         (if (eq ',keymap 'ergoemacs-keymap)
             (ergoemacs-debug "Theme: %s" ergoemacs-theme))
         ;; Fixed layout keys
         (mapc
          (lambda(x)
            (when (and (eq 'string (type-of (nth 0 x))))
              (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))
              
              (if ergoemacs-change-fixed-layout-to-variable-layout
                  (progn ;; Change to the fixed keyboard layout.
                    (setq key (ergoemacs-kbd trans-key)))
                (condition-case err
                    (setq key (read-kbd-macro
                               trans-key))
                  (error
                   (setq key (read-kbd-macro
                              (encode-coding-string
                               trans-key
                               locale-coding-system))))))
              (if (eq ',keymap 'ergoemacs-keymap)
                  (ergoemacs-debug "Fixed: %s -> %s %s (%s)" trans-key cmd key (key-description key)))
              (if (ergoemacs-global-changed-p trans-key)
                  (progn
                    (ergoemacs-debug "!!!Fixed %s has changed globally." trans-key)
                    (ergoemacs-setup-keys-for-keymap---internal ,keymap key (lookup-key (current-global-map) key)))
                (setq cmd (nth 1 x))
                (when (not (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd))
                  (ergoemacs-debug "Key %s->%s not setup." key cmd)))))
          (symbol-value (ergoemacs-get-fixed-layout)))
         
         ;; Variable Layout Keys
         (mapc
          (lambda(x)
            (when (and (eq 'string (type-of (nth 0 x))))
              (setq trans-key
                    (ergoemacs-get-kbd-translation (nth 0 x)))
              (setq key (ergoemacs-kbd trans-key nil (nth 3 x)))
              (if (ergoemacs-global-changed-p trans-key t)
                  (progn
                    (ergoemacs-debug "!!!Variable %s (%s) has changed globally."
                                     trans-key (ergoemacs-kbd trans-key t (nth 3 x))))
                ;; Add M-O and M-o handling for globally defined M-O and
                ;; M-o.
                ;; Only works if ergoemacs-mode is on...
                (setq cmd (nth 1 x))
                
                (if (and ergoemacs-fix-M-O (string= (ergoemacs-kbd trans-key t t) "M-O"))
                    (progn
                      (define-key ,keymap key  'ergoemacs-M-O)
                      (ergoemacs-setup-keys-for-keymap---internal ergoemacs-M-O-keymap [timeout] cmd)
                      (if (eq ',keymap 'ergoemacs-keymap)
                          (ergoemacs-debug "Variable: %s (%s) -> %s %s via ergoemacs-M-O" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
                  (if (and ergoemacs-fix-M-O
                           (string= (ergoemacs-kbd trans-key t t) "M-o"))
                      (progn
                        (define-key ,keymap key  'ergoemacs-M-o)
                        (ergoemacs-setup-keys-for-keymap---internal ergoemacs-M-o-keymap [timeout] cmd)
                        (if (eq ',keymap 'ergoemacs-keymap)
                            (ergoemacs-debug "Variable: %s (%s) -> %s %s via ergoemacs-M-o" trans-key
                                             (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
                    (when cmd
                      (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd)
                      (if (eq ',keymap 'ergoemacs-keymap)
                          (ergoemacs-debug "Variable: %s (%s) -> %s %s" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key))))))))
          (symbol-value (ergoemacs-get-variable-layout)))
         (when ergoemacs-fix-M-O
           (let ((M-O (lookup-key ,keymap (read-kbd-macro "M-O")))
                 (g-M-O (lookup-key global-map (read-kbd-macro "M-O")))
                 (M-o (lookup-key ,keymap (read-kbd-macro "M-o")))
                 (g-M-o (lookup-key global-map (read-kbd-macro "M-o"))))
             (ergoemacs-debug "M-O %s; Global M-O: %s; M-o %s; Global M-o: %s" M-O g-M-O M-o g-M-o)
             (when (and (not (functionp M-O))
                        (functionp g-M-O))
               (ergoemacs-debug "Fixed M-O")
               (define-key ,keymap (read-kbd-macro "M-O") 'ergoemacs-M-O)
               (define-key ergoemacs-M-O-keymap [timeout] g-M-O))
             (when (and (not (functionp M-o))
                        (functionp g-M-o))
               (ergoemacs-debug "Fixed M-o")
               (define-key ,keymap (read-kbd-macro "M-o") 'ergoemacs-M-o)
               (define-key ergoemacs-M-o-keymap [timeout] g-M-o)))))
     (error
      (ergoemacs-debug "Error: %s" err)
      (ergoemacs-debug-flush))))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  (ergoemacs-setup-fast-keys)
  ;; Reset shortcuts layer.
  (setq ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal))
  (let ((setup-ergoemacs-keymap t))
    (ergoemacs-setup-keys-for-keymap ergoemacs-keymap))
  
  ;; Now change `minor-mode-map-alist'.
  (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
    ;; Install keymap
    (if x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
    (add-to-list 'minor-mode-map-alist
                 `(ergoemacs-mode  ,(symbol-value 'ergoemacs-keymap))))
  (easy-menu-define ergoemacs-menu ergoemacs-keymap
    "ErgoEmacs menu"
    `("ErgoEmacs"
      ,(ergoemacs-get-layouts-menu)
      ,(ergoemacs-get-themes-menu)
      "--"
       ("Ctrl+C and Ctrl+X behavior"
        ["Ctrl+C and Ctrl+X are for Emacs Commands"
         (lambda()
           (interactive)
           (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
         :style radio
         :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)]
        ["Ctrl+C and Ctrl+X are only Copy/Cut"
         (lambda()
           (interactive)
           (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut))
         :style radio
         :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)]
        ["Ctrl+C and Ctrl+X are both Emacs Commands & Copy/Cut"
         (lambda()
           (interactive)
           (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'both))
         :style radio
         :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)]
        ["Customize Ctrl+C and Ctrl+X Cut/Copy Timeout"
         (lambda() (interactive)
           (customize-variable 'ergoemacs-ctl-c-or-ctl-x-delay))])
      "--"
      ["Make Bash aware of ergoemacs keys"
       (lambda () (interactive)
         (call-interactively 'ergoemacs-bash)) t]
      "--"
      ["Use Menus"
       (lambda() (interactive)
         (setq ergoemacs-use-menus (not ergoemacs-use-menus))
         (if ergoemacs-use-menus
             (progn
               (require 'ergoemacs-menus)
               (ergoemacs-menus-on))
           (when (featurep 'ergoemacs-menus)
             (ergoemacs-menus-off))))
       :style toggle :selected (symbol-value 'ergoemacs-use-menus)]
      "--"
      ;; ["Generate Documentation"
      ;;  (lambda()
      ;;    (interactive)
      ;;    (call-interactively 'ergoemacs-extras)) t]
      ["Customize Ergoemacs"
       (lambda ()
         (interactive)
         (customize-group 'ergoemacs-mode)) t]
      ["Save Settings for Future Sessions"
       (lambda ()
         (interactive)
         (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
         (customize-save-variable 'ergoemacs-theme ergoemacs-theme)
         (customize-save-variable 'ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
         (customize-save-variable 'ergoemacs-ctl-c-or-ctl-x-delay ergoemacs-ctl-c-or-ctl-x-delay)
         (customize-save-variable 'ergoemacs-handle-ctl-c-or-ctl-x ergoemacs-handle-ctl-c-or-ctl-x)
         (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
         (customize-save-customized)) t]
      ["Exit ErgoEmacs"
       (lambda ()
         (interactive)
         (ergoemacs-mode -1)) t]))
  
  (let ((existing (assq 'ergoemacs-mode minor-mode-map-alist)))
    (if existing
        (setcdr existing ergoemacs-keymap)
      (push (cons 'ergoemacs-mode ergoemacs-keymap) minor-mode-map-alist)))
  
  ;; Set appropriate mode-line indicator
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
                minor-mode-alist))
  (ergoemacs-setup-backward-compatability))

(require 'lookup-word-on-internet nil "NOERROR")
(require 'ergoemacs-extras)

;; ErgoEmacs hooks
(defun ergoemacs-key-fn-lookup (function &optional use-apps)
  "Looks up the key binding for FUNCTION based on `ergoemacs-get-variable-layout'."
  (let ((ret nil))
    (mapc
     (lambda(x)
       (when (and (equal (nth 1 x) function)
                  (if use-apps
                      (string-match "<apps>" (nth 0 x))
                    (not (string-match "<apps>" (nth 0 x)))))
         (setq ret (ergoemacs-kbd (nth 0 x) nil (nth 3 x)))))
     (symbol-value (ergoemacs-get-variable-layout)))
    (symbol-value 'ret)))

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
                    (setq prefix-arg current-prefix-arg)
                    (ergoemacs-shortcut-internal ,definition))))
      (if (and (eq translate 'remap)
               (functionp key-def)
               (functionp fn))
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
                  (when (ergoemacs-key-fn-lookup key-def t)
                    (define-key keymap (ergoemacs-key-fn-lookup key-def t) fn))
                  (ergoemacs-key-fn-lookup key-def))
                 ;; Define <apps>  key
                 ((ergoemacs-key-fn-lookup key-def t)
                  (ergoemacs-key-fn-lookup key-def t)
                  nil)
                 (t
                  (if (and (functionp key-def)
                           (functionp fn))
                      (eval
                       (macroexpand `[remap ,(intern (symbol-name key-def))]))
                    nil)))))
          (ergoemacs-debug "hook: %s->%s %s %s"
                           key-def key-code
                           fn translate)
          (when key-code
            (define-key keymap key-code fn)))))))

(defmacro ergoemacs-create-hook-function (hook keys &optional global)
  "Creates a hook function based on the HOOK and the list of KEYS defined."
  (let ((is-override (make-symbol "is-override"))
        (minor-mode-p (make-symbol "minor-mode-p"))
        (old-keymap (make-symbol "old-keymap"))
        (override-keymap (make-symbol "override-keymap")))
    (setq is-override (eq 'minor-mode-overriding-map-alist (nth 2 (nth 0 keys))))
    (setq minor-mode-p (eq 'override (nth 2 (nth 0 keys))))
    `(progn
       ,(if (or is-override minor-mode-p)
            (progn
	      (setq old-keymap nil)
	      `(progn
		 (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) nil
		   ,(concat "Ergoemacs overriding keymap for `" (symbol-name hook) "'"))))
	  (setq old-keymap t)
          (setq override-keymap (nth 2 (nth 0 keys)))
          `(defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap")) nil
             ,(concat "Old keymap for `" (symbol-name hook) "'.")))
       
       ,(when minor-mode-p
          `(define-minor-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-mode"))
             ,(concat "Minor mode for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-get-minor-mode-layout'. See `ergoemacs-mode'.")
             nil
             :lighter ""
             :global nil
             :keymap ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))))

       (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
         ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-get-minor-mode-layout'.")
         ;; Only generate keymap if it hasn't previously been generated.
         (unless ,(if (or minor-mode-p is-override) nil
                    (intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap")))
           
           ,(if (or is-override minor-mode-p)
                `(ergoemacs-setup-keys-for-keymap ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
              `(setq ,(intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))
                     (copy-keymap ,(nth 2 (nth 0 keys)))))
           ,@(mapcar
              (lambda(def)
                `(ergoemacs-hook-define-key
                  ,(if (or minor-mode-p
                           (and is-override
                                (equal (nth 2 def)
                                       'minor-mode-overriding-map-alist)))
                       (intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))
                     (nth 2 def))
                  ,(if (eq (type-of (nth 0 def)) 'string)
                       `,(nth 0 def)
                     `(quote ,(nth 0 def)))
                  ',(nth 1 def)
                  ',(nth 3 def)))
              keys)
           ,(when minor-mode-p
              `(progn
                 (let ((x (assq ',(intern (concat "ergoemacs-" (symbol-name hook) "-mode"))
                                minor-mode-map-alist)))
                   ;; Delete keymap.
                   (if x
                       (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
                   (add-to-list 'minor-mode-map-alist
                                (cons ',(intern (concat "ergoemacs-" (symbol-name hook) "-mode"))
                                      ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))))
                   (funcall ',(intern (concat "ergoemacs-" (symbol-name hook) "-mode")) 1))))
           ,(if is-override
                `(progn
                   (add-to-list 'minor-mode-overriding-map-alist
                                (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                                nil ,(if (equal hook 'minibuffer-setup-hook)
                                         '(lambda (x y)
                                            (equal (car y) (car x)))
                                       nil))
                   (push (cons 'ergoemacs-shortcut-mode ergoemacs-shortcut-keymap) minor-mode-overriding-map-alist))
              nil)
           (ergoemacs-debug-flush)
           t))
       (ergoemacs-add-hook ',hook ',(intern (concat "ergoemacs-" (symbol-name hook))) ',(if old-keymap (intern (concat "ergoemacs-" (symbol-name hook) "-old-keymap"))) ',override-keymap))))

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
depending the state of `ergoemacs-mode' variable.  If the mode
is being initialized, some global keybindings in current-global-map
will change."
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
     (let ((f (macroexpand `(ergoemacs-create-hook-function ,(car x) ,(car (cdr x))))))
       (eval f)))
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

(defcustom ergoemacs-translate-keys t
  "When translating extracted keymaps, attempt to translate to
the best match."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-extract-map-hash (make-hash-table :test 'equal))



(defmacro ergoemacs-extract-maps (keymap &optional prefix)
  "Extracts maps."
  `(save-excursion
     (let ((deactivate-mark nil)
           (buf (current-buffer))
           (normal '())
           (translations '())
           (prefixes '())
           (bound-regexp "")
           (tmp "")
           (fn nil)
           (new-key nil)
           (start-time (float-time))
           (last-time nil)
           (cur-prefix (or ,prefix "C-x"))
           (hashkey "")
           (prefix-regexp ""))
       (ergoemacs-debug (make-string 80 ?=))
       (ergoemacs-debug "Extracting maps for %s" cur-prefix)
       (ergoemacs-debug (make-string 80 ?=))
       (with-temp-buffer
         (let (ergoemacs-shortcut-mode)
           (describe-buffer-bindings buf (read-kbd-macro cur-prefix)))
         (goto-char (point-min))
         (while (re-search-forward (format "%s \\(.*?\\)[ \t]\\{2,\\}\\(.+\\)$" cur-prefix) nil t)
           (setq new-key (match-string 1))
           (setq fn (match-string 2))
           (unless (string-match " " new-key)
             (cond
              ((save-match-data
                 (string-match "[ \t]+[?][?]$" (match-string 0)))
               (ergoemacs-debug "Anonymous function for %s" new-key)
               (let (ergoemacs-shortcut-mode)
                 (setq fn (key-binding (read-kbd-macro new-key)))
                 (add-to-list 'normal (list new-key fn))))
              ((save-match-data
                 (string-match "Prefix Command$" (match-string 0)))
               (unless (string-match "ESC" new-key)
                 (ergoemacs-debug "Prefix: %s" new-key)
                 (add-to-list 'prefixes new-key)))
              (t
               (condition-case err
                   (with-temp-buffer
                     (insert "(if (condition-case err (keymapp '" fn
                             ") (error nil)) (unless (string-match \"ESC\" \"" new-key
                             "\") (add-to-list 'prefixes \"" new-key
                             "\") (ergoemacs-debug \"Prefix (keymap): %s\" new-key)) (add-to-list 'normal '(\""
                             new-key "\" " fn ")) (ergoemacs-debug \"Normal: %s -> %s\" new-key fn))")
                     (eval-buffer)
                     (when ergoemacs-translate-keys
                       (cond
                        ((string-match "\\( \\|^\\)C-\\([a-zA-Z'0-9{}/,.`]\\)$" new-key)
                         (add-to-list 'translations
                                      (list (replace-match "\\1\\2" t nil new-key)
                                            fn)))
                        ((string-match "\\( \\|^\\)\\([a-zA-Z'0-9{}/,.`]\\)$" new-key)
                         (add-to-list 'translations
                                      (list (replace-match "\\1C-\\2" t nil new-key)
                                            fn))))))
                 (error
                  (setq fn nil))))))))

       (ergoemacs-debug (make-string 80 ?=))
       (ergoemacs-debug "Finished (%1f sec); Building keymap" (- (float-time) start-time))
       (setq last-time (float-time))      (ergoemacs-debug (make-string 80 ?=))
       (setq hashkey (md5 (format "%s;%s;%s" cur-prefix normal prefixes)))
       (setq ,keymap (gethash hashkey ergoemacs-extract-map-hash))
       (unless ,keymap
         (setq ,keymap (make-keymap))
         (mapc
          (lambda(x)
            (let* ((normal (nth 0 x))
                   (ctl-to-alt
                    (replace-regexp-in-string
                     "\\<W-" "M-"
                     (replace-regexp-in-string
                      "\\<M-" "C-"
                      (replace-regexp-in-string "\\<C-" "W-" normal))))
                   (unchorded
                    (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
              (if (not (functionp (nth 1 x)))
                  (when (string-match "^C-x 8" cur-prefix)
                    (ergoemacs-debug "Not a function AND C-x 8, assuming translation.")
                    (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal (nth 1 x))
                    
                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Normal> %s %s" cur-prefix normal))
                      (read-kbd-macro (format "%s" (nth 1 x))))
                    
                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Ctl%sAlt> %s %s" 
                               (ergoemacs-unicode-char "↔" " to ")
                               cur-prefix ctl-to-alt))
                      (read-kbd-macro (format "%s" (nth 1 x))))
                    (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                     (ergoemacs-unicode-char "↔" " to ")
                                     cur-prefix ctl-to-alt (nth 1 x))

                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Unchorded> %s %s" cur-prefix unchorded))
                      (read-kbd-macro (format "%s" (nth 1 x))))

                    (ergoemacs-debug "<Unchorded> %s %s => %s"
                                     cur-prefix unchorded (nth 1 x)))
                (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal (nth 1 x))
                (define-key ,keymap
                  (read-kbd-macro (format "<Normal> %s %s" cur-prefix normal)) (nth 1 x))
                (define-key ,keymap
                  (read-kbd-macro
                   (format "<Ctl%sAlt> %s %s" 
                           (ergoemacs-unicode-char "↔" " to ")
                           cur-prefix ctl-to-alt)) (nth 1 x))
                (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                 (ergoemacs-unicode-char "↔" " to ")
                                 cur-prefix ctl-to-alt (nth 1 x))
                
                (define-key ,keymap
                  (read-kbd-macro
                   (format "<Unchorded> %s %s" cur-prefix unchorded)) (nth 1 x))
                (ergoemacs-debug "<Unchorded> %s %s => %s"
                                 cur-prefix unchorded (nth 1 x)))))
          normal)
         (ergoemacs-debug (make-string 80 ?=))
         (ergoemacs-debug "Built (%1f s;%1f s); Adding Prefixes" 
                          (- (float-time) start-time)
                          (- (float-time) last-time))
         (setq last-time (float-time))
         (ergoemacs-debug (make-string 80 ?=))
         
         ;; Now add prefixes.
         (mapc
          (lambda(x)
            (let ((new (replace-regexp-in-string
                        "\\<W-" "M-"
                        (replace-regexp-in-string
                         "\\<M-" "C-"
                         (replace-regexp-in-string "\\<C-" "W-" x)))))

              (condition-case err
                  (define-key ,keymap
                    (read-kbd-macro (format "<Normal> %s %s" cur-prefix x))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'normal)))
                (error nil))

              (condition-case err
                  (define-key ,keymap 
                    (read-kbd-macro
                     (format "<Ctl%sAlt> %s %s" 
                             (ergoemacs-unicode-char "↔" " to ")
                             cur-prefix new))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'ctl-to-alt)))
                (error nil))
              
              (setq new
                    (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" new))))
              
              (condition-case err
                  (define-key ,keymap 
                    (read-kbd-macro
                     (format "<Unchorded> %s %s"
                             cur-prefix new))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'unchorded)))
                (error nil))))
          prefixes)

         (ergoemacs-debug (make-string 80 ?=))
         (ergoemacs-debug "Built (%1f s;%1f s); Translating keys" 
                          (- (float-time) start-time)
                          (- (float-time) last-time))
         (setq last-time (float-time))
         (ergoemacs-debug (make-string 80 ?=))
         
         ;;
         (when ergoemacs-translate-keys
           (setq bound-regexp
                 (format "^%s$"
                         (regexp-opt
                          (append
                           (mapcar (lambda(x) (nth 0 x))
                                   normal) prefixes) t)))
           (ergoemacs-debug (make-string 80 ?=))
           (ergoemacs-debug "Translating keys for %s" cur-prefix)
           (ergoemacs-debug (make-string 80 ?=))
           (mapc
            (lambda(x)
              (if (string-match bound-regexp (nth 0 x))
                  (ergoemacs-debug "Assume %s is already defined" x)
                (ergoemacs-debug "Testing %s; %s" x (functionp (intern (nth 1 x))))
                (when (functionp (intern (nth 1 x)))    
                  (let* ((fn (intern (nth 1 x)))
                         (normal (nth 0 x))
                         (ctl-to-alt
                          (replace-regexp-in-string
                           "\\<W-" "M-"
                           (replace-regexp-in-string
                            "\\<M-" "C-"
                            (replace-regexp-in-string "\\<C-" "W-" normal))))
                         (unchorded
                          (replace-regexp-in-string
                           "\\<W-" ""
                           (replace-regexp-in-string
                            "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                            (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
                    (let ((curr-kbd (format "<Normal> %s %s" cur-prefix normal)))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (define-key ,keymap
                        (read-kbd-macro curr-kbd) fn)
                      (condition-case err
                          (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal fn)
                        (error (ergoemacs-debug "%s" err)))
                      (setq curr-kbd
                            (format "<Ctl%sAlt> %s %s" 
                                    (ergoemacs-unicode-char "↔" " to ")
                                    cur-prefix ctl-to-alt))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (condition-case err
                          (define-key ,keymap
                            (read-kbd-macro curr-kbd) fn)
                        (error (ergoemacs-debug "%s" err)))
                      (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                       (ergoemacs-unicode-char "↔" " to ")
                                       cur-prefix ctl-to-alt fn)
                      (setq curr-kbd (format "<Unchorded> %s %s" cur-prefix unchorded))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (condition-case err
                          (define-key ,keymap
                            (read-kbd-macro curr-kbd) fn)
                        (error (ergoemacs-debug "%s" err)))
                      (ergoemacs-debug "<Unchorded> %s %s => %s"
                                       cur-prefix unchorded fn))))))
            translations))

         (ergoemacs-debug (make-string 80 ?=))
         (ergoemacs-debug "Built (%1f s;%1f s); Adding swap" 
                          (- (float-time) start-time)
                          (- (float-time) last-time))
         (setq last-time (float-time))
         (ergoemacs-debug (make-string 80 ?=))
         
         ;; Now add root level swap.
         (ergoemacs-debug "Root: %s <%s>" cur-prefix (if (eq system-type 'windows-nt) "apps" "menu"))
         
         (condition-case err
             (define-key ,keymap
               (read-kbd-macro (format "<Normal> %s <%s>" cur-prefix
                                       (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'normal)))
           (error nil))

         (condition-case err
             (define-key ,keymap
               (read-kbd-macro (format "<Normal> %s <exit>" cur-prefix))
               'ignore)
           (error nil))
         
         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Ctl%sAlt> %s <%s>" 
                        (ergoemacs-unicode-char "↔" " to ")
                        cur-prefix
                        (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'ctl-to-alt)))
           (error nil))

         (condition-case err
             (define-key ,keymap
               (read-kbd-macro
                (format "<Ctl%sAlt> %s <exit>"
                        (ergoemacs-unicode-char "↔" " to ")
                        cur-prefix)) 'ignore)
           (error nil))

         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Unchorded> %s <%s>"
                        cur-prefix
                        (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'unchorded)))
           (error nil))

         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Unchorded> %s <exit>"
                        cur-prefix)) `ignore)
           (error nil))
         (puthash hashkey ,keymap ergoemacs-extract-map-hash))
       (ergoemacs-debug (make-string 80 ?=))
       (ergoemacs-debug-flush))))

(defcustom ergoemacs-echo-function-call nil
  "Will echo function call when `ergoemacs-menu-send-function' is called."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-menu-send-function (prefix-key untranslated-key fn)
  "Sends actual key for translation maps or runs function FN"
  (setq ergoemacs-push-M-O-timeout nil)
  (setq this-command last-command) ; Don't record this command.
  (setq prefix-arg current-prefix-arg)
  (condition-case err
      (progn
        (call-interactively fn)
        (when ergoemacs-echo-function-call
          (princ
           (format "%s%s%s: %s" (if current-prefix-arg
                                    (format "%s " current-prefix-arg)
                                  "")
                   (ergoemacs-pretty-key prefix-key)
                   (if (string= untranslated-key "<timeout>") ""
                     (ergoemacs-pretty-key untranslated-key))
                   fn))))
    (error
     (message "Error %s" err))))

(defun ergoemacs-menu-send-prefix (prefix-key untranslated-key type)
  "Extracts maps for PREFIX-KEY UNTRANSLATED-KEY of TYPE."
  (setq this-command last-command) ; Don't record this command.
  (setq prefix-arg current-prefix-arg)
  (ergoemacs-shortcut-internal (format "%s %s" prefix-key untranslated-key) type))

(defun ergoemacs-menu-swap (prefix-key untranslated-key type)
  "Swaps what <menu> key translation is in effect"
  (let* (deactivate-mark
         (new-type nil)
         (new-key nil)
         (kbd-code nil)
         (normal untranslated-key)
         (ctl-to-alt (replace-regexp-in-string
                      "\\<W-" "M-"
                      (replace-regexp-in-string
                       "\\<M-" "C-"
                       (replace-regexp-in-string "\\<C-" "W-" normal))))
         (unchorded (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
    (cond
     ((member ergoemacs-first-extracted-variant '(ctl-to-alt normal))
      (cond
       ((eq type 'ctl-to-alt)
        (setq new-type 'unchorded))
       ((eq type 'unchorded)
        (setq new-type 'normal))
       ((eq type 'normal)
        (setq new-type 'ctl-to-alt))))
     ((equal ergoemacs-first-extracted-variant 'unchorded)
      (cond
       ((eq type 'ctl-to-alt)
        (setq new-type 'normal))
       ((eq type 'unchorded)
        (setq new-type 'ctl-to-alt))
       ((eq type 'normal)
        (setq new-type 'unchorded)))))
    (setq kbd-code
          (cond
           ((eq new-type 'normal)
            (format "<Normal> %s %s" prefix-key normal))
           ((eq new-type 'ctl-to-alt)
            (format "<Ctl%sAlt> %s %s"
                    (ergoemacs-unicode-char "↔" " to ")
                    prefix-key
                    ctl-to-alt))
           ((eq new-type 'unchorded)
            (format "<Unchorded> %s %s" prefix-key
                    unchorded))))
    (setq new-key (listify-key-sequence (read-kbd-macro kbd-code)))
    (setq this-command last-command) ; Don't record this command.
    (setq prefix-arg current-prefix-arg)
    (set-temporary-overlay-map ergoemacs-current-extracted-map)
    (reset-this-command-lengths)
    (setq unread-command-events (append new-key unread-command-events))
    
    (save-match-data
      (when (string-match "<\\(.*?\\)> \\(.*\\)" kbd-code)
        (message "%s%s"
                 (if current-prefix-arg (format "%s " current-prefix-arg) "")
                 (replace-regexp-in-string "<Normal> +" ""
                                           (format "<%s> %s" (match-string 1 kbd-code)
                                                   (ergoemacs-pretty-key (match-string 2 kbd-code)))))))))


(defvar ergoemacs-repeat-shortcut-keymap (make-sparse-keymap)
  "Keymap for repeating often used shortcuts like C-c C-c.")

(defvar ergoemacs-repeat-shortcut-msg ""
  "Message for repeating keyboard shortcuts like C-c C-c")

(defun ergoemacs-shortcut-timeout ()
  (message ergoemacs-repeat-shortcut-msg)
  (set-temporary-overlay-map ergoemacs-repeat-shortcut-keymap))

(defvar ergoemacs-current-extracted-map nil
  "Current extracted map for `ergoemacs-shortcut' defined functions")

(defvar ergoemacs-first-extracted-variant nil
  "Current extracted variant")

(defcustom ergoemacs-shortcut-ignored-functions
  '(undo-tree-visualize)
  "Ignored functions for `ergoemacs-shortcut'."
  :group 'ergoemacs-mode
  :type '(repeat
          (symbol :tag "Function to ignore:")))

(defun ergoemacs-shortcut (&optional arg)
  "Shortcut for other key/function.
Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for `this-command-keys-vector'.  The
workhorse of this function is in `ergoemacs-shortcut-internal'."
  (interactive "P")
  (let ((args (gethash (this-command-keys-vector)
                       ergoemacs-command-shortcuts-hash)))
    (unless args
      (gethash (read-kbd-macro (key-description (this-command-keys)) t)))
    (if (not args)
        (progn
          ;; Remove reference to `ergoemacs-shortcut'
          (when (featurep 'keyfreq)
            (when keyfreq-mode
              (let ((command 'ergoemacs-shortcut) count)
                (setq count (gethash (cons major-mode command) keyfreq-table))
                (remhash (cons major-mode command) keyfreq-table)
                ;; Add `ergoemacs-undefined' to counter.
                (setq command 'ergoemacs-undefined)
                (setq count (gethash (cons major-mode command) keyfreq-table))
                (puthash (cons major-mode command) (if count (+ count 1) 1)
                         keyfreq-table))))
          (ergoemacs-undefined))
      (when (featurep 'keyfreq)
        (when keyfreq-mode
          (let ((command 'ergoemacs-shortcut) count)
            (setq count (gethash (cons major-mode command) keyfreq-table))
            (remhash (cons major-mode command) keyfreq-table))))
      (setq this-command last-command)
      (setq prefix-arg current-prefix-arg)
      (if (interactive-form (nth 0 args))
          (eval (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args))))
        (eval (macroexpand `(ergoemacs-shortcut-internal ,(nth 0 args) ',(nth 1 args))))))))

(defun ergoemacs-shortcut-internal (key &optional chorded repeat keymap-key)
  "Ergoemacs Shortcut.

KEY is the keyboard shortcut.

CHORDED is a variable that alters to keymap to allow unchorded
key sequences.  Also if CHORDED is 'global, then make this a
shortcut to a global command.

If CHORDED is nil, the NAME command will just issue the KEY sequence.

If CHORDED is 'unchorded or the NAME command will translate the control
bindings to be unchorded.  For example:

For example for the C-x map,

Original Key   Translated Key  Function
C-k C-n     -> k n             (kmacro-cycle-ring-next)
C-k a       -> k M-a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

If CHORDED is 'ctl-to-alt or the NAME command will translate the control
bindings to be unchorded.  For example:

C-k C-n     -> M-k M-n         (kmacro-cycle-ring-next)
C-k a       -> M-k a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

When REPEAT is a variable name, then an easy repeat is setup for the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows Ctrl+c Ctrl+c to be repeated by m.

When KEYMAP-KEY is non-nil, define the KEYMAP-KEY on the `ergoemacs-shortcut-override-keymap'
"
  (cond
   ((or (not chorded)
        (condition-case err
            (functionp key)
          (error nil))
        (memq chorded '(repeat repeat-global global-repeat global)))
    ;; A single function for the key shortcut.
    (let ((ctl-c-keys (key-description (this-command-keys))))
      (let (ergoemacs-shortcut-mode
            ergoemacs-unbind-mode
            (minor (intern-soft (format "ergoemacs-%s-hook-mode" major-mode)))
            old-minor
            ;; if chorded is undefined, shortcut is to ergoemacs-keys
            ;; if chorded doesn't have global in it it acts on
            ;; ergoemacs-keys.
            ;; otherwise the shortcut is acting on non-ergoemacs keys.
            (ergoemacs-mode
             (or (eq chorded 'repeat)
                 (not chorded)))
            fn fn-lst new-fn fn-override)
        (setq ergoemacs-unbind-mode ergoemacs-mode)
        ;; Temporarily unbind ergoemacs-major-mode-hook-mode
        (when minor
          (setq old-minor (symbol-value minor))
          (set minor nil))
        (cond
         ((condition-case err
              (interactive-form key)
            (error nil))
          ;; FIXME:  Overlays that are installed/removed based on
          ;; pre-command-hook status can be disrupted.  This is the
          ;; case in `auto-complete-mode'.  Therefore,
          ;; `ergoemacs-mode' currently does not translate these keys
          ;; correctly :(

          ;; I tried a post-command-hook setting up a temporary
          ;; overlay map with the actual command.  It was too slow...

          ;; Currently fixed by a hook :)
          
          ;; Lookup function on non-ergoemacs keymaps.
          (setq ergoemacs-mode nil)
          (setq ergoemacs-unbind-mode nil)
          (mapc
           (lambda(cur-key)
             (let ((binding (key-binding cur-key t nil (point))))
               (setq new-fn (intern-soft (format "erogemacs-%s" binding)))
               (when (and new-fn (interactive-form new-fn))
                 ;; When a lookup finds org-metadown and there is a
                 ;; function ergoemacs-org-metadown, use the
                 ;; ergoemacs-org-metadown instead.
                 (setq fn-override new-fn))
               (unless (or (eq binding key)
                           (memq binding ergoemacs-shortcut-ignored-functions))
                 (add-to-list 'fn-lst binding))))
           (or (remove-if '(lambda(x) (eq 'menu-bar (elt x 0))) ; Ignore
                                                           ; menu-bar
                                                           ; functions
                          (where-is-internal key (current-global-map)))
               (gethash key ergoemacs-where-is-global-hash)))
          (cond
           (fn-override
            (set fn fn-override))
           (fn-lst
            ;; FIXME: If new functions exist, have user option to use
            ;; these functions

            ;; For now, just use the first function.
            (setq fn (nth 0 fn-lst)))
           (t  ; Could not find another function, just use the
               ; function passed to `ergoemacs-shortcut'
            (setq fn key))))
         (t ;; lookup keybinding for the function keys.          
          (setq fn (key-binding (read-kbd-macro key)))))
        (if (not fn)
            (unless keymap-key
              (message "%s is not defined." (ergoemacs-pretty-key key)))
          (unless keymap-key
            (setq this-command fn) ; Don't record this command.
            (setq prefix-arg current-prefix-arg))
          (if (condition-case err
                  (interactive-form fn)
                (error nil))
              (if keymap-key
                  (progn
                    (define-key ergoemacs-shortcut-override-keymap
                      keymap-key fn))
                (when (featurep 'keyfreq)
                  (when keyfreq-mode
                    (let ((command fn) count)
                      ;; Add function name to to counter.
                      (setq count (gethash (cons major-mode command)
                                           keyfreq-table))
                      (puthash (cons major-mode command) (if count (+ count 1) 1)
                               keyfreq-table))))
                (call-interactively fn)
                ;; repeat only works with a function.
                (when (and repeat
                           (or (not chorded)
                               (not (eq chorded 'global))))
                  (when  (string-match "[A-Za-z]$" ctl-c-keys)
                    (setq ctl-c-keys (match-string 0 ctl-c-keys))
                    (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
                    (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys) fn)
                    (setq ergoemacs-repeat-shortcut-msg
                          (format  "Repeat %s with %s"
                                   (ergoemacs-pretty-key key)
                                   (ergoemacs-pretty-key ctl-c-keys)))
                    ;; Allow time to process the unread command events before
                    ;; installing temporary keymap
                    (setq ergoemacs-M-O-timer
                          (run-with-timer ergoemacs-M-O-delay nil
                                          #'ergoemacs-shortcut-timeout)))))
            ;; Not a function, probably a keymap
            (if keymap-key
                (progn
                  ;; (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys) (symbol-value fn))
                  )
              (setq prefix-arg current-prefix-arg)
              (setq unread-command-events
                    (append
                     (listify-key-sequence (read-kbd-macro key))
                     unread-command-events))
              (reset-this-command-lengths))))
        (when minor
          (set minor old-minor)))))
   (keymap-key ;; extract key prefixes.
    )
   (t ;; key prefix
    (setq ergoemacs-push-M-O-timeout nil) ;; Cancel timeouts
    (setq this-command last-command) ; Don't record this command.
    (setq prefix-arg current-prefix-arg)
    (let (key-seq
          (key-type
           (cond
            ((eq chorded 'unchorded)
             "Unchorded")
            ((eq chorded 'ctl-to-alt)
             (format "Ctl%sAlt"
                     (ergoemacs-unicode-char "↔" " to ")))
            (t "Normal")))
          deactivate-mark)
      (eval (macroexpand '(ergoemacs-extract-maps ergoemacs-current-extracted-map key)))
      (set-temporary-overlay-map ergoemacs-current-extracted-map)
      (setq ergoemacs-first-extracted-variant chorded)
      (setq key-seq
            (read-kbd-macro
             (format "<%s> %s" key-type key)))
      (setq key-seq (listify-key-sequence key-seq))
      (reset-this-command-lengths)
      (setq unread-command-events
            (append key-seq unread-command-events))
      (setq key-type (concat "<" key-type "> "))
      (when (string= key-type "<Normal> ")
        (setq key-type ""))
      (princ (concat
              (if current-prefix-arg
                  (format "%s " current-prefix-arg)
                "")
              (format "%s%s " key-type
                      (ergoemacs-pretty-key key))))))))

(defcustom ergoemacs-repeat-ctl-c-ctl-c t
  "Repeat C-c C-c"
  :group 'ergoemacs-mode
  :type 'boolean)

(defun ergoemacs-ctl-c-ctl-c (&optional arg)
  "Ergoemacs C-c C-c. If `ergoemacs-repeat-ctl-c-ctl-c', repeat the command"
  (interactive "P")
  (setq this-command last-command) ; Don't record this command.
  (setq prefix-arg current-prefix-arg)
  (ergoemacs-shortcut-internal "C-c C-c" 'repeat-global ergoemacs-repeat-ctl-c-ctl-c))


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

(defvar ergoemacs-org-CUA-compatible nil)
(defvar ergoemacs-shift-select-mode nil)
(defvar ergoemacs-delete-selection-mode nil)

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

The shortcuts defined are:

\\{ergoemacs-shortcut-keymap}
"
  nil
  :lighter " ErgoEmacs"
  :global t
  :group 'ergoemacs-mode
  :keymap ergoemacs-keymap
  
  ;; Try to turn on only rectangle support, global mark mode, and
  ;; other features of CUA mode.  Let ergoemacs handle C-c and C-v.
  ;; This will possibly allow swapping of C-c and M-c.
  (when (and ergoemacs-mode cua-mode)
    (cua-mode -1)
    (cua-selection-mode 1))
  ;; Turn on/off shift-select delete-selection and CUA-compatible org-mode
  (cond
   (ergoemacs-mode
    ;; (if (boundp 'org-CUA-compatible)
    ;;     (setq ergoemacs-org-CUA-compatible nil)
    ;;   (setq ergoemacs-org-CUA-compatible org-CUA-compatible))
    (setq ergoemacs-shift-select-mode shift-select-mode)
    (setq ergoemacs-delete-selection-mode delete-selection-mode)
    ;; (setq org-CUA-compatible t)
    (set-default 'shift-select-mode t)
    ;; turn on text selection highlighting and make typing override
    ;; selected text (Note: when delete-selection-mode is on, then
    ;; transient-mark-mode is automatically on too.)
    (delete-selection-mode 1))
   ((not ergoemacs-mode)
    ;; (when (boundp 'org-CUA-compatible)
    ;;   (setq org-CUA-compatible ergoemacs-org-CUA-compatible))
    (setq shift-select-mode ergoemacs-shift-select-mode)
    (unless ergoemacs-delete-selection-mode
      (delete-selection-mode -1))))
  
  (setq ergoemacs-shortcut-keymap (make-sparse-keymap))
  (ergoemacs-setup-keys t)
  (ergoemacs-debug "Ergoemacs Keys have loaded.")
  (if ergoemacs-use-menus
      (progn
        (require 'ergoemacs-menus)
        (ergoemacs-menus-on))
    (when (featurep 'ergoemacs-menus)
      (ergoemacs-menus-off)))
  (when (and (eq system-type 'darwin))
    (if ergoemacs-mode
        (progn
          (setq ergoemacs-old-ns-command-modifier ns-command-modifier)
          (setq ergoemacs-old-ns-alternate-modifier ns-alternate-modifier)
          (setq ns-command-modifier 'meta)
          (setq ns-alternate-modifier nil))
      (setq ns-command-modifier ergoemacs-old-ns-command-modifier)
      (setq ns-alternate-modifier ergoemacs-old-ns-alternate-modifier)))
  (if ergoemacs-mode
      (define-key cua--cua-keys-keymap (read-kbd-macro "M-v") nil)
    (define-key cua--cua-keys-keymap (read-kbd-macro "M-v") 'cua-repeat-replace-region))
  (condition-case err
      (when ergoemacs-cua-rect-modifier
        (if ergoemacs-mode
            (progn
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
                      (cua-mode . ,cua-global-keymap))))
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
                  (cua-mode . ,cua-global-keymap))))
        (ergoemacs-debug "CUA rectangle mode modifier changed."))
    (error (message "CUA rectangle modifier wasn't changed.")))
  
  (when ergoemacs-change-smex-M-x
    (if ergoemacs-mode
        (setq smex-prompt-string (concat (ergoemacs-pretty-key "M-x") " "))
      (setq smex-prompt-string "M-x ")))
  (if ergoemacs-mode
      (mapc ;; Now install hooks.
       (lambda(buf)
         (with-current-buffer buf
           (when (and (intern-soft (format "ergoemacs-%s-hook" major-mode)))
             (funcall (intern-soft (format "ergoemacs-%s-hook" major-mode))))))
       (buffer-list))
    (mapc ;; Remove overriding keys.
     (lambda(buf)
       (with-current-buffer buf
         (when (and (intern-soft (format "ergoemacs-%s-hook-mode" major-mode))
                    (symbol-value (intern-soft (format "ergoemacs-%s-hook-mode" major-mode))))
           (funcall (intern-soft (format "ergoemacs-%s-hook-mode" major-mode)) -1))
         (let ((x (assq 'ergoemacs-mode minor-mode-overriding-map-alist)))
           (if x
               (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist)))
           (setq x (assq 'ergoemacs-shortcut-mode minor-mode-overriding-map-alist))
           (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist)))))
     (buffer-list)))
  (if ergoemacs-mode
      (progn
        (ergoemacs-shortcut-mode 1)
        (ergoemacs-unbind-mode 1))
    (ergoemacs-shortcut-mode -1)
    (ergoemacs-unbind-mode -1))
  (ergoemacs-debug-flush))

(define-minor-mode ergoemacs-shortcut-mode
  "Toggle `ergoemacs-mode' shortcut keys.  In theory, this allows
`ergoemacs-mode' to overwrite commonly used shortcuts like C-c and C-x"
  nil
  :lighter ""
  :global t
  :group 'ergoemacs-mode
  (if ergoemacs-shortcut-mode
      (progn
        ;; (ergoemacs-debug "Ergoemacs Shortcut Keys have loaded been turned on.")
        (let ((x (assq 'ergoemacs-shortcut-mode minor-mode-map-alist)))
          (when x
            (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
          (push (cons 'ergoemacs-shortcut-mode ergoemacs-shortcut-keymap) minor-mode-map-alist)))
    ;; (ergoemacs-debug "Ergoemacs Shortcut Keys have loaded been turned off.")
    )
  ;; (ergoemacs-debug-flush)
  )

(define-minor-mode ergoemacs-shortcut-override-mode
  "Lookup the functions for `ergoemacs-mode' shortcut keys."
  nil
  :lighter ""
  :global t
  :group 'ergoemacs-mode
  (if ergoemacs-shortcut-override-mode
      (progn
        (ergoemacs-debug "Ergoemacs Shortcut Override have loaded been turned on.")
        (let ((x (assq 'ergoemacs-shortcut-override-mode
                       emulation-mode-map-alists)))
          (when x
            (setq emulation-mode-map-alists (delq x emulation-mode-map-alists)))
          ;; Create keymap
          (setq ergoemacs-shortcut-override-keymap (make-sparse-keymap))
          ;; Add M-O and M-o key-bindings; Pretend they are the actual
          ;; bindings instead of the M-O and M-o work-rounds.
          (when (eq (key-binding (read-kbd-macro "M-O"))
                    'ergoemacs-M-O)
            (define-key ergoemacs-shortcut-override-keymap
              (read-kbd-macro "M-O")
              (lookup-key ergoemacs-M-O-keymap [timeout])))
          (when (eq (key-binding (read-kbd-macro "M-o"))
                    'ergoemacs-M-o)
            (define-key ergoemacs-shortcut-override-keymap
              (read-kbd-macro "M-o")
              (lookup-key ergoemacs-M-o-keymap [timeout])))
          (maphash
           (lambda(key args)
             (if (interactive-form (nth 0 args))
                 (eval (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args) nil ,key)))
               (eval (macroexpand `(ergoemacs-shortcut-internal ,(nth 0 args) ',(nth 1 args) nil ,key)))))
           ergoemacs-command-shortcuts-hash)
          (push (cons 'ergoemacs-shortcut-override-mode
                      ergoemacs-shortcut-override-keymap)
                emulation-mode-map-alists)))
    (ergoemacs-debug "Ergoemacs Shortcut Keys have loaded been turned off."))
  (ergoemacs-debug-flush))

(defvar ergoemacs-unbind-keymap (make-sparse-keymap)
  "Keymap for `ergoemacs-unbind-mode'")

(defun ergoemacs-undefined (&optional arg)
  "Ergoemacs Undefined key, tells where to perform the old action."
  (interactive "P")
  (let* ((key (key-description (this-command-keys)))
         (fn (assoc key ergoemacs-emacs-default-bindings))
         (local-fn nil)
         (last (substring key -1))
         (ergoemacs-where-is-skip t)
         (curr-fn nil))
    ;; Lookup local key, if present and then issue that
    ;; command instead...
    ;;
    ;; This way the unbound keys are just above the global
    ;; map and doesn't actually change it.
    (cond
     ((progn
        ;; Local map present.  Use it, if there is a key
        ;; defined there.
        (setq local-fn (get-char-property (point) 'local-map))
        (if local-fn
            (setq local-fn (lookup-key local-fn
                                       (read-kbd-macro key)))
          (setq local-fn (lookup-key (current-local-map)
                                     (read-kbd-macro key))))
        (functionp local-fn))
      (setq this-command last-command) ; Don't record this
                                        ; command.
      (setq prefix-arg current-prefix-arg)
      (call-interactively local-fn t))
     (t
      ;; Not locally defined, complain.
      (beep)
      (message "%s keybinding is disabled! Use %s"
               (ergoemacs-pretty-key key)
               (ergoemacs-pretty-key-rep
                (with-temp-buffer
                  (setq curr-fn (nth 0 (nth 1 fn)))
                  (when (and fn (not (eq 'prefix curr-fn)))
                    (setq curr-fn (ergoemacs-translate-current-function curr-fn))
                    (where-is curr-fn t))
                  (ergoemacs-format-where-is-buffer)
                  (buffer-string))))))))

(defun ergoemacs-unbind-setup-keymap ()
  "Setup `ergoemacs-unbind-keymap' based on current layout."
  (setq ergoemacs-unbind-keymap (make-sparse-keymap))
  (mapc
   (lambda(x)
     (unless (ergoemacs-global-changed-p x)
       (define-key ergoemacs-unbind-keymap (read-kbd-macro x) 'ergoemacs-undefined
         )))
   (symbol-value (ergoemacs-get-redundant-keys))))

(define-minor-mode ergoemacs-unbind-mode
  "These are the unbound keys for `ergoemacs-mode'.  This should
be the last entry in `minor-mode-map-alist'.

Ideally, this keymap should be right above the global binding
map. However, by making this a minor mode, it masks bindings from:

- locally-defined maps in the text properties (get-char-property (point) 'local-map)
- `current-local-map'

If these keys exist, execute what was bound to them.
" 
  
  nil
  :lighter ""
  :global t
  :group 'ergoemacs-mode
  (if ergoemacs-unbind-mode
      (progn
        (ergoemacs-debug "Ergoemacs Unbind Keys have loaded been turned on.")
        (let ((x (assq 'ergoemacs-unbind-mode minor-mode-map-alist)))
          (when x
            (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
          ;; Put at the END of the list.
          (setq minor-mode-map-alist
                (append minor-mode-map-alist
                        (list (cons 'ergoemacs-unbind-mode ergoemacs-unbind-keymap))))))
    (ergoemacs-debug "Ergoemacs Unbind Keys have loaded been turned off.")))



;; ErgoEmacs replacements for local-set-key

(defvar ergoemacs-local-keymap nil
  "Local ergoemacs keymap")
(make-variable-buffer-local 'ergoemacs-local-keymap)

(defun ergoemacs-local-set-key (key command)
  "Set a key in the ergoemacs local map."
  ;; install keymap if not already installed
  (interactive)
  (progn
    (unless ergoemacs-local-keymap
      (ergoemacs-setup-keys-for-keymap ergoemacs-local-keymap)
      (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-local-keymap)))
    ;; add key
    (define-key ergoemacs-local-keymap key command)))

(defun ergoemacs-local-unset-key (key)
  "Unset a key in the ergoemacs local map."
  (ergoemacs-local-set-key key nil))



(require 'ergoemacs-advices)

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

(defun ergoemacs-pre-command-hook ()
  "Ergoemacs pre-command-hook."
  (let (deactivate-mark)
    (condition-case err
        (when ergoemacs-mode
          (when (and (not ergoemacs-show-true-bindings)
                     (memq this-command ergoemacs-describe-keybindings-functions))
            (ergoemacs-shortcut-mode -1)
            (ergoemacs-shortcut-override-mode 1)))
      (error nil)))
  t)

(defun ergoemacs-post-command-hook ()
  "Ergoemacs post-command-hook"
  (let (deactivate-mark)
    (condition-case err
        (when ergoemacs-mode
          (when (and (not ergoemacs-show-true-bindings)
                     (memq this-command ergoemacs-describe-keybindings-functions))
            (ergoemacs-shortcut-mode 1)
            (ergoemacs-shortcut-override-mode -1))
          
          (unless (eq 'ergoemacs-shortcut-mode
                      (car (car minor-mode-map-alist)))
            (ergoemacs-debug "Promote ergoemacs-shortcut-mode in `minor-mode-map-alist'")
            (let ((x (assq 'ergoemacs-shortcut-mode minor-mode-map-alist)))
              (when x
                (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
              (push (cons 'ergoemacs-shortcut-mode ergoemacs-shortcut-keymap) minor-mode-map-alist)))
          
          (when minor-mode-overriding-map-alist
            (unless (eq 'ergoemacs-shortcut-mode
                        (car (car minor-mode-overriding-map-alist)))
              (ergoemacs-debug "Promote ergoemacs-shortcut-mode in `minor-mode-overriding-map-overriding-map-alist'")
              (let ((x (assq 'ergoemacs-shortcut-mode minor-mode-overriding-map-alist)))
                (when x
                  (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist)))
                (push (cons 'ergoemacs-shortcut-mode ergoemacs-shortcut-keymap) minor-mode-overriding-map-alist)))))
      (error (message "Error %s" err))))
  t)

(add-hook 'post-command-hook 'ergoemacs-post-command-hook)
(add-hook 'pre-command-hook 'ergoemacs-pre-command-hook)

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
