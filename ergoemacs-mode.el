;;; ergoemacs-mode.el --- A minor mode, a keybinding set based on ergonomics. -*- coding: utf-8 -*-

;; Copyright © 2007, 2008, 2009 by Xah Lee
;; Copyright © 2009, 2010 by David Capello
;; Copyright © 2012, 2013 by Matthew Fidler

;; Author: Xah Lee <xah@xahlee.org> ( http://xahlee.org/ )
;;     David Capello <davidcapello@gmail.com>  ( http://www.davidcapello.com.ar/ )
;;     Matthew Fidler <matthew.fidler@gmail.com> ( http://github.com/mlf176f2/ )
;; Maintainer: Matthew Fidler, Xah Lee, David Capello
;; Created: August 01 2007
;; Version: 5.8.0.1
;; Keywords: convenience, qwerty, dvorak, keybinding, ergonomic, colemak
;; Package-Requires: ((org-cua-dwim "0.5"))

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; Commentary:
;; This keybinding set puts the most frequently used emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html
;; Also see the file "_README.txt"
;;
;; Install:
;; See the file “_INSTALL.txt”.

;;; Change Log:
;; See the file “_HISTORY.txt”.

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
;; Danish layout “da”. Contributors: Michael Budde
;; UK QWERTY layout “gb”. Contributor: Jorge Dias (aka theturingmachine)
;; UK Dvorak layout “gb-dv”. Contributor: Phillip Wood
;; French AZERTY layout “fr”. Contributor: Alexander Doe
;; Italian QWERTY layout “it”. Contributor: David Capello, Francesco Biccari


; (eval-when-compile (require 'cl))
(require 'cl)
(require 'easymenu)
(require 'cua-base)
(require 'cua-rect)

(require 'org-cua-dwim nil "NOERROR")
(when (featurep 'org-cua-dwim)
  (org-cua-dwim-activate))

;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.8.0"
  "Ergoemacs-keybindings minor mode version number.")

(defconst ergoemacs-mode-changes "Delete window Alt+0 changed to Alt+2.
Added beginning-of-buffer Alt+n (QWERTY notation) and end-of-buffer Alt+Shift+n")


;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs Directory")
(add-to-list 'load-path  ergoemacs-dir)


(defgroup ergoemacs-mode nil
  "Emulate CUA key bindings including C-x and C-c."
  :group 'editing-basics
  :group 'convenience
  :group 'emulations)

(defcustom ergoemacs-mode-used nil
  "Ergoemacs-keybindings minor mode version number used."
  :type 'string
  :group 'ergoemacs-mode)


(require 'ergoemacs-layouts)

(defvar ergoemacs-movement-functions
  '(scroll-down move-beginning-of-line move-end-of-line scroll-up scroll-down forward-block backward-block
                forward-word backward-word next-line previous-line forward-char backward-char
                ergoemacs-backward-block ergoemacs-forward-block ergoemacs-backward-open-bracket
                ergoemacs-forward-close-bracket move-end-of-line move-beginning-of-line backward-word forward-word
                subword-backward subword-forward
                beginning-of-buffer end-of-buffer)
  "Movement functions")

(defvar ergoemacs-deletion-functions
  '(delete-backward-char
  delete-char backward-kill-word kill-word kill-line
  ergoemacs-shrink-whitespaces ergoemacs-kill-line-backward)
  "Deletion functions")

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
  "Window/Tab switching functions")

(defun ergoemacs-set-default (symbol new-value)
  "Ergoemacs equivalent to set-default.  Will reload `ergoemacs-mode' after setting the values."
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

;; Movement commands need to be defined before ergoemacs-themes is
;; called to get the correct movement commands for isearch.
(defadvice cua--pre-command-handler (around ergoemacs-fix-shifted-commands activate)
  "Fixes shifted movement problems"
  (let ((do-it t)
        (case-fold-search nil)
        (send-timeout nil))
    (condition-case nil
        (progn
          ;; Fix shifted commands.
          (when (and (string-match "\\(^\\|-\\)M-" (key-descrtion (this-single-command-keys))) ;; Alt command
                     (or (eq (get this-command 'CUA) 'move)
                         (memq this-command ergoemacs-movement-functions)))
            (setq do-it nil))
          ;; Fix Issue 139.  However may introduce an issue when you
          ;; want to issue C-c commands quickly... 
          (when (and mark-active (string-match "^C-\\(c\\|x\\)" (key-description (this-single-command-keys))))
            (setq do-it t)
            (setq send-timeout t)))
      (error nil))
    (when cua--rectangle
      (setq do-it t))
    (when do-it
      ad-do-it)
    (when send-timeout
      (setq unread-command-events
            (cons 'timeout unread-command-events)))))

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

(defvar ergoemacs-M-O-trans
  '(("A" up)
    ("B" down)
    ("C" right)
    ("D" left)
    ("E" begin)
    ("F" end)
    ("H" home)
    ("M" kp-enter)
    ("P" f1)
    ("Q" f2)
    ("R" f3)
    ("S" f4)
    ("q" kp-1)
    ("s" kp-3)
    ("u" kp-9)
    ("w" kp-7)
    ("y" kp-5))
  "Terminal Translations.")

(defvar ergoemacs-M-O-keymap (make-keymap)
  "M-O translation map.")

(defvar ergoemacs-M-o-keymap (make-keymap)
  "M-o translation map.")

(defun ergoemacs-cancel-M-O ()
  "Cancels M-O [timeout] key."
  (setq ergoemacs-push-M-O-timeout nil)
  (when (timerp ergoemacs-M-O-timer)
    (cancel-timer ergoemacs-M-O-timer)))

(defvar ergoemacs-push-M-O-timeout nil
  "Should the M-O [timeout] key be canceled?")

(defvar ergoemacs-curr-prefix-arg nil)

(mapc
 (lambda(x)
   (define-key ergoemacs-M-o-keymap
     (read-kbd-macro (nth 0 x))
     `(lambda(&optional arg) (interactive "P")
        (setq ergoemacs-push-M-O-timeout nil)
        (when (timerp ergoemacs-M-O-timer)
          (cancel-timer ergoemacs-M-O-timer))
        (setq prefix-arg ergoemacs-curr-prefix-arg)
        (setq unread-command-events (cons ',(nth 1 x) unread-command-events))))
   (define-key ergoemacs-M-O-keymap
     (read-kbd-macro (nth 0 x))
     `(lambda(&optional arg) (interactive "P")
        (setq ergoemacs-push-M-O-timeout nil)
        (when (timerp ergoemacs-M-O-timer)
          (cancel-timer ergoemacs-M-O-timer))
        (setq prefix-arg ergoemacs-curr-prefix-arg)
        (setq unread-command-events (cons ',(nth 1 x) unread-command-events)))))
 ergoemacs-M-O-trans)


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

(defun ergoemacs-M-O-timeout ()
  "Push timeout on unread command events."
  (when (timerp ergoemacs-M-O-timer)
    (cancel-timer ergoemacs-M-O-timer))
  (when ergoemacs-push-M-O-timeout
    (setq prefix-arg ergoemacs-curr-prefix-arg)
    (setq unread-command-events (cons 'timeout unread-command-events))))

(defun ergoemacs-M-o (&optional arg use-map)
  "Ergoemacs M-o function to allow arrow keys and the like to work in the terminal. Call the true function immediately when `window-system' is true."
  (interactive "P")
  (setq ergoemacs-curr-prefix-arg current-prefix-arg)
  (let ((map (or use-map ergoemacs-M-o-keymap)))
    (if window-system
        (let ((fn (lookup-key map [timeout] t)))
          (call-interactively fn t))
      (when (timerp ergoemacs-M-O-timer)
        (cancel-timer ergoemacs-M-O-timer)
        ;; Issue correct command.
        (let ((window-system t))
          (ergoemacs-M-o arg use-map)))
      (setq ergoemacs-push-M-O-timeout t)
      (set-temporary-overlay-map map 'ergoemacs-exit-M-O-keymap)
      (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-M-O-timeout))))

(defun ergoemacs-M-O (&optional arg)
  "Ergoemacs M-O function to allow arrow keys and the like to work in the terminal."
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

(defvar ergoemacs-full-fast-keys-keymap (make-sparse-keymap)
  "Ergoemacs full fast keys keymap")

(defvar ergoemacs-full-alt-keymap (make-keymap)
  "Ergoemacs full Alt+ keymap.  Alt is removed from all these keys so that no key chord is necessary.")

(defvar ergoemacacs-full-alt-shift-keymap (make-keymap)
  "Ergoemacs full Alt+Shift+ keymap. Alt+shift is removed from all
  these keys so that no key chord is necessary. Unshifted keys are changed to shifted keys.")

(defun ergoemacs-exit-dummy ()
  "Dummy function for exiting keymaps."
  (interactive))

(defun ergoemacs-setup-fast-keys ()
  "Setup an array listing the fast keys."
  (interactive)
  (ergoemacs-create-undo-apps-keymap)
  (setq ergoemacs-full-fast-keys-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-keymap (make-keymap))
  (setq ergoemacs-full-alt-shift-keymap (make-keymap))
  (define-key ergoemacs-full-alt-keymap (kbd "<menu>") 'ergoemacs-exit-dummy)
  (define-key ergoemacs-full-alt-shift-keymap (kbd "<menu>") 'ergoemacs-exit-dummy)
  (mapc
   (lambda(var)
     (let* ((key (ergoemacs-kbd (nth 0 var) t))
            (cmd (nth 1 var))
            (stripped-key (replace-regexp-in-string "\\<[CM]-" "" key))
            (new-cmd (nth 1 var)))
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
  "Exit alt keys predicate"
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
                              'ergoemacs-exit-alt-keys))



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
  (set-temporary-overlay-map  ergoemacs-full-alt-shift-keymap
                              'ergoemacs-exit-alt-shift-keys))



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
(defvar ergoemacs-kbd-hash nil)

(setq ergoemacs-kbd-hash (make-hash-table :test 'equal))
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
     (eval `(setq ,(intern (concat "ergoemacs-" (symbol-name (nth 1 var)) "-key")) (ergoemacs-kbd (nth 0 var)))))
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

(defvar ergoemacs-debug nil
  "Debugging variable for ergoemacs.  Set to t to see debugging information in messages.")

(defmacro ergoemacs-setup-keys-for-keymap (keymap)
  "Setups ergoemacs keys for a specific keymap"
  `(let ((no-ergoemacs-advice t)
         (case-fold-search t)
         key
         trans-key
         cmd cmd-tmp)
     (setq ,keymap (make-sparse-keymap))
     
     (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
         (message "Theme: %s" ergoemacs-theme))
     ;; Fixed layout keys
     (mapc
      (lambda(x)
        (when (and (eq 'string (type-of (nth 0 x))))
          (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))
          (when (not (ergoemacs-global-changed-p trans-key))
            (setq cmd (nth 1 x))
            (condition-case err
                (setq key (read-kbd-macro
                           trans-key))
              (error
               (setq key (read-kbd-macro 
                          (encode-coding-string 
                           trans-key
                           locale-coding-system)))))
            (if (condition-case err
                    (keymapp (symbol-value cmd))
                  (error nil))
                (define-key ,keymap key (symbol-value cmd))
              (define-key ,keymap key cmd))
            (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                (message "Fixed: %s -> %s %s" trans-key cmd key)))))
       (symbol-value (ergoemacs-get-fixed-layout)))
     
     ;; Variable Layout Keys
     (mapc
      (lambda(x)
        (when (and (eq 'string (type-of (nth 0 x))))
          (setq trans-key
                (ergoemacs-get-kbd-translation (nth 0 x)))
          (when (or (string-match "<\\(apps\\|menu\\)>" trans-key)
                    (not (ergoemacs-global-changed-p trans-key t)))
            ;; Add M-O and M-o handling for globally defined M-O and
            ;; M-o.
            ;; Only works if ergoemacs-mode is on...
            (setq cmd (nth 1 x))
            (setq key (ergoemacs-kbd trans-key nil (nth 3 x)))
            (if (and ergoemacs-fix-M-O (string= (ergoemacs-kbd trans-key t t) "M-O"))
                (progn
                  (define-key ,keymap key  'ergoemacs-M-O)
                  (if (condition-case err
                          (keymapp (symbol-value cmd))
                        (error nil))
                      (define-key ergoemacs-M-O-keymap  [timeout] (symbol-value cmd))
                    (define-key ergoemacs-M-O-keymap  [timeout] cmd))
                  (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                      (message "Variable: %s (%s) -> %s %s via ergoemacs-M-O" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
              (if (and ergoemacs-fix-M-O
                       (string= (ergoemacs-kbd trans-key t t) "M-o"))
                  (progn
                    (define-key ,keymap key  'ergoemacs-M-o)
                    (if (condition-case err
                            (keymapp (symbol-value cmd))
                          (error nil))
                        (define-key ergoemacs-M-o-keymap  [timeout] (symbol-value cmd))
                      (define-key ergoemacs-M-o-keymap  [timeout] cmd))
                    (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                        (message "Variable: %s (%s) -> %s %s via ergoemacs-M-o" trans-key
                                 (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
                (when (string-match "<\\(apps\\|menu\\)>" trans-key)
                  ;; Retain globally defined <apps> or <menu> defines.
                  (setq cmd-tmp (lookup-key (current-global-map) key t))
                  (when (and ergoemacs-debug
                             (eq ',keymap 'ergoemacs-keymap))
                    (message "<apps>; %s -> cmd: %s; global cmd: %s" trans-key cmd cmd-tmp))
                  (if (functionp cmd-tmp)
                      (setq cmd cmd-tmp)
                    (when (and cmd-tmp
                               (or
                                (not (string-match "<menu>" trans-key))
                                (not (= cmd-tmp 1))
                                (not (eq (lookup-key (current-global-map) (read-kbd-macro "<menu>"))
                                         'execute-extended-command))))
                      (unless (and (string-match "<apps>" trans-key)
                                   (not (lookup-key (current-global-map) (read-kbd-macro "<apps>"))))
                        (setq cmd nil)))))
                (when cmd
                  (if (condition-case err
                          (keymapp (symbol-value cmd))
                        (error nil))
                      (define-key ,keymap  key (symbol-value cmd))
                    (define-key ,keymap  key cmd))
                  (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                      (message "Variable: %s (%s) -> %s %s" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key))))))))
      (symbol-value (ergoemacs-get-variable-layout)))
     (when ergoemacs-fix-M-O
       (let ((M-O (lookup-key ,keymap (read-kbd-macro "M-O")))
             (g-M-O (lookup-key global-map (read-kbd-macro "M-O")))
             (M-o (lookup-key ,keymap (read-kbd-macro "M-o")))
             (g-M-o (lookup-key global-map (read-kbd-macro "M-o"))))
         (when ergoemacs-debug
           (message "M-O %s; Global M-O: %s; M-o %s; Global M-o: %s" M-O g-M-O M-o g-M-o))
         (when (and (not (functionp M-O))
                    (functionp g-M-O))
           (when ergoemacs-debug
             (message "Fixed M-O"))
           (define-key ,keymap (read-kbd-macro "M-O") 'ergoemacs-M-O)
           (define-key ergoemacs-M-O-keymap [timeout] g-M-O))
         (when (and (not (functionp M-o))
                    (functionp g-M-o))
           (when ergoemacs-debug
             (message "Fixed M-o"))
           (define-key ,keymap (read-kbd-macro "M-o") 'ergoemacs-M-o)
           (define-key ergoemacs-M-o-keymap [timeout] g-M-o))))))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  (ergoemacs-setup-fast-keys)
  (ergoemacs-setup-keys-for-keymap ergoemacs-keymap)
  ;; Remove all overriding-map-alists...  I believe some are being
  ;; created out of context.  However, this seems to fix issue #134
  (mapc
   (lambda(buf)
     (save-excursion
       (set-buffer buf)
       (let ((x (assq 'ergoemacs-mode minor-mode-overriding-map-alist)))
         (if x
             (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist))))))
     (buffer-list))
  
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
      ["Generate Documentation"
       (lambda()
         (interactive)
         (call-interactively 'ergoemacs-extras)) t]
      ["Customize Ergoemacs"
       (lambda()
         (interactive)
         (customize-group 'ergoemacs-mode)) t]
      ["Save Settings for next session"
       (lambda()
         (interactive)
         (customize-save-variable 'ergoemacs-theme ergoemacs-theme)
         (customize-save-variable 'ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
         (customize-save-customized)) t]
      ["Exit ErgoEmacs"
       (lambda()
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
    (let* ((no-ergoemacs-advice t)
          (key-code
           (cond
            ((and translate (eq 'string (type-of key-def)))
             (ergoemacs-kbd key-def))
            ((eq 'string (type-of key-def))
             (condition-case err
                 (read-kbd-macro key-def)
               (error (read-kbd-macro
                       (encode-coding-string key-def locale-coding-system)))))
            ((ergoemacs-key-fn-lookup key-def)
             ;; Also define <apps> key
             (when (ergoemacs-key-fn-lookup key-def t)
               (define-key keymap (ergoemacs-key-fn-lookup key-def t) definition))
             (ergoemacs-key-fn-lookup key-def))
            ;; Define <apps>  key
            ((ergoemacs-key-fn-lookup key-def t)
             (ergoemacs-key-fn-lookup key-def t)
             nil)
            (t nil))))
        (when ergoemacs-debug
          (message "hook: %s->%s %s %s" key-def key-code
                   definition translate))
      (when key-code
        (define-key keymap key-code definition)))))

(defmacro ergoemacs-create-hook-function (hook keys &optional global)
  "Creates a hook function based on the HOOK and the list of KEYS defined."
  (let ((is-override (make-symbol "is-override"))
        (local-list '()))
    (setq is-override (eq 'minor-mode-overriding-map-alist (nth 2 (nth 0 keys))))
    `(progn
       ,(if is-override
            `(progn
               (defvar ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")) nil
                 ,(concat "Ergoemacs overriding keymap for `" (symbol-name hook) "'")))
          nil)
       (defun ,(intern (concat "ergoemacs-" (symbol-name hook))) ()
         ,(concat "Hook for `" (symbol-name hook) "' so ergoemacs keybindings are not lost.
This is an automatically generated function derived from `ergoemacs-get-minor-mode-layout'.")
         ,(if is-override
              `(ergoemacs-setup-keys-for-keymap ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
            nil)
         ,@(mapcar
            (lambda(def)
              `(progn
                 ,(if (and is-override (equal (nth 2 def) 'minor-mode-overriding-map-alist)) nil
                    (if (member (nth 2 def) local-list) nil
                      (add-to-list 'local-list (nth 2 def))
                      `(set (make-local-variable ',(nth 2 def)) ,(nth 2 def))))
                 (ergoemacs-hook-define-key ,(if (and is-override
                                                      (equal (nth 2 def)
                                                             'minor-mode-overriding-map-alist))
                                                 (intern (concat "ergoemacs-" (symbol-name hook) "-keymap"))
                                               (nth 2 def))
                                            ,(if (eq (type-of (nth 0 def)) 'string)
                                                 `,(nth 0 def)
                                               `(quote ,(nth 0 def)))
                                            ',(nth 1 def)
                                            ,(nth 3 def))))
            keys)
         ,(if is-override
              `(add-to-list 'minor-mode-overriding-map-alist
                            (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                            nil ,(if (equal hook 'minibuffer-setup-hook)
                                     '(lambda (x y)
                                        (equal (car y) (car x)))
                                   nil))
            nil)
         t)
       (ergoemacs-add-hook ',hook ',(intern (concat "ergoemacs-" (symbol-name hook)))))))

(defvar ergoemacs-hook-list (list)
"List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function)
  "Adds a pair of HOOK and HOOK-FUNCTION to the list `ergoemacs-hook-list'."
  (add-to-list 'ergoemacs-hook-list (cons hook hook-function)))

(defun ergoemacs-hook-modes ()
  "Installs/Removes ergoemacs minor mode hooks from major modes
depending the state of `ergoemacs-mode' variable.  If the mode
is being initialized, some global keybindings in current-global-map
will change."
  
  (let ((modify-hook (if (and (boundp 'ergoemacs-mode) ergoemacs-mode) 'add-hook 'remove-hook))
        (modify-advice (if (and (boundp 'ergoemacs-mode) ergoemacs-mode) 'ad-enable-advice 'ad-disable-advice)))
    
    
    ;; when ergoemacs-mode is on, activate hooks and unset global keys, else do inverse
    (if (and (boundp 'ergoemacs-mode) ergoemacs-mode (not (equal ergoemacs-mode 0)))
        (progn
          (ergoemacs-unset-redundant-global-keys)
          ;; alt+n is the new "Quit" in query-replace-map
          (when (ergoemacs-key-fn-lookup 'keyboard-quit)
            (ergoemacs-unset-global-key query-replace-map "\e")
            (define-key query-replace-map (ergoemacs-key-fn-lookup 'keyboard-quit) 'exit-prefix)))
      ;; if ergoemacs was disabled: restore original keys
      (ergoemacs-restore-global-keys))
    
    ;; install the mode-hooks
    (dolist (hook ergoemacs-hook-list)
      (funcall modify-hook (car hook) (cdr hook)))
    
    ;; enable advices
    (condition-case err
        (progn
          (funcall modify-advice 'global-set-key 'around 'ergoemacs-global-set-key-advice)
          (funcall modify-advice 'global-unset-key 'around 'ergoemacs-global-unset-key-advice)
          (funcall modify-advice 'local-set-key 'around 'ergoemacs-local-set-key-advice)
          (funcall modify-advice 'local-unset-key 'around 'ergoemacs-local-unset-key-advice)
          ;; update advices
          (ad-activate 'global-set-key)
          (ad-activate 'global-unset-key)
          (ad-activate 'local-set-key)
          (ad-activate 'local-unset-key))
      (error "Error modifying advices. %s" err))))

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
  (when ergoemacs-debug
    (message "Ergoemacs layout: %s" ergoemacs-keyboard-layout)
    (message "Ergoemacs theme: %s" ergoemacs-theme)
    (message "Emacs Version: %s" (emacs-version) ))
  (let ((ergoemacs-state (if (boundp 'ergoemacs-mode) ergoemacs-mode nil))
        (cua-state cua-mode)
        (layout
         (intern-soft
          (concat "ergoemacs-layout-" ergoemacs-keyboard-layout))))
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode -1)
          (when cua-state
            (cua-mode -1)))))
    (cond
     (layout
      (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout))
     (t ; US qwerty by default
      (ergoemacs-setup-keys-for-layout "us")))
    (ergoemacs-create-hooks)
    ;; (define-minor-mode ergoemacs-mode … )
    ;; 2012-12-15 Xah: problem: when calling describe-function on ergoemacs-mode, it will say “ergoemacs-mode is an interactive Lisp function in `.emacs.desktop'”. So comment3ed out this for now. What's the consequence of not updating keymap?
    (unless no-check
      (when ergoemacs-state
        (when (fboundp 'ergoemacs-mode)
          (ergoemacs-mode 1)
          (when cua-state
            (cua-mode 1)))))))

(defun ergoemacs-lookup-execute-extended-command ()
  "Lookup the execute-extended-command"
  (key-description
   (or (ergoemacs-key-fn-lookup 'execute-extended-command)
       (ergoemacs-key-fn-lookup 'smex)
       (ergoemacs-key-fn-lookup 'smex-if-exists)
       (ergoemacs-key-fn-lookup 'ergoemacs-smex-if-exists))))

(defvar ergoemacs-ctl-x-unchorded nil
  "Keymap for unchorded 【Ctl+x】 combinations.")

(defvar ergoemacs-ctl-x nil
  "Keymap for 【Ctl+x】combinations.  C-x C- are mapped to C-x M-")

(defvar ergoemacs-ctl-h nil
  "Keymap for 【Ctl+h】 combinations.")

(defvar ergoemacs-ctl-h-unchorded nil
  "Keymap for unchorded 【Ctl+h】 combinations.")

(defmacro ergoemacs-extract-map (keymap &optional prefix chord rep-chord new-chord)
  "Takes out the key-chords from the buffer-defined map.
If Prefix is nil assume C-x.
If chord is nil, assume C-
If new-chord is nil, assume M-

If chord is not an empty string and chorded is nil, then all
control sequences will be translate as follows:

Control characters will be translated to normal characters.
Normal characters will be translated to new-chord prefixed characters.
new-chord prefixed characters will be translated to the old chord.

For example for the C-x map,

Original Key   Translated Key  Function
C-k C-n     -> k n             (kmacro-cycle-ring-next)
C-k a       -> k M-a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

If prefix is an empty string extract the map and remove the prefix.

If rep-chord is non-nil, like M- instead these same translations would be:

C-k C-n     -> M-k M-n             (kmacro-cycle-ring-next)
C-k a       -> M-k M-a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

"
  `(let ((ret "")
         (buf (current-buffer))
         (ergoemacs-current-prefix (or ,prefix "C-x"))
         (new-key "")
         (fn "")
         (chord (or ,chord "C-"))
         (rep-chord (or ,rep-chord ""))
         (new-chord (or ,new-chord "M-")))
     
     (setq ,keymap (make-keymap))
     
     (with-temp-buffer
       (when ergoemacs-debug
         (message "Current prefix: %s" ergoemacs-current-prefix))
       (describe-buffer-bindings buf (read-kbd-macro ergoemacs-current-prefix))
       (goto-char (point-min))
       
       (while (re-search-forward
               (concat ergoemacs-current-prefix " \\("
                       (if (string= "" rep-chord)
                           chord
                         "") ".*?\\)[ \t]\\{2,\\}\\(.+\\)$")
               nil t)
         (setq new-key (match-string 1))
         (setq fn (match-string 2))
         (condition-case err
             (with-temp-buffer
               (insert "(setq fn '" fn ")")
               (eval-buffer))
           (error (setq fn nil)))
         (save-match-data
           (unless (string= chord "")
             (with-temp-buffer
               (insert new-key)
               (goto-char (point-min))
               (while (re-search-forward "\\<" nil t)
                 (if (looking-at chord)
                     (replace-match rep-chord)
                   (if (or (and (not (string= "" new-chord))
                                (looking-at new-chord))
                           (and (not (string= "" rep-chord))
                                (looking-at rep-chord)))
                       (replace-match chord)
                     (if (not (looking-at ".-"))
                         (insert new-chord))))
                 (forward-char))
               (setq new-key (buffer-string)))))
         (unless (or (string= new-key "")
                     (not fn)
                     (eq fn 'Prefix))
           (when ergoemacs-debug
             (message "Translate: %s -> %s (%s)" (match-string 1) new-key fn))
           (condition-case err
               (define-key ,keymap (kbd new-key) fn)
             (error
              (when ergoemacs-debug
                (message "Error defining %s: %s" new-key err)))))))))

(defun ergoemacs-ctl-c-unchorded (&optional arg)
  "Creates a keymap for the current major mode that extract the unchorded 【Ctl+c】 combinations."
  (interactive "P")
  (setq prefix-arg current-prefix-arg)
  (let ((ctl-c-unchorded (make-keymap))
        (ctl-c-unchorded-1 (make-sparse-keymap)))
    (ergoemacs-extract-map ctl-c-unchorded "C-c")
    (define-key ctl-c-unchorded-1 (kbd "C-c *") ctl-c-unchorded)
    (set-temporary-overlay-map ctl-c-unchorded-1)
    (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c *")))
    (message "C-c *-")))

(defun ergoemacs-ctl-c (&optional arg)
  "Creates a keymap for the current major mode that extract the 【Ctl+c】 combinations."
  (interactive "P")
  (setq prefix-arg current-prefix-arg)
  (let ((ctl-c (make-keymap))
        (ctl-c-1 (make-sparse-keymap)))
    (ergoemacs-extract-map ctl-c "C-c" "C-" "M-" "")
    (define-key ctl-c-1 (kbd "C-c") ctl-c)
    (set-temporary-overlay-map ctl-c-1)
    (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c")))
    (message "C-c-")))

(defcustom ergoemacs-repeat-ctl-c-ctl-c t
  "Allow [Ctl+c] [Ctl+c] to be repeated."
  :group 'ergoemacs-mode
  :type 'boolean)

(defvar ergoemacs-repeat-ctl-c-ctl-c-keymap (make-keymap)
  "Keymap for repeating Ctl-c Ctl-c.")

(defvar ergoemacs-repeat-ctl-c-ctl-c-msg ""
  "Message for repeating [Ctl-c] [Ctl-c]")

(defun ergoemacs-ctl-c-ctl-c-timeout ()
  (message ergoemacs-repeat-ctl-c-ctl-c-msg)
  (set-temporary-overlay-map ergoemacs-repeat-ctl-c-ctl-c-keymap))

(defun ergoemacs-ctl-c-ctl-c (&optional arg)
  "Creates a function that looks up and binds 【Ctl+c】 【Ctl+c】.
Optionally binds a temporary repeat keymap when `ergoemacs-repeat-ctl-c-ctl-c' is non-nil"
  (interactive "P")
  (let ((ctl-c-keys (key-description (this-command-keys))))
    (setq prefix-arg current-prefix-arg)
    (setq unread-command-events (listify-key-sequence (read-kbd-macro "C-c C-c")))
    (when ergoemacs-repeat-ctl-c-ctl-c
      (when (and (key-binding (read-kbd-macro "C-c C-c"))
                 (string-match "[A-Za-z]$" ctl-c-keys))
        (setq ctl-c-keys (match-string 0 ctl-c-keys))
        (setq ergoemacs-repeat-ctl-c-ctl-c-keymap (make-keymap))
        (define-key ergoemacs-repeat-ctl-c-ctl-c-keymap (read-kbd-macro ctl-c-keys)
          'ergoemacs-ctl-c-ctl-c)
        (setq ergoemacs-repeat-ctl-c-ctl-c-msg
              (format "Repeat 【Ctl+c】 【Ctl+c】 with `%s'"
                      ctl-c-keys))
        ;; Allow time to process the unread command events before
        ;; installing temporary keymap
        (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-ctl-c-ctl-c-timeout)))))


(require 'cus-edit)

(defun ergoemacs-check-for-new-version ()
  "This allows the user to keep an old-version of keybindings if they change."
  (condition-case err
      (progn
        (when ergoemacs-mode
          ;; Apply any settings...
          (when ergoemacs-debug
            (message "Reset ergoemacs-mode."))
          (ergoemacs-mode -1)
          (ergoemacs-mode 1))
         (when (and
               (custom-file t) ;; Make sure a custom file exists.
               (not ergoemacs-theme) ;; Ergoemacs default used.
               (or (not ergoemacs-mode-used)
                   (not (string= ergoemacs-mode-used ergoemacs-mode-version))))
          (if (yes-or-no-p (format "Ergoemacs keybindings changed, %s; Would you like to change as well?"
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
;; ErgoEmacs minor mode
;;;###autoload
(define-minor-mode ergoemacs-mode
  "Toggle ergoemacs keybinding minor mode.
This minor mode changes your emacs keybinding.

Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.

Home page URL `http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html'

For the standard layout, with A QWERTY keyboard the `execute-extended-command' 【Alt+x】 is now 【Alt+a】 or the PC keyboard's 【Menu】 key."
  nil
  :lighter " ErgoEmacs"
  :global t
  :group 'ergoemacs-mode
  :keymap ergoemacs-keymap
  
  (ergoemacs-setup-keys t)
  (when ergoemacs-debug
    (message "Ergoemacs Keys have loaded."))
  (if ergoemacs-mode
      (define-key cua--cua-keys-keymap (read-kbd-macro "M-v") nil)
    (define-key cua--cua-keys-keymap (read-kbd-macro "M-v") 'cua-repeat-replace-region))
  (condition-case err
      (when ergoemacs-cua-rect-modifier
        (if ergoemacs-mode
            (progn
              (ergoemacs-extract-map ergoemacs-ctl-x-unchorded)
              (ergoemacs-extract-map ergoemacs-ctl-x "C-x" "C-" "M-" "")
              (ergoemacs-extract-map ergoemacs-ctl-h-unchorded "C-h")
              (ergoemacs-extract-map ergoemacs-ctl-h "C-h" "C-" "M-" "")
              
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
        (when ergoemacs-debug
          (message "CUA rectangle mode modifier changed.")))
    (error (message "CUA rectangle modifier wasn't changed.")))
  
  (when ergoemacs-change-smex-M-x
    (if ergoemacs-mode
        (setq smex-prompt-string (concat (ergoemacs-pretty-key "M-x") " "))
      (setq smex-promt-string "M-x "))))



;; ErgoEmacs replacements for local-set-key

(defadvice define-key (around ergoemacs-define-key-advice (keymap key def))
  "This does the right thing when modifying `ergoemacs-keymap'"
  (if (and (equal keymap 'ergoemacs-keymap)
           (or (not (boundp 'no-ergoemacs-advice))
               (and (boundp 'no-ergoemacs-advice) (not no-ergoemacs-advice))))
      (progn
        (message "Define Key Advice %s %s" key def)
        (let ((found))
          (set (ergoemacs-get-fixed-layout)
               (mapcar
                (lambda(x)
                  (if (not (or (and (type-of (nth 0 x) 'string)
                                    (string= (key-description
                                              (condition-case err
                                                  (read-kbd-macro (nth 0 x))
                                                (error
                                                 (read-kbd-macro (encode-coding-string (nth 0 x) locale-coding-system)))))
                                             (key-description key)))
                               (and (not (type-of (nth 0 x) 'string))
                                    (string= (key-description (nth 0 x)) (key-description key)))))
                      x
                    (setq found t)
                    `(,(nth 0 x) ,command "")))
                (symbol-value (ergoemacs-get-fixed-layout))))
          (unless found
            (set (ergoemacs-get-variable-layout)
                 (mapcar
                  (lambda(x)
                    (if (not (and (type-of (nth 0 x) 'string)
                                  (string= (key-description (ergoemacs-kbd (nth 0 x) nil (nth 3 x))) (key-description key))))
                        x
                      (setq found t)
                      ;; Assume the complete sequence is translated?
                      `(,(nth 0 x) ,command "")))
                  (symbol-value (ergoemacs-get-variable-layout)))))
          (unless found
            (add-to-list (ergoemacs-get-fixed-layout) `(,(key-description key) ,command ""))))
        (message "Only changed ergoemacs-keybinding for current theme, %s" (or ergoemacs-theme "which happens to be the default key-binding"))
        (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
          (let ((no-ergoemacs-advice t))
            (define-key ergoemacs-keymap key def))))
    ad-do-it))

(ad-activate 'define-key)

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


;; ErgoEmacs advices for local-set-key

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use local-set-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))


(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use local-unset-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

(defadvice global-set-key (around ergoemacs-global-set-key-advice (key command))
  "This let you use global-set-key as usual when ergoemacs-mode is enabled."
  ad-do-it
  (add-to-list 'ergoemacs-do-not-restore-list (key-description key))
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (if (string-match "<\\(apps\\|menu\\)>" (key-description key))
      (let ((no-ergoemacs-advice t))
        (define-key ergoemacs-keymap key nil);; Take care of prefix
        ;; commands.
        (define-key ergoemacs-keymap key command))
    (if (and ergoemacs-fix-M-O
             (string= "M-O" (key-description key)))
        (let ((no-ergoemacs-advice t))
          (define-key ergoemacs-keymap key 'ergoemacs-M-O)
          (define-key ergoemacs-M-O-keymap [timeout] command))
      (if (and ergoemacs-fix-M-O
               (string= "M-o" (key-description key)))
          (let ((no-ergoemacs-advice t))
            (define-key ergoemacs-keymap key 'ergoemacs-M-o)
            (define-key ergoemacs-M-o-keymap [timeout] command)))
      (let ((no-ergoemacs-advice t))
        (define-key ergoemacs-keymap key nil)))))

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use global-unset-key as usual when ergoemacs-mode is enabled."
  ;; the global-unset-key will remove the key from ergoemacs as well.
  ad-do-it
  (add-to-list 'ergoemacs-do-not-restore-list (key-description key))
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (let ((no-ergoemacs-advice t))
    (define-key ergoemacs-keymap key nil)))

;; Org edit source bug fix to allow C-s to save the org file in a
;; source snippet.

(eval-after-load "org-src"
  '(progn
     (define-key org-src-mode-map [remap save-buffer] 'org-edit-src-save)))

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
