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
;; Package-Requires: ((org-cua-dwim "0.5"))

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

; (eval-when-compile (require 'cl))
; FIXME: Use cl-lib when available.
(require 'cl)
(require 'easymenu)
(require 'cua-base)
(require 'cua-rect)


;; Include extra files
(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)


(require 'ergoemacs-layouts)

(require 'org-cua-dwim nil "NOERROR")


(when (featurep 'org-cua-dwim)
  (org-cua-dwim-activate))

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
  '(scroll-down move-beginning-of-line move-end-of-line scroll-up scroll-down forward-block backward-block
                forward-word backward-word next-line previous-line forward-char backward-char
                ergoemacs-backward-block ergoemacs-forward-block ergoemacs-backward-open-bracket
                ergoemacs-forward-close-bracket move-end-of-line move-beginning-of-line backward-word forward-word
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

(defvar ergoemacs-full-alt-shift-keymap (make-keymap)
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
  (set-temporary-overlay-map ergoemacs-full-alt-shift-keymap
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
       (error (when ergoemacs-debug
                (message "Ignored backward compatability for %s" (nth 1 var))))))
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

(defvar ergoemacs-debug (if (getenv "ERGOEMACS_DEBUG")
                            t
                          nil)
  "Debugging variable for ergoemacs.  Set to t to see debugging information in messages.")

(defun ergoemacs-setup-keys-for-keymap---internal (keymap key def)
  "Defines KEY in KEYMAP to be DEF"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (mapc
       (lambda(new-def)
         (unless found
           (setq found
                 (ergoemacs-setup-keys-for-keymap---internal keymap key new-def))))
       def)
      (symbol-value 'found)))
   ((condition-case err
        (fboundp def)
      (error nil))
    (define-key keymap key def)
    t)
   ((condition-case err
        (keymapp (symbol-value def))
      (error nil))
    (define-key keymap key (symbol-value def))
    t)
   ((condition-case err
	(stringp def)
      (error nil))
    (eval (macroexpand `(progn
                          (ergoemacs-keyboard-shortcut
                           ,(intern (concat "ergoemacs-shortcut---"
                                            (md5 (format "%s" def)))) ,def)
                          (define-key keymap key
                            ',(intern (concat "ergoemacs-shortcut---"
                                              (md5 (format "%s" def))))))))
    t)
   (t nil)))

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
	    (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                (message "Fixed: %s -> %s %s" trans-key cmd key))
            (when (and (not (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd)) ergoemacs-debug)
	      (message "Not loaded")))))
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
                  (ergoemacs-setup-keys-for-keymap---internal ergoemacs-M-O-keymap [timeout] cmd)
                  (if (and ergoemacs-debug (eq ',keymap 'ergoemacs-keymap))
                      (message "Variable: %s (%s) -> %s %s via ergoemacs-M-O" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
              (if (and ergoemacs-fix-M-O
                       (string= (ergoemacs-kbd trans-key t t) "M-o"))
                  (progn
                    (define-key ,keymap key  'ergoemacs-M-o)
                    (ergoemacs-setup-keys-for-keymap---internal ergoemacs-M-o-keymap [timeout] cmd)
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
                  (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd)
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
        (eval (macroexpand `(progn
                              (ergoemacs-keyboard-shortcut
                               ,(intern (concat "ergoemacs-shortcut---"
                                                (md5 (format "%s" definition)))) ,definition)
                              (setq fn ',(intern (concat "ergoemacs-shortcut---"
                                                         (md5 (format "%s" definition)))))))))
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
          (when ergoemacs-debug
            (message "hook: %s->%s %s %s" key-def key-code
                     fn translate))
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
                `(add-to-list 'minor-mode-overriding-map-alist
                              (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                              nil ,(if (equal hook 'minibuffer-setup-hook)
                                       '(lambda (x y)
                                          (equal (car y) (car x)))
                                     nil))
              nil)
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
          (ergoemacs-unset-redundant-global-keys)
          ;; alt+n is the new "Quit" in query-replace-map
          (when (ergoemacs-key-fn-lookup 'keyboard-quit)
            (ergoemacs-unset-global-key query-replace-map "\e")
            (define-key query-replace-map (ergoemacs-key-fn-lookup 'keyboard-quit) 'exit-prefix)))
      ;; if ergoemacs was disabled: restore original keys
      (ergoemacs-restore-global-keys))
    
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
       (ergoemacs-key-fn-lookup 'helm-M-x))))

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
C-k a       -> M-k a           (kmacro-add-counter)
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

(defvar ergoemacs-repeat-shortcut-keymap (make-keymap)
  "Keymap for repeating often used shortcuts like C-c C-c.")

(defvar ergoemacs-repeat-shortcut-msg ""
  "Message for repeating keyboard shortcuts like C-c C-c")

(defun ergoemacs-shortcut-timeout ()
  (message ergoemacs-repeat-shortcut-msg)
  (set-temporary-overlay-map ergoemacs-repeat-shortcut-keymap))


;;;###autoload
(defmacro ergoemacs-keyboard-shortcut (name key &optional chorded repeat)
  "Creates a function NAME that issues a keyboard shortcut for KEY.
CHORDED is a variable that alters to keymap to allow unchorded
key sequences.

If CHORDED is nil, the NAME command will just issue the KEY sequence.

If CHORDED is 'ctl or the NAME command will translate the control
bindings to be unchorded.  For example:

For example for the C-x map,

Original Key   Translated Key  Function
C-k C-n     -> k n             (kmacro-cycle-ring-next)
C-k a       -> k M-a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

If CHORDED is 'ctl-to-alt or the NAME command will translate the control
bindings to be unchorded.  For example:

C-k C-n     -> M-k M-n             (kmacro-cycle-ring-next)
C-k a       -> M-k a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

When REPEAT is a variable name, then an easy repeat is setup for the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows Ctrl+c Ctrl+c to be repeated by m.
"
  `(progn
     ,(cond
       ((eq chorded 'ctl))
       ((eq chorded 'ctl-to-alt))
       (t
        (when repeat
            `(defcustom ,(intern (symbol-name repeat)) t
               ,(format "Allow %s to be repeated." (ergoemacs-pretty-key key))
               :group 'ergoemacs-mode
               :type 'boolean))))
     (defun ,(intern (symbol-name name)) (&optional arg)
       ,(cond
         ((eq chorded 'ctl)
          (format "Creates a keymap that extracts the unchorded %s combinations and then issues %s" (ergoemacs-pretty-key key) (ergoemacs-pretty-key key)))
         ((eq chorded 'ctl-to-alt)
          (format "Creates a keymap that extracts the %s combinations and translates Ctl+ to Alt+." (ergoemacs-pretty-key key)))
         (t
          (format "A shortcut to %s." (ergoemacs-pretty-key key))))
       (interactive "P")
       (setq this-command last-command) ; Don't record this command.
       (setq prefix-arg current-prefix-arg)
       (let (extract-map extract-map-1 key-seq)
         ,(cond
           ((eq chorded 'ctl)
            `(progn
               (setq extract-map (make-keymap))
               (setq extract-map-1 (make-sparse-keymap))
               (ergoemacs-extract-map extract-map ,key)
               (setq key-seq  (read-kbd-macro ,(format "<Unchorded> %s" key)))
               (define-key extract-map-1 key-seq extract-map)
               (set-temporary-overlay-map extract-map-1)
               (setq key-seq (listify-key-sequence key-seq))
               (reset-this-command-lengths)
               (setq unread-command-events key-seq)
               (message ,(format "<Unchorded> %s " (ergoemacs-pretty-key key)))))
           ((eq chorded 'ctl-to-alt)
            `(progn
               (setq extract-map (make-keymap))
               (setq extract-map-1 (make-sparse-keymap))
               (ergoemacs-extract-map extract-map ,key "C-" "M-" "")
               (setq key-seq (read-kbd-macro ,(format "<Ctl→Alt> %s" key)))
               (define-key extract-map-1 key-seq extract-map)
               (setq key-seq (listify-key-sequence key-seq))
               (set-temporary-overlay-map extract-map-1)
               (reset-this-command-lengths)
               (setq unread-command-events key-seq)
               (message ,(format "<Ctl→Alt> %s " (ergoemacs-pretty-key key)))))
           (t
            `(let ((ctl-c-keys (key-description (this-command-keys))))
               (setq prefix-arg current-prefix-arg)
               (reset-this-command-lengths)
               (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))
               ,(when repeat
                  `(when ,(intern (symbol-name repeat))
                     (when (and (key-binding (read-kbd-macro ,key))
                                (string-match "[A-Za-z]$" ctl-c-keys))
                       (setq ctl-c-keys (match-string 0 ctl-c-keys))
                       (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
                       (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys)
                         'ergoemacs-ctl-c-ctl-c)
                       (setq ergoemacs-repeat-shortcut-msg
                             (format ,(format "Repeat %s with %%s" (ergoemacs-pretty-key key))
                                     (ergoemacs-pretty-key ctl-c-keys)))
                       ;; Allow time to process the unread command events before
                       ;; installing temporary keymap
                       (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-shortcut-timeout)))))))))))

(ergoemacs-keyboard-shortcut ergoemacs-ctl-c-unchorded "C-c" ctl)
(ergoemacs-keyboard-shortcut ergoemacs-ctl-c-ctl-to-alt "C-c" ctl-to-alt)
(ergoemacs-keyboard-shortcut ergoemacs-ctl-x-unchorded "C-x" ctl)
(ergoemacs-keyboard-shortcut ergoemacs-ctl-x-ctl-to-alt "C-x" ctl-to-alt)
(ergoemacs-keyboard-shortcut ergoemacs-ctl-h-ctl-to-alt "C-h" ctl-to-alt)
(ergoemacs-keyboard-shortcut ergoemacs-ctl-c-ctl-c "C-c C-c" nil ergoemacs-repeat-ctl-c-ctl-c)


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
  (ergoemacs-setup-keys t)
  (when ergoemacs-debug
    (message "Ergoemacs Keys have loaded."))
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
        (when ergoemacs-debug
          (message "CUA rectangle mode modifier changed.")))
    (error (message "CUA rectangle modifier wasn't changed.")))
  
  (when ergoemacs-change-smex-M-x
    (if ergoemacs-mode
        (setq smex-prompt-string (concat (ergoemacs-pretty-key "M-x") " "))
      (setq smex-promt-string "M-x ")))
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
               (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist))))))
     (buffer-list))))



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

(when ergoemacs-ignore-prev-global
  (ergoemacs-ignore-prev-global))

;;; Frequently used commands as aliases

(defcustom ergoemacs-use-aliases t
  "Use aliases defined by `ergoemacs-aliases' to abbreviate commonly used commands.
Depending on how you use the completion engines, this may or may not be useful.
However instead of using M-a `eval-buffer', you could use M-a `eb'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-aliases
  '((ar    align-regexp)
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
    (sl    sort-lines))
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

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
