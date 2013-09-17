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


(defvar ergoemacs-M-O-trans
  '(
    ("2 A" [S-up])        ; xterm.el
    ("2 B" [S-down])      ; xterm.el
    ("2 C" [S-right])     ; xterm.el
    ("2 D" [S-left])      ; xterm.el
    ("2 F" [S-end])       ; xterm.el
    ("2 H" [S-home])      ; xterm.el
    ("2 P" [S-f1])        ; xterm.el
    ("2 Q" [S-f2])        ; xterm.el
    ("2 R" [S-f3])        ; xterm.el
    ("2 S" [S-f4])        ; xterm.el
    ("3 P" [M-f1])        ; xterm.el
    ("3 Q" [M-f2])        ; xterm.el
    ("3 R" [M-f3])        ; xterm.el
    ("3 S" [M-f4])        ; xterm.el
    ("4 P" [M-S-f1])      ; xterm.el
    ("4 Q" [M-S-f2])      ; xterm.el
    ("4 R" [M-S-f3])      ; xterm.el
    ("4 S" [M-S-f4])      ; xterm.el
    ("5 A" [C-up])        ; xterm.el
    ("5 B" [C-down])      ; xterm.el
    ("5 C" [C-right])     ; xterm.el
    ("5 D" [C-left])      ; xterm.el
    ("5 F" [C-end])       ; xterm.el
    ("5 H" [C-home])      ; xterm.el
    ("5 P" [C-f1])        ; xterm.el
    ("5 Q" [C-f2])        ; xterm.el
    ("5 R" [C-f3])        ; xterm.el
    ("5 S" [C-f4])        ; xterm.el
    ("6 P" [C-S-f1])      ; xterm.el
    ("6 Q" [C-S-f2])      ; xterm.el
    ("6 R" [C-S-f3])      ; xterm.el
    ("6 S" [C-S-f4])      ; xterm.el
    ("A" up)             ; xterm.el
    ("B" down)           ; xterm.el
    ("C" right)          ; xterm.el
    ("D" left)           ; xterm.el
    ("E" begin)          ; xterm.el
    ("F" end)            ; xterm.el
    ("H" home)           ; xterm.el
    ("I" [kp-tab])       ; lk201.el
    ("M" kp-enter)       ; xterm.el
    ("P" f1)             ; xterm.el
    ("Q" f2)             ; xterm.el
    ("R" f3)             ; xterm.el
    ("S" f4)             ; xterm.el
    ("a" [C-up])         ; rxvt.el
    ("b" [C-down])       ; rxvt.el
    ("c" [C-right])      ; rxvt.el
    ("d" [C-left])       ; rxvt.el
    ("j" [kp-multiply])  ; xterm.el
    ("k" [kp-add])       ; xterm.el
    ("l" [kp-separator]) ; xterm.el
    ("m" [kp-subtract])  ; xterm.el
    ("n" [kp-decimal])   ; lk201.el
    ("o" [kp-divide])    ; xterm.el
    ("p" [kp-0])         ; xterm.el
    ("q" [kp-1])         ; xterm.el
    ("r" [kp-2])         ; xterm.el
    ("s" [kp-3])         ; xterm.el
    ("t" [kp-4])         ; xterm.el
    ("u" [kp-5])         ; xterm.el
    ("v" [kp-6])         ; xterm.el
    ("w" [kp-7])         ; xterm.el
    ("x" [kp-8])         ; xterm.el
    ("y" [kp-9])         ; xterm.el
    )
  "Terminal Translations.")

(defvar ergoemacs-M-b-translations
  '(
    ("0 0 0 q" [begin]) ; iris-ansi
    ("0 0 1 q" [f1]) ; iris-ansi
    ("0 0 2 q" [f2]) ; iris-ansi
    ("0 0 3 q" [f3]) ; iris-ansi
    ("0 0 4 q" [f4]) ; iris-ansi
    ("0 0 5 q" [f5]) ; iris-ansi
    ("0 0 6 q" [f6]) ; iris-ansi
    ("0 0 7 q" [f7]) ; iris-ansi
    ("0 0 8 q" [f8]) ; iris-ansi
    ("0 0 9 q" [f9]) ; iris-ansi
    ("0 1 0 q" [f10]) ; iris-ansi
    ("0 1 1 q" [f11]) ; iris-ansi
    ("0 1 2 q" [f12]) ; iris-ansi
    ("0 1 3 q" [S-f1]) ; iris-ansi
    ("0 1 4 q" [S-f2]) ; iris-ansi
    ("0 1 5 q" [S-f3]) ; iris-ansi
    ("0 1 6 q" [S-f4]) ; iris-ansi
    ("0 1 7 q" [S-f5]) ; iris-ansi
    ("0 1 8 q" [S-f6]) ; iris-ansi
    ("0 1 9 q" [S-f7]) ; iris-ansi
    ("0 2 0 q" [S-f8]) ; iris-ansi
    ("0 2 1 q" [S-f9]) ; iris-ansi
    ("0 2 2 q" [S-f10]) ; iris-ansi
    ("0 2 3 q" [S-f11]) ; iris-ansi
    ("0 2 4 q" [S-f12]) ; iris-ansi
    ("0 2 5 q" [C-f1]) ; iris-ansi
    ("0 2 6 q" [C-f2]) ; iris-ansi
    ("0 2 7 q" [C-f3]) ; iris-ansi
    ("0 2 8 q" [C-f4]) ; iris-ansi
    ("0 2 9 q" [C-f5]) ; iris-ansi
    ("0 3 0 q" [C-f6]) ; iris-ansi
    ("0 3 1 q" [C-f7]) ; iris-ansi
    ("0 3 2 q" [C-f8]) ; iris-ansi
    ("0 3 3 q" [C-f9]) ; iris-ansi
    ("0 3 4 q" [C-f10]) ; iris-ansi
    ("0 3 5 q" [C-f11]) ; iris-ansi
    ("0 3 6 q" [C-f12]) ; iris-ansi
    ("0 3 8 q" [M-f2]) ; iris-ansi
    ("0 4 7 q" [M-f11]) ; iris-ansi
    ("0 4 8 q" [M-f12]) ; iris-ansi
    ("0 4 9 q" [?\C-1]) ; iris-ansi
    ("0 5 0 q" [?\C-3]) ; iris-ansi
    ("0 5 1 q" [?\C-4]) ; iris-ansi
    ("0 5 2 q" [?\C-5]) ; iris-ansi
    ("0 5 3 q" [?\C-7]) ; iris-ansi
    ("0 5 4 q" [?\C-8]) ; iris-ansi
    ("0 5 5 q" [?\C-9]) ; iris-ansi
    ("0 5 6 q" [?\C-0]) ; iris-ansi
    ("0 5 7 q" [?\C-`]) ; iris-ansi
    ("0 5 8 q" [?\M-1]) ; iris-ansi
    ("0 5 9 q" [?\M-2]) ; iris-ansi
    ("0 6 0 q" [?\M-3]) ; iris-ansi
    ("0 6 1 q" [?\M-4]) ; iris-ansi
    ("0 6 2 q" [?\M-5]) ; iris-ansi
    ("0 6 3 q" [?\M-6]) ; iris-ansi
    ("0 6 4 q" [?\M-7]) ; iris-ansi
    ("0 6 5 q" [?\M-8]) ; iris-ansi
    ("0 6 6 q" [?\M-9]) ; iris-ansi
    ("0 6 7 q" [?\M-0]) ; iris-ansi
    ("0 6 8 q" [?\M--]) ; iris-ansi
    ("0 6 9 q" [?\C-=]) ; iris-ansi
    ("0 7 0 q" [?\M-=]) ; iris-ansi
    ("0 7 2 q" [?\C-\t]) ; iris-ansi
    ("0 7 3 q" [?\M-\t]) ; iris-ansi
    ("0 7 4 q" [?\M-q]) ; iris-ansi
    ("0 7 5 q" [?\M-w]) ; iris-ansi
    ("0 7 6 q" [?\M-e]) ; iris-ansi
    ("0 7 7 q" [?\M-r]) ; iris-ansi
    ("0 7 8 q" [?\M-t]) ; iris-ansi
    ("0 7 9 q" [?\M-y]) ; iris-ansi
    ("0 8 0 q" [?\M-u]) ; iris-ansi
    ("0 8 1 q" [?\M-i]) ; iris-ansi
    ("0 8 2 q" [?\M-o]) ; iris-ansi
    ("0 8 3 q" [?\M-p]) ; iris-ansi
    ("0 8 4 q" [?\M-\[]) ; iris-ansi
    ("0 8 5 q" [?\M-\]]) ; iris-ansi
    ("0 8 6 q" [?\M-\\]) ; iris-ansi
    ("0 8 7 q" [?\M-a]) ; iris-ansi
    ("0 8 8 q" [?\M-s]) ; iris-ansi
    ("0 8 9 q" [?\M-d]) ; iris-ansi
    ("0 9 0 q" [?\M-f]) ; iris-ansi
    ("0 9 1 q" [?\M-g]) ; iris-ansi
    ("0 9 2 q" [?\M-h]) ; iris-ansi
    ("0 9 3 q" [?\M-j]) ; iris-ansi
    ("0 9 4 q" [?\M-k]) ; iris-ansi
    ("0 9 5 q" [?\M-l]) ; iris-ansi
    ("0 9 6 q" [?\C-\;]) ; iris-ansi
    ("0 9 7 q" [?\M-:]) ; iris-ansi
    ("0 9 8 q" [?\C-']) ; iris-ansi
    ("0 9 9 q" [?\M-']) ; iris-ansi
    ("1 0 0 q" [?\M-\n]) ; iris-ansi
    ("1 0 0 q" [M-enter]) ; iris-ansi
    ("1 0 1 q" [?\M-z]) ; iris-ansi
    ("1 0 2 q" [?\M-x]) ; iris-ansi
    ("1 0 3 q" [?\M-c]) ; iris-ansi
    ("1 0 4 q" [?\M-v]) ; iris-ansi
    ("1 0 5 q" [?\M-b]) ; iris-ansi
    ("1 0 6 q" [M-n]) ; iris-ansi
    ("1 0 7 q" [M-m]) ; iris-ansi
    ("1 0 8 q" [?\C-,]) ; iris-ansi
    ("1 0 9 q" [?\M-,]) ; iris-ansi
    ("1 1 0 q" [?\C-.]) ; iris-ansi
    ("1 1 1 q" [?\M-.]) ; iris-ansi
    ("1 1 2 q" [?\C-/]) ; iris-ansi
    ("1 1 3 q" [?\M-/]) ; iris-ansi
    ("1 1 5 q" [?\M-`]) ; iris-ansi
    ("1 1 ^" [C-f1]) ; rxvt
    ("1 1 ~" [f1])
    ("1 2 0 q" [S-escape]) ; iris-ansi
    ("1 2 1 q" [C-escape]) ; iris-ansi
    ("1 2 ^" [C-f2]) ; rxvt
    ("1 2 ~" [f2])
    ("1 3 9 q" [insert]) ; iris-ansi ;; Not sure 
    ("1 3 ^" [C-f3]) ; rxvt
    ("1 3 ~" [f3])
    ("1 4 0 q" [C-insert]) ; iris-ansi
    ("1 4 1 q" [M-insert]) ; iris-ansi
    ("1 4 2 q" [C-delete]) ; iris-ansi
    ("1 4 3 q" [S-home]) ; iris-ansi
    ("1 4 4 q" [C-home]) ; iris-ansi
    ("1 4 6 q" [end]) ; iris-ansi
    ("1 4 7 q" [S-end]) ; Those don't seem to generate anything. ; iris-ansi
    ("1 4 8 q" [C-end]) ; iris-ansi
    ("1 4 ^" [C-f4]) ; rxvt
    ("1 4 ~" [f4])
    ("1 4 ~" [f4]) ; lk201
    ("1 4 ~" [f4]) ; rxvt
    ("1 5 0 q" [prior]) ; iris-ansi
    ("1 5 1 q" [S-prior]) ;Those don't seem to generate anything. ; iris-ansi
    ("1 5 2 q" [C-prior]) ; iris-ansi
    ("1 5 4 q" [next]) ; iris-ansi
    ("1 5 5 q" [S-next]) ; iris-ansi
    ("1 5 6 q" [C-next]) ; iris-ansi
    ("1 5 8 q" [S-left]) ; iris-ansi
    ("1 5 9 q" [C-left]) ; iris-ansi
    ("1 5 ; 2 ~" [S-f5])
    ("1 5 ; 3 ~" [M-f5])
    ("1 5 ; 4 ~" [M-S-f5])
    ("1 5 ; 6 ~" [C-S-f5])
    ("1 5 ^" [C-f5]) ; rxvt
    ("1 5 ~" [f5])
    ("1 6 0 q" [M-left]) ; iris-ansi
    ("1 6 1 q" [S-up]) ; iris-ansi
    ("1 6 2 q" [C-up]) ; iris-ansi
    ("1 6 3 q" [M-up]) ; iris-ansi
    ("1 6 4 q" [S-down]) ; iris-ansi
    ("1 6 5 q" [C-down]) ; iris-ansi
    ("1 6 6 q" [M-down]) ; iris-ansi
    ("1 6 7 q" [S-right]) ; iris-ansi
    ("1 6 8 q" [C-right]) ; iris-ansi
    ("1 6 9 q" [M-right]) ; iris-ansi
    ("1 7 2 q" [C-home]) ; iris-ansi
    ("1 7 4 q" [C-left]) ; iris-ansi
    ("1 7 6 q" [C-end]) ; iris-ansi
    ("1 7 8 q" [C-inset]) ; iris-ansi
    ("1 7 9 q" [?\C-/]) ; iris-ansi
    ("1 7 ; 2 ~" [S-f6])
    ("1 7 ; 3 ~" [M-f6])
    ("1 7 ; 4 ~" [M-S-f6])
    ("1 7 ; 6 ~" [C-S-f6])
    ("1 7 ^" [C-f6]) ; rxvt
    ("1 7 ~" [f6])
    ("1 7 ~" [f6]) ; lk201
    ("1 7 ~" [f6]) ; rxvt
    ("1 8 0 q" [?\M-/]) ; iris-ansi
    ("1 8 2 q" [C-up]) ; iris-ansi
    ("1 8 4 q" [C-begin]) ; iris-ansi
    ("1 8 6 q" [C-down]) ; iris-ansi
    ("1 8 7 q" [?\C-*]) ; iris-ansi
    ("1 8 8 q" [?\M-*]) ; iris-ansi
    ("1 8 ; 3 ~" [M-f7])
    ("1 8 ; 4 ~" [M-S-f7])
    ("1 8 ; 6 ~" [C-S-f7])
    ("1 8 ^" [C-f7]) ; rxvt
    ("1 8 ~" [f7])
    ("1 8 ~" [f7]) ; lk201
    ("1 8 ~" [f7]) ; rxvt
    ("1 9 0 q" [C-prior]) ; iris-ansi
    ("1 9 2 q" [C-right]) ; iris-ansi
    ("1 9 4 q" [C-next]) ; iris-ansi
    ("1 9 6 q" [C-delete]) ; iris-ansi
    ("1 9 7 q" [M-delete]) ; iris-ansi    
    ("1 9 8 q" [?\C--]) ; iris-ansi
    ("1 9 9 q" [?\M--]) ; iris-ansi
    ("1 9 ; 3 ~" [M-f8])
    ("1 9 ; 4 ~" [M-S-f8])
    ("1 9 ; 6 ~" [C-S-f8])
    ("1 9 ^" [C-f8]) ; rxvt
    ("1 9 h" [S-erasepage]) ;; Not an X keysym ; tvi
    ("1 9 l" [key_seol])   ;; Not an X keysym ; tvi
    ("1 9 ~" [f8])
    ("1 ; 2 A" [S-up])
    ("1 ; 2 B" [S-down])
    ("1 ; 2 C" [S-right])
    ("1 ; 2 D" [S-left])
    ("1 ; 2 F" [S-end])
    ("1 ; 2 H" [S-home])
    ("1 ; 2 P" [S-f1])
    ("1 ; 2 Q" [S-f2])
    ("1 ; 2 R" [S-f3])
    ("1 ; 2 S" [S-f4])
    ("1 ; 3 A" [M-up])
    ("1 ; 3 B" [M-down])
    ("1 ; 3 C" [M-right])
    ("1 ; 3 D" [M-left])
    ("1 ; 3 F" [M-end])
    ("1 ; 3 H" [M-home])
    ("1 ; 4 A" [M-S-up])
    ("1 ; 4 B" [M-S-down])
    ("1 ; 4 C" [M-S-right])
    ("1 ; 4 D" [M-S-left])
    ("1 ; 4 F" [M-S-end])
    ("1 ; 4 H" [M-S-home])
    ("1 ; 5 A" [C-up])
    ("1 ; 5 B" [C-down])
    ("1 ; 5 C" [C-right])
    ("1 ; 5 D" [C-left])
    ("1 ; 5 F" [C-end])
    ("1 ; 5 H" [C-home])
    ("1 ; 6 A" [C-S-up])
    ("1 ; 6 B" [C-S-down])
    ("1 ; 6 C" [C-S-right])
    ("1 ; 6 D" [C-S-left])
    ("1 ; 6 F" [C-S-end])
    ("1 ; 6 H" [C-S-home])
    ("1 ; 7 A" [C-M-up])
    ("1 ; 7 B" [C-M-down])
    ("1 ; 7 C" [C-M-right])
    ("1 ; 7 D" [C-M-left])
    ("1 ; 7 F" [C-M-end])
    ("1 ; 7 H" [C-M-home])
    ("1 ; 8 A" [C-M-S-up])
    ("1 ; 8 B" [C-M-S-down])
    ("1 ; 8 C" [C-M-S-right])
    ("1 ; 8 D" [C-M-S-left])
    ("1 ; 8 F" [C-M-S-end])
    ("1 ; 8 H" [C-M-S-home])
    ("1 ~" [home])
    ("2 0 0 q" [?\C-+]) ; iris-ansi
    ("2 0 1 q" [?\M-+]) ; iris-ansi
    ("2 0 ; 3 ~" [M-f9])
    ("2 0 ; 4 ~" [M-S-f9])
    ("2 0 ; 6 ~" [C-S-f9])
    ("2 0 ^" [C-f9]) ; rxvt
    ("2 0 ~" [f9])
    ("2 1 ; 3 ~" [M-f10])
    ("2 1 ; 4 ~" [M-S-f10])
    ("2 1 ; 6 ~" [C-S-f10])
    ("2 1 ^" [C-f10]) ; rxvt
    ("2 1 ~" [f10])
    ("2 3 ; 3 ~" [M-f11])
    ("2 3 ; 4 ~" [M-S-f11])
    ("2 3 ; 6 ~" [C-S-f11])
    ("2 3 ^" [C-S-f1]) ; rxvt
    ("2 4 ; 3 ~" [M-f12])
    ("2 4 ; 4 ~" [M-S-f12])
    ("2 4 ; 5 ~" [C-f12])
    ("2 4 ; 6 ~" [C-S-f12])
    ("2 4 ^" [C-S-f2]) ; rxvt
    ("2 5 ^" [C-S-f3]) ; rxvt
    ("2 5 ~" [S-f3]) ; rxvt
    ("2 6 ^" [C-S-f4]) ; rxvt
    ("2 6 ~" [S-f4]) ; rxvt
    ("2 7 ; 1 3 ; 1 3~" [C-M-return])
    ("2 7 ; 1 3 ; 3 9~" [?\C-\M-\'])
    ("2 7 ; 1 3 ; 4 4~" [?\C-\M-,])
    ("2 7 ; 1 3 ; 4 5~" [?\C-\M--])
    ("2 7 ; 1 3 ; 4 6~" [?\C-\M-.])
    ("2 7 ; 1 3 ; 4 7~" [?\C-\M-/])
    ("2 7 ; 1 3 ; 4 8~" [?\C-\M-0])
    ("2 7 ; 1 3 ; 4 9~" [?\C-\M-1])
    ("2 7 ; 1 3 ; 5 0~" [?\C-\M-2])
    ("2 7 ; 1 3 ; 5 1~" [?\C-\M-3])
    ("2 7 ; 1 3 ; 5 2~" [?\C-\M-4])
    ("2 7 ; 1 3 ; 5 3~" [?\C-\M-5])
    ("2 7 ; 1 3 ; 5 4~" [?\C-\M-6])
    ("2 7 ; 1 3 ; 5 5~" [?\C-\M-7])
    ("2 7 ; 1 3 ; 5 6~" [?\C-\M-8])
    ("2 7 ; 1 3 ; 5 7~" [?\C-\M-9])
    ("2 7 ; 1 3 ; 5 9~" [?\C-\M-\;])
    ("2 7 ; 1 3 ; 6 1~" [?\C-\M-=])
    ("2 7 ; 1 3 ; 9 2~" [?\C-\M-\\])
    ("2 7 ; 1 3 ; 9 ~"  [C-M-tab])
    ("2 7 ; 1 4 ; 3 3~"  [?\C-\M-!])
    ("2 7 ; 1 4 ; 3 4~"  [?\C-\M-\"])
    ("2 7 ; 1 4 ; 3 5~"  [?\C-\M-#])
    ("2 7 ; 1 4 ; 3 6~"  [?\C-\M-$])
    ("2 7 ; 1 4 ; 3 7~"  [?\C-\M-%])
    ("2 7 ; 1 4 ; 3 8~"  [?\C-\M-&])
    ("2 7 ; 1 4 ; 4 0~"  [?\C-\M-\(])
    ("2 7 ; 1 4 ; 4 1~"  [?\C-\M-\)])
    ("2 7 ; 1 4 ; 4 2~"  [?\C-\M-*])
    ("2 7 ; 1 4 ; 4 3~"  [?\C-\M-+])
    ("2 7 ; 1 4 ; 5 8~"  [?\C-\M-:])
    ("2 7 ; 1 4 ; 6 0~"  [?\C-\M-<])
    ("2 7 ; 1 4 ; 6 2~"  [?\C-\M->])
    ("2 7 ; 1 4 ; 6 3~"  [(control meta ??)])
    ("2 7 ; 2 ; 1 3 ~"  [S-return])
    ("2 7 ; 2 ; 9 ~"   [S-tab])
    ("2 7 ; 5 ; 1 3 ~"  [C-return])
    ("2 7 ; 5 ; 3 9 ~"  [?\C-\'])
    ("2 7 ; 5 ; 4 4 ~"  [?\C-,])
    ("2 7 ; 5 ; 4 5 ~"  [?\C--])
    ("2 7 ; 5 ; 4 6 ~"  [?\C-.])
    ("2 7 ; 5 ; 4 7 ~"  [?\C-/])
    ("2 7 ; 5 ; 4 8 ~"  [?\C-0])
    ("2 7 ; 5 ; 4 9 ~"  [?\C-1])
    ("2 7 ; 5 ; 5 7 ~"  [?\C-9])
    ("2 7 ; 5 ; 5 9 ~"  [?\C-\;])
    ("2 7 ; 5 ; 6 1 ~"  [?\C-=])
    ("2 7 ; 5 ; 9 2 ~"  [?\C-\\])
    ("2 7 ; 5 ; 9 ~"   [C-tab])
    ("2 7 ; 6 ; 1 3 ~"  [C-S-return])
    ("2 7 ; 6 ; 3 3 ~"  [?\C-!])
    ("2 7 ; 6 ; 3 4 ~"  [?\C-\"])
    ("2 7 ; 6 ; 3 5 ~"  [?\C-#])
    ("2 7 ; 6 ; 3 6 ~"  [?\C-$])
    ("2 7 ; 6 ; 3 7 ~"  [?\C-%])
    ("2 7 ; 6 ; 3 8 ~"  [?\C-&])
    ("2 7 ; 6 ; 4 0 ~"  [?\C-(])
     ("2 7 ; 6 ; 4 1 ~"  [?\C-)])
    ("2 7 ; 6 ; 4 2 ~"  [?\C-*])
    ("2 7 ; 6 ; 4 3 ~"  [?\C-+])
    ("2 7 ; 6 ; 5 8 ~"  [?\C-:])
    ("2 7 ; 6 ; 6 0 ~"  [?\C-<])
    ("2 7 ; 6 ; 6 2 ~"  [?\C->])
    ("2 7 ; 6 ; 6 3 ~"  [(control ??)])
    ("2 7 ; 6 ; 9 ~"   [C-S-tab])
    ("2 7 ; 7 ; 1 3 ~" [C-M-return])
    ("2 7 ; 7 ; 3 2 ~" [?\C-\M-\s])
    ("2 7 ; 7 ; 3 9 ~" [?\C-\M-\'])
    ("2 7 ; 7 ; 4 4 ~" [?\C-\M-,])
    ("2 7 ; 7 ; 4 5 ~" [?\C-\M--])
    ("2 7 ; 7 ; 4 6 ~" [?\C-\M-.])
    ("2 7 ; 7 ; 4 7 ~" [?\C-\M-/])
    ("2 7 ; 7 ; 4 8 ~" [?\C-\M-0])
    ("2 7 ; 7 ; 4 9 ~" [?\C-\M-1])
    ("2 7 ; 7 ; 5 0 ~" [?\C-\M-2])
    ("2 7 ; 7 ; 5 1 ~" [?\C-\M-3])
    ("2 7 ; 7 ; 5 2 ~" [?\C-\M-4])
    ("2 7 ; 7 ; 5 3 ~" [?\C-\M-5])
    ("2 7 ; 7 ; 5 4 ~" [?\C-\M-6])
    ("2 7 ; 7 ; 5 5 ~" [?\C-\M-7])
    ("2 7 ; 7 ; 5 6 ~" [?\C-\M-8])
    ("2 7 ; 7 ; 5 7 ~" [?\C-\M-9])
    ("2 7 ; 7 ; 5 9 ~" [?\C-\M-\;])
    ("2 7 ; 7 ; 6 1 ~" [?\C-\M-=])
    ("2 7 ; 7 ; 9 2 ~" [?\C-\M-\\])
    ("2 7 ; 7 ; 9 ~"  [C-M-tab])
    ("2 7 ; 8 ; 3 3 ~"  [?\C-\M-!])
    ("2 7 ; 8 ; 3 4 ~"  [?\C-\M-\"])
    ("2 7 ; 8 ; 3 5 ~"  [?\C-\M-#])
    ("2 7 ; 8 ; 3 6 ~"  [?\C-\M-$])
    ("2 7 ; 8 ; 3 7 ~"  [?\C-\M-%])
    ("2 7 ; 8 ; 3 8 ~"  [?\C-\M-&])
    ("2 7 ; 8 ; 4 0 ~"  [?\C-\M-\(])
    ("2 7 ; 8 ; 4 1 ~"  [?\C-\M-\)])
    ("2 7 ; 8 ; 4 2 ~"  [?\C-\M-*])
    ("2 7 ; 8 ; 4 3 ~"  [?\C-\M-+])
    ("2 7 ; 8 ; 5 8 ~"  [?\C-\M-:])
    ("2 7 ; 8 ; 6 0 ~"  [?\C-\M-<])
    ("2 7 ; 8 ; 6 2 ~"  [?\C-\M->])
    ("2 7 ; 8 ; 6 3 ~"  [(control meta ??)])
    ("2 8 ^" [C-S-f5]) ; rxvt
    ("2 8 ~" [S-f5]) ; rxvt
    ("2 9 ^" [C-S-f6]) ; rxvt
    ("2 9 ~" [S-f6]) ; rxvt
    ("2 ; 2 ~" [S-insert])
    ("2 ; 2 ~" [S-insert]) ; rxvt
    ("2 ; 3 ~" [M-insert])
    ("2 ; 4 ~" [M-S-insert])
    ("2 ; 5 ~" [C-insert])
    ("2 ; 6 ~" [C-S-insert])
    ("2 ; 7 ~" [C-M-insert])
    ("2 ; 8 ~" [C-M-S-insert])
    ("2 J" [key_clear])    ;; Not an X keysym ; tvi
    ("2 K" [S-clearentry]) ;; Not an X keysym ; tvi
    ("2 N" [clearentry])   ;; Not an X keysym ; tvi
    ("2 ^" [C-insert]) ; rxvt
    ("2 ~" [insert])
    ("3 $" [S-delete]) ; rxvt
    ("3 1 ^" [C-S-f7]) ; rxvt
    ("3 1 ~" [S-f7]) ; rxvt
    ("3 2 ^" [C-S-f8]) ; rxvt
    ("3 2 ~" [S-f8]) ; rxvt
    ("3 3 ^" [C-S-f9]) ; rxvt
    ("3 3 ~" [S-f9]) ; rxvt
    ("3 4 ^" [C-S-f10]) ; rxvt
    ("3 4 ~" [S-f10]) ; rxvt
    ("3 ; 2 ~" [S-delete])
    ("3 ; 3 ~" [M-delete])
    ("3 ; 4 ~" [M-S-delete])
    ("3 ; 5 ~" [C-delete])
    ("3 ; 6 ~" [C-S-delete])
    ("3 ; 7 ~" [C-M-delete])
    ("3 ; 8 ~" [C-M-S-delete])
    ("3 ^" [C-delete]) ; rxvt
    ("3 ~" [delete])
    ("4 h" [key_sic])            ;; Not an X
    ("4 l" [S-delete])           ;; Not an X
    ("4 ~" [select])
    ("5 $" [S-prior]) ; rxvt
    ("5 ; 2 ~" [S-prior])
    ("5 ; 3 ~" [M-prior])
    ("5 ; 4 ~" [M-S-prior])
    ("5 ; 5 ~" [C-prior])
    ("5 ; 6 ~" [C-S-prior])
    ("5 ; 7 ~" [C-M-prior])
    ("5 ; 8 ~" [C-M-S-prior])
    ("5 ^" [C-prior]) ; rxvt
    ("5 ~" [prior])
    ("6 $" [S-next]) ; rxvt
    ("6 ; 2 ~" [S-next])
    ("6 ; 3 ~" [M-next])
    ("6 ; 4 ~" [M-S-next])
    ("6 ; 5 ~" [C-next])
    ("6 ; 6 ~" [C-S-next])
    ("6 ; 7 ~" [C-M-next])
    ("6 ; 8 ~" [C-M-S-next])
    ("6 ^" [C-next]) ; rxvt
    ("6 ~" [next])
    ("7 $" [S-home]) ; rxvt
    ("7 ^" [C-home]) ; rxvt
    ("7 ~" [home]) ; rxvt
    ("8 $" [S-end]) ; rxvt
    ("8 ^" [C-end]) ; rxvt
    ("8 ~" [end]) ; rxvt
    ("? 1 i" [key_sprint]) ;; Not an X keysym ; tvi
    ("@" [insert]) ; tvi
    ("A" [up])
    ("A" [up]) ; rxvt
    ("B" [down])
    ("B" [down]) ; rxvt
    ("C" [right])
    ("C" [right]) ; rxvt
    ("D" [left])
    ("D" [left]) ; rxvt
    ("E" [?\C-j])        ;; Not an X keysym ; tvi
    ("H" [home]) ; iris-ansi
    ("H" [home]) ; tvi
    ("J" [key_eos])      ;; Not an X keysym ; tvi
    ("K" [key_eol])      ;; Not an X keysym ; tvi
    ("L" [insertline]) ; tvi
    ("M" [M-delete]) ; iris-ansi
    ("P" [key_dc])       ;; Not an X keysym ; tvi
    ("Q" [S-insertline])       ;; Not an X keysym ; tvi
    ("U" [next]) ;; actually the `page' key ; tvi
    ("V" [S-page])              ;; Not an X keysym ; tvi8
    ("Z" [?\S-\t]) ; iris-ansi
    ("a" [S-up]) ; rxvt
    ("b" [S-down]) ; rxvt
    ("c" [S-right]) ; rxvt
    ("d" [S-left]) ; rxvt
    ("e 1 5 ; 5 ~" [C-f5])
    ("e 1 7 ; 5 ~" [C-f6])
    ("e 1 8 ; 2 ~" [S-f7])
    ("e 1 8 ; 5 ~" [C-f7])
    ("e 1 9 ; 2 ~" [S-f8])
    ("e 1 9 ; 5 ~" [C-f8])
    ("e 2 0 ; 2 ~" [S-f9])
    ("e 2 0 ; 5 ~" [C-f9])
    ("e 2 1 ; 2 ~" [S-f10])
    ("e 2 1 ; 5 ~" [C-f10])
    ("e 2 3 ; 2 ~" [S-f11])
    ("e 2 3 ; 5 ~" [C-f11])
    ("e 2 4 ; 2 ~" [S-f12])
    ("g" [S-tab])        ;; Not an X keysym ; tvi
    )
  "Ergoemacs terminal ESC [ translations.")

(defvar ergoemacs-esc-translations
  '(("5" [S-send]) 
    ("E" [insertline])
    ("I" [key-stab])  ;; Not an X keysym
    ("J" [key-snext]) ;; Not an X keysym
    ("K" [next])
    ("N" [ALT])
    ("P" [print])
    ("Q" [insert])
    ("R" [deleteline])
    ("S" [send])
    ("T" [clearline])
    ("W" [?\C-?])                ;; Not an X keysym
    ("Y" [key-clear]) ;; Not an X keysym
    ("r" [replace]))
  "Ergoemacs terminal extra translations."
  )



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

(defvar ergoemacs-M-O-delay 0.05
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
    (if ergoemacs-M-O-prefix-keys
        (let (fn)
          (let (ergoemacs-shortcut-mode)
            (setq fn (key-binding
                      (read-kbd-macro
                       (format "%s <timeout>"
                               ergoemacs-M-O-prefix-keys)))))
          ;; Lookup keys, and then send <exit> event.
          (setq prefix-arg ergoemacs-curr-prefix-arg)
          (call-interactively fn t)
          (reset-this-command-lengths)
          (setq unread-command-events (cons 'exit unread-command-events)))
      (setq prefix-arg ergoemacs-curr-prefix-arg)
      (reset-this-command-lengths)
      (setq unread-command-events (cons 'timeout unread-command-events))
      (call-interactively fn))
    (setq ergoemacs-M-O-prefix-keys nil)))

(defun ergoemacs-M-o (&optional arg use-map)
  "Ergoemacs M-o function.
Allows arrow keys and the to work in the terminal. Call the true
function immediately when `window-system' is true."
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

(defun ergoemacs-setup-keys-for-keymap---internal (keymap key def)
  "Defines KEY in KEYMAP to be DEF"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (if (condition-case err
              (stringp (nth 0 def))
            (error nil))
          (progn
            (eval
             (macroexpand
              `(progn
                 (ergoemacs-keyboard-shortcut
                  ,(intern (concat "ergoemacs-shortcut---"
                                   (md5 (format "%s; %s" (nth 0 def)
                                                (nth 1 def))))) ,(nth 0 def)
                                                ,(nth 1 def))
                 (define-key ergoemacs-shortcut-keymap key
                   ',(intern (concat "ergoemacs-shortcut---"
                                     (md5 (format "%s; %s" (nth 0 def)
                                                  (nth 1 def))))))))))
        (mapc
         (lambda(new-def)
           (unless found
             (setq found
                   (ergoemacs-setup-keys-for-keymap---internal keymap key new-def))))
         def))
      (symbol-value 'found)))
   ((condition-case err
        (fboundp def)
      (error nil))
    (cond
     ((memq def '(ergoemacs-ctl-c ergoemacs-ctl-x))
      (define-key ergoemacs-shortcut-keymap key def))
     ((and ergoemacs-prefer-shortcuts
           (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
      (let (shortcut-key)
        (setq shortcut-key (where-is-internal def (current-global-map)))
        (if (not shortcut-key)
            (define-key keymap key def)
          ;; The shortcut key is the first-one that matches
          (ergoemacs-debug "\tShortcut %s for %s"
                           (key-description (nth 0 shortcut-key))
                           def)
          (setq shortcut-key (key-description (nth 0 shortcut-key)))
          (eval
           (macroexpand
            `(progn
               (ergoemacs-keyboard-shortcut
                ,(intern (concat "ergoemacs-shortcut---"
                                 (md5 (format "%s; global" shortcut-key)))) ,shortcut-key global)
               (define-key ergoemacs-shortcut-keymap key
                 ',(intern (concat "ergoemacs-shortcut---"
                                   (md5 (format "%s; global" shortcut-key))))))))
          )))
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
    (eval (macroexpand `(progn
                          (ergoemacs-keyboard-shortcut
                           ,(intern (concat "ergoemacs-shortcut---"
                                            (md5 (format "%s; nil" def)))) ,def)
                          (define-key ergoemacs-shortcut-keymap key
                            ',(intern (concat "ergoemacs-shortcut---"
                                              (md5 (format "%s; nil" def))))))))
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
     (if (eq ',keymap 'ergoemacs-keymap)
         (ergoemacs-debug "Theme: %s" ergoemacs-theme))
     ;; Fixed layout keys
     (mapc
      (lambda(x)
        (when (and (eq 'string (type-of (nth 0 x))))
          (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))
          (condition-case err
              (setq key (read-kbd-macro
                         trans-key))
            (error
             (setq key (read-kbd-macro
                        (encode-coding-string
                         trans-key
                         locale-coding-system)))))
          (if (ergoemacs-global-changed-p trans-key)
              (progn
                (ergoemacs-debug "!!!Fixed %s has changed globally." trans-key)
                (ergoemacs-setup-keys-for-keymap---internal ,keymap key (lookup-key (current-global-map) key)))
            (setq cmd (nth 1 x))
	    (if (eq ',keymap 'ergoemacs-keymap)
                (ergoemacs-debug "Fixed: %s -> %s %s" trans-key cmd key))
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
           (define-key ergoemacs-M-o-keymap [timeout] g-M-o))))))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  (ergoemacs-setup-fast-keys)
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
                `(add-to-list 'minor-mode-overriding-map-alist
                              (cons 'ergoemacs-mode ,(intern (concat "ergoemacs-" (symbol-name hook) "-keymap")))
                              nil ,(if (equal hook 'minibuffer-setup-hook)
                                       '(lambda (x y)
                                          (equal (car y) (car x)))
                                     nil))
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
         (let (ergoemacs-shortcut-mode
               ergoemacs-unbind-mode)
           (describe-buffer-bindings buf (read-kbd-macro cur-prefix)))
         (goto-char (point-min))
         (while (re-search-forward (format "%s \\(.*?\\)[ \t]\\{2,\\}\\(.+\\)$" cur-prefix) nil t)
           (setq new-key (match-string 1))
           (setq fn (match-string 2))
           (unless (string-match " " new-key)
             (if (string-match "Prefix Command$" (match-string 0))
                 (unless (string-match "ESC" new-key)
                   (ergoemacs-debug "Prefix: %s" new-key)
                   (add-to-list 'prefixes new-key))
               (unless (string-match "ergoemacs-old-key---" fn)
                 (condition-case err
                     (with-temp-buffer
                       (insert "(if (keymapp '" fn
                               ") (unless (string-match \"ESC\" \"" new-key
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
  (let ((fn (concat "ergoemacs-shortcut---"
                    (md5 (format "%s %s; %s" prefix-key untranslated-key
                                 type)))))
    (eval
     (macroexpand
      `(progn
         (ergoemacs-keyboard-shortcut
          ,(intern fn) ,(format "%s %s" prefix-key untranslated-key) ,type))))
    (call-interactively (intern fn))))

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


(defvar ergoemacs-repeat-shortcut-keymap (make-keymap)
  "Keymap for repeating often used shortcuts like C-c C-c.")

(defvar ergoemacs-repeat-shortcut-msg ""
  "Message for repeating keyboard shortcuts like C-c C-c")

(defun ergoemacs-shortcut-timeout ()
  (message ergoemacs-repeat-shortcut-msg)
  (set-temporary-overlay-map ergoemacs-repeat-shortcut-keymap))

(defvar ergoemacs-current-extracted-map nil
  "Current extracted map for `ergoemacs-keyboard-shortcut' defined functions")

(defvar ergoemacs-first-extracted-variant nil
  "Current extracted variant")

(defun ergoemacs-shortcut (&optional arg key chorded repeat)
  "Ergoemacs Shortcut.

ARG is the prefix arg that was called.

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

C-k C-n     -> M-k M-n             (kmacro-cycle-ring-next)
C-k a       -> M-k a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

When REPEAT is a variable name, then an easy repeat is setup for the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows Ctrl+c Ctrl+c to be repeated by m.
"
  (interactive "P")
  (when key
    ))

;;;###autoload
(defmacro ergoemacs-keyboard-shortcut (name key &optional chorded repeat)
  "Creates a function NAME that issues a keyboard shortcut for KEY.
CHORDED is a variable that alters to keymap to allow unchorded
key sequences.

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

C-k C-n     -> M-k M-n             (kmacro-cycle-ring-next)
C-k a       -> M-k a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

When REPEAT is a variable name, then an easy repeat is setup for the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows Ctrl+c Ctrl+c to be repeated by m.
"
  `(progn
     ,(cond
       ((eq chorded 'unchorded))
       ((eq chorded 'ctl-to-alt))
       (t
        (when repeat
          `(defcustom ,(intern (symbol-name repeat)) t
             ,(format "Allow %s to be repeated." (ergoemacs-pretty-key key))
             :group 'ergoemacs-mode
             :type 'boolean))))
     (defun ,(intern (symbol-name name)) (&optional arg)
       ,(cond
         ((eq chorded 'unchorded)
          (format "Creates a keymap that extracts the unchorded %s combinations and then issues %s.  Also allows unbound or normal variants by pressing the <menu> key." key key))
         ((eq chorded 'ctl-to-alt)
          (format "Creates a keymap that extracts the %s combinations and translates Ctl+ to Alt+. Also allows the unbound or Ctl to alt variants by pressing the <menu>" key))
         ((eq chorded 'normal)
          (format "Creates a keymap that extracts the %s keymap. Also allows the unbound or Ctl+ to Alt+ and unbound variants by pressing the <menu>" key))
         (t
          (format "A shortcut to %s." (ergoemacs-pretty-key key))))
       (interactive "P")
       (setq ergoemacs-push-M-O-timeout nil) ;; Cancel timeouts
       (setq this-command last-command) ; Don't record this command.
       (setq prefix-arg current-prefix-arg)
       (let (key-seq (key ,key) deactivate-mark)
         (eval (macroexpand '(ergoemacs-extract-maps ergoemacs-current-extracted-map key)))
         ,(cond
           ((eq chorded 'unchorded)
            `(progn
               (setq ergoemacs-first-extracted-variant 'unchorded)
               (setq key-seq  (read-kbd-macro (format "<Unchorded> %s" ,key)))
               (set-temporary-overlay-map ergoemacs-current-extracted-map)
               (setq key-seq (listify-key-sequence key-seq))
               (reset-this-command-lengths)
               (setq unread-command-events
                     (append key-seq unread-command-events))
               (princ (concat (if current-prefix-arg
                                  (format "%s " current-prefix-arg)
                                "")
                              ,(format "<Unchorded> %s "
                                       (ergoemacs-pretty-key key))))))
           ((eq chorded 'normal)
            `(progn
               (setq ergoemacs-first-extracted-variant 'normal)
               (setq key-seq  (read-kbd-macro (format "<Normal> %s" ,key)))
               (set-temporary-overlay-map ergoemacs-current-extracted-map)
               (setq key-seq (listify-key-sequence key-seq))
               (setq unread-command-events
                     (append key-seq unread-command-events))
               (reset-this-command-lengths)
               (princ (concat (if current-prefix-arg
                                  (format "%s " current-prefix-arg)
                                "")
                              ,(format "%s "
                                       (ergoemacs-pretty-key key))))))
           ((eq chorded 'ctl-to-alt)
            `(progn
               (setq ergoemacs-first-extracted-variant 'ctl-to-alt)
               (setq key-seq (read-kbd-macro (format "<Ctl%sAlt> %s" 
                                                     (ergoemacs-unicode-char "↔" " to ")
                                                     ,key)))
               (setq key-seq (listify-key-sequence key-seq))
               (set-temporary-overlay-map ergoemacs-current-extracted-map)
               (setq unread-command-events
                     (append key-seq unread-command-events))
               (reset-this-command-lengths)
               (princ (concat (if current-prefix-arg
                                  (format "%s " current-prefix-arg)
                                "")
                              ,(format "<Ctl%sAlt> %s "
                                       (ergoemacs-unicode-char "↔" " to ") (ergoemacs-pretty-key key))))))
           (t
            `(let ((ctl-c-keys (key-description (this-command-keys))))
               (let (ergoemacs-shortcut-mode
                     ergoemacs-unbind-mode
                     (ergoemacs-mode ,(not (eq chorded 'global)))
                     fn)
                 (setq fn (key-binding (read-kbd-macro ,key)))
                 (if (not fn)
                     (message "%s is not defined." (ergoemacs-pretty-key ,key))
                   (setq this-command last-command) ; Don't record this command.
                   (setq prefix-arg current-prefix-arg)
                   (if (condition-case err
                           (functionp fn)
                         (error nil))
                       (call-interactively fn)
                     (setq prefix-arg current-prefix-arg)
                     (setq unread-command-events
                           (append
                            (listify-key-sequence (read-kbd-macro ,key))
                            unread-command-events))
                     (reset-this-command-lengths))
                   ,(when repeat
                      `(when ,(intern (symbol-name repeat))
                         (when  (string-match "[A-Za-z]$" ctl-c-keys)
                           (setq ctl-c-keys (match-string 0 ctl-c-keys))
                           (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
                           (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys) fn)
                           (setq ergoemacs-repeat-shortcut-msg
                                 (format ,(format "Repeat %s with %%s" (ergoemacs-pretty-key key))
                                         (ergoemacs-pretty-key ctl-c-keys)))
                           ;; Allow time to process the unread command events before
                           ;; installing temporary keymap
                           (setq ergoemacs-M-O-timer (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-shortcut-timeout))))))))))))))


(ergoemacs-keyboard-shortcut ergoemacs-ctl-c-ctl-c "C-c C-c" nil ergoemacs-repeat-ctl-c-ctl-c)


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
               (setq minor-mode-overriding-map-alist (delq x minor-mode-overriding-map-alist))))))
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
        (ergoemacs-debug "Ergoemacs Shortcut Keys have loaded been turned on.")
        (let ((x (assq 'ergoemacs-shortcut-mode minor-mode-map-alist)))
          (when x
            (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
          (push (cons 'ergoemacs-shortcut-mode ergoemacs-shortcut-keymap) minor-mode-map-alist)))
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
        local-fn)
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
    ergoemacs-describe-major-mode
    helm-M-x)
  "Functions that describe keys.
Setup C-c and C-x keys to be described properly.")

(defun ergoemacs-pre-command-hook ()
  "Ergoemacs pre-command-hook"
  (let (deactivate-mark)
    (condition-case err
        (when ergoemacs-mode
          (cond
           ((memq this-command ergoemacs-describe-keybindings-functions)
            ;; Turn off shortcut mode for describing bindings. 
            (ergoemacs-shortcut-mode -1))
           ((not (eq 'ergoemacs-ctl-c (key-binding (read-kbd-macro "C-c"))))
            ;; Promote shortcut mode if overwritten somehow.
            (when ergoemacs-shortcut-mode
              (ergoemacs-shortcut-mode -1))
            (ergoemacs-shortcut-mode 1))))
      (error nil)))
  t)

(defun ergoemacs-post-command-hook ()
  "Ergoemacs post-command-hook"
  (let (deactivate-mark)
    (condition-case err
        (when ergoemacs-mode
          ;; Promote/activate shortcut mode if overwritten/disabled somehow.
          (unless (eq 'ergoemacs-ctl-c (key-binding (read-kbd-macro "C-c")))
            (when ergoemacs-shortcut-mode
              (ergoemacs-shortcut-mode -1))
            (ergoemacs-shortcut-mode 1)))
      (error nil)))
  t)

(add-hook 'post-command-hook 'ergoemacs-post-command-hook)
(add-hook 'pre-command-hook 'ergoemacs-pre-command-hook)

(provide 'ergoemacs-mode)

;;; ergoemacs-mode.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
