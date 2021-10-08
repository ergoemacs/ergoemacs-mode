;;; ergoemacs-key-description.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-key-description.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; Ergoemacs-mode key description library.
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

(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(require 'descr-text)
(require 'faces)
(require 'help-mode)

(defvar ergoemacs-use-unicode-symbols)
(defvar ergoemacs-display-unicode-characters)
(defvar ergoemacs-display-capitalize-keys)
(defvar ergoemacs-display-key-use-face)
(defvar ergoemacs-display-small-symbols-for-key-modifiers nil)
(defvar ergoemacs-display-use-unicode-brackets-around-keys nil)
(defvar ergoemacs-display-without-brackets nil
  "Display the key without brackets.")

(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifiers "ergoemacs-translate")
(declare-function ergoemacs-translate--event-basic-type "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifier-hash "ergoemacs-translate")

(declare-function ergoemacs-map-properties--composed-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-lessp "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")

(declare-function ergoemacs-map--cache-- "ergoemacs-map")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(defvar ergoemacs-key-description--display-char-cache nil
  "List of characters and fonts and if they display or not.")

(defun ergoemacs-key-description--display-char-p (char)
  "Determines if CHAR can be displayed."
  (cond
   ((= 1 (length char))
    (ignore-errors
      (let* (ret
             (buf (current-buffer))
             (face (font-xlfd-name (face-attribute 'default :font)))
             (found (assoc (list face char window-system) ergoemacs-key-description--display-char-cache)))
        (if found
            (nth 0 (cdr found))
          (switch-to-buffer (get-buffer-create " *ergoemacs-display-char-p*") t)
          (delete-region (point-min) (point-max))
          (insert char)
          (let ((display (describe-char-display (point-min) (char-after (point-min)))))
            (if (display-graphic-p (selected-frame))
                (if display
                    (setq ret (font-at (point-min))))
              (if display
                  (setq ret (font-at (point-min))))))
          (switch-to-buffer buf)
          ;; Save it so the user doesn't see the buffer popup very much
          ;; (if at all).
          (push (list (list face char window-system) ret) ergoemacs-key-description--display-char-cache)
          ret))))
   ((stringp char)
    (catch 'does-not-display
      (dolist (c (mapcar (lambda(x) (make-string 1 x)) (vconcat char)))
        (when (not (ergoemacs-key-description--display-char-p c))
          (throw 'does-not-display nil)))
      t))
   (t nil)))

(defun ergoemacs-key-description--unicode-char--internal (char alt-char)
  "Return CHAR if it can be displayed, otherwise use ALT-CHAR.
This assumes `ergoemacs-display-unicode-characters' is non-nil.  When
`ergoemacs-display-unicode-characters' is nil display ALT-CHAR"
  (if (and ergoemacs-display-unicode-characters
           (ergoemacs-key-description--display-char-p char))
      char
    alt-char))

(defun ergoemacs-key-description--unicode-char (&rest chars)
  "Return the first displayable character in CHARS.
This uses `ergoemacs-key-description--unicode-char--internal'"
  (if ergoemacs-use-unicode-symbols
      (let* ((char-list chars)
             (test-char (pop char-list))
             tmp
             (next-char test-char))
        (catch 'found-char
          (while char-list
            (setq next-char (pop char-list)
                  tmp (ergoemacs-key-description--unicode-char--internal test-char next-char))
            (if (string= tmp next-char)
                (setq test-char next-char)
              (throw 'found-char tmp)))
          next-char))
    (car (last chars))))

(defun ergoemacs-key-description--key (key mod)
  "Key description.
KEY is the fundamental event of a key.
MOD ar the modifiers applied to the key."
  (let ((ret ""))
    (cond
     ((eq key 'deletechar)
      (setq ret "Del"))
     ((memq key '(insert insertchar))
      (setq ret "Ins"))
     ((eq key 'home)
      (setq ret "Home"))
     ((eq key 'end)
      (setq ret "End"))
     ((eq key 32)
      (setq ret "Space"))
     ((eq key 127)
      (setq ret "←Backspace"))
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret "Tab↹"))
     ((eq key 'return)
      (setq ret "Enter↵"))
     ((memq key '(apps menu))
      (setq ret "▤"))
     ((eq key 'left)
      (setq ret "←"))
     ((eq key 'right)
      (setq ret "→"))
     ((eq key 'up)
      (setq ret "↑"))
     ((eq key 'down)
      (setq ret "↓"))
     ((eq key 'prior)
      (setq ret "PgUp"))
     ((eq key 'next)
      (setq ret "PgDn"))
     ((eq key 'remap)
      (setq ret "➩"))
     ((eq key 'ergoemacs-timeout)
      (setq ret "⌚"))
     ((integerp key)
      (setq ret (or (and (or (and (eq ergoemacs-display-capitalize-keys 'with-modifiers)
                                  mod)
                             (eq ergoemacs-display-capitalize-keys t))
                         (upcase (make-string 1 key)))
                    (make-string 1 key))))
     ((and (symbolp key) (string-match "^f[0-9]+$" (symbol-name key)))
      (setq ret (upcase (symbol-name key))))
     (t
      (setq ret (format "%s" key))))
    (setq ret (concat (copy-sequence ret) ""))
    (when (and ergoemacs-display-key-use-face
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (length ret)
                           '(face ergoemacs-display-key-face)
                           ;; Need to make a copy of ret because the
                           ;; (length ret) call makes it sometimes
                           ;; immutable
                           ret))
    ret))

(defun ergoemacs-key-description--modifier (mod)
  "Modifier MOD description."
  (let (ret)
    (cond
     ;; OSX specific key descriptions
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret "⌘"))
     ((and (eq mod 'meta)
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret "⌘Cmd+"))
     ((and (eq mod 'meta)
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret "⌥Opt+"))
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret "⌥"))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'shift))
      (setq ret "⇧"))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'meta))
      (setq ret "♦"))
     ((and (or (eq system-type 'darwin) ergoemacs-display-small-symbols-for-key-modifiers)
           (memq mod '(control ergoemacs-control)))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret "⇧Shift+"))
     ((memq mod '(control ergoemacs-control))
      (setq ret "Ctrl+"))
     ((eq mod 'meta)
      (setq ret "Alt+"))
     ((and (eq mod 'super) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'windows-nt))
      (setq ret "⊞"))
     ((and (eq mod 'super)
           (eq system-type 'windows-nt))
      (setq ret "⊞Win+"))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-display-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-display-key-face) ret))))
    (when (and ergoemacs-display-key-use-face
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (- (length ret) 1)
                           '(face ergoemacs-display-key-face) ret))
    ret))

(defun ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers (mod)
  "Change `ergoemacs-mode' special modifiers in MOD to the Emacs modifiers."
  (let ((tmp '()))
    (dolist (m mod)
      (cond
       ((eq m 'ergoemacs-control)
        (push 'control tmp))
       ((eq m 'control))
       (t
        (push m tmp))))
    tmp))

(defun ergoemacs-key-description--menu (kbd &optional layout)
  "Create pretty keyboard bindings for menus.
KBD is the keyboard code, LAYOUT is the keyboard layout."
  (let ((ergoemacs-display-without-brackets t)
        (ergoemacs-display-key-use-face nil)
        (ergoemacs-display-small-symbols-for-key-modifiers nil))
    (ergoemacs-key-description kbd layout)))


(defun ergoemacs-key-description (kbd &optional layout)
  "Create Pretty keyboard binding from kbd from M- to Alt+.

KBD is the keyboard code.  LAYOUT is the layout that is used."
  (if (not kbd) ""
    (let ((kbd (or (ergoemacs-translate--escape-to-meta kbd)
                   (and (stringp kbd) (vconcat kbd)) kbd)))
      (if (eq kbd (vector)) ""
        (let ((ret "")
              tmp
              mod ev)
          (dolist (key (listify-key-sequence kbd))
            (setq mod (ergoemacs-translate--event-modifiers key)
                  ev (ergoemacs-translate--event-basic-type key))
            (cond
             ((and (memq 'control mod) (eq ev ?\[))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'escape))
             ((and (memq 'control mod) (eq ev ?m))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'return))
             ((and (memq 'control mod) (eq ev ?i))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'tab))
             ((memq 'ergoemacs-shift mod)
              (setq tmp '())
              (dolist (m mod)
                (unless (eq m 'ergoemacs-shift)
                  (push m tmp)))
              (setq mod tmp
                    ev (ergoemacs-gethash (intern (format "s%s" ev))
                                          (ergoemacs-translate--event-modifier-hash layout)))))
	    (when (memq 'ergoemacs-gui mod)
	      (setq tmp '())
              (dolist (m mod)
                (unless (eq m 'ergoemacs-gui)
                  (push m tmp)))
	      (setq mod tmp))
            (setq tmp (format "%s%s%s%s"
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face) "")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys "【")
                                  "[")
                              (mapconcat #'ergoemacs-key-description--modifier
                                         mod "")
                              (ergoemacs-key-description--key ev mod)
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face) "")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys "】")
                                  "]")))
            (when (and ergoemacs-display-small-symbols-for-key-modifiers ergoemacs-display-key-use-face)
              (add-text-properties 0 (length tmp)
                                   '(face ergoemacs-display-key-face) tmp))
            (setq ret (format "%s%s%s" ret
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face) " ")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys "")
                                  " ")
			      tmp)))
          (substring ret (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face) 1)
                             (and ergoemacs-display-use-unicode-brackets-around-keys 0)
                             1)))))))

(defun ergoemacs-key-description-kbd (code)
  "Create `ergoemacs-mode' style description of kbd macro CODE."
  (if (not code) ""
    (save-match-data
      (ergoemacs-key-description (read-kbd-macro code t)))))

(provide 'ergoemacs-key-description)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-key-description.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
