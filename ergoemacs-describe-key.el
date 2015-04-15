;;; ergoemacs-describe-key.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-describe-key.el
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


(defvar ergoemacs-display-char-list nil
  "List of characters and fonts and if they display or not.")

(require 'descr-text)
(require 'faces)

(defvar ergoemacs-use-unicode-char t
  "Use unicode characters when available.")

(defun ergoemacs-display-char-p (char)
  "Determines if CHAR can be displayed."
  (ignore-errors
    (let* (ret
           (buf (current-buffer))
           (face (font-xlfd-name (face-attribute 'default :font)))
           (found (assoc (list face char window-system) ergoemacs-display-char-list)))
      (if found
          (nth 0 (cdr found))
        (switch-to-buffer (get-buffer-create " *ergoemacs-display-char-p*") t)
        (delete-region (point-min) (point-max))
        (insert char)
        (let ((display (describe-char-display (point-min) (char-after (point-min)))))
          (if (display-graphic-p (selected-frame))
              (if display
                  (setq ret t))
            (if display
                (setq ret t))))
        (switch-to-buffer buf)
        ;; Save it so the user doesn't see the buffer popup very much
        ;; (if at all).
        (push (list (list face char window-system) ret) ergoemacs-display-char-list)
        ret))))

(defun ergoemacs-unicode-char (char alt-char)
  "Uses CHAR if it can be displayed, otherwise use ALT-CHAR.
This assumes `ergoemacs-use-unicode-char' is non-nil.  When
`ergoemacs-use-unicode-char' is nil display ALT-CHAR"
  (if (and ergoemacs-use-unicode-char (ergoemacs-display-char-p char))
      char
    alt-char))

(defcustom ergoemacs-use-ergoemacs-key-descriptions t
  "Use ergoemacs key descriptions (Alt+) instead of emacs key descriptors (M-)"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-unicode-brackets t
  "Use unicode brackets."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-small-symbols nil
  "Use small symbols to represent alt+ ctl+ etc. on windows/linux."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-capitalize-keys 'with-modifiers
  "Capitalize keys like Ctrl+C.
`ergoemacs-mode' should show Ctrl+Shift+C if you are pressing these keys."
  :type '(choice
          (const :tag "Don't Capitalize Keys" nil)
          (const :tag "Capitalize Keys with modifiers" with-modifiers)
          (const :tag "Capitalize Keys" t))
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defface ergoemacs-pretty-key
  '((t :inverse-video t :box (:line-width 1 :style released-button) :weight bold))
  "Button Face for a `ergoemacs-mode' pretty key."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-pretty-key-use-face t
  "Use a button face for keys."
  :group 'ergoemacs-mode)

(defun ergoemacs-pretty-key-description--key (key mod)
  "Key description"
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
      (setq ret (format "%sBackspace" (ergoemacs-unicode-char "←" "left"))))
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret (format "Tab%s"
                        (ergoemacs-unicode-char "↹" ""))))
     ((eq key 'return)
      (setq ret (format "Enter%s"
                        (ergoemacs-unicode-char "↵" ""))))
     ((memq key '(apps menu))
      (setq ret (ergoemacs-unicode-char "▤" "Menu")))
     ((eq key 'left)
      (setq ret (ergoemacs-unicode-char "←" "left")))
     ((eq key 'right)
      (setq ret (ergoemacs-unicode-char "→" "right")))
     ((eq key 'up)
      (setq ret (ergoemacs-unicode-char "↑" "up")))
     ((eq key 'down)
      (setq ret (ergoemacs-unicode-char "↓" "down")))
     ((eq key 'prior)
      (setq ret "PgUp"))
     ((eq key 'next)
      (setq ret "PgDn"))
     ((integerp key)
      (setq ret (or (and (or (and (eq ergoemacs-capitalize-keys 'with-modifiers)
                                  mod)
                             (eq ergoemacs-capitalize-keys t))
                         (upcase (make-string 1 key)))
                    (make-string 1 key))))
     ((and (symbolp key) (string-match "^f[0-9]+$" (symbol-name key)))
      (setq ret (upcase (symbol-name key))))
     (t
      (setq ret (format "%s" key))))
    (when (and ergoemacs-pretty-key-use-face
               (not ergoemacs-use-small-symbols))
      (add-text-properties 0 (length ret)
                           '(face ergoemacs-pretty-key) ret))
    ret))

(defun ergoemacs-pretty-key-description--modifier (mod)
  "Modifier description"
  (let (ret)
    (cond
     ;; OSX specific key descriptions
     ((and (eq mod 'meta) ergoemacs-use-small-symbols
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%s"
                        (ergoemacs-unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%sCmd+"
                        (ergoemacs-unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%sOpt+" (ergoemacs-unicode-char "⌥" "+"))))
     ((and (eq mod 'meta) ergoemacs-use-small-symbols
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%s" (ergoemacs-unicode-char "⌥" "+"))))
     ((and ergoemacs-use-small-symbols (eq mod 'shift))
      (setq ret (ergoemacs-unicode-char "⇧" "+")))
     ((and ergoemacs-use-small-symbols (eq mod 'meta))
      (setq ret (ergoemacs-unicode-char "♦" "!")))
     ((and (or (eq system-type 'darwin) ergoemacs-use-small-symbols)
           (memq mod '(control ergoemacs-control)))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret (format "%sShift+"
                        (ergoemacs-unicode-char "⇧" ""))))
     ((memq mod '(control ergoemacs-control))
      (setq ret "Ctrl+"))
     ((eq mod 'meta)
      (setq ret "Alt+"))
     ((and (eq mod 'super) ergoemacs-use-small-symbols
           (eq system-type 'windows-nt))
      (setq ret (ergoemacs-unicode-char "⊞" "#")))
     ((and (eq mod 'super)
           (eq system-type 'windows-nt))
      (setq ret (format "%sWin+" (ergoemacs-unicode-char "⊞" "#"))))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret))))
    (when (and ergoemacs-pretty-key-use-face
               (not ergoemacs-use-small-symbols))
      (add-text-properties 0 (- (length ret) 1)
                           '(face ergoemacs-pretty-key) ret))
    ret))

(defun ergoemacs-pretty-key-description--ctl (mod)
  "Put in the correct modifiers for special keys"
  (let ((tmp '()))
    (dolist (m mod)
      (cond
       ((eq m 'ergoemacs-control)
        (push 'control tmp))
       ((eq m 'control))
       (t
        (push m tmp))))
    tmp))

(defun ergoemacs-pretty-key-description (kbd &optional layout)
  "Creates Pretty keyboard binding from kbd from M- to Alt+"
  (if (eq kbd (vector)) ""
    (let ((ret "")
          tmp
          mod ev)
      (dolist (key (listify-key-sequence kbd))
        (setq mod (ergoemacs-event-modifiers key)
              ev (ergoemacs-event-basic-type key))
        (cond
         ((and (memq 'control mod) (eq ev ?\[))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'escape))
         ((and (memq 'control mod) (eq ev ?m))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'return))
         ((and (memq 'control mod) (eq ev ?i))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'tab))
         ((memq 'ergoemacs-shift mod)
          (setq tmp '())
          (dolist (m mod)
            (unless (eq m 'ergoemacs-shift)
              (push m tmp)))
          (setq mod tmp
                ev (gethash (intern (format "s%s" ev))
                            (ergoemacs-event-modifier-hash layout)))))
        (setq tmp (format "%s%s%s%s"
                          (or (and ergoemacs-pretty-key-use-face "")
                              (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "【" "["))
                              "[")
                          (mapconcat #'ergoemacs-pretty-key-description--modifier
                                     mod "")
                          (ergoemacs-pretty-key-description--key ev mod)
                          (or (and ergoemacs-pretty-key-use-face "")
                              (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "】" "]"))
                              "]")))
        (when (and ergoemacs-use-small-symbols ergoemacs-pretty-key-use-face)
          (add-text-properties 0 (length tmp)
                               '(face ergoemacs-pretty-key) tmp))
        (setq ret (format "%s%s%s" ret
                          (or (and ergoemacs-pretty-key-use-face " ")
                              (and ergoemacs-use-unicode-brackets "")) tmp)))
      (substring ret (or (and ergoemacs-pretty-key-use-face 1)
                         (and ergoemacs-use-unicode-brackets 0))))))

(defun ergoemacs-pretty-key (code)
  "Creates Pretty keyboard binding from kbd CODE from M- to Alt+"
  (if (not code) ""
    (save-match-data
      (if (string-match "^\\(M-x\\|<execute>\\) " code)
          (if ergoemacs-use-M-x-p
              code
            (replace-match ergoemacs-M-x t t code))
        (ergoemacs-pretty-key-description (read-kbd-macro code t))))))

(defun ergoemacs-kbd-translate (kbd &optional just-first-keys variable-modifiers variable-prefixes layout-to layout-from)
  "Translates between ergoemacs-mode keyboard layouts.
KBD is the key.

VARIABLE-MODIFIERS are the modifiers that cause translation
between keyboards to occur.

VARIABLE-PREFIXES are the list of prefix keys that are variable.

JUST-FIRST-KEYS is a list of keys where the keyboard translation
stops.  For example when JUST-FIRST-KEYS is [apps ?n] would
translate QWERTY [apps ?n ?n] to colemak [apps ?k ?n] instead of
 [apps ?k ?k]
"
  (let ((var-mod variable-modifiers)
        (var-pre variable-prefixes)
        (ret [])
        (untranslated [])
        (just-first-keys (or (and (vectorp just-first-keys) (list just-first-keys))
                             just-first-keys))
        translated-event
        just-first-p
        translate-prefix-p
        basic modifiers)
    (dolist (event (listify-key-sequence kbd))
      (setq basic (ergoemacs-event-basic-type event layout-from)
            modifiers (ergoemacs-event-modifiers event layout-from))
      (unless translate-prefix-p
        (setq translate-prefix-p (member untranslated var-pre)))
      (when (and just-first-keys (not just-first-p))
        (setq just-first-p (member untranslated just-first-keys)))
      (cond
       ((and (or (catch 'found-modifiers
                   (dolist (m modifiers)
                     (when (memq m var-mod)
                       (throw 'found-modifiers t)))
                   nil)
                 translate-prefix-p)
             (not just-first-p))
        (setq translated-event
              (ergoemacs-event-translate event layout-to layout-from basic modifiers)))
       ((and (eq system-type 'windows-nt) (eq basic 'menu))
        (setq translated-event (ergoemacs-event-convert-list (append modifiers '(apps)))))
       ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
        (setq translated-event (ergoemacs-event-convert-list (append modifiers '(menu)))))
       (t (setq translated-event event)))
      (setq untranslated (vconcat untranslated (list event))
            ret (vconcat ret (list translated-event))))
    ret))

(provide 'ergoemacs-describe-key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-describe-key.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
