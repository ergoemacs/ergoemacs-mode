;;; ergoemacs-key-description.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-key-description.el
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

(require 'descr-text)
(require 'faces)

(defvar ergoemacs-display-unicode-characters)
(defvar ergoemacs-display-capitalize-keys)
(defvar ergoemacs-display-key-use-face-p)
(defvar ergoemacs-display-small-symbols-for-key-modifiers)
(defvar ergoemacs-display-use-unicode-brackets-around-keys)

(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifiers "ergoemacs-translate")
(declare-function ergoemacs-translate--event-basic-type "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifier-hash "ergoemacs-translate")

(defvar ergoemacs-key-description--display-char-cache nil
  "List of characters and fonts and if they display or not.")

(defun ergoemacs-key-description--display-char-p (char)
  "Determines if CHAR can be displayed."
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
                  (setq ret t))
            (if display
                (setq ret t))))
        (switch-to-buffer buf)
        ;; Save it so the user doesn't see the buffer popup very much
        ;; (if at all).
        (push (list (list face char window-system) ret) ergoemacs-key-description--display-char-cache)
        ret))))

(defun ergoemacs-key-description--unicode-char (char alt-char)
  "Uses CHAR if it can be displayed, otherwise use ALT-CHAR.
This assumes `ergoemacs-display-unicode-characters' is non-nil.  When
`ergoemacs-display-unicode-characters' is nil display ALT-CHAR"
  (if (and ergoemacs-display-unicode-characters (ergoemacs-key-description--display-char-p char))
      char
    alt-char))

(defun ergoemacs-key-description--key (key mod)
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
      (setq ret (format "%sBackspace" (ergoemacs-key-description--unicode-char "←" "left"))))
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret (format "Tab%s"
                        (ergoemacs-key-description--unicode-char "↹" ""))))
     ((eq key 'return)
      (setq ret (format "Enter%s"
                        (ergoemacs-key-description--unicode-char "↵" ""))))
     ((memq key '(apps menu))
      (setq ret (ergoemacs-key-description--unicode-char "▤" "Menu")))
     ((eq key 'left)
      (setq ret (ergoemacs-key-description--unicode-char "←" "left")))
     ((eq key 'right)
      (setq ret (ergoemacs-key-description--unicode-char "→" "right")))
     ((eq key 'up)
      (setq ret (ergoemacs-key-description--unicode-char "↑" "up")))
     ((eq key 'down)
      (setq ret (ergoemacs-key-description--unicode-char "↓" "down")))
     ((eq key 'prior)
      (setq ret "PgUp"))
     ((eq key 'next)
      (setq ret "PgDn"))
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
    (when (and ergoemacs-display-key-use-face-p
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (length ret)
                           '(face ergoemacs-display-key-face) ret))
    ret))

(defun ergoemacs-key-description--modifier (mod)
  "Modifier description"
  (let (ret)
    (cond
     ;; OSX specific key descriptions
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%s"
                        (ergoemacs-key-description--unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%sCmd+"
                        (ergoemacs-key-description--unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%sOpt+" (ergoemacs-key-description--unicode-char "⌥" "+"))))
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%s" (ergoemacs-key-description--unicode-char "⌥" "+"))))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'shift))
      (setq ret (ergoemacs-key-description--unicode-char "⇧" "+")))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'meta))
      (setq ret (ergoemacs-key-description--unicode-char "♦" "!")))
     ((and (or (eq system-type 'darwin) ergoemacs-display-small-symbols-for-key-modifiers)
           (memq mod '(control ergoemacs-control)))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret (format "%sShift+"
                        (ergoemacs-key-description--unicode-char "⇧" ""))))
     ((memq mod '(control ergoemacs-control))
      (setq ret "Ctrl+"))
     ((eq mod 'meta)
      (setq ret "Alt+"))
     ((and (eq mod 'super) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'windows-nt))
      (setq ret (ergoemacs-key-description--unicode-char "⊞" "#")))
     ((and (eq mod 'super)
           (eq system-type 'windows-nt))
      (setq ret (format "%sWin+" (ergoemacs-key-description--unicode-char "⊞" "#"))))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-display-key-use-face-p
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-display-key-face) ret))))
    (when (and ergoemacs-display-key-use-face-p
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (- (length ret) 1)
                           '(face ergoemacs-display-key-face) ret))
    ret))

(defun ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers (mod)
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

(defun ergoemacs-key-description (kbd &optional layout)
  "Creates Pretty keyboard binding from kbd from M- to Alt+"
  (let ((kbd (or (ergoemacs-translate--escape-to-meta kbd) kbd)))
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
                  ev (gethash (intern (format "s%s" ev))
                              (ergoemacs-translate--event-modifier-hash layout)))))
          (setq tmp (format "%s%s%s%s"
                            (or (and ergoemacs-display-key-use-face-p "")
                                (and ergoemacs-display-use-unicode-brackets-around-keys (ergoemacs-key-description--unicode-char "【" "["))
                                "[")
                            (mapconcat #'ergoemacs-key-description--modifier
                                       mod "")
                            (ergoemacs-key-description--key ev mod)
                            (or (and ergoemacs-display-key-use-face-p "")
                                (and ergoemacs-display-use-unicode-brackets-around-keys (ergoemacs-key-description--unicode-char "】" "]"))
                                "]")))
          (when (and ergoemacs-display-small-symbols-for-key-modifiers ergoemacs-display-key-use-face-p)
            (add-text-properties 0 (length tmp)
                                 '(face ergoemacs-display-key-face) tmp))
          (setq ret (format "%s%s%s" ret
                            (or (and ergoemacs-display-key-use-face-p " ")
                                (and ergoemacs-display-use-unicode-brackets-around-keys "")) tmp)))
        (substring ret (or (and ergoemacs-display-key-use-face-p 1)
                           (and ergoemacs-display-use-unicode-brackets-around-keys 0)))))))

(defun ergoemacs-key-description-kbd (code)
  "Creates `ergoemacs-mode' style description of kbd macro CODE"
  (if (not code) ""
    (save-match-data
      (ergoemacs-key-description (read-kbd-macro code t)))))



(provide 'ergoemacs-key-description)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-key-description.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
