;;; ergoemacs-translate.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2018  Free Software Foundation, Inc.

;; Filename: ergoemacs-translate.el
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

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))


(defvar ergoemacs-define-key-after-p)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keyboard-mirror)
(defvar ergoemacs-translation-hash)
(defvar ergoemacs-translate--hash)
(defvar ergoemacs-translate--event-hash)
(defvar ergoemacs-dir)
(defvar ergoemacs-theme)
(defvar ergoemacs-inkscape)
(defvar ergoemacs-command-loop--universal-functions)

(declare-function ergoemacs-layouts--list "ergoemacs-layouts")
(declare-function ergoemacs-theme--list "ergoemacs-theme-engine")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-layouts--custom-documentation "ergoemacs-layouts")
(declare-function ergoemacs-theme--custom-documentation "ergoemacs-theme-engine")

(declare-function ergoemacs-mode-line "ergoemacs-mode")

(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")
(declare-function ergoemacs-key-description-kbd "ergoemacs-key-description")
(declare-function ergoemacs-key-description--display-char-p "ergoemacs-key-description")

(declare-function ergoemacs-layouts--current "ergoemacs-layouts")

(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label-map "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")


(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-command-loop--modal-p "ergoemacs-command-loop")

(declare-function ergoemacs-translate--key-description "ergoemacs-translate")

(fset #'ergoemacs-translate--key-description (symbol-function #'key-description))

(defun ergoemacs-translate--get-hash (&optional layout-to layout-from)
  "Gets the translation hash."
  (let* ((to (ergoemacs :layout  (or layout-to ergoemacs-keyboard-layout)))
         (from (ergoemacs :layout  (or layout-from "us")))
         (hash-f (ergoemacs-gethash from ergoemacs-translate--hash (make-hash-table)))
         (hash-f-t (ergoemacs-gethash to hash-f))
         (i 0)
         hash-t hash-t-f lay-t lay-f r-t r-f)
    (if hash-f-t hash-f-t
      (setq hash-f-t (make-hash-table)
            hash-t (ergoemacs-gethash to ergoemacs-translate--hash (make-hash-table))
            hash-t-f (make-hash-table)
            lay-t (symbol-value to)
            lay-f (symbol-value from))
      (while (< i 120)
        (unless (or (string= "" (nth i lay-t))
                    (string= "" (nth i lay-f)))
          (setq r-t (aref (read-kbd-macro (nth i lay-t) t) 0)
                r-f (aref (read-kbd-macro (nth i lay-f) t) 0))
          (puthash r-t r-f hash-t-f)
          (puthash r-f r-t hash-f-t))
        (setq i (+ i 1)))
      (puthash from hash-t-f hash-t)
      (puthash to hash-f-t hash-f)
      (puthash to hash-t ergoemacs-translate--hash)
      (puthash from hash-f ergoemacs-translate--hash)
      hash-f-t)))

(defun ergoemacs-translate--emacs-shift (key-seq &optional modifier prefix)
  "Uses emacs style shift-translation: M-Q becomes M-q.

KEY-SEQ must be a vector.  If there is no need to shift-translate
the key sequence return nil.

Optionally you can change how this function behaves.

Instead of translating the shifted key to the unshifted key, you
can remove another modifier.  For example if you wanted to
convert C-M-a to C-a, you could use 'meta as the MODIFIER
argument to remove the M- modifier.

The PREFIX argument can add a key before the key where the
modifier occurred, such as in `ergoemacs-translate--meta-to-escape'.
"
  (if (not (vectorp key-seq)) nil
    (let ((rev-seq (reverse (append key-seq ())))
          (which-mod (or modifier 'shift))
          modifiers new-mod
          found 
          (seq '()))
      (dolist (event rev-seq)
        (setq modifiers (ergoemacs-translate--event-modifiers event))
        (if (not (memq which-mod modifiers)) (push event seq)
          (setq new-mod (list (ergoemacs-translate--event-basic-type event)))
          (dolist (mod modifiers)
            (unless (eq which-mod mod)
              (push mod new-mod)))
          (push (ergoemacs-translate--event-convert-list new-mod) seq)
          (when prefix
            (push prefix seq))
          (setq found t)))
      (if found (vconcat seq) nil))))

(defun ergoemacs-translate--meta-to-escape (key-seq)
  "Escapes a KEY-SEQ M-q becomes ESC q.
KEY-SEQ must be a vector.  If there is no need to escape the key sequence return nil."
  (ergoemacs-translate--emacs-shift key-seq 'meta 27))

(defun ergoemacs-translate--escape-to-meta (key-seq)
  "Changes key sequences ESC q to M-q.
KEY-SEQ must be a vector or string.  If there is no need to change the sequence, return nil."
  (let ((key-seq (or (and (vectorp key-seq) key-seq)
                     (vconcat key-seq))))
    (let ((rev-seq (reverse (append key-seq ())))
          old-event
          modifiers
          found
          seq)
      (dolist (event rev-seq)
        ;; [27 134217736] -> nil
        ;; [27 8] -> [134217832]
        ;; [27 8 27] -> [134217832 27]
        ;; [27 27 8 27] -> [27 134217832 27]
        ;; [27 9] -> [134217737]
        (cond
         ((and (eq 27 event) seq)
          (setq old-event (pop seq)
                modifiers (event-modifiers old-event))
          (if (memq 'meta modifiers)
              (progn
                (push old-event seq)
                (push event seq))
            (setq found t)
            (push (event-convert-list (append '(meta) modifiers (list (event-basic-type old-event)))) seq)))
         (t
          (push event seq))))
      (and found (vconcat seq)))))

(defun ergoemacs-translate--swap-apps (key &optional what with)
  "In KEY, swap apps key with menu key.
Optionally specify WHAT you want to replace WITH.

If no changes have been done, return nil."
  (let ((seq (reverse (append key ())))
        (what (or what 'apps))
        (with (or with 'menu))
        found-p
        ret)
    (dolist (e seq)
      (cond
       ((eq e what)
        (push with ret)
        (setq found-p t))
       (t (push e ret))))
    (if found-p
        (vconcat ret)
      nil)))

(defun ergoemacs-translate--swap-menu (key)
  "In KEY swap menu key with apps key."
  (ergoemacs-translate--swap-apps key 'menu 'apps))

(defun ergoemacs-translate--to-vector (key)
  "Translates KEY to vector format.

If no changes are performed, return nil."
  (when (stringp key)
    (let ((new-key (vconcat key))
          ret)
      (unless (equal key new-key)
        (setq ret new-key))
      ret)))

(defun ergoemacs-translate--to-string (key)
  "Translates KEY to string format.

If no chanegs are performed, return nil."
  (catch 'not-ascii
    (mapconcat
     (lambda(key)
       (if (and (integerp key) (< key 256))
           (make-string 1 key)
         (throw 'not-ascii nil)))
     (append key) "")))

(defvar ergoemacs-translate--apply-funs
  '(ergoemacs-translate--escape-to-meta
    ergoemacs-translate--meta-to-escape
    ergoemacs-translate--swap-apps
    ergoemacs-translate--swap-menu
    ergoemacs-translate--to-string
    ergoemacs-translate--to-vector)
  "Functions to apply to key.

These functions take a key as an argument and translate it in
some way.  If there is no appropriate translation, the function
should return nil.")

(defun ergoemacs-translate--apply-key (key function &rest args)
  "Apply KEY to FUNCTION with ARGS.
In addition to the normal KEY variants are also applied.  These
variants are created using `ergoemacs-translate--apply-funs'."
  (let ((key key) test-key)
    (apply function key args)
    (dolist (fn ergoemacs-translate--apply-funs)
      (when (setq test-key (funcall fn key))
        (apply function test-key args)))))

(defun ergoemacs-translate--define-key (keymap key def)
  "Similar to `define-key', with the following differences:
- Both the Meta and escape sequences are bound.
- Both <apps> and <menu> key sequences are bound.
- `ergoemacs-mode' advice to `define-key' is supressed.

KEYMAP is the keymap that will be used for the definition.
KEY is the key that is Emacs key that will be defined.
DEF is the definition of what will be run.

This uses `ergoemacs-translate--apply-key'"
  (unwind-protect
      (ergoemacs-translate--apply-key key (lambda(new-key) (define-key keymap new-key def)))
    (setq ergoemacs-define-key-after-p nil)))

(defun ergoemacs-translate--event-modifier-hash (&optional layout)
  "Gets the event modifier hash for LAYOUT."
  (let* ((layout-symbol (ergoemacs :layout  layout))
         (hash (ergoemacs-gethash layout-symbol ergoemacs-translate--event-hash)))
    (if hash hash
      ;; Not present setup modifier hash
      (setq hash (make-hash-table))
      (let ((lay (symbol-value layout-symbol))
            (i 0)
            r1 r2)
        (while (< i 60)
          (unless (or (string= "" (nth i lay))
                      (string= "" (nth (+ i 60) lay)))
            (setq r1 (aref (read-kbd-macro (nth i lay) t) 0)
                  r2 (aref (read-kbd-macro (nth (+ i 60) lay) t) 0))
            (unless (eq (event-basic-type r1) (event-basic-type r2))
              ;; This shifted expression isn't covered by basic emacs.
              (puthash r2 r1 hash)
              (puthash (intern (format "s%s" r1)) r2 hash)))
          (setq i (+ i 1)))
        (puthash layout-symbol hash ergoemacs-translate--event-hash))
      hash)))

(defun ergoemacs-translate--event-modifiers (event &optional layout)
  "Return a list of symbols representing the modifier keys in event EVENT.
This is different than `event-modifiers' in two ways:
- Symbol keys like # will return 'ergoemacs-shift for a QWERTY keyboard.
- Special keys like C-RET will return 'ergoemacs-control
LAYOUT is the keyboard layout."
  (let ((modifiers (event-modifiers event))
	tmp 
        basic)
    (unless (memq 'shift modifiers)
      ;; Add 'shift for # type events.
      (setq basic (event-basic-type event))
      (when (ergoemacs-gethash basic (ergoemacs-translate--event-modifier-hash layout))
        (push 'ergoemacs-shift modifiers)))
    ;; Add 'ergoemacs-gui to the modifiers
    (when (and (symbolp event)
	       (setq tmp (symbol-name event))
	       (char-equal (aref (substring tmp -2 -1) 0) ?-)
	       (memq (aref (substring tmp -1) 0)
		     (list ?\[ ?i ?m)))
      (push 'ergoemacs-gui modifiers))
    ;; Also add 'ergoemacs-control to C-RET which translates to C-m
    (when (and (integerp event)
               (> event 1000)
               (memq 'control modifiers))
      (unless basic
        (setq basic (event-basic-type event)))
      (cond
       ((and (memq basic '(?m ?\[))
	     (string-match-p "C-" (ergoemacs-translate--key-description (vector event))))
        (push 'ergoemacs-control modifiers))
       ((and (eq basic ?i)
             (string-match-p "C-.*TAB" (ergoemacs-translate--key-description (vector event))))
        (push 'ergoemacs-control modifiers))))
    modifiers))

(defun ergoemacs-translate--event-layout (event &optional layout-to layout-from basic modifiers)
  "Translate EVENT to the appropriate keyboard layout.

EVENT can be an event, or a vector.  When EVENT is a vector,
each of the events within the vector/key are translated.

LAYOUT-TO is the layout to translate to, (default
`ergoemacs-keyboard-layout')

LAYOUT-FROM is the layout to translate from, (default: \"us\")

BASIC is the precalculated basic event from
`ergoemacs-translate--event-basic-type'.

MODIFIERS is the precalculated modifiers from
`ergoemacs-translate--event-modifiers'."
  (if (vectorp event)
      (progn
        (apply 'vector (mapcar (lambda(x) (ergoemacs-translate--event-layout x layout-to layout-from basic modifiers)) event)))
    (let* ((basic (or basic (ergoemacs-translate--event-basic-type event layout-from)))
           (modifiers (or modifiers (ergoemacs-translate--event-modifiers event layout-from)))
           new-modifiers
           new-event
           (translation-hash (ergoemacs-translate--get-hash layout-to layout-from)))
      (cond
       ((and (eq system-type 'windows-nt) (eq basic 'menu))
        (setq basic 'apps))
       ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
        (setq basic 'menu)))
      (if (memq 'ergoemacs-control modifiers)
          (setq new-event basic
                new-modifiers modifiers)
        (if (or (memq 'shift modifiers)
                (memq 'ergoemacs-shift modifiers))
            (dolist (m modifiers)
              (if (not (memq m '(shift ergoemacs-shift)))
                  (push m new-modifiers)
                (setq new-event (ergoemacs-translate--event-convert-list (list m basic) layout-from))
                (setq new-event (or (ergoemacs-gethash new-event translation-hash) new-event))))
          (setq new-event (or (ergoemacs-gethash basic translation-hash) basic)
                new-modifiers modifiers)))
      (ergoemacs-translate--event-convert-list (append new-modifiers (list new-event)) layout-to))))

(defun ergoemacs-translate--event-basic-type (event &optional layout)
  "Return the basic type of the given event (all modifiers removed).
This is different than `event-basic-type' because ?# would return
?3 on a QWERTY LAYOUT.

This also translates <C-i> to ?i, <C-m> to ?m <C-[> to ?[
"
  (let* ((basic (event-basic-type event))
         (new-basic (and basic (ergoemacs-gethash basic (ergoemacs-translate--event-modifier-hash layout)))))
    (or new-basic basic
        (and (symbolp event)
             (setq basic (symbol-name event))
             (string-match "\\([im[]\\)$" basic)
             (aref (match-string 1 basic) 0)))))

(defun ergoemacs-translate--event-convert-list (list &optional layout)
   "Convert the event description LIST to an event type.
This is different than `event-convert-list' because:
 -  '(shift ?3) or '(ergoemacs-shift ?3) produces ?# on a QWERTY LAYOUT.
 -  '(ergoemacs-control control ?m) produces C-RET
 -  '(ergoemacs-gui control ?m) produces <C-m>. this applies for ?i and ?[ as well.
 - Mouse events allow click modifiers"
  (let ((cur-list list)
        elt
        tmp
        control-p
        new-list
        first second base
        (gui-p (memq 'ergoemacs-gui list)))
    (when (or gui-p (setq control-p (memq 'ergoemacs-control cur-list)))
      (setq cur-list (reverse cur-list))
      (if (and gui-p (memq (car cur-list) (list '\[ 'm 'i ?\[ ?m ?i))
               (memq 'control cur-list))
          (setq gui-p (pop cur-list))
        (setq gui-p nil))
      (dolist (elt cur-list)
        (unless (memq elt '(ergoemacs-control ergoemacs-gui))
          (push elt new-list)))
      (setq cur-list new-list)
      (setq new-list nil))
    
    (if (not (or (memq 'shift list) (memq 'ergoemacs-shift list)))
        (setq new-list cur-list)
      (while (> (length cur-list) 0)
        (setq elt (pop cur-list))
        (cond
         ((and cur-list (memq elt '(shift ergoemacs-shift))))
         
         ((and (not cur-list)
               (setq tmp (ergoemacs-gethash (intern (format "s%s" elt))
                                  (ergoemacs-translate--event-modifier-hash layout))))
          ;; Special case.
          (setq new-list (append new-list (list tmp))))
         ((not cur-list)
          (push 'shift new-list)
          (setq new-list (append new-list (list elt))))
         (t
          (push elt new-list)))))
    (setq new-list (reverse new-list)
          base (pop new-list)
          tmp nil)
    (dolist (elt new-list)
      (cond
       ((memq elt '(double triple))
        (setq first (format "%s-" elt)))
       ((memq elt '(click)))
       ((memq elt '(drag down))
        (setq second (format "%s-" elt)))
       (t (push elt tmp))))
    
    (when (or first second)
      (setq base (intern (format "%s%s%s" (or first "") (or second "") base))))
    (setq new-list (append tmp (list base)))
    (cond
     (gui-p (aref (read-kbd-macro (concat (substring (ergoemacs-translate--key-description (vector (event-convert-list (append new-list (list 'ack))))) 0 -4) (or (and (symbolp gui-p) (symbol-name gui-p)) (make-string 1 gui-p)) ">")) 0))
     (control-p (aref (read-kbd-macro (concat "C-" (ergoemacs-translate--key-description (vector (event-convert-list new-list)))) t) 0))
     (t (event-convert-list new-list)))))


(defun ergoemacs-translate (kbd &optional just-first-keys variable-modifiers
                                variable-prefixes layout-to layout-from)
  "Translate between ergoemacs-mode keyboard layouts.

KBD is the key.

JUST-FIRST-KEYS is a list of keys where the keyboard translation
stops.  For example when JUST-FIRST-KEYS is [apps ?n] would
translate QWERTY [apps ?n ?n] to colemak [apps ?k ?n] instead of
 [apps ?k ?k]. 

VARIABLE-MODIFIERS are the modifiers that cause translation
between keyboards to occur.


VARIABLE-PREFIXES are the list of prefix keys that are variable.

LAYOUT-TO is the layout that `ergoemacs-mode' will translate
to.  By default it is the layout specified by
`ergoemacs-keyboard-layout'.

LAYOUT-FROM is the layout that `ergoemacs-mode' will translate
from.  By default this is the us layout.

This uses the function `ergoemacs-translate--event-layout' to
make the translation."
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
      (setq basic (ergoemacs-translate--event-basic-type event layout-from)
            modifiers (ergoemacs-translate--event-modifiers event layout-from))
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
              (ergoemacs-translate--event-layout event layout-to layout-from basic modifiers)))
       ((and (eq system-type 'windows-nt) (eq basic 'menu))
        (setq translated-event (ergoemacs-translate--event-convert-list (append modifiers '(apps)))))
       ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
        (setq translated-event (ergoemacs-translate--event-convert-list (append modifiers '(menu)))))
       (t (setq translated-event event)))
      (setq untranslated (vconcat untranslated (list event))
            ret (vconcat ret (list translated-event))))
    ret))

(defun ergoemacs-translate--event-trials (event &optional exclude-basic extra-modifiers mirror-p)
  "Gets a list of `ergoemacs-mode' event trials.
When EXCLUDE-BASIC is non-nil, don't include the keys that are likely to produce a character when typing.

When MIRROR-P is non-nil, this is another call to the event trials for a keyboard mirror."
  (if (not (eventp event))
      (error "Need an event for event trials (%s)" event))
  (let* ((key event)
         (ret '())
         (basic (ergoemacs-translate--event-basic-type key))
         (gui-p (and (symbolp key) (memq basic '(m i \[ ?m ?i ?\[))))
         (extra-modifiers extra-modifiers)
         trial)
    (when gui-p
      (push 'ergoemacs-gui extra-modifiers))
    (unless exclude-basic
      (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list basic))))
      (unless (eq trial key)
        (push trial ret))

      (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'shift basic))))
      (unless (eq trial key)
        (push trial ret)))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'control basic))))
    (unless (eq trial key)
      (push trial ret))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'meta basic))))
    (unless (eq trial key)
      (push trial ret))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'meta 'control basic))))
    (unless (eq trial key)
      (push trial ret))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'shift 'control basic))))
    (unless (eq trial key)
      (push trial ret))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'shift 'meta basic))))
    (unless (eq trial key)
      (push trial ret))

    (setq trial (ergoemacs-translate--event-convert-list (append extra-modifiers (list 'shift 'meta 'control basic))))
    (unless (eq trial key)
      (push trial ret))
    
    (unless extra-modifiers
      (setq ret (append (ergoemacs-translate--event-trials event exclude-basic (list 'hyper)) ret))
      (setq ret (append (ergoemacs-translate--event-trials event exclude-basic (list 'super)) ret))
      (setq ret (append (ergoemacs-translate--event-trials event exclude-basic (list 'hyper 'super)) ret)))

    (when gui-p
      (setq extra-modifiers (cdr extra-modifiers)
            event (ergoemacs-translate--event-convert-list (append extra-modifiers (list basic))))
      (setq ret (append (ergoemacs-translate--event-trials event exclude-basic nil) ret)))
    (when (and (not mirror-p) ergoemacs-keyboard-mirror)
      (setq ret (append ret
			(ergoemacs-translate--event-trials
			 (ergoemacs-translate--event-layout event (format "%s" ergoemacs-keyboard-layout) (format "%s" ergoemacs-keyboard-mirror))
			 exclude-basic extra-modifiers t)
			(ergoemacs-translate--event-trials
                         (ergoemacs-translate--event-layout event (format "%s" ergoemacs-keyboard-mirror) (format "%s" ergoemacs-keyboard-layout))
			 exclude-basic extra-modifiers t))))
    ret))

(defun ergoemacs-translate--trials (key)
  "Get the `ergoemacs-mode' keys to lookup for KEY.
For keys, the list consists of:
- 1: The key itself
- 2: The `ergoemacs-mode' shift translation (if needed/applicable or nil) 
- 3: The other translations"
  ;; (nth 1 (ergoemacs-translate--trials (kbd "M-A"))) = (kbd "M-a")
  (let* ((key (vconcat key))
         (event (elt (substring key -1) 0))
         (base (substring key 0 -1))
         ret)
    (if (consp event)
        (let ((mod (event-modifiers event))
              (basic (event-basic-type event))
              double-p
              triple-p
              strip-mod)
          (when (or (setq double-p (memq 'double mod))
                    (setq triple-p (memq 'triple mod)))
            (dolist (a mod)
              (unless (memq a '(double triple))
                (push a strip-mod))))
          (cond
           (double-p
            ;; Double -> single
            (push (vector (list (event-convert-list (append strip-mod (list basic)))
                                (cdr event)))
                  ret)
            (push nil ret)
            (push key ret))
           (triple-p
            ;; double -> single
            (push (vector (list (event-convert-list (append strip-mod (list basic)))
                                (cdr event)))
                  ret)
            ;; triple->double
            (push (vector (list (event-convert-list (append strip-mod (list 'double basic)))
                                (cdr event)))
                  ret)
            (push nil ret)
            (push key ret))
           (t
            ;; Do not put any trials in just use the mouse event.
            (push key ret)))
          ret)
      (dolist (new (ergoemacs-translate--event-trials event (= 1 (length key))))
        (push (vconcat base (vector new)) ret))
      (when (memq 'ergoemacs-gui (ergoemacs-translate--event-modifiers event))
	(push (ergoemacs-translate--no-gui key) ret))
      (if (= 1 (length key))
          (push (or (ergoemacs-translate--emacs-shift key 'shift)
                    (ergoemacs-translate--emacs-shift key 'ergoemacs-shift)) ret)
        (push nil ret) ;; No shift translation
        )
      
      (push key ret))
    ret))

(defstruct ergoemacs-translation-struct
  "A basic ergoemacs translation structure."
  (name "default-name")
  (translation '())
  (universal-argument nil)
  (negative-argument nil)
  (digit-argument nil)
  (modal nil)
  (text "")
  (keymap (make-sparse-keymap))
  (keymap-modal (make-sparse-keymap))
  (modal-always nil)
  (modal-color nil)
  (key nil)
  (unchorded nil))

(defvar ergoemacs-translate--setup-command-loop-regexp
  "^\\(?:ergoemacs\\(?:-translate-\\)?\\)-\\(.*?\\)-\\(universal-argument\\|negative-argument\\|digit-argument\\|modal\\)$"
  "Command loop command match/setup regular expression.")

(defun ergoemacs-translate--setup-command-loop ()
  "Setup command loop.
To do anything, `this-command' must match
`ergoemacs-translate--setup-command-loop-regexp'.  The first
match is the NAME of the translation, the second match is the
TYPE of command.  This command will then
call (ergoemacs-command-loop-TYPE :NAME)."
  (interactive)
  (let ((command-str (symbol-name this-command))
	name type)
    (save-match-data
      (when (string-match ergoemacs-translate--setup-command-loop-regexp command-str)
	(setq name (match-string 1 command-str)
	      type (match-string 2 command-str))
	(funcall (intern (concat "ergoemacs-command-loop--" type)) (intern (concat ":" name)))))))

(defun ergoemacs-translate--setup-translation (&optional name)
  "Setup translation functions and keymaps.
If NAME is nil, setup all translations.
When NAME is a symbol, setup the translation function for the symbol."
  (if (not name)
      (maphash
       (lambda(name _item)
	 (ergoemacs-translate--setup-translation name))
       ergoemacs-translation-hash)
    (let ((name-str (and (symbolp name) (substring (symbol-name name) 1))))
      (eval
       (macroexpand
	`(progn
	   (defvar ,(intern (concat "ergoemacs-translate--" name-str "-map")) (make-sparse-keymap)
	     ,(concat "Ergoemacs local map for translation :"
		      name-str
		      " while completing a key sequence."))
	   (define-obsolete-variable-alias ',(intern (concat "ergoemacs-" name-str "-translation-local-map"))
             ',(intern (concat "ergoemacs-translate--" name-str "-map"))))))
      (ergoemacs-map-properties--label-map (intern (concat "ergoemacs-translate--" name-str "-map")) t)
      (ergoemacs (symbol-value (intern (concat "ergoemacs-translate--" name-str "-map"))) :only-local-modifications-p t)
      ;; 
      (dolist (type '("-universal-argument" "-negative-argument"
                      "-digit-argument" "-modal"))
	(fset (intern (concat "ergoemacs-translate--" name-str type))
	      'ergoemacs-translate--setup-command-loop)
	(fset (intern (concat "ergoemacs-" name-str type))
	      'ergoemacs-translate--setup-command-loop)
	(when (string= type "-universal-argument")
	  (pushnew (intern (concat "ergoemacs-" name-str type)) ergoemacs-command-loop--universal-functions)
	  (pushnew (intern (concat "ergoemacs-translate--" name-str type)) ergoemacs-command-loop--universal-functions))))))

(add-hook 'ergoemacs-mode-intialize-hook #'ergoemacs-translate--setup-translation)

(defun ergoemacs-translate--create (&rest plist)
  "Create a translation from PLIST and return translation object."
  (let ((plist plist)
        struct
        -universal-argument
        -negative-argument
        -digit-argument
        -modal
        translation
        (local-keymap (or (plist-get plist :keymap) (make-sparse-keymap)))
	(trans-keymap (intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map"))))
    (when (ergoemacs-keymapp trans-keymap)
      (set-keymap-parent (setq trans-keymap (symbol-value trans-keymap)) local-keymap)
      (ergoemacs trans-keymap :only-local-modifications-p t))
    ;; (ergoemacs :label local-keymap)
    ;; (ergoemacs local-keymap :only-local-modifications-p t)
    (let (tmp
          cur-trans
          ret)
      (dolist (elt plist)
        (cond
         ((and (symbolp elt)
               (progn
                 (setq cur-trans nil)
                 (string-match-p "\\(hyper\\|super\\|shift\\|meta\\|control\\|alt\\|cn?tr?l\\)"
                                 (setq tmp (symbol-name elt)))))
          (while (string-match "\\(hyper\\|super\\|shift\\|meta\\|control\\)" tmp)
            (cond
             ((string-match-p "cn?tr?l" (match-string 1 tmp))
              (push 'conrtol cur-trans))
             ((string= "alt" (match-string 1 tmp))
              (push 'meta cur-trans))
             (t
              (push (intern (match-string 1 tmp)) cur-trans)
              (setq tmp (replace-match "" t t tmp))))))
         (cur-trans
          (cond
           ((consp elt) ;; Assume list is correct.
            (push (list (sort cur-trans #'string<) elt) ret))
           ((stringp elt)
            ;; Allow "C-M-" syntax (backward compatible)
            (push (list (sort cur-trans #'string<) (event-modifiers (elt (read-kbd-macro (concat elt "q") t) 0))) ret)))
          (setq cur-trans nil))))
      (when (setq tmp (plist-get plist :unchorded))
        (push (list nil tmp) ret))
      (setq translation ret))
    (setq struct
          (make-ergoemacs-translation-struct
           :name (plist-get plist :name)
           :translation translation
           :universal-argument -universal-argument
           :negative-argument -negative-argument
           :digit-argument -digit-argument
           :modal -modal
           :text (plist-get plist :text)
           :keymap local-keymap
           :keymap-modal (or (plist-get plist :keymap-modal) (make-sparse-keymap))
           :modal-always (plist-get plist :modal-always)
           :modal-color (plist-get plist :modal-color)
           :key (plist-get plist :key)
           :unchorded (plist-get plist :unchorded)))
    (puthash (plist-get plist :key) struct ergoemacs-translation-hash)
    struct))

(defun ergoemacs-translate--get (type)
  "Get translation object TYPE."
  (let ((ret (ergoemacs-gethash type ergoemacs-translation-hash)))
    (cond
     ((and ret (ergoemacs-translation-struct-p ret))
      ret)
     ((and ret (functionp ret))
      (setq ret (funcall ret)))
     (t
      (error "Somethings wrong; cant get translation %s" type)))))

(defun ergoemacs-translate--event-mods (event &optional type)
  "Translate EVENT modifiers by the translation TYPE.
If TYPE is unspecified, assume :normal translation"
  (let* ((type (or type :normal))
         (translation (and (symbolp type) (ergoemacs-translate--get type)))
         (basic (ergoemacs-translate--event-basic-type event))
         (e-mod (ergoemacs-translate--event-modifiers event))
         (modifiers (sort (mapcar
                           (lambda(e)
                             (cond
                              ((eq 'ergoemacs-shift e) 'shift)
                              ((eq 'ergoemacs-control e) 'control)
                              (t e)))
                           e-mod) #'string<))
         (special-p (memq basic (list ?m ?i ?\[)))
         (ambiguous-p (and special-p (memq 'control e-mod)))
         tmp
         (ret event))
    (when ambiguous-p
      (dolist (m modifiers)
        (unless (eq 'control m)
          (push m tmp)))
      (when (memq 'ergoemacs-control e-mod)
        (push 'control tmp))
      (setq modifiers tmp))
    (when (catch 'found-mod
            (dolist (mod (or (and (ergoemacs-translation-struct-p translation)
                                  (ergoemacs-translation-struct-translation translation))
                             (and (consp type) (consp (nth 0 type)) type)
                             (and (consp type) (list (list nil type)))))
              (when (equal (nth 0 mod) modifiers)
                (setq modifiers (nth 1 mod))
                (cond
                 ((and ambiguous-p
                       (memq 'control modifiers))
                  (push 'ergoemacs-control modifiers))
                 ((and special-p (display-graphic-p)
                       (memq 'control modifiers))
                  (push 'ergoemacs-gui modifiers)))
                (throw 'found-mod t))) nil)
      (if ambiguous-p
          (setq ret (ergoemacs-translate--event-convert-list `(control ,@modifiers ,basic)))
        (setq ret (ergoemacs-translate--event-convert-list `(,@modifiers ,basic)))))
    ret))

(defun ergoemacs-translate--no-gui (event)
  "Remove any gui elements to the EVENT.
If there are no gui elements, return nil."
  (if (vectorp event)
      (apply #'vector (mapcar (lambda (x) (or (ergoemacs-translate--no-gui x) x)) event))
    (let* ((last-event event)
	   (last-mod (ergoemacs-translate--event-modifiers last-event))
	   (last-basic-event (ergoemacs-translate--event-basic-type last-event))
	   new-mod)
      (when (memq 'ergoemacs-gui last-mod)
	(dolist (elt last-mod)
	  (unless (eq elt 'ergoemacs-gui)
	    (push elt new-mod)))
        (ergoemacs-translate--event-convert-list `(,@new-mod ,last-basic-event))))))

(defvar ergoemacs-translate--parent-map nil
  "Parent map for keymaps when completing a key sequence.")

(defun ergoemacs-translate--parent-map ()
  (or ergoemacs-translate--parent-map
      (let ((map (make-sparse-keymap)))
	(setq ergoemacs-translate--parent-map map)
        (ergoemacs map :label)
        (ergoemacs map :only-local-modifications-p t)
        (ergoemacs map :map-list-hash '(ergoemacs-translate--parent-map))
        map)))

(add-hook 'ergoemacs-mode-intialize-hook #'ergoemacs-translate--parent-map)


(defvar ergoemacs-translate--modal-parent-map nil
  "Parent map for modal `ergoemacs-mode'")

(defun ergoemacs-translate--modal-parent-map ()
  (or ergoemacs-translate--modal-parent-map
      (let ((map (make-sparse-keymap)))
	(setq ergoemacs-translate--modal-parent-map map)
        (ergoemacs map :label)
        (ergoemacs map :only-local-modifications-p t)
        (ergoemacs map :map-list-hash '(ergoemacs-translate--modal-parent-map))
        map)))
(add-hook 'ergoemacs-mode-intialize-hook #'ergoemacs-translate--modal-parent-map)

(defvar ergoemacs-translate--keymap-hash (make-hash-table)
  "Translation keymaps")

(defun ergoemacs-translate--keymap-reset ()
  "Reset `ergoemacs-translate--keymap-hash'"
  (setq ergoemacs-translate--keymap-hash (make-hash-table)))

;; (add-hook 'ergoemacs-mode-intialize-hook #'ergoemacs-translate--keymap-reset)

(defun ergoemacs-translate--keymap (&optional translation)
  "Get the keymap for TRANSLATION.
This takes into consideration the modal state of `ergoemacs-mode'."
  (let* ((modal (ergoemacs :modal-p))
         (translation (or (and (ergoemacs-translation-struct-p translation)
                               (or (not modal) ;; prefer modal when :normal 
                                   (not (eq :normal (ergoemacs-translation-struct-key translation))))
                               translation)
                          modal
                          (ergoemacs-translate--get (or translation :normal))))
         (key (or (and modal (intern (concat ":" (ergoemacs-translation-struct-name translation) "-modal")))
                  (ergoemacs-translation-struct-key translation)))
         (ret (ergoemacs-gethash key ergoemacs-translate--keymap-hash))
         keymap)
    (unless ret
      (if modal
          (setq keymap (ergoemacs-translation-struct-keymap-modal translation)
                ret keymap)
        (setq keymap (ergoemacs-translation-struct-keymap translation)
              ret (make-composed-keymap (ergoemacs keymap) (ergoemacs ergoemacs-translate--parent-map))))
      (puthash key ret ergoemacs-translate--keymap-hash))
    ret))

(defun ergoemacs-translate--ergoemacs-to-quail (layout)
  "Translates an ergoemacs-mode layout to a quail layout."
  (let ((lay layout)
        (ret (make-string 32 ? ))
        (i 2))
    (while (< i 14)
      (setq ret (concat ret (or (and (string= "" (nth i lay)) " ")
                                (nth i lay))
                        (or (and (string= "" (nth (+ i 60) lay)) " ")
                            (nth (+ i 60) lay)))
            i (+ i 1)))
    (setq i 1
          ret (concat ret (or (and (string= "" (nth i lay)) " ")
                              (nth i lay))
                      (or (and (string= "" (nth (+ i 60) lay)) " ")
                          (nth (+ i 60) lay))
                      (make-string 4 ? ))
          i 17)
    (while (< i 29)
      (setq ret (concat ret (or (and (string= "" (nth i lay)) " ")
                                (nth i lay))
                        (or (and (string= "" (nth (+ i 60) lay)) " ")
                            (nth (+ i 60) lay)))
            i (+ i 1)))
    (setq ret (concat ret (make-string 6 ? ))
          i 32)
    (while (< i 43)
      (setq ret (concat ret (or (and (string= "" (nth i lay)) " ")
                                (nth i lay))
                        (or (and (string= "" (nth (+ i 60) lay)) " ")
                            (nth (+ i 60) lay)))
            i (+ i 1)))
    (setq i 29
          ret (concat ret (or (and (string= "" (nth i lay)) " ")
                              (nth i lay))
                      (or (and (string= "" (nth (+ i 60) lay)) " ")
                          (nth (+ i 60) lay))
                      (make-string 6 ? ))
          i 47)
    (while (< i 57)
      (setq ret (concat ret (or (and (string= "" (nth i lay)) " ")
                                (nth i lay))
                        (or (and (string= "" (nth (+ i 60) lay)) " ")
                            (nth (+ i 60) lay)))
            i (+ i 1)))
    (setq ret (concat ret (make-string 38 ? )))
    ret))

(defun ergoemacs-translate--quail-to-ergoemacs (quail)
  "Translate QUAIL layout to ergoemacs layout"
  (let ((vec (vconcat quail))
        (i 0)
        retu
        retl)
    (while (< i 3)
      (push "" retu)
      (push "" retl)
      (setq i (+ i 1)))
    (setq i (+ (* 4 30) 21))
    (while (>= i (+ (* 4 30) 2))
      (push (make-string 1 (aref vec i)) retu)
      (setq i (- i 1))
      (push (make-string 1 (aref vec i)) retl)
      (setq i (- i 1)))
    (setq i 0)
    (while (< i 4)
      (push "" retu)
      (push "" retl)
      (setq i (+ i 1)))
    (setq i (+ (* 3 30) 23))
    (while (>= i (+ (* 3 30) 2))
      (push (make-string 1 (aref vec i)) retu)
      (setq i (- i 1))
      (push (make-string 1 (aref vec i)) retl)
      (setq i (- i 1)))
    (setq i 0)
    (while (< i 2)
      (push "" retu)
      (push "" retl)
      (setq i (+ i 1)))
    ;; 24 and 25 are the | \ chars.
    (setq i (+ (* 3 30) 25))
    (push (make-string 1 (aref vec i)) retu)
    (setq i (- i 1))
    (push (make-string 1 (aref vec i)) retl)
    (setq i (- i 1))
    (setq i (+ (* 2 30) 25))
    (while (>= i (+ (* 2 30) 2))
      (push (make-string 1 (aref vec i)) retu)
      (setq i (- i 1))
      (push (make-string 1 (aref vec i)) retl)
      (setq i (- i 1)))
    (setq i 0)
    (while (< i 3)
      (push "" retu)
      (push "" retl)
      (setq i (+ i 1)))
    (setq i (+ (* 1 30) 25))
    (while (>= i (+ (* 1 30) 2))
      (push (make-string 1 (aref vec i)) retu)
      (setq i (- i 1))
      (push (make-string 1 (aref vec i)) retl)
      (setq i (- i 1)))
    ;; ~ `
    (setq i (+ (* 1 30) 27))
    (push (make-string 1 (aref vec i)) retu)
    (setq i (- i 1))
    (push (make-string 1 (aref vec i)) retl)
    (setq i (- i 1))
    (push "" retu)
    (push "" retl)
    (append retl retu)))

(defun ergoemacs-translate--ahk-code (event)
  "Get event code for AHK ini file."
  (let* ((event (or (and (vectorp event) (elt event 0)) event))
         (mod (event-modifiers event))
         (basic (event-basic-type event))
         (code 0))
    (when (memq 'shift mod)
      (setq code (+ 8 code)))
    (when (memq 'meta mod)
      (setq code (+ 2 code)))
    (when (memq 'control mod)
      (setq code (+ 4 code)))
    (format "%s%s" code basic)))

(defun ergoemacs-translate--ahk-layout (&optional layout)
  "Translate LAYOUT to that used in the windows autohotkey script"
  (let* ((layout (or layout ergoemacs-keyboard-layout))
         (sym (ergoemacs :layout layout))
         (val (symbol-value sym))
         (i 1))
    (with-temp-buffer
      (insert "[" layout "]\n")
      (dolist (v val)
        (insert (format "%s=%s\n" i (string-to-char v)))
        (setq i (+ i 1)))
      (buffer-string))))

(defun ergoemacs-translate--ahk-functions ()
  "Get a list of functions supported by the autohotkey script."
  (let (ret tmp)
    (with-temp-buffer
      (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\):" nil t)
        (setq tmp (intern (match-string 1)))
        (when (fboundp tmp)
          (push tmp ret))))
    ret))

(defun ergoemacs-translate--ahk-functions-ini ()
  "Gets the autohotkey ini section for the current layout and theme."
  (let ((ret ""))
    (dolist (f (ergoemacs-translate--ahk-functions))
      (dolist (key (where-is-internal f))
        (when (and (= (length key) 1) (integerp (event-basic-type (elt key 0))))
          (setq ret (format "%s\n%s=%s" ret f (ergoemacs-translate--ahk-code key))))))
    ret))

(defun ergoemacs-translate--ahk-ini (&optional all-layouts all-themes)
  "Creates the ini file used with the autohotkey script."
  (let ((layouts (or (and all-layouts (sort (ergoemacs-layouts--list) 'string<))
                     (and (eq (ergoemacs :layout) 'ergoemacs-layout-us) (list "us"))
                     (list "us" ergoemacs-keyboard-layout)))
        (themes (or (and all-themes (sort (ergoemacs-theme--list) 'string<))
                    (list ergoemacs-theme)))
        (original-layout ergoemacs-keyboard-layout)
        (original-theme ergoemacs-theme)
        ret)
    (unwind-protect
        (setq ret (with-temp-buffer
                    (insert "[Layouts]\n"
                            (ergoemacs-layouts--custom-documentation layouts t) "\n")
                    (dolist (lay layouts)
                      (insert (ergoemacs-translate--ahk-layout lay)))
                    (insert "[Themes]\n"
                            (ergoemacs-theme--custom-documentation themes t)
                            "\n")
                    (dolist (lay layouts)
                      (dolist (theme themes)
                        (message "Getting information from %s-%s" lay theme)
                        (setq ergoemacs-keyboard-layout lay
                              ergoemacs-theme theme)
                        (ergoemacs-mode-reset)
                        (insert "[" lay "-" theme "]"
                                (ergoemacs-translate--ahk-functions-ini)
                                "\n")))
                    (buffer-string)))
      (setq ergoemacs-keyboard-layout original-layout
            ergoemacs-theme original-theme)
      (ergoemacs-mode-reset))
    ret))

(defun ergoemacs-translate-layout (&optional layout type)
  "Translates keyboard LAYOUT to between ergoemacs and different types of keyboard layouts.

If :type is :ergoemacs use the 120 length list that `ergoemacs-mode' uses.

If :type is :quail use the 180 length string that
`quail-insert-keyboard-layout' uses.
"
  (cond
   ((or (not type) (eq type :ergoemacs))
    (cond
     ((and (listp layout) (= 120 (length layout))) layout)
     ((and (stringp layout) (= (length layout) 180))
      (ergoemacs-translate--quail-to-ergoemacs layout))
     (t (let ((ret (ergoemacs :layout  (or layout ergoemacs-keyboard-layout))))
          (when ret
            (setq ret (symbol-value ret)
                  ret (ergoemacs-translate-layout ret :ergoemacs)))
          ret))))
   ((eq type :quail)
    (cond
     ((and (stringp layout) (= 180 (length layout))) layout)
     ((and (listp layout) (= 120 (length layout)))
      (ergoemacs-translate--ergoemacs-to-quail layout))
     (t (let ((ret (ergoemacs :layout  (or layout ergoemacs-keyboard-layout))))
          (when ret
            (setq ret (symbol-value ret)
                  ret (ergoemacs-translate-layout ret :quail)))
          ret))))))

(defun ergoemacs-translate--svg-quote (char)
  "Quote Character."
  (let ((case-fold-search nil)
        (i 0)
        (ret "")
        (default-font (face-attribute 'default :font))
        font)
    (if (= (length char) 1)
        (save-match-data
          (cond
           ((string= char "")
            " ")
           ((string= char "&")
            "&amp;")
           ((string= char ">")
            "&gt;")
           ((string= char ">")
            "&lt;")
           ((string= char "\"")
            "&quot;")
           ((string-match "[a-zA-Z0-9]" char)
            char)
           (t
            (setq ret (format "&#x%04X;"
                               (encode-char
                                (with-temp-buffer
                                  (insert char)
                                  (char-before))
                                'unicode))
                  font (ergoemacs-key-description--display-char-p char))
            (when font
              (setq font (or (font-get font :name)
                             (font-get font :family)))
              (when (and font (symbolp font))
                (setq font (symbol-name font)))
              (setq default-font (or (font-get default-font :name)
                                     (font-get default-font :family)))
              (when (and default-font (symbolp default-font))
                (setq default-font (symbol-name default-font)))
              (unless (string= font default-font)
                (setq ret (format "<text style=\"font-family: '%s'\">%s</text>" font ret))))
            ret)))
      (while (< i (length char))
        (setq ret (concat ret (ergoemacs-translate--svg-quote (substring char i (+ i 1))))
              i (+ i 1)))
      ret)))

(defvar ergoemacs-translate--svg-layout nil)
(defun ergoemacs-translate--svg-layout (&optional layout reread)
  "Create SVG for LAYOUT. Optionally REREAD kbd.svg before creating svg."
  (let* ((lay (or layout ergoemacs-keyboard-layout))
         (layout (symbol-value (ergoemacs :layout  lay)))
         (file-dir (expand-file-name "layouts" (expand-file-name "ergoemacs-extras" user-emacs-directory)))
         (file-name (expand-file-name (concat lay ".svg") file-dir))
         (reread reread)
         pt)
    (if (and (file-exists-p file-name) (not reread)) file-name
      (when (eq reread :svg)
        (setq reread nil))
      (unless (file-exists-p file-dir)
        (make-directory file-dir t))
      (when reread
        (setq ergoemacs-translate--svg-layout nil))
      (unless ergoemacs-translate--svg-layout
        (with-temp-buffer
          (insert-file-contents (expand-file-name "kbd.svg" ergoemacs-dir))
          (goto-char (point-min))
          (setq pt (point))
          (while (re-search-forward ">\\([0-9]+\\)<" nil t)
            (push (buffer-substring pt (match-beginning 0)) ergoemacs-translate--svg-layout)
            (push (string-to-number (match-string 1)) ergoemacs-translate--svg-layout)
            (setq pt (match-end 0)))
          (push (buffer-substring pt (point-max)) ergoemacs-translate--svg-layout)
          (setq ergoemacs-translate--svg-layout (reverse ergoemacs-translate--svg-layout))))
      (with-temp-file file-name
        (dolist (w ergoemacs-translate--svg-layout)
          (cond
           ((stringp w)
            (insert w))
           ((integerp w)
            (insert ">" (ergoemacs-translate--svg-quote (nth w layout)) "<"))))))
    file-name))

(defun ergoemacs-translate--png-layout (&optional layout reread)
  "Get png file for layout, or create one.
Requires `ergoemacs-inkscape' to be specified."
  (let* ((svg-file (ergoemacs-translate--svg-layout layout reread))
         (png-file (concat (file-name-sans-extension svg-file) ".png")))
    (if (file-exists-p png-file) png-file
      (if (and ergoemacs-inkscape (file-readable-p ergoemacs-inkscape))
          (progn
            (message "Converting to png.")
            (shell-command (format "%s -z -f \"%s\" -e \"%s\"" ergoemacs-inkscape svg-file png-file))
            png-file)
        (message "Need inkscape and to specify inkscape location with `ergoemacs-inkscape'.")
        nil))))

    (provide 'ergoemacs-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-translate.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
