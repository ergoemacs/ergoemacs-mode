;;; ergoemacs-translate.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

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

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-translation-hash)
(defvar ergoemacs-translate--hash)
(defvar ergoemacs-translate--event-hash)

(declare-function ergoemacs-mode-line "ergoemacs-mode")

(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")
(declare-function ergoemacs-key-description-kbd "ergoemacs-key-description")

(declare-function ergoemacs-layouts--current "ergoemacs-layouts")

(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-command-loop--modal-p "ergoemacs-command-loop")

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
"
  (let ((modifiers (event-modifiers event))
        basic)
    (unless (memq 'shift modifiers)
      ;; Add 'shift for # type events.
      (setq basic (event-basic-type event))
      (when (ergoemacs-gethash basic (ergoemacs-translate--event-modifier-hash layout))
        (push 'ergoemacs-shift modifiers)))
    ;; Also add 'ergoemacs-control to C-RET which translates to C-m
    (when (and (integerp event)
               (> event 1000)
               (memq 'control modifiers))
      (unless basic
        (setq basic (event-basic-type event)))
      (when (memq basic '(?m ?i ?\[))
        (push 'ergoemacs-control modifiers)))
    modifiers))

(defun ergoemacs-translate--event-layout (event &optional layout-to layout-from basic modifiers)
  "Translate EVENT to the appropriate keyboard layout.
BASIC is the precalculated basic event from `ergoemacs-translate--event-basic-type'
MODIFIERS is the precalculated modifiers from `ergoemacs-translate--event-modifiers'
LAYOUT-TO is the layout to translate to, (default `ergoemacs-keyboard-layout')
LAYOUT-FROM is the layout to translate from, (defualt is \"us\" or QWERTY)"
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
    (ergoemacs-translate--event-convert-list (append new-modifiers (list new-event)) layout-to)))

(defun ergoemacs-translate--event-basic-type (event &optional layout)
  "Return the basic type of the given event (all modifiers removed).
This is different than `event-basic-type' because ?# would return
?3 on a QWERTY LAYOUT."
  (let* ((basic (event-basic-type event))
         (new-basic (ergoemacs-gethash basic (ergoemacs-translate--event-modifier-hash layout))))
    (or new-basic basic)))

(defun ergoemacs-translate--event-convert-list (list &optional layout)
  "Convert the event description list EVENT-DESC to an event type.
This is different than `event-convert-list' because:
 -  '(shift ?3) or '(ergoemacs-shift ?3) produces ?# on a QWERTY LAYOUT.
 -  '(ergoemacs-control control ?m) produces C-RET"
  (let ((cur-list list)
        elt
        tmp
        control-p
        new-list)
    (when (memq 'ergoemacs-control cur-list)
      (setq control-p t)
      (dolist (elt (reverse cur-list))
        (unless (equal elt 'ergoemacs-control)
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
    (if control-p
        (aref (read-kbd-macro (concat "C-" (key-description (vector (event-convert-list new-list)))) t) 0)
      (event-convert-list new-list))))


(defun ergoemacs-translate (kbd &optional just-first-keys variable-modifiers variable-prefixes layout-to layout-from)
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

(defun ergoemacs-translate--event-trials (event &optional exclude-basic extra-modifiers )
  "Gets a list of `ergoemacs-mode' event trials.
When EXCLUDE-BASIC is non-nil, don't include the keys that are likely to produce a character when typing"
  (if (not (eventp event))
      (error "Need an event for event trials (%s)" event))
  (let* ((key event)
         (ret '())
         (basic (ergoemacs-translate--event-basic-type key))
         trial)
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
            (push key ret)
            ))
          ret)
      (dolist (new (ergoemacs-translate--event-trials event (= 1 (length key))))
        (push (vconcat base (vector new)) ret))
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

(defun ergoemacs-translate--create (&rest plist)
  "Create a translation from PLIST and return translation object."
  (let ((plist plist)
        struct
        -universal-argument
        -negative-argument
        -digit-argument
        -modal
        translation
        (local-keymap (or (plist-get plist :keymap) (make-sparse-keymap))))
    (ergoemacs :label local-keymap)
    (ergoemacs local-keymap :only-local-modifications-p t)
    (eval (macroexpand
           `(progn
              (defvar ,(intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map")) nil
                ,(concat "Ergoemacs local map for translation :"
                         (plist-get plist :name)
                         " while completing a key sequence."))
              ;; Backward compatible names.
              (define-obsolete-variable-alias ',(intern (concat "ergoemacs-" (plist-get plist :name) "-translation-local-map"))
                ',(intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map"))))))

    ;; Assign `map-list-hash'
    (ergoemacs local-keymap :map-list-hash (list (intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map"))
                                                 (intern (concat "ergoemacs-" (plist-get plist :name) "-translation-local-map"))))
    (set (intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map")) local-keymap)
    ;; (ergoemacs local-keymap :map-list-hash (list (intern (concat "ergoemacs-" (plist-get plist :name) "-translation-local-map"))
    ;;                                              ;; (intern (concat "ergoemacs-translate--" (plist-get plist :name) "-map"))
    ;;                                              ))
    ;; Create the functions 
    (dolist (type '("-universal-argument" "-negative-argument"
                    "-digit-argument" "-modal"))
      (set (intern type) (intern (concat "ergoemacs-translate--" (plist-get plist :name) type)))
      (eval (macroexpand
             `(progn
                (defun ,(intern (concat "ergoemacs-translate--" (plist-get plist :name) type)) ()
                  ,(concat "Ergoemacs"
                           (replace-regexp-in-string "-" " " type)
                           ", with :"
                           (plist-get plist :name)
                           " translation setup.
This is called through `ergoemacs-command-loop-" type "'.
This function is made in `ergoemacs-translate--create'")
                  (interactive)
                  (,(intern (concat "ergoemacs-command-loop-" type)) ,(plist-get plist ':key)))
                ;; Backward compatible names.
                (defalias ',(intern (concat "ergoemacs-" (plist-get plist :name) type))
                  ',(intern (concat "ergoemacs-translate--" (plist-get plist :name) type)))
                ,(cond
                  ((eq "-universal-argument" type)
                   `(progn
                      (push ',(intern (concat "ergoemacs-" (plist-get plist :name) type)) ergoemacs-command-loop--universal-functions)
                      (push ',(intern (concat "ergoemacs-translate--" (plist-get plist :name) type)) ergoemacs-command-loop--universal-functions))))))))
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
  "Get translation object TYPE"
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
  ;; A:(key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0))))
  ;; M-A:(key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0) :unchorded-alt)))
  ;; C-S-A:(key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0) :unchorded-ctl)))
  ;; M-A: (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-S-A" t) 0) :ctl-to-alt)))
  (let* ((type (or type :normal))
         (translation (and (symbolp type) (ergoemacs-translate--get type)))
         (basic (ergoemacs-translate--event-basic-type event))
         (modifiers (sort (mapcar
                           (lambda(e)
                             (cond
                              ((eq 'ergoemacs-shift e) 'shift)
                              ((eq 'ergoemacs-control e) 'control)
                              (t e)))
                           (ergoemacs-translate--event-modifiers event)) #'string<))
         (ret event))
    (when (catch 'found-mod
            (dolist (mod (or (and (ergoemacs-translation-struct-p translation)
                                  (ergoemacs-translation-struct-translation translation))
                             (and (consp type) (consp (nth 0 type)) type)
                             (and (consp type) (list (list nil type)))))
              (when (equal (nth 0 mod) modifiers)
                (setq modifiers (nth 1 mod))
                (throw 'found-mod t))) nil)
      (setq ret (ergoemacs-translate--event-convert-list `(,@modifiers ,basic))))
    ret))

(defvar ergoemacs-translate--parent-map (let ((map (make-sparse-keymap)))
                                          (ergoemacs map :label (- most-positive-fixnum 1))
                                          (ergoemacs map :only-local-modifications-p t)
                                          (ergoemacs map :map-list-hash '(ergoemacs-translate--parent-map))
                                          map)
  "Parent map for keymaps when completing a key sequence.")


(defvar ergoemacs-translate--modal-parent-map (let ((map (make-sparse-keymap)))
                                          (ergoemacs map :label (- most-positive-fixnum 2))
                                          (ergoemacs map :only-local-modifications-p t)
                                          (ergoemacs map :map-list-hash '(ergoemacs-translate--modal-parent-map))
                                          map)
  "Parent map for modal `ergoemacs-mode'")  

(defvar ergoemacs-translate--keymap-hash (make-hash-table)
  "Translation keymaps")

(defun ergoemacs-translate--keymap-reset ()
  "Reset `ergoemacs-translate--keymap-hash'"
  (setq ergoemacs-translate--keymap-hash (make-hash-table)))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-translate--keymap-reset)

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

(provide 'ergoemacs-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-translate.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
