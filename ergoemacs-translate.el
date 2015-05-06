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

(declare-function ergoemacs-mode-line "ergoemacs-mode")
(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")
(declare-function ergoemacs-key-description-kbd "ergoemacs-key-description")
(declare-function ergoemacs-layouts--current "ergoemacs-layouts")


(defvar ergoemacs-translate--hash (make-hash-table)
  "")

(defun ergoemacs-translate--get-hash (&optional layout-to layout-from)
  "Gets the translation hash."
  (let* ((to (ergoemacs :layout  (or layout-to ergoemacs-keyboard-layout)))
         (from (ergoemacs :layout  (or layout-from "us")))
         (hash-f (gethash from ergoemacs-translate--hash (make-hash-table)))
         (hash-f-t (gethash to hash-f))
         (i 0)
         hash-t hash-t-f lay-t lay-f r-t r-f)
    (if hash-f-t hash-f-t
      (setq hash-f-t (make-hash-table)
            hash-t (gethash to ergoemacs-translate--hash (make-hash-table))
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
        (setq modifiers (event-modifiers event))
        (if (not (memq which-mod modifiers)) (push event seq)
          (setq new-mod (list (event-basic-type event)))
          (dolist (mod modifiers)
            (unless (eq which-mod mod)
              (push mod new-mod)))
          (push (event-convert-list new-mod) seq)
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

(defvar ergoemacs-translate--event-hash (make-hash-table)
  "Event modifiers not covered by standard emacs")

(defun ergoemacs-translate--event-modifier-hash (&optional layout)
  "Gets the event modifier hash for LAYOUT."
  (let* ((layout-symbol (ergoemacs :layout  layout))
         (hash (gethash layout-symbol ergoemacs-translate--event-hash)))
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
      (when (gethash basic (ergoemacs-translate--event-modifier-hash layout))
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

(defun ergoemacs-translate--event (event &optional layout-to layout-from basic modifiers)
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
              (setq new-event (or (gethash new-event translation-hash) new-event))))
        (setq new-event (or (gethash basic translation-hash) basic)
              new-modifiers modifiers)))
    (ergoemacs-translate--event-convert-list (append new-modifiers (list new-event)) layout-to)))

;;; Actual Translations
(defvar ergoemacs-translate--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (read-kbd-macro "<deletechar>") (read-kbd-macro "DEL"))
    map)
  "Map of defined translations that are applied if the original key wasn't found.")

(defvar ergoemacs-translate--needed-p nil
  "Tells if ergoemacs keybindings need a translation")

(defvar ergoemacs-translate--from nil
  "Translation from keyboard layout")

(defvar ergoemacs-translate--to nil
  "Translation to keyboard layout")

(defvar ergoemacs-translation--assoc nil
  "Translation alist")

(defvar ergoemacs-translate--regexp nil
  "Translation regular expression")

(defvar ergoemacs-translate--unshifted-regexp nil
  "Unshifted regular expression.")

(defvar ergoemacs-translate--shifted-regexp nil
  "Shifted regular expression.")

(defvar ergoemacs-translate--shifted-assoc nil
  "Translation alist.")

(defvar ergoemacs-translate--universal-fns
  '(universal-argument ergoemacs-command-loop--universal-argument)
  "Universal argument functions")

(defvar ergoemacs-translate--key-hash (make-hash-table :test 'equal))
(defvar ergoemacs-translate--translation-hash (make-hash-table :test 'equal))
(defvar ergoemacs-translate--text-hash (make-hash-table :test 'equal))

(defun ergoemacs-translate--reset ()
  "Resets translations."
  (setq ergoemacs-translate--key-hash (make-hash-table :test 'equal))
  (setq ergoemacs-translate--translation-hash (make-hash-table :test 'equal))
  (setq ergoemacs-translate--text-hash (make-hash-table :test 'equal))
  (setq ergoemacs-translate--universal-fns '(universal-argument ergoemacs-command-loop--universal-argument)))

(defun ergoemacs-translate--local-map (type &optional modal)
  "Gets local keymap for TYPE, or returns nil.
If MODAL is true, get the modal override map."
  (let ((map (intern-soft (concat "ergoemacs-" (symbol-name type) (if modal "-modal-map" "-translation-local-map")))))
    (if (not map) nil
      (ergoemacs-sv map))))

(defun ergoemacs-translate--get (&optional type key)
  "Get precalculated translation, or calculate and get translation for TYPE.
If TYPE is not specified, assume it is the normal translation."
  (let* ((type (or type 'normal))
         (ret (gethash type ergoemacs-translate--translation-hash)))
    (when (and (consp ret) (eq (car ret) 'lambda))
      (setq ret (funcall ret)))
    (if key
        (plist-get ret key)
      ret)))

(defun ergoemacs-translate--defered (arg-plist)
  "Deferred calculation of an `ergoemacs-translation'."
  (let ((ret-plist arg-plist)
        (keymap (plist-get arg-plist ':keymap))
        tmp
        (trans-text '())
        (pretty-trans '())
        (key-text '())
        (key-pretty '()))
    (setq ret-plist (plist-put ret-plist ':keymap-modal nil))
    (setq ret-plist (plist-put ret-plist ':keymap nil))
    ;; Now put the translation text together as a list.
    (dolist (x '((:alt "M-")
                 (:ctl "C-")
                 (:shift "S-")
                 (:alt-ctl "M-C-")
                 (:alt-shift "M-S-")
                 (:ctl-shift "C-S-")
                 (:alt-ctl-shift "C-M-S-")))
      (let ((trans (plist-get arg-plist (nth 0 x)))
            (orig (nth 1 x))
            case-fold-search)
        (when trans
          (push (concat orig (ergoemacs-key-description--unicode-char "→" "->") trans)
                trans-text)
          (push (concat
                 (replace-regexp-in-string
                  "[Qq]" ""
                  (ergoemacs-key-description-kbd (concat orig "q")))
                 (ergoemacs-key-description--unicode-char "→" "->")
                 (replace-regexp-in-string
                  "[Qq]" ""
                  (ergoemacs-key-description-kbd (concat trans "q"))))
                pretty-trans))))
    (when keymap
      (dolist (x '((ergoemacs-read-key-next-key-is-alt "M-")
                   (ergoemacs-read-key-next-key-is-ctl "C-")
                   (ergoemacs-read-key-next-key-is-alt-ctl "C-M-")
                   (ergoemacs-read-key-next-key-is-quoted "")
                   (ergoemacs-read-key-force-next-key-is-alt "M-")
                   (ergoemacs-read-key-force-next-key-is-ctl "C-")
                   (ergoemacs-read-key-force-next-key-is-alt-ctl "C-M-")
                   (ergoemacs-read-key-force-next-key-is-quoted "")))
        (let ((key (where-is-internal (nth 0 x) keymap t))
              (trans (nth 1 x)))
          (when key
            (setq key (key-description key))
            (push (concat key (ergoemacs-key-description--unicode-char "→" "->") trans)
                  key-text)
            (push (concat
                   (ergoemacs-key-description-kbd key)
                   (ergoemacs-key-description--unicode-char "→" "->")
                   (replace-regexp-in-string
                    "[Qq]" ""
                    (ergoemacs-key-description-kbd (concat trans "q"))))
                  key-pretty)))))

    (setq tmp '("" ""))
    (when (plist-get arg-plist ':unchorded)
      (setq tmp (list (plist-get arg-plist ':unchorded)
                      (replace-regexp-in-string
                       "[Qq]" ""
                       (ergoemacs-key-description-kbd
                        (concat (plist-get arg-plist ':unchorded) "q"))))))
    (puthash (plist-get arg-plist ':name)
             (list :trans-text trans-text
                   :pretty-trans pretty-trans
                   :key-text key-text
                   :key-pretty key-pretty
                   :lst tmp
                   :text (or (plist-get arg-plist ':text) ""))
             ergoemacs-translate--text-hash)
    (puthash (plist-get arg-plist ':name) ret-plist
             ergoemacs-translate--translation-hash)))

(defun ergoemacs-translation (&rest arg-plist)
  "Add or modifies an ergoemacs-translation.

The argument ARG-PLIST should be a plist with the following properties:

:name -- name of translation, should be a symbol
:text -- Text to display while completing this translation
:keymap -- Local Keymap for translation
:keymap-modal -- Modal keymap for overrides.
:modal-always -- If the modal state is always on, regardless of
                 the values of  `ergoemacs-modal-ignored-buffers',
                `ergoemacs-modal-emacs-state-modes' `minibufferp'
The following arguments allow the keyboard presses to be translated:
 - :alt
 - :ctl
 - :shift
 - :alt-ctl
 - :alt-shift
 - :ctl-shift
 - :alt-ctl-shift
 - :unchorded (no modifiers)

This will be called by `ergoemacs-translate--key'.

The translations plists are stored in `ergoemacs-translate--translation-hash'.
The keymap translation text is stored in `ergoemacs-translate--text-hash'

This also creates functions:
- ergoemacs-NAME-universal-argument
- ergoemacs-NAME-digit-argument
- ergoemacs-NAME-negative-argument
- ergoemacs-NAME-modal
"
  ;; Take off all the `ergoemacs-translate--key' hashes
  ;; (setq ergoemacs-translate--key-hash (make-hash-table :test 'equal))
  (let ((keymap (plist-get arg-plist ':keymap))
        (keymap-modal (plist-get arg-plist ':keymap-modal)))

    (eval (macroexpand
           `(defvar ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-modal-map"))
              ',keymap-modal
              ,(concat "Ergoemacs modal override map for "
                      (symbol-name (plist-get arg-plist ':name))
                      " translation.
This keymap is made in `ergoemacs-translation'"))))

    (eval (macroexpand
           `(defvar ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-translation-local-map"))
              ',keymap
              ,(concat "Ergoemacs translation local map for "
                      (symbol-name (plist-get arg-plist ':name))
                      " translation setup.
This keymap is made in `ergoemacs-translation'"))))
    
    ;; Create the universal argument functions.
    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-universal-argument")) ()
              ,(concat "Ergoemacs universal argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-command-loop--universal-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-command-loop--universal-argument ',(plist-get arg-plist ':name)))))
    (push (intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-universal-argument")) ergoemacs-translate--universal-fns)

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-digit-argument")) ()
              ,(concat "Ergoemacs digit argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-command-loop--digit-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-command-loop--digit-argument ',(plist-get arg-plist ':name)))))

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-negative-argument")) ()
              ,(concat "Ergoemacs digit argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-command-loop--negative-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-command-loop--negative-argument ',(plist-get arg-plist ':name)))))

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-modal")) ()
              ,(concat "Toggle modal "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation.
This function is made in `ergoemacs-translation' and calls `ergoemacs-modal-toggle'.")
              (interactive)
              (ergoemacs-modal-toggle ',(plist-get arg-plist ':name)))))
    
    ;; Now get keys that change the next key's behavior
    (puthash (plist-get arg-plist ':name) `(lambda() (ergoemacs-translate--defered ',arg-plist))
             ergoemacs-translate--translation-hash)))

(defun ergoemacs-translate--update-text ()
  "Updates the translation text help variable."
  (maphash
   (lambda(key val)
     (let ((keys0 (nth 0 val))
           (new-pretty1 '()) ; 1
           (keys2 (nth 2 val)) ;2
           (new-pretty3 '()) ;3
           (elt4 (nth 4 val))
           tmp1 tmp2 tmp3)
       (dolist (cur-key (reverse keys0))
         (if (not (string-match "\\`\\(.*\\)\\(→\\|->\\)\\(.*\\)\\'" cur-key))
             (push cur-key new-pretty1)
           (setq tmp1 (match-string 1 cur-key)
                 tmp2 (match-string 2 cur-key)
                 tmp3 (match-string 3 cur-key))
           (setq tmp1 (substring
                       (ergoemacs-key-description-kbd
                        (concat
                         (replace-regexp-in-string
                          "\\` +\\(.*?\\) +\\'" "\\1" tmp1) "q"))
                       0 -1)
                 tmp3 (substring
                       (ergoemacs-key-description-kbd
                        (concat
                         (replace-regexp-in-string
                          "\\` +\\(.*?\\) +\\'" "\\1" tmp3) "q"))
                       0 -1))
           (push (concat tmp1 tmp2 tmp3) new-pretty1)))
       (dolist (cur-key (reverse keys2))
         (if (not (string-match "\\`\\(.*\\)\\(→\\|->\\)\\(.*\\)\\'" cur-key))
             (push cur-key new-pretty3)
           (setq tmp1 (match-string 1 cur-key)
                 tmp2 (match-string 2 cur-key)
                 tmp3 (match-string 3 cur-key))
           (setq tmp1 (ergoemacs-key-description-kbd
                       (replace-regexp-in-string
                        "\\` +\\(.*?\\) +\\'" "\\1" tmp1))
                 tmp3 (substring
                       (ergoemacs-key-description-kbd
                        (concat
                         (replace-regexp-in-string
                          "\\` +\\(.*?\\) +\\'" "\\1" tmp3) "q"))
                       0 -1))
           (push (concat tmp1 tmp2 tmp3) new-pretty3)))
       (unless (string= "" (nth 0 elt4))
         (setq elt4
               (list (nth 0 elt4)
                     (substring
                      (ergoemacs-key-description-kbd (concat (nth 0 elt4) "q"))
                      0 -1))))
       (puthash
        key (list keys0 new-pretty1 keys2 new-pretty3 elt4 (nth 5 val))
        ergoemacs-translate--text-hash)))
   ergoemacs-translate--text-hash))

;; Reset translations in case this is re-sourced
(ergoemacs-translate--reset)

;; Add ergoemacs-mode default translations
(ergoemacs-translation
 :name 'normal
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
           map))

(ergoemacs-translation
 :name 'ctl-to-alt
 :text (lambda() (format "<Ctl%sAlt> " (ergoemacs-key-description--unicode-char "↔" " to ")))
 :alt "C-"
 :ctl "M-"
 :modal-color "blue"
 :modal-always t
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "M-h") 'ergoemacs-read-key-help)
           (define-key map (if (eq system-type 'windows-nt) [M-apps] [M-menu]) 'ergoemacs-read-key-force-next-key-is-quoted)
           (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-force-next-key-is-ctl)
           (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-force-next-key-is-alt)
           
           ;; (define-key map "G" 'ergoemacs-read-key-next-key-is-quoted)
           ;; (define-key map "g" 'ergoemacs-read-key-next-key-is-alt)
           map))

(ergoemacs-translation
 :name 'unchorded
 :text "<Ctl+>"
 :unchorded "C-"
 :alt ""
 :ctl "M-"
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-force-next-key-is-quoted)
           (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-force-next-key-is-alt-ctl)
           (define-key map "g" 'ergoemacs-read-key-force-next-key-is-alt)
           (define-key map "G" 'ergoemacs-read-key-force-next-key-is-alt-ctl)
           map))

(ergoemacs-translation
 :name 'unchorded-alt
 :text "<Alt+>"
 :unchorded "M-"
 :shift "M-S-"
 :alt "M-S-"
 :modal-color "red"
 :keymap-modal (let ((map (make-sparse-keymap)))
                 (define-key map (read-kbd-macro "<return>") 'ergoemacs-unchorded-alt-modal)
                 (define-key map (read-kbd-macro "RET") 'ergoemacs-unchorded-alt-modal)
                 map))

(defun ergoemacs-translate--shifted (kbd)
  "Translates anything with S- and no C- in it to an upper-case character.
Translates C-A into C-S-a."
  (if (not kbd) nil
    (let ((ret kbd)
          case-fold-search)
      (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\)" ret)
        (if (string-match "C-" ret)
            (progn
              (when (and (string-match "\\(.\\)$" ret)
                         (string= (upcase (match-string 1 ret))
                                  (match-string 1 ret))
                         (not (string= (downcase (match-string 1 ret))
                                       (match-string 1 ret))))
                (setq ret
                      (replace-match
                       (concat "S-" (downcase (match-string 1 ret))) t t ret)))
              (when (and
                     (string-match-p "\\<S-" ret)
                     (string-match "\\(.\\)$" ret)
                     (string= (downcase (match-string 1 ret)) (upcase (match-string 1 ret)))
                     (or (not (string= (match-string 1 ret) ">"))
                         (not (string-match-p "<.+?>" ret)))
                     (assoc (match-string 1 ret) ergoemacs-translate--shifted-assoc))
                (setq ret (replace-match
                           (cdr (assoc (match-string 1 ret) ergoemacs-translate--shifted-assoc)) t t ret))
                (when (string-match "\\<S-" ret)
                  (setq ret (replace-match "" nil nil ret)))))
          (when (string-match "^\\(.*\\)S-\\(.*\\)\\(.\\)$" ret)
            (cond
             ((and (or (not (string= (match-string 3 ret) ">"))
                       (not (string-match-p "<.+?>" ret)))
                   (assoc (match-string 3 ret) ergoemacs-translate--shifted-assoc)
                   (string= (upcase (match-string 3 ret)) (downcase (match-string 3 ret))))
              (setq ret (concat (match-string 1 ret)
                                (match-string 2 ret)
                                (cdr (assoc (match-string 3 ret) ergoemacs-translate--shifted-assoc)))))
             (t
              (setq ret (concat (match-string 1 ret)
                                (match-string 2 ret)
                                (upcase (match-string 3 ret)))))))))
      ret)))

(defun ergoemacs-translate--install-shift (trans-plist ret-plist)
  "Install shift translation"
  (let (shift-translated
        (name (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated")))
        (k (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated-key")))
        (p (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated-pretty")))
        (key (plist-get ret-plist (intern (concat ":" (symbol-name (plist-get trans-plist ':name))))))
        (ret ret-plist))
    (unless (stringp key)
      (setq key (key-description key)))
    (setq shift-translated key)
    (cond
     ((string-match "S-" key)
      (setq shift-translated (replace-regexp-in-string "S-" "" key t)))
     ((string-match "-\\(.\\)$" key)
      (setq shift-translated
            (replace-match
             (concat "-"
                     (downcase (match-string 1 key))) t t key)))
     ((string-match (format "\\(-\\|^\\)\\(%s\\)$" ergoemacs-translate--shifted-regexp) key)
      (setq shift-translated (replace-match (format "\\1%s" (cdr (assoc (match-string 2 key) ergoemacs-translate--shifted-assoc))) t nil key))))
    (unless (string= shift-translated key)
      (setq ret (plist-put ret name shift-translated))
      (setq ret (plist-put ret k (read-kbd-macro shift-translated t)))
      (setq ret (plist-put ret p (ergoemacs-key-description-kbd shift-translated))))
    ret))

(defun ergoemacs-translate--install (trans-plist orig-key ret-plist)
  "Installs the translation.
TRANS-PLIST is the plist defining the translation.
ORIG-KEY is the original kbd-code
RET-PLIST is  the plist that the translation will be installed into.

Should install
 - :translation-name kbd-code
 - :translation-name-key [key-vector]
 - :translation-name-pretty ergoemacs-key-description-kbd

If the command can be shift translated, then the following
properties are also added:

 - :translation-name-shift-translated kbd
 - :translation-name-shift-translated-key [key-vector]
 - :translation-name-shift-translated-pretty ergoemacs-key-description-kbd
"
  (let ((name (intern (concat ":" (symbol-name (plist-get trans-plist ':name)))))
         (key (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-key")))
         (pretty (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-pretty")))
         (ret ret-plist)
         case-fold-search
         new-key)
    (cond
     ;; Trifecta
     ((string-match "C-M-S-" orig-key)
      (if (plist-get trans-plist ':alt-ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Double keys
     ((string-match "C-S-" orig-key)
      (if (plist-get trans-plist ':ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "C-M-" orig-key)
      (if (plist-get trans-plist ':alt-ctl)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-S-" orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Emacs saves these as M-A instead of M-S-a 
     ((string-match (format "M%s" ergoemacs-translate--shifted-regexp) orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key
                  (concat (plist-get trans-plist ':alt-shift)
                          (cdr (assoc (match-string 2 orig-key) ergoemacs-translate--shifted-assoc))))
        (setq new-key orig-key)))
     ;; Single Key combinations
     ((string-match "C-" orig-key)
      (if (plist-get trans-plist ':ctl)
          (setq new-key
                (concat (plist-get trans-plist ':ctl)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-" orig-key)
      (if (plist-get trans-plist ':alt)
          (setq new-key
                (concat (plist-get trans-plist ':alt)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match ergoemacs-translate--shifted-regexp orig-key)
      (if (plist-get trans-plist ':shift)
          (setq new-key
                (concat (plist-get trans-plist ':shift)
                        (cdr (assoc (match-string 2 orig-key) ergoemacs-translate--shifted-assoc))))
        (setq new-key orig-key)))
     ;; Unchorded
     ((plist-get trans-plist ':unchorded)
      (setq new-key
            (concat (plist-get trans-plist ':unchorded) orig-key)))
     (t
      (setq new-key orig-key)))
    (setq new-key (ergoemacs-translate--shifted new-key))
    (setq ret (plist-put ret name new-key))
    (setq ret (plist-put ret key (read-kbd-macro new-key t)))
    (setq ret (plist-put ret pretty (ergoemacs-key-description-kbd new-key)))
    (setq ret (ergoemacs-translate--install-shift trans-plist ret))
    ret))

(defvar ergoemacs-display-ergoemacs-key-descriptions)
(defun ergoemacs-translate--key (key)
  "Translates KEY and returns a plist of the translations.

:shift-translated
    S-a    -> a
    M-S-a  -> M-a
    C-S-a  -> C-a
    Anything without shift is nil.

This also works with characters that are not A-Z.  In the case of
non letter characters, :caps-translated is also defined, that
defines the \"capitalized\" version of the key.  For example /
would capitalize to ? for QWERTY keyboards.

All other translations are defined in `ergoemacs-translate--translation-hash'.

There are also :XXX-key and :XXX-pretty for actual key-strokes
and `ergoemacs-key-description-kbd' descriptions.

"
  (let* ((ret (gethash key ergoemacs-translate--key-hash))
         (orig-key key)
         case-fold-search
         only-key
         shift-translated
         (ergoemacs-display-ergoemacs-key-descriptions t)
         shifted-key
         unshifted-key tmp)
    (if ret ret
      (unless (stringp key)
        (setq key (key-description key)
              orig-key key))
      (cond
       ((string-match "\\(^<.+>$\\|SPC\\|DEL\\|ESC\\|RET\\|TAB\\)" key)
        (setq only-key (replace-regexp-in-string "[CMS]-" "" key t))
        (if (string-match "S-" key)
            (setq shifted-key (replace-match "" t nil key))
          (setq shifted-key (concat "S-" key))))
       (t
        (setq only-key (replace-regexp-in-string "^.*\\(.\\)$" "\\1" key t)
              shifted-key (assoc only-key ergoemacs-translate--shifted-assoc))
        (when shifted-key
          (setq shifted-key (cdr shifted-key)))))
      (when (and (string-match "\\([A-Z]\\)$" key)
                 (not (string-match "\\<\\(SPC\\|DEL\\|ESC\\|RET\\|TAB\\)\\>" key)))
        (setq key
              (replace-match
               (concat "S-" (downcase (match-string 1 key))) t t key)))
      (when shifted-key
        (setq unshifted-key only-key)
        (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\)" shifted-key)
          (when (string-match "[A-Z]" shifted-key)
            (setq shifted-key (concat "S-" (downcase shifted-key))))
          (when (string-match "[A-Z]" unshifted-key)
            (setq unshifted-key (concat "S-" (downcase unshifted-key))))))
      (when (string-match "S-" key)
        (setq shift-translated (replace-regexp-in-string "S-" "" key t)))
      
      (if shift-translated
          (progn
            (setq ret (plist-put ret ':shift-translated (ergoemacs-translate--shifted shift-translated)))
            (setq ret (plist-put ret ':shift-translated-key (read-kbd-macro (ergoemacs-translate--shifted shift-translated) t)))
            (setq ret (plist-put ret ':shift-translated-pretty (ergoemacs-key-description-kbd shift-translated))))
        (if (and shifted-key (not (string= shifted-key only-key)))
            (if (string-match (format "^%s$" ergoemacs-translate--shifted-regexp) only-key)
                (setq shift-translated (replace-regexp-in-string
                                        (format"%s$" (regexp-quote only-key))
                                        shifted-key key)
                      ret (plist-put ret ':shift-translated (ergoemacs-translate--shifted shift-translated))
                      ret (plist-put ret ':shift-translated-key (read-kbd-macro (ergoemacs-translate--shifted shift-translated) t))
                      ret (plist-put ret ':shift-translated-pretty (ergoemacs-key-description-kbd  shift-translated))
                      ret (plist-put ret ':caps-translated nil)
                      ret (plist-put ret ':caps-translated-key nil)
                      ret (plist-put ret ':caps-translated-pretty nil))
              (if (and (string= (upcase only-key) (downcase only-key))
                       (string-match (format "^%s$" ergoemacs-translate--unshifted-regexp) only-key))
                  (setq tmp (replace-regexp-in-string
                             (format"%s$" (regexp-quote only-key))
                             shifted-key key)
                        ret (plist-put ret ':caps-translated (ergoemacs-translate--shifted tmp))
                        ret (plist-put ret ':caps-translated-key (read-kbd-macro (ergoemacs-translate--shifted tmp) t))
                        ret (plist-put ret ':caps-translated-pretty (ergoemacs-key-description-kbd tmp)))
                (setq ret (plist-put ret ':shift-translated nil)
                      ret (plist-put ret ':shift-translated-key nil)
                      ret (plist-put ret ':shift-translated-pretty nil)
                      ret (plist-put ret ':caps-translated nil)
                      ret (plist-put ret ':caps-translated-key nil)
                      ret (plist-put ret ':caps-translated-pretty nil))))
          (setq ret (plist-put ret ':shift-translated nil)
                ret (plist-put ret ':shift-translated-key nil)
                ret (plist-put ret ':shift-translated-pretty nil)
                ret (plist-put ret ':caps-translated nil)
                ret (plist-put ret ':caps-translated-key nil)
                ret (plist-put ret ':caps-translated-pretty nil))))
      
      (when shifted-key
        (setq ret (plist-put ret ':shifted (ergoemacs-translate--shifted shifted-key)))
        (setq ret (plist-put ret ':shifted-key (read-kbd-macro (ergoemacs-translate--shifted shifted-key) t)))
        (setq ret (plist-put ret ':shifted-pretty (ergoemacs-key-description-kbd shifted-key))))
      (when unshifted-key
        (setq ret (plist-put ret ':unshifted (ergoemacs-translate--shifted unshifted-key)))
        (setq ret (plist-put ret ':unshifted-key (read-kbd-macro (ergoemacs-translate--shifted unshifted-key) t)))
        (setq ret (plist-put ret ':unshifted-pretty (ergoemacs-key-description-kbd unshifted-key))))
      (setq ret (plist-put ret ':ctl (ergoemacs-translate--shifted
                                      (concat "C-" unshifted-key))))
      (setq ret (plist-put ret ':ctl-key (read-kbd-macro (plist-get ret ':ctl) t)))
      (setq ret (plist-put ret ':ctl-pretty (ergoemacs-key-description-kbd (plist-get ret ':ctl))))

      (setq ret (plist-put ret ':raw (ergoemacs-translate--shifted
                                      (replace-regexp-in-string
                                       "\\<[CSMS]-" "" key))))
      (setq ret (plist-put ret ':raw-key  (read-kbd-macro (plist-get ret ':raw) t)))
      (setq ret (plist-put ret ':raw-pretty (ergoemacs-key-description-kbd
                                             (plist-get ret ':raw))))
      (if (assoc (plist-get ret ':raw) ergoemacs-translate--shifted-assoc)
          (progn
            (setq ret (plist-put ret ':raw-shift
                                 (ergoemacs-translate--shifted
                                  (replace-regexp-in-string
                                   "\\<[CSM]-" ""
                                   (cdr (assoc (plist-get ret ':raw) ergoemacs-translate--shifted-assoc))))))
            (setq ret (plist-put ret ':raw-shift-key
                                 (read-kbd-macro (plist-get ret ':raw-shift) t)))
            (setq ret (plist-put ret ':raw-shift-pretty
                                 (ergoemacs-key-description-kbd
                                  (plist-get ret ':raw-shift)))))
        (setq ret (plist-put ret ':raw-shift nil))
        (setq ret (plist-put ret ':raw-shift-key nil))
        (setq ret (plist-put ret ':raw-shift-pretty nil)))
      
      (setq ret (plist-put ret ':alt (ergoemacs-translate--shifted
                                      (concat "M-" unshifted-key))))
      (setq ret (plist-put ret ':alt-key (read-kbd-macro (plist-get ret ':alt) t)))
      (setq ret (plist-put ret ':alt-pretty (ergoemacs-key-description-kbd (plist-get ret ':alt))))
      
      (when unshifted-key
        (setq ret (plist-put ret ':alt-ctl (ergoemacs-translate--shifted
                                            (concat "M-C-" unshifted-key))))
        (setq ret (plist-put ret ':alt-ctl-key (read-kbd-macro (plist-get ret ':alt-ctl) t)))
        (setq ret (plist-put ret ':alt-ctl-pretty (ergoemacs-key-description-kbd (plist-get ret ':alt-ctl)))))

      (when shifted-key
        (setq ret (plist-put ret ':ctl-shift (ergoemacs-translate--shifted
                                              (concat "C-" shifted-key))))
        (setq ret (plist-put ret ':ctl-shift-key (read-kbd-macro (plist-get ret ':ctl-shift) t)))
        (setq ret (plist-put ret ':ctl-shift-pretty (ergoemacs-key-description-kbd (plist-get ret ':ctl-shift))))
        (setq ret (plist-put ret ':alt-shift (ergoemacs-translate--shifted
                                              (concat "M-" shifted-key))))
        (setq ret (plist-put ret ':alt-shift-key (read-kbd-macro (plist-get ret ':alt-shift) t)))
        (setq ret (plist-put ret ':alt-shift-pretty (ergoemacs-key-description-kbd (plist-get ret ':alt-shift))))
        (setq ret (plist-put ret ':alt-ctl-shift (ergoemacs-translate--shifted
                                                  (concat "M-C-" shifted-key))))
        (setq ret (plist-put ret ':alt-ctl-shift-key (read-kbd-macro (plist-get ret ':alt-ctl-shift) t)))
        (setq ret (plist-put ret ':alt-ctl-shift-pretty (ergoemacs-key-description-kbd (plist-get ret ':alt-ctl-shift)))))
      (maphash
       (lambda(_ignore plist)
         (setq ret (ergoemacs-translate--install plist orig-key ret)))
       ergoemacs-translate--translation-hash)
      (puthash orig-key ret ergoemacs-translate--key-hash)
      (puthash key ret ergoemacs-translate--key-hash)
      ret)))

(defun ergoemacs-translate--setup (layout &optional base-layout)
  "Setup translation from BASE-LAYOUT to LAYOUT."
  (let ((orig-base (or base-layout "us"))
        lay shifted-list unshifted-list base
        len i)
    (setq lay (ergoemacs-sv (intern (concat "ergoemacs-layout-" layout))))
    (setq base (ergoemacs-sv (intern (concat "ergoemacs-layout-" orig-base))))
    
    (setq len (length base))
    (setq i 0)
    (while (< i 60)
      (unless (or (string= "" (nth i lay))
                  (string= "" (nth (+ i 60) lay)))
        ;; Add to list is incompatible with lexical scoping.  However
        ;; this use is OK since `ergoemacs-translate--shifted-assoc' is defined
        ;; in a defvar statement.
        (add-to-list 'ergoemacs-translate--shifted-assoc
                     `(,(nth i lay) . ,(nth (+ i 60) lay)))
        (add-to-list 'ergoemacs-translate--shifted-assoc
                     `(,(nth (+ i 60) lay) . ,(nth i lay)))
        (pushnew (nth i lay)
                 unshifted-list
                 :test 'equal)
        (pushnew (nth (+ i 60) lay)
                 shifted-list
                 :test 'equal))
      (setq i (+ i 1)))
    (setq ergoemacs-translate--shifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt shifted-list nil)))
    (setq ergoemacs-translate--unshifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt unshifted-list nil)))
    (unless (and (string= layout ergoemacs-translate--to)
                 (string= orig-base ergoemacs-translate--from))
      (if (equal layout orig-base)
          (progn
            (setq ergoemacs-translate--from orig-base)
            (setq ergoemacs-translate--to layout)
            (setq ergoemacs-translate--needed-p nil)
            (setq ergoemacs-translation--assoc nil)
            (setq ergoemacs-translate--regexp nil))
        (setq ergoemacs-translate--from orig-base)
        (setq ergoemacs-translate--to layout)
        (setq ergoemacs-translate--needed-p t)
        (setq ergoemacs-translation--assoc nil)
        (setq len (length base))
        (setq i 0)
        (while (< i len)
          (unless (or (string= "" (nth i base))
                      (string= "" (nth i lay)))
            (add-to-list 'ergoemacs-translation--assoc
                         `(,(nth i base) . ,(nth i lay))))
          (setq i (+ i 1)))
        (setq ergoemacs-translate--regexp
              (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                      (regexp-opt (mapcar (lambda(x) (nth 0 x))
                                          ergoemacs-translation--assoc) nil)))))
    ;; Pre-cache the translations...?  Takes too long to load :(
    (when nil
      (dolist (char (append lay '("<f1>"  "<S-f1>"
                                  "<f2>"  "<S-f2>"
                                  "<f3>"  "<S-f3>"
                                  "<f4>"  "<S-f4>"
                                  "<f5>"  "<S-f5>"
                                  "<f6>"  "<S-f6>"
                                  "<f7>"  "<S-f7>"
                                  "<f8>"  "<S-f8>"
                                  "<f9>"  "<S-f9>"
                                  "<f10>" "<S-f10>"
                                  "<f11>" "<S-f11>"
                                  "<f12>" "<S-f12>"
                                  "SPC" "RET" "ESC" "DEL" "TAB"
                                  "<home>" "<S-home>"
                                  "<next>" "<S-next>"
                                  "<prior>" "<S-prior>"
                                  "<end>" "<S-end>"
                                  "<insert>" "<S-insert>"
                                  "<deletechar>" "<S-deletechar>")))
        (unless (string= "" char)
          (ergoemacs-translate--key char)
          (ergoemacs-translate--key (concat "C-" char))
          (ergoemacs-translate--key (concat "M-" char))
          (ergoemacs-translate--key (concat "M-C-" char)))))))

(defun ergoemacs-translate-setup (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-translate--setup layout base-layout)
  ;; Set appropriate mode-line indicator
  (ergoemacs-mode-line))

(defun ergoemacs-translate--event-basic-type  (event &optional layout)
  "Return the basic type of the given event (all modifiers removed).
This is different than `event-basic-type' because ?# would return
?3 on a QWERTY LAYOUT."
  (let* ((basic (event-basic-type event))
         (new-basic (gethash basic (ergoemacs-translate--event-modifier-hash layout))))
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
               (setq tmp (gethash (intern (format "s%s" elt))
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
              (ergoemacs-translate--event event layout-to layout-from basic modifiers)))
       ((and (eq system-type 'windows-nt) (eq basic 'menu))
        (setq translated-event (ergoemacs-translate--event-convert-list (append modifiers '(apps)))))
       ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
        (setq translated-event (ergoemacs-translate--event-convert-list (append modifiers '(menu)))))
       (t (setq translated-event event)))
      (setq untranslated (vconcat untranslated (list event))
            ret (vconcat ret (list translated-event))))
    ret))

(provide 'ergoemacs-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-translate.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
