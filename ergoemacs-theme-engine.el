;; ergoemacs-theme-engine.el --- Engine for ergoemacs-themes

;; Copyright © 2014  Free Software Foundation, Inc.

;; Filename: ergoemacs-theme-engine.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Mar 20 10:41:30 2014 (-0500)
;; Version: 
;; Package-Requires: ()
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

(eval-when-compile (require 'cl))
(require 'ergoemacs-advices)

(defgroup ergoemacs-themes nil
  "Default Ergoemacs Layout"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-theme-options
  '()
  "List of theme options"
  :type '(repeat
          (list
           (sexp :tag "Theme Component")
           (choice
            (const :tag "Force Off" 'off)
            (const :tag "Force On" 'on)
            (const :tag "Let theme decide" nil))))
  :group 'ergoemacs-themes)

(defcustom ergoemacs-theme-version
  '()
  "Each themes set version"
  :type '(repeat
          (string :tag "Theme Component")
          (choice
           (const :tag "Latest Version" 'nil)
           (string :tag "Version")))
  :group 'ergoemacs-theme)

(defcustom ergoemacs-function-short-names
  '((backward-char  "← char")
    (forward-char "→ char")
    (previous-line "↑ line")
    (next-line "↓ line")
    (left-word  "← word")
    (right-word "→ word")
    (backward-paragraph "↑ ¶")
    (forward-paragraph "↓ ¶")
    (backward-word "← word")
    (forward-word "→ word")
    (ergoemacs-backward-block "← ¶")
    (ergoemacs-forward-block  "→ ¶")
    (ergoemacs-beginning-of-line-or-what "← line/*")
    (ergoemacs-end-of-line-or-what "→ line/*")
    (scroll-down "↑ page")
    (scroll-down-command "↑ page")
    (scroll-up-command "↓ page")
    (scroll-up "↓ page")
    (ergoemacs-beginning-or-end-of-buffer "↑ Top*")
    (ergoemacs-end-or-beginning-of-buffer "↓ Bottom*")
    (ergoemacs-backward-open-bracket "← bracket")
    (ergoemacs-forward-close-bracket "→ bracket")
    (isearch-forward "→ isearch")
    (isearch-backward "← isearch")
    (recenter-top-bottom "recenter")
    (delete-backward-char "⌫ char")
    (delete-char "⌦ char")
    (backward-kill-word "⌫ word")
    (kill-word "⌦ word")
    (ergoemacs-cut-line-or-region "✂ region")
    (ergoemacs-copy-line-or-region "copy")
    (ergoemacs-paste "paste")
    (ergoemacs-paste-cycle "paste ↑")
    (ergoemacs-copy-all "copy all")
    (ergoemacs-cut-all "✂ all")
    (undo-tree-redo "↷ redo")
    (redo "↷ redo")
    (undo "↶ undo")
    (kill-line "⌦ line")
    (ergoemacs-kill-line-backward "⌫ line")
    (mark-paragraph "Mark Paragraph")
    (ergoemacs-shrink-whitespaces "⌧ white")
    (comment-dwim "cmt dwim")
    (ergoemacs-toggle-camel-case "tog. camel")
    (ergoemacs-toggle-letter-case "tog. case")
    (ergoemacs-call-keyword-completion "↯ compl")
    (flyspell-auto-correct-word "flyspell")
    (ergoemacs-compact-uncompact-block "fill/unfill ¶")
    (set-mark-command "Set Mark")
    (execute-extended-command "M-x")
    (shell-command "shell cmd")
    (ergoemacs-move-cursor-next-pane "next pane")
    (ergoemacs-move-cursor-previous-pane "prev pane")
    (ergoemacs-switch-to-previous-frame "prev frame")
    (ergoemacs-switch-to-next-frame "next frame")
    (query-replace "rep")
    (vr/query-replace "rep reg")
    (query-replace-regexp "rep reg")
    (delete-other-windows "x other pane")
    (delete-window "x pane")
    (split-window-vertically "split |")
    (split-window-right "split |")
    (split-window-horizontally "split —")
    (split-window-below "split —")
    (er/expand-region "←region→")
    (ergoemacs-extend-selection "←region→")
    (er/expand-region "←region→")
    (ergoemacs-extend-selection "←region→")
    (er/mark-outside-quotes "←quote→")
    (ergoemacs-select-text-in-quote "←quote→")
    (ergoemacs-select-current-block "Sel. Block")
    (ergoemacs-select-current-line "Sel. Line")
    (ace-jump-mode "Ace Jump")    (delete-window "x pane")
    (delete-other-windows "x other pane")
    (split-window-vertically "split —")
    (query-replace "rep")
    (ergoemacs-cut-all "✂ all")
    (ergoemacs-copy-all "copy all")
    (execute-extended-command "M-x")
    (execute-extended-command "M-x")
    (indent-region "indent-region")  ;; Already in CUA
    (set-mark-command "Set Mark")
    (mark-whole-buffer "Sel All"))
  "Ergoemacs short command names"
  :group 'ergoemacs-themes
  :type '(repeat :tag "Command abbreviation"
                 (list (sexp :tag "Command")
                       (string :tag "Short Name"))))
(require 'eieio)
(require 'eieio-base)
(defclass ergoemacs-fixed-map (eieio-named)
  ;; object-name is the object name.
  ((global-map-p :initarg :global-map-p
                 :initform nil
                 :type boolean)
   (read-map :initarg :read-map
             :initform (make-sparse-keymap)
             :type keymap)
   (shortcut-map :initarg :shortcut-map
                 :initform (make-sparse-keymap)
                 :type keymap)
   (no-shortcut-map :initarg :no-shortcut-map
                    :initform (make-sparse-keymap)
                    :type keymap)
   (map :initarg :map
        :initform (make-sparse-keymap)
        :type keymap)
   (unbind-map :initarg :unbind-map
               :initform (make-sparse-keymap)
               :type keymap)
   (shortcut-list :initarg :shortcut-list
                  :initform '()
                  :type list)
   (shortcut-movement :initarg :shortcut-movement
                      :initform '()
                      :type list)
   (shortcut-shifted-movement :initarg :shortcut-shifted-movement
                              :initform '()
                              :type list)
   (rm-keys :initarg :rm-keys
            :initform '()
            :type list)
   (cmd-list :initarg :cmd-list
             :initform '()
             :type list)
   (modify-map :initarg :modify-map
               :initform nil
               :type boolean)
   (hook :initarg :hook
         :type symbol)
   (full-map :initarg :full-map
             :initform nil
             :type boolean)
   (always :initarg :always
           :initform nil
           :type boolean)
   (deferred-keys :initarg :deferred-keys
     :initform '()
     :type list))
  "`ergoemacs-mode' fixed-map class")

(defmethod ergoemacs-define-map--shortcut-list ((obj ergoemacs-fixed-map) key-vect def)
  "Define KEY-VECT with DEF in slot shortcut-list for OBJ."
  (with-slots (shortcut-list) obj
    (let ((tmp (list key-vect (list def 'global))))
      (setq shortcut-list
            (mapcar
             (lambda(elt)
               (if (equal (nth 0 elt) key-vect)
                   (prog1 tmp
                     (setq tmp nil))
                 elt))
             shortcut-list))
      (when tmp
        (push tmp shortcut-list))
      (oset obj shortcut-list shortcut-list))))

(defmethod ergoemacs-define-map--deferred-list ((obj ergoemacs-fixed-map) key deferred-list)
  "Add/Replace DEFERRED-LIST for KEY in OBJ."
  (with-slots (deferred-keys) obj
    (let ((deferred-list deferred-list))
      (setq deferred-keys
          (mapcar
           (lambda(x)
             (if (equal (nth 0 x) key)
                 (prog1 (list key deferred-list)
                   (setq deferred-list nil))
               x))
           deferred-keys))
      (when deferred-list
        (push (list key deferred-list) deferred-keys))
      (oset obj deferred-keys deferred-keys))))

(defmethod ergoemacs-define-map--cmd-list ((obj ergoemacs-fixed-map) key-desc def &optional desc)
  "Add KEY-DESC for DEF to OBJ cmd-list slot.
Optionally use DESC when another description isn't found in `ergoemacs-function-short-names'."
  (with-slots (cmd-list) obj
    (let ((tmp (assoc def ergoemacs-function-short-names)))
      (if tmp
          (setq tmp (nth 1 tmp))
        (cond
         ((symbolp def)
          (setq tmp (symbol-name def)))
         ((stringp def)
          (setq tmp def))
         (t (setq tmp (or desc "")))))
      (setq tmp (list key-desc def tmp))
      (setq cmd-list
            (mapcar
             (lambda(x)
               (if (equal (nth 0 x) key-desc)
                   (prog1 tmp
                     (setq tmp nil))
                 x))
             cmd-list))
      (when tmp
        (push tmp cmd-list))
      (oset obj cmd-list cmd-list))))

(defmethod ergoemacs-define-map--read-map ((obj ergoemacs-fixed-map) key)
  "Defines KEY in the OBJ read-key slot if it is a vector over 2.
Key sequences starting with `ergoemacs-ignored-prefixes' are not added."
  (with-slots (read-map) obj
    (when (< 1 (length key))
      (let* ((new-key (substring key 0 1))
             (kd (key-description new-key)))
        (unless (member kd ergoemacs-ignored-prefixes)
          (define-key read-map new-key
            `(lambda()
               (interactive)
               (ergoemacs-read-key ,kd 'normal)))
          (oset obj read-map read-map))))))

(defgeneric ergoemacs-define-map (obj key def &optional no-unbind)
  "Method to define a key in an `ergoemacs-mode' key class.

Arguments are OBJ KEY DEF NO-UNBIND

OBJ is the object where the key is defined.

Define key sequence KEY as DEF.

NO-UNBIND is an optional component that forces keys to be removed
from final keymaps instead of being added to a ergoemacs-unbound
keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command that is globally bound
   (If this occurs, `ergoemacs-mode' and this is for the general
    `ergoemacs-mode' map, will remap to mode-specific definitions)
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a list of key/translation 
   (kbd-code translation) for example '(\"C-x\" unchorded)
 a list of commands.  The first bound command is used. This will
    be reassessed when loading other libraries.
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
")

(defmethod ergoemacs-define-map ((obj ergoemacs-fixed-map) key def &optional no-unbind)
  (with-slots (object-name
               shortcut-map
               no-shortcut-map
               map
               unbind-map
               rm-keys
               shortcut-movement
               global-map-p
               shortcut-shifted-movement) obj
    (let* ((name (or (and (symbolp object-name) (symbol-name object-name))
                     object-name))
           (key-desc (key-description key))
           (key-vect (read-kbd-macro key-desc t))
           tmp)
      (ergoemacs-theme-component--ignore-globally-defined-key key-vect)
      (ergoemacs-define-map--read-map obj key-vect)
      (cond
       ((and global-map-p (eq def nil))
        ;; Unbound keymap
        (define-key unbind-map key-vect 'ergoemacs-undefined)
        (oset obj unbind-map unbind-map))
       ((and global-map-p (commandp def t)
             (not (string-match "\\(mouse\\|wheel\\)" (key-description key)))
             (ergoemacs-shortcut-function-binding def))
        ;; This key could have some smart interpretations.
        (ergoemacs-define-map--shortcut-list obj key-vect def)
        (if (ergoemacs-is-movement-command-p def)
            (if (let (case-fold-search)
                  (string-match "\\(S-\\|[A-Z]$\\)" key-desc))
                (progn
                  (pushnew key-vect shortcut-shifted-movement :test 'equal)
                  (oset obj shortcut-shifted-movement shortcut-shifted-movement)
                  (define-key shortcut-map key 'ergoemacs-shortcut-movement-no-shift-select))
              (pushnew key-vect shortcut-movement :test 'equal)
              (oset obj shortcut-movement shortcut-movement)
              (define-key shortcut-map key 'ergoemacs-shortcut-movement))
          (define-key shortcut-map key 'ergoemacs-shortcut))
        (oset obj no-shortcut-map no-shortcut-map)
        (ergoemacs-define-map--cmd-list obj key-desc def)
        (define-key no-shortcut-map key def)
        (oset obj shortcut-map shortcut-map))
       ((or (commandp def t) (keymapp def) (stringp def))
        ;; Normal command
        (ergoemacs-define-map--cmd-list obj key-desc def)
        (define-key map key-vect def)
        (oset obj map map))
       ((and (listp def) (stringp (nth 0 def)))
        ;; `ergoemacs-read-key' shortcut
        (ergoemacs-define-map--shortcut-list obj key-vect def)
        (ergoemacs-define-map--cmd-list obj key-desc def (nth 0 def))
        (define-key shortcut-map key 'ergoemacs-shortcut)
        (oset obj shortcut-map shortcut-map))
       ((listp def)
        (catch 'found-command
          (dolist (command def)
            (if (not (commandp command t))
                (push command tmp)
              (define-key map key-vect def)
              (ergoemacs-define-map--cmd-list obj key-desc def)
              (oset obj map map)
              (throw 'found-command t))))
        (when tmp
          ;; Add to deferred key list
          (ergoemacs-define-map--deferred-list obj key-vect tmp)))
       ((symbolp def)
        ;; Unbound symbol, add to deferred key list
        (ergoemacs-define-map--deferred-list obj key-vect (list def)))
       ((eq def nil)
        (push key-vect rm-keys)
        (oset obj rm-keys rm-keys))))))


(defclass ergoemacs-variable-map (eieio-named)
  ((global-map-p :initarg :global-map-p
                 :initform nil
                 :type boolean)
   (layout :initarg :layout
           :initform "us"
           :type string)
   (translation-regexp :initarg :translation-regexp
                       :initform ""
                       :type string)
   (translation-assoc :initarg :translation-assoc
                      :initform ()
                      :type list)
   (just-first :initarg :just-first
               :initform ""
               :type string)
   (cmd-list :initarg :cmd-list
             :initform nil
             :type list)
   (keymap-list :initarg :keymap-list
                :initform nil
                :type list)
   (modify-map :initarg :modify-map
               :initform nil
               :type boolean)
   (hook :initarg :hook
         :type symbol)
   (full-map :initarg :full-map
             :initform nil
             :type boolean)
   (always :initarg :always
           :initform nil
           :type boolean))
  "`ergoemacs-mode' variable-map class")

(defmethod ergoemacs-define-map--cmd-list ((obj ergoemacs-variable-map) key-desc def no-unbind &optional desc)
  "Add KEY-DESC for DEF to OBJ cmd-list slot.
Optionally use DESC when another description isn't found in `ergoemacs-function-short-names'."
  (with-slots (cmd-list
               layout
               translation-regexp
               translation-assoc
               just-first) obj
    (let* ((final-desc (assoc def ergoemacs-function-short-names))
           (only-first (if (string= just-first "") nil
                         (ignore-errors (string-match-p just-first key-desc))))
           (us-key
            (or (and (string= layout "us") key-desc) 
                (let ((ergoemacs-translation-from layout)
                      (ergoemacs-translation-to "us")
                      (ergoemacs-needs-translation t)
                      (ergoemacs-translation-regexp translation-regexp)
                      (ergoemacs-translation-assoc translation-assoc))
                  (when (string= "" translation-regexp)
                    (setq ergoemacs-translation-from nil
                          ergoemacs-translation-to nil
                          ergoemacs-translation-regexp nil
                          ergoemacs-translation-assoc nil)
                    (ergoemacs-setup-translation "us" layout)
                    (oset obj translation-regexp ergoemacs-translation-regexp)
                    (oset obj translation-assoc ergoemacs-translation-assoc))
                  (ergoemacs-kbd key-desc t only-first)))))
      (if final-desc
          (setq final-desc (nth 1 final-desc))
        (cond
         ((symbolp def)
          (setq final-desc (symbol-name def)))
         ((stringp def)
          (setq final-desc def))
         (t (setq final-desc (or desc "")))))
      (setq final-desc (list us-key def final-desc only-first no-unbind))
      (setq cmd-list
            (mapcar
             (lambda(x)
               (if (equal (nth 0 x) key-desc)
                   (prog1 final-desc
                     (setq final-desc nil))
                 x))
             cmd-list))
      (when final-desc
        (push final-desc cmd-list))
      (oset obj cmd-list cmd-list))))

(defmethod ergoemacs-define-map ((obj ergoemacs-variable-map) key def &optional no-unbind)
  (with-slots (object-name) obj
    (let* ((name (or (and (symbolp object-name) (symbol-name object-name))
                     object-name))
           (key-desc (key-description key))
           (key-vect (read-kbd-macro key-desc t)))
      (ergoemacs-define-map--cmd-list obj key-desc def no-unbind)
      ;; Defining key resets the fixed-maps...
      (oset obj keymap-list '()))))

(defmethod ergoemacs-get-fixed-map ((obj ergoemacs-variable-map) &optional layout)
  (with-slots (keymap-list
               cmd-list
               global-map-p) obj
    (let (ret
          (lay (or layout ergoemacs-keyboard-layout))
          ergoemacs-translation-from
          ergoemacs-translation-to
          ergoemacs-needs-translation
          ergoemacs-translation-regexp
          ergoemacs-translation-assoc)
      (catch 'found-map
        (dolist (fixed-keymap keymap-list)
          (when (string= lay (oref fixed-keymap object-name))
            (setq ret fixed-keymap)
            (throw 'found-map t)))
        nil)
      (unless ret
        (setq ret (ergoemacs-fixed-map lay :global-map-p global-map-p))
        (ergoemacs-setup-translation lay "us")
        (dolist (cmd cmd-list)
          (ergoemacs-define-map ret (ergoemacs-kbd (nth 0 cmd) nil (nth 3 cmd))
                                (nth 1 cmd) (nth 4 cmd)))
        (push ret keymap-list)
        (oset obj keymap-list keymap-list))
      ret)))

(defclass ergoemacs-composite-map (eieio-named)
  ((global-map-p :initarg :global-map-p
                 :initform nil
                 :type boolean)
   (variable-reg :initarg :variable-reg
                 :initform (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>")))
                 :type string)
   (just-first :initarg :just-first
               :initform ""
               :type string)
   (layout :initarg :layout
           :initform "us"
           :type string)
   (modify-map :initarg :modify-map
               :initform nil
               :type boolean)
   (hook :initarg :hook
         :type symbol)
   (full-map :initarg :full-map
             :initform nil
             :type boolean)
   (always :initarg :always
           :initform nil
           :type boolean)
   (fixed :initarg :fixed
          :type ergoemacs-fixed-map)
   (variable :initarg :fixed
             :type ergoemacs-variable-map))
  "`ergoemacs-mode' composite-map class")

(defmethod ergoemacs-composite-map--ini ((obj ergoemacs-composite-map))
  (unless (slot-boundp obj 'fixed)
    (let ((fixed (ergoemacs-fixed-map (oref obj object-name)
                                      :global-map-p (oref obj global-map-p)
                                      :modify-map (oref obj modify-map)
                                      :full-map (oref obj full-map)
                                      :always (oref obj always))))
      (when (slot-boundp obj 'hook)
        (oset fixed hook (oref obj hook)))
      (oset obj fixed fixed)))
  (unless (slot-boundp obj 'variable)
    (let ((var (ergoemacs-variable-map
                (oref obj object-name)
                :global-map-p (oref obj global-map-p)
                :just-first (oref obj just-first)
                :layout (oref obj layout)
                :modify-map (oref obj modify-map)
                :full-map (oref obj full-map)
                :always (oref obj always))))
      (when (slot-boundp obj 'hook)
        (oset var hook (oref obj hook)))
      (oset obj variable var))))

(defmethod ergoemacs-define-map ((obj ergoemacs-composite-map) key def &optional no-unbind)
  (ergoemacs-composite-map--ini obj)
  (with-slots (object-name
               fixed
               variable
               variable-reg) obj
    (let* ((name (or (and (symbolp object-name) (symbol-name object-name))
                     object-name))
           (key-desc (key-description key))
           (key-vect (read-kbd-macro key-desc t)))
      (if (and (not (string= variable-reg ""))
               (ignore-errors (string-match-p variable-reg key-desc)))
          (ergoemacs-define-map variable key def no-unbind)
        (ergoemacs-define-map fixed key def no-unbind)))))

(defun ergoemacs-get-fixed-map--combine-maps (keymap1 keymap2 &optional parent)
  "Combines KEYMAP1 and KEYMAP2.
When parent is a keymap, make a composed keymap of KEYMAP1 and KEYMAP2 with PARENT keymap
When parent is non-nil, make a composed keymap
When parent is nil collapse the keymaps into a single keymap.
Assumes maps are orthogonal."
  (let ((map1 keymap1) (map2 keymap2))
    (cond
     ((equal map1 '(keymap))
      (if (keymapp parent)
          (make-composed-keymap map2 parent)
        map2))
     ((equal map2 '(keymap))
      (if (keymapp parent)
          (make-composed-keymap map1 parent)
        map1))
     ((keymapp parent)
      (make-composed-keymap (list map1 map2) parent))
     (parent
      (make-composed-keymap (list map1 map2)))
     (t
      (pop map1)
      (pop map2)
      (setq map1 (append map1 map2))
      (push 'keymap map1)
      map1))))

(defmethod ergoemacs-get-fixed-map ((obj ergoemacs-composite-map) &optional layout)
  (ergoemacs-composite-map--ini obj)
  (with-slots (variable object-name fixed modify-map full-map always
                        global-map-p) obj
    (let* ((lay (or layout ergoemacs-keyboard-layout))
           (var (ergoemacs-get-fixed-map variable lay))
           (fix fixed) map1 map2
           (ret (ergoemacs-fixed-map
                 object-name
                 :global-map-p global-map-p
                 :modify-map modify-map
                 :full-map full-map
                 :always always
                 :read-map (ergoemacs-get-fixed-map--combine-maps (oref var read-map) (oref fix read-map))
                 :shortcut-map (ergoemacs-get-fixed-map--combine-maps (oref var shortcut-map) (oref fix shortcut-map))
                 :no-shortcut-map (ergoemacs-get-fixed-map--combine-maps (oref var no-shortcut-map) (oref fix no-shortcut-map))
                 :map (ergoemacs-get-fixed-map--combine-maps (oref var map) (oref fix map))
                 :unbind-map (ergoemacs-get-fixed-map--combine-maps (oref var unbind-map) (oref fix unbind-map))
                 :shortcut-list (append (oref var shortcut-list) (oref fix shortcut-list))
                 :shortcut-movement (append (oref var shortcut-movement) (oref fix shortcut-movement))
                 :shortcut-shifted-movement (append (oref var shortcut-shifted-movement) (oref fix shortcut-shifted-movement))
                 :rm-keys (append (oref var rm-keys) (oref fix rm-keys))
                 :cmd-list (append (oref var cmd-list) (oref fix cmd-list))
                 :deferred-keys (append (oref var deferred-keys) (oref fix deferred-keys)))))
      (when (slot-boundp obj 'hook)
        (oset ret hook (oref obj hook)))
      ret)))




(defclass ergoemacs-theme-component-maps (eieio-named)
  ((variable-reg :initarg :variable-reg
                 :initform (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>")))
                 :type string)
   (description :initarg :description
                :initform ""
                :type string)
   (just-first :initarg :just-first
               :initform ""
               :type string)
   (layout :initarg :layout
           :initform "us"
           :type string)
   (global :initarg :global
           :type ergoemacs-composite-map)
   (maps :initarg :fixed
         :initform ()
         :type list)
   (init :initarg :init
         :initform ()
         :type list)
   (applied-init :initarg :applied-init
                 :initform ()
                 :type list)
   (version :initarg :version ;; "" is default version
            :initform ""
            :type string)
   (versions :initarg :versions
             :initform ()
             :type list)
   (deferred-init :initarg :deferred-init
     :initform ()
     :type list))
  "`ergoemacs-mode' theme-component maps")

(defmethod ergoemacs-theme-component-maps--ini ((obj ergoemacs-theme-component-maps))
  (with-slots (object-name
               variable-reg
               just-first
               layout) obj
    (unless (slot-boundp obj 'global)
      (oset obj global
            (ergoemacs-composite-map
             object-name
             :global-map-p t
             :variable-reg variable-reg
             :just-first just-first
             :layout layout)))))

(defvar ergoemacs-theme-component-maps--always nil)
(defvar ergoemacs-theme-component-maps--full-map nil)
(defvar ergoemacs-theme-component-maps--modify-map nil)
(defvar ergoemacs-theme-component-maps--global-map nil)
(defvar ergoemacs-theme-component-maps--curr-component nil)
(defvar ergoemacs-theme-component-maps--versions '())
(defvar ergoemacs-theme-component-maps--hook nil) 

(defmethod ergoemacs-theme-component-maps--keymap ((obj ergoemacs-theme-component-maps) keymap)
  (ergoemacs-theme-component-maps--ini obj)
  (with-slots (variable-reg
               just-first
               layout
               maps) obj
      (let (ret)
        (catch 'found-keymap
          (dolist (map maps)
            (when (equal keymap (oref map object-name))
              (setq ret map)
              (throw 'found-keymap t)))
          nil)
        (unless ret
          (setq ret
                (ergoemacs-composite-map
                 keymap
                 :variable-reg variable-reg
                 :just-first just-first
                 :layout layout
                 :always ergoemacs-theme-component-maps--always
                 :full-map ergoemacs-theme-component-maps--full-map
                 :modify-map ergoemacs-theme-component-maps--modify-map))
          (if ergoemacs-theme-component-maps--hook
              (oset ret hook ergoemacs-theme-component-maps--hook)
            (oset ret hook (intern (save-match-data (replace-regexp-in-string "-map.*\\'" "-hook" (symbol-name keymap))))))
          (push ret maps)
          (oset obj maps maps))
        ret)))

(defmethod ergoemacs-define-map ((obj ergoemacs-theme-component-maps) keymap key def)
  (ergoemacs-theme-component-maps--ini obj)
  (with-slots (global) obj
    (cond
     ((eq keymap 'global-map)
      (ergoemacs-define-map global key def))
     ((eq keymap 'ergoemacs-keymap)
      (ergoemacs-define-map global key def t))
     (t
      (let ((composite-map (ergoemacs-theme-component-maps--keymap obj keymap)))
        (if (not (ergoemacs-composite-map-p composite-map))
            (warn "`ergoemacs-define-map' cannot find map for %s" keymap)
          (ergoemacs-define-map composite-map key def)))))))

(defmethod ergoemacs-get-fixed-map ((obj ergoemacs-theme-component-maps) &optional keymap layout)
  (ergoemacs-theme-component-maps--ini obj)
  (with-slots (global) obj
    (cond
     ((not keymap) (ergoemacs-get-fixed-map global layout))
     (t (ergoemacs-get-fixed-map
         (ergoemacs-theme-component-maps--keymap obj keymap) layout)))))

(defmethod ergoemacs-get-hooks ((obj ergoemacs-theme-component-maps) &optional match ret)
  (ergoemacs-theme-component-maps--ini obj)
  (let ((ret (or ret '()))
        (match (or match "-hook\\'")))
    (with-slots (maps) obj
      (dolist (map-obj maps)
        (when (and (slot-boundp map-obj 'hook)
                   (string-match-p match (symbol-name (oref map-obj hook))))
          (pushnew (oref map-obj hook) ret))))
    ret))


(defclass ergoemacs-theme-component-map-list (eieio-named)
  ((map-list :initarg :map-list
             :initform ()
             :type list))
  "`ergoemacs-mode' theme-component maps")

(defmethod ergoemacs-get-hooks ((obj ergoemacs-theme-component-map-list) &optional match ret)
  (with-slots (map-list) obj
    (let ((ret (or ret '())))
      (dolist (map map-list)
        (when (ergoemacs-theme-component-maps-p map)
          (setq ret (ergoemacs-get-hooks map match ret))))
      ret)))

(defmethod ergoemacs-get-fixed-map ((obj ergoemacs-theme-component-map-list) &optional keymap layout)
  (with-slots (map-list) obj
    (let ((fixed-maps (mapcar (lambda(map) (and map (ergoemacs-get-fixed-map map keymap layout))) map-list))
          new-global-map-p
          new-read-map
          new-shortcut-map
          new-no-shortcut-map
          new-map
          new-unbind-map
          new-shortcut-list
          new-shortcut-movement
          new-shortcut-shifted-movement
          new-rm-keys
          new-cmd-list
          new-modify-map
          new-hook
          new-full-map
          new-always
          new-deferred-keys
          (first t)
          ret)
      (dolist (map-obj fixed-maps)
        (when (ergoemacs-fixed-map-p map-obj)
          (with-slots (global-map-p
                       read-map
                       shortcut-map
                       no-shortcut-map
                       map
                       unbind-map
                       shortcut-list
                       shortcut-movement
                       shortcut-shifted-movement
                       rm-keys
                       cmd-list
                       modify-map
                       full-map
                       always
                       deferred-keys) map-obj
            (unless (equal read-map '(keymap))
              (push read-map new-read-map))
            (unless (equal shortcut-map '(keymap))
              (push shortcut-map new-shortcut-map))
            (unless (equal no-shortcut-map '(keymap))
              (push no-shortcut-map new-no-shortcut-map))
            (unless (equal map '(keymap))
              (push map new-map))
            (unless (equal unbind-map '(keymap))
              (push unbind-map new-unbind-map))
            (when (slot-boundp map-obj 'hook)
              (setq new-hook (oref map-obj hook)))
            (setq new-global-map-p global-map-p
                  new-modify-map modify-map
                  new-full-map full-map
                  new-always always)
            (if first
                (setq new-shortcut-list shortcut-list
                      new-shortcut-movement shortcut-movement
                      new-shortcut-shifted-movement shortcut-shifted-movement
                      new-rm-keys rm-keys
                      new-cmd-list cmd-list
                      new-deferred-keys deferred-keys
                      first nil)
              (setq new-global-map-p global-map-p
                    new-modify-map modify-map
                    new-full-map full-map
                    new-always always
                    new-shortcut-list (append new-shortcut-list shortcut-list)
                    new-shortcut-movement (append new-shortcut-movement shortcut-movement)
                    new-shortcut-shifted-movement (append new-shortcut-shifted-movement shortcut-shifted-movement)
                    new-rm-keys (append new-rm-keys rm-keys)
                    new-cmd-list (append new-cmd-list cmd-list)
                    new-deferred-keys (append new-deferred-keys deferred-keys))))))
      (setq ret
            (ergoemacs-fixed-map
             "composite"
             :global-map-p new-global-map-p
             :read-map (or (and new-read-map (make-composed-keymap (reverse new-read-map))) (make-sparse-keymap))
             :shortcut-map (or (and new-shortcut-map (make-composed-keymap (reverse new-shortcut-map))) (make-sparse-keymap))
             :no-shortcut-map (or (and new-no-shortcut-map (make-composed-keymap (reverse new-no-shortcut-map))) (make-sparse-keymap))
             :map (or (and new-map (make-composed-keymap (reverse new-map))) (make-sparse-keymap))
             :unbind-map (or (and new-unbind-map (make-composed-keymap (reverse new-unbind-map))) (make-sparse-keymap))
             :shortcut-list new-shortcut-list
             :shortcut-movement new-shortcut-movement
             :shortcut-shifted-movement new-shortcut-shifted-movement
             :rm-keys new-rm-keys
             :cmd-list new-cmd-list
             :modify-map new-modify-map
             :full-map new-full-map
             :always new-always
             :deferred-keys new-deferred-keys))
      (when new-hook
        (oset ret hook new-hook))
      ret)))


(defun ergoemacs-define-key (keymap key def)
  "Defines KEY to be DEF in KEYMAP for object `ergoemacs-theme-component-maps--curr-component'."
  (if (not (ergoemacs-theme-component-maps-p ergoemacs-theme-component-maps--curr-component))
      (warn "`ergoemacs-define-key' is meant to be called in a theme definition.")
    (ergoemacs-define-map
     ergoemacs-theme-component-maps--curr-component
     (or (and (memq keymap '(global-map ergoemacs-keymap)) ergoemacs-theme-component-maps--global-map) keymap)
     key def)))

(defun ergoemacs-set (symbol newval)
  (if (not (ergoemacs-theme-component-maps-p ergoemacs-theme-component-maps--curr-component))
      (warn "`ergoemacs-set' is meant to be called in a theme definition.")
    ;; ergoemacs-set definition.
    (with-slots (init) ergoemacs-theme-component-maps--curr-component
      (push (list symbol newval) init)
      (oset ergoemacs-theme-component-maps--curr-component
            init init))))

(defun ergoemacs-theme-component--version (version)
  "Changes the theme component version to VERSION."
  (if (not (ergoemacs-theme-component-maps-p ergoemacs-theme-component-maps--curr-component))
      (warn "`ergoemacs-theme-component--version' is meant to be called in a theme definition.")
    ;; ergoemacs-set definition.
    (push ergoemacs-theme-component-maps--curr-component
          ergoemacs-theme-component-maps--versions)
    (setq ergoemacs-theme-component-maps--curr-component
          (clone ergoemacs-theme-component-maps--curr-component))
    (oset ergoemacs-theme-component-maps--curr-component
          version version)))

(defun ergoemacs-theme-component--with-hook (hook plist body)
  ;; Adapted from Stefan Monnier
  (let* ((ergoemacs-theme-component-maps--hook
          (or (and (string-match-p "-hook\\'" (symbol-name hook)) hook)
              (and (string-match-p "mode.*" (symbol-name hook))
                   (save-match-data
                     (intern-soft
                      (replace-regexp-in-string
                       "-mode.*" "mode-hook"
                       (symbol-name hook)))))
              (and (string-match-p "(key)?map" (symbol-name hook))
                   (save-match-data
                     (intern-soft
                      (replace-regexp-in-string
                       "(key)?map.*" "hook"
                       (symbol-name hook)))))))
         ;; Globally set keys should be an emulation map for the mode.
         (ergoemacs-theme-component-maps--global-map
          (and (string-match-p "mode.*" (symbol-name hook))
               (save-match-data
                 (intern-soft
                  (replace-regexp-in-string
                   "mode.*" "mode" (symbol-name hook))))))
         (ergoemacs-theme-component-maps--modify-map ;; boolean
          (or (plist-get plist ':modify-keymap)
              (plist-get plist ':modify-map)))
         (ergoemacs-theme-component-maps--full-map
          (or (plist-get plist ':full-shortcut-keymap)
              (plist-get plist ':full-shortcut-map)
              (plist-get plist ':full-map)
              (plist-get plist ':full-keymap)))
         (ergoemacs-theme-component-maps--always
          (plist-get plist ':always)))
    (funcall body)))

(defun ergoemacs-theme-component--parse-remaining (remaining)
  "In parsing, this function converts
- `define-key' is converted to `ergoemacs-define-key' and keymaps are quoted
- `global-set-key' is converted to `ergoemacs-define-key' with keymap equal to `global-map'
- `global-unset-key' is converted to `ergoemacs-define-key' with keymap equal to `global-map' and function definition is `nil'
- `global-reset-key' is converted `ergoemacs-define-key'
- `setq' and `set' is converted to `ergoemacs-set'
- Mode initialization like (delete-selection-mode 1)
  or (delete-selection) is converted to
  `ergoemacs-set'
- Allows :version statement expansion
- Adds with-hook syntax or (when -hook) or (when -mode)
"
  (let* ((last-was-version nil)
         (remaining
          (mapcar
           (lambda(elt)
             (cond
              (last-was-version
               (setq last-was-version nil)
               (if (stringp elt)
                   `(ergoemacs-theme-component--version ,elt)
                 `(ergoemacs-theme-component--version ,(symbol-name elt))))
              ((ignore-errors (eq elt ':version))
               (setq last-was-version t)
               nil)
              ((ignore-errors (eq (nth 0 elt) 'global-reset-key))
               `(ergoemacs-define-key 'global-map ,(nth 1 elt) nil))
              ((ignore-errors (eq (nth 0 elt) 'global-unset-key))
               `(ergoemacs-define-key 'global-map ,(nth 1 elt) nil))
              ((ignore-errors (eq (nth 0 elt) 'set))
               ;; Currently doesn't support (setq a b c d ), but it should.
               `(ergoemacs-set ,(nth 1 elt) ,(nth 2 elt)))
              ((ignore-errors (eq (nth 0 elt) 'setq))
               (let ((tmp-elt elt)
                     (ret '()))
                 (pop tmp-elt)
                 (while (and (= 0 (mod (length tmp-elt) 2)) (< 0 (length tmp-elt)))
                   (push `(ergoemacs-set (quote ,(pop tmp-elt)) ,(pop tmp-elt)) ret))
                 (push 'progn ret)
                 ret))
              ((ignore-errors (string-match "-mode$" (symbol-name (nth 0 elt))))
               `(ergoemacs-set (quote ,(nth 0 elt)) ,(nth 1 elt)))
              ((ignore-errors (eq (nth 0 elt) 'global-set-key))
               (if (ignore-errors (keymapp (symbol-value (nth 2 elt))))
                   `(ergoemacs-define-key 'global-map ,(nth 1 elt) (quote ,(nth 2 elt)))
                 `(ergoemacs-define-key 'global-map ,(nth 1 elt) ,(nth 2 elt))))
              ((ignore-errors (eq (nth 0 elt) 'define-key))
               (if (equal (nth 1 elt) '(current-global-map))
                   (if (ignore-errors (keymapp (symbol-value (nth 3 elt))))
                       `(ergoemacs-define-key 'global-map ,(nth 2 elt) (quote ,(nth 3 elt)))
                     `(ergoemacs-define-key 'global-map ,(nth 2 elt) ,(nth 3 elt)))
                 (if (ignore-errors (keymapp (symbol-value (nth 3 elt))))
                     `(ergoemacs-define-key (quote ,(nth 1 elt)) ,(nth 2 elt) (quote ,(nth 3 elt)))
                   `(ergoemacs-define-key (quote ,(nth 1 elt)) ,(nth 2 elt) ,(nth 3 elt)))))
              ((or (ignore-errors (eq (nth 0 elt) 'with-hook))
                   (and (ignore-errors (eq (nth 0 elt) 'when))
                        (ignore-errors (string-match "-\\(hook\\|mode\\)$" (symbol-name (nth 1 elt))))))
               (let ((tmp (ergoemacs-theme-component--parse (cdr (cdr elt)) t)))
                 `(ergoemacs-theme-component--with-hook
                   ',(nth 1 elt) ',(nth 0 tmp)
                   '(lambda () ,@(nth 1 tmp)))))
              (t elt)))
           remaining)))
    remaining))

(defun ergoemacs-theme-component--parse (keys-and-body &optional skip-first)
  "Parse KEYS-AND-BODY, optionally skipping the name and
documentation with SKIP-FIRST.

Uses `ergoemacs-theme-component--parse-keys-and-body' and
  `ergoemacs-theme-component--parse-remaining'."
  (ergoemacs-theme-component--parse-keys-and-body
   keys-and-body
   'ergoemacs-theme-component--parse-remaining
   skip-first))

(defun ergoemacs-theme-component--parse-keys-and-body (keys-and-body parse-function &optional skip-first)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body.

This has been stolen directly from ert by Christian Ohler <ohler@gnu.org>

Afterward it was modified for use with `ergoemacs-mode' to use
additional parsing routines defined by PARSE-FUNCTION."
  (let ((extracted-key-accu '())
        last-was-version
        plist
        (remaining keys-and-body))
    ;; Allow
    ;; (component name)
    (unless (or (keywordp (first remaining)) skip-first)
      (if (condition-case nil
              (stringp (first remaining))
            (error nil))
          (push `(:name . ,(pop remaining)) extracted-key-accu)
        (push `(:name . ,(symbol-name (pop remaining))) extracted-key-accu))
      (when (memq (type-of (first remaining)) '(symbol cons))
        (pop remaining))
      (when (stringp (first remaining))
        (push `(:description . ,(pop remaining)) extracted-key-accu)))
    (while (and (consp remaining) (keywordp (first remaining)))
      (let ((keyword (pop remaining)))
        (unless (consp remaining)
          (error "Value expected after keyword %S in %S"
                 keyword keys-and-body))
        (when (assoc keyword extracted-key-accu)
          (warn "Keyword %S appears more than once in %S" keyword
                keys-and-body))
        (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    (when parse-function
        (setq remaining
              (funcall parse-function remaining)))
    (setq plist (loop for (key . value) in extracted-key-accu
                      collect key
                      collect value))
    (list plist remaining)))

(defun ergoemacs-theme-component--create-component (plist body)
  (let* ((ergoemacs-theme-component-maps--versions '())
         (ergoemacs-theme-component-maps--curr-component
          (ergoemacs-theme-component-maps
           (plist-get plist ':name)
           :description (plist-get plist :description)
           :layout (or (plist-get plist ':layout) "us")
           :variable-reg (or (plist-get plist ':variable-reg)
                             (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>"))))
           :just-first (or (plist-get plist ':just-first)
                           (plist-get plist ':first-is-variable-reg)
                           "")))
         ver-list tmp)
    (funcall body)
    (if (equal ergoemacs-theme-component-maps--versions '())
        (puthash (oref ergoemacs-theme-component-maps--curr-component object-name)
                 ergoemacs-theme-component-maps--curr-component
                 ergoemacs-theme-comp-hash)
      (push ergoemacs-theme-component-maps--curr-component
            ergoemacs-theme-component-maps--versions)
      (dolist (comp ergoemacs-theme-component-maps--versions)
        (setq tmp (oref comp version))
        (unless (string= tmp "")
          (push tmp ver-list)))
      (dolist (comp ergoemacs-theme-component-maps--versions)
        (oset comp versions ver-list)
        (setq tmp (oref comp version))
        (unless (string= tmp "")
          (setq tmp (concat "::" tmp)))
        (puthash (concat (oref comp object-name) tmp)
                 comp ergoemacs-theme-comp-hash)))))

(defvar ergoemacs-theme-comp-hash (make-hash-table :test 'equal)
  "Hash of ergoemacs theme components")
(defmacro ergoemacs-theme-comp (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs-theme-component--parse body-and-plist))
    `(puthash ,(plist-get (nth 0 kb) ':name)
              (lambda() (ergoemacs-theme-component--create-component
                    ',(nth 0 kb)
                    '(lambda () ,@(nth 1 kb)))) ergoemacs-theme-comp-hash)))

(defmacro ergoemacs-t (&rest body-and-plist)
  "Define an ergoemacs-theme.
:components -- list of components that this theme uses. These can't be seen or toggled
:optional-on -- list of components that are optional and are on by default
:optional-off -- list of components that are optional and off by default
:options-menu -- Menu options list
:silent -- If this theme is \"silent\", i.e. doesn't show up in the Themes menu.

The rest of the body is an `ergoemacs-theme-component' named THEME-NAME-theme
"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist"))
        (tmp (make-symbol "tmp")))
    (setq kb (ergoemacs--parse-keys-and-body body-and-plist))
    (setq tmp (eval (plist-get (nth 0 kb) ':components)))
    (push (intern (concat (plist-get (nth 0 kb) ':name) "-theme")) tmp)
    (setq tmp (plist-put (nth 0 kb) ':components tmp))
    (mapc
     (lambda(comp)
       (setq tmp (plist-put (nth 0 kb) comp
                            (eval (plist-get (nth 0 kb) comp)))))
     '(:optional-on :optional-off :options-menu))
    
    `(let (themes silent)
       (setq themes (gethash "defined-themes" ergoemacs-theme-hash)
             silent (gethash "silent-themes" ergoemacs-theme-hash))
       (push ,(plist-get (nth 0 kb) ':name) themes)
       (push ,(plist-get (nth 0 kb) ':name) silent)
       (puthash ,(plist-get (nth 0 kb) ':name) ',tmp ergoemacs-theme-hash)
       (if ,(plist-get (nth 0 kb) ':silent)
           (puthash "silent-themes" silent ergoemacs-theme-hash)
         (puthash "defined-themes" themes ergoemacs-theme-hash))
       (ergoemacs-theme-comp ,(intern (concat (plist-get (nth 0 kb) ':name) "-theme")) ()
         ,(format "Generated theme component for %s theme" (concat (plist-get (nth 0 kb) ':name) "-theme"))
         ,@(nth 1 kb)))))

(defun ergoemacs-theme-component-get-closest-version (version version-list)
  "Return the closest version to VERSION in VERSION-LIST.
Formatted for use with `ergoemacs-theme-component-hash' it will return ::version or an empty string"
  (if (or (not version) (string= "nil" version)) ""
    (if version-list
        (let ((use-version (version-to-list version))
              biggest-version
              biggest-version-list
              smallest-version
              smallest-version-list
              best-version
              best-version-list
              test-version-list
              ret)
          (mapc
           (lambda (v)
             (setq test-version-list (version-to-list v))
             (if (not biggest-version)
                 (setq biggest-version v
                       biggest-version-list test-version-list)
               (when (version-list-< biggest-version-list test-version-list)
                 (setq biggest-version v
                       biggest-version-list test-version-list)))
             (if (not smallest-version)
                 (setq smallest-version v
                       smallest-version-list test-version-list)
               (when (version-list-< test-version-list smallest-version-list)
                 (setq smallest-version v
                       smallest-version-list test-version-list)))
             (cond
              ((and (not best-version)
                    (version-list-<= test-version-list use-version))
               (setq best-version v
                     best-version-list test-version-list))
              ((and (version-list-<= best-version-list test-version-list) ;; Better than best 
                    (version-list-<= test-version-list use-version))
               (setq best-version v
                     best-version-list test-version-list))))
           version-list)
          (if (version-list-< biggest-version-list use-version)
              (setq ret "")
            (if best-version
                (setq ret (concat "::" best-version))
              (setq ret (concat "::" smallest-version))))
          ret)
      "")))

(defun ergoemacs-theme-get-component (component &optional version name)
  "Gets the VERSION of COMPONENT from `ergoemacs-theme-comp-hash'.
COMPONENT can be defined as component::version"
  (if (listp component)
      (ergoemacs-theme-component-map-list
       (or name "list") :map-list (mapcar (lambda(comp) (ergoemacs-theme-get-component comp version)) component))    
    (let* ((comp-name (or (and (symbolp component) (symbol-name component))
                          component))
           (version (or (and (symbolp version) (symbol-name version))
                        version ""))
           comp ver-list ver)
      (save-match-data
        (when (string-match "::\\([0-9.]+\\)$" comp-name)
          (setq version (match-string 1 comp-name)
                comp-name (replace-match "" nil nil comp-name))))
      (setq comp (gethash comp-name ergoemacs-theme-comp-hash))
      (when (and (not (ergoemacs-theme-component-maps-p comp))
                 (functionp comp))
        ;; Calculate component (and versions)
        (funcall comp)
        (setq comp (gethash comp-name ergoemacs-theme-comp-hash)))
      (if (not (ergoemacs-theme-component-maps-p comp))
          (message "Component %s has not been defined!" component)
        (when (not (string= "" version))
          (setq ver-list (oref comp versions))
          (setq version
                (ergoemacs-theme-component-get-closest-version
                 version ver-list))
          (setq comp (gethash (concat comp-name version)
                              ergoemacs-theme-comp-hash))))
      comp)))

(defun ergoemacs-theme-get-obj (&optional theme version)
  "Get the VERSION of THEME from `ergoemacs-theme-get-component' and `ergoemacs-theme-components'"
  (ergoemacs-theme-get-component (ergoemacs-theme-components theme) version (or theme ergoemacs-theme)))

(defun ergoemacs-keymap-empty-p (keymap &optional dont-collapse)
  "Determines if the KEYMAP is an empty keymap.
DONT-COLLAPSE doesn't collapse empty keymaps"
  (let ((keymap (or (and dont-collapse keymap)
                    (ergoemacs-keymap-collapse keymap))))
    (or (equal keymap nil)
        (equal keymap '(keymap))
        (and (keymapp keymap) (stringp (nth 1 keymap)) (= 2 (length keymap))))))

(defun ergoemacs-keymap-collapse (keymap)
  "Takes out all empty keymaps from a composed keymap"
  ;;(ergoemacs-keymap-collapse '(keymap (keymap 27 (keymap)) (keymap) (keymap "me")))
  (let ((ret '()) tmp)
    (dolist (item keymap)
      (cond
       ((eq item 'keymap) (push item ret))
       ((ignore-errors (keymapp item))
        (unless (ergoemacs-keymap-empty-p item t)
          (setq tmp (ergoemacs-keymap-collapse item))
          (when tmp
            (push tmp ret))))
       (t (push item ret))))
    (setq ret (reverse ret))
    (if (ergoemacs-keymap-empty-p ret t)
        nil
      ret)))

(defun ergoemacs-theme-i (&optional theme  version)
  "Gets the keymaps for THEME for VERSION."
  (let* ((theme-obj (ergoemacs-theme-get-obj theme version))
         (fixed-obj (ergoemacs-get-fixed-map theme-obj))
         (menu-keymap (make-sparse-keymap))
         (ergoemacs-emulation-mode-map-alist '()))
    (with-slots (read-map
                 shortcut-map
                 map
                 shortcut-list
                 unbind-map
                 rm-keys) fixed-obj
      ;; Add menu.
      (define-key menu-keymap [menu-bar ergoemacs-mode]
        `("ErgoEmacs" . ,(ergoemacs-keymap-menu theme)))
      (setq new-map (copy-keymap map))
      (pop new-map)
      (push menu-keymap new-map)
      (push 'keymap new-map)
      
      (setq ergoemacs-read-input-keymap  read-map
            ergoemacs-shortcut-keymap shortcut-map
            ergoemacs-keymap new-map
            ergoemacs-theme-shortcut-reset-list shortcut-list
            ergoemacs-unbind-keymap unbind-map
            ergoemacs-theme (or (and (stringp theme) theme)
                                (and (not (eq nil theme))(symbolp theme) (symbol-name theme))
                                (and (stringp ergoemacs-theme) ergoemacs-theme)
                                (and (not (eq nil ergoemacs-theme)) (symbolp ergoemacs-theme) (symbol-name ergoemacs-theme))))
      
      (setq ergoemacs-emulation-mode-map-alist
            `(,(cons 'ergoemacs-read-input-keys read-map)
              ,(cons 'ergoemacs-shortcut-keys shortcut-map)
              ,@(mapcar
                 (lambda(remap)
                   (cons remap (oref (ergoemacs-get-fixed-map theme-obj remap) map)))
                 (ergoemacs-get-hooks theme-obj "-mode\\'")))))))




;; (setq ergoemacs-theme-component-maps--curr-component
;;       (ergoemacs-theme-component-maps "test" :layout "colemak"))

;; (ergoemacs-define-key 'global-map (kbd "M-u") 'previous-line)
;; (ergoemacs-define-key 'global-map (kbd "C-o") 'find-file)

;; Dummy variables
(setq ergoemacs-component-version-curr nil
      ergoemacs-component-version-list nil
      ergoemacs-component-version-fixed-layout nil
      ergoemacs-component-version-fixed-layout-rm nil
      ergoemacs-component-version-redundant-keys nil
      ergoemacs-component-version-minor-mode-layout nil
      ergoemacs-component-version-variable-layout-rm nil
      ergoemacs-component-version-variable-layout nil
      ergoemacs-theme-save-variable '())


(defun ergoemacs--parse-keys-and-body (keys-and-body &optional is-theme)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body.

This has been stolen directly from ert by Christian Ohler <ohler@gnu.org>

Afterward it was modified for use with `ergoemacs-mode'. In
particular it:
- `define-key' is converted to `ergoemacs-theme-component--define-key' and keymaps are quoted
- `global-set-key' is converted to `ergoemacs-theme-component--global-set-key'
- `global-unset-key' is converted to `ergoemacs-theme-component--global-set-key'
- `global-reset-key' is converted `ergoemacs-theme-component--global-reset-key'
- `setq' and `set' is converted to `ergoemacs-theme-component--set'
- Mode initialization like (delete-selection-mode 1)
  or (delete-selection) is converted to
  `ergoemacs-theme-component--mode'
- Allows :version statement expansion
- Adds with-hook syntax or (when -hook) or (when -mode)
"
  (let ((extracted-key-accu '())
        last-was-version
        plist
        (remaining keys-and-body))
    ;; Allow
    ;; (component name)
    (unless (or (keywordp (first remaining)) (boundp 'skip-first))
      (if (condition-case nil
              (stringp (first remaining))
            (error nil))
          (push `(:name . ,(pop remaining)) extracted-key-accu)
        (push `(:name . ,(symbol-name (pop remaining))) extracted-key-accu))
      (when (memq (type-of (first remaining)) '(symbol cons))
        (pop remaining))
      (when (stringp (first remaining))
        (push `(:description . ,(pop remaining)) extracted-key-accu)))
    (while (and (consp remaining) (keywordp (first remaining)))
      (let ((keyword (pop remaining)))
        (unless (consp remaining)
          (error "Value expected after keyword %S in %S"
                 keyword keys-and-body))
        (when (assoc keyword extracted-key-accu)
          (warn "Keyword %S appears more than once in %S" keyword
                keys-and-body))
        (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    ;; Now change remaining (define-key keymap key def) to
    ;; (define-key 'keymap key def)
    ;; Also change (with-hook hook-name ) to (let ((ergoemacs-hook 'hook-name)))
    (unless is-theme
      (setq remaining
            (mapcar
             (lambda(elt)
               (cond
                (last-was-version
                 (setq last-was-version nil)
                 (if (stringp elt)
                     `(when (boundp 'component-version) (setq component-version ,elt))
                   `(when (boundp 'component-version) (setq component-version ,(symbol-name elt)))))
                ((condition-case nil
                     (eq elt ':version)
                   (error nil))
                 (setq last-was-version t)
                 nil)
                ((condition-case err
                     (eq (nth 0 elt) 'global-reset-key)
                   (error nil))
                 `(ergoemacs-theme-component--global-reset-key ,(nth 1 elt)))
                ((condition-case err
                     (eq (nth 0 elt) 'global-unset-key)
                   (error nil))
                 `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) nil))
                ((condition-case err
                     (eq (nth 0 elt) 'setq)
                   (error nil))
                 ;; Currently doesn't support (setq a b c d ), but it should.
                 `(ergoemacs-theme-component--set (quote ,(nth 1 elt)) ,(nth 2 elt)))
                ((condition-case err
                     (eq (nth 0 elt) 'set)
                   (error nil))
                 `(ergoemacs-theme-component--set (nth 1 elt) ,(nth 2 elt)))
                ((condition-case err
                     (string-match "-mode$" (symbol-name (nth 0 elt)))
                   (error nil))
                 `(ergoemacs-theme-component--mode (quote ,(nth 0 elt)) ,(nth 1 elt)))
                ((condition-case err
                     (eq (nth 0 elt) 'global-set-key)
                   (error nil))
                 (if (condition-case nil
                         (keymapp (symbol-value (nth 2 elt)))
                       (error nil))
                     (progn
                       `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) (quote ,(nth 2 elt))))
                   `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) ,(nth 2 elt))))
                ((condition-case err
                     (eq (nth 0 elt) 'define-key)
                   (error nil))
                 (if (equal (nth 1 elt) '(current-global-map))
                     (if (condition-case nil
                             (keymapp (symbol-value (nth 3 elt)))
                           (error nil))
                         `(ergoemacs-theme-component--global-set-key ,(nth 2 elt) (quote ,(nth 3 elt)))
                       `(ergoemacs-theme-component--global-set-key ,(nth 2 elt) ,(nth 3 elt)))
                   (if (condition-case nil
                           (keymapp (symbol-value (nth 3 elt)))
                         (error nil))
                       `(ergoemacs-theme-component--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) (quote ,(nth 3 elt)))
                     `(ergoemacs-theme-component--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) ,(nth 3 elt)))))
                ((or (condition-case err
                         (eq (nth 0 elt) 'with-hook)
                       (error nil))
                     (and (condition-case err
                              (eq (nth 0 elt) 'when)
                            (error nil))
                          (condition-case err
                              (string-match "-\\(hook\\|mode\\)$" (symbol-name (nth 1 elt)))
                            (error nil))))
                 (let (tmp skip-first)
                   (setq tmp (ergoemacs--parse-keys-and-body (cdr (cdr elt))))
                   `(let ((ergoemacs-hook (quote ,(nth 1 elt)))
                          (ergoemacs-hook-modify-keymap
                           ,(or (plist-get (nth 0 tmp)
                                           ':modify-keymap)
                                (plist-get (nth 0 tmp)
                                           ':modify-map)))
                          (ergoemacs-hook-full-shortcut-map
                           ,(or (plist-get (nth 0 tmp)
                                           ':full-shortcut-keymap)
                                (plist-get (nth 0 tmp)
                                           ':full-shortcut-map)
                                (plist-get (nth 0 tmp)
                                           ':full-map)
                                (plist-get (nth 0 tmp)
                                           ':full-keymap)))
                          (ergoemacs-hook-always ,(plist-get (nth 0 tmp)
                                                             ':always)))
                      ,@(nth 1 tmp))))
                (t elt)))
             remaining)))
    (setq plist (loop for (key . value) in extracted-key-accu
                      collect key
                      collect value))
    (list plist remaining)))

(defvar ergoemacs-theme-component-hash (make-hash-table :test 'equal))
(defvar ergoemacs-theme-component-cache (make-hash-table :test 'equal))
(defun ergoemacs-theme-component--version-bump ()
  (when (and (boundp 'component-version)
             component-version
             (boundp 'ergoemacs-component-version-minor-mode-layout)
             (boundp 'ergoemacs-component-version-curr)
             (boundp 'fixed-layout) (boundp 'variable-layout)
             (boundp 'fixed-layout-rm) (boundp 'variable-layout-rm)
             (boundp 'redundant-keys) (boundp 'defined-keys)
             (boundp 'versions)
             (boundp 'ergoemacs-just-first-reg)
             (not (equal ergoemacs-component-version-curr component-version)))
    ;; Create/Update component-version fixed or variable layouts.
    (when ergoemacs-component-version-curr
      (push (list ergoemacs-component-version-curr
                  ergoemacs-component-version-fixed-layout
                  ergoemacs-component-version-variable-layout
                  ergoemacs-component-version-redundant-keys
                  ergoemacs-component-version-minor-mode-layout
                  ergoemacs-component-version-fixed-layout-rm
                  ergoemacs-component-version-variable-layout-rm)
            ergoemacs-component-version-list))
    (setq ergoemacs-component-version-curr component-version)
    (push ergoemacs-component-version-curr versions)
    (unless ergoemacs-component-version-minor-mode-layout
      (setq ergoemacs-component-version-minor-mode-layout ergoemacs-component-version-minor-mode-layout))
    (unless ergoemacs-component-version-fixed-layout
      (setq ergoemacs-component-version-fixed-layout fixed-layout))
    (unless ergoemacs-component-version-fixed-layout-rm
      (setq ergoemacs-component-version-fixed-layout-rm fixed-layout-rm))
    (unless ergoemacs-component-version-fixed-layout
      (setq ergoemacs-component-version-variable-layout variable-layout))
    (unless ergoemacs-component-version-fixed-layout-rm
      (setq ergoemacs-component-version-variable-layout-rm variable-layout-rm))
    (unless ergoemacs-component-version-redundant-keys
      (setq ergoemacs-component-version-redundant-keys redundant-keys))))

(defun ergoemacs-theme-component--rm-key (key)
  "Remove KEY from `ergoemacs-mode' keymaps"
  (let* ((kd (key-description key))  jf 
         (variable-p (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                       (and (boundp 'ergoemacs-variable-reg)
                            ergoemacs-variable-reg
                            (condition-case nil
                                (string-match ergoemacs-variable-reg kd)
                              (error nil))))))
    (when variable-p
      (setq jf (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                 (and (boundp 'ergoemacs-just-first-reg) ergoemacs-just-first-reg
                      (condition-case nil
                          (string-match ergoemacs-just-first-reg kd)
                        (error nil))))))
    (cond
     ((and variable-p (boundp 'variable-layout-rm))
      (setq kd (ergoemacs-kbd kd t jf))
      (push (list kd jf) variable-layout-rm))
     ((boundp 'fixed-layout-rm)
      (push key fixed-layout-rm)))))

(defun ergoemacs-theme-component--global-reset-key (key)
  "Reset KEY.
will take out KEY from `ergoemacs-component-version-redundant-keys'"
  (when (and (boundp 'component-version)
             component-version
             (boundp 'ergoemacs-component-version-curr)
             (boundp 'fixed-layout) (boundp 'variable-layout)
             (boundp 'redundant-keys) (boundp 'defined-keys)
             (boundp 'versions)
             (boundp 'ergoemacs-component-version-redundant-keys)
             (boundp 'ergoemacs-just-first-reg))
    (ergoemacs-theme-component--version-bump)
    (let ((kd (key-description key))
          tmp)
      (setq tmp '())
      (mapc
       (lambda(x)
         (unless (string= x kd)
           (push x tmp)))
       ergoemacs-component-version-redundant-keys)
      (setq ergoemacs-component-version-redundant-keys tmp))))

(defun ergoemacs-theme-component--global-set-key (key command)
  "Setup ergoemacs theme component internally.
When fixed-layout and variable-layout are bound"
  (cond
   ((and (boundp 'ergoemacs-hook)
         (string-match "mode$" (symbol-name ergoemacs-hook)))
    (ergoemacs-theme-component--define-key ergoemacs-hook key command))
   ((and (vectorp key) (eq (elt key 0) 'remap))
    (let ((ergoemacs-hook 'ergoemacs-mode)
          (ergoemacs-hook-modify-keymap nil)
          (ergoemacs-hook-full-shortcut-map nil)
          (ergoemacs-hook-always nil))
      (ergoemacs-theme-component--define-key 'ergoemacs-mode key command)))
   ((and (boundp 'component-version)
         component-version
         (boundp 'ergoemacs-component-version-curr)
         (boundp 'fixed-layout) (boundp 'variable-layout)
         (boundp 'redundant-keys) (boundp 'defined-keys)
         (boundp 'versions)
         (boundp 'ergoemacs-just-first-reg))
    (ergoemacs-theme-component--version-bump)
    (let* ((kd (key-description key)) cd jf removed
           (variable-p (if (boundp 'ergoemacs-force-variable-reg)
                           ergoemacs-force-variable-reg
                         (and (boundp 'ergoemacs-variable-reg)
                              ergoemacs-variable-reg
                              (condition-case nil
                                  (string-match ergoemacs-variable-reg kd)
                                (error nil))))))
      (when cd
        (setq cd (car (cdr cd))))
      (if (not command)
          (mapc ;; Remove command from lists.
           (lambda(y)
             (let (tmp '())
               (mapc
                (lambda(x)
                  (unless (equal (nth 0 x) kd)
                    (push x tmp)))
                (symbol-value y))
               (set y tmp)))
           '(ergoemacs-component-version-fixed-layout ergoemacs-component-version-variable-layout))
        (if (not variable-p)
            (progn ;; Fixed Layout component
              (setq ergoemacs-component-version-fixed-layout
                    (mapcar
                     (lambda(x)
                       (if (not (equal (nth 0 x) kd))
                           x
                         (setq removed t)
                         (list kd command cd)))
                     ergoemacs-component-version-fixed-layout))
              (unless removed
                (push (list kd command cd) ergoemacs-component-version-fixed-layout)))
          ;; (push (list kd command) defined-keys)
          (setq jf (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                     (and (boundp 'ergoemacs-just-first-reg) ergoemacs-just-first-reg
                          (condition-case nil
                              (string-match ergoemacs-just-first-reg kd)
                            (error nil)))))
          (setq kd (ergoemacs-kbd kd t jf))
          (setq ergoemacs-component-version-variable-layout
                (mapcar
                 (lambda(x)
                   (if (not (equal (nth 0 x) kd))
                       x
                     (setq removed t)
                     (list kd command cd jf)))
                 ergoemacs-component-version-variable-layout))
          (unless removed
            (push (list kd command cd jf) ergoemacs-component-version-variable-layout))))))
   ((and (boundp 'fixed-layout) (boundp 'variable-layout)
         (boundp 'component-version)
         (not component-version)
         (boundp 'redundant-keys) (boundp 'defined-keys))
    (let ((kd (key-description key)) cd jf)
      (if (not command) ; redundant key
          (push kd redundant-keys)
        (setq cd (assoc command ergoemacs-function-short-names)) ; Short key description
        (when cd
          (setq cd (car (cdr cd))))
        (if (not (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                   (condition-case nil
                       (string-match ergoemacs-variable-reg kd)
                     (error nil))))
            (push (list kd command cd) fixed-layout) ;; Fixed layout component
          (push (list kd command) defined-keys)
          (setq jf (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                     (and ergoemacs-just-first-reg
                          (condition-case nil
                              (string-match ergoemacs-just-first-reg kd)
                            (error nil)))))
          (setq kd (ergoemacs-kbd kd t jf))
          (push (list kd command cd jf) variable-layout)))))))

(defun ergoemacs-theme-component--define-key (keymap key def)
  "Setup mode-specific information."
  (when (and (boundp 'fixed-layout) (boundp 'variable-layout))
    (if (memq keymap '(global-map ergoemacs-keymap))
        (if (and (eq keymap 'ergoemacs-keymap) (not def))
            (ergoemacs-theme-component--rm-key key)
          (ergoemacs-theme-component--global-set-key key def))
      (let* ((hook (or
                    (and (boundp 'ergoemacs-hook) ergoemacs-hook)
                    (intern (if (string-match "mode" (symbol-name keymap))
                                (replace-regexp-in-string "mode.*" "mode-hook" (symbol-name keymap))
                              ;; Assume -keymap or -map defines -mode-hook
                              (string-match "(key)?map" "mode-hook" (symbol-name keymap))))))
             (modify-keymap-p
              (and (boundp 'ergoemacs-hook-modify-keymap)
                   ergoemacs-hook-modify-keymap))
             (full-shortcut-p
              (and (boundp 'ergoemacs-hook-full-shortcut-map)
                   ergoemacs-hook-full-shortcut-map))
             (always-run-p (and (boundp 'ergoemacs-hook-always)
                                ergoemacs-hook-always))
             (kd (key-description key))
             (variable-p (and (boundp 'ergoemacs-variable-reg)
                              ergoemacs-variable-reg
                              (condition-case nil
                                  (string-match ergoemacs-variable-reg kd)
                                (error nil))))
             a-key
             jf found-1-p found-2-p)
        (when (and (boundp 'emulation-setup) (boundp 'ergoemacs-hook)
                   (string-match "mode$" (symbol-name ergoemacs-hook)))
          (add-to-list 'emulation-setup ergoemacs-hook nil 'eq))
        (when (boundp 'minor-mode-hook-list)
          (add-to-list 'minor-mode-hook-list hook nil 'eq))
        (when variable-p
          (setq variable-p t)
          (setq jf (if (boundp 'ergoemacs-force-variable-reg) ergoemacs-force-variable-reg
                     (and ergoemacs-just-first-reg
                          (condition-case nil
                              (string-match ergoemacs-just-first-reg kd)
                            (error nil)))))
          (setq kd (ergoemacs-kbd kd t jf)))
        (cond
         ((and (boundp 'component-version)
               component-version
               (boundp 'ergoemacs-component-version-curr)
               (boundp 'fixed-layout) (boundp 'variable-layout)
               (boundp 'redundant-keys) (boundp 'defined-keys)
               (boundp 'versions)
               (boundp 'ergoemacs-just-first-reg))
          (ergoemacs-theme-component--version-bump) ;; Change version information
          )
         ((and (boundp 'fixed-layout) (boundp 'variable-layout)
               (boundp 'component-version)
               (not component-version)
               (boundp 'redundant-keys) (boundp 'defined-keys))
          ;; Keymaps modified are stored as (hook (keymaps))
          ;; Keys are stored as ((hook keymap/t variable-p) ((key def)))
          (setq a-key (list hook (if modify-keymap-p keymap
                                   (or (and (boundp 'ergoemacs-hook)
                                            (string-match "mode$" (symbol-name ergoemacs-hook))
                                            ergoemacs-hook) t)) variable-p))
          (setq minor-mode-layout
                (mapcar
                 (lambda(elt)
                   (cond
                    ((eq (car elt) hook)
                     (let ((lst (car (cdr elt))))
                       (add-to-list 'lst (if modify-keymap-p keymap
                                           (or (and (boundp 'ergoemacs-hook)
                                                    (string-match "mode$" (symbol-name ergoemacs-hook))
                                                    ergoemacs-hook) t))  nil 'eq)
                       (setq found-1-p t)
                       (list hook lst)))
                    ((equal (car elt) a-key)
                     (let ((lst (car (cdr elt))) new-lst)
                       (mapc
                        (lambda(elt-2)
                          (cond
                           ((equal (car elt-2) kd)
                            (setq found-2-p t)
                            (push (list kd def jf) new-lst))
                           (t
                            (push elt-2 new-lst))))
                        lst)
                       (unless found-2-p
                         (push (list kd def) new-lst))
                       (setq found-2-p t)
                       (list a-key new-lst always-run-p full-shortcut-p)))
                    (t
                     elt)))
                 minor-mode-layout))
          (unless found-1-p
            (push (list hook (list (if modify-keymap-p keymap
                                     (or (and (boundp 'ergoemacs-hook)
                                              (string-match "mode$" (symbol-name ergoemacs-hook))
                                              ergoemacs-hook) t)))) minor-mode-layout))
          (unless found-2-p
            (push (list a-key (list (list kd def)) always-run-p full-shortcut-p) minor-mode-layout))))))))


(defun ergoemacs-theme-component--ignore-globally-defined-key (key)
  "Adds KEY to `ergoemacs-global-override-rm-keys' and `ergoemacs-global-override-map' if globally redefined."
  (let ((ergoemacs-ignore-advice t)
        (key (read-kbd-macro (key-description key) t)) lk)
    (catch 'found-global-command
      (while (>= (length key) 1)
        (setq lk (lookup-key (current-global-map) key))
        (when (and (ergoemacs-global-changed-p key)
                   (or (commandp lk t)
                       (keymapp lk)))
          (add-to-list 'ergoemacs-global-override-rm-keys key)
          (throw 'found-global-command t))
        (setq key (substring key 0 (- (length key) 1)))))))

(defun ergoemacs-theme-component--define-key-in-keymaps (keymap keymap-shortcut key def)
  "Defines KEY in KEYMAP or KEYMAP-SHORTCUT to be DEF.
Similar to `define-key'.

DEF can be:
1. A function; If globally defined, this is defined by
   `ergoemacs-shortcut-remap'
2. A list of functions
3. A keymap
4. A kbd-code that this shortcuts to with `ergoemacs-read'

"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (if (condition-case err
              (stringp (nth 0 def))
            (error nil))
          (progn
            (when (boundp 'shortcut-list)
              (push (list (read-kbd-macro (key-description key) t)
                          `(,(nth 0 def) ,(nth 1 def)))
                    shortcut-list))
            (ergoemacs-theme-component--ignore-globally-defined-key key)
            (define-key keymap-shortcut key 'ergoemacs-shortcut))
        (mapc
         (lambda(new-def)
           (unless found
             (when (condition-case err
                       (interactive-form new-def)
                     (error nil))
               (setq found
                     (ergoemacs-theme-component--define-key-in-keymaps
                      keymap keymap-shortcut key new-def)))))
         def))
      found))
   ((condition-case err
        (interactive-form def)
      (error nil))
    (cond
     ;; only setup on `ergoemacs-shortcut-keymap' when setting up
     ;; ergoemacs default keymap.
     ((memq def '(ergoemacs-ctl-c ergoemacs-ctl-x))
      (ergoemacs-theme-component--ignore-globally-defined-key key)
      (define-key keymap-shortcut key def))
     ((and (not (string-match "\\(mouse\\|wheel\\)" (key-description key)))
           (ergoemacs-shortcut-function-binding def))
      (when (boundp 'shortcut-list)
        (push (list (read-kbd-macro (key-description key) t)
                    (list def 'global)) shortcut-list))
      (if (ergoemacs-is-movement-command-p def)
          (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
              (progn
                (ergoemacs-theme-component--ignore-globally-defined-key key)
                (define-key keymap-shortcut key 'ergoemacs-shortcut-movement-no-shift-select))
            (ergoemacs-theme-component--ignore-globally-defined-key key)
            (define-key keymap-shortcut key 'ergoemacs-shortcut-movement))
        (ergoemacs-theme-component--ignore-globally-defined-key key)
        (define-key keymap-shortcut key 'ergoemacs-shortcut)))
     ((or (commandp def t) (keymapp def))
      (ergoemacs-theme-component--ignore-globally-defined-key key)
      (define-key keymap key def)))
    t)
   ((condition-case err
        (keymapp (symbol-value def))
      (error nil))
    (ergoemacs-theme-component--ignore-globally-defined-key key)
    (define-key keymap key (symbol-value def))
    t)
   ((condition-case err
        (or (vectorp def) (stringp def))
      (error nil))
    (progn
      (when (boundp 'shortcut-list)
        (push (list (read-kbd-macro (key-description key) t)
                    `(,(if (stringp def) def
                         (key-description def)) nil)) shortcut-list))
      (if (ergoemacs-is-movement-command-p def)
          (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
              (progn
                (ergoemacs-theme-component--ignore-globally-defined-key key)
                (define-key keymap-shortcut key 'ergoemacs-shortcut-movement-no-shift-select))
            (ergoemacs-theme-component--ignore-globally-defined-key key)
            (define-key keymap-shortcut key 'ergoemacs-shortcut-movement))
        (ergoemacs-theme-component--ignore-globally-defined-key key)
        (define-key keymap-shortcut key 'ergoemacs-shortcut)))
    t)
   (t nil)))

(defun ergoemacs-theme-component--set (elt value)
  "Direct `ergoemacs-mode' to set quoted ELT to VALUE when enabling a theme-component.
Will attempt to restore the value when turning off the component/theme."
  (push (list elt value) ergoemacs-theme-save-variables))

(defun ergoemacs-theme-component--mode (mode &optional value)
  "Direct `ergoemacs-mode' to turn on/off a minor-mode MODE with the optional argument VALUE.
Will attempt to restore the mode state when turning off the component/theme."
  (push (list mode (or value 1)) ergoemacs-theme-save-variables))

(defcustom ergoemacs-prefer-variable-keybindings t
  "Prefer Variable keybindings over fixed keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-theme--install-shortcut-item (key args keymap lookup-keymap
                                                   full-shortcut-map-p)
  (let (fn-lst)
    (cond
     ((condition-case err
          (interactive-form (nth 0 args))
        (error nil))
      (setq fn-lst (ergoemacs-shortcut-remap-list
                    (nth 0 args) lookup-keymap))
      (if fn-lst
          (condition-case nil
              (progn
                (ergoemacs-theme-component--ignore-globally-defined-key key)
                (define-key keymap key (nth 0 (nth 0 fn-lst)))))
        (when full-shortcut-map-p
          (condition-case nil
              (progn
                (ergoemacs-theme-component--ignore-globally-defined-key key)
                (when (or (commandp (nth 0 args) t)
                          (keymapp (nth 0 args)))
                  (define-key keymap key (nth 0 args))))))))
     (full-shortcut-map-p
      (condition-case nil
          (progn
            (ergoemacs-theme-component--ignore-globally-defined-key key)
            (define-key keymap key
              `(lambda(&optional arg)
                 (interactive "P")
                 (ergoemacs-read-key ,(nth 0 args) ',(nth 1 args))))))))))

(defun ergoemacs-theme--install-shortcuts-list (shortcut-list keymap lookup-keymap full-shortcut-map-p)
  "Install shortcuts for SHORTCUT-LIST into KEYMAP.
LOOKUP-KEYMAP
FULL-SHORTCUT-MAP-P "
  (mapc
   (lambda(y)
     (let ((key (nth 0 y))
           (args (nth 1 y)))
       (ergoemacs-theme--install-shortcut-item
        key args keymap lookup-keymap
        full-shortcut-map-p)))
   shortcut-list))

(defun ergoemacs-theme-component-keymaps-for-hook (hook component &optional version)
  "Gets the keymaps for COMPONENT and component VERSION for HOOK.
If the COMPONENT has the suffix :fixed, just get the fixed component.
If the COMPONENT has the suffix :variable, just get the variable component.
If the COMPONENT has the suffix ::version, just get the closest specified version.

If COMPONENT is a list, return the composite keymaps of all the
components listed.

The return value is an alist of keymaps needed for this hook.  The format is:
  ((map-name always-modify-p keymap-stub full-map))

map-name is the map name that will be modified
always-modify-p is a flag that will modify the keymap every time the hook is run.
keymaps-stub is the keymap overrides that will be installed.

When map-name is t or ends in -mode, it is for a keymap put in
`ergoemacs-emulation-mode-map-alist'.

This function does not finalize maps by installing them into the original maps.
"
  (if (eq (type-of component) 'cons)
      (let ((ret nil) ;; List of components.
            already-done-list
            (version version)
            always-modify-p) 
        (mapc
         (lambda(comp)
           (let ((new-ret (ergoemacs-theme-component-keymaps-for-hook hook comp version)))
             (if (not ret)
                 (setq ret new-ret)
               (setq already-done-list '())
               (setq ret
                     (mapcar
                      (lambda(keymap-list)
                        (let ((map-name (nth 0 keymap-list))
                              old-map new-map
                              full-map-p)
                          (setq new-map (assoc map-name new-ret))
                          (if (not new-map)
                              keymap-list
                            ;; Try to decompose keymaps as much as
                            ;; possible.  That way you won't have
                            ;; keymaps like:
                            ;; (keymap (keymap (keymap (keymap...))))
                            ;; For no reason.
                            (push map-name already-done-list)
                            (setq always-modify-p (or (nth 1 keymap-list) (nth 1 new-map)))
                            (setq new-map (nth 2 new-map)
                                  old-map (nth 2 keymap-list)
                                  full-map-p (or (nth 3 keymap-list)
                                                 (nth 3 new-map)))
                            (cond
                             ((and (keymapp (nth 1 new-map)) ; 2 composed.
                                   (keymapp (nth 1 old-map)))
                              (pop new-map)
                              (pop old-map)
                              (setq new-map (append new-map old-map))
                              (push 'keymap new-map))
                             ((and (keymapp (nth 1 old-map)) (keymapp new-map))
                              (pop old-map)
                              (setq new-map (append (list new-map) old-map))
                              (push 'keymap new-map))
                             ((and (keymapp (nth 1 new-map)) (keymapp old-map)) ;; New map is composed.
                              (pop new-map)
                              (setq new-map (append new-map (list old-map)))
                              (push 'keymap new-map))
                             ((and (keymapp new-map) (keymapp old-map))
                              ;; decomposed maps.
                              (setq new-map (make-composed-keymap (list new-map old-map))))
                             ((keymapp old-map)
                              (setq new-map old-map))
                             ((keymapp new-map)))
                            (list map-name always-modify-p new-map full-map-p))))
                      ret))
               (mapc
                (lambda(keymap-list)
                  (unless (member (nth 0 keymap-list) already-done-list)
                    (push keymap-list ret)))
                new-ret))))
         (reverse component))
        ret)
    ;; Single component
    (let ((true-component (replace-regexp-in-string ":\\(fixed\\|variable\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component))))
          (only-variable (string-match ":variable" (or (and (stringp component) component)
                                                       (symbol-name component))))
          (only-fixed (string-match ":fixed" (or (and (stringp component) component)
                                                 (symbol-name component))))
          fixed-maps variable-maps
          (true-version version)
          (version (or version (ergoemacs-theme-get-version)))
          minor-alist keymap-list shortcut-list
          always-p full-shortcut-map-p ret already-done-list)
      (when (string-match "::\\([0-9.]+\\)$" true-component)
        (setq version (match-string 1 true-component)
              true-component (replace-match "" nil nil true-component)))
      (if (not version)
          (setq version "")
        (setq version (ergoemacs-theme-component-get-closest-version
                       version
                       (gethash (concat true-component ":version")
                                ergoemacs-theme-component-hash))))
      (unless only-variable
        (setq fixed-maps (gethash (concat true-component version ":" (symbol-name hook) ":maps")
                                  ergoemacs-theme-component-cache))
        (unless fixed-maps
          ;; Setup fixed fixed-keymap for this component.
          (setq minor-alist
                (gethash (concat true-component version ":minor")
                         ergoemacs-theme-component-hash))
          (when minor-alist
            (setq keymap-list (assoc hook minor-alist))
            (when keymap-list
              (setq keymap-list (car (cdr keymap-list))
                    fixed-maps '())
              (mapc
               (lambda(map-name)
                 (let ((keys (assoc (list hook map-name nil) minor-alist))
                       (map (make-sparse-keymap))
                       always-p)
                   (when keys
                     (setq always-p (nth 2 keys)
                           full-shortcut-map-p (nth 3 keys)
                           keys (nth 1 keys))
                     (mapc
                      (lambda(key-list)
                        (cond
                         ((stringp (nth 1 key-list))
                          (ergoemacs-theme-component--ignore-globally-defined-key (read-kbd-macro (nth 0 key-list) t))
                          (define-key map (read-kbd-macro (nth 0 key-list) t)
                            `(lambda() (interactive) (ergoemacs-read-key ,(nth 1 key-list)))))
                         ((or (commandp (nth 1 key-list) t)
                              (keymapp (nth 1 key-list)))
                          (ergoemacs-theme-component--ignore-globally-defined-key (read-kbd-macro (nth 0 key-list) t))
                          (define-key map (read-kbd-macro (nth 0 key-list) t)
                            (nth 1 key-list)))))
                      (reverse keys))
                     (unless (equal map '(keymap))
                       (push `(,map-name ,always-p ,map
                                         ,full-shortcut-map-p) fixed-maps)))))
               keymap-list)
              (unless (equal fixed-maps '())
                (puthash (concat true-component version ":" (symbol-name hook) ":maps") fixed-maps
                         ergoemacs-theme-component-cache))))))

      (unless only-fixed
        (setq variable-maps (gethash (concat true-component version ":" ergoemacs-keyboard-layout ":" (symbol-name hook) ":maps") ergoemacs-theme-component-cache))
        (unless variable-maps
          ;; Setup variable keymaps for this component.
          (setq minor-alist
                (gethash (concat true-component version ":minor") ergoemacs-theme-component-hash))
          (when minor-alist
            (setq keymap-list (assoc hook minor-alist))
            (when keymap-list
              (setq keymap-list (car (cdr keymap-list))
                    variable-maps '())
              (mapc
               (lambda(map-name)
                 (let ((keys (assoc (list hook map-name t) minor-alist))
                       (map (make-sparse-keymap))
                       full-map
                       always-p)
                   (when keys
                     (setq always-p (nth 2 keys))
                     (setq full-shortcut-map-p (nth 3 keys))
                     (setq keys (nth 1 keys))
                     (mapc
                      (lambda(key-list)
                        (cond
                         ((stringp (nth 1 key-list))
                          (ergoemacs-theme-component--ignore-globally-defined-key (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list)))
                          (define-key map (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list))
                            `(lambda() (interactive) (ergoemacs-read-key ,(nth 1 key-list)))))
                         (t
                          (ergoemacs-theme-component--ignore-globally-defined-key (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list)))
                          (define-key map (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list))
                            (nth 1 key-list)))))
                      (reverse keys))
                     (unless (equal map '(keymap))
                       (push `(,map-name ,always-p ,map ,full-shortcut-map-p) variable-maps)))))
               keymap-list)
              (unless (equal variable-maps '())
                (puthash (concat true-component version ":" ergoemacs-keyboard-layout ":" (symbol-name hook) ":maps")
                         variable-maps ergoemacs-theme-component-cache))))))
      ;; Now variable maps
      (setq already-done-list '())
      (setq ret
            (mapcar
             (lambda(keymap-list)
               (let ((map-name (nth 0 keymap-list))
                     fixed-map composed-map tmp full-shortcut-map-p)
                 (setq fixed-map (assoc map-name fixed-maps))
                 (if (not fixed-map)
                     keymap-list
                   (push map-name already-done-list)
                   (setq full-shortcut-map-p (or (nth 3 keymap-list) (nth 2 fixed-map)))
                   ;; Need to decompose if needed...
                   (cond
                    ((and (keymapp (nth 1 (nth 2 keymap-list)))
                          (keymapp (nth 1 (nth 2 fixed-map))))
                     (setq composed-map (nth 2 keymap-list))
                     (pop composed-map)
                     (setq tmp (nth 2 fixed-map))
                     (pop tmp))
                    ((keymapp (nth 1 (nth 2 keymap-list)))
                     (setq composed-map (nth 2 keymap-list))
                     (pop composed-map)
                     (setq tmp (list (nth 2 fixed-map))))
                    ((keymapp (nth 1 (nth 2 fixed-map)))
                     (setq composed-map (list (nth 2 keymap-list)))
                     (setq tmp (nth 2 fixed-map))
                     (pop tmp))
                    (t
                     (setq composed-map (list (nth 2 keymap-list)))
                     (setq tmp (list (nth 2 fixed-map)))))
                   (setq composed-map
                         (if ergoemacs-prefer-variable-keybindings
                             (append composed-map tmp)
                           (append tmp composed-map))) 
                   (push 'keymap composed-map)
                   (setq tmp `(,map-name ,(or (nth 1 keymap-list) (nth 1 fixed-map))
                                         ,composed-map ,full-shortcut-map-p))
                   tmp)))
             variable-maps))
      (mapc
       (lambda(keymap-list)
         (unless (member (nth 0 keymap-list) already-done-list)
           (push keymap-list ret)))
       fixed-maps)
      ret)))

(defun ergoemacs-theme-component-keymaps (component &optional version)
  "Gets the keymaps for COMPONENT for component VERSION.
If the COMPONENT has the suffix :fixed, just get the fixed component.
If the COMPONENT has the suffix :variable, just get the variable component.
If the COMPONENT has the suffix ::version, just get the closest specified version.

If COMPONENT is a list, return the composite keymaps of all the
components listed.

Returns list of: read-keymap shortcut-keymap keymap shortcut-list unbind-keymap rm-keys emulation-setup vars.
"
  (if (eq (type-of component) 'cons)
      (let ((ret nil)
            k-l
            (l0 '())
            (l1 '())
            (l2 '())
            (l3 '())
            (l4 '())
            (l5 '())
            (l6 '())
            (l7 '())) ;; List of components.
        (mapc
         (lambda(comp)
           (let ((new-ret (ergoemacs-theme-component-keymaps comp version)))
             (when (and (nth 0 new-ret) (not (equal (nth 0 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 0 new-ret))))
                   (push (nth 0 new-ret) l0) ;; Not a composed keymap.
                 (setq k-l (nth 0 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l0 (append k-l l0))))
             (when (and (nth 1 new-ret) (not (equal (nth 1 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 1 new-ret))))
                   (push (nth 1 new-ret) l1) ;; Not a composed keymap.
                 (setq k-l (nth 1 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l1 (append k-l l1))))
             (when (and (nth 2 new-ret) (not (equal (nth 2 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 2 new-ret))))
                   (push (nth 2 new-ret) l2) ;; Not a composed keymap.
                 (setq k-l (nth 2 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l2 (append k-l l2))))
             (when (nth 3 new-ret)
               (setq l3 (append l3 (nth 3 new-ret))))
             (when (and (nth 4 new-ret) (not (equal (nth 4 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 4 new-ret))))
                   (push (nth 4 new-ret) l4) ;; Not a composed keymap.
                 (setq k-l (nth 4 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l4 (append k-l l4))))
             (when (nth 5 new-ret)
               (setq l5 (append l5 (nth 5 new-ret))))
             (when (nth 6 new-ret)
               (setq l6 (append l6 (nth 6 new-ret))))
             (when (nth 7 new-ret)
               (setq l7 (append l7 (nth 7 new-ret))))))
         (reverse component))
        (setq ret
              (list
               (make-composed-keymap l0)
               (make-composed-keymap l1)
               (make-composed-keymap l2)
               l3
               (make-composed-keymap l4)
               l5 l6 l7)))
    (let (fixed-shortcut
          fixed-read
          fixed-shortcut-list
          unbind
          variable-shortcut
          variable-read
          fixed variable
          fixed-rm variable-rm
          rm-lst
          key-list
          (ergoemacs-ignore-advice t)
          (case-fold-search t)
          key
          trans-key input-keys
          cmd cmd-tmp
          emulation-setup
          vars
          (version (or version (ergoemacs-theme-get-version)))
          (shortcut-list '())
          (true-component (replace-regexp-in-string ":\\(fixed\\|variable\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component))))
          (only-variable (string-match ":variable" (or (and (stringp component) component)
                                                       (symbol-name component))))
          (only-fixed (string-match ":fixed" (or (and (stringp component) component)
                                                 (symbol-name component)))))
      (when (string-match "::\\([0-9.]+\\)$" true-component)
        (setq version (match-string 1 true-component))
        (setq true-component (replace-match "" nil nil true-component)))
      (if (not version)
          (setq version "")
        (setq version (ergoemacs-theme-component-get-closest-version
                       version
                       (gethash (concat true-component ":version")
                                ergoemacs-theme-component-hash))))
      
      (setq unbind (gethash (concat true-component version ":unbind") ergoemacs-theme-component-cache))
      (setq emulation-setup (gethash (concat true-component ":emulation") ergoemacs-theme-component-hash))
      (setq vars (gethash (concat true-component ":vars") ergoemacs-theme-component-hash))
      (unless unbind
        (setq unbind (make-sparse-keymap))
        (mapc
         (lambda(x)
           (ergoemacs-theme-component--ignore-globally-defined-key (read-kbd-macro x))
           (define-key unbind (read-kbd-macro x) 'ergoemacs-undefined))
         (gethash (concat true-component version ":redundant") ergoemacs-theme-component-hash))
        (puthash (concat true-component version ":unbind") unbind ergoemacs-theme-component-cache))
      (unless only-variable
        (setq fixed-shortcut (gethash (concat true-component version ":fixed:shortcut") ergoemacs-theme-component-cache)
              fixed-read (gethash (concat true-component version ":fixed:read") ergoemacs-theme-component-cache)
              fixed (gethash (concat true-component version ":fixed:map") ergoemacs-theme-component-cache)
              fixed-rm (gethash (concat true-component version ":fixed-rm") ergoemacs-theme-component-cache)
              fixed-shortcut-list (gethash (concat true-component version
                                                   ":fixed:shortcut:list")
                                           ergoemacs-theme-component-cache))
        (unless (or fixed fixed-shortcut fixed-read fixed-shortcut-list)
          ;; Setup fixed fixed-keymap for this component.
          (setq key-list (gethash (concat true-component version ":fixed") ergoemacs-theme-component-hash))
          (when key-list
            (setq fixed-shortcut (make-sparse-keymap))
            (setq fixed (make-sparse-keymap))
            (setq fixed-read (make-sparse-keymap))
            (mapc
             (lambda(x)
               (when (and (eq 'string (type-of (nth 0 x))))
                 (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))
                 (setq key (read-kbd-macro trans-key))
                 (when (string-match "^\\([^ ]+\\) " (nth 0 x))
                   (add-to-list 'input-keys (match-string 1 (nth 0 x))))
                 (ergoemacs-theme-component--ignore-globally-defined-key key)
                 (setq cmd (nth 1 x))
                 (ergoemacs-theme-component--define-key-in-keymaps fixed fixed-shortcut key cmd)))
             (reverse key-list))
            (when input-keys
              (mapc
               (lambda(key)
                 (unless (member key ergoemacs-ignored-prefixes)
                   (ergoemacs-theme-component--ignore-globally-defined-key (read-kbd-macro key))
                   (define-key fixed-read (read-kbd-macro key)
                     `(lambda()
                        (interactive)
                        (ergoemacs-read-key ,key 'normal)))))
               (reverse input-keys)))
            (setq fixed-shortcut-list shortcut-list)
            (setq input-keys '())
            (setq shortcut-list '())
            (puthash (concat true-component version ":fixed:shortcut") fixed-shortcut
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component version ":fixed:read") fixed-read
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component version ":fixed:map") fixed
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component version ":fixed:shortcut:list")
                     fixed-shortcut-list ergoemacs-theme-component-cache))))

      (unless only-fixed
        (setq variable-shortcut (gethash (concat true-component ":" ergoemacs-keyboard-layout  version ":variable:shortcut") ergoemacs-theme-component-cache)
              variable-read (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:read") ergoemacs-theme-component-cache)
              variable (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:map") ergoemacs-theme-component-cache)
              variable-rm (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable-rm") ergoemacs-theme-component-cache)
              variable-shortcut-list (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:shortcut:list") ergoemacs-theme-component-cache))
        (unless (or variable variable-shortcut variable-read variable-shortcut-list)
          (setq variable-rm
                (mapcar
                 (lambda(x)
                   (ergoemacs-kbd (nth 0 x) nil (nth 1 x)))
                 (gethash (concat true-component version ":variable-rm") ergoemacs-theme-component-hash)))
          ;; Setup variable variable-keymap for this component.
          (setq key-list (gethash (concat true-component version ":variable") ergoemacs-theme-component-hash))
          (when key-list
            (setq variable-shortcut (make-sparse-keymap))
            (setq variable (make-sparse-keymap))
            (setq variable-read (make-sparse-keymap))
            (mapc
             (lambda(x)
               (when (and (eq 'string (type-of (nth 0 x))))
                 (setq key (ergoemacs-kbd (nth 0 x) nil (nth 3 x)))
                 (when (string-match "^\\([^ ]+\\) " (nth 0 x))
                   (add-to-list 'input-keys (match-string 1 (nth 0 x))))
                 (setq cmd (nth 1 x))
                 (ergoemacs-theme-component--define-key-in-keymaps variable variable-shortcut key cmd)))
             (reverse key-list))
            (when input-keys
              (mapc
               (lambda(key)
                 (unless (member key ergoemacs-ignored-prefixes)
                   (define-key variable-read (read-kbd-macro key)
                     `(lambda()
                        (interactive)
                        (ergoemacs-read-key ,key 'normal)))))
               (reverse input-keys)))
            (setq variable-shortcut-list shortcut-list
                  input-keys '())
            (setq shortcut-list '())
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:shortcut") variable-shortcut
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:read") variable-read
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:map") variable
                     ergoemacs-theme-component-cache)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable-rm") variable-rm ergoemacs-theme-component-cache)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:shortcut:list") variable-shortcut-list
                     ergoemacs-theme-component-cache))))
      (mapc
       (lambda(var)
         (when (equal (symbol-value var) '(keymap))
           (set var nil)))
       '(fixed-read
         fixed-shortcut
         fixed
         variable-read
         variable-shortcut
         variable
         unbind))
      (cond
       ((and variable-rm fixed-rm)
        (setq rm-lst (append variable-rm fixed-rm)))
       (variable-rm
        (setq rm-lst variable-rm))
       (fixed-rm
        (setq rm-lst fixed-rm)))
      (cond
       (only-fixed
        (list fixed-read fixed-shortcut fixed fixed-shortcut-list nil rm-lst emulation-setup vars))
       (only-variable
        (list variable-read variable-shortcut variable variable-shortcut-list nil rm-lst emulation-setup vars))
       (t
        (list (or (and variable-read fixed-read
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable-read fixed-read)
                                               (list fixed-read variable-read))))
                  variable-read fixed-read)
              (or (and variable-shortcut fixed-shortcut
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable-shortcut fixed-shortcut)
                                               (list fixed-shortcut variable-shortcut))))
                  variable-shortcut fixed-shortcut)
              (or (and variable fixed
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable fixed)
                                               (list fixed variable))))
                  variable fixed)
              (if ergoemacs-prefer-variable-keybindings
                  (append variable-shortcut-list fixed-shortcut-list)
                (append fixed-shortcut-list variable-shortcut-list))
              unbind rm-lst emulation-setup vars))))))


(defvar ergoemacs-theme-hook-installed '()
  "Installed hooks")
(defun ergoemacs-theme-hook (hook)
  "Run `ergoemacs-mode' HOOK."
  (let (deactivate-mark
        ;; Emulation variable for map.
        (emulation-var (if (not (string-match "mode$" (symbol-name hook)))
                           (intern (concat "ergoemacs--emulation-for-" (symbol-name hook)))
                         hook))
        (all-always-p t)
        x)
    (unless (string-match "mode$" (symbol-name hook))
      (unless (boundp hook)
        (set-default hook nil)
        (set hook nil))
      (when (boundp emulation-var)
        (set (make-local-variable emulation-var) t)))
    (unless (member hook ergoemacs-theme-hook-installed)
      (mapc
       (lambda(x)
         (let ((map-name (nth 0 x))
               (always-modify-p (nth 1 x))
               (replace (nth 2 x)))
           (cond
            ((or (and (eq hook map-name)
                      (string-match "mode$" (symbol-name map-name))
                      (not (member (list hook map-name)
                                   ergoemacs-theme-hook-installed)))
                 (and (eq map-name 't)
                      (not (member (list hook t) ergoemacs-theme-hook-installed))))
             (unless (boundp emulation-var)
               (set-default emulation-var nil)
               (set (make-local-variable emulation-var) t))
             (setq x (assq emulation-var ergoemacs-emulation-mode-map-alist))
             (when x
               (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
             (setq ergoemacs-emulation-mode-map-alist
                   (append ergoemacs-emulation-mode-map-alist
                           (list (cons emulation-var replace))))
             (if always-modify-p
                 (setq all-always-p nil)
               (push (list hook map-name) ergoemacs-theme-hook-installed)))
            ((not (member (list hook map-name) ergoemacs-theme-hook-installed))
             (unless (string-match "mode$" (symbol-name map-name))
               (set map-name (copy-keymap replace)))
             (if always-modify-p
                 (setq all-always-p nil)
               (push (list hook map-name) ergoemacs-theme-hook-installed))))))
       (ergoemacs-theme-keymaps-for-hook hook ergoemacs-theme))
      (unless all-always-p
        (push hook ergoemacs-theme-hook-installed)))))

(defun ergoemacs-theme-component-make-hooks (component &optional remove-p)
  "Make ergoemacs-mode hooks for COMPONENT.
COMPONENT may also be a list of components.

When REMOVE-P, remove the created ergoemacs-mode hook functions
from the appropriate startup hooks.  Otherwise the hooks are
added to the appropriate startup hooks.
"
  (if (eq (type-of component) 'cons)
      (mapc
       (lambda(c)
         (ergoemacs-theme-component-make-hooks c remove-p))
       component)
    (let ((true-component (replace-regexp-in-string ":\\(fixed\\|variable\\|:[0-9.]+\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component)))))
      (mapc
       (lambda(hook)
         (when (string-match "hook\\'" (symbol-name hook))
           (fset (intern (concat "ergoemacs-for-" (symbol-name hook)))
                 `(lambda ()
                    ,(format "Run `ergoemacs-theme-hook' for `%s'"
                             (symbol-name hook))
                    (ergoemacs-theme-hook ',hook)))
           (funcall (if remove-p #'remove-hook #'add-hook) hook
                    (intern (concat "ergoemacs-for-" (symbol-name hook))))))
       (gethash (concat true-component ":minor-list")
                ergoemacs-theme-component-hash)))))

(defun ergoemacs--theme-component (plist body)
  (let ((name (plist-get plist ':name))
       (desc (or (plist-get plist ':description) ""))
       (layout (or (plist-get plist ':layout) "us"))
       (ergoemacs-variable-reg (or (plist-get plist ':variable-reg)
                                   (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>")))))
       (ergoemacs-just-first-reg (or (plist-get plist ':first-is-variable-reg)
                                     nil))
       (versions '())
       (component-version nil)
       (ergoemacs-component-version-variable-layout nil)
       (ergoemacs-component-version-variable-layout-rm nil)
       (ergoemacs-component-version-fixed-layout nil)
       (ergoemacs-component-version-fixed-layout-rm nil)
       (ergoemacs-component-version-redundant-keys nil)
       (ergoemacs-component-version-minor-mode-layout nil)
       (ergoemacs-component-version-curr nil)
       (ergoemacs-component-version-list '())
       (defined-keys '())
       (variable-layout '())
       (variable-layout-rm '())
       (fixed-layout '())
       (fixed-layout-rm '())
       (defined-commands '())
       (minor-mode-layout '())
       (minor-mode-hook-list '())
       (emulation-setup '())
       (redundant-keys '())
       (ergoemacs-translation-from ergoemacs-translation-from)
       (ergoemacs-translation-to ergoemacs-translation-to)
       (ergoemacs-shifted-assoc ergoemacs-shifted-assoc)
       (ergoemacs-needs-translation ergoemacs-needs-translation)
       (ergoemacs-translation-assoc ergoemacs-translation-assoc)
       (ergoemacs-translation-regexp ergoemacs-translation-regexp)
       (case-fold-search nil)
       (ergoemacs-theme-save-variables '()))
    (when (ad-is-advised 'define-key)
      ;; FIXME:
      ;; - AFAICT, this advice is never re-enabled.
      ;; - ad-disable-advice only marks the advice as disabled, but doesn't
      ;;   actually disable it yet (we'd need to deactivate+reactivate the
      ;;   advice for that).
      (ad-disable-advice 'define-key 'around 'ergoemacs-define-key-advice))
    (ergoemacs-setup-translation "us" layout) ; Make sure keys are
                                             ; stored in QWERTY
                                             ; notation.
    (funcall body)
    ;; Finalize version setup
    (when ergoemacs-component-version-curr
      (push (list ergoemacs-component-version-curr
                 ergoemacs-component-version-fixed-layout
                 ergoemacs-component-version-variable-layout
                 ergoemacs-component-version-redundant-keys
                 ergoemacs-component-version-minor-mode-layout
                 ergoemacs-component-version-fixed-layout-rm
                 ergoemacs-component-version-variable-layout-rm)
           ergoemacs-component-version-list))
    (puthash (concat name ":plist") plist ergoemacs-theme-component-hash)
    (puthash (concat name ":fixed") fixed-layout ergoemacs-theme-component-hash)
    (puthash (concat name ":fixed-rm") fixed-layout-rm ergoemacs-theme-component-hash)
    (puthash (concat name ":variable") variable-layout ergoemacs-theme-component-hash)
    (puthash (concat name ":variable-rm") variable-layout-rm ergoemacs-theme-component-hash)
    (puthash (concat name ":version") versions ergoemacs-theme-component-hash)
    (puthash (concat name ":redundant") redundant-keys ergoemacs-theme-component-hash)
    (puthash (concat name ":minor") minor-mode-layout ergoemacs-theme-component-hash)
    (puthash (concat name ":minor-list") minor-mode-hook-list ergoemacs-theme-component-hash)
    (puthash (concat name ":emulation") emulation-setup ergoemacs-theme-component-hash)
    (puthash (concat name ":vars") ergoemacs-theme-save-variables ergoemacs-theme-component-hash)
    (mapc
     (lambda(x)
       (let ((ver (nth 0 x))
            (fixed (nth 1 x))
            (var (nth 2 x))
            (red (nth 3 x))
            (fixed-rm (nth 4 x))
            (var-rm (nth 5 x)))
        (puthash (concat name "::" ver ":fixed") fixed ergoemacs-theme-component-hash)
        (puthash (concat name "::" ver ":variable") var ergoemacs-theme-component-hash)
        (puthash (concat name "::" ver ":redundant") var ergoemacs-theme-component-hash)
        (puthash (concat name "::" ver ":fixed-rm") fixed-rm ergoemacs-theme-component-hash)
        (puthash (concat name "::" ver ":variable-rm") var-rm ergoemacs-theme-component-hash)))
     ergoemacs-component-version-list)))

(defmacro ergoemacs-theme-component (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs--parse-keys-and-body body-and-plist))
    `(let ((name ,(plist-get (nth 0 kb) ':name))
           (desc ,(or (plist-get (nth 0 kb) ':description) ""))
           (layout ,(or (plist-get (nth 0 kb) ':layout) "us"))
           (ergoemacs-variable-reg ,(or (plist-get (nth 0 kb) ':variable-reg)
                                        (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>")))))
           (ergoemacs-just-first-reg ,(or (plist-get (nth 0 kb) ':first-is-variable-reg)
                                          nil))
           (versions '())
           (component-version nil)
           (ergoemacs-component-version-variable-layout nil)
           (ergoemacs-component-version-variable-layout-rm nil)
           (ergoemacs-component-version-fixed-layout nil)
           (ergoemacs-component-version-fixed-layout-rm nil)
           (ergoemacs-component-version-redundant-keys nil)
           (ergoemacs-component-version-minor-mode-layout nil)
           (ergoemacs-component-version-curr nil)
           (ergoemacs-component-version-list '())
           (defined-keys '())
           (variable-layout '())
           (variable-layout-rm '())
           (fixed-layout '())
           (fixed-layout-rm '())
           (defined-commands '())
           (minor-mode-layout '())
           (minor-mode-hook-list '())
           (emulation-setup '())
           (redundant-keys '())
           (ergoemacs-translation-from ergoemacs-translation-from)
           (ergoemacs-translation-to ergoemacs-translation-to)
           (ergoemacs-shifted-assoc ergoemacs-shifted-assoc)
           (ergoemacs-needs-translation ergoemacs-needs-translation)
           (ergoemacs-translation-assoc ergoemacs-translation-assoc)
           (ergoemacs-translation-regexp ergoemacs-translation-regexp)
           (case-fold-search nil)
           (ergoemacs-theme-save-variables '()))
       (when (ad-is-advised 'define-key)
         (ad-disable-advice 'define-key 'around 'ergoemacs-define-key-advice))
       (ergoemacs-setup-translation "us" layout) ; Make sure keys are
                                        ; stored in QWERTY
                                        ; notation.
       ,@(nth 1 kb)
       ;; Finalize version setup
       (when ergoemacs-component-version-curr
         (push (list ergoemacs-component-version-curr
                     ergoemacs-component-version-fixed-layout
                     ergoemacs-component-version-variable-layout
                     ergoemacs-component-version-redundant-keys
                     ergoemacs-component-version-minor-mode-layout
                     ergoemacs-component-version-fixed-layout-rm
                     ergoemacs-component-version-variable-layout-rm)
               ergoemacs-component-version-list))
       (puthash (concat name ":plist") ',(nth 0 kb) ergoemacs-theme-component-hash)
       (puthash (concat name ":fixed") fixed-layout ergoemacs-theme-component-hash)
       (puthash (concat name ":fixed-rm") fixed-layout-rm ergoemacs-theme-component-hash)
       (puthash (concat name ":variable") variable-layout ergoemacs-theme-component-hash)
       (puthash (concat name ":variable-rm") variable-layout-rm ergoemacs-theme-component-hash)
       (puthash (concat name ":version") versions ergoemacs-theme-component-hash)
       (puthash (concat name ":redundant") redundant-keys ergoemacs-theme-component-hash)
       (puthash (concat name ":minor") minor-mode-layout ergoemacs-theme-component-hash)
       (puthash (concat name ":minor-list") minor-mode-hook-list ergoemacs-theme-component-hash)
       (puthash (concat name ":emulation") emulation-setup ergoemacs-theme-component-hash)
       (puthash (concat name ":vars") ergoemacs-theme-save-variables ergoemacs-theme-component-hash)
       (mapc
        (lambda(x)
          (let ((ver (nth 0 x))
                (fixed (nth 1 x))
                (var (nth 2 x))
                (red (nth 3 x))
                (fixed-rm (nth 4 x))
                (var-rm (nth 5 x)))
            (puthash (concat name "::" ver ":fixed") fixed ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":variable") var ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":redundant") var ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":fixed-rm") fixed-rm ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":variable-rm") var-rm ergoemacs-theme-component-hash)))
        ergoemacs-component-version-list))))
;;; Theme functions

(defun ergoemacs-theme-set-version (version)
  "Sets the current themes default VERSION"
  (let (found)
    (setq ergoemacs-theme-version
          (mapcar
           (lambda(elt)
             (if (not (equal ergoemacs-theme (nth 0 elt)))
                 elt
               (setq found t)
               (list ergoemacs-theme version)))
           ergoemacs-theme-version))
    (unless found
      (push (list ergoemacs-theme version) ergoemacs-theme-version))))

(defun ergoemacs-theme-get-version ()
  "Gets the current version for the current theme"
  (let ((theme-ver (assoc ergoemacs-theme ergoemacs-theme-version)))
    (if (not theme-ver) nil
      (car (cdr theme-ver)))))

(defun ergoemacs-theme-versions (theme)
  "Get a list of versions for the current theme."
  (let ((theme-plist (gethash (if (stringp theme) theme
                                (symbol-name theme))
                              ergoemacs-theme-hash))
        versions)
    (mapc
     (lambda(component)
       (let ((true-component
              (replace-regexp-in-string
               ":\\(fixed\\|variable\\)" ""
               (or (and (stringp component) component)
                   (symbol-name component))))
             vers)
         (when (string-match "::\\([0-9.]+\\)$" true-component)
           (setq true-component (replace-match "" nil nil true-component)))
         (mapc
          (lambda(ver)
            (add-to-list 'versions ver))
          (gethash (concat true-component ":version")
                   ergoemacs-theme-component-hash))))
     (append (plist-get theme-plist ':optional-off)
             (plist-get theme-plist ':optional-on)
             (plist-get theme-plist ':components)))
    (setq versions (sort versions 'string<))
    versions))

(defun ergoemacs-theme-components (&optional theme)
  "Get a list of components used for the current theme.
This respects `ergoemacs-theme-options'."
  (let* ((theme (or theme ergoemacs-theme))
         (theme-plist (gethash (if (stringp theme) theme
                                 (symbol-name theme))
                               ergoemacs-theme-hash))
         components)
    (setq components (reverse (plist-get theme-plist ':components)))
    (mapc
     (lambda(x)
       (let ((a (assoc x ergoemacs-theme-options)))
         (if (not a)
             (push x components)
           (setq a (car (cdr a)))
           (when (or (not a) (eq a 'on))
             (push x components)))))
     (reverse (plist-get theme-plist ':optional-on)))
    (mapc
     (lambda(x)
       (let ((a (assoc x ergoemacs-theme-options)))
         (when a
           (setq a (car (cdr a)))
           (when (eq a 'on)
             (push x components)))))
     (reverse (plist-get theme-plist ':optional-off)))
    (setq components (reverse components))
    components))

(defun ergoemacs-theme-make-hooks (theme &optional remove-p)
  "Creates hooks for THEME.

When REMOVE-P, remove the created ergoemacs-mode hook functions
from the appropriate startup hooks.  Otherwise the hooks are
added to the appropriate startup hooks.
"
  (ergoemacs-theme-component-make-hooks (ergoemacs-theme-components theme) remove-p))

(defun ergoemacs-theme-keymaps-for-hook (hook theme &optional version)
  "Gets the keymaps for the HOOK specific to the THEME and VERSION specified.

The return value is an alist of keymaps needed for this hook.
The format is:

  ((map-name always-modify-p keymap-replacement))

map-name is the map name that will be modified
always-modify-p is a flag that will modify the keymap every time the hook is run.
keymaps-stub is the keymap overrides that will be installed.

When map-name is t, it is for a keymap put in
`ergoemacs-emulation-mode-map-alist'

Uses `ergoemacs-theme-component-keymaps-for-hook' and
`ergoemacs-theme-components'"
  ;;
  (let ((theme-components (ergoemacs-theme-components theme))
        overall-keymaps)
    (setq overall-keymaps (ergoemacs-theme-keymaps theme version))
    ;; 0:read-keymap 1:shortcut-keymap 2:keymap 3:shortcut-list 4:unbind-keymap.

    (mapcar
     (lambda(c)
       (if (or (eq (nth 0 c) 't)
               (and (string-match "mode$" (symbol-name (nth 0 c)))
                    (eq (nth 0 c) hook)))
           (progn
             (list (nth 0 c) (nth 1 c) (nth 2 c)))
         (let ((map-name (nth 0 c))
               (always-modify-p (nth 1 c))
               (base-keymap (nth 2 c))
               (full-keymap-p (nth 3 c))
               (shortcut-map (make-sparse-keymap)) tmp
               orig-map
               final-map)
           (setq orig-map (gethash (concat (symbol-name map-name) (symbol-name hook) ":original-map") ergoemacs-theme-component-cache))
           (unless orig-map
             (when (boundp map-name)
               (unwind-protect
                   (setq orig-map (copy-keymap (symbol-value map-name)))))
             (when orig-map
               (puthash (concat (symbol-name map-name) (symbol-name hook) ":original-map") orig-map ergoemacs-theme-component-cache)))
           (if (not orig-map)
               (setq orig-map (make-sparse-keymap))
             (setq orig-map (copy-keymap orig-map))
             (ergoemacs-theme--install-shortcuts-list
              (nth 3 overall-keymaps) shortcut-map
              orig-map full-keymap-p))
           
           (if (and (keymapp (nth 1 base-keymap))
                    (eq 'keymap (nth 0 base-keymap)))
               (pop base-keymap)
             (setq base-keymap (list base-keymap)))
           (if (not full-keymap-p)
               (setq final-map (make-composed-keymap
                                (if (not (and (eq 'keymap (nth 0 shortcut-map))
                                              (keymapp (nth 1 shortcut-map))))
                                    (append base-keymap (list shortcut-map))
                                  (pop shortcut-map)
                                  (append base-keymap shortcut-map)) orig-map))
             (setq base-keymap
                   (if (not (and (eq 'keymap (nth 0 shortcut-map))
                                 (keymapp (nth 1 shortcut-map))))
                       (append base-keymap (list shortcut-map))
                     (pop shortcut-map)
                     (append base-keymap shortcut-map)))
             (when (nth 0 overall-keymaps)
               (setq base-keymap (append (nth 0 overall-keymaps) base-keymap)))
             ;; Set parent to original keymap and compose read-keymap.
             (when (= (length base-keymap) 1)
               ;; ((keymap)) to (keymap)
               (setq base-keymap (nth 0 base-keymap)))
             (setq final-map (make-composed-keymap base-keymap orig-map)))
           (when (and (= 2 (length final-map))
                      (eq (nth 0 final-map) 'keymap)
                      (keymapp (nth 1 final-map)))
             ;; Take care of (keymap (keymap ...))
             (setq final-map (nth 1 final-map)))
           (when full-keymap-p  ; Don't install shortcuts up.
             (define-key final-map '[ergoemacs] 'ignore))
           (list map-name always-modify-p final-map))))
     (ergoemacs-theme-component-keymaps-for-hook hook theme-components version))))

;;;###autoload
(defun ergoemacs-theme-option-off (option)
  "Turns OPTION off.
Uses `ergoemacs-theme-option-on'."
  (ergoemacs-theme-option-on option 'off))

(defun ergoemacs-require (option &optional theme type)
  "Requires an OPTION on ergoemacs themes.

THEME can be a single theme or list of themes to apply the option
to.  If unspecified, it is all themes.

TYPE can be nil, where the option will be turned on by default
but shown as something that can be toggled in the ergoemacs-mode
menu.

TYPE can also be 'required-hidden, where the option is turned on,
and it dosen't show up on the ergoemacs-mode menu.

TYPE can also be 'off, where the option will be included in the
theme, but assumed to be disabled by default.
"
  (if (eq (type-of option) 'cons)
      (mapc
       (lambda(new-option)
         (let (ergoemacs-mode)
           (ergoemacs-require new-option theme type)))
       option)
    (let ((option-sym
           (or (and (stringp option) (intern option)) option)))
      (mapc
     (lambda(theme)
       (let ((theme-plist (gethash (if (stringp theme) theme
                                     (symbol-name theme))
                                   ergoemacs-theme-hash))
             comp on off)
         (setq comp (plist-get theme-plist ':components)
               on (plist-get theme-plist ':optional-on)
               off (plist-get theme-plist ':optional-off))
         (setq comp (delq option-sym comp)
               on (delq option-sym on)
               off (delq option-sym off))
         (cond
          ((eq type 'required-hidden)
           (push option-sym comp))
          ((eq type 'off)
           (push option-sym off))
          (t
           (push option-sym on)))
         (setq theme-plist (plist-put theme-plist ':components comp))
         (setq theme-plist (plist-put theme-plist ':optional-on on))
         (setq theme-plist (plist-put theme-plist ':optional-off off))
         (puthash (if (stringp theme) theme (symbol-name theme)) theme-plist
                  ergoemacs-theme-hash)))
     (or (and theme (or (and (eq (type-of theme) 'cons) theme) (list theme)))
         (ergoemacs-get-themes)))))
  (ergoemacs-theme-option-on option))

;;;###autoload
(defun ergoemacs-theme-option-on (option &optional off)
  "Turns OPTION on.
When OPTION is a list turn on all the options in the list
If OFF is non-nil, turn off the options instead."
  (if (eq (type-of option) 'cons)
      (mapc
       (lambda(new-option)
         (let (ergoemacs-mode)
           (ergoemacs-theme-option-on new-option off)))
       option)
    (let (found)
      (setq ergoemacs-theme-options
            (mapcar
             (lambda(elt)
               (if (not (eq (nth 0 elt) option))
                   elt
                 (setq found t)
                 (if off
                     (list option 'off)
                   (list option 'on))))
             ergoemacs-theme-options))
      (unless found
        (push (if off (list option 'off) (list option 'on))
              ergoemacs-theme-options))))
  (when ergoemacs-mode
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)))

(defun ergoemacs-theme-toggle-option (option)
  "Toggles theme OPTION."
  (if (ergoemacs-theme-option-enabled-p option)
      (ergoemacs-theme-option-off option)
    (ergoemacs-theme-option-on option)))

(defun ergoemacs-theme-option-enabled-p (option)
  "Determines if OPTION is enabled."
  (let ((plist (gethash ergoemacs-theme ergoemacs-theme-hash))
        options-on options-off)
    (setq options-on (plist-get plist ':optional-on)
          options-off (plist-get plist ':optional-off))
    (or (and (member option options-on)
             (not (member (list option 'off) ergoemacs-theme-options)))
        (and (member option options-off)
             (member (list option 'on) ergoemacs-theme-options)))))

(defun ergoemacs-keymap-menu-theme-options (theme)
  "Gets the options menu for THEME."
  (let ((plist (gethash theme ergoemacs-theme-hash))
        (menu-list '())
        (menu-pre '())
        (options-on '())
        (options-off '())
        (menu-options '())
        (options-list '())
        (options-alist '())
        (i 0))
    (setq options-on (plist-get plist ':optional-on)
          options-off (plist-get plist ':optional-off)
          menu-list (plist-get plist ':options-menu))
    (if (= 0 (length (append options-on options-off))) nil
      (mapc
       (lambda(elt)
         (let ((menu-name (nth 0 elt))
               (menu-items (nth 1 elt))
               desc plist2
               (ret '()))
           (mapc
            (lambda(option)
              (when (memq option (append options-on options-off))
                (setq plist2 (gethash (concat (symbol-name option) ":plist") ergoemacs-theme-component-hash))
                (setq desc (plist-get plist2 ':description))
                (push option menu-options)
                (push
                 `(,option
                   menu-item ,desc
                   (lambda()
                     (interactive)
                     (ergoemacs-theme-toggle-option ',option)
                     (ergoemacs-mode -1)
                     (ergoemacs-mode 1))
                   :button (:toggle . (ergoemacs-theme-option-enabled-p ',option)))
                 ret)))
            (reverse menu-items))
           (unless (eq ret '())
             (setq ret
                   `(,(intern (format "options-menu-%s" i))
                     menu-item ,menu-name
                     (keymap ,@ret)))
             (setq i (+ i 1))
             (push ret menu-pre))))
       (reverse menu-list))
      (mapc
       (lambda(option)
         (unless (member option menu-options)
           (let ((plist2 (gethash (concat (symbol-name option) ":plist") ergoemacs-theme-component-hash))
                 desc)
             (setq desc (plist-get plist2 ':description))
             (push desc options-list)
             (push (list desc option) options-alist))))
       (append options-on options-off))
      `(ergoemacs-theme-options
        menu-item "Theme Options"
        (keymap
         ,@menu-pre
         ,@(mapcar
            (lambda(desc)
              (let ((option (car (cdr (assoc desc options-alist)))))
                `(,option
                  menu-item ,desc
                  (lambda()
                    (interactive)
                    (ergoemacs-theme-toggle-option ',option)
                    (ergoemacs-mode -1)
                    (ergoemacs-mode 1))
                  :button (:toggle . (ergoemacs-theme-option-enabled-p ',option)))))
            (sort options-list 'string<)))))))

(defun ergoemacs-keymap-menu-theme-version (theme)
  "Gets version menu for THEME"
  (let ((theme-versions (ergoemacs-theme-versions theme)))
    (if (not theme-versions) nil
      `(ergoemacs-versions
        menu-item "Theme Versions"
        (keymap
         (ergoemacs-current-version
          menu-item "Current Version"
          (lambda()
            (interactive)
            (ergoemacs-theme-set-version nil)
            (ergoemacs-mode -1)
            (ergoemacs-mode 1))
          :button (:radio . (equal (ergoemacs-theme-get-version) nil)))
         ,@(mapcar
            (lambda(version)
              `(,(intern version) menu-item ,version
                (lambda() (interactive)
                  (ergoemacs-theme-set-version ,version)
                  (ergoemacs-mode -1)
                  (ergoemacs-mode 1))
                :button (:radio . (equal (ergoemacs-theme-get-version) ,version))))
            theme-versions))))))

(defun ergoemacs-keymap-menu (theme)
  "Defines menus for current THEME."
  `(keymap
    ,(ergoemacs-get-layouts-menu)
    (ergoemacs-theme-sep "--")
    (ergoemacs-themes
     menu-item "Themes"
     (keymap
      ,@(mapcar
         (lambda(theme)
           `(,(intern theme) menu-item ,(concat theme " - " (plist-get (gethash theme ergoemacs-theme-hash) ':description))
             (lambda() (interactive)
               (ergoemacs-set-default 'ergoemacs-theme ,theme))
             :button (:radio . (string= ergoemacs-theme ,theme))))
         (sort (ergoemacs-get-themes) 'string<))))
    ,(ergoemacs-keymap-menu-theme-options theme)
    ,(ergoemacs-keymap-menu-theme-version theme)
    (ergoemacs-c-x-sep "--")
    (ergoemacs-c-x-c-c
     menu-item "Ctrl+C and Ctrl+X behavior"
     (keymap
      (c-c-c-x-emacs
       menu-item "Ctrl+C and Ctrl+X are for Emacs Commands"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)))
      (c-c-c-x-cua
       menu-item "Ctrl+C and Ctrl+X are only Copy/Cut"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)))
      (c-c-c-x-both
       menu-item "Ctrl+C and Ctrl+X are both Emacs Commands & Copy/Cut"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'both))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)))
      (c-c-c-x-timeout
       menu-item "Customize Ctrl+C and Ctrl+X Cut/Copy Timeout"
       (lambda() (interactive)
         (customize-variable 'ergoemacs-ctl-c-or-ctl-x-delay)))))
    (c-v
     menu-item "Paste behavior"
     (keymap
      (c-v-multiple
       menu-item "Repeating Paste pastes multiple times"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-smart-paste nil))
       :button (:radio . (eq ergoemacs-smart-paste 'nil)))
      (c-v-cycle
       menu-item "Repeating Paste cycles through previous pastes"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-smart-paste t))
       :button (:radio . (eq ergoemacs-smart-paste 't)))
      (c-v-kill-ring
       menu-item "Repeating Paste starts browse-kill-ring"
       (lambda()
         (interactive)
         (set-default 'ergoemacs-smart-paste 'browse-kill-ring))
       :enable (condition-case err (interactive-form 'browse-kill-ring)
                 (error nil))
       :button (:radio . (eq ergoemacs-smart-paste 'browse-kill-ring)))))
    (ergoemacs-sep-bash "--")
    (ergoemacs-bash
     menu-item "Make Bash aware of ergoemacs keys"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-bash)))
    (ergoemacs-ahk
     menu-item "Make Windows aware of ergoemacs keys (Requires Autohotkey)"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-gen-ahk)))
    (ergoemacs-sep-menu "--")
    (ergoemacs-cheat
     menu-item "Generate/Open Key binding Cheat Sheet"
     (lambda()
       (interactive)
       (call-interactively 'ergoemacs-display-current-svg)))
    (ergoemacs-menus
     menu-item "Use Menus"
     (lambda() (interactive)
       (setq ergoemacs-use-menus (not ergoemacs-use-menus))
       (if ergoemacs-use-menus
           (progn
             (require 'ergoemacs-menus)
             (ergoemacs-menus-on))
         (when (featurep 'ergoemacs-menus)
           (ergoemacs-menus-off))))
     :button (:radio . ergoemacs-use-menus))
    (ergoemacs-save
     menu-item "Save Settings for Future Sessions"
     (lambda ()
       (interactive)
       (customize-save-variable 'ergoemacs-smart-paste ergoemacs-smart-paste)
       (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
       (customize-save-variable 'ergoemacs-theme ergoemacs-theme)
       (customize-save-variable 'ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
       (customize-save-variable 'ergoemacs-ctl-c-or-ctl-x-delay ergoemacs-ctl-c-or-ctl-x-delay)
       (customize-save-variable 'ergoemacs-handle-ctl-c-or-ctl-x ergoemacs-handle-ctl-c-or-ctl-x)
       (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
       (customize-save-variable 'ergoemacs-theme-options ergoemacs-theme-options)
       (customize-save-customized)))
    (ergoemacs-customize
     menu-item "Customize ErgoEmacs"
     (lambda ()
       (interactive)
       (customize-group 'ergoemacs-mode)))
    (ergoemacs-mode-exit
     menu-item "Exit ergoemacs-mode"
     (lambda() (interactive) (ergoemacs-mode -1)))))

(defvar ergoemacs-get-variable-layout  nil)
(defun ergoemacs-get-variable-layout ()
  "Get the old-style variable layout list for `ergoemacs-extras'."
  (let ((ret '()))
    (mapc
     (lambda(c)
       (let ((variable (gethash (concat (or (and (stringp c) c) (symbol-name c)) ":variable") ergoemacs-theme-component-hash)))
         (mapc
          (lambda(k)
            (let (desc (fun (nth 1 k)))
              (if (not (listp fun))
                  (progn
                    (setq desc (assoc fun ergoemacs-function-short-names))
                    (when desc
                      (setq desc (nth 1 desc))))
                (mapc
                 (lambda(d)
                   (let ((tmp (assoc d ergoemacs-function-short-names)))
                     (when tmp
                       (setq desc (nth 1 tmp)))))
                 fun))
              (push `(,(nth 0 k) ,fun ,desc ,(nth 3 k)) ret)))
          variable)))
     (ergoemacs-theme-components ergoemacs-theme))
    (setq ergoemacs-get-variable-layout ret)
    'ergoemacs-get-variable-layout))

(defvar ergoemacs-get-fixed-layout nil)
(defun ergoemacs-get-fixed-layout ()
  "Get the old-style fixed layout list for `ergoemacs-extras'."
  (let ((ret '()))
    (mapc
     (lambda(c)
       (let ((fixed (gethash (concat (or (and (stringp c) c) (symbol-name c)) ":fixed") ergoemacs-theme-component-hash)))
         (mapc
          (lambda(k)
            (let (desc (fun (nth 1 k)))
              (if (not (listp fun))
                  (progn
                    (setq desc (assoc fun ergoemacs-function-short-names))
                    (when desc
                      (setq desc (nth 1 desc))))
                (mapc
                 (lambda(d)
                   (let ((tmp (assoc d ergoemacs-function-short-names)))
                     (when tmp
                       (setq desc (nth 1 tmp)))))
                 fun))
              (push `(,(nth 0 k) ,fun ,desc) ret)))
          fixed)))
     (ergoemacs-theme-components ergoemacs-theme))
    (setq ergoemacs-get-fixed-layout ret)
    'ergoemacs-get-fixed-layout))

(defun ergoemacs-theme-keymaps (theme &optional version)
  "Gets the keymaps for THEME for VERSION.
Returns list of: read-keymap shortcut-keymap keymap shortcut-list unbind-keymap rm-keys emulation-setup vars.
Uses `ergoemacs-theme-component-keymaps' and `ergoemacs-theme-components'"
  (let* ((ret (ergoemacs-theme-component-keymaps (ergoemacs-theme-components theme) version))
         (menu-keymap (make-sparse-keymap))
         (ergoemacs-ignore-advice t)
         prior keys)
    (push (pop ret) prior)
    (push (pop ret) prior)
    (setq prior (reverse prior))
    (setq keys (pop ret))
    (define-key menu-keymap [menu-bar ergoemacs-mode]
      `("ErgoEmacs" . ,(ergoemacs-keymap-menu theme)))
    (pop keys)
    (push menu-keymap keys)
    (push 'keymap keys)
    (setq ret`(,@prior ,keys ,@ret))
    ret))

(defun ergoemacs-theme-restore-maps (&optional no-message)
  "Restore original keymaps.
When NO-MESSAGE is true, don't tell the user."
  (mapc
   (lambda(x)
     (when (eq 'cons (type-of x))
       (let ((hook (nth 0 x))
             (map-name (nth 1 x))
             orig-map)
         (unless (string-match "-mode$" (symbol-name map-name))
           (unless (eq map-name 't)
             (setq orig-map
                   (gethash (concat (symbol-name map-name) (symbol-name hook) ":original-map") ergoemacs-theme-component-cache))
             (when orig-map
               (unless no-message
                 (message "Restoring %s" map-name))
               (set map-name (copy-keymap orig-map))))))))
   ergoemacs-theme-hook-installed))

(defun ergoemacs-theme-remove (&optional no-message)
  "Remove the currently installed theme and reset to emacs keys.
When NO-MESSAGE is true, don't tell the user."
  (ergoemacs-theme-make-hooks ergoemacs-theme 'remove-hooks)
  (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
  ;;; Restore maps
  (ergoemacs-theme-restore-maps no-message)
  (setq ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal)
        ergoemacs-extract-map-hash (make-hash-table :test 'equal)
        ergoemacs-shortcut-function-binding-hash (make-hash-table :test 'equal)
        ergoemacs-emulation-mode-map-alist '()
        ergoemacs-shortcut-keys nil
        ergoemacs-shortcut-override-mode nil
        ergoemacs-modal nil
        ergoemacs-repeat-keys nil
        ergoemacs-read-input-keys nil
        ergoemacs-theme-hook-installed '())
  (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
    ;; Remove keymap
    (when x
      (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
    (setq x (assq 'ergoemacs-unbind-keys minor-mode-map-alist))
    (when x
      (setq minor-mode-map-alist (delq x minor-mode-map-alist)))))

(defun ergoemacs-rm-key (keymap key)
  "Removes KEY from KEYMAP even if it is an ergoemacs composed keymap.
Also add global overrides from the current global map, if necessary.
Returns new keymap."
  (if keymap
      (let ((new-keymap (copy-keymap keymap)))
        (cond
         ((keymapp (nth 1 new-keymap))
          (pop new-keymap)
          (setq new-keymap
                (mapcar
                 (lambda(map)
                   (let ((lk (lookup-key map key)) lk2 lk3)
                     (cond
                      ((integerp lk)
                       (setq lk2 (lookup-key (current-global-map) key))
                       (setq lk3 (lookup-key map (substring key 0 lk)))
                       (when (and (or (commandp lk2) (keymapp lk2)) (not lk3))
                         (define-key map key lk2)))
                      (lk
                       (define-key map key nil))))
                   map)
                 new-keymap))
          (push 'keymap new-keymap)
          new-keymap)
         (t
          (define-key new-keymap key nil)
          new-keymap)))))

(defvar ergoemacs-theme-hook nil)
(defun ergoemacs-theme-remove-key-list (list &optional no-message dont-install)
  "Removes shortcuts keys in LIST from:
- `ergoemacs-read-input-keymap'
- `ergoemacs-shortcut-keymap'
- `ergoemacs-keymap'
- `ergoemacs-unbind-keymap'

This also:
- Restores all changed keymaps with `ergoemacs-theme-restore-maps'
- Blanks out ergoemacs-mode changes by resetting `ergoemacs-emulation-mode-map-alist'
- Reapplies maps to either `minor-mode-map-alist' or `ergoemacs-emulation-mode-map-alist'
- Set-up persistent remaps defined in `ergoemacs-theme-mode-based-remaps'
- Sets up read-key maps by running `ergoemacs-theme-hook'.

"
  (dolist (key list)
    ;; Read input keymap shouldn't interfere with global map needs.
    (setq ergoemacs-read-input-keymap (ergoemacs-rm-key ergoemacs-read-input-keymap key))
    (let ((vector-key (or (and (vectorp key) key)
                          (read-kbd-macro  (key-description key) t))))
      ;; ergoemacs-shortcut-keymap should always have `ergoemacs-ctl-c'
      ;; and `ergoemacs-ctl-x' for C-c and C-x, don't unbind here.
      (unless (and (memq (elt vector-key 0) '(3 24))
                   (memq (lookup-key ergoemacs-shortcut-keymap (vector (elt vector-key 0)))
                         '(ergoemacs-ctl-x ergoemacs-ctl-c)))
        (setq ergoemacs-shortcut-keymap (ergoemacs-rm-key ergoemacs-shortcut-keymap key))))
    (setq ergoemacs-keymap (ergoemacs-rm-key ergoemacs-keymap key))
    (setq ergoemacs-unbind-keymap (ergoemacs-rm-key ergoemacs-unbind-keymap key)))
  (unless dont-install
    (ergoemacs-theme-remove no-message)
    ;; Reset Shortcut hash.
    (dolist (c ergoemacs-theme-shortcut-reset-list)
      (puthash (nth 0 c) (nth 1 c) ergoemacs-command-shortcuts-hash))
    (setq ergoemacs-emulation-mode-map-alist '())
    ;; Install persistent mode-based remaps.
    (dolist (mode ergoemacs-theme-mode-based-remaps)
      (ergoemacs-theme-hook mode))
    ;; `ergoemacs-keymap' top in `minor-mode-map-alist'
    (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      (push (cons 'ergoemacs-mode ergoemacs-keymap) minor-mode-map-alist))

    ;; `ergoemacs-unbind-keys' at the bottom in `minor-mode-map-alist'
    (let ((x (assq 'ergoemacs-unbind-keys minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      ;; Put at the END of the list.
      (setq minor-mode-map-alist
            (append minor-mode-map-alist
                    (list (cons 'ergoemacs-unbind-keys ergoemacs-unbind-keymap)))))

    ;; `ergoemacs-read-input-keymap', then `ergoemacs-shortcut-keymap'
    ;; in `ergoemacs-emulation-mode-map-alist'
    (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist)
    (push (cons 'ergoemacs-read-input-keys ergoemacs-read-input-keymap) ergoemacs-emulation-mode-map-alist)
    (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
    (ergoemacs-theme-make-hooks ergoemacs-theme)
    (set-default 'ergoemacs-mode t)
    (set-default 'ergoemacs-shortcut-keys t)
    (set-default 'ergoemacs-read-input-keys t)
    (set-default 'ergoemacs-unbind-keys t)
    (setq ergoemacs-mode t
          ergoemacs-shortcut-keys t
          ergoemacs-read-input-keys t
          ergoemacs-unbind-keys t)
    (unwind-protect
        (run-hooks 'ergoemacs-theme-hook))))

(defvar ergoemacs-theme-mode-based-remaps nil)
(defvar ergoemacs-theme-shortcut-reset-list nil)
(defvar ergoemacs-theme-save-variables-actual nil)
(defvar ergoemacs-theme-save-variables-state nil)
(defun ergoemacs-theme-apply-variables (var-list)
  "Apply the themes' modes and variables, defined in VAR-LIST"
  (unless ergoemacs-theme-save-variables-state
    (setq ergoemacs-theme-save-variables-actual var-list)
    (setq ergoemacs-theme-save-variables-actual
          (mapcar
           (lambda(x)
             (let (val val2)
               (if (condition-case err (or (= 1 (nth 1 x)) (= -1 (nth 1 x))) (error nil))
                   (progn
                     (funcall (nth 0 x) (nth 1 x))
                     (setq val (if (= (nth 1 x) 1) -1 1)))
                 (set (nth 0 x) (nth 1 x))
                 (set-default (nth 0 x) (nth 1 x))
                 (if (not (nth 2 x))
                     (setq val (not (nth 1 x)))
                   (setq val (nth 2 x))
                   (setq val2 (nth 1 x))))
               `(,(nth 0 x) ,val ,val2)))
           ergoemacs-theme-save-variables-actual))
    (setq ergoemacs-theme-save-variables-state t)))

(defun ergoemacs-theme-revert-variables ()
  "Revert the theme's variables/modes"
  (when ergoemacs-theme-save-variables-state
    (setq ergoemacs-theme-save-variables-actual
          (mapcar
           (lambda(x)
             (let (val val2)
               (if (condition-case err (or (= 1 (nth 1 x)) (= -1 (nth 1 x))) (error nil))
                   (progn
                     (funcall (nth 0 x) (nth 1 x))
                     (setq val (if (= 1 (nth 1 x)) -1 1)))
                 (set (nth 0 x) (nth 1 x))
                 (if (not (nth 2 x))
                     (setq val (not (nth 1 x)))
                   (setq val (nth 2 x))
                   (setq val2 (nth 1 x)))
                 (set-default (nth 0 x) (nth 1 x)))
               `(,(nth 0 x) ,val ,val2)))
           ergoemacs-theme-save-variables-actual))
    (setq ergoemacs-theme-save-variables-actual  nil
          ergoemacs-theme-save-variables-state   nil)))

(defun ergoemacs-theme-install (theme &optional version)
  "Installs `ergoemacs-theme' THEME into appropriate keymaps."
  (let ((tc (ergoemacs-theme-keymaps theme version)))
    (setq ergoemacs-read-input-keymap (nth 0 tc)
          ergoemacs-shortcut-keymap (nth 1 tc)
          ergoemacs-keymap (nth 2 tc)
          ergoemacs-theme-shortcut-reset-list (nth 3 tc)
          ergoemacs-unbind-keymap (nth 4 tc)
          ergoemacs-theme-mode-based-remaps (nth 6 tc)
          ergoemacs-theme (or (and (stringp theme) theme)
                              (symbol-name theme)))
    (ergoemacs-theme-revert-variables)
    (ergoemacs-theme-apply-variables (nth 7 tc))
    ;; Remove unneeded shortcuts & setup `ergoemacs-mode'
    (ergoemacs-theme-remove-key-list
     (if (nth 5 tc)
         (append (nth 5 tc) ergoemacs-global-override-rm-keys)
       ergoemacs-global-override-rm-keys))))

(defvar ergoemacs-theme-hash (make-hash-table :test 'equal))

(defun ergoemacs-theme-refresh-customize ()
  "Refreshes the customize interface to `ergoemacs-theme'."
  (eval
   (macroexpand
    `(defcustom ergoemacs-theme (if (and (boundp 'ergoemacs-variant) ergoemacs-variant)
                                    ergoemacs-variant
                                  (if (and (boundp 'ergoemacs-theme) ergoemacs-theme)
                                      ergoemacs-theme
                                    (if (getenv "ERGOEMACS_THEME")
                                        (getenv "ERGOEMACS_THEME")
                                      nil)))
       ,(concat "Ergoemacs Themes\n"
                (ergoemacs-get-themes-doc t))
       :type `,(ergoemacs-get-themes-type t)
       :set 'ergoemacs-set-default
       :group 'ergoemacs-mode))))

(defmacro ergoemacs-theme (&rest body-and-plist)
  "Define an ergoemacs-theme.
:components -- list of components that this theme uses. These can't be seen or toggled
:optional-on -- list of components that are optional and are on by default
:optional-off -- list of components that are optional and off by default
:options-menu -- Menu options list
:silent -- If this theme is \"silent\", i.e. doesn't show up in the Themes menu.

The rest of the body is an `ergoemacs-theme-component' named THEME-NAME-theme
"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist"))
        (tmp (make-symbol "tmp")))
    (setq kb (ergoemacs--parse-keys-and-body body-and-plist))
    (setq tmp (eval (plist-get (nth 0 kb) ':components)))
    (push (intern (concat (plist-get (nth 0 kb) ':name) "-theme")) tmp)
    (setq tmp (plist-put (nth 0 kb) ':components tmp))
    (mapc
     (lambda(comp)
       (setq tmp (plist-put (nth 0 kb) comp
                            (eval (plist-get (nth 0 kb) comp)))))
     '(:optional-on :optional-off :options-menu))
    
    `(let (themes silent)
       (setq themes (gethash "defined-themes" ergoemacs-theme-hash)
             silent (gethash "silent-themes" ergoemacs-theme-hash))
       (push ,(plist-get (nth 0 kb) ':name) themes)
       (push ,(plist-get (nth 0 kb) ':name) silent)
       (puthash ,(plist-get (nth 0 kb) ':name) ',tmp ergoemacs-theme-hash)
       (if ,(plist-get (nth 0 kb) ':silent)
           (puthash "silent-themes" silent ergoemacs-theme-hash)
         (puthash "defined-themes" themes ergoemacs-theme-hash))
       (ergoemacs-theme-component ,(intern (concat (plist-get (nth 0 kb) ':name) "-theme")) ()
         ,(format "Generated theme component for %s theme" (concat (plist-get (nth 0 kb) ':name) "-theme"))
         ,@(nth 1 kb)))))

(make-obsolete-variable 'ergoemacs-variant 'ergoemacs-theme
                        "ergoemacs-mode 5.8.0.1")



(defun ergoemacs-get-themes-doc (&optional silent)
  "Gets the list of all known themes and the documentation associated with the themes."
  (mapconcat
   (lambda(theme)
     (concat theme " - " (plist-get (gethash theme ergoemacs-theme-hash) ':description)))
   (sort (ergoemacs-get-themes silent) 'string<) "\n"))

(defun ergoemacs-get-themes (&optional silent)
  "Gets the list of themes.
When SILENT is true, also include silent themes"
  (let (ret)
    ;; All this is done to copy lists so that sorts will not
    ;; destroy the final list.  Please keep this here so that errors
    ;; will not be introduced (seems silly)
    (setq ret
          (mapcar
           (lambda(x)
             x)
           (or (and silent
                    (append (gethash "defined-themes" ergoemacs-theme-hash)
                            (gethash "silent-themes" ergoemacs-theme-hash)))
               (gethash "defined-themes" ergoemacs-theme-hash))))
    ret))

(defun ergoemacs-get-themes-type (&optional silent)
  "Gets the customization types for `ergoemacs-theme'"
  `(choice
    ,@(mapcar
       (lambda(theme)
         `(const :tag ,(concat theme " - "
                               (plist-get (gethash theme ergoemacs-theme-hash) ':description)) :value ,theme))
       (sort (ergoemacs-get-themes silent) 'string<))
    (symbol :tag "Other")))

;;;###autoload
(defun ergoemacs-key (key function &optional desc only-first fixed-key)
  "Defines KEY in ergoemacs keyboard based on QWERTY and binds to FUNCTION.
DESC is ignored, as is FIXED-KEY."
  (let* ((key (or
               (and (vectorp key) key)
               (read-kbd-macro key t)))
         (ergoemacs-force-just-first only-first)
         (ergoemacs-force-variable-reg t))
    (ergoemacs-theme-component--global-set-key key function)))

(defun ergoemacs-fixed-key (key function &optional desc)
  "Defines fixed KEY in ergoemacs  and binds to FUNCTION."
  (let* ((key (or
               (and (vectorp key) key)
               (read-kbd-macro key t)))
         (ergoemacs-force-just-first nil)
         (ergoemacs-force-variable-reg nil))
    (ergoemacs-theme-component--global-set-key key function)))

(defmacro ergoemacs-deftheme (name desc based-on &rest differences)
  "Creates a theme layout for Ergoemacs keybindings -- Compatability layer.

NAME is the theme name.
DESC is the theme description
BASED-ON is the base name theme that the new theme is based on.

DIFFERENCES are the differences from the layout based on the functions.  These are based on the following functions:

`ergoemacs-key' = defines/replaces variable key with function by (ergoemacs-key QWERTY-KEY FUNCTION DESCRIPTION ONLY-FIRST)
`ergoemacs-fixed-key' = defines/replace fixed key with function by (ergoemacs-fixed-key KEY FUNCTION DESCRIPTION)
"
  (declare (indent 1))
  `(let (silent pl tmp)
     (setq pl (gethash (or ,based-on "standard") ergoemacs-theme-hash))
     (plist-put pl ':name ,(symbol-name name))
     (setq tmp (plist-get pl ':components))
     (push (intern (concat ,(symbol-name name) "-theme")) tmp)
     (setq tmp (plist-put pl ':components tmp))
     (setq silent (gethash "silent-themes" ergoemacs-theme-hash))
     (push ,(symbol-name name) silent)
     (puthash "silent-themes" silent ergoemacs-theme-hash)
     (puthash ,(symbol-name name) tmp ergoemacs-theme-hash)
     (ergoemacs-theme-component ,(intern (concat (symbol-name name) "-theme")) ()
       ,(format "Generated theme component for %s theme" (symbol-name name))
       ,@differences)))

(provide 'ergoemacs-theme-engine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-theme-engine.el ends here
