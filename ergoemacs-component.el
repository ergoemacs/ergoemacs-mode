;;; ergoemacs-component.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-component.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;; Commentary:
;;
;; Code for ergoemacs components.
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

(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(require 'help-mode)
(require 'find-func)

(defvar ergoemacs-command-loop--minibuffer-unsupported-p)
(defvar ergoemacs-map-properties--label-atoms-maps)
(defvar ergoemacs--last-start-emacs-state-2)
(defvar ergoemacs--start-emacs-state-2)
(defvar ergoemacs-component-hash)
(defvar ergoemacs-display-key-use-face-p)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-map-properties--known-maps)
(defvar ergoemacs-mode--fast-p)
(defvar ergoemacs-mode-version)
(defvar ergoemacs-saved-global-map)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-translate--translation-hash)
(defvar ergoemacs-translation-hash)

(declare-function ergoemacs--emacs-state "ergoemacs-mode")
(declare-function ergoemacs-timing-- "ergoemacs-mode")
(declare-function ergoemacs-mode--setup-hash-tables--setq "ergoemacs-mode")
(declare-function ergoemacs-mode-clear-cache "ergoemacs-mode")

(declare-function ergoemacs-set "ergoemacs-lib")
(declare-function ergoemacs-reset "ergoemacs-lib")
(declare-function ergoemacs-warn "ergoemacs-lib")

(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")
(declare-function ergoemacs-theme--regexp "ergoemacs-theme-engine")

(declare-function ergoemacs-translate "ergoemacs-translate")
(declare-function ergoemacs-translate--apply-key "ergoemacs-translate")
(declare-function ergoemacs-translate--define-key "ergoemacs-translate")

(declare-function ergoemacs-map-properties--label-known "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-regexp "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--empty-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-layout--regexp "ergoemacs-layouts")

;; ergoemacs-translate

(defcustom ergoemacs-ignore-prev-global t
  "If non-nil, the ergoemacs-mode will ignore previously defined global keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

;; for compatability
;;;###autoload
(defun ergoemacs-ignore-prev-global ()
  "Ignore previously defined global keys."
  (setq ergoemacs-ignore-prev-global t))

(defun ergoemacs-remap (function)
  "Remap the FUNCTION to the appropriate key and then call that function."
  (let ((key (where-is-internal function ergoemacs-keymap t)))
    (call-interactively (key-binding key t  nil (point)))))


;;; Translation between layouts

(defvar ergoemacs-component-struct--define-key-current nil)

(defun ergoemacs-component-struct--parse-list (list function &rest args)
  "Handle :bind LIST and call FUNCTION.

The FUNCTION calls the with the first argument as the string
piece and the second argument the symbol piece of the definition.
It also passes ARGS if any are specified."
  (let (arg1 arg2)
    (cond
     ;; :list ("a" b "c" d)
     ((ignore-errors (and (consp list) (> (length list) 2)
                          (stringp (car list))
                          (setq arg1 (pop list))
                          (setq arg2 (pop list))))
      (while (and arg1 arg2)
        (apply function arg1 arg2 args)
        (setq arg1 (pop list)
              arg2 (pop list))))
     ((and (consp list) (stringp (car list)))
      (apply function (car list)
             (or (and (consp (cdr list)) (nth 1 list))
                 (cdr list))
             args))
     ((and (consp list) (consp (car list)))
      (dolist (elt list)
        (when (and (consp elt) (stringp (car elt)))
          (apply function (car elt)
                 (or (and (consp (cdr elt)) (nth 1 elt))
                     (cdr elt))
                 args)))))))

(defun ergoemacs-component-struct--with-hook (when-condition plist body &optional object)
  "How the (when...) conditions in an ergoemacs-mode theme are handled.
WHEN-CONDITION is the when condition that is defined in a theme.

PLIST is the theme's property

BODY is the (when ...) body.

OBJECT is the ergoemacs component object, and defaults to
`ergoemacs-component-struct--define-key-current'."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--with-hook' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current))
          (hook
           (or (and (symbol-name when-condition) (string-match-p "\\(-hook\\|-mode\\|\\`mark-active\\)\\'" (symbol-name when-condition)) when-condition)
               (and (symbol-name when-condition) (string-match-p "mode-.*" (symbol-name when-condition))
                    (save-match-data
                      (intern-soft
                       (replace-regexp-in-string
                        "-mode-.*" "mode-hook"
                        (symbol-name when-condition)))))
               (and (string-match-p "(key)?map" (symbol-name when-condition))
                    (save-match-data
                      (intern-soft
                       (replace-regexp-in-string
                        "(key)?map.*" "hook"
                        (symbol-name when-condition))))))))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure")
	(puthash hook plist (ergoemacs-component-struct-hook-plists obj))
        (setf (ergoemacs-component-struct-when-condition obj) when-condition)
        (setf (ergoemacs-component-struct-hook obj) hook)
        (funcall body)
        (setf (ergoemacs-component-struct-when-condition obj) nil)
        (setf (ergoemacs-component-struct-hook obj) nil))))))

(defun ergoemacs-component-struct--component-description (component)
  "Gets the description of a COMPONENT.
Allows the component not to be calculated."
  (let* ((comp-name (or (and (symbolp component) (symbol-name component))
                        component))
         (comp (ergoemacs-gethash comp-name ergoemacs-component-hash)))
    (cond
     ((functionp comp)
      (replace-regexp-in-string "[\n ]*(fn)[ \n]*\\'" "" (documentation comp t)))
     ((ergoemacs-component-struct-p comp)
      (plist-get (ergoemacs-component-struct-plist comp) :description))
     (t ""))))

(defun ergoemacs-component-struct--new-version (version &optional object)
  "Add VERSION to component OBJECT."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--new-version' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current))
          new-obj tmp)
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure")
        (puthash (concat (ergoemacs-component-struct-name obj)
                         (and (ergoemacs-component-struct-version obj)
                              (concat "::" (ergoemacs-component-struct-version obj))))
                 ergoemacs-component-struct--define-key-current ergoemacs-component-hash)
        ;; Get the base object without version changes
        (setq new-obj (ergoemacs-gethash (ergoemacs-component-struct-name obj) ergoemacs-component-hash))
        ;; Update all versions to include the new version information.
        (dolist (old-version (ergoemacs-component-struct-versions new-obj))
          (setq tmp (ergoemacs-gethash (concat (ergoemacs-component-struct-name new-obj) "::" old-version) ergoemacs-component-hash))
          (when (ergoemacs-component-struct-p tmp)
            (push version (ergoemacs-component-struct-versions tmp))))
        (push version (ergoemacs-component-struct-versions new-obj))
        ;; Use the last object as the base of the new object
        (setq ergoemacs-component-struct--define-key-current (copy-tree obj t))
        (setf (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current) version))))))

(defvar ergoemacs-component-struct--define-key-temp-map nil)

(defun ergoemacs-component-struct--define-key-get-def (def)
  "Gets the `ergoemacs-mode' function definition for DEF."
  (let (tmp)
    (cond
     ((and (consp def) (memq (nth 0 def) '(kbd read-kbd-macro))
           (stringp (nth 1 def)))
      (read-kbd-macro (nth 1 def)))
     ((and (consp def) (= 1 (length def)) (symbolp (nth 0 def)))
      (nth 0 def))
     ((and (consp def) (= 1 (length def)) (consp (nth 0 def))
           (= 2 (length (nth 0 def)))
           (eq (nth 0 (nth 0 def)) 'quote)
           (symbolp (nth 1 (nth 0 def))))
      (nth 1 (nth 0 def)))
     ((and (consp def) (= 2 (length def)) (eq (nth 1 def) 'quote) (symbolp (nth 1 def)))
      (nth 1 def))
     ((and (consp def)
           (= 2 (length def))
           (stringp (nth 0 def))
           (eq (nth 1 def) :emacs)
           (setq tmp (lookup-key global-map (read-kbd-macro (nth 0 def))))
           (commandp tmp))
      tmp)
     ((ergoemacs-keymapp (ergoemacs-sv def))
      (ergoemacs-sv def))
     (t def))))

(defun ergoemacs-component-struct--ini-map (obj)
  "Initilize keymap in OBJ.

OBJ is an `egoemacs-component-struct' object.

Returns the map, if it hasn't been initialized, initialize
with the label, and then return."
  (or (ergoemacs-component-struct-map obj)
      (let ((map (make-sparse-keymap)))
        (ergoemacs map :label
                   (list (ergoemacs (ergoemacs :global-map) :key-hash)
                         (intern (format "%s%s" (ergoemacs-component-struct-name obj) (or (ergoemacs-component-struct-version obj) "")))
                         (intern (ergoemacs-component-struct-layout obj))))
        (setf (ergoemacs-component-struct-map obj) map)
        map)))

(defun ergoemacs-component-struct--clear-cache (struct-map)
  "Clears STRUCT-MAP's cache of keymaps.

STRUCT-MAP can be a list of `ergoemacs-component-struct' structures as well."
  (cond
   ((ergoemacs-component-struct-p struct-map)
    (setf (ergoemacs-component-struct-calculated-layouts struct-map) (make-hash-table :test 'equal)))
   ((consp struct-map)
    (dolist (cur-map struct-map)
      (ergoemacs-component-struct--clear-cache cur-map)))))

(defun ergoemacs-component-struct--closest-version (version version-list)
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
          (dolist (v version-list)
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
          (if (version-list-< biggest-version-list use-version)
              (setq ret "")
            (if best-version
                (setq ret (concat "::" best-version))
              (setq ret (concat "::" smallest-version))))
          ret)
      "")))

(defun ergoemacs-component-struct--lookup-closest (comp &optional current-version)
  "Look up closest component version from `ergoemacs-component-hash'.

COMP is the component where the version information should be stored.

Optionally assume that CURRENT-VERSION is active"
  (if (not (ergoemacs-component-struct-p comp)) nil
    (let (versions)
      (cond
       ((not (setq versions (ergoemacs-component-struct-versions comp)))
        comp)
       ((string= "" (setq versions (ergoemacs-component-struct--closest-version
                                    current-version versions)))
        comp)
       (t
        (ergoemacs-component-struct--lookup-hash (concat (ergoemacs-component-struct-name comp) versions)))))))

(defun ergoemacs-component-struct--lookup-hash (map-or-map-list &optional version)
  "Lookup `ergoemacs-component-hash' from MAP-OR-MAP-LIST if necessary.

VERSION is the version of the `ergoemacs-mode' keys that you wish
to lookup.

This takes into consideration any versions defined, and the
closest `ergoemacs-theme-version' calculated from
`ergoemacs-component-struct--closest-version' by using
`ergoemacs-component-struct--lookup-closest'"
  (if (consp map-or-map-list)
      (mapcar #'ergoemacs-component-struct--lookup-hash map-or-map-list)
    (if (ergoemacs-component-struct-p map-or-map-list)
        (ergoemacs-component-struct--lookup-closest map-or-map-list version)
      (let ((map map-or-map-list)
            ret)
        (when (symbolp map) ;; If map is a symbol, change to string.
          (setq map (symbol-name map)))
        (when (stringp map) ;; If map is a string, get the component from `ergoemacs-component-hash'
          (setq ret (ergoemacs-gethash map ergoemacs-component-hash))
          (when (and ret (functionp ret))
            (funcall ret)
            (setq ret (ergoemacs-gethash map ergoemacs-component-hash))))
        (if (and map (string-match-p "::" map)) ret
	  (ergoemacs-component-struct--lookup-closest ret version))))))

(defvar ergoemacs-component-struct--get-keymap nil)
(defvar ergoemacs-component-struct--get-keymap-extra nil)
(defun ergoemacs-component-struct--lookup-list (lookup-keymap &optional layout obj map-list)
  "Get list of extra maps based on LOOKUP-KEYMAP.

The LAYOUT argument specifies the ergoemacs layout to use.
Otherwise, the layout used is `ergoemacs-keyboard-layout'.

The OBJ list is the list of ergoemacs theme components to use.
If it is nil, it is the components specifed by
`ergoemacs-theme-components'.

The MAP-LIST is the list symbols that LOOKUP-KEYMAP is bound to.
If unspecified, use `ergoemacs-map-properties--map-list' to try
to figure out what variables LOOKUP-KEYMAP is bound to."
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (reverse (ergoemacs-theme-components)))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        (map-list (or map-list (ergoemacs lookup-keymap :map-list)))
        ;; (ergoemacs-component-struct--lookup-list org-mode-map)
        extra-hash
        ret extra-map)
    (if (consp obj)
        (dolist (cobj obj)
          (setq extra-hash (ergoemacs-component-struct-maps cobj))
          (dolist (map-name map-list)
            (setq extra-map (ergoemacs-gethash map-name extra-hash))
            (when extra-map
              (push (ergoemacs-component-struct--get cobj cur-layout map-name extra-map) ret)))))
    ret))

(defun ergoemacs-component-struct--get (map cur-layout &optional lookup-key translate-map)
  "Get component MAP and return keymap updating MAP cache.

CUR-LAYOUT is the current keymboard layout used.

This keymap is cached using LOOKUP-KEY.

The keymap to translate is TRANSLATE-MAP, otherwise it is the
`ergoemacs-component-struct-map' for MAP."
  (let* (ret
         (cmap (or translate-map (ergoemacs-component-struct-map map)))
         (just-first-keys (ergoemacs-component-struct-just-first-keys map))
         (variable-modifiers (ergoemacs-component-struct-variable-modifiers map))
         (variable-prefixes (ergoemacs-component-struct-variable-prefixes map))
         (layout-from (ergoemacs-component-struct-layout map))
         (hash (ergoemacs-component-struct-calculated-layouts map)))
    (cond
     ((string= layout-from cur-layout)
      (setq ret (copy-keymap cmap))
      ret)
     ((setq ret (ergoemacs-gethash (list lookup-key (intern cur-layout)) hash))
      ret)
     (t
      (setq ergoemacs-component-struct--get-keymap (make-sparse-keymap))
      (ergoemacs-timing translate-keymap
        (ergoemacs-timing (intern (format "translate-keymap-%s" (ergoemacs-component-struct-name map)))
          (ergoemacs-map-keymap
           (lambda (key item)
             (if (consp key)
                 (ergoemacs-warn "Keymap range currently not supported %s,%s" key item)
               (unless (eq item 'ergoemacs-prefix)
                 (ergoemacs :define-key
                            ergoemacs-component-struct--get-keymap
                            (ergoemacs-translate
                             key just-first-keys variable-modifiers
                             variable-prefixes cur-layout layout-from) item))))
           cmap)))
      (setq ret (copy-keymap ergoemacs-component-struct--get-keymap))
      (ergoemacs ret :label (list (or lookup-key (ergoemacs (ergoemacs :global-map) :key-hash)) (intern (format "%s%s" (ergoemacs-component-struct-name map) (or (ergoemacs-component-struct-version map) ""))) (intern cur-layout)))
      (puthash (list lookup-key (intern cur-layout)) ret hash)
      (setq ergoemacs-component-struct--get-keymap nil)
      ret))))

(defun ergoemacs-component-struct--minor-mode-map-alist-hash (&optional obj layout)
  "Get `minor-mode-map-alist' additions in hash-table form.

OBJ is the ergoemacs theme components.  Defaults to the value
returned from the function `ergoemacs-theme-components'.

LAYOUT is the current keyboard layout.  Defaults to
`ergoemacs-keyboard-layout'"
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        (hash (make-hash-table)))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (maphash
         (lambda(key value)
           (puthash key (append (ergoemacs-gethash key hash) value) hash))
         (ergoemacs-component-struct--minor-mode-map-alist-hash cur-obj)))
      hash)
     (t
      (maphash
       (lambda(key value)
         ;; Put the translated keymap in a list in the hash.
         (puthash key (list (ergoemacs-component-struct--get obj cur-layout (list 'cond-map key) value)) hash))
       (ergoemacs-component-struct-cond-maps obj))
      hash))))


(defvar ergoemacs-component-struct--unbound-maps nil)

(defun ergoemacs-component-struct--minor-mode-map-alist (&optional obj)
  "Get the ending maps for `minor-mode-map-alist' using the ergoemacs structures OBJ."
  (let (ret map parent)
    (maphash
     (lambda(key value)
       (setq parent (make-composed-keymap value)
             map (make-sparse-keymap))
       (ergoemacs map :label (list 'cond-map key (intern ergoemacs-keyboard-layout)))
       (set-keymap-parent map parent)
       (if (boundp key)
           (push (cons key map) ret)
         (push (cons key map) ergoemacs-component-struct--unbound-maps)))
     (ergoemacs-component-struct--minor-mode-map-alist-hash obj))
    ret))

(defun ergoemacs-component-struct--hooks (&optional obj ret)
  "Gets a list of hooks that need to be defined eor OBJ.

You can prespecify RET so that new hooks are pushed to the list."
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        tmp
        (ret ret))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (setq ret (ergoemacs-component-struct--hooks cur-obj ret)))
      ret)
     (t
      (when (and (setq tmp (ergoemacs-component-struct-hook-maps obj))
                 (hash-table-p tmp))
        (maphash
         (lambda(hook _value)
           (cl-pushnew hook ret))
         tmp))
      ret))))

(defun ergoemacs-component-struct--hook-hash (hook &optional layout obj)
  "Get HOOK hash.

LAYOUT is the keyboard layout, defaulting to `ergoemacs-keyboard-layout'.

OBJ is the theme components, defaulting to `ergoemacs-theme-components'."
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        tmp
        (hash (make-hash-table)))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (maphash
         (lambda(key value)
           (puthash key (append (ergoemacs-gethash key hash) value) hash))
         (ergoemacs-component-struct--hook-hash hook layout cur-obj)))
      hash)
     (t
      (when (and (setq tmp (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj)))
                 (hash-table-p tmp))
        (maphash
         (lambda(key value)
           ;; Put the translated keymap in a list in the hash.
           (puthash key (list (ergoemacs-component-struct--get obj cur-layout (list 'hook-maps hook key) value)) hash))
         tmp))
      hash))))

(defun ergoemacs-component-struct--hook (hook &optional layout obj)
  "Get keymaps applied in an alist similiar to `minor-mode-map-alist'.

The `car' of the alist should be the keymap that should be
modified, the `cdr' of the alsit should be the keymap that should
be composed over the keymap.  This is done in
`ergoemacs-component-struct--composed--composed-hook'.

HOOK is the hook that is being run.  In the
`ergoemacs-theme-component', these are defined as:

\(when icicle-minibuffer-setup-hook
  ...)

LAYOUT is the current keyboard layout, or the layout of the
current keyboard theme.

OBJ is the curent ergoemacs-mode object being modified."
  (let* (ret tmp label parent)
    (maphash
     (lambda(key value)
       (setq tmp (when (ergoemacs-keymapp (ergoemacs-sv key))
                   (ergoemacs-sv key))
             label (list 'hook-maps key (or layout ergoemacs-keyboard-layout) (if tmp t nil))
             parent (make-composed-keymap value tmp)
             tmp (make-sparse-keymap))
       (ergoemacs tmp :label label)
       (set-keymap-parent tmp parent)
       (push (cons key tmp) ret))
     (ergoemacs-component-struct--hook-hash hook layout obj))
    ret))

(defvar ergoemacs-component-struct--composed-hook-minibuffer nil
  "`ergoemacs-mode' hooks deferred until after `ergoemacs-mode' modifies the current minibuffer map.")

(defun ergoemacs-component-struct--composed-hook (hook &optional layout obj)
  "Apply keymaps defined in HOOK.

LAYOUT is the current keyboard layout.

OBJ is the current object being modified, passed to
`ergoemacs-component-struct--hook'."
  (dolist (elt (ergoemacs-component-struct--hook hook layout obj))
    (if (minibufferp)
	(progn
	  (unless ergoemacs-command-loop--minibuffer-unsupported-p
	    (catch 'unsupported-p
	      (dolist (elt (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
		(let ((plist (gethash hook (ergoemacs-component-struct-hook-plists elt))))
		  (when (and plist (plist-get plist :command-loop-unsupported-p))
		    (ergoemacs-save-buffer-state
		     (set (make-local-variable 'ergoemacs-command-loop--minibuffer-unsupported-p) t))
		    (throw 'unsupported-p t))))))
	  (if ergoemacs-component-struct--composed-hook-minibuffer
	      (push elt ergoemacs-component-struct--composed-hook-minibuffer)
	    (ergoemacs-save-buffer-state
	     (set (make-local-variable 'ergoemacs-component-struct--composed-hook-minibuffer)
		  (list elt)))))
      (ergoemacs-save-buffer-state
       (set (make-local-variable (car elt)) (make-composed-keymap (cdr elt) (symbol-value (car elt))))))))

(defvar ergoemacs-component-struct--refresh-variables nil
  "To reset a current theme, the variables are refreshed when this is non-nil.")

;;; Change variable values.
(defun ergoemacs-component-struct--set (symbol newval &optional hook object)
  "Set variables up for components.

SYMBOL is the symbol being set.

NEWVAL is the new value that will be used.

HOOK tells if this was called in the (with ..-hook ...) syntax.

OBJECT is the object being modified, defaulting to
`ergoemacs-component-struct--define-key-current'."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--set' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure")
        (push (list symbol newval hook) (ergoemacs-component-struct-variables obj)))))))

(defun ergoemacs-component-struct--deferred (what &optional object)
  "Setup deferred initilizations.

WHAT is the defered initilization list.

OBJECT is the `ergoemacs-component-struct' object being changed."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--deferred' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure")
        (push (list what nil nil) (ergoemacs-component-struct-variables obj)))))))

(defvar ergoemacs-component-struct--refresh-variables nil)

(defun ergoemacs-component-at-point (&optional theme-instead)
  "Get the `ergoemacs-component' defined at or before point.

When THEME-INSTEAD is non-nil, return the theme defined at that
point instead.

Return 0 if there is no such symbol.  Based on
`variable-at-point'."
  (let ((hash-table (or (and theme-instead ergoemacs-theme-hash)
                        ergoemacs-component-hash)))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (or (condition-case ()
              (save-excursion
                (skip-chars-forward "'")
                (or (not (zerop (skip-syntax-backward "_w")))
                    (eq (char-syntax (following-char)) ?w)
                    (eq (char-syntax (following-char)) ?_)
                    (forward-sexp -1))
                (skip-chars-forward "'")
                (let ((obj (read (current-buffer))))
                  (and (symbolp obj)
                       (ergoemacs-gethash (symbol-name obj) hash-table) obj)))
            (error nil))
          (let* ((str (find-tag-default))
                 (sym (if str (intern str))))
            (if (and sym (ergoemacs-gethash (symbol-name sym) hash-table))
                sym
              (save-match-data
                (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                  (setq sym (intern (match-string 1 str)))
                  (and (ergoemacs-gethash (symbol-name sym) hash-table) sym)))))
          0))))

(provide 'ergoemacs-component)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-component.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
