;;; ergoemacs-component.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-component.el
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

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-translate--translation-hash)
(defvar ergoemacs-map-properties--unlabeled)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-translation-hash)

(declare-function ergoemacs-set "ergoemacs-lib")
(declare-function ergoemacs-reset "ergoemacs-lib")

(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")

(declare-function ergoemacs-translate "ergoemacs-translate")

(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")

(declare-function ergoemacs-debug "ergoemacs-debug")
(declare-function ergoemacs-debug-heading "ergoemacs-debug")
(declare-function ergoemacs-debug-keymap "ergoemacs-debug")

(declare-function ergoemacs-theme--get-version "ergoemacs-theme-engine")

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

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

(defstruct ergoemacs-component-struct
  "A basic ergoemacs component map structure."
  (name "default-name")
  (plist '())
  (map nil)
  (maps (make-hash-table))
  (cond-maps (make-hash-table))
  (hook-maps (make-hash-table))
  (when-condition nil)
  (hook nil)
  (dynamic-keys '())
  (version nil)
  (versions '())
  (undefined '())
  (unbind '())
  (variables nil)
  (just-first-keys nil :read-only t)
  (variable-modifiers '(meta) :read-only t)
  (variable-prefixes '([apps] [menu] [27]) :read-only t)
  (layout "us" :read-only t)
  (calculated-layouts (make-hash-table :test 'equal))
  (relative-to 'global-map))

(defvar ergoemacs-component-hash (make-hash-table :test 'equal)
  "Hash of ergoemacs-components")

(defvar ergoemacs-component-struct--define-key-current nil)

(defun ergoemacs-component-struct--create-component (plist body)
  "PLIST is the component properties
BODY is the body of function."
  (unwind-protect
      (progn
        (setq ergoemacs-component-struct--define-key-current
              (make-ergoemacs-component-struct
               :name (plist-get plist :name)
               :plist plist
               :just-first-keys (or (plist-get plist :just-first-keys) nil)
               :variable-modifiers (or (plist-get plist :variable-modifiers) '(meta))
               :variable-prefixes (or (plist-get plist :variable-prefixes) '([apps] [menu] [27]))
               :layout (or (plist-get plist :layout) "us")))
        (funcall body))
    (puthash (concat (ergoemacs-component-struct-name ergoemacs-component-struct--define-key-current)
                     (and (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current)
                          (concat "::" (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current))))
             ergoemacs-component-struct--define-key-current ergoemacs-component-hash)
    (setq ergoemacs-component-struct--define-key-current nil)))

(defun ergoemacs-component-struct--with-hook (when-condition _plist body &optional object)
  "How the (when...) conditions in an ergoemacs-mode theme are handled."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--with-hook' is confused."))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current))
          (hook
           (or (and (string-match-p "\\(-hook\\|-mode\\|\\`mark-active\\)\\'" (symbol-name when-condition)) when-condition)
               (and (string-match-p "mode-.*" (symbol-name when-condition))
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
          (error "OBJECT is not an ergoemacs-component-structure.")
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
         (comp (gethash comp-name ergoemacs-component-hash)))
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
          (error "OBJECT is not an ergoemacs-component-structure.")
        (puthash (concat (ergoemacs-component-struct-name obj)
                         (and (ergoemacs-component-struct-version obj)
                              (concat "::" (ergoemacs-component-struct-version obj))))
                 ergoemacs-component-struct--define-key-current ergoemacs-component-hash)
        ;; Get the base object without version changes
        (setq new-obj (gethash (ergoemacs-component-struct-name obj) ergoemacs-component-hash))
        ;; Update all versions to include the new version information.
        (dolist (old-version (ergoemacs-component-struct-versions new-obj))
          (setq tmp (gethash (concat (ergoemacs-component-struct-name new-obj) "::" old-version) ergoemacs-component-hash))
          (when (ergoemacs-component-struct-p tmp)
            (push version (ergoemacs-component-struct-versions tmp))))
        (push version (ergoemacs-component-struct-versions new-obj))
        ;; Use the last object as the base of the new object
        (setq ergoemacs-component-struct--define-key-current (copy-ergoemacs-component-struct obj))
        (setf (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current) version))))))

(defvar ergoemacs-component-struct--define-key-temp-map nil)

(defun ergoemacs-component-struct--define-key-get-def (def)
  "Gets the `ergoemacs-mode' function definition for DEF."
  (cond
   ((and (consp def)
         (= 2 (length def))
         (stringp (nth 0 def))
         (or (not (nth 1 def))
             (gethash (nth 1 def) ergoemacs-translation-hash)))
    `(lambda(&optional arg)
       (interactive "P")
       (ergoemacs-command-loop ,(nth 0 def) ',(nth 1 def))))
   ((ergoemacs-keymapp (ergoemacs-sv def))
    (ergoemacs-sv def))
   (t def)))

(defun ergoemacs-component-struct--refresh-keys (&optional obj)
  "Refreshes the keys in OBJ based on any new interactive functions found."
  (let ((obj (or obj (ergoemacs-theme-components))))
    (if (consp obj)
        (dolist (cur-obj (ergoemacs-component-struct--lookup-hash obj))
          (ergoemacs-component-struct--refresh-keys cur-obj))
      (let* ((obj (ergoemacs-component-struct--lookup-hash obj))
             (cur-dynamic (ergoemacs-component-struct-dynamic-keys obj))
             new-dynamic keymap key global-map-p cur-map
             fn-lst new-fn-lst new-fn cur-layout)
        (dolist (cur-lst cur-dynamic)
          (setq keymap (nth 0 cur-lst)
                key (nth 1 cur-lst)
                fn-lst (nth 2 cur-lst)
                global-map-p (eq keymap 'global-map)
                cur-map (or (and global-map-p (ergoemacs-component-struct-map obj))
                            (gethash keymap (ergoemacs-component-struct-maps obj)))
                new-fn-lst '())
          (if (catch 'found-fn
                (dolist (fn fn-lst)
                  (if (not (commandp fn t))
                      (push new-fn-lst fn)
                    (setq new-fn fn)
                    (throw 'found-fn nil)))
                t) (push cur-lst new-dynamic)
            (when new-fn-lst ;; For later checks
              (push (list keymap key (reverse new-fn-lst)) new-dynamic))
            (define-key cur-map key new-fn)
            ;; Now fix cached layouts
            (maphash
             (lambda(key value)
               (setq cur-layout (nth 1 key))
               (when (or (and global-map-p (not (nth 0 key)))
                         (eq (nth 0 key) keymap))
                 ;; Update keymap (in place).
                 (define-key value
                   (ergoemacs-translate
                    key (ergoemacs-component-struct-just-first-keys obj)
                    (ergoemacs-component-struct-variable-modifiers obj)
                    (ergoemacs-component-struct-variable-prefixes obj) cur-layout
                    (ergoemacs-component-struct-layout obj)) new-fn)))
             (ergoemacs-component-struct-calculated-layouts obj))))
        ;; Update dynamic/deferred keys
        (fset (ergoemacs-component-struct-dynamic-keys obj) new-dynamic)))))

(defun ergoemacs-component-struct--ini-map (obj)
  "Returns the map, if it hasn't been initialized, initialize with the label, and then return."
  (or (ergoemacs-component-struct-map obj)
      (let ((map (make-sparse-keymap)))
        (ergoemacs map :label
                   (list (ergoemacs (ergoemacs :global-map) :key-hash)
                         (intern (format "%s%s" (ergoemacs-component-struct-name obj) (or (ergoemacs-component-struct-version obj) "")))
                         (intern (ergoemacs-component-struct-layout obj))))
        (setf (ergoemacs-component-struct-map obj) map)
        map)))

(defun ergoemacs-component-struct--define-key (keymap key def &optional object)
  "Defines KEY to be DEF in KEYMAP for OBJECT.
If not specified, OBJECT is `ergoemacs-component-struct--define-key-current'."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--define-key' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current))
          (def (ergoemacs-component-struct--define-key-get-def def)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT not a ergoemacs-component-structure.")
        (let* ((global-map-p (or (eq keymap 'global-map) (eq keymap 'ergoemacs-mode-map)
                                 (eq keymap 'ergoemacs-keymap)))
               (when-condition (ergoemacs-component-struct-when-condition obj))
               (hook (ergoemacs-component-struct-hook obj))
               (cur-map (or (and global-map-p (not when-condition)
                                 (ergoemacs-component-struct--ini-map obj))
                            (and (not when-condition) (gethash keymap (ergoemacs-component-struct-maps obj)))
                            (and global-map-p when-condition (gethash when-condition (ergoemacs-component-struct-cond-maps obj)))
                            (and when-condition hook (ignore-errors (gethash keymap (gethash hook (ergoemacs-component-struct-hook-maps obj)))))))
               fn-lst
               (key (or (and (vectorp key) key)
                        (and (stringp key) (vconcat key))))
               tmp)
          (cond
           ((and (not cur-map) (not when-condition))
            (pushnew keymap ergoemacs-map-properties--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-component-struct-maps obj)))
           ((and (not cur-map) when-condition global-map-p)
            (setq cur-map (make-sparse-keymap))
            (puthash when-condition cur-map (ergoemacs-component-struct-cond-maps obj)))
           ((and (not cur-map) when-condition hook)
            (unless (gethash hook (ergoemacs-component-struct-hook-maps obj))
              (puthash hook (make-hash-table) (ergoemacs-component-struct-hook-maps obj)))
            (pushnew keymap ergoemacs-map-properties--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (gethash hook (ergoemacs-component-struct-hook-maps obj)))))
          (cond
           ((and global-map-p (not when-condition) (not def) (setq tmp (lookup-key (ergoemacs-component-struct-map obj) key))
                 (not (integerp tmp)))
            ;; Remove the key from the keymap, do not set it to
            ;; nil; Its as if it was never defined
            (setq ergoemacs-component-struct--define-key-temp-map (make-sparse-keymap))
            (ergoemacs-map-keymap
             (lambda (cur-key item)
               (if (consp cur-key)
                   (warn "Keymap range currently not supported %s %s" cur-key item)
                 (unless (eq item 'ergoemacs-prefix)
                   (unless (equal key cur-key)
                     (define-key ergoemacs-component-struct--define-key-temp-map cur-key item)))))
             cur-map)
            (setf (ergoemacs-component-struct-map obj)
                  (copy-keymap ergoemacs-component-struct--define-key-temp-map))
            (setq ergoemacs-component-struct--define-key-temp-map nil))
           ((and global-map-p (not (eq keymap 'global-map)) (not when-condition) (not def));; Add to unbind keys
            (unless (member key (ergoemacs-component-struct-unbind obj))
              (push key (ergoemacs-component-struct-unbind obj))))
           ((and global-map-p (not when-condition) (not def)) ;; Add to undefined keys
            (unless (member key (ergoemacs-component-struct-undefined obj))
              (push key (ergoemacs-component-struct-undefined obj))))
           ((and (not when-condition) (lookup-key cur-map key) (not def))
            ;; Remove the key from the keymap.  Do not set it to nil.
            ;; Its as if it was never defined.
            (setq ergoemacs-component-struct--define-key-temp-map (make-sparse-keymap))
            (ergoemacs-map-keymap
             (lambda (cur-key item)
               (if (consp cur-key)
                   (message "Key range not supported %s, %s" cur-key item)
                 (unless (eq item 'ergoemacs-prefix)
                   (unless (equal key cur-key)
                     (define-key ergoemacs-component-struct--define-key-temp-map cur-key item)))))
             cur-map)
            (puthash keymap (copy-keymap ergoemacs-component-struct--define-key-temp-map) (ergoemacs-component-struct-maps obj))
            (setq ergoemacs-component-struct--define-key-temp-map nil))
           ((and (consp def) (stringp (nth 0 def)) (symbolp (nth 1 def)) (eq (nth 1 def) 'keymap))
            (define-key cur-map key def))
           ((and (consp def) (symbolp (nth 1 def))) ;; (fn1 fn2 fn3 fn4)
            (unless (catch 'found-fn
                      (dolist (cur-def def)
                        (if (not (commandp cur-def t))
                            (push cur-def fn-lst)
                          (define-key cur-map key cur-def)
                          (throw 'found-fn t)))
                      nil)
              ;; Not found
              (define-key cur-map key `(lambda() (interactive) (error ,(format "This key is undefined without one of the following functions: %s" fn-lst)))))
            (when fn-lst ;; Test for later
              (push (list keymap key fn-lst)
                    (ergoemacs-component-struct-dynamic-keys obj))))
           (t
            (define-key cur-map key def)))))))))

(defvar ergoemacs-component-struct--hash (make-hash-table)
  "Hash table of `ergoemacs-mode' component structures.")

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

(defun ergoemacs-component-struct--lookup-closest (comp)
  "Looks up closest component version from `ergoemacs-component-hash'"
  (if (not (ergoemacs-component-struct-p comp)) nil
    (let (versions)
      (cond
       ((not (setq versions (ergoemacs-component-struct-versions comp)))
        comp)
       ((string= "" (setq versions (ergoemacs-component-struct--closest-version (ergoemacs :current-version)  versions)))
        comp)
       (t
        (ergoemacs-component-struct--lookup-hash (concat (ergoemacs-component-struct-name comp) versions)))))))

(defun ergoemacs-component-struct--lookup-hash (map-or-map-list)
  "Lookup `ergoemacs-component-hash' from MAP-OR-MAP-LIST if necessary.

This takes into consideration any versions defined, and the
closest `ergoemacs-theme-version' calculated from
`ergoemacs-component-struct--closest-version' by using `ergoemacs-component-struct--lookup-closest'"
  (if (consp map-or-map-list)
      (mapcar #'ergoemacs-component-struct--lookup-hash map-or-map-list)
    (if (ergoemacs-component-struct-p map-or-map-list)
        (ergoemacs-component-struct--lookup-closest map-or-map-list)
      (let ((map map-or-map-list)
            ret)
        (when (symbolp map) ;; If map is a symbol, change to string.
          (setq map (symbol-name map)))
        (when (stringp map) ;; If map is a string, get the component from `ergoemacs-component-hash'
          (setq ret (gethash map ergoemacs-component-hash))
          (when (and ret (functionp ret))
            (funcall ret)
            (setq ret (gethash map ergoemacs-component-hash))))
        (ergoemacs-component-struct--lookup-closest ret)))))

(defvar ergoemacs-component-struct--get-keymap nil)
(defvar ergoemacs-component-struct--get-keymap-extra nil)
(defun ergoemacs-component-struct--lookup-list (lookup-keymap &optional layout obj map-list)
  "Get list of extra maps based on LOOKUP-KEYMAP"
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
            (setq extra-map (gethash map-name extra-hash))
            (when extra-map
              (push (ergoemacs-component-struct--get cobj cur-layout map-name extra-map) ret)))))
    ret))

(defun ergoemacs-component-struct--get (map cur-layout &optional lookup-key translate-map)
  "Get component MAP and return KEYMAP updating MAP cache.
Cache using LOOKUP-KEY. "
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
     ((setq ret (gethash (list lookup-key (intern cur-layout)) hash))
      ret)
     (t
      (setq ergoemacs-component-struct--get-keymap (make-sparse-keymap))
      (ergoemacs-map-keymap
       (lambda (key item)
         (if (consp key)
             (warn "Keymap range currently not supported %s,%s" key item)
           (unless (eq item 'ergoemacs-prefix)
             (define-key
              ergoemacs-component-struct--get-keymap
              (ergoemacs-translate
               key just-first-keys variable-modifiers
               variable-prefixes cur-layout layout-from) item))))
       cmap)
      (setq ret (copy-keymap ergoemacs-component-struct--get-keymap))
      (ergoemacs ret :label (list (or lookup-key (ergoemacs (ergoemacs :global-map) :key-hash)) (intern (format "%s%s" (ergoemacs-component-struct-name map) (or (ergoemacs-component-struct-version map) ""))) (intern cur-layout)))
      (puthash (list lookup-key (intern cur-layout)) ret hash)
      (setq ergoemacs-component-struct--get-keymap nil)
      ret))))

(defun ergoemacs-component-struct--minor-mode-map-alist-hash (&optional obj layout)
  "Get `minor-mode-map-alist' additions in hash-table form."
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        (hash (make-hash-table)))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (maphash
         (lambda(key value)
           (puthash key (append (gethash key hash) value) hash))
         (ergoemacs-component-struct--minor-mode-map-alist-hash cur-obj)))
      hash)
     (t
      (maphash
       (lambda(key value)
         ;; Put the translated keymap in a list in the hash.
         (puthash key (list (ergoemacs-component-struct--get obj cur-layout (list 'cond-map key) value)) hash))
       (ergoemacs-component-struct-cond-maps obj))
      hash))))

(defun ergoemacs-component-struct--minor-mode-map-alist (&optional obj)
  "Get the ending maps for `minor-mode-map-alist' using the ergoemacs structures."
  (let (ret map)
    (maphash
     (lambda(key value)
       (setq map (ergoemacs-map-keymap nil (make-composed-keymap value)))
       (ergoemacs map :label (list 'cond-map key (intern ergoemacs-keyboard-layout)))
       (push (cons key map) ret))
     (ergoemacs-component-struct--minor-mode-map-alist-hash obj))
    ret))

(defun ergoemacs-component-struct--hooks (&optional obj ret)
  "Gets a list of hooks that need to be defined eor OBJ."
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        tmp
        (ret ret))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (setq ret (ergoemacs-component-struct--hooks cur-obj ret)))
      ret)
     (t
      (when (hash-table-p (setq tmp (ergoemacs-component-struct-hook-maps obj)))
        (maphash
         (lambda(hook _value)
           (pushnew hook ret))
         tmp))
      ret))))

(defun ergoemacs-component-struct--hook-hash (hook &optional layout obj)
  "Get hook hash"
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        tmp
        (hash (make-hash-table)))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (maphash
         (lambda(key value)
           (puthash key (append (gethash key hash) value) hash))
         (ergoemacs-component-struct--hook-hash hook layout cur-obj)))
      hash)
     (t
      (when (hash-table-p (setq tmp (gethash hook (ergoemacs-component-struct-hook-maps obj))))
        (maphash
         (lambda(key value)
           ;; Put the translated keymap in a list in the hash.
           (puthash key (list (ergoemacs-component-struct--get obj cur-layout (list 'hook-maps hook key) value)) hash))
         tmp))
      hash))))

(defun ergoemacs-component-struct--hook (hook &optional layout obj)
  "Get keymaps that should be applied in an alist similiar to `minor-mode-map-alist'.
The `car' of the alist should be the keymap that should be
modified, the `cdr' of the alsit should be the keymap that should
be composed over the keymap.  This is done in
`ergoemacs-component-struct--composed--composed-hook'."
  (let* (ret tmp label)
    (maphash
     (lambda(key value)
       (setq tmp (when (ergoemacs-keymapp (ergoemacs-sv key))
                   (ergoemacs-sv key))
             label (list 'hook-maps key (or layout ergoemacs-keyboard-layout) (if tmp t nil))
             tmp (ergoemacs-map-keymap nil (make-composed-keymap value tmp)))
       (ergoemacs tmp :label label)
       (push (cons key tmp) ret))
     (ergoemacs-component-struct--hook-hash hook layout obj))
    ret))

(defun ergoemacs-component-struct--composed-hook (hook &optional layout obj)
  "Apply keymaps defined in HOOK. "
  (dolist (elt (ergoemacs-component-struct--hook hook layout obj))
    (set (car elt) (make-composed-keymap (cdr elt) (symbol-value (car elt))))))

(defvar ergoemacs-component-struct--create-hooks nil)
(defun ergoemacs-component-struct--create-hooks (&optional obj)
  "Gets a list of hooks that need to be defined eor OBJ."
  (dolist (hook (ergoemacs-component-struct--hooks obj))
    (eval `(progn
             (defun ,(intern (concat "ergoemacs--" (symbol-name hook))) ()
               ,(format "`ergoemacs-mode' hook for `%s'" (symbol-name hook))
               (ergoemacs-component-struct--composed-hook ',hook))
             ;; (push )
             (push ',hook ergoemacs-component-struct--create-hooks)
             (add-hook ',hook #',(intern (concat "ergoemacs--" (symbol-name hook))))))))

(defun ergoemacs-component-struct--rm-hooks ()
  "Remove hooks that were created with `ergoemacs-component-struct--create-hooks'"
  (dolist (hook ergoemacs-component-struct--create-hooks)
    (remove-hook hook (intern (concat "ergoemacs--" (symbol-name hook)))))
  (setq ergoemacs-component-struct--create-hooks nil))

(defun ergoemacs-component-struct--translated-list (obj list &optional layout)
  "Translate LIST based on OBJ translation and LAYOUT."
  (let ((cur-layout (or layout ergoemacs-keyboard-layout))
        new-list)
    (dolist (key list)
      (push (ergoemacs-translate
             key (ergoemacs-component-struct-just-first-keys obj)
             (ergoemacs-component-struct-variable-modifiers obj)
             (ergoemacs-component-struct-variable-prefixes obj) cur-layout
             (ergoemacs-component-struct-layout obj))
            new-list))
    new-list))

(defvar ergoemacs-component-struct--refresh-variables nil
  "To reset a current theme, the variables are refreshed when this is non-nil.")

;;; Change variable values.
(defun ergoemacs-component-struct--set (symbol newval &optional hook object)
  "Set variables up for components."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--set' is confused."))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure.")
        (push (list symbol newval hook) (ergoemacs-component-struct-variables obj)))))))

(defun ergoemacs-component-struct--variables (&optional obj)
  "Get a list of variables for the OBJ."
  (let ((obj (or obj (ergoemacs-theme-components))))
    (cond
     ((consp obj)
      (let (ret)
        (dolist (cur-obj (ergoemacs-component-struct--lookup-hash obj))
          (setq ret (append ret (ergoemacs-component-struct--variables cur-obj))))
        ret))
     ((ergoemacs-component-struct-p obj)
      (ergoemacs-component-struct-variables obj))
     (t (ergoemacs-component-struct--variables (ergoemacs-component-struct--lookup-hash obj))))))

(defvar ergoemacs-component-struct--refresh-variables nil)
(defvar ergoemacs-component-struct--applied-inits '())

(defun ergoemacs-component-struct--apply-inits (&optional _file obj)
  "Apply the initializations from the OBJ."
  (when (eq ergoemacs-component-struct--refresh-variables t)
    (setq ergoemacs-component-struct--refresh-variables ergoemacs-component-struct--applied-inits))
  (let ((obj (or obj (ergoemacs-theme-components))))
    (dolist (init (ergoemacs-component-struct--variables obj))
      (let ((x (and ergoemacs-component-struct--refresh-variables (boundp (nth 0 init))
                    (assq (nth 0 init) ergoemacs-component-struct--refresh-variables))))
        (cond
         ((and x
               (not (nth 2 init))
               (not
                (equal (ergoemacs-sv (nth 0 init))
                       (funcall (nth 1 init)))))
          ;; Values have changed, so reapply.
          (setq ergoemacs-component-struct--refresh-variables (delq x ergoemacs-component-struct--refresh-variables)
                x nil))
         ((and x (nth 2 init))
          ;; Reapply hooks
          (setq ergoemacs-component-struct--refresh-variables (delq x ergoemacs-component-struct--refresh-variables)
                x nil)))
        (cond
         (x ;; Values have not changed
          (setq ergoemacs-component-struct--refresh-variables (delq x ergoemacs-component-struct--refresh-variables)))
         ((not (boundp (nth 0 init))) ;; Do nothing, not bound yet.
          )
         ((assq (nth 0 init) ergoemacs-component-struct--applied-inits)
          ;; Already applied, Do nothing for now.
          )
         ((nth 2 init)
          ;; Hook
          (let ((add-hook-p (nth 0 (nth 2 init)))
                (append-p (nth 1 (nth 2 init)))
                (local-p (nth 2 (nth 2 init))))
            (if add-hook-p
                (funcall 'add-hook (nth 0 init) (nth 1 init) append-p local-p)
              (funcall 'remove-hook (nth 0 init) (nth 1 init) local-p))
            (push (list (nth 0 init) (nth 1 init)
                        (list (not add-hook-p) append-p local-p))
                  ergoemacs-component-struct--applied-inits)))
         (t
          ;; (Nth 0 Init)iable state change
          (push (list (nth 0 init) (ergoemacs-sv (nth 0 init)))
                ergoemacs-component-struct--applied-inits)
          (ergoemacs-set (nth 0 init) (funcall (nth 1 init))))))))
  ;; Now remove things that were not set
  (when ergoemacs-component-struct--refresh-variables
    (let ((tmp ergoemacs-component-struct--applied-inits))
      (setq ergoemacs-component-struct--applied-inits ergoemacs-component-struct--refresh-variables)
      (setq ergoemacs-component-struct--refresh-variables nil)
      (unwind-protect
          (ergoemacs-component-struct--remove-inits)
        (setq ergoemacs-component-struct--applied-inits tmp)))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-component-struct--apply-inits)
(add-hook 'ergoemacs-after-load-functions #'ergoemacs-component-struct--apply-inits)

(defun ergoemacs-component-struct--remove-inits ()
  "Remove the applied initializations of modes and variables.
This assumes the variables are stored in `ergoemacs-component-struct--applied-inits'"
  (if ergoemacs-component-struct--refresh-variables
      (setq ergoemacs-component-struct--refresh-variables ergoemacs-component-struct--applied-inits)
    (dolist (init ergoemacs-component-struct--applied-inits)
      (let ((var (nth 0 init))
            ;; (val (nth 1 init))
            (hook (nth 2 init)))
        (cond
         (hook
          (let ((add-hook-p (nth 0 hook))
                (append-p (nth 1 hook))
                (local-p (nth 2 hook)))
            (if add-hook-p
                (funcall 'add-hook (nth 0 init) (nth 1 init) append-p local-p)
              (funcall 'remove-hook (nth 0 init) (nth 1 init) local-p))))
         (t
          (ergoemacs-reset var))))))
  (setq ergoemacs-component-struct--applied-inits '()))

(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-component-struct--remove-inits)

(defun ergoemacs-component-struct--versions (&optional obj)
  "Get Versions available for OBJ.
If Object isn't specified assume it is for the current ergoemacs theme."
  (let ((obj (or obj (ergoemacs-theme-components obj))))
    (if (not obj)
        (error "`ergoemacs-theme-components' could not be detected...")
      (sort (cond
             ((consp obj)
              (let (ret)
                (dolist (cur-obj (ergoemacs-component-struct--lookup-hash obj))
                  (dolist (ver (ergoemacs-component-struct-versions cur-obj))
                    (unless (member ver ret)
                      (push ver ret))))
                ret))
             (t (ergoemacs-component-struct--versions (ergoemacs-component-struct--lookup-hash obj))))
            'string<))))

(defun ergoemacs-component--checkout (&optional obj dont-show)
  "Checks out ergoemacs-component OBJ"
  (interactive)
  (let ((obj (ergoemacs-component-struct--lookup-hash (or obj (ergoemacs-theme-components obj)))))
    (cond
     ((consp obj)
      (ergoemacs-debug-heading "Composite Keymap")
      (ergoemacs-map-- nil nil nil nil obj nil)
      (dolist (cur-obj obj)
        (ergoemacs-component--checkout cur-obj t))
      (call-interactively 'ergoemacs-debug))
     ((ergoemacs-component-struct-p obj)
      (ergoemacs-debug-heading "%s" (ergoemacs-component-struct-name obj))
      (ergoemacs-debug "Base Layout: %s" (ergoemacs-component-struct-layout obj))
      (ergoemacs-debug "Relative To: %s" (ergoemacs-component-struct-relative-to obj))
      (ergoemacs-debug "Variable Modifiers: %s" (ergoemacs-component-struct-relative-to obj))
      (ergoemacs-debug "Variable Prefixes: %s" (mapconcat (lambda(x) (key-description x)) (ergoemacs-component-struct-variable-prefixes obj) ", "))
      (ergoemacs-debug "True Keymap:")
      (ergoemacs-debug-keymap (ergoemacs-component-struct-map obj))
      (ergoemacs-debug "Translated Keymap (%s):" ergoemacs-keyboard-layout)
      (ergoemacs-debug-keymap (ergoemacs-component-struct--get obj ergoemacs-keyboard-layout))
      (unless dont-show
        (call-interactively 'ergoemacs-debug))))))

(provide 'ergoemacs-component)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-component.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
