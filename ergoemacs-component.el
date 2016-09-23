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
;; 
;;; Code:
;; (require 'guide-key nil t)
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
(defvar package--initialized)

(declare-function diminish "diminish")
(declare-function diminish-undo "diminish")

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

(declare-function ergoemacs-theme--get-version "ergoemacs-theme-engine")

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--keymap "ergoemacs-key-description")
(declare-function ergoemacs-key-description--unicode-char "ergoemacs-key-description")

(declare-function package-installed-p "package")

(declare-function ergoemacs-layout--regexp "ergoemacs-layouts")
(declare-function ergoemacs-layouts--list "ergoemacs-layouts")


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

(cl-defstruct ergoemacs-component-struct
  "A basic ergoemacs component map structure."
  (name "default-name")
  (plist '())
  (map nil)
  (maps (make-hash-table))
  (cond-maps (make-hash-table))
  (hook-maps (make-hash-table))
  (hook-plists (make-hash-table))
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
  (package-name nil)
  (autoloads nil)
  (ensure nil)
  (layout "us" :read-only t)
  (calculated-layouts (make-hash-table :test 'equal))
  (relative-to 'global-map)
  (defer nil))

(defvar ergoemacs-component-struct--define-key-current nil)

(defvar ergoemacs-component-struct--ensure-refreshed-p nil)
(defun ergoemacs-component-struct--ensure (package &optional defer autoloads)
  "Ensure PACKAGE is installed.

When DEFER is non-nil, dont `require' the package, just make sure
it is installed.

The AUTOLOADS is a list of functions that need to be autoloaded
if the package is deferred."
  (when package
    (ergoemacs-timing ensure
      (ergoemacs-timing (intern (format "ensure-%s" package))
        (let ((package (or (and (symbolp package) package)
                           (and (stringp package) (intern package)))))
          (unless (or defer (featurep package))
            (require package nil t))
          (when (and package (not (featurep package)) (numberp defer))
            (run-with-idle-timer defer nil #'require package  ;; `(lambda()
                           ;; (message ,(format "Defer: %s %s" package defer))
                           ;; (require ,package)
                                 ;; (ergoemacs-component-struct--apply-inits))
				 )
            )
          (when (and defer autoloads)
            (dolist (c autoloads)
              (unless (fboundp (car c))
                (autoload (car c) (format "%s" (cdr c)) nil t))))
          (unless (featurep package)
            (unless package--initialized
              (package-initialize))
            (if (package-installed-p package) t
              (unless ergoemacs-component-struct--ensure-refreshed-p
                (package-refresh-contents)
                (setq ergoemacs-component-struct--ensure-refreshed-p t))
              (unless (progn (ignore-errors (package-install package))
                             (package-installed-p package))
                (ergoemacs-warn "ergoemacs-mode could not install %s." package))
              (unless defer
                (require package nil t)))))))))

(defun ergoemacs-component-struct--parse-list (list function &rest args)
  "Handle :bind and :mode LIST and call FUNCTION.

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
      ;; :list ("C-." . ace-jump-mode)
      ;; :list ("C-." ace-jump-mode)
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

(defun ergoemacs-component-struct--handle-bind-1 (kbd-str def keymap)
  "Tell `ergoemacs-mode' to bind KBD-STR to DEF in KEYMAP."
  (ergoemacs-component-struct--define-key keymap (read-kbd-macro kbd-str) def))

(defun ergoemacs-component-struct--handle-bind (bind &optional keymap)
  "Handle :bind and related properties.

BIND is the property list from the component definition.

KEYMAP is the symbol of a bound keymap.  If unspecified, the
binding assumes KEYMAP is `global-map'."
  (let ((keymap (or keymap 'global-map)))
    (ergoemacs-component-struct--parse-list
     bind #'ergoemacs-component-struct--handle-bind-1 keymap)))

(defun ergoemacs-component-struct--handle-mode-1 (regexpr mode)
  "Add (cons REGEXPR MODE) to `auto-mode-alist'.
Also autoload MODE.

Requires `ergoemacs-component-struct--define-key-current' to be
an `ergoemacs-component-struct' object."
  ;; (message "Handle Mode #2: %s %s" regexpr mode)
  (when (ergoemacs-component-struct-p ergoemacs-component-struct--define-key-current)
    (let* ((c (cons regexpr mode))
           (obj ergoemacs-component-struct--define-key-current)
           (package-name (ergoemacs-component-struct-package-name obj)))
      (ergoemacs-component-struct--deferred
       `(unless (member ',c auto-mode-alist)
          (push ',c auto-mode-alist)))
      (when (and package-name mode (not (fboundp mode)))
        ;; Create autoload.
        (autoload mode (format "%s, a major mode defined in %s" mode package-name) nil t)
        (setq c (cons mode package-name))
        (unless (member c (ergoemacs-component-struct-autoloads obj))
          (push (cons mode package-name) (ergoemacs-component-struct-autoloads obj)))))))

(defun ergoemacs-component-struct--handle-mode (mode)
  "Handle MODE list from :mode keyword."
  (when mode
    (ergoemacs-component-struct--parse-list mode #'ergoemacs-component-struct--handle-mode-1)))

(defun ergoemacs-component-struct--create-component (plist body file)
  "Create ergoemacs component.

PLIST is the component properties

BODY is the body of function.

FILE is the file name where the component was created."
  (ergoemacs-timing (intern (format "create-component-%s" (plist-get plist :name)))
    (unwind-protect
        (progn
          (setq ergoemacs-component-struct--define-key-current
                (make-ergoemacs-component-struct
                 :name (plist-get plist :name)
                 :plist (plist-put plist :file file)
                 :just-first-keys (or (plist-get plist :just-first-keys) nil)
                 :variable-modifiers (or (plist-get plist :variable-modifiers) '(meta))
                 :variable-prefixes (or (plist-get plist :variable-prefixes) '([apps] [menu] [27]))
                 :package-name (plist-get plist :package-name)
                 :ensure (plist-get plist :ensure)
                 :layout (or (plist-get plist :layout) "us")
                 :defer (plist-get plist :defer)))
          (let* ((tmp (plist-get plist :bind-keymap))
                 (package-name (plist-get plist :package-name))
                 (demand (plist-get plist :demand))
                 (defer (if demand nil (plist-get plist :defer)))
                 (defer-present-p (or demand defer (memq :defer plist))))
            
            ;; Handle :bind-keymap commands
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (ergoemacs-component-struct--handle-bind tmp)

            ;; Handle :bind-keymap* commands
            (setq tmp (plist-get plist :bind-keymap*))
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (ergoemacs-component-struct--handle-bind tmp 'ergoemacs-override-keymap)
            
            ;; Handle :bind keys
            (setq tmp (plist-get plist :bind))
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (ergoemacs-component-struct--handle-bind tmp)
            
            ;; Handle :bind* commands
            (setq tmp (plist-get plist :bind*))
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (ergoemacs-component-struct--handle-bind tmp 'ergoemacs-override-keymap)

            ;; Handle :mode
            (setq tmp (plist-get plist :mode))
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (ergoemacs-component-struct--handle-mode tmp)
            
            ;; Handle :commands
            (setq tmp (plist-get plist :commands))
            (when (and tmp (not defer-present-p) (not defer))
              (setq defer-present-p t defer t)
              (setf (ergoemacs-component-struct-defer ergoemacs-component-struct--define-key-current) t))
            (when package-name
              (cond
               ((and tmp (symbolp tmp))
                (autoload tmp (format "%s" package-name) nil t)
                (push (cons tmp package-name) (ergoemacs-component-struct-autoloads ergoemacs-component-struct--define-key-current)))
               ((consp tmp)
                (dolist (f tmp)
                  (when (and f (symbolp f))
                    (autoload f (format "%s" package-name) nil t)
                    (push (cons f package-name) (ergoemacs-component-struct-autoloads ergoemacs-component-struct--define-key-current))))))))
          (funcall body)
          (setf (ergoemacs-component-struct-variables ergoemacs-component-struct--define-key-current)
                (reverse (ergoemacs-component-struct-variables ergoemacs-component-struct--define-key-current))))
      
      (puthash (concat (ergoemacs-component-struct-name ergoemacs-component-struct--define-key-current)
                       (and (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current)
                            (concat "::" (ergoemacs-component-struct-version ergoemacs-component-struct--define-key-current))))
               ergoemacs-component-struct--define-key-current ergoemacs-component-hash)
      (setq ergoemacs-component-struct--define-key-current nil))))

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
     ((and (consp def)
           (= 2 (length def))
           (stringp (nth 0 def))
           (or (not (nth 1 def))
               (ergoemacs-gethash (nth 1 def) ergoemacs-translation-hash)))
      `(lambda(&optional arg)
         (interactive "P")
         (ergoemacs-command-loop ,(nth 0 def) ',(nth 1 def))))
     ((ergoemacs-keymapp (ergoemacs-sv def))
      (ergoemacs-sv def))
     (t def))))

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
                            (ergoemacs-gethash keymap (ergoemacs-component-struct-maps obj)))
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
            (ergoemacs :define-key cur-map key new-fn)
            ;; Now fix cached layouts
            (maphash
             (lambda(key value)
               (setq cur-layout (nth 1 key))
               (when (or (and global-map-p (not (nth 0 key)))
                         (eq (nth 0 key) keymap))
                 ;; Update keymap (in place).
                 (ergoemacs :define-key value
                            (ergoemacs-translate
                             key (ergoemacs-component-struct-just-first-keys obj)
                             (ergoemacs-component-struct-variable-modifiers obj)
                             (ergoemacs-component-struct-variable-prefixes obj) cur-layout
                             (ergoemacs-component-struct-layout obj)) new-fn)))
             (ergoemacs-component-struct-calculated-layouts obj))))
        ;; Update dynamic/deferred keys
        (fset (ergoemacs-component-struct-dynamic-keys obj) new-dynamic)))))

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

(defun ergoemacs-component-struct--define-key (keymap key def &optional object)
  "In KEYMAP, define KEY to be DEF for OBJECT.
If not specified, OBJECT is `ergoemacs-component-struct--define-key-current'."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--define-key' is confused"))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current))
          (key (or (and (consp key) (memq (car key) '(kbd read-kbd-macro))
                        (stringp (nth 1 key)) (read-kbd-macro (nth 1 key)))
                   key))
          (def (ergoemacs-component-struct--define-key-get-def def)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT not a ergoemacs-component-structure")
        (setq key (vconcat key))
        (let* ((global-map-p (or (eq keymap 'global-map) (eq keymap 'ergoemacs-mode-map)
                                 (eq keymap 'ergoemacs-keymap)))
               (when-condition (ergoemacs-component-struct-when-condition obj))
               (hook (ergoemacs-component-struct-hook obj))
               (cur-map (or (and global-map-p (not when-condition)
                                 (ergoemacs-component-struct--ini-map obj))
                            (and (not when-condition) (ergoemacs-gethash keymap (ergoemacs-component-struct-maps obj)))
                            (and global-map-p when-condition (ergoemacs-gethash when-condition (ergoemacs-component-struct-cond-maps obj)))
                            (and when-condition hook (ignore-errors (ergoemacs-gethash keymap (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj)))))))
               (package-name (ergoemacs-component-struct-package-name obj))
               fn-lst
               (key (or (and (vectorp key) key)
                        (and (stringp key) (vconcat key))))
               tmp)
          (cond
           ((and (not cur-map) (not when-condition))
            (cl-pushnew keymap ergoemacs-map-properties--known-maps)
	    (cl-pushnew keymap ergoemacs-map-properties--label-atoms-maps)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-component-struct-maps obj)))
           ((and (not cur-map) when-condition global-map-p)
            (setq cur-map (make-sparse-keymap))
            (puthash when-condition cur-map (ergoemacs-component-struct-cond-maps obj)))
           ((and (not cur-map) when-condition hook)
            (unless (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj))
              (puthash hook (make-hash-table) (ergoemacs-component-struct-hook-maps obj)))
            (cl-pushnew keymap ergoemacs-map-properties--known-maps)
	    (cl-pushnew keymap ergoemacs-map-properties--label-atoms-maps)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj)))))
          (cond
           ((and global-map-p (not when-condition) (not def) (setq tmp (lookup-key (ergoemacs-component-struct-map obj) key))
                 (not (integerp tmp)))
            ;; Remove the key from the keymap, do not set it to
            ;; nil; Its as if it was never defined
            (setq ergoemacs-component-struct--define-key-temp-map (make-sparse-keymap))
            (ergoemacs-timing remove-global-map-map-keymap
              (ergoemacs-map-keymap
               (lambda (cur-key item)
                 (if (consp cur-key)
                     (ergoemacs-warn "Keymap range currently not supported %s %s" cur-key item)
                   (unless (eq item 'ergoemacs-prefix)
                     (unless (equal key cur-key)
                       (ergoemacs :define-key ergoemacs-component-struct--define-key-temp-map cur-key item)))))
               cur-map))
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
            (ergoemacs-timing remove-local-keymap-map-keymap
              (ergoemacs-map-keymap
               (lambda (cur-key item)
                 (if (consp cur-key)
                     (message "Key range not supported %s, %s" cur-key item)
                   (unless (eq item 'ergoemacs-prefix)
                     (unless (equal key cur-key)
                       (ergoemacs :define-key ergoemacs-component-struct--define-key-temp-map cur-key item)))))
               cur-map))
            (puthash keymap (copy-keymap ergoemacs-component-struct--define-key-temp-map) (ergoemacs-component-struct-maps obj))
            (setq ergoemacs-component-struct--define-key-temp-map nil))
           ((and (consp def) (stringp (nth 0 def)) (symbolp (nth 1 def)) (eq (nth 1 def) 'keymap))
            (ergoemacs :define-key cur-map key def))
           ((and (consp def) (symbolp (nth 1 def))) ;; (fn1 fn2 fn3 fn4)
            (unless (catch 'found-fn
                      (dolist (cur-def def)
                        (if (not (commandp cur-def t))
                            (push cur-def fn-lst)
                          (if (ergoemacs-keymapp cur-def)
			      (ergoemacs :define-key cur-map key (copy-keymap cur-def))
			    (ergoemacs :define-key cur-map key cur-def))
                          (throw 'found-fn t)))
                      nil)
              ;; Not found
              (ergoemacs :define-key cur-map key `(lambda() (interactive) (error ,(format "This key is undefined without one of the following functions: %s" fn-lst)))))
            (when fn-lst ;; Test for later
              (push (list keymap key fn-lst)
                    (ergoemacs-component-struct-dynamic-keys obj))))
           (t
            (if (ergoemacs-keymapp def)
		(ergoemacs :define-key cur-map key (copy-keymap def))
	      (ergoemacs :define-key cur-map key def))
            (when (and package-name def (not (fboundp def)))
              ;; Create autoload.
              (autoload def (format "%s" package-name) nil t)
              (push (cons def package-name) (ergoemacs-component-struct-autoloads obj)))))))))))

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
                                    (or current-version (ergoemacs :current-version))  versions)))
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
        (if (string-match-p "::" map) ret
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

(defun ergoemacs-component-struct--minor-mode-map-alist (&optional obj)
  "Get the ending maps for `minor-mode-map-alist' using the ergoemacs structures OBJ."
  (let (ret map parent)
    (maphash
     (lambda(key value)
       (setq parent (make-composed-keymap value)
             map (make-sparse-keymap))
       (ergoemacs map :label (list 'cond-map key (intern ergoemacs-keyboard-layout)))
       (set-keymap-parent map parent)
       (push (cons key map) ret))
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
		    (set (make-local-variable 'ergoemacs-command-loop--minibuffer-unsupported-p) t)
		    (throw 'unsupported-p t))))))
	  (if ergoemacs-component-struct--composed-hook-minibuffer
	      (push elt ergoemacs-component-struct--composed-hook-minibuffer)
	    (set (make-local-variable 'ergoemacs-component-struct--composed-hook-minibuffer)
		 (list elt))))
      (set (make-local-variable (car elt)) (make-composed-keymap (cdr elt) (symbol-value (car elt)))))))

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
  "Remove hooks.

These hooks are those created with
`ergoemacs-component-struct--create-hooks'."
  (dolist (hook ergoemacs-component-struct--create-hooks)
    (remove-hook hook (intern (concat "ergoemacs--" (symbol-name hook)))))
  (setq ergoemacs-component-struct--create-hooks nil))

(defun ergoemacs-component-struct--translated-list (obj list &optional layout)
  "Base on OBJ translation, Translate LIST using LAYOUT."
  (let ((cur-layout (or layout ergoemacs-keyboard-layout))
        new-list)
    (dolist (key list)
      (ergoemacs :apply-key key
                 (lambda(trans-key)
                   (push (ergoemacs-translate
                          trans-key (ergoemacs-component-struct-just-first-keys obj)
                          (ergoemacs-component-struct-variable-modifiers obj)
                          (ergoemacs-component-struct-variable-prefixes obj) cur-layout
                          (ergoemacs-component-struct-layout obj))
                         new-list))))
    new-list))

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
      (mapcar (lambda(x)
		(append x (list (ergoemacs-component-struct-name obj))))
	      (ergoemacs-component-struct-variables obj)))
     (t (ergoemacs-component-struct--variables (ergoemacs-component-struct--lookup-hash obj))))))

(defvar ergoemacs-component-struct--refresh-variables nil)
(defvar ergoemacs-component-struct--applied-inits '())
(defvar ergoemacs-component-struct--deferred-functions '())

(defvar ergoemacs-component-struct--apply-inits-first-p t)
(defvar ergoemacs-component-struct--apply-ensure-p nil)
(defvar ergoemacs-component-struct--applied-plists nil)

(defvar ergoemacs-component-echo-loaded-file-p nil)

(defvar ergoemacs-component-struct--apply-inits nil)
(defun ergoemacs-component-struct--apply-inits (&optional file obj)
  "Apply the initializations after loading FILE from the object OBJ.

This is a wrapper for `ergoemacs-component-struct--apply-inits--'
to prevent infinite recursion."
  (unless ergoemacs-component-struct--apply-inits
    (setq ergoemacs-component-struct--apply-inits t)
    (unwind-protect
	(ergoemacs-component-struct--apply-inits-- file obj))
    (setq ergoemacs-component-struct--apply-inits nil)))

(defun ergoemacs-component-struct--apply-inits-- (&optional file obj)
  "Apply the initializations after loading FILE from the object OBJ."
  (ergoemacs-map-properties--label-known)
  (when (and ergoemacs-component-echo-loaded-file-p file)
    (message "`ergoemacs-mode' Loaded %s" file))
  (when (eq ergoemacs-component-struct--refresh-variables t)
    (setq ergoemacs-component-struct--refresh-variables ergoemacs-component-struct--applied-inits))
  (let* ((obj (or obj (ergoemacs-theme-components)))
         package-name ensure defer comp tmp autoloads)
    (when ergoemacs-component-struct--apply-inits-first-p
      (setq ergoemacs-component-struct--apply-inits-first-p nil
            ergoemacs-component-struct--apply-ensure-p t)
      (if (not ergoemacs-mode--fast-p)
          (setq ergoemacs--start-emacs-state-2 (ergoemacs--emacs-state))
        ;; Check to see if emacs state has changed.
        (setq ergoemacs--start-emacs-state-2 (ergoemacs--emacs-state))
        (ergoemacs-mode--setup-hash-tables--setq
         nil
         'ergoemacs--last-start-emacs-state-2 nil)
        (unless (equal ergoemacs--last-start-emacs-state-2 ergoemacs--start-emacs-state-2)
          (if (not ergoemacs--last-start-emacs-state-2)
              (progn
                (message "Saving fast startup state.")
                (setq ergoemacs--last-start-emacs-state-2 ergoemacs--start-emacs-state-2)
                (ergoemacs-mode--setup-hash-tables--setq
                 t
                 'ergoemacs--last-start-emacs-state-2 ergoemacs--last-start-emacs-state-2))
            (ergoemacs-mode-clear-cache t)
            (ergoemacs-warn "ergoemacs-mode cache reset AFTER loading; Keys may be slightly inconsistent until emacs restart.")))))
    (when ergoemacs-component-struct--apply-ensure-p
      (setq ergoemacs-component-struct--apply-ensure-p nil)
      ;; Ensure packages
      (dolist (elt obj)
        (setq comp (ergoemacs-component-struct--lookup-hash elt)
              package-name (ergoemacs-component-struct-package-name comp)
              ensure (ergoemacs-component-struct-ensure comp)
              autoloads (ergoemacs-component-struct-autoloads comp)
              defer (ergoemacs-component-struct-defer comp))
        (cond
         ((eq ensure t)
          (ergoemacs-component-struct--ensure package-name defer autoloads))
         ((and ensure (symbolp ensure))
          (ergoemacs-component-struct--ensure ensure defer autoloads))
	 ((and (consp ensure) (memq (car ensure) '(memq member and or if when = string= not string< eq equal)))
	  (when (ignore-errors (eval ensure))
	    (ergoemacs-component-struct--ensure package-name defer autoloads)))
         ((consp ensure)
          (dolist (elt ensure)
            (cond
             ((and elt (symbolp elt))
              (ergoemacs-component-struct--ensure elt defer autoloads))
             ((stringp elt)
              (ergoemacs-component-struct--ensure (intern elt) defer autoloads)))
            (setq autoloads nil)))
         ((stringp ensure)
          (ergoemacs-component-struct--ensure (intern ensure) defer autoloads)))))
    ;; Turn on plist options (like :diminish)
    (dolist (elt obj)
      (unless (memq elt ergoemacs-component-struct--applied-plists)
        (let* ((comp (ergoemacs-component-struct--lookup-hash elt))
               (plist (ergoemacs-component-struct-plist comp))
               fn)
          (dolist (elt plist)
            (when (and (symbolp elt)
                       (setq fn (intern (format "ergoemacs-component--%s-on"
                                                (substring (symbol-name elt) 1))))
                       (fboundp fn))
              (funcall fn plist)))
          (push elt ergoemacs-component-struct--applied-plists))))
    
    ;; Turn off plist options
    (setq tmp nil)
    (dolist (elt ergoemacs-component-struct--applied-plists)
      (if (memq elt obj)
          (push elt tmp)
        (let* ((comp (ergoemacs-component-struct--lookup-hash elt))
               (plist (ergoemacs-component-struct-plist comp))
               fn)
          (dolist (elt plist)
            (when (and (symbolp elt)
                       (setq fn (intern (format "ergoemacs-component--%s-off"
                                                (substring (symbol-name elt) 1))))
                       (fboundp fn))
              (funcall fn plist))))))
    (setq ergoemacs-component-struct--applied-plists tmp)
    (dolist (cur-obj obj)
      (ergoemacs-timing (intern (format "initialize-%s" cur-obj))
        (dolist (init (ergoemacs-component-struct--variables cur-obj))
          (if (and (consp (nth 0 init)) (not (nth 1 init)) (not (nth 2 init)))
              (unless (member (nth 0 init) ergoemacs-component-struct--deferred-functions)
                (cond
                 ((eq (car (nth 0 init)) 'add-to-list)
                  (when (ignore-errors (boundp (nth 1 (nth 0 init))))
                    (ignore-errors
                      (apply (car (nth 0 init)) (cdr (nth 0 init)))
                      (push (nth 0 init) ergoemacs-component-struct--deferred-functions)))
                  (when (ignore-errors (eq 'quote (nth 0 (nth 1 (nth 0 init)))))
                    (if (ignore-errors (eq 'quote (nth 0 (nth 2 (nth 0 init)))))
                        (when (ignore-errors (boundp (nth 1 (nth 1 (nth 0 init)))))
                          (apply 'add-to-list  (nth 1 (nth 2 (nth 0 init))) (cdr (cdr (cdr (nth 0 init)))))
                          (push (nth 0 init) ergoemacs-component-struct--deferred-functions))
                      (when (ignore-errors (boundp (nth 1 (nth 1 (nth 0 init)))))
                        (apply 'add-to-list (nth 1 (nth 1 (nth 0 init))) (cdr (cdr (nth 0 init))))
                        (push (nth 0 init) ergoemacs-component-struct--deferred-functions)))))
                 ((memq (car (nth 0 init)) '(push pushnew cl-pushnew))
                  (when (ignore-errors (boundp (nth 2 (nth 0 init))))
                    (if (ignore-errors (eq 'quote (nth 1 (nth 1 (nth 0 init)))))
                        (ignore-errors
                          (apply (car (nth 0 init)) (nth 1 (nth 1 (nth 0 init))) (cdr (cdr (nth 0 init))))
                          (push (nth 0 init) ergoemacs-component-struct--deferred-functions))
                      (ignore-errors
                        (apply (car (nth 0 init)) (cdr (nth 0 init)))
                        (push (nth 0 init) ergoemacs-component-struct--deferred-functions)))
                    (ignore-errors
                      (apply (car (nth 0 init)) (cdr (nth 0 init)))
                      (push (nth 0 init) ergoemacs-component-struct--deferred-functions))))
                 ((eq (car (nth 0 init)) 'require)
                  (require (nth 1 (nth 1 (nth 0 init))) nil t)
                  (when (not (featurep (nth 1 (nth 1 (nth 0 init)))))
                    ;; Attempt to ensure the feature, if specified.
                    (ergoemacs-warn "Could not load %s; %s" (nth 1 (nth 1 (nth 0 init)))
                          (nth 3 init))))
                 (t
                  (condition-case err
                      (eval (nth 0 init))
                    (error (progn
			     (ergoemacs-warn "%s while evaluating %s" err (nth 0 init))
			     (debug err))))
                  (push (nth 0 init) ergoemacs-component-struct--deferred-functions))
                 ;; (t (ergoemacs-warn "Theme did not handle: %s" (nth 0 init)))
                 ))
            (let ((x (and ergoemacs-component-struct--refresh-variables (boundp (nth 0 init))
                          (assq (nth 0 init) ergoemacs-component-struct--refresh-variables)))
                  add-hook-p append-p local-p)
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
               ((and (nth 2 init) ;; Already applied hook?
                     (setq add-hook-p (nth 0 (nth 2 init))
                           append-p (nth 1 (nth 2 init))
                           local-p (nth 2 (nth 2 init)))
                     (member (list (nth 0 init) (nth 1 init)
                                   (list (not add-hook-p) append-p local-p))
                             ergoemacs-component-struct--applied-inits)))
               ((nth 2 init)
                ;; Hook
                (if add-hook-p
                    (progn
                      (funcall 'add-hook (nth 0 init) (nth 1 init) append-p local-p)
                      ;; (message "%s: (add-hook %s %s %s %s)"
                      ;;          cur-obj (nth 0 init) (nth 1 init)
                      ;;          append-p local-p)
                      )
                  (funcall 'remove-hook (nth 0 init) (nth 1 init) local-p)
                  ;; (message "%s: (remove-hook %s %s %s %s)"
                  ;;          cur-obj (nth 0 init) (nth 1 init)
                  ;;          append-p local-p)
                  )
                (push (list (nth 0 init) (nth 1 init)
                            (list (not add-hook-p) append-p local-p))
                      ergoemacs-component-struct--applied-inits))
               ((and (not (nth 2 init)) (assq (nth 0 init) ergoemacs-component-struct--applied-inits))
                ;; Already applied, Do nothing for now.
                )
               (t
                ;; (Nth 0 Init)iable state change
                (when (ergoemacs-set (nth 0 init) (funcall (nth 1 init))
                                     (ergoemacs-component-struct-defer (ergoemacs-component-struct--lookup-hash cur-obj)))
                  (push (list (nth 0 init) (ergoemacs-sv (nth 0 init)))
                        ergoemacs-component-struct--applied-inits)
                  )))))))))
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
        (error "`ergoemacs-theme-components' could not be detected")
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

(defun ergoemacs-component--regexp (&optional at-end)
  "Return a regexp of `ergoemacs-mode' components.

AT-END will append a \"$\" to the end of the regular expression."
  (let (ret)
    (maphash
     (lambda(key _item) (push key ret))
     ergoemacs-component-hash)
    (setq ret (regexp-opt ret 'symbols))
    (when at-end
      (setq ret (concat ret "$")))
    ret))


(defun ergoemacs-component--help-link-1 ()
  "Setup crosreferences for help."
  (let (tmp)
    ;; Link commands
    (goto-char (match-beginning 0))
    (when (and (re-search-backward "\\_<\\(.*?\\)\\_> *\\=" nil t)
               (setq tmp (intern (match-string 1)))
               (fboundp tmp)
               (commandp tmp))
      (help-xref-button 1 'help-function tmp))
    ;; Add button properties back
    (when (and tmp ergoemacs-display-key-use-face-p)
      (setq tmp (point))
      (beginning-of-line)
      (while (and (not (looking-at "Relative To:")) (re-search-forward "\\(.*?\\)[ +]" tmp t))
        (add-text-properties (match-beginning 1) (match-end 1) '(face ergoemacs-display-key-face))))
    (end-of-line)))

(defun ergoemacs-component--help-link ()
  "Links `ergoemacs-mode' components in `help-mode' buffer."
  (when (eq major-mode 'help-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (ree (format "^ - %s -- " (ergoemacs-component--regexp)))
            (ret (format "^\"%s\" - " (ergoemacs-theme--regexp)))
            (re (ergoemacs-component--regexp t))
            (rem (ergoemacs-map-properties--map-regexp t))
            (rel1 (ergoemacs-layout--regexp))
            (rel2 (ergoemacs-layout--regexp 2))
            tmp)
        (with-syntax-table emacs-lisp-mode-syntax-table
          (when (re-search-forward "^\\(\\_<.*\\_>\\) is .* component defined in `\\(.*\\)'" nil t)
            (help-xref-button 2 'ergoemacs-component-def (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward rel1 nil t)
            (help-xref-button 1 'ergoemacs-layout-help (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward rel2 nil t)
            (help-xref-button 1 'ergoemacs-layout-help (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward "^\\(\\_<.*\\_>\\) is .* component defined in `\\(.*\\)'" nil t)
            (help-xref-button 2 'ergoemacs-component-def (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward "\\(Variable Prefixes:\\|Unbound keys:\\|Masked emacs keys:\\) +" nil t)
            (while (and (not (looking-at " *$")) (re-search-forward "\\(.*?\\)\\(, +\\| *$\\|[+ ]+\\)" (point-at-eol) t))
              (add-text-properties (match-beginning 1) (match-end 1) '(face ergoemacs-display-key-face))))
          (goto-char (point-min))
          (when (re-search-forward "^\\(\\_<.*\\_>\\) is .* theme defined in `\\(.*\\)'" nil t)
            (help-xref-button 2 'ergoemacs-theme-def (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward "^This theme is based on: *\\(\\_<.*\\_>\\)" nil t)
            (help-xref-button 1 'ergoemacs-theme-help (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward ret nil t)
            (help-xref-button 1 'ergoemacs-theme-help (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward ree nil t)
            (help-xref-button 1 'ergoemacs-component-help (match-string 1)))
          (goto-char (point-min))
          (while (re-search-forward re  nil t)
            (help-xref-button 1 'ergoemacs-component-help (match-string 1))
            (ergoemacs-component--help-link-1))
          (goto-char (point-min))
          (while (re-search-forward rem  nil t)
            (when (and (setq tmp (intern (match-string 1)))
                       (boundp tmp))
              (help-xref-button 1 'help-variable tmp))
            (ergoemacs-component--help-link-1))
          (goto-char (point-min))
          (when (search-forward "an `ergoemacs-mode' layout defined" nil t)
            (setq bidi-display-reordering nil)))))))

(define-button-type 'ergoemacs-component-help
  :supertype 'help-xref
  'help-function #'ergoemacs-component-describe
  'help-echo (purecopy "mouse-2, RET: describe this ergoemacs component"))

(define-button-type 'ergoemacs-component-def
  :supertype 'help-xref
  'help-function #'ergoemacs-component-find-definition
  'help-echo (purecopy "mouse-2, RET: find this ergoemacs component's definition"))


(defcustom ergoemacs-component-find-regexp
  (concat"^\\s-*(ergoemacs-\\(?:theme-?\\)?\\(?:component\\|package\\|autoload\\)?" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used to search for a component definition.

This is used by `ergoemacs-find-component' and it must contain a
`%s' at the place where `format' should insert the compenent
name."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(unless (assoc 'ergoemacs-component find-function-regexp-alist)
  (push (cons 'ergoemacs-component 'ergoemacs-component-find-regexp) find-function-regexp-alist))

(defun ergoemacs-component-find-no-select (component &optional type)
  "Find COMPONENT of TYPE.

TYPE can be 'ergoemacs-theme, if not it defaults to a single component."
  (let* ((comp (or (and (eq type 'ergoemacs-theme)
                        (ergoemacs-gethash (format "%s" (or component "standard")) ergoemacs-theme-hash))
                   (ergoemacs-component-struct--lookup-hash (or component ""))))
         (plist (and comp (or (and (eq type 'ergoemacs-theme) comp)
                              (ergoemacs-component-struct-plist comp))))
         (file (and comp (plist-get plist :file)))
         (el-file (and file (concat (file-name-sans-extension file) ".el")))
         (name (plist-get plist :name))
         (sym (intern name))
         loc)
    (if (not comp)
        (message "Invalid %s %s" (or (and (eq type 'ergoemacs-theme) "theme")
                                     "component") component)
      (setq loc (find-function-search-for-symbol sym (or type 'ergoemacs-component) el-file))
      (when (and (eq type 'ergoemacs-component) (not (cdr loc)) (< 6 (length name)))
        (setq sym (intern (substring name 0 -6))
              loc (find-function-search-for-symbol
                   sym (or type 'ergoemacs-component) el-file)))
      loc)))

(defun ergoemacs-component-find-1 (symbol type switch-fn &optional buffer-point)
  "Find `ergoemacs-mode' component or theme.

SYMBOL is the symbol representing the component or theme.

TYPE is nil to search for a component definition, or
'ergoemacs-theme, to find the theme.

The variable `find-function-recenter-line' controls how
to recenter the display.  SWITCH-FN is the function to call
to display and select the buffer.
See also `find-function-after-hook'.

BUFFER-POINT is the point to move to.  If it isn't specified,
find it with `ergoemacs-component-find-no-select'.

Modified from `find-definition-noselect'.

Set mark before moving, if the buffer already existed."
  (let* ((orig-point (point))
         (orig-buffers (buffer-list))
         (buffer-point (or buffer-point
                           (save-excursion
                             (ergoemacs-component-find-no-select symbol type))))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point)))
    (when buffer-point
      (when (memq new-buf orig-buffers)
        (push-mark orig-point))
      (funcall switch-fn new-buf)
      (when new-point (goto-char new-point))
      (recenter find-function-recenter-line)
      (run-hooks 'find-function-after-hook))))

(defun ergoemacs-component-find-definition (component)
  "Find the definition of COMPONENT.  COMPONENT defaults to the name near point.

Finds the `ergoemacs-mode' containing the definition of the component
near point (selected by `ergoemacs-component-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

The library where FACE is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (list (ergoemacs-component-at-point)))
  (ergoemacs-component-find-1 component 'ergoemacs-component 'switch-to-buffer))



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

(defun ergoemacs-component--prompt (&optional theme-instead)
  "Prompt for component or theme (when THEME-INSTEAD is non-nil)."
  (let ((c (or (and (eq theme-instead :layout) ergoemacs-keyboard-layout)
               (ergoemacs-component-at-point theme-instead)))
        (enable-recursive-minibuffers t)
        val)
    (setq val (completing-read (if (or (symbolp c) (stringp c))
                                   (format
                                    "Describe ergoemacs %s (default %s): "
                                    (or (and (eq theme-instead :layout) "layout")
                                        (and theme-instead "theme") "component")
                                    c)
                                 (format
                                  "Describe ergoemacs %s: "
                                  (or (and (eq theme-instead :layout) "layout")
                                      (and theme-instead "theme") "component")))
                               (or (and (eq theme-instead :layout) (ergoemacs-layouts--list))
                                   (and theme-instead ergoemacs-theme-hash)
                                   ergoemacs-component-hash)
                               nil
                               ;; (lambda (vv)
                               ;;   (or (get vv 'variable-documentation)
                               ;;       (and (boundp vv) (not (keywordp vv)))))
                               t nil nil
                               (format "%s" c)))
    (list (or (and (equal val "") (format "%s" c)) val))))

(defun ergoemacs-component-cached-p (component)
  "Determine if COMPONENT is cached instead of loaded."
  (let* ((component (and component
                         (or (and (stringp component) component)
                             (and (symbolp component) (symbol-name component)))))
         (comp (ergoemacs-component-struct--lookup-hash (or component "")))
         (plist (ergoemacs-component-struct-plist comp))
         (file (plist-get plist :file))
         (el-file (and file (concat (file-name-sans-extension file) ".el")))
         (elc-file (and file (concat (file-name-sans-extension file) ".elc"))))
    (when file
      (catch 'loaded
	(dolist (load load-history)
	  (when (or (string= elc-file (car load))
		    (string= el-file (car load)))
	    (throw 'loaded nil))) t))))

(defun ergoemacs-component-describe (component)
  "Display the full documentation of COMPONENT (a symbol or string)."
  (interactive (ergoemacs-component--prompt))
  (let* ((component (and component
                         (or (and (stringp component) component)
                             (and (symbolp component) (symbol-name component)))))
         (comp (ergoemacs-component-struct--lookup-hash (or component "")))
         (plist (ergoemacs-component-struct-plist comp))
         (file (plist-get plist :file))
         (el-file (and file (concat (file-name-sans-extension file) ".el")))
         tmp vers
         lst)
    (if (not comp)
        (message "You did not specify a valid ergoemacs component %s" component)
      (help-setup-xref (list #'ergoemacs-component-describe (or component ""))
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (or component ""))
          ;; Use " is " instead of a colon so that
          ;; it is easier to get out the function name using forward-sexp.
          (insert " is an `ergoemacs-mode' component")
          (when (and el-file (file-readable-p el-file))
            (insert " defined in `")
            (insert (file-name-nondirectory el-file))
            (when (looking-back "`\\(.*\\)" nil)
              (help-xref-button 1 'ergoemacs-component-def component))
            (insert "'."))
          (insert "\n\n")
          (insert "Documentation:\n")
          (insert (plist-get plist :description))
          (insert "\n\n")
          (insert (format "Cached instead of loaded: %s\n" (or (and (ergoemacs-component-cached-p component) "Yes") "No")))

          (insert "\nKnown component properties:\n")
          (dolist (prop ergoemacs-theme-component-properties)
            (when (setq tmp (plist-get plist prop))
              (insert (format "  %s -- %s\n" prop tmp))))
          (insert "\n")
          (insert (format "Plist: %s\n" plist))
          
          (insert (format "Base Layout: %s\n" (ergoemacs-component-struct-layout comp)))
          (when (looking-back ": \\(.*\\)\n" nil)
            (help-xref-button 1 'ergoemacs-layout-help (match-string 1)))
          (insert (format "Relative To: %s\n" (ergoemacs-component-struct-relative-to comp)))
          (when (looking-back ": \\(.*\\)\n" nil)
            (help-xref-button 1 'ergoemacs-layout-help (match-string 1)))
          (insert (format "Variable Modifiers: %s\n" (ergoemacs-component-struct-variable-modifiers comp)))
          (insert (format "Variable Prefixes: %s\n"
                          (mapconcat
                           (lambda(x) (ergoemacs-key-description x))
                           (ergoemacs-component-struct-variable-prefixes comp) ", ")))

          (when (setq tmp (ergoemacs-component-struct-unbind comp))
            (insert (format "Unbound keys: %s\n"
                            (mapconcat
                             (lambda(x) (ergoemacs-key-description x)) tmp ", "))))

          (when (setq tmp (ergoemacs-component-struct-undefined comp))
            (insert (format "Masked emacs keys: %s\n"
                            (mapconcat
                             (lambda(x) (ergoemacs-key-description x)) tmp ", "))))

          ;; FIXME: Describe major-mode / minor-mode differences
          
          ;; FIXME: Describe what keys are deferred, and what they would
          ;; possibly bind to...

          (if (not (setq vers (ergoemacs-component-struct-versions comp)))
              (setq lst `(("Specified Keymap" ,(ergoemacs-component-struct-map comp))
                          (,(format "Translated Keymap (%s)" ergoemacs-keyboard-layout) ,(ergoemacs-component-struct--get comp ergoemacs-keyboard-layout))))
            (insert (format "Versions: %s, %s\n" ergoemacs-mode-version
                            (mapconcat
                             (lambda(x) x) vers ", ")))
            
            (setq lst `((,(format "Specified Keymap (%s)" (or (ergoemacs-theme--get-version) ergoemacs-mode-version))
                         ,(ergoemacs-component-struct-map comp))
                        ,@(mapcar
                           (lambda(ver)
                             (unless (string= ver (ergoemacs-theme--get-version))
                               ;; (ergoemacs-component-struct--get (ergoemacs-component-struct--lookup-hash 'search  "5.7.5"))
                               `(,(format "Specified keymap for Version %s" ver)
                                 ,(ergoemacs-component-struct-map (ergoemacs-component-struct--lookup-hash component ver)))))
                           vers)
                        (,(format "Translated Keymap (%s; %s)" ergoemacs-keyboard-layout (or (ergoemacs-theme--get-version) ergoemacs-mode-version))
                         ,(ergoemacs-component-struct--get comp ergoemacs-keyboard-layout))
                        ,@(mapcar
                           (lambda(ver)
                             (unless (string= ver (ergoemacs-theme--get-version))
                               `(,(format "Translated keymap for Version %s" ver)
                                 ,(ergoemacs-component-struct--get (ergoemacs-component-struct--lookup-hash component ver) ergoemacs-keyboard-layout))))
                           vers)
                        )))
          
          (dolist (elt lst)
            (unless (or (not elt) (ergoemacs (nth 1 elt) :empty-p))
              (insert "\n")
              (insert (nth 0 elt))
              (insert ":\n")
              (insert (make-string 78 ?-))
              (ergoemacs-key-description--keymap (nth 1 elt) t)
              (insert "\n")))
          (with-current-buffer standard-output
            ;; Return the text we displayed.
            (buffer-string)))))))

(defalias 'describe-ergoemacs-component 'ergoemacs-component-describe)

(defun ergoemacs-component--diminish-on (plist &optional dim type)
  "Apply `diminish' to PLIST for theme component.

The :dimininish tag can be of the form:

- t -- Removes the package name (PLIST :package-name) from the
  minor mode display list.

- minor-mode symbol -- Removes the symbol from the minor mode
  display list.

- string -- Replace the minor mode symbol with a string.

- (string1 string2) -- Unicode (string1) and terminal (string2)
  displays.  The display is determined by
  `ergoemacs-key-description--unicode-char'.

- (minor-mode-symbol) -- Suppress minor mode symbol

- (minor-mode-symbol string) -- Replace minor mode symbol
  modeline indicator with string

- (minor-mode-symbol string1 string2) -- Replace
  minor-mode-symbol indicator with unicode (string1) or
  terminal (string2) indicators.  The display is determined by
  `ergoemacs-key-description--unicode-char'.

- List of minor mode symbols, or list specifications that include
  the minor- mode symbol, so that multiple minor modes may be
   processed by a single :diminish specifciation.

DIM is the replacement for the PLIST :diminish, this is used in
recursive calls to `ergoemacs-component--diminish-on' to process
lists.  It can also be the symbol name of the package.
uu
When TYPE is non-nil, the function turns off the diminish
modifications with `diminish-undo'"
  (ignore-errors (ergoemacs-component-struct--ensure 'diminish))
  (require 'diminish nil t)
  (if (not (featurep 'diminish))
      (message "Error installing diminish package.")
    (let ((diminish-symbol (or (plist-get plist :package-name)
                               (plist-get plist :name)
                               plist))
          (dim (or dim (plist-get plist :diminish))))
      (when (and diminish-symbol (stringp diminish-symbol))
        (setq diminish-symbol (intern diminish-symbol)))

      (cond
       ((not dim))
       ;; :diminish t
       ((eq t dim)
        (cond
         (type (diminish-undo diminish-symbol))
         ((ignore-errors (and (commandp diminish-symbol t) (not (ergoemacs-autoloadp diminish-symbol))
                              (diminish diminish-symbol)))
          )
         (t (eval-after-load diminish-symbol
              `(diminish ',diminish-symbol)))))
       ;; :diminish mode
       ((symbolp dim)
        (cond
         (type (diminish-undo dim))
         ((ignore-errors (and (commandp diminish-symbol t) (not (ergoemacs-autoloadp diminish-symbol))
                              (diminish dim))))
         (t (eval-after-load diminish-symbol
              `(diminish ',dim)))))

       ;; :diminish " g"
       ((stringp dim)
        (cond
         (type (diminish-undo diminish-symbol))
         ((ignore-errors (and (commandp diminish-symbol t) (not (ergoemacs-autoloadp diminish-symbol))
                              (diminish diminish-symbol dim))))
         (t (eval-after-load diminish-symbol
              `(diminish ',diminish-symbol ,dim)))))
       ((and (consp dim)
             (= 1 (length dim))
             (symbolp (nth 0 dim)))
        (cond
         (type (diminish-undo (nth 0 dim)))
         ((ignore-errors (and (commandp (nth 0 dim) t) (not (ergoemacs-autoloadp (nth 0 dim)))
                              (diminish (nth 0 dim) dim))))
         (t (eval-after-load diminish-symbol
              `(diminish ',(nth 0 dim))))))
       
       ;; :diminish (" " " g")
       ((and (consp dim)
             (= 2 (length dim))
             (stringp (nth 0 dim))
             (stringp (nth 1 dim)))
        (cond
         (type (diminish-undo diminish-symbol))
         ((ignore-errors (and (commandp diminish-symbol t) (not (ergoemacs-autoloadp diminish-symbol))
                              (diminish diminish-symbol (ergoemacs :unicode (nth 0 dim) (nth 1 dim))))))
         (t (eval-after-load diminish-symbol
              `(ignore-errors
                 (diminish ',diminish-symbol
                           ,(ergoemacs :unicode (nth 0 dim) (nth 1 dim))))))))
       ;;:diminish (" 🌈" " ⓡ" " r")
       ((and (consp dim)
             (= 3 (length dim))
             (stringp (nth 0 dim))
             (stringp (nth 1 dim))
             (stringp (nth 3 dim)))
        (cond
         (type (diminish-undo diminish-symbol))
         ((ignore-errors (and (commandp diminish-symbol t) (not (ergoemacs-autoloadp diminish-symbol))
                              (diminish diminish-symbol (ergoemacs :unicode (nth 0 dim) (ergoemacs :unicode (nth 1 dim) (nth 2 dim)))))))
         (t (eval-after-load diminish-symbol
              `(ignore-errors
                 (diminish ',diminish-symbol
                           ,(ergoemacs :unicode (nth 0 dim) (ergoemacs :unicode (nth 1 dim) (nth 2 dim)))))))))
       ;; :diminish (mode " " " g")
       ((and (consp dim)
             (= 3 (length dim))
             (symbolp (nth 0 dim))
             (stringp (nth 1 dim))
             (stringp (nth 2 dim)))
        (cond
         (type (diminish-undo (nth 0 dim)))
         ((ignore-errors (and (commandp (nth 0 dim) t) (not (ergoemacs-autoloadp (nth 0 dim)))
                              (diminish (nth 0 dim) (ergoemacs :unicode (nth 1 dim) (nth 2 dim))))))
         (t (eval-after-load diminish-symbol
              `(ignore-errors
                 (diminish ',(nth 0 dim)
                           ,(ergoemacs :unicode (nth 1 dim) (nth 2 dim))))))))
       ;; :diminish (rainbow-mode " 🌈" " ⓡ" " r")
       ((and (consp dim)
             (= 4 (length dim))
             (symbolp (nth 0 dim))
             (stringp (nth 1 dim))
             (stringp (nth 2 dim))
             (stringp (nth 3 dim)))
        (cond
         (type (diminish-undo (nth 0 dim)))
         ((ignore-errors (and (commandp (nth 0 dim) t) (not (ergoemacs-autoloadp (nth 0 dim)))
                              (diminish (nth 0 dim) (ergoemacs :unicode (nth 1 dim) (ergoemacs :unicode (nth 2 dim) (nth 3 dim)))))))
         (t (eval-after-load diminish-symbol
              `(ignore-errors
                 (diminish ',(nth 0 dim)
                           ,(ergoemacs :unicode (nth 1 dim) (ergoemacs :unicode (nth 2 dim) (nth 3 dim)))))))))
       ;; :diminish (mode " ")
       ((and (consp dim)
             (= 2 (length dim))
             (symbolp (nth 0 dim))
             (stringp (nth 1 dim)))
        (cond
         (type (diminish-undo (nth 0 dim)))
         ((ignore-errors (and (commandp (nth 0 dim) t) (not (ergoemacs-autoloadp (nth 0 dim)))
                              (diminish (nth 0 dim) (nth 1 dim)))))
         (t (eval-after-load diminish-symbol
              `(diminish ',(nth 0 dim) ,(nth 1 dim))))))
       ((consp dim)
        (dolist (elt dim)
          (ergoemacs-component--diminish-on plist elt type)))))))

(defun ergoemacs-component--diminish-off (plist)
  "Remove `diminish' top PLIST for theme component.
Wrapper for `ergoemacs-component--diminish-on'."
  (ergoemacs-component--diminish-on plist nil t))

(provide 'ergoemacs-component)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-component.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
