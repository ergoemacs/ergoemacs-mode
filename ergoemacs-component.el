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

(require 'help-mode)
(require 'find-func)

(defvar ergoemacs--last-start-emacs-state-2)
(defvar ergoemacs--start-emacs-state-2)
(defvar ergoemacs-component-hash)
(defvar ergoemacs-display-key-use-face-p)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-map-properties--unlabeled)
(defvar ergoemacs-mode--fast-p)
(defvar ergoemacs-mode-version)
(defvar ergoemacs-theme-comp-hash)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-translate--translation-hash)
(defvar ergoemacs-translation-hash)
(defvar package--initialized)

(declare-function ergoemacs--emacs-state "ergoemacs-mode")
(declare-function ergoemacs-mode--setup-hash-tables--setq "ergoemacs-mode")
(declare-function ergoemacs-mode-clear-cache "ergoemacs-mode")

(declare-function ergoemacs-set "ergoemacs-lib")
(declare-function ergoemacs-reset "ergoemacs-lib")

(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")

(declare-function ergoemacs-translate "ergoemacs-translate")

(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-regexp "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--empty-p "ergoemacs-map-properties")

(declare-function ergoemacs-debug "ergoemacs-debug")
(declare-function ergoemacs-debug-heading "ergoemacs-debug")
(declare-function ergoemacs-debug-keymap "ergoemacs-debug")

(declare-function ergoemacs-theme--get-version "ergoemacs-theme-engine")

(declare-function ergoemacs-map-- "ergoemacs-map")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--keymap "ergoemacs-key-description")

(declare-function package-installed-p "package")


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
  (package-name nil)
  (autoloads nil)
  (ensure nil)
  (layout "us" :read-only t)
  (calculated-layouts (make-hash-table :test 'equal))
  (relative-to 'global-map)
  (defer nil))

(defvar ergoemacs-component-struct--define-key-current nil)

(defvar ergoemacs-component-struct--ensure-refreshed-p nil)
(defun ergoemacs-component-struct--ensure (package)
  "Ensure PACKAGE is installed."
  (when package
    (let ((package (or (and (symbolp package) package)
                       (and (stringp package) (intern package)))))
      (unless (featurep package)
        (require package nil t))
      (unless (featurep package)
        (unless package--initialized
          (package-initialize))
        (if (package-installed-p package) t
          (unless ergoemacs-component-struct--ensure-refreshed-p
            (package-refresh-contents)
            (setq ergoemacs-component-struct--ensure-refreshed-p t))
          (unless (progn (ignore-errors (package-install package))
                         (package-installed-p package))
            (warn "ergoemacs-mode could not install %s." package)))))))


(defun ergoemacs-component-struct--handle-bind (bind &optional keymap)
  "Handle :bind and related keywords."
  (let ((keymap (or keymap 'global-map))
        key def)
    (cond
     ((ignore-errors (and (consp bind) (> (length bind) 2)
                          (setq key (pop bind))
                          (setq def (pop bind))))
      (while (and key def)
        (when (stringp key)
          (ergoemacs-component-struct--define-key keymap (read-kbd-macro key) def))
        (setq key (pop bind)
              def (pop bind))))
     ((and (consp bind) (stringp (car bind)))
      ;; :bind ("C-." . ace-jump-mode)
      (ergoemacs-component-struct--define-key keymap (read-kbd-macro (car bind)) (cdr bind)))
     ((and (consp bind) (consp (car bind)))
      (dolist (elt bind)
        (when (and (consp elt) (stringp (car elt)))
          (ergoemacs-component-struct--define-key keymap (read-kbd-macro (car elt)) (cdr elt))))))))

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
          (error "OBJECT is not an ergoemacs-component-structure.")
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
         (or (not (nth 1 def))
             (ergoemacs-gethash (nth 1 def) ergoemacs-translation-hash)))
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
          (key (or (and (consp key) (memq (car key) '(kbd read-kbd-macro))
                        (stringp (nth 1 key)) (read-kbd-macro (nth 1 key)))
                   key))
          (def (ergoemacs-component-struct--define-key-get-def def)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT not a ergoemacs-component-structure.")
        ;; Change apps <-> menu
        (setq key (vconcat key)
              key (vconcat
                   (mapcar
                    (lambda(x)
                      (cond
                       ((and (eq system-type 'windows-nt) (eq x 'menu))
                        'apps)
                       ((and (not (eq system-type 'windows-nt)) (eq x 'apps))
                        'menu)
                       (t x))) key)))
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
            (pushnew keymap ergoemacs-map-properties--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-component-struct-maps obj)))
           ((and (not cur-map) when-condition global-map-p)
            (setq cur-map (make-sparse-keymap))
            (puthash when-condition cur-map (ergoemacs-component-struct-cond-maps obj)))
           ((and (not cur-map) when-condition hook)
            (unless (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj))
              (puthash hook (make-hash-table) (ergoemacs-component-struct-hook-maps obj)))
            (pushnew keymap ergoemacs-map-properties--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-gethash hook (ergoemacs-component-struct-hook-maps obj)))))
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
            (define-key cur-map key def)
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
  "Looks up closest component version from `ergoemacs-component-hash'.
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

This takes into consideration any versions defined, and the
closest `ergoemacs-theme-version' calculated from
`ergoemacs-component-struct--closest-version' by using `ergoemacs-component-struct--lookup-closest'"
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
        (ergoemacs-component-struct--lookup-closest ret version)))))

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
            (setq extra-map (ergoemacs-gethash map-name extra-hash))
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
     ((setq ret (ergoemacs-gethash (list lookup-key (intern cur-layout)) hash))
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
      (when (and (setq tmp (ergoemacs-component-struct-hook-maps obj))
                 (hash-table-p tmp))
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

(defun ergoemacs-component-struct--deferred (what &optional object)
  "Setup deferred initilizations."
  (cond
   ((and (not ergoemacs-component-struct--define-key-current) (not object)) ;; Old
    (error "`ergoemacs-component-struct--deferred' is confused."))
   (t
    (let ((obj (or object ergoemacs-component-struct--define-key-current)))
      (if (not (ergoemacs-component-struct-p obj))
          (error "OBJECT is not an ergoemacs-component-structure.")
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
      (ergoemacs-component-struct-variables obj))
     (t (ergoemacs-component-struct--variables (ergoemacs-component-struct--lookup-hash obj))))))

(defvar ergoemacs-component-struct--refresh-variables nil)
(defvar ergoemacs-component-struct--applied-inits '())
(defvar ergoemacs-component-struct--deferred-functions '())

(defvar ergoemacs-component-struct--apply-inits-first-p t)
(defun ergoemacs-component-struct--apply-inits (&optional _file obj)
  "Apply the initializations from the OBJ."
  (when (eq ergoemacs-component-struct--refresh-variables t)
    (setq ergoemacs-component-struct--refresh-variables ergoemacs-component-struct--applied-inits))
  (let* ((obj (or obj (ergoemacs-theme-components)))
         package-name ensure defer comp plist)
    (when ergoemacs-component-struct--apply-inits-first-p
      (setq ergoemacs-component-struct--apply-inits-first-p nil)
      (when ergoemacs-mode--fast-p
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
            (warn "ergoemacs-mode cache reset AFTER loading; Keys may be slightly inconsistent until emacs restart."))))
      (dolist (elt obj)
        (setq comp (ergoemacs-component-struct--lookup-hash elt)
              package-name (ergoemacs-component-struct-package-name comp)
              ensure (ergoemacs-component-struct-ensure comp)
              plist (ergoemacs-component-struct-plist comp)
              defer (ergoemacs-component-struct-defer comp))
        (cond
         ((eq ensure t)
          (ergoemacs-component-struct--ensure package-name))
         ((and ensure (symbolp ensure))
          (ergoemacs-component-struct--ensure ensure))
         ((consp ensure)
          (dolist (elt ensure)
            (cond
             ((and elt (symbolp elt))
              (ergoemacs-component-struct--ensure elt))
             ((stringp elt)
              (ergoemacs-component-struct--ensure (intern elt))))))
         ((stringp ensure)
          (ergoemacs-component-struct--ensure (intern ensure)))))
      ;; Load non-deferred packages.
      (when package-name
        (unless (or defer (plist-get plist :no-require) (plist-get plist :no-load))
          (load (format "%s" package-name)))
        (when (numberp defer)
          (run-with-idle-timer defer nil #'load (format "%s" package-name)))))
    (dolist (init (ergoemacs-component-struct--variables obj))
      (if (and (consp (nth 0 init)) (not (nth 1 init)) (not (nth 2 init)))
          (unless (member (nth 0 init) ergoemacs-component-struct--deferred-functions)
            (when (ignore-errors (fboundp (car (nth 0 init))))
              (apply (car (nth 0 init)) (cdr (nth 0 init)))
              (push (nth 0 init) ergoemacs-component-struct--deferred-functions)))
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
            (ergoemacs-set (nth 0 init) (funcall (nth 1 init)))))))))
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

(defun ergoemacs-component--regexp (&optional at-end)
  "Returns a regexp of `ergoemacs-mode' components."
  (let (ret)
    (maphash
     (lambda(key _item) (push key ret))
     ergoemacs-theme-comp-hash)
    (setq ret (regexp-opt ret 'symbols))
    (when at-end
      (setq ret (concat ret "$")))
    ret))


(defun ergoemacs-component--help-link-1 ()
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
  "Links `ergoemacs-mode' components in help-mode buffer."
  (when (eq major-mode 'help-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (ree (format "^ - %s -- " (ergoemacs-component--regexp)))
            (re (ergoemacs-component--regexp t))
            (rem (ergoemacs-map-properties--map-regexp t))
            tmp)
        (with-syntax-table emacs-lisp-mode-syntax-table
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
          (when (re-search-forward "^This theme is based on: *\\(\\_<.(\\_>\\)" nil t)
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
            (ergoemacs-component--help-link-1)))))))

(define-button-type 'ergoemacs-component-help
  :supertype 'help-xref
  'help-function #'ergoemacs-component-describe
  'help-echo (purecopy "mouse-2, RET: describe this ergoemacs component"))

(define-button-type 'ergoemacs-component-def
  :supertype 'help-xref
  'help-function #'ergoemacs-component-find-definition
  'help-echo (purecopy "mouse-2, RET: find this ergoemacs component's definition"))


(defcustom ergoemacs-component-find-regexp
  (concat"^\\s-*(ergoemacs-\\(?:theme-?\\)?\\(?:component\\)?" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used by `ergoemacs-find-component' to search for a component definition.
Note it must contain a `%s' at the place where `format'
should insert the face name."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(unless (assoc 'ergoemacs-component find-function-regexp-alist)
  (push (cons 'ergoemacs-component 'ergoemacs-component-find-regexp) find-function-regexp-alist))

(defun ergoemacs-component-find-no-select (component &optional type)
  "Find COMPONENT"
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

(defun ergoemacs-component-find-1 (symbol type switch-fn)
  "Find `ergoemacs-mode' component or theme
TYPE is nil to search for a component definition,

The variable `find-function-recenter-line' controls how
to recenter the display.  SWITCH-FN is the function to call
to display and select the buffer.
See also `find-function-after-hook'.

Modified from `find-definition-noselect'.

Set mark before moving, if the buffer already existed."
  (let* ((orig-point (point))
         (orig-buffers (buffer-list))
         (buffer-point (save-excursion
                         (ergoemacs-component-find-no-select symbol type)))
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
Return 0 if there is no such symbol. Based on `variable-at-point'"
  (let ((hash-table (or (and theme-instead ergoemacs-theme-hash)
                        ergoemacs-theme-comp-hash)))
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
                       (gethash (symbol-name obj) hash-table) obj)))
            (error nil))
          (let* ((str (find-tag-default))
                 (sym (if str (intern str))))
            (if (and sym (gethash (symbol-name sym) hash-table))
                sym
              (save-match-data
                (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                  (setq sym (intern (match-string 1 str)))
                  (and (gethash (symbol-name sym) hash-table) sym)))))
          0))))

(defun ergoemacs-component--prompt (&optional theme-instead)
  "Prompt for component or theme (when THEME-INSTEAD is non-nil)."
  (let ((c (ergoemacs-component-at-point theme-instead))
        (enable-recursive-minibuffers t)
        val)
    (setq val (completing-read (if (symbolp c)
                                   (format
                                    "Describe ergoemacs %s (default %s): "
                                    (or (and theme-instead "theme") "component")
                                    c)
                                 (format
                                  "Describe ergoemacs %s: "
                                  (or (and theme-instead "theme") "component")))
                               (or (and theme-instead ergoemacs-theme-hash)
                                   ergoemacs-theme-comp-hash)
                               nil
                               ;; (lambda (vv)
                               ;;   (or (get vv 'variable-documentation)
                               ;;       (and (boundp vv) (not (keywordp vv)))))
                               t nil nil
                               (format "%s" c)))
    (list (or (and (equal val "") (format "%s" c)) val))))

(defun ergoemacs-component-describe (component)
  "Display the full documentation of COMPONENT (a symbol or string)."
  (interactive (ergoemacs-component--prompt))
  (let* ((component (and component
                         (or (and (stringp component) component)
                             (and (symbolp component) (symbol-name component)))))
         (comp (ergoemacs-component-struct--lookup-hash (or component "")))
         (plist (ergoemacs-component-struct-plist comp))
         (file (plist-get plist :file))
         (el-file (concat (file-name-sans-extension file) ".el"))
         tmp vers
         lst)
    (if (not comp)
        (message "You did not specify a valid ergoemacs component %s" component)
      (help-setup-xref (list #'ergoemacs-component-describe (or component ""))
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ (or component ""))
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is an `ergoemacs-mode' component")
        (when (file-readable-p el-file)
          (princ " defined in `")
          (princ (file-name-nondirectory el-file))
          (princ "'."))
        (princ "\n\n")
        (princ "Documentation:\n")
        (princ (plist-get (ergoemacs-component-struct-plist comp) :description))
        (princ "\n\n")
        (princ (format "Base Layout: %s\n" (ergoemacs-component-struct-layout comp)))
        (princ (format "Relative To: %s\n" (ergoemacs-component-struct-relative-to comp)))
        (princ (format "Variable Modifiers: %s\n" (ergoemacs-component-struct-variable-modifiers comp)))
        (princ (format "Variable Prefixes: %s\n"
                       (mapconcat
                        (lambda(x) (ergoemacs-key-description x))
                        (ergoemacs-component-struct-variable-prefixes comp) ", ")))

        (when (setq tmp (ergoemacs-component-struct-unbind comp))
          (princ (format "Unbound keys: %s\n"
                         (mapconcat
                          (lambda(x) (ergoemacs-key-description x)) tmp ", "))))

        (when (setq tmp (ergoemacs-component-struct-undefined comp))
          (princ (format "Masked emacs keys: %s\n"
                         (mapconcat
                          (lambda(x) (ergoemacs-key-description x)) tmp ", "))))

        ;; FIXME: Describe major-mode / minor-mode differences
        
        ;; FIXME: Describe what keys are deferred, and what they would
        ;; possibly bind to...

        (if (not (setq vers (ergoemacs-component-struct-versions comp)))
            (setq lst `(("Specified Keymap" ,(ergoemacs-component-struct-map comp))
                        (,(format "Translated Keymap (%s)" ergoemacs-keyboard-layout) ,(ergoemacs-component-struct--get comp ergoemacs-keyboard-layout))))
          (princ (format "Versions: %s, %s\n" ergoemacs-mode-version
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
            (princ "\n")
            (princ (nth 0 elt))
            (princ ":\n")
            (princ (make-string 78 ?-))
            (princ (ergoemacs-key-description--keymap (nth 1 elt)))
            (princ "\n")))
        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string))))))

(defalias 'describe-ergoemacs-component 'ergoemacs-component-describe)

(provide 'ergoemacs-component)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-component.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
