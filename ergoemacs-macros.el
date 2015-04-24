;;; ergoemacs-macros.el --- Macros for ergoemacs-mode -*- lexical-binding: t -*-

;; Copyright © 2013, 2014  Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Keywords: convenience

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Todo:

;; 

;;; Code:

;; These should only be called when byte compiled

;;;###autoload
(defmacro ergoemacs-keymapp (keymap)
  "Error free check of keymap by `keymapp'"
  `(ignore-errors (keymapp ,keymap)))

;;;###autoload
(defmacro ergoemacs-sv (symbol &optional default)
  "Error free `symbol-value'.
If SYMBOL is void, return nil"
  `(if ,default
       (ignore-errors (default-value ,symbol))
     (ignore-errors (symbol-value ,symbol))))

;; This shouldn't be called at run-time; This fixes the byte-compile warning.
(fset 'ergoemacs-theme-component--parse
      #'(lambda(keys-and-body &optional skip-first)
          "Parse KEYS-AND-BODY, optionally skipping the name and
documentation with SKIP-FIRST.

Uses `ergoemacs-theme-component--parse-keys-and-body' and
  `ergoemacs-theme-component--parse-remaining'."
          (ergoemacs-theme-component--parse-keys-and-body
           keys-and-body
           'ergoemacs-theme-component--parse-remaining
           skip-first)))

;;;###autoload
(defun ergoemacs-theme-component--parse-remaining (remaining)
  "In parsing, this function converts
- `define-key' is converted to `ergoemacs-component-struct--define-key' and keymaps are quoted
- `global-set-key' is converted to `ergoemacs-component-struct--define-key' with keymap equal to `global-map'
- `global-unset-key' is converted to `ergoemacs-component-struct--define-key' with keymap equal to `global-map' and function definition is `nil'
- `global-reset-key' is converted `ergoemacs-component-struct--define-key'
- `setq' and `set' is converted to `ergoemacs-component-struct--set'
- `add-hook' and `remove-hook' is converted to `ergoemacs-component-struct--set'
- Mode initialization like (delete-selection-mode 1)
  or (delete-selection) is converted to
  `ergoemacs-component-struct--set'
- Allows :version statement expansion to `ergoemacs-component-struct--new-version'
- Adds with-hook syntax or (when -hook) or (when -mode) using `ergoemacs-component-struct--with-hook'
"
  (let* ((last-was-version nil)
         (remaining
          (mapcar
           (lambda(elt)
             (cond
              (last-was-version
               (setq last-was-version nil)
               (if (stringp elt)
                   `(ergoemacs-component-struct--new-version ,elt)
                 `(ergoemacs-component-struct--new-version ,(symbol-name elt))))
              ((ignore-errors (eq elt ':version))
               (setq last-was-version t)
               nil)
              ((ignore-errors (eq (nth 0 elt) 'global-reset-key))
               `(ergoemacs-component-struct--define-key 'global-map ,(nth 1 elt) nil))
              ((ignore-errors (eq (nth 0 elt) 'global-unset-key))
               `(ergoemacs-component-struct--define-key 'global-map ,(nth 1 elt) nil))
              ((ignore-errors (eq (nth 0 elt) 'set))
               ;; Currently doesn't support (setq a b c d ), but it should.
               `(ergoemacs-component-struct--set ,(nth 1 elt) '(lambda() ,(nth 2 elt))))
              ((ignore-errors (eq (nth 0 elt) 'add-hook))
               `(ergoemacs-component-struct--set ,(nth 1 elt) ,(nth 2 elt)
                               (list t ,(nth 3 elt) ,(nth 4 elt))))
              ((ignore-errors (eq (nth 0 elt) 'remove-hook))
               `(ergoemacs-component-struct--set ,(nth 1 elt) ,(nth 2 elt)
                               (list nil nil ,(nth 3 elt))))
              ((ignore-errors (eq (nth 0 elt) 'setq))
               (let ((tmp-elt elt)
                     (ret '()))
                 (pop tmp-elt)
                 (while (and (= 0 (mod (length tmp-elt) 2)) (< 0 (length tmp-elt)))
                   (push `(ergoemacs-component-struct--set (quote ,(pop tmp-elt)) '(lambda() ,(pop tmp-elt))) ret))
                 (push 'progn ret)
                 ret))
              ((ignore-errors (string-match "-mode$" (symbol-name (nth 0 elt))))
               `(ergoemacs-component-struct--set (quote ,(nth 0 elt)) '(lambda() ,(nth 1 elt))))
              ((ignore-errors (eq (nth 0 elt) 'global-set-key))
               (if (ergoemacs-keymapp (ergoemacs-sv (nth 2 elt))) 
                   `(ergoemacs-component-struct--define-key 'global-map ,(nth 1 elt) (quote ,(nth 2 elt)))
                 `(ergoemacs-component-struct--define-key 'global-map ,(nth 1 elt) ,(nth 2 elt))))
              ((ignore-errors (eq (nth 0 elt) 'define-key))
               (if (equal (nth 1 elt) '(current-global-map))
                   (if (ergoemacs-keymapp (ergoemacs-sv (nth 3 elt))) 
                       `(ergoemacs-component-struct--define-key 'global-map ,(nth 2 elt) (quote ,(nth 3 elt)))
                     `(ergoemacs-component-struct--define-key 'global-map ,(nth 2 elt) ,(nth 3 elt)))
                 (if (ergoemacs-keymapp (ergoemacs-sv (nth 3 elt))) 
                     `(ergoemacs-component-struct--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) (quote ,(nth 3 elt)))
                   `(ergoemacs-component-struct--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) ,(nth 3 elt)) )))
              ((or (ignore-errors (eq (nth 0 elt) 'with-hook))
                   (and (ignore-errors (eq (nth 0 elt) 'when))
                        (ignore-errors (string-match "\\(-hook\\|-mode\\|^mark-active\\)$" (symbol-name (nth 1 elt))))))
               (let ((tmp (ergoemacs-theme-component--parse (cdr (cdr elt)) t)))
                 `(ergoemacs-component-struct--with-hook
                   ',(nth 1 elt) ',(nth 0 tmp)
                   '(lambda () ,@(nth 1 tmp)))))
              ((ignore-errors (memq (nth 0 elt) '(mapcar mapc dolist when if)))
               (macroexpand-all (ergoemacs-theme-component--parse-remaining elt)))
              (t elt)))
           remaining)))
    remaining))

;;;###autoload
(defmacro ergoemacs-component (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (macroexpand-all `(ergoemacs-theme-component ,@body-and-plist)))

;;;###autoload
(defmacro ergoemacs-theme-component (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs-theme-component--parse body-and-plist))
    `(progn
       (unless (boundp 'ergoemacs-component-hash)
         (defvar ergoemacs-component-hash (make-hash-table :test 'equal)
           "Hash of ergoemacs theme components"))
       (puthash ,(plist-get (nth 0 kb) ':name)
                (lambda() ,(plist-get (nth 0 kb) ':description)
                  (ergoemacs-theme-component--create-component
                   ',(nth 0 kb)
                   '(lambda () ,@(nth 1 kb)))) ergoemacs-theme-comp-hash)
       (puthash ,(plist-get (nth 0 kb) ':name)
                (lambda() ,(plist-get (nth 0 kb) ':description)
                  (ergoemacs-component-struct--create-component
                   ',(nth 0 kb)
                   '(lambda () ,@(nth 1 kb)))) ergoemacs-component-hash)
       ,(when (plist-get (nth 0 kb) ':require)
          `(ergoemacs-require ',(intern (plist-get (nth 0 kb) ':name)))))))

(defmacro ergoemacs-package (name &rest keys-and-body)
  "Defines a required package named NAME.
Maybe be similar to use-package"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist"))
        (plist (make-symbol "plist"))
        (body (make-symbol "body"))
        (doc (make-symbol "doc")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body keys-and-body  nil t)
          plist (nth 0 kb)
          body (nth 1 kb))
    (when (equal (car body) '())
      (setq body (cdr body)))
    (setq doc (if (stringp (car body)) (pop body) (symbol-name name)))
    (unless (plist-get plist ':require) ;; Its a required theme component.
      (setq plist (plist-put plist ':require name)))
    (macroexpand-all
     `(ergoemacs-theme-component ,name ()
        ,doc
        ,@plist ,@body))))

;;;###autoload
(defmacro ergoemacs-test-layout (&rest keys-and-body)
  (let ((kb (make-symbol "body-and-plist"))
        (plist (make-symbol "plist"))
        (body (make-symbol "body")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body keys-and-body  nil t)
          plist (nth 0 kb)
          body (nth 1 kb))
    `(let ((old-ergoemacs-theme (or ergoemacs-theme "standard"))
           (old-version (ergoemacs-theme-get-version))
           (macro
            ,(if (plist-get plist ':macro)
                 `(edmacro-parse-keys ,(plist-get plist ':macro) t)))
           (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout))
       (setq ergoemacs-theme ,(plist-get plist ':theme))
       (setq ergoemacs-keyboard-layout ,(or (plist-get plist ':layout) "us"))
       (ergoemacs-theme-set-version ,(or (plist-get plist ':version) nil))
       (ergoemacs-theme-reset)
       ,(if (plist-get plist :cua)
            `(cua-mode 1))
       (unwind-protect
           (progn
             ,@body)
         (setq ergoemacs-theme old-ergoemacs-theme)
         (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
         (ergoemacs-theme-set-version old-version)
         (ergoemacs-theme-reset)))))

(fset 'ergoemacs-theme-component--parse-keys-and-body
      #'(lambda (keys-and-body &optional parse-function  skip-first)
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
                plist
                (remaining keys-and-body))
            ;; Allow
            ;; (component name)
            (unless (or (keywordp (first remaining)) skip-first)
              (if (condition-case nil
                      (stringp (first remaining))
                    (error nil))
                  (push (cons ':name (pop remaining)) extracted-key-accu)
                (push (cons ':name  (symbol-name (pop remaining))) extracted-key-accu))
              (when (memq (type-of (first remaining)) '(symbol cons))
                (setq remaining (cdr remaining)))
              (when (stringp (first remaining))
                (push (cons ':description (pop remaining)) extracted-key-accu)))
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
            (list plist remaining))))

;;;###autoload
(defmacro ergoemacs-theme (&rest body-and-plist)
  "Define an ergoemacs-theme.
:components -- list of components that this theme uses. These can't be seen or toggled
:optional-on -- list of components that are optional and are on by default
:optional-off -- list of components that are optional and off by default
:options-menu -- Menu options list
:silent -- If this theme is \"silent\", i.e. doesn't show up in the Themes menu.

:based-on

The rest of the body is an `ergoemacs-theme-component' named THEME-NAME-theme
"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist"))
        (tmp (make-symbol "tmp"))
        (based-on (make-symbol "based-on")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body body-and-plist))
    (setq tmp (eval (plist-get (nth 0 kb) :components)))
    (push (intern (concat (plist-get (nth 0 kb) :name) "-theme")) tmp)
    (setq tmp (plist-put (nth 0 kb) :components tmp))
    (setq based-on (plist-get (nth 0 kb) :based-on))
    ;; (message "First Based-On: %s" based-on)
    (setq based-on (or (and (stringp based-on) based-on)
                       (and (symbolp based-on) (symbol-name based-on))
                       (and (eq (car based-on) 'quote) (symbol-name (car (cdr based-on))))
                       nil))
    ;; (message "Last Based-On: %s" based-on)
    (dolist (comp '(:optional-on :optional-off :options-menu))
      (setq tmp (plist-put (nth 0 kb) comp
                           (eval (plist-get (nth 0 kb) comp)))))
    
    `(let* ((based-on (gethash ,based-on ergoemacs-theme-hash))
            (curr-plist ',tmp)
            (opt-on (plist-get curr-plist ':optional-on))
            (opt-off (plist-get curr-plist ':optional-off))
            (comp (plist-get curr-plist ':components))
            (themes (gethash "defined-themes" ergoemacs-theme-hash))
            (silent (gethash "silent-themes" ergoemacs-theme-hash))
            (included (append opt-on opt-off comp)))
       (push ,(plist-get (nth 0 kb) ':name) themes)
       (push ,(plist-get (nth 0 kb) ':name) silent)
       (if (not based-on)
           (puthash ,(plist-get (nth 0 kb) ':name) curr-plist ergoemacs-theme-hash)
         (dolist (type '(:optional-on :optional-off :components))
           (dolist (comp (plist-get based-on type))
             (unless (memq comp included)
               (setq curr-plist
                     (plist-put curr-plist type
                                (append (plist-get curr-plist type)
                                        (list comp)))))))
         (when (and (not (plist-get curr-plist ':options-menu))
                    (plist-get based-on ':options-menu))
           (setq curr-plist
                 (plist-put curr-plist ':options-menu
                            (plist-get based-on ':options-menu))))
         (puthash ,(plist-get (nth 0 kb) ':name) curr-plist
                  ergoemacs-theme-hash))
       (if ,(plist-get (nth 0 kb) ':silent)
           (puthash "silent-themes" silent ergoemacs-theme-hash)
         (puthash "defined-themes" themes ergoemacs-theme-hash))
       (ergoemacs-theme-component ,(intern (concat (plist-get (nth 0 kb) ':name) "-theme")) ()
         ,(format "Generated theme component for %s theme" (concat (plist-get (nth 0 kb) ':name) "-theme"))
         ,@(nth 1 kb)))))

;;;###autoload
(defmacro ergoemacs-deftheme (name _desc based-on &rest differences)
  "Creates a theme layout for Ergoemacs keybindings -- Compatability layer.

NAME is the theme name.
_DESC is the theme description and is currently ignored.
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

;;;###autoload
(defmacro ergoemacs-save-buffer-state (&rest body)
  "Eval BODY,
then restore the buffer state under the assumption that no significant
modification has been made in BODY.  A change is considered
significant if it affects the buffer text in any way that isn't
completely restored again.  Changes in text properties like `face' or
`syntax-table' are considered insignificant.  This macro allows text
properties to be changed, even in a read-only buffer.

This macro should be placed around all calculations which set
\"insignificant\" text properties in a buffer, even when the buffer is
known to be writeable.  That way, these text properties remain set
even if the user undoes the command which set them.

This macro should ALWAYS be placed around \"temporary\" internal buffer
changes \(like adding a newline to calculate a text-property then
deleting it again\), so that the user never sees them on his
`buffer-undo-list'.  

However, any user-visible changes to the buffer \(like auto-newlines\)
must not be within a `ergoemacs-save-buffer-state', since the user then
wouldn't be able to undo them.

The return value is the value of the last form in BODY.

This was stole/modified from `c-save-buffer-state'"
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
          (inhibit-read-only t) (inhibit-point-motion-hooks t)
          before-change-functions after-change-functions
          deactivate-mark
          buffer-file-name buffer-file-truename ; Prevent primitives checking
                                        ; for file modification
          )
     (unwind-protect
         (progn ,@body)
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

;;;###autoload
(defmacro ergoemacs (&optional keymap property set-value)
  "Get/Set keymaps and `ergoemacs-mode' properties

When KEYMAP can be a property.  The following properties are supported:
- :layout - returns the current (or specified by PROPERTY) keyboard layout.
- :remap - Use `ergoemacs-mode' to remap to an appropriate function.
- :md5 -- returns an md5 of the currently enabled `ergoemacs-mode' options.
- :theme-debug -- Debugs the theme by calling `ergoemacs-component--checkout'
- :original -- Returns the original keymap
- :map-list, :original,  :composed-p, :composed-list, :key-struct :empty-p calls ergoemacs-mat-properties-- equivalent functions.

"
  (let ((map-properties-list '(:map-list
                               :original
                               :user
                               :composed-p
                               :composed-list
                               :key-struct
                               :empty-p
                               :label
                               :current-local-map-p
                               :keys
                               :where-is
                               :lookup
                               :user-original)))
    (cond
     ((and keymap (symbolp keymap)
           (memq keymap map-properties-list))
      `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name keymap) 1))) ,property ,set-value))

     ((and keymap property (eq property :new-command) set-value)
      ;; (ergoemacs keymap :new-command 'next-line)
      `(ergoemacs-map-properties--new-command ,keymap ,set-value))
     
     ((and keymap (symbolp keymap)
           (memq keymap '(:debug-theme :theme-debug :debug)))
      `(ergoemacs-component--checkout ,property ,set-value))
     
     ((and keymap (symbolp keymap)
           (eq keymap :remap) property)
      `(call-interactively ,property))

     ((and keymap (symbolp keymap)
           (eq keymap :md5))
      `(ergoemacs-map--md5 ,property))
     
     ((and keymap (symbolp keymap)
           (eq keymap :layout))
      `(ergoemacs-layouts--current ,property))
     
     ((and keymap property (not set-value)
           (symbolp property)
           (string= ":" (substring (symbol-name property) 0 1)))
      
      ;; Get a property
      (cond
       ((eq property :full)
        `(ignore-errors (char-table-p (nth 1 (ergoemacs-map-properties--keymap-value ,keymap)))))
       ((eq property :indirect)
        (macroexpand-all `(ergoemacs-keymapp (symbol-function ,keymap))))
       ((memq property '(:map-key :key))
        ;; FIXME Expire any ids that are no longer linked??
        `(ignore-errors (plist-get (ergoemacs-map-properties--map-fixed-plist ,keymap) :map-key)))
       ((eq property :prefixes)
        `(ergoemacs-map-properties--extract-prefixes ,keymap))
       ((memq property map-properties-list)
        `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name property) 1))) ,keymap))
       (t
        `(ignore-errors (gethash ,property (gethash (ergoemacs-map-properties--key-struct (ergoemacs-map-properties--keymap-value ,keymap)) ergoemacs-map-properties--plist-hash))))))

     ((and keymap property set-value
           (symbolp property)
           (string= ":" (substring (symbol-name property) 0 1)))
      ;; Assign a property.
      `(ergoemacs-map-properties--put ,keymap ,property ,set-value))
     
     ((and (not set-value) (eq keymap 'emulation-mode-map-alists))
      `(ergoemacs-map--emulation-mode-map-alists ,property))
     
     ((and (not set-value) (eq keymap 'minor-mode-overriding-map-alist))
      `(ergoemacs-map--minor-mode-overriding-map-alist ,property))

     ((and (not set-value) (eq keymap 'minor-mode-map-alist))
      `(ergoemacs-map--minor-mode-map-alist ,property))

     (t
      `(ergoemacs-map-- ,keymap))
     )))

;;;###autoload
(defmacro ergoemacs-advice* (function args &rest body-and-plist)
  "Defines an `ergoemacs-mode' advice that replaces a function (like `define-key')"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "kb")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body `(nil nil ,@body-and-plist)))
    `(progn
       (defalias ',(intern (format "ergoemacs-advice--real-%s" (symbol-name function)))
         (symbol-function ',function) (concat ,(format "ARGS=%s\n\n" args) (documentation ',function)
                                              ,(format "\n\n`ergoemacs-mode' preserved the real `%s' in this function."
                                                       (symbol-name function))))
       (defun ,(intern (format "ergoemacs-advice--%s--" function)) ,args
         ,(format "%s\n\n`ergoemacs-mode' replacement function for `%s'.\nOriginal function is preserved in `ergoemacs-advice--real-%s'"
                  (plist-get (nth 0 kb) :description) (symbol-name function) (symbol-name function))
         ,@(nth 1 kb))
       ;; Hack to make sure the documentation is in the function...
       (defalias ',(intern (format "ergoemacs-advice--%s" function)) ',(intern (format "ergoemacs-advice--%s--" function))
         ,(format "ARGS=%s\n\n%s\n\n`ergoemacs-mode' replacement function for `%s'.\nOriginal function is preserved in `ergoemacs-advice--real-%s'"
                  args (plist-get (nth 0 kb) :description) (symbol-name function) (symbol-name function)))
       ,(if (plist-get (nth 0 kb) :always)
            `(push ',function ergoemacs-advice--permanent-replace-functions)
          `(push ',function ergoemacs-advice--temp-replace-functions)))))

(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
