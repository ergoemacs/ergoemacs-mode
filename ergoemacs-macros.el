;;; ergoemacs-macros.el --- Macros for ergoemacs-mode -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

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
(require 'custom)

(declare-function ergoemacs-warn "ergoemacs-lib")

;;;###autoload
(defmacro ergoemacs-keymapp (keymap)
  "Error free check of keymap by `keymapp'"
  `(ignore-errors (keymapp ,keymap)))

(defmacro ergoemacs-gethash (key table &optional dflt)
  "Safe `gethash'.
Will only use `gethash' when `table' is a hash table"
  `(and ,table (hash-table-p ,table) (gethash ,key ,table ,dflt)))

;;;###autoload
(defmacro ergoemacs-sv (symbol &optional default)
  "Error free `symbol-value'.
If SYMBOL is void, return nil"
  `(if ,default
       (ignore-errors (default-value ,symbol))
     (ignore-errors (symbol-value ,symbol))))

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
            (unless (or (keywordp (cl-first remaining)) skip-first)
              (if (condition-case nil
                      (stringp (cl-first remaining))
                    (error nil))
                  (push (cons ':name (pop remaining)) extracted-key-accu)
                (push (cons ':name  (symbol-name (pop remaining))) extracted-key-accu))
              (when (memq (type-of (cl-first remaining)) '(symbol cons))
                (setq remaining (cdr remaining)))
              (when (stringp (cl-first remaining))
                (push (cons ':description (pop remaining)) extracted-key-accu)))
            (while (and (consp remaining) (keywordp (cl-first remaining)))
              (let ((keyword (pop remaining)))
                (unless (consp remaining)
                  (error "Value expected after keyword %S in %S"
                         keyword keys-and-body))
                (when (assoc keyword extracted-key-accu)
                  (ergoemacs-warn "Keyword %S appears more than once in %S" keyword
                                  keys-and-body))
                (push (cons keyword (pop remaining)) extracted-key-accu)))
            (setq extracted-key-accu (nreverse extracted-key-accu))
            (setq plist (cl-loop for (key . value) in extracted-key-accu
                              collect key
                              collect value))
            (when parse-function
              (setq remaining
                    (funcall parse-function remaining)))
            (list plist remaining))))

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

(defvar ergoemacs--map-properties-list
  '(
    :composed-list
    :composed-p
    :deferred-maps
    :empty-p
    :installed-p
    :key-hash
    :key-lessp
    :key-struct
    :keys
    :label
    :lookup
    :movement-p
    :original
    :original-user
    :override-map-p
    :override-maps
    :revert-original
    :sequence
    :set-map-p
    :use-local-unbind-list-p
    :user
    :where-is
    :map-list
    )
  "Partial list of `ergoemacs' supported properties.

These proprerties are aliaes for ergoemacs-map-properties--
functions.")

;;;###autoload
(defmacro ergoemacs (&rest args)
  "Get/Set keymaps and `ergoemacs-mode' properties

When arg1 can be a property.  The following properties are supported:
- :layout - returns the current (or specified by PROPERTY) keyboard layout.
- :map-list,  :composed-p, :composed-list, :key-hash :empty-p calls ergoemacs-map-properties-- equivalent functions.

"
  (let ((arg1 (nth 0 args))
        (arg2 (nth 1 args))
        (arg3 (nth 2 args))
        (arg4 (nth 3 args)))
    (cond
     ((and arg1 (symbolp arg1) (eq arg1 :custom-p) (symbolp arg2))
      (if (fboundp 'custom-variable-p)
          `(custom-variable-p ,arg2)
        `(user-variable-p ,arg2)))
     ((and arg1 (symbolp arg1) (eq arg1 :apply-key) arg2 arg3)
      `(ergoemacs-translate--apply-key ,@(cdr args)))
     ((and arg1 (symbolp arg1) (eq arg1 :spinner) arg2)
      `(ergoemacs-command-loop--spinner-display ,@(cdr args)))
     ((and arg1 (symbolp arg1) (eq arg1 :define-key) arg2 arg3)
      `(ergoemacs-translate--define-key ,arg2 ,arg3 ,arg4))
     ((eq arg1 :ignore-global-changes-p) (not arg2) (not arg3)
      `(ergoemacs-map-properties--ignore-global-changes-p))
     ((eq arg1 :user-before) (not arg2) (not arg3)
      `(ergoemacs-map-properties--before-ergoemacs))
     ((eq arg1 :user-after) (not arg2) (not arg3)
      `(ergoemacs-map-properties--before-ergoemacs t))
     ((and arg1 (symbolp arg1) (eq arg1 :combine) arg2 arg3)
      `(ergoemacs-command-loop--combine ,arg2 ,arg3))
     ((and arg1 (symbolp arg1) (eq arg1 :modifier-desc)
           arg2)
      `(mapconcat #'ergoemacs-key-description--modifier ,arg2 ""))
     ((and arg1 (symbolp arg1)
           (memq arg1 ergoemacs--map-properties-list))
      `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg1) 1))) ,@(cdr args)))
     ((and arg1 (symbolp arg1)
           (eq arg1 :layout))
      `(ergoemacs-layouts--current ,arg2))
     ((and arg1 arg2 (not arg3)
           (symbolp arg2)
           (string= ":" (substring (symbol-name arg2) 0 1)))
      ;; Get a arg2
      (cond
       ((eq arg2 :full)
        `(ignore-errors (char-table-p (nth 1 (ergoemacs-map-properties--keymap-value ,arg1)))))
       ((eq arg2 :indirect)
        (macroexpand-all `(ergoemacs-keymapp (symbol-function ,arg1))))
       ((memq arg2 '(:map-key :key))
        ;; FIXME Expire any ids that are no longer linked??
        `(ignore-errors (plist-get (ergoemacs-map-properties--map-fixed-plist ,arg1) :map-key)))
       ((memq arg2 ergoemacs--map-properties-list)
        `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg2) 1))) ,arg1))
       (t
        `(ergoemacs-map-properties--get ,arg1 ,arg2))))
     ((and arg1 arg2 arg3
           (symbolp arg2)
	   (memq arg2 ergoemacs--map-properties-list))
      ;; Assign a property.
      `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg2) 1))) ,arg1 ,@(cdr (cdr args))))
     ((and arg1 arg2 arg3
           (symbolp arg2)
           (string= ":" (substring (symbol-name arg2) 0 1)))
      ;; Assign a property.
      `(ergoemacs-map-properties--put ,arg1 ,arg2 ,arg3))
     ((and (not arg3) (eq arg1 'emulation-mode-map-alists))
      `(ergoemacs-map--emulation-mode-map-alists ,arg2))
     ((and (not arg3) (eq arg1 'minor-mode-overriding-map-alist))
      `(ergoemacs-map--minor-mode-overriding-map-alist ,arg2))
     ((and (not arg3) (eq arg1 'minor-mode-map-alist))
      `(ergoemacs-map--minor-mode-map-alist ,arg2))
     )
    )
  )

(defmacro ergoemacs-cache (item &rest body)
  "Either read ITEM's cache or evaluate BODY, cache ITEM and return value."
  (declare (indent 1))
  (or (and (symbolp item)
           (macroexpand-all
            `(progn
               (or (ergoemacs-map--cache-- ',item)
                   (ergoemacs-map--cache--
                    ',item (progn ,@body))))))
      (macroexpand-all
       `(let ((--hash-key ,item))
          (or (ergoemacs-map--cache-- --hash-key)
              (ergoemacs-map--cache-- --hash-key (progn ,@body)))))))

(defmacro ergoemacs-no-specials (&rest body)
  "Revert some `ergoemacs-mode' function  s to their C defintions in BODY."
  `(cl-letf (((symbol-function 'read-key-sequence) #'ergoemacs--real-read-key-sequence)
	     ((symbol-function 'describe-key) #'ergoemacs--real-describe-key))
     ,@body))

(defmacro ergoemacs-autoloadp (object)
  "Non-nil if OBJECT is an autoload."
  (cond
   ((fboundp #'autoloadp) `(autoloadp ,object))
   (t `(eq 'autoload (car-safe ,object)))))

(defmacro ergoemacs-buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (cond
   ((fboundp #'buffer-narrowed-p) `(buffer-narrowed-p))
   (t `(/= (- (point-max) (point-min)) (buffer-size)))))

;;;###autoload
(defmacro ergoemacs-translation (&rest body-and-plist)
  "Defines an `ergoemacs-mode' translation.
:text -- Text to display while completing this translation
:keymap -- Local Keymap for translation
:keymap-modal -- Modal keymap for overrides.
:modal-always -- If the modal state is always on, regardless of
                 the values of  `ergoemacs-modal-ignored-buffers',
                `ergoemacs-modal-emacs-state-modes' `minibufferp'
The following arguments allow the keyboard presses to be translated:
 - :meta
 - :control
 - :shift
 - :meta-control
 - :meta-shift
 - :control-shift
 - :meta-control-shift
 - :unchorded (no modifiers)
This also creates functions:
- ergoemacs-translate--NAME-universal-argument
- ergoemacs-translate--NAME-digit-argument
- ergoemacs-translate--NAME-negative-argument
- ergoemacs-translate--NAME-modal"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "kb")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body body-and-plist))
    
    `(progn (puthash ,(intern (concat ":" (plist-get (nth 0 kb) ':name)))
                     (lambda() ,(plist-get (nth 0 kb) ':description)
                       (ergoemacs-translate--create :key ,(intern (concat ":" (plist-get (nth 0 kb) ':name)))
                                                    ,@(nth 0 kb))) ergoemacs-translation-hash))))

(defmacro ergoemacs-advice (function args &rest body-and-plist)
  "Defines an `ergoemacs-mode' advice.

The structure is (ergoemacs-advice function args tags body-and-plist)

When the tag :type equals :replace, the advice replaces the function.

When :type is :replace that replaces a function (like `define-key')"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "kb")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body `(nil nil ,@body-and-plist)))
    (cond
     ((eq (plist-get (nth 0 kb) :type) :around)
      ;; FIXME: use `nadvice' for emacs 24.4+
      (macroexpand-all `(progn
                          (defadvice ,function (around ,(intern (format "ergoemacs-advice--%s" (symbol-name function))) ,args activate)
                            ,(plist-get (nth 0 kb) :description)
                            ,@(nth 1 kb)))))
     ((eq (plist-get (nth 0 kb) :type) :after)
      ;; FIXME: use `nadvice' for emacs 24.4+
      (macroexpand-all
       `(progn
          (defadvice ,function (after ,(intern (format "ergoemacs-advice--after-%s" (symbol-name function))) ,args activate)
            ,(plist-get (nth 0 kb) :description)
            ,@(nth 1 kb)))))
     ((eq (plist-get (nth 0 kb) :type) :before)
      ;; FIXME: use `nadvice' for emacs 24.4+
      (macroexpand-all `(progn
                          (defadvice ,function (before ,(intern (format "ergoemacs-advice--%s" (symbol-name function))) ,args activate)
                            ,(plist-get (nth 0 kb) :description)
                            ,@(nth 1 kb)))))
     ((eq (plist-get (nth 0 kb) :type) :replace)
      (macroexpand-all `(progn
                          (defalias ',(intern (format "ergoemacs-advice--real-%s" (symbol-name function)))
                            (symbol-function ',function) (concat ,(format "ARGS=%s\n\n" args) (documentation ',function)
                                                                 ,(format "\n\n`ergoemacs-mode' preserved the real `%s' in this function."
                                                                          (symbol-name function))))
                          (defun ,(intern (format "ergoemacs-advice--%s--" function)) ,args
                            ,(format "%s\n\n%s\n\n`ergoemacs-mode' replacement function for `%s'.\nOriginal function is preserved in `ergoemacs-advice--real-%s'"
                                     (documentation function)
                                     (plist-get (nth 0 kb) :description) (symbol-name function) (symbol-name function))
                            ,@(nth 1 kb))
                          ;; Hack to make sure the documentation is in the function...
                          (defalias ',(intern (format "ergoemacs-advice--%s" function)) ',(intern (format "ergoemacs-advice--%s--" function))
                            ,(format "ARGS=%s\n\n%s\n\n%s\n\n`ergoemacs-mode' replacement function for `%s'.\nOriginal function is preserved in `ergoemacs-advice--real-%s'"
                                     args (documentation function) (plist-get (nth 0 kb) :description) (symbol-name function) (symbol-name function)))
                          ,(if (plist-get (nth 0 kb) :always)
                               `(push ',function ergoemacs-advice--permanent-replace-functions)
                             `(push ',function ergoemacs-advice--temp-replace-functions))))))))


(defmacro ergoemacs-save-key-state (keymap-symbol &rest body)
  "Save keys in KEYMAP-SYMBOL, eval BODY."
  `(progn
     (ergoemacs-mode--save-map ,keymap-symbol)
     ,@body
     (ergoemacs-mode--save-map ,keymap-symbol t)))

(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
