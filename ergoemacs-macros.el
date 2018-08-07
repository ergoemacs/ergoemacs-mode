;;; ergoemacs-macros.el --- Macros for ergoemacs-mode -*- lexical-binding: t -*-

;; Copyright © 2013, 2014, 2015, 2018  Free Software Foundation, Inc.

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

;; This shouldn't be called at run-time; This fixes the byte-compile warning.
(defun ergoemacs-theme-component--parse
    (keys-and-body &optional skip-first)
  "Parse KEYS-AND-BODY, optionally skipping the name and
documentation with SKIP-FIRST.

Uses `ergoemacs-theme-component--parse-keys-and-body' and
  `ergoemacs-theme-component--parse-remaining'."
  (ergoemacs-theme-component--parse-keys-and-body
   keys-and-body
   'ergoemacs-theme-component--parse-remaining
   skip-first))

(defun ergoemacs-theme-component--parse-key-str (str)
  "Wrap C-i, C-m and C-[ in <>."
  (cond
   ((not (stringp str)) str)
   ((string-match-p "^\\(?:M-\\|S-\\)*C-\\(?:M-\\|S-\\)*[im[]$" str) (concat "<" str ">"))
   (t str)))

(defun ergoemacs-theme-component--parse-key (item)
  "Change `kbd' and `read-kbd-macro' on C-i, C-m, and C-[ to allow calling on GUI."
  (cond
   ((not (consp item)) item)
   ((eq (nth 0 item) 'kbd)
    (list 'kbd (ergoemacs-theme-component--parse-key-str (nth 1 item))))
   ((eq (nth 0 item) 'read-kbd-macro)
    (list 'read-kbd-macro (ergoemacs-theme-component--parse-key-str (nth 1 item)) (nth 2 item)))
   (t item)))

(defun ergoemacs-theme-component--parse-fun (fun)
  "Determine how FUN should be used with `ergoemacs-component-struct--define-key'."
  (let (tmp)
    (or (and (ergoemacs-keymapp (ergoemacs-sv fun)) `(quote ,fun))
        (ignore-errors
          (and (consp fun)
               (stringp (nth 0 fun))
               (symbolp (nth 1 fun))
               (eq (nth 1 fun) :emacs)
               (setq tmp (lookup-key global-map (read-kbd-macro (nth 0 fun))))
               (commandp tmp)
               `(quote ,tmp)))
        (ignore-errors
          (and (consp fun)
               (eq 'quote (nth 0 fun))
               (consp (nth 1 fun))
               (stringp (nth 0 (nth 1 fun)))
               (symbolp (nth 1 (nth 1 fun)))
               (eq (nth 1 (nth 1 fun)) :emacs)
               (setq tmp (lookup-key global-map (read-kbd-macro (nth 0 (nth 1 fun)))))
               (commandp tmp)
               `(quote ,tmp)))
        (ignore-errors
          (and (consp fun)
               (stringp (nth 0 fun))
               (symbolp (nth 1 fun))
               `(quote ,fun)))
        fun)))

;;;###autoload
(defun ergoemacs-theme-component--parse-remaining (remaining)
  "Parse the REMAINING list, and convert:

- `define-key' is converted to
  `ergoemacs-component-struct--define-key' and keymaps are quoted.

- `global-set-key' is converted to
  `ergoemacs-component-struct--define-key' with keymap equal to
  `global-map'.

- `bind-key' is converted to
  `ergoemacs-component-struct--define-key'.

- `global-unset-key' is converted to
  `ergoemacs-component-struct--define-key' with keymap equal to
  `global-map' and function definition is nil.

- `global-reset-key' is converted
  `ergoemacs-component-struct--define-key'

- `setq' and `set' is converted to
  `ergoemacs-component-struct--set'

- `add-hook' and `remove-hook' is converted to
  `ergoemacs-component-struct--set'

- Mode initialization like (delete-selection-mode 1)
  or (delete-selection) is converted to
  `ergoemacs-component-struct--set'

- Allows :version statement expansion to
  `ergoemacs-component-struct--new-version'

- Adds with-hook syntax or (when -hook) or (when -mode) using
  `ergoemacs-component-struct--with-hook'

Since `ergoemacs-mode' tries to distinguish return, escape, and
tab from their ASCII equivalents In the GUI, the following Emacs
keyboard codes are converted to keys that `ergoemacs-mode' can
distinguish from the ASCII equivalents:

- C-i (TAB) is changed to <C-i>

- C-m (RET) is changed to <C-m>

- C-[ (ESC)  is changed to <C-]>"
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
               `(ergoemacs-component-struct--define-key 'global-map ,(ergoemacs-theme-component--parse-key (nth 1 elt)) nil))
              ((ignore-errors (eq (nth 0 elt) 'global-unset-key))
               `(ergoemacs-component-struct--define-key 'global-map ,(ergoemacs-theme-component--parse-key (nth 1 elt)) nil))
              ((ignore-errors (eq (nth 0 elt) 'set))
               ;; Currently doesn't support (setq a b c d ), but it should.
               `(ergoemacs-component-struct--set ,(nth 1 elt) '(lambda() ,(nth 2 elt))))
              ((ignore-errors (eq (nth 0 elt) 'add-hook))
               `(ergoemacs-component-struct--set ,(nth 1 elt) ,(nth 2 elt)
                                                 (list t ,(nth 3 elt) ,(nth 4 elt))))
              ((ignore-errors (eq (nth 0 elt) 'remove-hook))
               `(ergoemacs-component-struct--set ,(nth 1 elt) ,(nth 2 elt)
                                                 (list nil nil ,(nth 3 elt))))
              ((ignore-errors (memq (nth 0 elt) '(setq setq-default)))
               ;; in the theme component `setq' is equivalent to
               ;; `seq-default' since the component uses `set' and `set-default'
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
               `(ergoemacs-component-struct--define-key 'global-map ,(ergoemacs-theme-component--parse-key (nth 1 elt))
                                                        ,(ergoemacs-theme-component--parse-fun (nth 2 elt))))
              
              ;; (bind-key "C-c x" 'my-ctrl-c-x-command)
              ((ignore-errors (and (eq (nth 0 elt) 'bind-key)
                                   (= (length elt) 3)))
               `(ergoemacs-component-struct--define-key 'global-map (kbd ,(ergoemacs-theme-component--parse-key-str (nth 1 elt)))
                                                        ,(ergoemacs-theme-component--parse-fun (nth 2 elt))))

              ;; (bind-key "C-c x" 'my-ctrl-c-x-command some-other-map)
              ((ignore-errors (and (eq (nth 0 elt) 'bind-key)
                                   (= (length elt) 4)))
               `(ergoemacs-component-struct--define-key (quote ,(nth 3 elt)) (kbd ,(ergoemacs-theme-component--parse-key-str (nth 1 elt)))
                                                        ,(ergoemacs-theme-component--parse-fun (nth 2 elt))))
              
              ((ignore-errors (eq (nth 0 elt) 'define-key))
               (if (equal (nth 1 elt) '(current-global-map))
                   `(ergoemacs-component-struct--define-key 'global-map ,(ergoemacs-theme-component--parse-key (nth 2 elt))
                                                            ,(ergoemacs-theme-component--parse-fun (nth 3 elt)))
                 `(ergoemacs-component-struct--define-key (quote ,(nth 1 elt)) ,(ergoemacs-theme-component--parse-key (nth 2 elt))
                                                          ,(ergoemacs-theme-component--parse-fun (nth 3 elt)))))
              ((or (ignore-errors (eq (nth 0 elt) 'with-hook))
                   (and (ignore-errors (eq (nth 0 elt) 'when))
                        (ignore-errors (string-match "\\(-hook\\|-mode\\|^mark-active\\)$" (symbol-name (nth 1 elt))))))
               (let ((tmp (ergoemacs-theme-component--parse (cdr (cdr elt)) t)))
                 `(ergoemacs-component-struct--with-hook
                   ',(nth 1 elt) ',(nth 0 tmp)
                   '(lambda () ,@(nth 1 tmp)))))
              ((ignore-errors (memq (nth 0 elt) '(dolist when unless if)))
               `(,(car elt) ,(car (cdr elt)) ,@(macroexpand-all (ergoemacs-theme-component--parse-remaining (cdr (cdr elt))))))
              ((ignore-errors (memq (nth 0 elt) '(ergoemacs-advice defadvice)))
               (macroexpand-all elt))
              (t `(ergoemacs-component-struct--deferred ',elt))))
           remaining)))
    remaining))

;;;###autoload
(defmacro ergoemacs-component (&rest body-and-plist)
  "A component of an ergoemacs-theme.

This places BODY-AND-PLIST in the `ergoemacs-theme-component'
macro."
  (declare (doc-string 2)
           (indent 2))
  (macroexpand-all `(ergoemacs-theme-component ,@body-and-plist)))


(defvar ergoemacs-theme-component-properties
  '(:bind
    :bind-keymap
    :bind*
    :bind-keymap*
    :commands
    :mode
    :interpreter
    :defer
    :demand
    :diminish
    :ensure
    :package-name
    :ergoemacs-require
    :no-load
    :no-require
    :just-first-keys
    :variable-modifiers
    :variable-prefixes
    :layout)
  "List of ergoemacs-theme-component properties.")
;;;###autoload
(defmacro ergoemacs-theme-component (&rest body-and-plist)
  "A component of an ergoemacs-theme.

This macro parses BODY-AND-PLIST to Emacs code to generate an
`erogemacs-mode' theme component.

This accepts the following keywords:

:bind -- What keys to bind.  This is compatible with use-package
    definitions.  That is it can take a command like:

    :bind (\"C-.\" . ace-jump-mode)

    or list of commands

    :bind ((\"M-o l\" . highlight-lines-matching-regexp)
           (\"M-o r\" . highlight-regexp)
           (\"M-o w\" . highlight-phrase))

    This list of commands can just be a list without the extra
    parentheses for each command:

    :bind (\"M-o l\" . highlight-lines-matching-regexp
           \"M-o r\" . highlight-regexp
           \"M-o w\" . highlight-phrase)


    Note that these keys may change based on keyboard layouts,
    and also these keys can accept special ergoemacs-commands and
    keymaps (unlike use-package).

    When package-name is non-nil, create autoloads for undefined commands.

    Default: nil

:bind-keymap -- A keymap to bind.  Similar to :bind but used for
    keymaps.  This is processed before :bind keywords.  While
    this is necessary for use-package, it is not necessary for
    `ergoemacs-mode'.  However, this keyword is provided for convenience. 

    Default: nil

:bind* -- Keys to bind above minor modes (in
  `ergoemacs-override-keymap').

   Default: nil

:bind-keymap* -- Keymap to bind above minor modes (in
   `ergoemacs-override-keymap').

    Default: nil

:commands -- List of commands to create autoloads for.  This can
    take a command like:

    :commands ace-jump-mode

    Or

    :commands (isearch-moccur isearch-all)

    When :package-name is non-nil, this will create autoloads for
    the commands.

:defer -- Should this package's loading be deferred?
    When using :commands :bind :bind* :bind-keymap :bind-keymap*
   :mode or :interperter, defer is implied.  When :package-name
   is nil, this dosen't do anything.

:demand -- Prevent deferred loading in all cases

:diminish -- Diminish this mode.  It can be of the following forms:

    :diminish t -- Assumes that :package-name is diminshed
    :diminish package-name -- Diminshes package-name
    :diminish (package-name \" New Description\")
    :diminish (package-name \" unicode\" \" str\")

    For more information, see `ergoemacs-component--diminish-on'.

    By default this is nil.

:mode -- Modes to be added to `auto-mode-alist'. This can be a string such as:

(ergoemacs-package ruby-mode
    :mode \"\\\\.rb\\\\'\")

or a list 

(ergoemacs-package ruby-mode
    :mode (\"\\\\.rb\\\\'\" . ruby-mode))

or a list of modes:

(ergoemacs-package ess-site
    :ensure ess
    :mode ((\"\\\\.R\\\\'\" . R.mode)
           (\"\\\\.[Ss][Aa][Ss]\\\\'\" . SAS-mode)))

Borrowed from `use-package'.

:ensure -- If the package should be installed by `package' if not present.

This can be t to install the :package-name symbol.  Otherwise
it can be a list of symbols or single symbol.

:package-name -- Name of package to load.  When non-nil any key
defition to a single command will create an autoload for that
command.

Default: nil

:no-load / :no-require -- Don't load/require the package-name.

:ergoemacs-require -- when non-nil, this ergoemacs-component is
required with `ergoemacs-require'. By default this is disabled

:just-first-keys -- Keys where a fixed amount of the key is based
on variable keyboard placement, then the rest of the key is
based on letter.  For example with the apps component, the
just first keys are defined to be [apps ?h], which means the
[apps h] will be defined based on finger placement, but the
keys afterward would be based on letter.

By default this is defined as nil, meaning no special keys
like this occur.

:just-first-keys (list [apps ?h] [menu ?h])
Defaults to nil

:variable-modifiers -- Modifiers that are considierd variable.
These modifiers have keys change among various keyboard
layouts.  That is keys are bound based on finger placement
among various keyboard layouts.

Defaults to '(meta)

:variable-prefixes -- Keyboard prefixes that are considiered
variable.  After these keys are pressed, the keyboard layout
dictates the keys.  That is, keys are bound based on finger
placement among various keyboard layouts.

Defaults to '([apps] [menu] [27])

:layout -- Layout that the key bindings are based on.

Defaults to us (QWERTY)

Please do not use the following tags, since they are parsed based
on the definition:

:name -- Component Name

:description -- Component Description

:file -- File where the component was defined."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs-theme-component--parse body-and-plist))
    `(let ((plist ',(nth 0 kb))
           (fun '(lambda () ,@(nth 1 kb))))
       (unless (boundp 'ergoemacs-component-hash)
         (defvar ergoemacs-component-hash (make-hash-table :test 'equal)
           "Hash of ergoemacs theme components"))
       (defvar ergoemacs-mode-reset)
       (setq ergoemacs-mode-reset t)
       (puthash ,(plist-get (nth 0 kb) :name)
                `(lambda() ,(plist-get plist :description)
                  (ergoemacs-component-struct--create-component
                   ',plist ',fun ,(or load-file-name buffer-file-name)))
                ergoemacs-component-hash)
       ,(when (plist-get (nth 0 kb) :ergoemacs-require)
          `(ergoemacs-require ',(intern (plist-get (nth 0 kb) :name)))))))

(defmacro ergoemacs-package (name &rest keys-and-body)
  "Defines a required package named NAME.

KEYS-AND-BODY will be processed by
`ergoemacs-theme-component--parse-keys-and-body'.

The documentation of the supported keys are in
`ergoemacs-theme-component'.

The NAME will be assumed to be the :package-name keyword.

By default, this package also set the :ergoemacs-require to t,
requiring the ergoemacs theme component immediately.  To turn off
this feature, you can specify :ergoemacs-require nil in the body
of the `ergoemacs-package' macro.  Another option is to use
`ergoemacs-autoload', which is the same as `ergoemacs-package'
with :ergoemacs-require set to nil."
  (declare (indent 2))
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
    (unless (memq :ergoemacs-require plist) ;; Its a required theme component.
      (setq plist (plist-put plist :ergoemacs-require name)))
    (unless (plist-get plist :package-name)
      (setq plist (plist-put plist :package-name name)))
    (macroexpand-all
     `(ergoemacs-theme-component ,name ()
        ,doc
        ,@plist
        ;; (require ',name)
        ,@body))))

(defmacro ergoemacs-autoload (name &rest keys-and-body)
  "Defines a required package named NAME.

KEYS-AND-BODY will be processed by
`ergoemacs-theme-component--parse-keys-and-body'.

The documentation of the supported keys are in
`ergoemacs-theme-component'.

The NAME will be assumed to be the :package-name keyword.

By default, this package also set the :ergoemacs-require to nil,
deferring the ergoemacs theme component until it is required by
the user by either `ergoemacs-require' or turning it on/off in an
ergoemacs-mode theme.  To turn off this feature, you can
specify :ergoemacs-require t in the body of the
`ergoemacs-autoload' macro.  Another option is to use
`ergoemacs-package', which is the same as `ergoemacs-autoload'
with :ergoemacs-require set to t."
  (declare (indent 2))
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
    (unless (plist-get plist :package-name)
      (setq plist (plist-put plist :package-name name)))
    (macroexpand-all
     `(ergoemacs-theme-component ,name ()
        ,doc
        ,@plist
        ,@body))))

;;;###autoload
(defmacro ergoemacs-test-layout (&rest keys-and-body)
  (let ((kb (make-symbol "body-and-plist"))
        (plist (make-symbol "plist"))
        (body (make-symbol "body")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body keys-and-body  nil t)
          plist (nth 0 kb)
          body (nth 1 kb))
    (macroexpand-all
     `(let ((old-ergoemacs-theme (ergoemacs :current-theme))
            (old-type ergoemacs-command-loop-type)
            (old-paste interprogram-paste-function)
            (old-cut interprogram-cut-function)
            ;; (old-kill kill-ring)
            ;; (old-pointer kill-ring-yank-pointer)
            (old-version (ergoemacs :current-version))
            (macro
             ,(if (plist-get plist :macro)
                  `(edmacro-parse-keys ,(plist-get plist :macro) t)))
            (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
            (reset-ergoemacs nil))
        (setq ergoemacs-theme ,(plist-get plist ':current-theme)
              ergoemacs-keyboard-layout ,(or (plist-get plist ':layout) "us")
              ergoemacs-command-loop-type nil
              interprogram-paste-function nil
              interprogram-cut-function nil
              ;; kill-ring nil
              ;; kill-ring-yank-pointer nil
              
              ;; Make sure the copy functions don't think the last
              ;; command was a copy.
              last-command 'ergoemacs-test)
        (ergoemacs-theme-set-version ,(or (plist-get plist ':version) nil))
        (unless (and (equal old-ergoemacs-theme ergoemacs-theme)
                     (equal old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
                     (equal old-version (ergoemacs :current-vresion)))
          (setq reset-ergoemacs t)
          (ergoemacs-mode-reset))
        
        ,(if (plist-get plist :cua)
             `(cua-mode 1))
        (unwind-protect
            (progn
              ,@body)
          (setq ergoemacs-command-loop-type old-type
                ergoemacs-theme old-ergoemacs-theme
                ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout
                interprogram-paste-function old-paste
                interprogram-cut-function old-cut
                ;; kill-ring old-kill
                ;; kill-ring-yank-pointer old-pointer
                )
          (ergoemacs-theme-set-version old-version)
          (when reset-ergoemacs
            (ergoemacs-mode-reset)))))))

(defvar ergoemacs-theme-components--modified-plist nil
  "Modified plist.")

(defun ergoemacs-theme-component--add-ensure (plist pkg)
  "Add PKG to the :ensure keyword."
  (let ((cur-ensure (plist-get plist :ensure))
        (cur-pkg (intern (format "%s" (plist-get plist :package-name)))))
    (cond
     ((eq cur-ensure t)
      (setq ergoemacs-theme-components--modified-plist
            (plist-put plist :ensure (list pkg cur-pkg))))
     ((not cur-ensure)
      (setq ergoemacs-theme-components--modified-plist
            (plist-put plist :ensure pkg)))
     ((not (memq pkg cur-ensure))
      (push pkg cur-ensure)
      (setq ergoemacs-theme-components--modified-plist
            (plist-put plist :ensure cur-ensure))))))

(defun ergoemacs-theme-component--parse-keys-and-body
    (keys-and-body &optional parse-function  skip-first)
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
      (push (cons ':name (if (stringp (cl-first remaining))
                             (pop remaining)
                           (symbol-name (pop remaining))))
            extracted-key-accu)
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
    (list plist remaining)))

;;;###autoload
(defmacro ergoemacs-theme (&rest body-and-plist)
  "Define an ergoemacs-theme.

This macro parses BODY-AND-PLIST into an `ergoemacs-mode' theme.

- :components -- list of components that this theme uses.  These
  can't be seen or toggled.

- :optional-on -- list of components that are optional and are on
  by default

- :optional-off -- list of components that are optional and off
  by default

- :options-menu -- Menu options list

- :silent -- If this theme is \"silent\", i.e. doesn't show up in
  the Themes menu.

- :based-on -- what `ergoemacs-mode' theme this is based on.

The rest of the body is an `ergoemacs-theme-component' named
 THEME-NAME-theme."
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
    (macroexpand-all
     `(let* ((based-on (ergoemacs-gethash ,based-on ergoemacs-theme-hash))
             (curr-plist ',tmp)
             (opt-on (plist-get curr-plist ':optional-on))
             (opt-off (plist-get curr-plist ':optional-off))
             (comp (plist-get curr-plist ':components))
             (themes (ergoemacs-gethash "defined-themes" ergoemacs-theme-hash))
             (silent (ergoemacs-gethash "silent-themes" ergoemacs-theme-hash))
             (included (append opt-on opt-off comp))
             (file (or load-file-name (buffer-file-name)))
             (mod (list file (and (stringp file) (nth 5 (file-attributes file))))))
        (when (not (boundp 'ergoemacs--component-file-mod-time-list))
          (setq ergoemacs--component-file-mod-time-list nil))
        (push ,(plist-get (nth 0 kb) :name) themes)
        (push ,(plist-get (nth 0 kb) :name) silent)
        (setq curr-plist (plist-put curr-plist :file file))
        (unless (member mod ergoemacs--component-file-mod-time-list)
          (push mod ergoemacs--component-file-mod-time-list))
        (if (not based-on)
            (puthash ,(plist-get (nth 0 kb) ':name) curr-plist ergoemacs-theme-hash)
          (dolist (type '(:optional-on :optional-off :components))
            (dolist (comp (plist-get based-on type))
              (unless (memq comp included)
                (setq curr-plist
                      (plist-put curr-plist type
                                 (append (plist-get curr-plist type)
                                         (list comp)))))))
          (when (and (not (plist-get curr-plist :options-menu))
                     (plist-get based-on :options-menu))
            (setq curr-plist
                  (plist-put curr-plist :options-menu
                             (plist-get based-on :options-menu))))
          (puthash ,(plist-get (nth 0 kb) :name) curr-plist
                   ergoemacs-theme-hash))
        (if ,(plist-get (nth 0 kb) :silent)
            (puthash "silent-themes" silent ergoemacs-theme-hash)
          (puthash "defined-themes" themes ergoemacs-theme-hash))
        (ergoemacs-theme-component ,(intern (concat (plist-get (nth 0 kb) :name) "-theme")) ()
          ,(format "Generated theme component for %s theme" (plist-get (nth 0 kb) :name))
          ,@(nth 1 kb))))))

;;;###autoload
(defmacro ergoemacs-deftheme (name desc based-on &rest differences)
  "Create theme layout for `ergoemacs-mode' key-bindings.

This is compatibility layer.

- NAME is the theme name.

- DESC is the theme description

- BASED-ON is the base name theme that the new theme is based on.

- DIFFERENCES are the differences from the layout based on the
  functions.  These are based on the following functions:

- `ergoemacs-key' = defines/replaces variable key with function
  by (ergoemacs-key QWERTY-KEY FUNCTION DESCRIPTION ONLY-FIRST)

- `ergoemacs-fixed-key' = defines/replace fixed key with function
   by (ergoemacs-fixed-key KEY FUNCTION DESCRIPTION)."
  (declare (indent 1))
  ;; FIXME: Why `macroexpand-all', given that the output will itself go through
  ;; macroexpand(-all) anyway?
  (macroexpand-all
   `(let (silent pl tmp)
      (setq pl (ergoemacs-gethash (or ,based-on "standard") ergoemacs-theme-hash))
      (plist-put pl ':name ,(symbol-name name))
      (setq tmp (plist-get pl ':components))
      (push (intern (concat ,(symbol-name name) "-theme")) tmp)
      (setq tmp (plist-put pl ':components tmp))
      (setq silent (ergoemacs-gethash "silent-themes" ergoemacs-theme-hash))
      (push ,(symbol-name name) silent)
      (puthash "silent-themes" silent ergoemacs-theme-hash)
      (puthash ,(symbol-name name) tmp ergoemacs-theme-hash)
      (ergoemacs-theme-component ,(intern (concat (symbol-name name) "-theme")) ()
        ,(or desc (format "Generated theme component for %s theme" (symbol-name name)))
        ,@differences))))

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
- :remap - Use `ergoemacs-mode' to remap to an appropriate function.
- :md5 -- returns an md5 of the currently enabled `ergoemacs-mode' options.
- :map-list,  :composed-p, :composed-list, :key-hash :empty-p calls ergoemacs-map-properties-- equivalent functions.

"
  (let ((arg1 (nth 0 args))
        (arg2 (nth 1 args))
        (arg3 (nth 2 args))
        (arg4 (nth 3 args)))
    (cond
     ((and arg1 (symbolp arg1) (eq arg1 :reset-prefix))
      (if (>= 25 emacs-major-version)
	  `(prefix-command-preserve-state)
	`(reset-this-command-lengths)))
     ((and arg1 (symbolp arg1) (eq arg1 :set-selection))
      (if (>= 25 emacs-major-version)
          `(gui-set-selection ,@(cdr args))
        `(x-set-selection ,@(cdr args))))
     ((and arg1 (symbolp arg1) (eq arg1 :set-selection))
      (if (>= 25 emacs-major-version)
	  `(gui-set-selection ,@(cdr args))
	`(x-set-selection ,@(cdr args))))
     ((and arg1 (symbolp arg1) (eq arg1 :width))
      `(ergoemacs-mode--eval-width ,arg2))
     ((and arg1 (symbolp arg1) (eq arg1 :mode-if) arg2)
      `(ergoemacs-mode-line--if ,arg2 ,arg3 ,arg4))
     ((and arg1 (symbolp arg1) (memq arg1 '(:sep :separator)))
      `(ergoemacs-mode-line--sep ,@(cdr args)))
     ((and arg1 (symbolp arg1) (memq arg1 '(:sep-right :separator-right)))
      `(ergoemacs-mode-line--sep 'right ,@(cdr args)))
     ((and arg1 (symbolp arg1) (memq arg1 '(:sep-left :separator-left)))
      `(ergoemacs-mode-line--sep 'left ,@(cdr args)))
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
     ((and arg1 (symbolp arg1) (eq arg1 :ignore-global-changes-p) (not arg2) (not arg3))
      `(ergoemacs-map-properties--ignore-global-changes-p))
     ((and arg1 (symbolp arg1) (eq arg1 :user-before) (not arg2) (not arg3))
      `(ergoemacs-map-properties--before-ergoemacs))
     ((and arg1 (symbolp arg1) (eq arg1 :user-after) (not arg2) (not arg3))
      `(ergoemacs-map-properties--before-ergoemacs t))
     ((and arg1 (symbolp arg1) (eq arg1 :modal-p))
      `(ergoemacs-command-loop--modal-p))
     ((and arg1 (symbolp arg1) (eq arg1 :combine) arg2 arg3)
      `(ergoemacs-command-loop--combine ,arg2 ,arg3))
     ((and arg1 (symbolp arg1) (memq arg1 '(:unicode-or-alt :unicode)))
      `(ergoemacs-key-description--unicode-char ,@(cdr args)))
     ((and arg1 (symbolp arg1) (eq arg1 :modifier-desc)
           arg2)
      `(mapconcat #'ergoemacs-key-description--modifier ,arg2 ""))
     ((and arg1 (symbolp arg1) (eq arg1 :current-version))
      `(ergoemacs-theme--get-version))
     ((and arg1 (symbolp arg1) (eq arg1 :current-theme))
      `(or (and ergoemacs-theme (stringp ergoemacs-theme) ergoemacs-theme)
           (and ergoemacs-theme (symbolp ergoemacs-theme) (symbol-name ergoemacs-theme))
           "standard"))
     ((and arg1 (symbolp arg1)
           (memq arg1 ergoemacs--map-properties-list))
      `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg1) 1))) ,@(cdr args)))

     ((and arg1 arg2 (eq arg2 :new-command) arg3)
      ;; (ergoemacs arg1 :new-command 'next-line)
      `(ergoemacs-map-properties--new-command ,arg1 ,arg3))
     ((and arg1 (symbolp arg1)
           (eq arg1 :global-map))
      `(ergoemacs-map-properties--original (or ergoemacs-saved-global-map global-map)))
     ((and arg1 (symbolp arg1)
           (eq arg1 :revert-global-map))
      `(ergoemacs-map-properties--original (or ergoemacs-saved-global-map global-map) :setcdr))
     ((and arg1 (symbolp arg1)
           (eq arg1 :remap) arg2)
      `(progn
         (setq this-command (or (key-binding (vector 'ergoemacs-remap ,arg2) t nil (point)) ,arg2))
         (call-interactively (or (key-binding (vector 'ergoemacs-remap ,arg2) t nil (point)) ,arg2))))
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
     (t
      `(ergoemacs-map-- ,arg1)))))

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

;;;###autoload
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

(defmacro ergoemacs-cache-p (item)
  "Does ITEM cache exist?"
  (or (and (symbolp item)
           (macroexpand-all
            `(ergoemacs-map-cache--exists-p ',item)))
      (macroexpand-all
       `(let ((--hash-key ,item))
          (ergoemacs-map-cache--exists-p --hash-key)))))

(defmacro ergoemacs-timing (key &rest body)
  "Save the timing using KEY for BODY."
  (declare (indent 1))
  (if (listp key)
      `(ergoemacs-timing-- ,key (lambda() ,@body))
    `(ergoemacs-timing-- ',key (lambda() ,@body))))

(defmacro ergoemacs-no-specials (&rest body)
  "Revert some `ergoemacs-mode' functions to their C defintions in BODY."
  `(cl-letf (((symbol-function 'read-key-sequence) #'ergoemacs--real-read-key-sequence)
	     ((symbol-function 'describe-key) #'ergoemacs--real-describe-key))
     ,@body))

(defmacro ergoemacs-specials (&rest body)
  "Use `ergoemacs-mode' special functions in BODY."
  `(cl-letf (((symbol-function 'read-key-sequence) #'ergoemacs-command-loop--read-key-sequence)
	     ((symbol-function 'key-description) #'ergoemacs-key-description))
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

(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
