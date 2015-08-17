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
- `bind-key' is converted to `ergoemacs-component-struct--define-key'
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
              
              ;; (bind-key "C-c x" 'my-ctrl-c-x-command)
              ((ignore-errors (and (eq (nth 0 elt) 'bind-key)
                                   (= (length elt) 3)))
               (if (ergoemacs-keymapp (ergoemacs-sv (nth 2 elt))) 
                   `(ergoemacs-component-struct--define-key 'global-map (kbd ,(nth 1 elt)) (quote ,(nth 2 elt)))
                 `(ergoemacs-component-struct--define-key 'global-map (kbd ,(nth 1 elt)) ,(nth 2 elt))))

              ;; (bind-key "C-c x" 'my-ctrl-c-x-command some-other-map)
              ((ignore-errors (and (eq (nth 0 elt) 'bind-key)
                                   (= (length elt) 4)))
               (if (ergoemacs-keymapp (ergoemacs-sv (nth 2 elt))) 
                   `(ergoemacs-component-struct--define-key (quote ,(nth 3 elt)) (kbd ,(nth 1 elt)) (quote ,(nth 2 elt)))
                 `(ergoemacs-component-struct--define-key (quote ,(nth 3 elt)) (kbd ,(nth 1 elt)) ,(nth 2 elt))))
              
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
              (t `(ergoemacs-component-struct--deferred ',elt))))
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
  "A component of an ergoemacs-theme.
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

    By default this is nil.

:delight -- Delight this mode.  It takes the same arguments
    as :diminish.  It also makes sure delight is installed.

    By default this is nil.

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

    By default this is defined as nil, meaning no special keys like this occur.

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
    `(let ((fn (or load-file-name (buffer-file-name))))
       (unless (boundp 'ergoemacs-component-hash)
         (defvar ergoemacs-component-hash (make-hash-table :test 'equal)
           "Hash of ergoemacs theme components"))
       (unless (boundp 'ergoemacs-component-hash)
         (defvar ergoemacs-theme-comp-hash (make-hash-table :test 'equal)
           "Hash of ergoemacs themes"))
       (puthash ,(plist-get (nth 0 kb) :name)
                (lambda() ,(plist-get (nth 0 kb) :description)
                  (ergoemacs-theme-component--create-component
                   (append (list :file fn)
                           ',(nth 0 kb))
                   '(lambda () ,@(nth 1 kb)))) ergoemacs-theme-comp-hash)
       (puthash ,(plist-get (nth 0 kb) :name)
                (lambda() ,(plist-get (nth 0 kb) :description)
                  (ergoemacs-component-struct--create-component
                   (append (list :file fn)
                           ',(nth 0 kb))
                   '(lambda () ,@(nth 1 kb)))) ergoemacs-component-hash)
       ,(when (plist-get (nth 0 kb) :ergoemacs-require)
          `(ergoemacs-require ',(intern (plist-get (nth 0 kb) :name)))))))

(defmacro ergoemacs-package (name &rest keys-and-body)
  "Defines a required package named NAME.
Maybe be similar to use-package"
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

(defvar ergoemacs-theme-component--tags
  '(:diminish
    :delight)
  "Tags that are known to `ergoemacs-theme-component'")

(defvar ergoemacs-theme-components--modified-plist nil
  "Modified plist.")

(fset 'ergoemacs-theme-component--add-ensure
      #'(lambda  (plist pkg)
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
                   (plist-put plist :ensure cur-ensure)))))))

(defun ergoemacs-theme-component--tag-diminish (plist tag-value remaining)
  "Handle :diminish tag for `ergoemacs-theme-component'.

Will add (diminish 'package-name) for :diminish t"
  ;; :diminish keyword support
  (let ((tag-value tag-value)
        pkg ret)
    (cond
     ((and (eq tag-value t) (setq pkg (plist-get plist :package-name)))
      (setq ret `((require 'diminish) (diminish ',(intern (format "%s" pkg))) ,@remaining)))
     ((and tag-value (symbolp tag-value))
      (setq ret `((require 'diminish) (diminish ',tag-value) ,@remaining)))
     ((and tag-value (consp tag-value) (symbolp (car tag-value)) (setq pkg (car tag-value))
           (or (and (stringp (cdr tag-value)) (setq tag-value (cdr tag-value)))
               (ignore-errors (and (stringp (nth 1 tag-value)) (setq tag-value (nth 1 tag-value))))))
      (setq ret `((require 'diminish) (diminish ',pkg ,tag-value) ,@remaining)))
     (t (setq ret remaining)))
    ret))

(defun ergoemacs-theme-component--tag-delight (plist tag-value remaining)
  "Handle :delight tag for `ergoemacs-theme-component'.
This tag implies :ensure delight"
  (let ((tag-value tag-value)
        pkg ret)
    (cond
     ((and (eq tag-value t) (setq pkg (plist-get plist :package-name)))
      (ergoemacs-theme-component--add-ensure plist 'delight)
      (setq ret `((require 'delight) (delight ',(intern (format "%s" pkg))) ,@remaining)))
     ((and tag-value (symbolp tag-value))
      (ergoemacs-theme-component--add-ensure plist 'delight)
      (setq ret `((require 'delight) (delight ',tag-value ,@remaining))))
     ((and tag-value (consp tag-value) (symbolp (car tag-value)) (setq pkg (car tag-value))
           (or (and (stringp (cdr tag-value)) (setq tag-value (cdr tag-value)))
               (ignore-errors (and (stringp (nth 1 tag-value)) (setq tag-value (nth 1 tag-value))))))
      (ergoemacs-theme-component--add-ensure plist 'delight)
      (setq ret `((require 'delight) (delight ',pkg ,tag-value) ,@remaining)))
     (t (setq ret remaining)))
    ret))

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
                tag-value
                tag-fn
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
            (setq plist (loop for (key . value) in extracted-key-accu
                              collect key
                              collect value))
            (when parse-function
              (when (eq parse-function 'ergoemacs-theme-component--parse-remaining)
                ;; Handle known tags.
                (dolist (tag ergoemacs-theme-component--tags)
                  (when (and (setq tag-value (plist-get plist tag))
                             (fboundp (setq tag-fn (intern (concat "ergoemacs-theme-component--tag-" (substring (symbol-name tag) 1)))))
                             (setq tag-fn (funcall tag-fn plist tag-value remaining)))
                    (setq remaining tag-fn)
                    (when ergoemacs-theme-components--modified-plist
                      (setq plist ergoemacs-theme-components--modified-plist
                            ergoemacs-theme-components--modified-plist nil)))))
              (setq remaining
                    (funcall parse-function remaining)))
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
    (macroexpand-all
     `(let* ((based-on (ergoemacs-gethash ,based-on ergoemacs-theme-hash))
             (curr-plist ',tmp)
             (opt-on (plist-get curr-plist ':optional-on))
             (opt-off (plist-get curr-plist ':optional-off))
             (comp (plist-get curr-plist ':components))
             (themes (ergoemacs-gethash "defined-themes" ergoemacs-theme-hash))
             (silent (ergoemacs-gethash "silent-themes" ergoemacs-theme-hash))
             (included (append opt-on opt-off comp))
             (fn (or load-file-name (buffer-file-name))))
        (push ,(plist-get (nth 0 kb) :name) themes)
        (push ,(plist-get (nth 0 kb) :name) silent)
        (setq curr-plist (plist-put curr-plist :file fn))
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
        ,(format "Generated theme component for %s theme" (symbol-name name))
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

;;;###autoload
(defmacro ergoemacs (&optional arg1 arg2 arg3)
  "Get/Set keymaps and `ergoemacs-mode' properties

When arg1 can be a property.  The following properties are supported:
- :layout - returns the current (or specified by PROPERTY) keyboard layout.
- :remap - Use `ergoemacs-mode' to remap to an appropriate function.
- :md5 -- returns an md5 of the currently enabled `ergoemacs-mode' options.
- :theme-debug -- Debugs the theme by calling `ergoemacs-component--checkout'
- :map-list,  :composed-p, :composed-list, :key-hash :empty-p calls ergoemacs-map-properties-- equivalent functions.

"
  (let ((map-properties-list '(:map-list
                               :user
                               :composed-p
                               :composed-list
                               :key-struct
                               :key-hash
                               :key-lessp
                               :empty-p
                               :label
                               :keys
                               :where-is
                               :lookup
                               :original
                               :original-user
                               :installed-p
                               :sequence
                               :movement-p
                               :override-map-p
                               :override-maps
                               :deferred-maps
                               :use-local-unbind-list-p
                               :set-map-p)))
    (cond
     ((and arg1 (symbolp arg1) (eq arg1 :user-before) (not arg2) (not arg3))
      `(ergoemacs-map-properties--before-ergoemacs))
     ((and arg1 (symbolp arg1) (eq arg1 :modal-p))
      `(ergoemacs-command-loop--modal-p))
     ((and arg1 (symbolp arg1) (eq arg1 :combine) arg2 arg3)
      `(ergoemacs-command-loop--combine ,arg2 ,arg3))
     ((and arg1 (symbolp arg1) (eq arg1 :unicode-or-alt)
           arg2 arg3)
      `(ergoemacs-key-description--unicode-char ,arg2 ,arg3))
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
           (memq arg1 map-properties-list))
      `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg1) 1))) ,arg2 ,arg3))

     ((and arg1 arg2 (eq arg2 :new-command) arg3)
      ;; (ergoemacs arg1 :new-command 'next-line)
      `(ergoemacs-map-properties--new-command ,arg1 ,arg3))

     ((and arg1 (symbolp arg1)
           (eq arg1 :global-map))
      `(ergoemacs-map-properties--original global-map))
     
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
       ((memq arg2 map-properties-list)
        `(,(intern (format "ergoemacs-map-properties--%s" (substring (symbol-name arg2) 1))) ,arg1))
       (t
        `(ignore-errors (ergoemacs-gethash ,arg2 (ergoemacs-gethash (ergoemacs-map-properties--key-hash (ergoemacs-map-properties--keymap-value ,arg1)) ergoemacs-map-properties--plist-hash))))))

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
      `(ergoemacs-map-- ,arg1))
     )))

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

(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
