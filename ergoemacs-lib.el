;;; ergoemacs-lib.el --- Ergoemacs libraries -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Author: Matthew L. Fidler, Xah Lee
;; Maintainer: 
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
  (require 'ergoemacs-macros)
  (require 'cl))

(defvar ergoemacs-excluded-major-modes)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-mode)
(defvar ergoemacs-mode-names)
(defvar ergoemacs-require)
(defvar ergoemacs-theme-hash)

(defvar package-archives)

(defvar tabbar-mode)
(defvar ergoemacs-handle-ctl-c-or-ctl-x)

(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-theme--list "ergoemacs-theme-engine")
(declare-function ergoemacs-theme-option-on "ergoemacs-theme-engine")

(declare-function ergoemacs-key-description--menu "ergoemacs-key-description")

(declare-function tabbar-install-faces "tabbar-ruler")
(declare-function tabbar-mode "tabbar")


(defun ergoemacs-setcdr (var val &optional default)
  "Use `setcdr' on VAL to VAL.
If VAL is a symbol, use `ergoemacs-sv' to determine the value.
If VAR is nil, return nil and do nothing.
If DEFAULT is non-nil set the default value, instead of the symbol value."
  (if (symbolp var)
      (setcdr (ergoemacs-sv var default) val)
    (if (not var) nil
      (setcdr var val))))

;;;###autoload
(defun ergoemacs-set (variable value &optional force)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value))) ; Otherwise negative integers are the same as nil
         (last-value (ignore-errors (ergoemacs-sv variable))))
    (when (and minor-mode-p (not last-value))
      (setq last-value -1))
    (cond
     ((and minor-mode-p (not (boundp variable)))
      (unless (get variable 'ergoemacs-save-value)
        (put variable 'ergoemacs-save-value (if new-value nil 1)))
      (funcall variable new-value))
     ((not (equal last-value value))
      (cond
       ((and (custom-variable-p variable) (or force (not (get variable 'save-value))))
        ;; (message "Changed customizable %s" variable)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (customize-set-variable variable new-value)
        (customize-mark-to-save variable)
        (when (and minor-mode-p (not new-value))
          (funcall variable -1)))
       ((or force (equal (ergoemacs-sv variable) (default-value variable)))
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        ;; (message "Changed variable %s" variable)
        (set variable new-value)
        (set-default variable new-value)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (when minor-mode-p
          (if new-value
              (funcall variable new-value)
            (funcall variable -1))))
       (t
        ;; (message "%s changed outside ergoemacs-mode, respecting." variable)
        )))
     (t
      ;; (message "%s not changed" variable)
      ))))

;;;###autoload
(defun ergoemacs-save (variable value)
  "Set VARIABLE to VALUE and tell customize it needs to be saved."
  (if (not (custom-variable-p variable))
      (set variable value)
    (customize-set-variable variable value)
    (customize-mark-as-set variable)))

(defun ergoemacs-reset (variable)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (value (get variable 'ergoemacs-save-value))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value)
                        ;; Otherwise negative integers are the same as nil
                        )))
    (put variable 'ergoemacs-save-value nil)
    (if (and minor-mode-p (not (boundp variable)))
        (funcall variable new-value)
      (if (custom-variable-p variable)
          (progn
            (customize-set-variable variable new-value)
            (customize-mark-to-save variable)
            (when (and minor-mode-p (not new-value))
              (funcall variable -1)))
        (set variable new-value)
        (set-default variable new-value)
        (when minor-mode-p ;; Change minor mode
          (if new-value
              (funcall variable new-value)
            (funcall variable -1)))))))

(defun ergoemacs-remove (option &optional theme type keep)
  "Removes an OPTION on ergoemacs themes.

Calls `ergoemacs-require' with TYPE defaulting to 'off and
remove defaulting to t.

KEEP can change remove to nil.
"
  (ergoemacs-require option theme (or type 'off) (if keep nil t)))

(defvar ergoemacs-require--ini-p nil)
(defun ergoemacs-require--ini-p ()
  (setq ergoemacs-require--ini-p t))
(add-hook 'ergoemacs-mode-after-startup-run-load-hooks #'ergoemacs-require--ini-p)

(defun ergoemacs-require (option &optional theme type remove)
  "Requires an OPTION on ergoemacs themes.

THEME can be a single theme or list of themes to apply the option
to.  If unspecified, it is all themes.

TYPE can be 'on, where the option will be turned on by default
but shown as something at can be toggled in the ergoemacs-mode
menu.

TYPE can also be 'required-hidden, where the option is turned on,
and it dosen't show up on the ergoemacs-mode menu.

TYPE can also be 'off, where the option will be included in the
theme, but assumed to be disabled by default.

When TYPE is nil, assume the type is 'required-hidden

REMOVE represents when you would remove the OPTION from the
ergoemacs THEME.
"
  (unless (member (list option theme type remove) ergoemacs-require)
    (push (list option theme type remove) ergoemacs-require))
  (if ergoemacs-require--ini-p
      (if (eq (type-of option) 'cons)
          (dolist (new-option option)
            (let (ergoemacs-mode)
              (ergoemacs-require new-option theme type)))
        (let ((option-sym
               (or (and (stringp option) (intern option)) option)))
          (dolist (theme (or (and theme (or (and (eq (type-of theme) 'cons) theme) (list theme)))
                             (ergoemacs-theme--list)))
            (let ((theme-plist (ergoemacs-gethash (if (stringp theme) theme
                                                    (symbol-name theme))
                                                  ergoemacs-theme-hash))
                  comp on off)
              (setq comp (plist-get theme-plist :components)
                    on (plist-get theme-plist :optional-on)
                    off (plist-get theme-plist :optional-off))
              (setq comp (delq option-sym comp)
                    on (delq option-sym on)
                    off (delq option-sym off))
              (cond
               (remove) ;; Don't do anything.
               ((or (not type) (memq type '(required-hidden :required-hidden)))
                (push option-sym comp))
               ((memq type '(off :off))
                (push option-sym off))
               ((memq type '(on :on))
                (push option-sym on)))
              (setq theme-plist (plist-put theme-plist :components comp))
              (setq theme-plist (plist-put theme-plist :optional-on on))
              (setq theme-plist (plist-put theme-plist :optional-off off))
              (puthash (if (stringp theme) theme (symbol-name theme)) theme-plist
                       ergoemacs-theme-hash)))))
    (unless (eq ergoemacs-require--ini-p :ini)
      (ergoemacs-theme-option-on option t))))


(defvar ergoemacs-xah-emacs-lisp-tutorial-url
  "http://ergoemacs.org/emacs/elisp.html")

(defvar ergoemacs-mode-web-page-url
  "http://ergoemacs.github.io/")


(defun ergoemacs-menu--get-major-mode-name (mode)
  "Gets the MODE language name.
Tries to get the value from `ergoemacs-mode-names'.  If not guess the language name."
  (let ((ret (assoc mode ergoemacs-mode-names)))
    (if (not ret)
        (setq ret (replace-regexp-in-string
                   "-" " "
                   (replace-regexp-in-string
                    "-mode" ""
                    (symbol-name mode))))
      (setq ret (car (cdr ret))))
    (setq ret (concat (upcase (substring ret 0 1))
                      (substring ret 1)))
    ret))

(defun ergoemacs-menu--get-major-modes ()
  "Gets a list of language modes known to `ergoemacs-mode'.
This gets all major modes known from the variables:
-  `interpreter-mode-alist';
-  `magic-mode-alist'
-  `magic-fallback-mode-alist'
-  `auto-mode-alist'

All other modes are assumed to be minor modes or unimportant.
"
  ;; Get known major modes
  (let ((ret '())
        all dups cur-lst current-letter
        added-modes
        (modes '()))
    (dolist (elt (append
                  interpreter-mode-alist
                  magic-mode-alist
                  magic-fallback-mode-alist
                  auto-mode-alist))
      (unless (memq (cdr elt) modes)
        (when (and (functionp (cdr elt))
                   (ignore-errors (string-match "-mode$" (symbol-name (cdr elt)))))
          (unless (or (memq (cdr elt) ergoemacs-excluded-major-modes)
                      (member (downcase (symbol-name (cdr elt))) added-modes))
            (let* ((name (ergoemacs-menu--get-major-mode-name (cdr elt)))
                   (first (upcase (substring name 0 1))))
              (if (member first all)
                  (unless (member first dups)
                    (push first dups))
                (push first all))
              (push (list (cdr elt) 'menu-item
                          name
                          (cdr elt)) ret))
            (push (downcase (symbol-name (cdr elt))) added-modes)
            (push (cdr elt) modes)))))
    (setq modes (sort ret (lambda(x1 x2) (string< (downcase (nth 2 x2))
                                             (downcase (nth 2 x1))))))
    (setq ret '())
    (dolist (elt modes)
      (let ((this-letter (upcase (substring (nth 2 elt) 0 1))))
        (cond
         ((not (member this-letter dups))
          ;; not duplicated -- add prior list and push current element.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (push elt ret)
          (setq current-letter this-letter)
          (setq cur-lst nil))
         ((not (equal this-letter current-letter))
          ;; duplicated, but not last letter.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (setq cur-lst nil)
          (setq current-letter this-letter)
          (push elt cur-lst))
         (t
          ;; duplicated and last letter
          (push elt cur-lst)))))
    (when cur-lst
      (push `(,(intern current-letter) menu-item ,current-letter
              (keymap ,@cur-lst)) ret))
    ;; Now create nested menu.
    `(keymap ,@ret
             (separator1 menu-item "--")
             (package menu-item  "Manage Packages" list-packages))))

(defun ergoemacs-menu-tabbar-toggle ()
  "Enables/Disables (and installs if not present) a tab-bar for emacs."
  (interactive)
  (require 'package nil t)
  (if (not (fboundp 'tabbar-mode))
      (let ((package-archives '(("melpa" . "http://melpa.org/packages/"))))
        (require 'tabbar-ruler nil t)
        (if (fboundp 'tabbar-install-faces)
            (tabbar-install-faces)
          (when (fboundp 'package-install)
            (package-refresh-contents)
            (package-initialize)
            (package-install 'tabbar-ruler)
            (require 'tabbar-ruler nil t)
            (tabbar-install-faces))))
    (if (not (featurep 'tabbar-ruler))
        (require 'tabbar-ruler nil t)
      (if tabbar-mode
          (tabbar-mode -1)
        (tabbar-mode 1)))))


(defun ergoemacs-menu--filter-key-shortcut (cmd &optional keymap)
  "Figures out ergoemacs-mode menu's preferred key-binding for CMD."
  (cond
   ((not cmd))
   ((and (memq ergoemacs-handle-ctl-c-or-ctl-x '(only-copy-cut both))
         (eq cmd 'ergoemacs-cut-line-or-region)) (ergoemacs-key-description--menu (kbd "C-x")) )
   ((and (memq ergoemacs-handle-ctl-c-or-ctl-x '(only-copy-cut both))
         (eq cmd 'ergoemacs-copy-line-or-region)) (ergoemacs-key-description--menu (kbd "C-c")))
   (t
    ;;; FIXME: faster startup by creating component alists
    ;; SLOW: 2-seconds 
    (let ((key (where-is-internal cmd (or keymap ergoemacs-keymap) 'meta nil t)))
      (when (memq (elt key 0) '(menu-bar remap again redo cut copy paste help open find ergoemacs-remap execute))
        (setq key nil))
      (and key (ergoemacs-key-description--menu key)))
    
    ;; (let ((key (ergoemacs-gethash cmd (ergoemacs (ergoemacs :global-map) :where-is))))
    ;;   (when key
    ;;     (setq key (nth 0 key)))
    ;;   (when (memq (elt key 0) '(menu-bar remap again redo cut copy paste help open find ergoemacs-remap execute))
    ;;     (setq key nil))
    ;;   (or (and key (ergoemacs-key-description--menu key)) ""))
    )))


(defun ergoemacs-menu--filter-key-menu-item (item &optional keymap)
  "Key menu item."
  (if (and (>= (safe-length item) 4)
           (symbolp (car item))
           (eq (cadr item) 'menu-item)
           (stringp (caddr item))
           (symbolp (cadddr item))
           (not (ergoemacs-keymapp (cadddr item))))
      ;; Look if this item already has a :keys property
      (if (catch 'found-keys
            (dolist (i item)
              (when (eq i :keys)
                (throw 'found-keys t))) nil) nil
        (ergoemacs-menu--filter-key-shortcut (cadddr item) keymap))
    nil))

;;;###autoload
(defun ergoemacs-menu--filter (menu &optional fn keymap)
  "Put `ergoemacs-mode' key bindings on menus."
  (let ((menu (or (and (not fn) menu)
                  (funcall fn menu)))
        tmp tmp2)
    ;; (when fn
    ;;   (message "%s:\n\t%s" fn menu))
    (if (not (ergoemacs-keymapp menu) )
        (progn
          (when menu
            (message "Invalid menu in ergoemacs-menu--filter %s" menu))
          menu)
      (when (symbolp menu)
        (setq menu (ergoemacs-sv menu)))
      ;; For each element in the menu
      (ergoemacs-setcdr
       menu
       (mapcar
        (lambda (item)
          (let (key)
            (cond
             ;; ((ergoemacs-keymapp item)
             ;;  (ergoemacs-menu--filter item)
             ;;  (message "1:%s..." (substring (format "%s" item) 0 (min (length (format "%s" item)) 60)))
             ;;  item)
             ((and (ergoemacs-keymapp keymap) (ergoemacs-keymapp (cdr (cdr item))))
              ;; JIT processing
              `(,(nth 0 item) menu-item ,(nth 1 item) ,(cdr (cdr item))
                :filter (lambda(bind) (ergoemacs-menu--filter bind nil ',keymap))))
             ((ergoemacs-keymapp (cdr (cdr item)))
              ;; JIT processing
              `(,(nth 0 item) menu-item ,(nth 1 item) ,(cdr (cdr item))
                :filter ergoemacs-menu--filter))
             
             ((ergoemacs-keymapp (car (cdr (cdr (cdr item)))))
              ;; (message "3:%s..." (substring (format "%s" item) 0 (min (length (format "%s" item)) 60)))
              (if (setq tmp (plist-get item :filter))
                  (mapcar
                   (lambda(elt)
                     (cond
                      ((eq elt :filter)
                       (setq tmp2 t)
                       :filter)
                      ((not tmp2)
                       elt)
                      ((eq elt 'ergoemacs-menu--filter)
                       (setq tmp2 nil)
                       'ergoemacs-menu--filter)
                      ((ignore-errors
                         (and (consp elt)
                              (eq (nth 0 elt) 'lambda)
                              (eq (nth 0 (nth 2 elt)) 'ergoemacs-menu--filter)))
                       (setq tmp2 nil)
                       elt)
                      (t
                       (setq tmp2 nil)
                       `(lambda(bind) (ergoemacs-menu--filter bind ',tmp ',keymap)))))
                   item)
                `(,@item :filter ergoemacs-menu--filter)))
             ((setq key (ergoemacs-menu--filter-key-menu-item item keymap))
              (append item (cons :keys (cons key nil))))
             (t item))))
        (cdr menu))))
    menu))

;;;###autoload
(defun ergoemacs-set-layout (layout)
  "Set `ergoemacs-keyboard-layout' to LAYOUT and reset `ergoemacs-mode'.
The reset is done with `ergoemacs-mode-reset'."
  (setq ergoemacs-keyboard-layout layout)
  (ergoemacs-mode-reset))

(provide 'ergoemacs-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-lib.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
