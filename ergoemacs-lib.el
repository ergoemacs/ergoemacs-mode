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

(defvar powerline-default-separator-dir)
(defvar mode-icons-show-mode-name)
(defvar mode-icons-read-only-space)
(defvar mode-icons-cached-mode-name)
(defvar mode-icons-eol-text)
(defvar ergoemacs-theme)
(defvar ergoemacs-excluded-major-modes)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-mode)
(defvar ergoemacs-mode-names)
(defvar ergoemacs-require)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-timing-hash)
(defvar ergoemacs-component-struct--apply-ensure-p)

(defvar package-archives)

(defvar tabbar-mode)
(defvar ergoemacs-handle-ctl-c-or-ctl-x)
(defvar ergoemacs-dir)


(declare-function ergoemacs-next-emacs-buffer "ergoemacs-functions")
(declare-function ergoemacs-next-user-buffer "ergoemacs-functions")
(declare-function ergoemacs-previous-emacs-buffer "ergoemacs-functions")
(declare-function ergoemacs-previous-user-buffer "ergoemacs-functions")

(declare-function powerline-current-separator "powerline")
(declare-function powerline-selected-window-active "powerline")

(declare-function mode-icons-get-mode-icon "mode-icons")

(declare-function ergoemacs-autoloadp "ergoemacs-macros")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-theme--list "ergoemacs-theme-engine")
(declare-function ergoemacs-theme-option-on "ergoemacs-theme-engine")

(declare-function ergoemacs-key-description--menu "ergoemacs-key-description")

(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-translate--ahk-ini "ergoemacs-translate")
(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")

(declare-function ergoemacs-component-find-definition "ergoemacs-component")
(declare-function ergoemacs-component-find-1 "ergoemacs-component")

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

(defvar ergoemacs-set-ignore-customize nil
  "List of variables that should not be saved by customize.")

;;;###autoload
(defun ergoemacs-set (variable value &optional defer force)
  "Sets VARIABLE to VALUE without disturbing customize or setq.

If the user changed the value with either `setq' or `customize',
then respect the varaible.

If FORCE is true, set it even if it changed.

Whe changed return t, otherwise return nil."
  ;; (message "set:%s %s %s %s" variable value defer force)
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value))) ; Otherwise negative integers are the same as nil
         (last-value (ignore-errors (ergoemacs-sv variable)))
         ret)
    (when (and minor-mode-p (not last-value))
      (setq last-value -1))
    (cond
     ((and minor-mode-p (functionp variable))
      (unless (and defer (ergoemacs-autoloadp variable))
        (unless (get variable :ergoemacs-save-value)
          (put variable :ergoemacs-save-value (if new-value nil 1)))
        (ergoemacs :spinner :new "Call (%s %s)" variable new-value)
        ;; (message "(%s %s) #1" variable new-value)
        (funcall variable new-value)
        (ergoemacs :spinner :new "Done (%s %s)" variable new-value)
        (put variable :ergoemacs-set-value (ergoemacs-sv variable))
        (pushnew variable ergoemacs-set-ignore-customize)
        (setq ret t)))
     ((not (equal last-value value))
      (cond
       ((and minor-mode-p (not new-value) (functionp variable))
        (unless (and defer (ergoemacs-autoloadp variable))
          ;; (message "(%s -1) #2" variable)
          (ergoemacs :spinner :new "Call (%s -1)" variable)
          (funcall variable -1)
          (ergoemacs :spinner :new "Done (%s -1)" variable)
          (unless (get variable :ergoemacs-save-value)
            (put variable :ergoemacs-save-value (ergoemacs-sv variable)))
          (put variable :ergoemacs-set-value (ergoemacs-sv variable))
          (pushnew variable ergoemacs-set-ignore-customize)
          (setq ret t)))
       ((and minor-mode-p new-value (functionp variable))
        (unless (and defer (ergoemacs-autoloadp variable))
          ;; (message "(%s %s) #3" variable new-value)
          (ergoemacs :spinner :new "Call (%s %s)" variable new-value)
          (funcall variable new-value)
          (ergoemacs :spinner :new "Done (%s %s)" variable new-value)
          (unless (get variable :ergoemacs-save-value)
            (put variable :ergoemacs-save-value (ergoemacs-sv variable)))
          (put variable :ergoemacs-set-value (ergoemacs-sv variable))
          (pushnew variable ergoemacs-set-ignore-customize)
          (setq ret t)))
       ((and (ergoemacs :custom-p variable)
             (not minor-mode-p)
             (or force (not (get variable 'save-value))))
        ;; (message "%s->%s #1" variable new-value)
        (unless (get variable :ergoemacs-save-value)
          (put variable :ergoemacs-save-value (ergoemacs-sv variable)))
        (set variable new-value)
        ;; Don't save ergoemacs-mode intilization
        (put variable :ergoemacs-set-value (ergoemacs-sv variable))
        (pushnew variable ergoemacs-set-ignore-customize)
        (setq ret t))
       ((or force (not minor-mode-p)
            (equal (ergoemacs-sv variable) (default-value variable)))
        ;; (message "%s->%s #1" variable new-value)
        (unless (get variable :ergoemacs-save-value)
          (put variable :ergoemacs-save-value (ergoemacs-sv variable)))
        (set variable new-value)
        (set-default variable new-value)
        (unless (get variable :ergoemacs-save-value)
          (put variable :ergoemacs-save-value (ergoemacs-sv variable)))
        (put variable :ergoemacs-set-value (ergoemacs-sv variable))
        (pushnew variable ergoemacs-set-ignore-customize)
        (setq ret t))
       (t
        ;; (ergoemacs-warn "%s changed outside ergoemacs-mode, respecting." variable)
        (setq ret t))))
     (t
      ;; (message "%s was not changed by ergoemacs-mode, since it has the same value.\n\tlast-value: %s\n\tnew-value: %s" variable
      ;;          last-value new-value)
      ))
    ret))

;;;###autoload
(defun ergoemacs-save (variable value)
  "Set VARIABLE to VALUE and tell customize it needs to be saved."
  (if (not (ergoemacs :custom-p variable))
      (set variable value)
    (customize-set-variable variable value)
    (customize-mark-as-set variable)))

(defun ergoemacs-reset (variable)
  "Sets VARIABLE to VALUE without disturbing customize or setq."
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (value (get variable :ergoemacs-save-value))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value)
                        ;; Otherwise negative integers are the same as nil
                        )))
    (put variable :ergoemacs-save-value nil)
    (if (and minor-mode-p (not (boundp variable)))
        (funcall variable new-value)
      (if (ergoemacs :custom-p variable)
          (progn
            ;; (customize-mark-to-save variable)
            (if (and minor-mode-p (not new-value))
                (funcall variable -1)
              (set variable new-value)
              (set-default variable new-value)))
        (set variable new-value)
        (set-default variable new-value)
        (put variable :ergoemacs-set-value (ergoemacs-sv variable))
        (pushnew variable ergoemacs-set-ignore-customize)
        (when minor-mode-p ;; Change minor mode
          (if new-value
              (funcall variable new-value)
            (funcall variable -1)))))))

(defun ergoemacs-remove (option &optional theme type keep)
  "Removes an OPTION on ergoemacs themes.

Calls `ergoemacs-require' with TYPE defaulting to 'off and
remove defaulting to t.

KEEP can change remove to nil."
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
ergoemacs THEME."
  (setq ergoemacs-component-struct--apply-ensure-p t)
  (unless (member (list option theme type remove) ergoemacs-require)
    (push (list option theme type remove) ergoemacs-require))
  (if ergoemacs-require--ini-p
      (if (eq (type-of option) 'cons)
          (dolist (new-option option)
            (let (ergoemacs-mode)
              (ergoemacs-require new-option theme type)))
        (let ((option-sym
               (or (and option (stringp option) (intern option)) option)))
          (dolist (theme (or (and theme (or (and (eq (type-of theme) 'cons) theme) (list theme)))
                             (ergoemacs-theme--list)))
            (let ((theme-plist (ergoemacs-gethash (if (and theme (stringp theme)) theme
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
              (puthash (if (and theme (stringp theme)) theme (symbol-name theme)) theme-plist
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

(defcustom ergoemacs-major-mode-menu-map-extra-modes
  '(fundamental-mode lisp-interaction-mode)
  "List of extra modes that should bring up the major-mode menu."
  :type '(repeat (function :tag "Major Mode"))
  :group 'ergoemacs-mode)

(defvar ergoemacs-menu--get-major-modes nil
  "List of major-modes known to `ergoemacs-mode'.")

(defun ergoemacs-menu--get-major-modes ()
  "Gets a list of language modes known to `eurgoemacs-mode'.
This gets all major modes known from the variables:
-  `interpreter-mode-alist';
-  `magic-mode-alist'
-  `magic-fallback-mode-alist'
-  `auto-mode-alist'
- `ergoemacs-major-mode-menu-map-extra-modes'

All other modes are assumed to be minor modes or unimportant.
"
  ;; Get known major modes
  (let ((ret '())
        all dups cur-lst current-letter
        added-modes
        (modes '()))
    (dolist (elt ergoemacs-major-mode-menu-map-extra-modes)
      (unless (memq elt modes)
        (when (and (functionp elt)
                   (ignore-errors (string-match "-mode$" (symbol-name elt))))
          (unless (or (memq elt ergoemacs-excluded-major-modes)
                      (member (downcase (symbol-name elt)) added-modes))
            (let* ((name (ergoemacs-menu--get-major-mode-name elt))
                   (first (upcase (substring name 0 1))))
              (if (member first all)
                  (unless (member first dups)
                    (push first dups))
                (push first all))
              (push (list elt 'menu-item
                          name
                          elt) ret))
            (push (downcase (symbol-name elt)) added-modes)
            (push elt modes)))))
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
                                                  (downcase (nth 2 x1)))))
	  ergoemacs-menu--get-major-modes (mapcar (lambda(x) (intern x)) added-modes))
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
           (and (caddr item) (stringp (caddr item)))
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

;;;###autoload
(defun ergoemacs-gen-ahk (&optional all)
  "Generates autohotkey for all layouts and themes"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
	(setenv "ERGOEMACS_KEYBOARD_LAYOUT" ergoemacs-keyboard-layout)
	(setenv "ERGOEMACS_THEME" ergoemacs-theme)
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode --eval \"(ergoemacs-gen-ahk %s)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir (if current-prefix-arg "t" "nil"))))
    (let ((xtra "ahk")
          (extra-dir)
          file-temp)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq file-temp (expand-file-name "ergoemacs.ini" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert (ergoemacs-translate--ahk-ini all all)))
      (setq file-temp (expand-file-name "ergoemacs.ahk" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir)))
      (message "Generated ergoemacs.ahk")
      (when (executable-find "ahk2exe")
        (shell-command (format "ahk2exe /in %s" file-temp))
        (message "Generated ergoemacs.exe")))))


(defvar ergoemacs-warn nil
  "List of warnings that `ergoemacs-mode' already gave.")

(defun ergoemacs-warn (&rest args)
  "Warn user only once.
When not contaiend in the variable `ergoemacs-mode', apply ARGS
to the `warn' function."
  (unless (member args ergoemacs-warn)
    (display-warning 'ergoemacs (apply #'format args) :warning)
    (push args ergoemacs-warn)))

(defvar ergoemacs-field-len nil)
(defvar ergoemacs-cc-len nil)
(defvar ergoemacs-at-len nil)
(defvar ergoemacs-et-len nil)
(defvar ergoemacs-mn-len nil)
(defvar ergoemacs-mx-len nil)


(defvar ergoemacs-timing-results-buffer "*Ergoemacs Profiling Results*"
  "Buffer name for outputting profiling results.")

(defun ergoemacs-timing-sort-by-call-count (vec1 vec2)
  "Sort by highest call count.  See `sort'."
  (>= (aref vec1 0) (aref vec2 0)))

(defun ergoemacs-timing-sort-by-total-time (vec1 vec2)
  "Sort by highest total time spent in function. See `sort'."
  (>= (aref vec1 1) (aref vec2 1)))

(defun ergoemacs-timing-sort-by-average-time (vec1 vec2)
  "Sort by highest average time spent in function. See `sort'."
  (>= (aref vec1 2) (aref vec2 2)))


(defcustom ergoemacs-timing-sort-by-function
  'ergoemacs-timing-sort-by-total-time
  "Non-nil specifies ELP results sorting function.
These functions are currently available:

  `ergoemacs-timing-sort-by-call-count'   -- sort by the highest call count
  `ergoemacs-timing-sort-by-total-time'   -- sort by the highest total time
  `ergoemacs-timing-sort-by-average-time' -- sort by the highest average times

You can write your own sort function.  It should adhere to the
interface specified by the PREDICATE argument for `sort'.  Each
\"element of LIST\" is really a 4 element vector where element 0
is the call count, element 1 is the total time spent in the
function, element 2 is the average time spent in the function,
and element 3 is the symbol's name string."
  :type 'function
  :group 'ergoemacs-mode)

(defsubst ergoemacs-timing-pack-number (number width)
  "Pack the NUMBER string into WIDTH characters, watching out for
very small or large numbers"
  (if (<= (length number) width)
      number
    ;; check for very large or small numbers
    (if (string-match "^\\(.*\\)\\(e[+-].*\\)$" number)
        (concat (substring
                 (match-string 1 number)
                 0
                 (- width (match-end 2) (- (match-beginning 2)) 3))
                "..."
                (match-string 2 number))
      (substring number 0 width))))

(defun ergoemacs-timing-results-jump-to-component (&optional event)
  "Jump to component linked to the current button.
EVENT is used when this is calledf rom a mouse event."
  (interactive (list last-nonmenu-event))
  (if event (posn-set-point (event-end event)))
  (ergoemacs-component-find-definition
   (get-text-property (point) 'ergoemacs-component)))

(defvar ergoemacs-timing-component-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ergoemacs-timing-results-jump-to-component)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'ergoemacs-timing-results-jump-to-component)
    map)
  "Keymap used on the component maps.")

(defcustom ergoemacs-timing-find-regexp
  (concat"^\\s-*(ergoemacs-timing" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used by `ergoemacs-timing-find-no-select' to search for a timing definition.
Note it must contain a `%s' at the place where `format' should
insert the face name."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(unless (assoc 'ergoemacs-timing find-function-regexp-alist)
  (push (cons 'ergoemacs-timing 'ergoemacs-timing-find-regexp) find-function-regexp-alist))

(defun ergoemacs-timing-find-no-select (timing-definition file)
  "Find TIMING-DEFINITION in FILE.
This uses `find-function-search-for-symbol'."
  (let* ((el-file (and file (concat (file-name-sans-extension file) ".el")))
         (sym (intern (format "%s" timing-definition))))
    (find-function-search-for-symbol sym 'ergoemacs-timing el-file)))

(defun ergoemacs-timing-results-jump (&optional event)
  "Jump to timing item linked to the current button.
EVENT is used when this is called from a mouse event."
  (interactive (list last-nonmenu-event))
  (if event (posn-set-point (event-end event)))
  (let ((file (get-text-property (point) 'ergoemacs-timing-file))
        (symbol (get-text-property (point) 'ergoemacs-timing-symbol)))
    (when file
      (ergoemacs-component-find-1
       symbol 'ergoemacs-timing 'switch-to-buffer
       (save-excursion
         (ergoemacs-timing-find-no-select symbol file))))))

(defvar ergoemacs-timing-jump-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ergoemacs-timing-results-jump)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'ergoemacs-timing-results-jump)
    map)
  "Jump to a timing definition.")

(defun ergoemacs-timing-output-result--sym (symname &optional file)
  "Insert SYMNAME with appropriate links."
  (let ((sym (format "%s" symname)))
    (unless (catch 'found
              (dolist (lst '(("initialize-\\(.*\\)\\'" "Initialize ")
                             ("ensure-\\(.*\\)\\'" "Ensure ")
                             ("create-component-\\(.*\\)\\'" "Create Component ")
                             ("translate-keymap-\\(.*\\)\\'" "Translate Keymap ")))
                (when (string-match (nth 0 lst) sym)
                  (insert (nth 1 lst)
                          (propertize (match-string 1 sym)
                                      'ergoemacs-component (intern (match-string 1 sym))
                                      'keymap ergoemacs-timing-component-map
                                      'mouse-face 'highlight
                                      'face 'link
                                      'help-echo "mouse-1 or RET jumps to definition"))
                  (throw 'found t)))
              nil)
      (if file
	  (insert (propertize sym
			      'ergoemacs-timing-symbol (intern sym)
			      'ergoemacs-timing-file file
			      'keymap ergoemacs-timing-jump-map
			      'mouse-face 'highlight
			      'face 'link
			      'help-echo "mouse-1 or RET jumps to definition"))
	(insert sym)))))

(defun ergoemacs-timing-output-result (resultvec)
  "Output the RESULTVEC into the results buffer. RESULTVEC is a 4
or more element vector where aref 0 is the call count, aref 1 is
the total time spent in the function, aref 2 is the average time
spent in the function, aref 3 is the minimum time spent on the
function, aref 4 is the maximum time spend on the function, and
aref 3 is the symbol's string name. All other elements in the
vector are ignored."
  (let* ((cc (aref resultvec 0))
         (tt (aref resultvec 1))
         (at (aref resultvec 2))
         (mn (aref resultvec 3))
         (mx (aref resultvec 4))
         (symname (aref resultvec 5))
         (file (aref resultvec 6))
         callcnt totaltime avetime mntime mxtime)
    (setq callcnt (number-to-string cc)
          totaltime (number-to-string tt)
          avetime (number-to-string at)
          mntime (number-to-string mn)
          mxtime (number-to-string mx))
    ;; possibly prune the results
    (ergoemacs-timing-output-result--sym symname file)
    (insert-char 32 (+ ergoemacs-field-len (- (length symname)) 2))
    ;; print stuff out, formatting it nicely
    (insert callcnt)
    (insert-char 32 (+ ergoemacs-cc-len (- (length callcnt)) 2))
    (let ((ttstr (ergoemacs-timing-pack-number totaltime ergoemacs-et-len))
          (atstr (ergoemacs-timing-pack-number avetime ergoemacs-at-len))
          (mnstr (ergoemacs-timing-pack-number mntime ergoemacs-mn-len))
          (mxstr (ergoemacs-timing-pack-number mxtime ergoemacs-mx-len)))
      (insert ttstr)
      (insert-char 32 (+ ergoemacs-et-len (- (length ttstr)) 2))
      (insert atstr)
      (insert-char 32 (+ ergoemacs-at-len (- (length atstr)) 2))
      (insert mnstr)
      (insert-char 32 (+ ergoemacs-mn-len (- (length mnstr)) 2))
      (insert mxstr))
    (insert "\n")))

(defun ergoemacs-timing-results ()
  "Display current ergoemacs-mode  profiling results.
Based on `elp-results'."
  (interactive)
  (let ((curbuf (current-buffer))
        (resultsbuf (get-buffer-create ergoemacs-timing-results-buffer)))
    (set-buffer resultsbuf)
    (erase-buffer)
    ;; get the length of the longest function name being profiled
    (let* ((longest 0)
           (title "What")
           (titlelen (length title))
           (ergoemacs-field-len titlelen)
           (cc-header "Count")
           (ergoemacs-cc-len    (length cc-header))
           (et-header "Elapsed")
           (ergoemacs-et-len    (length et-header))
           (at-header "Average")
           (ergoemacs-at-len    (length at-header))
           (mn-header "Min")
           (ergoemacs-mn-len    (length mn-header))
           (mx-header "Max")
           (ergoemacs-mx-len    (length mx-header))
           (resvec '()))
      (maphash
       (lambda(key item)
         (let* ((symname (format "%s" key))
                (cc (aref item 0))
                (tt (aref item 1))
                (mn (aref item 2))
                (mx (aref item 3))
                (file (aref item 4)))
           (setq longest (max longest (length symname)))
           (push (vector cc tt (if (zerop cc) 0.0
                                 (/ (float tt) (float cc)))
                         mn mx symname file)
                 resvec)))
       ergoemacs-timing-hash)
      (setq ergoemacs-field-len (max titlelen longest))
      (let ((column 0))
        (setq header-line-format
              (mapconcat
               (lambda (title)
                 (prog1
                     (concat
                      (propertize " "
                                  'display (list 'space :align-to column)
                                  'face 'fixed-pitch)
                      title)
                   (setq column (+ column 2
                                   (if (= column 0)
                                       ergoemacs-field-len
                                     (length title))))))
               (list title cc-header et-header at-header
                     mn-header mx-header) "")))
      ;; if sorting is enabled, then sort the results list. in either
      ;; case, call ergoemacs-timing-output-result to output the result in the
      ;; buffer
      (if ergoemacs-timing-sort-by-function
          (setq resvec (sort resvec ergoemacs-timing-sort-by-function)))
      (mapc #'ergoemacs-timing-output-result resvec))
    ;; now pop up results buffer
    (set-buffer curbuf)
    (pop-to-buffer resultsbuf)
    (goto-char (point-min))))

(declare-function ergoemacs--real-mouse-menu-major-mode-map "ergoemacs-lib")
(fset 'ergoemacs--real-mouse-menu-major-mode-map (symbol-function #'mouse-menu-major-mode-map))

(defcustom ergoemacs-swap-major-modes-when-clicking-major-mode-name nil
  "Allow Swapping of major-modes when clicking the mode-name."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-major-mode-menu-map nil)
(defun ergoemacs-major-mode-menu-map ()
  "Popup major modes and information about current mode."
  (interactive)
  (or ergoemacs-major-mode-menu-map
      (set (make-local-variable 'ergoemacs-major-mode-menu-map)
	   (let ((map (and ergoemacs-swap-major-modes-when-clicking-major-mode-name
			   ;; Mode in menu
			   (memq major-mode ergoemacs-menu--get-major-modes)
			   (key-binding [menu-bar languages])))
		 mmap)
	     (if (not map)
		 (ergoemacs--real-mouse-menu-major-mode-map)
	       (setq mmap (ergoemacs--real-mouse-menu-major-mode-map))
	       (define-key map [major-mode-sep-b] '(menu-item  "---"))
	       (define-key map [major-mode] (cons (nth 1 mmap) mmap))
	       map)))))

(defcustom ergoemacs-mode-line-change-buffer 'ergoemacs-mode-line-group-function
  "Method of changing buffer."
  :type '(choice
	  (function :tag "Function to group buffers.")
	  (const :tag "Switch to next/previous user/emacs buffer." 'ergoemacs)
	  (const :tag "Use emacs default method." nil))
  :group 'ergoemacs-mode)

(defun ergoemacs-mode-line-buffer-list ()
  "List of buffers shown in popup menu."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defun ergoemacs-mode-line-group-function (&optional buffer)
  "What group does the current buffer belong to?"
  (if (char-equal ?\* (aref (buffer-name buffer) 0))
      "Emacs Buffer"
    "User Buffer"))

(defun ergoemacs-mode-line-menu (&optional buffer)
  (let* ((cb (or buffer (current-buffer)))
	 (group (with-current-buffer cb
		  (if (functionp ergoemacs-mode-line-change-buffer)
		      (funcall ergoemacs-mode-line-change-buffer)
		    '("Common"))))
	 (groups '())
	 (buf-list (sort
		    (mapcar
		     ;; for each buffer, create list: buffer, buffer name, groups-list
		     ;; sort on buffer name; store to bl (buffer list)
		     (lambda (b)
		       (let (tmp0 tmp1 tmp2)
			 (with-current-buffer b
			   (setq tmp0 b
				 tmp1 (buffer-name b)
				 tmp2 (if (functionp ergoemacs-mode-line-change-buffer)
					  (funcall ergoemacs-mode-line-change-buffer)
					"Common"))
			   (unless (or (string= group tmp2) (assoc tmp2 groups))
			     (push (cons tmp2 (intern tmp2)) groups))
			   (list tmp0 tmp1 tmp2 (intern tmp1)))))
		     (ergoemacs-mode-line-buffer-list))
		    (lambda (e1 e2)
		      (or (and (string= (nth 2 e2) (nth 2 e2))
			       (not (string-lessp (nth 1 e1) (nth 1 e2))))
			  (not (string-lessp (nth 2 e1) (nth 2 e2)))))))
	 menu menu2 tmp)
    (dolist (item buf-list)
      (if (string= (nth 2 item) group)
	  (unless (eq cb (nth 0 item))
	    (push `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item)))) menu))
	(if (setq tmp (assoc (nth 2 item) menu2))
	    (push `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item))))
		  (cdr tmp))
	  (push (list (nth 2 item) `(,(nth 3 item) menu-item ,(nth 1 item) (lambda() (interactive) (funcall menu-bar-select-buffer-function ,(nth 0 item))))) menu2))))
    (setq menu `(keymap ,(if (or menu (> (length menu2) 1))
			     group
			   (car (car menu2)))
			,@(if (or menu (> (length menu2) 1))
			      (mapcar
			       (lambda(elt)
				 `(,(intern (car elt)) menu-item ,(car elt) (keymap ,@(cdr elt))))
			       menu2)
			    (cdr (car menu2)))
			,(when (and menu (>= (length menu2) 1))
			   '(sepb menu-item "--"))
			,@menu))
    menu))

(defun ergoemacs-mode-line-next-buffer (event)
  "Next ergoemacs buffer"
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let ((emacs-buffer-p (string-match-p "^[*]" (buffer-name))))
      (cond
       ((functionp ergoemacs-mode-line-change-buffer)
	(popup-menu (ergoemacs-mode-line-menu)))
       ((not ergoemacs-mode-line-change-buffer)
	(next-buffer))
       (emacs-buffer-p
	(ergoemacs-next-emacs-buffer))
       (t
	(ergoemacs-next-user-buffer))))))

(defun ergoemacs-mode-line-previous-buffer (event)
  "Prevous ergoemacs buffer"
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (let ((emacs-buffer-p (string-match-p "^[*]" (buffer-name))))
      (cond
       ((functionp ergoemacs-mode-line-change-buffer)
	(popup-menu (ergoemacs-mode-line-menu)))
       ((not ergoemacs-mode-line-change-buffer)
	(previous-buffer))
       (emacs-buffer-p
	(ergoemacs-previous-emacs-buffer))
       (t
	(ergoemacs-previous-user-buffer))))))

(defcustom ergoemacs-mode-line-read-only-status t
  "Include minor-modes in `mode-line-format'."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-line-minor-modes t
  "Include minor-modes in `mode-line-format'."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-line-coding t
  "Include coding system information in `mode-line-format'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-line-use-vc t
  "Include vc in mode-line."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-mode-line--property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  ;; Taken from powerline by Donald Ephraim Curtis, Jason Milkins and
  ;; Nicolas Rougier
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun ergoemacs-mode-line--ensure-list (item)
  (or (and (listp item) item) (list item)))

(defun ergoemacs-mode-line--add-text-property (str prop val)
  ;; Taken from powerline by Donald Ephraim Curtis, Jason Milkins and
  ;; Nicolas Rougier
  ;; Changed so that prop is not just 'face
  ;; Also changed to not force list, or add a nil to the list
  (mapconcat
   (lambda (mm)
     (let ((cur (get-text-property 0 prop mm)))
       (if (not cur)
	   (propertize mm prop val)
	 (propertize mm prop (append (ergoemacs-mode-line--ensure-list cur) (list val))))))
   (ergoemacs-mode-line--property-substrings str prop)
   ""))

(defun ergoemacs-mode-line--if--1 (lst-or-string)
  (cond
   ((consp lst-or-string)
    (catch 'found-it
      (dolist (elt lst-or-string)
	(cond
	 ((functionp elt)
	  (let ((tmp (funcall elt)))
	    (if (or (and tmp (stringp tmp)) (listp tmp))
		(throw 'found-it tmp)
	      ;; Otherwise, assume its a boolean.
	      ;; If didn't
	      (when (and (booleanp tmp)
			 (not tmp))
		(throw 'found-it "")))))
	 ((and (symbolp elt) (boundp elt) (not (symbol-value elt)))
	  (throw 'found-it ""))
	 ((and elt (stringp elt))
	  (throw 'found-it elt))
	 ((and elt (consp elt))
	  (throw 'found-it elt))
	 ((and (symbolp elt) (boundp elt) (or (consp (symbol-value elt)) (stringp (symbol-value elt))))
	  (throw 'found-it (symbol-value elt)))))
      ""))
   ((and lst-or-string (stringp lst-or-string))
    lst-or-string)
   (t "")))

(defun ergoemacs-mode-line--if (str &optional face pad)
  "Render STR as mode-line data using FACE and optionally PAD import on left (l), right (r) or both (b)."
  (let* ((rendered-str (format-mode-line (ergoemacs-mode-line--if--1 str)))
	 (padded-str (concat
		      (when (and (> (length rendered-str) 0) (memq pad '(l left b both))) " ")
		      rendered-str
		      (when (and (> (length rendered-str) 0) (memq pad '(r right b both))) " "))))
    (if face
	(ergoemacs-mode-line--add-text-property padded-str 'face face)
      padded-str)))

(defvar ergoemacs-old-mode-line-format nil)

(defun ergoemacs-mode-line--set-buffer-file-coding-system (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively #'set-buffer-file-coding-system)))

(defun ergoemacs-mode-line--encoding ()
  "Encoding mode-line."
  (ergoemacs-save-buffer-state
   (propertize (replace-regexp-in-string "\\(-unix\\|-mac\\|-dos\\|-undecided\\|-emacs\\|prefer-\\)" "" (format "%s" buffer-file-coding-system))
	      'mouse-face 'mode-line-highlight
	      'help-echo "mouse-1: Change buffer coding system"
	      'local-map '(keymap
			   (mode-line keymap
				      (mouse-1 . ergoemacs-mode-line--set-buffer-file-coding-system))))))


(defun ergoemacs-mode-line--sep (dir &rest args)
  "Separator"
  (let ((separator (and (fboundp #'powerline-current-separator)
			(intern (format "powerline-%s-%s"
					(powerline-current-separator)
					(or (and (eq dir 'left)
						 (car powerline-default-separator-dir))
					    (cdr powerline-default-separator-dir)))))))
    (when (fboundp separator)
      (let ((img (apply separator args)))
	(when (and (listp img) (eq 'image (car img)))
	  (ergoemacs-save-buffer-state
	   (propertize " " 'display img
		       'face (plist-get (cdr img) :face))))))))

(defvar ergoemacs-mode-line--lhs nil)
(defvar ergoemacs-mode-line--center nil)
(defvar ergoemacs-mode-line--rhs nil)

(defun ergoemacs-minor-mode-menu-from-indicator (indicator &optional dont-popup)
  "Show menu for minor mode specified by INDICATOR.
Interactively, INDICATOR is read using completion.
If there is no menu defined for the minor mode, then create one with
items `Turn Off', `Hide' and `Help'."
  (interactive
   (list (completing-read
	  "Minor mode indicator: "
	  (describe-minor-mode-completion-table-for-indicator))))
  (let* ((minor-mode (or (and (stringp indicator) (lookup-minor-mode-from-indicator indicator))
			 (and (symbolp indicator) indicator)))
         (mm-fun (or (get minor-mode :minor-mode-function) minor-mode)))
    (unless minor-mode (error "Cannot find minor mode for `%s'" indicator))
    (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
           (menu (and (keymapp map) (lookup-key map [menu-bar]))))
      (setq menu
            (if menu
                `(,@(mouse-menu-non-singleton menu)
		  (sep-minor-mode-ind menu-item "--")
		  (hide menu-item ,(if dont-popup
				       "Show this minor-mode"
				     "Hide this minor-mode")
		      (lambda () (interactive)
			(ergoemacs-minor-mode-hide ',mm-fun ,dont-popup))))
	      `(keymap
                ,indicator
                (turn-off menu-item "Turn Off minor mode" ,mm-fun)
		(hide menu-item ,(if dont-popup
				     "Show this minor-mode"
				   "Hide this minor-mode")
		      (lambda () (interactive)
			(ergoemacs-minor-mode-hide ',mm-fun ,dont-popup)))
                (help menu-item "Help for minor mode"
                      (lambda () (interactive)
                        (describe-function ',mm-fun))))))
      (if dont-popup menu
	(popup-menu menu)))))

(defun ergoemacs-mode--minor-mode-mouse (click-group click-type string)
  "Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING."
  ;; Taken from Powerline
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (ergoemacs-minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defvar ergoemacs-mode--space-hidden-minor-modes nil
  "List of minor modes hidden due to space limitations.")

;; Adapted from powerline.
(defvar ergoemacs-mode--hidden-minor-modes '(isearch-mode)
  "List of hidden modes and their indicators.")

(defun ergoemacs-minor-mode-hide (minor-mode &optional show)
  "Hide a minor-mode based on mode indicator MINOR-MODE."
  (if show
      (setq ergoemacs-mode--hidden-minor-modes (delq minor-mode ergoemacs-mode--hidden-minor-modes))
    (unless (memq minor-mode ergoemacs-mode--hidden-minor-modes)
      (push minor-mode ergoemacs-mode--hidden-minor-modes)))
  (force-mode-line-update))

(defun ergoemacs-minor-mode-hidden-menu (&optional _event)
  "Display a list of the hidden minor modes."
  (interactive "@e")
  (popup-menu
   `(keymap
     ,@(mapcar
       (lambda(m)
	 `(,m menu-item ,(format "%s" m) ,(ergoemacs-minor-mode-menu-from-indicator m t)))
       (let (ret)
	 (dolist (elt (append ergoemacs-mode--hidden-minor-modes
			      ergoemacs-mode--space-hidden-minor-modes))
	   (when (and (boundp elt) (symbol-value elt))
	     (push elt ret)))
	 ret)))))

(defun ergoemacs-minor-mode-alist ()
  "Get a list of the minor-modes"
  (let (ret)
    (dolist (a (reverse minor-mode-alist))
      (unless (memq (car a) ergoemacs-mode--hidden-minor-modes)
	(push a ret)))
    ret))

(defvar ergoemacs-mode--minor-modes-p t
  "Determine if `ergoemacs-mode--minor-modes' generates space")

(defvar ergoemacs-mode--minor-modes-available nil)
(defun ergoemacs-mode--minor-modes-available (mode-line face1 face2 &optional reduce)
  (let (lhs rhs center)
    (setq ergoemacs-mode--minor-modes-p nil)
    (unwind-protect
	(setq lhs (ergoemacs-mode-line--eval-lhs mode-line face1 face2 reduce)
	      rhs (ergoemacs-mode-line--eval-rhs mode-line face1 face2 reduce)
	      center (ergoemacs-mode-line--eval-center mode-line face1 face2))
      (setq ergoemacs-mode--minor-modes-p t
	    ergoemacs-mode--minor-modes-available (- (ergoemacs :width)
						     (+ (ergoemacs :width lhs)
							(ergoemacs :width rhs)
							(ergoemacs :width center)))))))
(defun ergoemacs-mode--minor-modes ()
  "Get minor modes"
  (if (not ergoemacs-mode--minor-modes-p) "     "
    (setq ergoemacs-mode--space-hidden-minor-modes nil)
    (let* ((width 0)
	   (ret (replace-regexp-in-string
		 " +$" ""
		 (concat
		  (mapconcat (lambda (mm)
			       (if (or (not (numberp ergoemacs-mode--minor-modes-available))
				       (< width ergoemacs-mode--minor-modes-available))
				   (let ((cur (propertize mm
							  'mouse-face 'mode-line-highlight
							  'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
							  'local-map (let ((map (make-sparse-keymap)))
								       (define-key map
									 [mode-line down-mouse-1]
									 (ergoemacs-mode--minor-mode-mouse 'minor 'menu mm))
								       (define-key map
									 [mode-line mouse-2]
									 (ergoemacs-mode--minor-mode-mouse 'minor 'help mm))
								       (define-key map
									 [mode-line down-mouse-3]
									 (ergoemacs-mode--minor-mode-mouse 'minor 'menu mm))
								       (define-key map
									 [header-line down-mouse-3]
									 (ergoemacs-mode--minor-mode-mouse 'minor 'menu mm))
								       map))))
				     (setq width (+ width (ergoemacs :width cur) 1))
				     (if (or (not (numberp ergoemacs-mode--minor-modes-available))
				       (< width ergoemacs-mode--minor-modes-available))
					 cur
				       (push (lookup-minor-mode-from-indicator mm) ergoemacs-mode--space-hidden-minor-modes)
				       ""))
				 (push (lookup-minor-mode-from-indicator mm) ergoemacs-mode--space-hidden-minor-modes)
				 ""))
			     (split-string (format-mode-line (ergoemacs-minor-mode-alist)))
			     " ")))))
      (when (or ergoemacs-mode--space-hidden-minor-modes
		(catch 'found
		  (dolist (elt ergoemacs-mode--hidden-minor-modes)
		    (when (and (boundp elt) (symbol-value elt))
		      (throw 'found t)))
		  nil))
	(setq ret (concat ret " "
			  (propertize (if (and (fboundp #'mode-icons-propertize-mode))
					  (mode-icons-propertize-mode "+" (list "+" #xf151 'FontAwesome))
					"+")
				      'mouse-face 'mode-line-highlight
				      'help-echo "Hidden Minor Modes\nmouse-1: Display hidden minor modes"
				      'local-map (let ((map (make-sparse-keymap)))
						   (define-key map [mode-line down-mouse-1] 'ergoemacs-minor-mode-hidden-menu)
						   (define-key map [mode-line down-mouse-3] 'ergoemacs-minor-mode-hidden-menu)
						   map)))))
      ret)))

(defun ergoemacs-mode-line--atom ()
  (setq ergoemacs-mode-line--lhs
	'(((ergoemacs-mode-line-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p t)
	  ((sml/compile-position-construct mode-line-position) :reduce 2 :pad b)
	  ((ergoemacs-mode-line-use-vc powerline-vc) :reduce 1 :pad r))
	ergoemacs-mode-line--center
	'(((ergoemacs-mode--minor-modes)  :reduce 4)
	  ((mode-icons--generate-narrow powerline-narrow "%n") :reduce 4)
	  (" " :reduce 4))
	ergoemacs-mode-line--rhs
	'((global-mode-string :pad r)
	  ((ergoemacs-mode-line-coding (lambda() (not (string= "undecided" (ergoemacs-mode-line--encoding)))) ergoemacs-mode-line--encoding) :pad b :reduce 2)
	  ((ergoemacs-mode-line-coding (lambda() (not (string= ":" (mode-line-eol-desc)))) mode-icons--mode-line-eol-desc mode-line-eol-desc) :pad l :reduce 2)
	  ((mode-icons--generate-major-mode-item powerline-major-mode) :pad l)))
  (force-mode-line-update))

(defun ergoemacs-mode-line--center ()
  (setq ergoemacs-mode-line--lhs
	'(((mode-icons--generate-major-mode-item powerline-major-mode) :pad b)
	  ((ergoemacs-mode-line-use-vc powerline-vc) :reduce 1 :pad r)
	  ((sml/compile-position-construct mode-line-position) :reduce 2 :pad b))
	ergoemacs-mode-line--center
	'(((ergoemacs-mode-line-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p r)
	  )
	ergoemacs-mode-line--rhs
	'((global-mode-string :pad r)
	  ((ergoemacs-mode-line-coding (lambda() (not (string= "undecided" (ergoemacs-mode-line--encoding)))) ergoemacs-mode-line--encoding) :pad b :reduce 2)
	  ((ergoemacs-mode-line-coding (lambda() (not (string= ":" (mode-line-eol-desc)))) mode-icons--mode-line-eol-desc mode-line-eol-desc) :pad l :reduce 2)
	  ((ergoemacs-mode--minor-modes)  :reduce 4)
	  ((mode-icons--generate-narrow powerline-narrow "%n") :reduce 4 :last-p t)))
  (force-mode-line-update))

(defun ergoemacs-mode-line--xah ()

  (setq ergoemacs-mode-line--lhs
	'(((ergoemacs-mode-line-read-only-status mode-icons--read-only-status "%*") :reduce 3 :pad l)
	  ((mode-line-buffer-identification) :last-p t :pad b)
	  (((lambda() (and (buffer-file-name) t)) mode-icons--modified-status "%1") :last-p t)
	  ((sml/compile-position-construct mode-line-position) :reduce 2 :pad b)
	  ;; ((ergoemacs-mode-line-use-vc powerline-vc) :reduce 1 :pad r)
	  ((mode-icons--generate-major-mode-item powerline-major-mode) :pad b))
	ergoemacs-mode-line--center nil
	ergoemacs-mode-line--rhs nil)
  (force-mode-line-update))

(ergoemacs-mode-line--center)

(defun ergoemacs-mode-line--eval-center (mode-line face1 _face2 &optional reduce)
  (ergoemacs-mode-line--stack ergoemacs-mode-line--center (list mode-line face1) 'center reduce))

(defun ergoemacs-mode-line--eval-lhs (mode-line face1 _face2 &optional reduce)
  (ergoemacs-mode-line--stack ergoemacs-mode-line--lhs (list mode-line face1) 'left reduce))

(defun ergoemacs-mode-line--eval-rhs (mode-line face1 _face2 &optional reduce)
  (ergoemacs-mode-line--stack ergoemacs-mode-line--rhs (list mode-line face1) 'right reduce))

(defun ergoemacs-mode-line--stack (mode-line-list face-list dir &optional reduction-level)
  "Stacks mode-line elements"
  (let ((face-i (if (eq dir 'center) 0 -1))
	(len (length face-list))
	(last-face (nth 0 face-list))
	cur-face final tmp
	(mode-line-list mode-line-list)
	last-reduced-p)
    (when (eq dir 'right)
      (setq mode-line-list (reverse mode-line-list)))
    (setq final (apply 'append
		       (mapcar
			(lambda(elt)
			   (unless (eq dir 'center)
			    (setq face-i (1+ face-i)))
			  (let* ((ifs (car elt))
				 (plist (cdr elt))
				 (reduce (plist-get plist :reduce))
				 tmp
				 ret)
			    (if (and reduce (integerp reduce)
				     reduction-level (integerp reduction-level)
				     (<= reduce reduction-level))
				(progn
				  (unless (eq dir 'center)
				    (setq face-i (- face-i 1)))
				  (setq last-reduced-p t)
				  nil)
			      (when (and (not (eq dir 'center)) (plist-get plist :last-p))
				(unless last-reduced-p
				  (setq face-i (- face-i 1)
					last-reduced-p nil)))
			      (setq cur-face (nth (mod face-i len) face-list))
			      (setq tmp (ergoemacs :mode-if ifs cur-face (plist-get plist :pad)))
			      (if (and tmp (stringp tmp) (string= (format-mode-line tmp) ""))
				  (if (or (eq dir 'center) (plist-get plist :last-p) last-reduced-p) nil
				    (setq face-i (- face-i 1))
				    nil)
				(setq last-reduced-p nil) 
				(push tmp ret)
				(unless (eq cur-face last-face)
				  (cond
				   ((eq dir 'left)
				    (push (ergoemacs :sep dir last-face cur-face) ret))
				   ((eq dir 'right)
				    (push (ergoemacs :sep dir cur-face last-face) ret))))
				(setq last-face cur-face))
			      ret)))
			mode-line-list))
	  cur-face (car (last face-list)))
    (unless (eq last-face cur-face)
      (setq final (append final
			  (list (cond
				 ((eq dir 'left)
				  (ergoemacs :sep dir last-face cur-face))
				 ((eq dir 'right)
				  (ergoemacs :sep dir cur-face last-face)))))))
    (dolist (elt (reverse final))
      (when elt
	(push elt tmp)))
    (setq final tmp)    
    (when (and final (eq dir 'center))
      (setq final (append (list (ergoemacs :sep-left (car (last face-list)) (nth 0 face-list)))
			  final
			  (list (ergoemacs :sep-right (nth 0 face-list) (car (last face-list)))))))
    (when (eq dir 'right)
      (setq final (reverse final)))
    (or final "")))

(defcustom ergoemacs-mode-extra-width 0
  "Extra width to add."
  :type 'integer
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-width-multiplier 1.0
  "Multiplier for width."
  :type 'number
  :group 'ergoemacs-mode)

(defvar ergoemacs-mode--pixel-width-p nil
  "Determines if the mode line tries to calculate width")

(defun ergoemacs-mode--eval-width (&optional what)
  (if ergoemacs-mode--pixel-width-p
      (ergoemacs-mode--eval-width-pixels what)
    (ergoemacs-mode--eval-width-col what)))

(defun ergoemacs-mode--eval-string-width-pixels (str)
  "Get string width in pixels."
  (with-current-buffer (get-buffer-create " *ergoemacs-eval-width*")
	(delete-region (point-min) (point-max))
	(insert str)
	(car (window-text-pixel-size nil (point-min) (point-max)))))

(defun ergoemacs-mode--eval-width-pixels (&optional what)
  "Get the width of the display in pixels."
  (ergoemacs-mode--eval-width-col what t))

(defun ergoemacs-mode--eval-width-col (&optional what pixels-p)
  "Eval width of WHAT, which is formated with `format-mode-line'.
When WHAT is nil, return the width of the window"
  (or (and what (apply
		 '+
		 (mapcar (lambda(x)
			   (let ((display (get-text-property 0 'display x)))
			     (if display
				 (car (image-size display pixels-p))
			       (if pixels-p
				   (ergoemacs-mode--eval-string-width-pixels x)
				 (string-width x)))))
			 (ergoemacs-mode-line--property-substrings (format-mode-line what) 'display))))
      (if pixels-p
	  (let ((width (ergoemacs-mode--eval-width-col))
		(cw (frame-char-width)))
	    (* cw width))
	(let ((width (window-width))
	      ;; (cw (frame-char-width))
	      tmp)
	  (when (setq tmp (window-margins))
	    (setq width (apply '+ width (list (or (car tmp) 0) (or (cdr tmp) 0)))))
	  (* ergoemacs-mode-width-multiplier (+ width ergoemacs-mode-extra-width))))))

(defvar ergoemacs-mode-line-max-reduction 4)
(defun ergoemacs-mode-line--eval ()
  ;; This will dynamically grow/fill areas
  (setq mode-icons-read-only-space nil
	mode-icons-show-mode-name t
	mode-icons-eol-text t
	mode-name (or (and (fboundp #'mode-icons-get-mode-icon)
			   (mode-icons-get-mode-icon (or mode-icons-cached-mode-name mode-name)))
		      mode-name))
  (let* ((active (or (and (fboundp #'powerline-selected-window-active) (powerline-selected-window-active)) t))
	 (mode-line (if active 'mode-line 'mode-line-inactive))
	 (face1 (if active 'powerline-active1 'powerline-inactive1))
	 (face2 (if active 'powerline-active2 'powerline-inactive2))
	 (mode-icons-read-only-space nil)
	 (mode-icons-show-mode-name t)
	 lhs rhs center
	 wlhs wrhs wcenter
	 available
	 (reduce-level 1))
    (ergoemacs-mode--minor-modes-available mode-line face1 face2)
    (setq lhs (ergoemacs-mode-line--eval-lhs mode-line face1 face2)
	  rhs (ergoemacs-mode-line--eval-rhs mode-line face1 face2)
	  center (ergoemacs-mode-line--eval-center mode-line face1 face2)
	  wlhs (ergoemacs :width lhs)
	  wrhs (ergoemacs :width rhs)
	  wcenter (ergoemacs :width center))
    (when (> (+ wlhs wrhs wcenter) (ergoemacs :width))
      (setq mode-icons-read-only-space nil
	    mode-icons-show-mode-name nil
	    mode-icons-eol-text nil
	    mode-name (or (and (fboundp #'mode-icons-get-mode-icon)
			   (mode-icons-get-mode-icon (or mode-icons-cached-mode-name mode-name)))
			  mode-name)
	    lhs (ergoemacs-mode--minor-modes-available mode-line face1 face2)
	    lhs (ergoemacs-mode-line--eval-lhs mode-line face1 face2)
	    rhs (ergoemacs-mode-line--eval-rhs mode-line face1 face2)
	    center (ergoemacs-mode-line--eval-center mode-line face1 face2)
	    wlhs (ergoemacs :width lhs)
	    wrhs (ergoemacs :width rhs)
	    wcenter (ergoemacs :width center))
      (while (and (<= reduce-level ergoemacs-mode-line-max-reduction)
		  (> (+ wlhs wrhs wcenter) (ergoemacs :width)))
	(setq mode-icons-read-only-space nil
	      mode-icons-show-mode-name nil
	      mode-icons-eol-text nil
	      lhs (ergoemacs-mode--minor-modes-available mode-line face1 face2)
	      lhs (ergoemacs-mode-line--eval-lhs mode-line face1 face2 reduce-level)
	      rhs (ergoemacs-mode-line--eval-rhs mode-line face1 face2 reduce-level)
	      center (ergoemacs-mode-line--eval-center mode-line face1 face2 reduce-level)
	      wlhs (ergoemacs :width lhs)
	      wrhs (ergoemacs :width rhs)
	      wcenter (ergoemacs :width center)
	      reduce-level (+ reduce-level 1))))
    (setq available (/ (- (ergoemacs :width) (+ wlhs wrhs wcenter)) 2))
    (if ergoemacs-mode--pixel-width-p
	(setq available (list available)))
    ;; (message "a: %s (%3.1f %3.1f %3.1f; %3.1f)" available wlhs wrhs wcenter (ergoemacs :width))
    (list lhs
	   (propertize " " 'display `((space :width ,available))
		       'face face1)
	   center
	   (propertize " " 'display `((space :width ,available))
		       'face face1)
	   rhs)))

(defun ergoemacs-mode-line--variable-pitch (&optional frame)
  (dolist (face '(mode-line mode-line-inactive
			    powerline-active1
			    powerline-inactive1
			    powerline-active2 powerline-inactive2))
    (set-face-attribute face frame
			  :family (face-attribute 'variable-pitch :family)
			  :foundry (face-attribute 'variable-pitch :foundry)
			  :height 125)))


(defvar ergoemacs-mode-line-front-space nil
  "Save `mode-line-front-space' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-mule-info nil
  "Save `mode-line-mule-info' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-client nil
  "Save `mode-line-client' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-modified nil
  "Save `mode-line-modified' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-remote nil
  "Save `mode-line-remote' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-frame-identification nil
  "Save `mode-line-frame-identification' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-buffer-identification nil
  "Save `mode-line-buffer-identification' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-position nil
  "Save `mode-line-position' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-modes nil
  "Save `mode-line-modes' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-misc-info nil
  "Save `mode-line-misc-info' for `ergoemacs-mode' restore.")

(defvar ergoemacs-mode-line-end-spaces nil
  "Save `mode-line-end-spaces' for `ergoemacs-mode' restore.")

(defun ergoemacs-mode-line-format (&optional restore)
  (if restore
      (set-default 'mode-line-format
		   ergoemacs-old-mode-line-format)
    (unless ergoemacs-old-mode-line-format
      (setq ergoemacs-old-mode-line-format mode-line-format))

    (setq-default mode-line-format
		  `("%e"
		    (:eval (ergoemacs-mode-line--eval))))
    (setq mode-line-format `("%e"
			     (:eval (ergoemacs-mode-line--eval))))
    (force-mode-line-update)))

(provide 'ergoemacs-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-lib.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
