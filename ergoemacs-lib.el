;;; ergoemacs-lib.el --- Ergoemacs libraries -*- lexical-binding: t -*-

;; Copyright © 2013-2018  Free Software Foundation, Inc.

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

(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

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
        (cl-pushnew variable ergoemacs-set-ignore-customize)
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
          (cl-pushnew variable ergoemacs-set-ignore-customize)
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
          (cl-pushnew variable ergoemacs-set-ignore-customize)
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
        (cl-pushnew variable ergoemacs-set-ignore-customize)
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
        (cl-pushnew variable ergoemacs-set-ignore-customize)
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
        (cl-pushnew variable ergoemacs-set-ignore-customize)
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
      (if (consp option)
          (dolist (new-option option)
            (let (ergoemacs-mode)
              (ergoemacs-require new-option theme type)))
        (let ((option-sym
               (or (and option (stringp option) (intern option)) option)))
          (dolist (theme (or (and theme (or (and (consp theme) theme) (list theme)))
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
                          elt)
                    ret))
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
                          (cdr elt))
                    ret))
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
           (and (cl-caddr item) (stringp (cl-caddr item)))
           (symbolp (cl-cadddr item))
           (not (ergoemacs-keymapp (cl-cadddr item))))
      ;; Look if this item already has a :keys property
      (if (catch 'found-keys
            (dolist (i item)
              (when (eq i :keys)
                (throw 'found-keys t))) nil) nil
        (ergoemacs-menu--filter-key-shortcut (cl-cadddr item) keymap))
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

(provide 'ergoemacs-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-lib.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
