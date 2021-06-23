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

(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(defvar mode-icons-show-mode-name)
(defvar mode-icons-read-only-space)
(defvar mode-icons-cached-mode-name)
(defvar mode-icons-eol-text)
(defvar ergoemacs-excluded-major-modes)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-mode)
(defvar ergoemacs-mode-names)
(defvar ergoemacs-require)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-timing-hash)

(defvar ergoemacs-dir)


(declare-function ergoemacs-next-emacs-buffer "ergoemacs-functions")
(declare-function ergoemacs-next-user-buffer "ergoemacs-functions")
(declare-function ergoemacs-previous-emacs-buffer "ergoemacs-functions")
(declare-function ergoemacs-previous-user-buffer "ergoemacs-functions")

(declare-function mode-icons-get-mode-icon "mode-icons")

(declare-function ergoemacs-autoloadp "ergoemacs-macros")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-theme-option-on "ergoemacs-theme-engine")

(declare-function ergoemacs-key-description--menu "ergoemacs-key-description")

(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-translate--ahk-ini "ergoemacs-translate")
(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")

(declare-function ergoemacs-component-find-definition "ergoemacs-component")
(declare-function ergoemacs-component-find-1 "ergoemacs-component")

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
  (unless (member (list option theme type remove) ergoemacs-require)
    (push (list option theme type remove) ergoemacs-require))
  (if ergoemacs-require--ini-p
      (if (eq (type-of option) 'cons)
          (dolist (new-option option)
            (let (ergoemacs-mode)
              (ergoemacs-require new-option theme type)))
        (let ((option-sym
               (or (and option (stringp option) (intern option)) option))
              (theme "standard")
              )
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
                     ergoemacs-theme-hash)
            )
          )
        )
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
  "Gets a list of language modes known to `ergoemacs-mode'.
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
    `(keymap ,@ret)))

;;;###autoload
(defun ergoemacs-gen-ahk (&optional all)
  "Generates autohotkey for all layouts and themes"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
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
        (insert (ergoemacs-translate--ahk-ini all)))
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

(provide 'ergoemacs-lib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-lib.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
