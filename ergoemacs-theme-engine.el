;;; ergoemacs-theme-engine.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2018  Free Software Foundation, Inc.

;; Filename: ergoemacs-theme-engine.el
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

(eval-when-compile
  (require 'ergoemacs-macros)
  (require 'cl))

(defvar ergoemacs-mode)
(defvar ergoemacs-require)
(defvar ergoemacs-theme)
(defvar ergoemacs-theme-hash)
(defvar ergoemacs-theme-options)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-keymap)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs--start-emacs-state-2)
(defvar ergoemacs-dir)
(defvar ergoemacs-inkscape)

(declare-function ergoemacs-save "ergoemacs-lib")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")
(declare-function ergoemacs-component-struct--component-description "ergoemacs-component")
(declare-function ergoemacs-component-struct--versions "ergoemacs-component")
(declare-function ergoemacs-layouts--menu "ergoemacs-layouts")
(declare-function ergoemacs-component-at-point "ergoemacs-component")
(declare-function ergoemacs-component-find-1 "ergoemacs-component")
(declare-function ergoemacs-component--prompt "ergoemacs-component")
(declare-function ergoemacs-require "ergoemacs-lib")
(declare-function ergoemacs-command-loop--message "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")
(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-key-description--keymap "ergoemacs-key-description")
(declare-function ergoemacs-key-description--modifier "ergoemacs-key-description")
(declare-function ergoemacs-layouts--current "ergoemacs-layouts")
(declare-function ergoemacs-map--hashkey "ergoemacs-map")
(declare-function ergoemacs-translate--svg-quote "ergoemacs-translate")

(defun ergoemacs-theme-components--required-p (comp)
  "Is COMP a required component?"
  (let ((comp (or (and (stringp comp) (intern comp)) comp))
        e2)
    (catch 'found
      (dolist (r ergoemacs-require)
        (setq e2 (or (and (stringp (car r)) (intern (car r)))
                     (car r)))
        (when (eq comp e2)
          (throw 'found t)))
      nil)))

(defun ergoemacs-theme-components (&optional theme)
  "Get a list of components used for the current theme.
This respects `ergoemacs-theme-options'."
  (let* ((theme (or theme (ergoemacs :current-theme)))
         (theme-plist (ergoemacs-gethash theme ergoemacs-theme-hash))
         components opt first tmp required)
    (if (not theme)
        (error "Could not figure out the theme that you are trying to use...")
      (setq components (plist-get theme-plist :components))
      (while (and (< 1 (length components))
                  (ergoemacs-theme-components--required-p (nth 0 components)))
        (push (pop components) required))
      (when (and (< 1 (length components))
                 (symbolp (nth 1 components))
                 (setq tmp (symbol-name (nth 1 components)))
                 (< 5 (length tmp))
                 (string= "theme" (substring tmp -5)))
        (setq first (pop components)))
      
      (dolist (x (reverse (plist-get theme-plist :optional-off)))
        (let ((a (assoc x ergoemacs-theme-options)))
          (when a
            (setq a (car (cdr a)))
            (when (eq a 'on)
              (push x opt)))))
      (dolist (x (reverse (plist-get theme-plist :optional-on)))
        (let ((a (assoc x ergoemacs-theme-options)))
          (if (not a)
              (push x opt)
            (setq a (car (cdr a)))
            (when (or (not a) (eq a 'on))
              (push x opt)))))
      (setq components (append (reverse required) (reverse opt) components))
      (when first
        (push first components)))
    components))

;;;###autoload
(defun ergoemacs-theme-set-version (version)
  "Set the current themes default VERSION."
  (let (found)
    (setq ergoemacs-theme-version
          (mapcar
           (lambda(elt)
             (if (not (equal (ergoemacs :current-theme) (nth 0 elt)))
                 elt
               (setq found t)
               (list (ergoemacs :current-theme) version)))
           ergoemacs-theme-version))
    (unless found
      (push (list (ergoemacs :current-theme) version) ergoemacs-theme-version))))

;;;###autoload
(defun ergoemacs-theme-option-off (option &optional no-custom)
  "Turns OPTION off.
Uses `ergoemacs-theme-option-on'."
  (ergoemacs-theme-option-on option no-custom 'off))

;;;###autoload
(defun ergoemacs-theme-option-on (option &optional no-custom off)
  "Turns OPTION on.
When OPTION is a list turn on all the options in the list
If OFF is non-nil, turn off the options instead."
  (if (consp option)
      (dolist (new-option option)
        (let (ergoemacs-mode)
          (ergoemacs-theme-option-on new-option no-custom off)))
    (let* (found
           (tmp (mapcar
                 (lambda(elt)
                   (if (not (eq (nth 0 elt) option))
                       elt
                     (setq found t)
                     (if off
                         (list option 'off)
                       (list option 'on))))
                 ergoemacs-theme-options)))
      (unless found
        (push (if off (list option 'off) (list option 'on))
              tmp))
      (if no-custom
          (setq ergoemacs-theme-options tmp)
        (ergoemacs-save 'ergoemacs-theme-options tmp))))
  (when ergoemacs-mode
    (ergoemacs-mode-reset)))

;;;###autoload
(defun ergoemacs-theme-toggle-option (option)
  "Toggle theme OPTION."
  (if (ergoemacs-theme-option-enabled-p option)
      (ergoemacs-theme-option-off option)
    (ergoemacs-theme-option-on option)))


(defun ergoemacs-theme--list (&optional silent)
  "Gets the list of themes.
When SILENT is true, also include silent themes"
  (let (ret)
    ;; All this is done to copy lists so that sorts will not
    ;; destroy the final list.  Please keep this here so that errors
    ;; will not be introduced (seems silly)
    (setq ret (mapcar (lambda(x) x)
           (or (and silent
                    (append (ergoemacs-gethash "defined-themes" ergoemacs-theme-hash)
                            (ergoemacs-gethash "silent-themes" ergoemacs-theme-hash)))
               (ergoemacs-gethash "defined-themes" ergoemacs-theme-hash))))
    ret))

(defun ergoemacs-theme--custom-documentation (&optional themes ini)
  "Get list of all known layouts and their documentation.

THEMES is the list of themes for the customize documentation.

INI is provided for initilazation, to shorten the descriptions."
  (let ((themes (or themes (sort (ergoemacs-theme--list) 'string<))))
    (mapconcat
     (lambda(theme)
       (if ini
           (concat theme "=" (plist-get (ergoemacs-gethash theme ergoemacs-theme-hash) :description))
         (concat "\"" theme "\" - " (plist-get (ergoemacs-gethash theme ergoemacs-theme-hash) :description))))
     themes "\n")))

(defun ergoemacs-theme--customization-type ()
  "Gets the customization types for `ergoemacs-theme'."
  `(choice
    (const :tag "Standard" :value nil)
    ,@(mapcar
       (lambda(elt)
         `(const :tag ,elt :value ,elt))
       (sort (ergoemacs-theme--list t) 'string<))))

(defun ergoemacs-theme--regexp (&optional at-end)
  "Return a regexp of `ergoemacs-mode' themes.
When AT-END is non-nil, append a $ to the regular expression."
  (let (ret)
    (setq ret (regexp-opt (ergoemacs-theme--list t) 'symbols))
    (when at-end
      (setq ret (concat ret "$")))
    ret))

(defun ergoemacs-theme-option-enabled-p (option)
  "Determines if OPTION is enabled."
  (let* ((plist (ergoemacs-gethash (ergoemacs :current-theme) ergoemacs-theme-hash))
         (options-on (plist-get plist :optional-on))
         (options-off (plist-get plist :optional-off))
         (required (plist-get plist :components)))
    (or (member option required)
        (and (member option options-on)
             (not (member (list option 'off) ergoemacs-theme-options)))
        (and (member option options-off)
             (member (list option 'on) ergoemacs-theme-options)))))


(defun ergoemacs-theme--menu-options (theme)
  "Gets the options menu for THEME."
  (let ((plist (ergoemacs-gethash theme ergoemacs-theme-hash))
        (menu-list '())
        (menu-pre '())
        (options-on '())
        (options-off '())
        (menu-options '())
        (options-list '())
        (options-alist '())
        (i 0))
    (setq options-on (plist-get plist ':optional-on)
          options-off (plist-get plist ':optional-off)
          menu-list (plist-get plist ':options-menu))
    (if (= 0 (length (append options-on options-off))) nil
      (dolist (elt (reverse menu-list))
        (let ((menu-name (nth 0 elt))
              (menu-items (nth 1 elt))
              desc
              (ret '()))
          (dolist (option (reverse menu-items))
            (when (memq option (append options-on options-off))
              (setq desc (ergoemacs-component-struct--component-description (symbol-name option)))
              (push option menu-options)
              (push
               `(,option
                 menu-item ,desc
                 (lambda()
                   (interactive)
                   (ergoemacs-theme-toggle-option ',option)
                   (customize-mark-as-set 'ergoemacs-theme-options)
                   (ergoemacs-mode-reset))
                 :button (:toggle . (ergoemacs-theme-option-enabled-p ',option)))
               ret)))
          (unless (eq ret '())
            (setq ret
                  `(,(intern (format "options-menu-%s" i))
                    menu-item ,menu-name
                    (keymap ,@ret)))
            (setq i (+ i 1))
            (push ret menu-pre))))
      (dolist (option (append options-on options-off))
        (unless (member option menu-options)
          (let ((desc (ergoemacs-component-struct--component-description (symbol-name option))))
            (push desc options-list)
            (push (list desc option) options-alist))))
      `(ergoemacs-theme-options
        menu-item "Theme Options"
        (keymap
         ,@menu-pre
         ,@(mapcar
            (lambda(desc)
              (let ((option (car (cdr (assoc desc options-alist)))))
                `(,option
                  menu-item ,desc
                  (lambda()
                    (interactive)
                    (ergoemacs-theme-toggle-option ',option)
                    (customize-mark-as-set 'ergoemacs-theme-options)
                    (ergoemacs-mode-reset))
                  :button (:toggle . (ergoemacs-theme-option-enabled-p ',option)))))
            (sort options-list 'string<)))))))

(defun ergoemacs-theme--get-version ()
  "Get the current version for the current theme."
  (let ((theme-ver (assoc (ergoemacs :current-theme) ergoemacs-theme-version)))
    (if (not theme-ver) nil
      (car (cdr theme-ver)))))


(defun ergoemacs-theme--version-menu (theme)
  "Get version menu for THEME."
  (let ((theme-versions (ergoemacs-component-struct--versions (ergoemacs-theme-components theme))))
    (if (not theme-versions) nil
      `(ergoemacs-versions
        menu-item "Theme Versions"
        (keymap
         (ergoemacs-current-version
          menu-item "Current Version"
          (lambda()
            (interactive)
            (ergoemacs-theme-set-version nil)
            (customize-mark-as-set 'ergoemacs-theme-version)
            (ergoemacs-mode-reset))
          :button (:radio . (equal (ergoemacs :current-version) nil)))
         ,@(mapcar
            (lambda(version)
              `(,(intern version) menu-item ,version
                (lambda() (interactive)
                  (ergoemacs-theme-set-version ,version)
                  (customize-mark-as-set 'ergoemacs-theme-version)
                  (ergoemacs-mode-reset))
                :button (:radio . (equal (ergoemacs :current-version) ,version))))
            theme-versions))))))

(defun ergoemacs-theme--menu (theme)
  "Define menus for current THEME."
  `(keymap
    ,(ergoemacs-layouts--menu)
    (ergoemacs-theme-sep "--")
    (ergoemacs-themes
     menu-item "Themes"
     (keymap
      ,@(mapcar
         (lambda(theme)
           `(,(intern theme) menu-item ,(concat theme " - " (plist-get (ergoemacs-gethash theme ergoemacs-theme-hash) ':description))
             (lambda() (interactive)
               (ergoemacs-save 'ergoemacs-theme ,theme))
             :button (:radio . (string= (ergoemacs :current-theme) ,theme))))
         (sort (ergoemacs-theme--list) 'string<))))
    ,(ergoemacs-theme--menu-options theme)
    ,(ergoemacs-theme--version-menu theme)
    (ergoemacs-c-x-sep "--")
    (ergoemacs-c-x-c-c
     menu-item "Ctrl+C and Ctrl+X behavior"
     (keymap
      (c-c-c-x-emacs
       menu-item "Ctrl+C and Ctrl+X are for Emacs Commands"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)))
      (c-c-c-x-cua
       menu-item "Ctrl+C and Ctrl+X are only Copy/Cut"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)))
      (c-c-c-x-both
       menu-item "Ctrl+C and Ctrl+X are both Emacs Commands & Copy/Cut"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-handle-ctl-c-or-ctl-x 'both))
       :button (:radio . (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)))
      (c-c-c-x-timeout
       menu-item "Customize Ctrl+C and Ctrl+X Cut/Copy Timeout"
       (lambda() (interactive)
         (ergoemacs-save 'ergoemacs-ctl-c-or-ctl-x-delay)))))
    (c-v
     menu-item "Paste behavior"
     (keymap
      (c-v-multiple
       menu-item "Repeating Paste pastes multiple times"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-smart-paste nil))
       :button (:radio . (eq ergoemacs-smart-paste 'nil)))
      (c-v-cycle
       menu-item "Repeating Paste cycles through previous pastes"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-smart-paste t))
       :button (:radio . (eq ergoemacs-smart-paste 't)))
      (c-v-kill-ring
       menu-item "Repeating Paste starts browse-kill-ring"
       (lambda()
         (interactive)
         (ergoemacs-save 'ergoemacs-smart-paste 'browse-kill-ring))
       :enable (commandp 'browse-kill-ring)
       :button (:radio . (eq ergoemacs-smart-paste 'browse-kill-ring)))))
    (ergoemacs-sep-bash "--")
    (ergoemacs-bash
     menu-item "Make Bash aware of ergoemacs keys"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-theme-create-bash)))
    (ergoemacs-ahk
     menu-item "Make Windows aware of ergoemacs keys (Requires Autohotkey)"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-gen-ahk)))
    (ergoemacs-sep-menu "--")
    (ergoemacs-cheat
     menu-item "Generate/Open Key binding Cheat Sheet"
     (lambda()
       (interactive)
       (call-interactively 'ergoemacs-display-current-theme)))

    (ergoemacs-save
     menu-item "Save Settings for Future Sessions"
     (lambda ()
       (interactive)
       (ergoemacs-exit-customize-save-customized)))

    (ergoemacs-reset-cache
     menu-item "Reset ergoemacs-mode cache"
     (lambda ()
       (interactive)
       (ergoemacs-mode-clear-cache)))
    
    (ergoemacs-customize
     menu-item "Customize ErgoEmacs"
     (lambda ()
       (interactive)
       (customize-group 'ergoemacs-mode)))
    (ergoemacs-mode-exit
     menu-item "Exit ergoemacs-mode"
     (lambda() (interactive) (ergoemacs-mode -1)))))

(defun ergoemacs-theme-at-point ()
  "Get the `ergoemacs-theme' defined at or before point.
Return 0 if there is no such symbol.  Uses
`ergoemacs-component-at-point'."
  (ergoemacs-component-at-point t))

(defcustom ergoemacs-theme-find-regexp
  (concat"^\\s-*(ergoemacs-theme" find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp to search for a component definition.

This is used by `ergoemacs-find-theme'.

Note it must contain a `%s' at the place where `format'
should insert the face name."
  :type 'regexp
  :group 'find-function
  :version "22.1")

(unless (assoc 'ergoemacs-theme find-function-regexp-alist)
  (push (cons 'ergoemacs-theme 'ergoemacs-theme-find-regexp) find-function-regexp-alist))

(define-button-type 'ergoemacs-theme-help
  :supertype 'help-xref
  'help-function #'ergoemacs-theme-describe
  'help-echo (purecopy "mouse-2, RET: describe this ergoemacs theme"))

(define-button-type 'ergoemacs-theme-def
  :supertype 'help-xref
  'help-function #'ergoemacs-theme-find-definition
  'help-echo (purecopy "mouse-2, RET: find this ergoemacs theme's definition"))

(defvar ergoemacs-theme--svg-list nil)

(defun ergoemacs-theme-find-definition (theme)
  "Find the definition of THEME.  THEME defaults to the name near point.

Finds the `ergoemacs-mode' containing the definition of the component
near point (selected by `ergoemacs-theme-at-point') in a buffer and
places point before the definition.

Set mark before moving, if the buffer already existed.

The library where FACE is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (list (ergoemacs-theme-at-point)))
  (ergoemacs-component-find-1 theme 'ergoemacs-theme 'switch-to-buffer))

(defun ergoemacs-theme-describe (theme)
  "Display the full documentation of THEME (a symbol or string)."
  (interactive (ergoemacs-component--prompt t))
  (let* ((theme (and theme
                     (or (and (stringp theme) theme)
                         (and (symbolp theme) (symbol-name theme)))))
         (plist (ergoemacs-gethash (or theme "") ergoemacs-theme-hash))
         (file (plist-get plist :file))
         (el-file (and (stringp file) (concat (file-name-sans-extension file) ".el")))
         (old-theme ergoemacs-theme)

	 (key (concat theme "-" ergoemacs-keyboard-layout "-" (symbol-name (ergoemacs-map--hashkey ergoemacs--start-emacs-state-2))))
         required-p
         svg png tmp)
    (if (not plist)
        (message "You did not specify a valid ergoemacs theme %s" theme)
      (if current-prefix-arg
	  (setq svg (ergoemacs-theme--svg theme nil t)
                 png (ergoemacs-theme--png theme nil t))
	(setq svg (ergoemacs-theme--svg theme)
	      png (ergoemacs-theme--png theme)))
      (help-setup-xref (list #'ergoemacs-theme-describe (or theme ""))
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (or theme ""))
          ;; Use " is " instead of a colon so that
          ;; it is easier to get out the function name using forward-sexp.
          (insert " is an `ergoemacs-mode' theme")
          (when (and el-file (file-readable-p el-file))
            (insert " defined in `")
            (insert (file-name-nondirectory el-file))
            (insert "'.")
            (save-excursion
              (when (re-search-backward "`\\(.*\\)'" nil t)
                (help-xref-button 1 'ergoemacs-theme-def theme))))
          (insert "\n\n")
          (insert "Documentation:\n")
          (insert (plist-get plist :description))
          (insert "\n\n")
          (insert "Diagram:\n")
          (cond
           ((and (image-type-available-p 'png)
                 (car png)
                 (file-exists-p (car png)))
            
            (insert-image (create-image (car png)))
            (insert "\n"))
           ((and (car svg)
                 (file-exists-p (car svg)) (image-type-available-p 'svg))
            (insert-image (create-image (car svg)))
            (insert "\n")))
          (if (and (car png) (file-exists-p (car png)))
              (insert "[svg] [png]")
            (insert "[svg]"))
          (beginning-of-line)
          (if (looking-at "\\(\\[svg\\]\\) \\(\\[png\\]\\)")
              (progn
                (help-xref-button 1 'help-url (car svg))
                (help-xref-button 2 'help-url (car png)))
            (if (looking-at "\\(\\[svg\\]\\)")
                (help-xref-button 1 'help-url (car svg))))
          (goto-char (point-max))
	  (when ergoemacs-theme--svg-list
	    (insert "\n")
	    (dolist (elt ergoemacs-theme--svg-list)
	      (when (string= key (nth 0 elt))
		(insert (ergoemacs-key-description (nth 1 elt)) ":\n")
                (cond
                 ((and (image-type-available-p 'png)
                       (nth 2 elt)
                       (file-exists-p (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt))))
                  (insert-image (create-image (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt))))
                  (insert "\n"))
                 ((and (image-type-available-p 'svg)
                       (nth 2 elt)
                       (file-exists-p (nth 2 elt)))
                  (insert-image (create-image (nth 2 elt)))
                  (insert "\n")))
                (when (file-exists-p (nth 2 elt))
                  (insert "[svg]")
                  (when (looking-back "\\(\\[svg\\]\\)" nil)
                    (help-xref-button 1 'help-url (nth 2 elt))))
                (when (file-exists-p (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt)))
                  (insert " [png]")
                  (when (looking-back "\\(\\[png\\]\\)" nil)
                    (help-xref-button 1 'help-url (replace-regexp-in-string "[.]svg\\'" ".png" (nth 2 elt)))))
                (insert "\n\n"))))
          (insert "\n\n")
          (when (setq tmp (plist-get plist :based-on))
            (when (eq (car tmp) 'quote)
              (setq tmp (car (cdr tmp))))
            (insert (format "This theme is based on: %s\n\n" tmp))
            (when (looking-back "on: \\(.*\\)\n\n" nil)
              (help-xref-button 1 'ergoemacs-theme-help (match-string 1))))

          (when (member theme (ergoemacs-gethash "silent-themes" ergoemacs-theme-hash))
            (insert (format "This theme does not appear in menus because of the :silent option.\n\n")))

          (setq required-p t)
          (dolist (elt '((:components . "Applied Components (from `ergoemacs-require')")
                         (:components . "Theme Required Components")
                         (:optional-on . "Optional Components (enabled by default)")
                         (:optional-off . "Optional Components (disabled by default)")))
            (when (setq tmp (plist-get plist (car elt)))
              (insert (cdr elt))
              (princ ":\n")
              (dolist (comp tmp)
                (when (or (and (eq (car elt) :components)
                               (or (and required-p (memq comp (mapcar (lambda(x) (car x)) ergoemacs-require)))
                                   (and (not required-p) (not (memq comp (mapcar (lambda(x) (car x)) ergoemacs-require))))))
                          (not (eq (car elt) :components)))
                  (insert (format " - %s -- " comp))
                  (when (looking-back "- \\(.*\\) -- " nil)
                    (help-xref-button 1 'ergoemacs-component-help (match-string 1)))
                  (insert (format "%s (currently %s)\n"
                                  (ergoemacs-component-struct--component-description comp)
                                  (or (and (ergoemacs-theme-option-enabled-p comp)
                                           "enabled") "disabled")))
                  ))
              (insert "\n"))
            (setq required-p nil))
          
          (insert "\n\n")
          (if (equal (format "%s" old-theme) (format "%s" theme))
              (ergoemacs-key-description--keymap ergoemacs-keymap t)
            (unwind-protect
                (progn
                  (setq ergoemacs-theme theme)
                  (ergoemacs-mode-reset)
                  (ergoemacs-key-description--keymap ergoemacs-keymap t))
              (setq ergoemacs-theme old-theme)
              (ergoemacs-mode-reset)))
          (buffer-string))))))

(defalias 'describe-ergoemacs-theme 'ergoemacs-theme-describe)

(defvar ergoemacs-theme-create-bash-functions
  '((backward-char)
    (forward-char)
    (previous-history)
    (next-history)
    (beginning-of-line ergoemacs-beginning-of-line-or-what)
    (end-of-line ergoemacs-end-of-line-or-what)
    (backward-word subward-backward backward-sexp)
    (forward-word subword-forward forward-sexp)
    (kill-line ergoemacs-cut-line-or-region)
    (backward-kill-word)
    (kill-word)
    (backward-delete-char)
    (delete-char)
    (undo undo-tree-undo)
    (kill-region ergoemacs-cut-line-or-region)
    (copy-region-as-kill ergoemacs-copy-line-or-region)
    (yank ergoemacs-paste)
    (forward-search-history isearch-forward)
    (reverse-search-history isearch-backward)))

;;;###autoload
(defun ergoemacs-theme-create-bash ()
  "Create bash ~/.inputrc for use with bash."
  (interactive)
  (let ((ret "# Based on Brendan Miller's initial bash .inputrc
# INSTALL
# to install, rename this file to just \".inputrc\"
# place this file in your home dir. e.g. ~/.inputrc
# restart your terminal. Then, bash's keybinding for editing
# should be like ErgoEmacs.
# If no key works, try replace all \\e to \\M-. That's means change Esc to Meta key.
\nset editing-mode emacs") tmp)
    (with-temp-buffer
      (dolist (cmds ergoemacs-theme-create-bash-functions)
        (dolist (cmd cmds)
          (when (setq tmp (where-is-internal cmd nil t))
            (setq ret (concat ret "\n\"\\" (key-description tmp) "\": "
                              (symbol-name (nth 0 cmds)))))) t))
    (with-temp-file "~/.inputrc"
      (insert ret)
      (insert "\n"))
    (message "Wrote current ergoemacs bindings to ~/.inputrc")))

;;;###autoload
(defalias 'ergoemacs-bash 'ergoemacs-theme-create-bash)


(defcustom ergoemacs-function-short-names
      '(
        (abort-recursive-edit "Abort Edit")
        (ace-jump-mode "Ace Jump")
        (backward-char  "← char")
        (backward-kill-word "⌫ word")
        (backward-paragraph "↑ ¶")
        (backward-word "← word")
        (bm-next "Next Bookmark")
        (bm-toggle "Toggle bookmark")
        (comment-dwim "cmt dwim")
        (count-words-region "Count Words")
        (cua-set-mark "Set Mark")
        (delete-backward-char "⌫ char")
        (delete-char "⌦ char")
        (delete-frame "x Frame")
        (delete-indentation "⌧ indentation")
        (delete-other-windows "x other pane")
        (delete-other-windows "x other pane")
        (delete-window "x pane")
        (delete-window "x pane")
        (digit-argument "#Argument")
        (er/contract-region "→ region ←")
        (er/expand-region "←region→")
        (er/expand-region "←region→")
        (er/mark-outside-quotes "←quote→")
        (ergoemacs-backward-block "← ¶")
        (ergoemacs-backward-open-bracket "← bracket")
        (ergoemacs-beginning-of-line-or-what "← line/*")
        (ergoemacs-beginning-or-end-of-buffer "↑ Top*")
        (ergoemacs-call-keyword-completion "↯ compl")
        (ergoemacs-close-current-buffer "x Close Buffer")
        (ergoemacs-command-loop--universal-argument "Argument")
        (ergoemacs-compact-uncompact-block "fill/unfill ¶")
        (ergoemacs-copy-all "copy all")
        (ergoemacs-copy-all "copy all")
        (ergoemacs-copy-line-or-region "copy")
        (ergoemacs-ctl-c "Copy/Ctl+c")
        (ergoemacs-ctl-x "Cut/Ctl+x")
        (ergoemacs-cut-all "✂ all")
        (ergoemacs-cut-all "✂ all")
        (ergoemacs-cut-line-or-region "✂ region")
        (ergoemacs-delete-frame "Close Frame")
        (ergoemacs-end-of-line-or-what "→ line/*")
        (ergoemacs-end-or-beginning-of-buffer "↓ Bottom*")
        (ergoemacs-extend-selection "←region→")
        (ergoemacs-extend-selection "←region→")
        (ergoemacs-forward-block  "→ ¶")
        (ergoemacs-forward-close-bracket "→ bracket")
        (ergoemacs-kill-line-backward "⌫ line")
        (ergoemacs-move-cursor-next-pane "next pane")
        (ergoemacs-move-cursor-previous-pane "prev pane")
        (ergoemacs-new-empty-buffer "New")
        (ergoemacs-open-in-external-app "OS Open")
        (ergoemacs-open-last-closed "Open Last Closed")
        (ergoemacs-org-edit-src "Edit Source")
        (ergoemacs-paste "paste")
        (ergoemacs-paste-cycle "paste ↑")
        (ergoemacs-print-buffer-confirm "Print")
        (ergoemacs-read-key--universal-argument "Argument")
        (ergoemacs-select-current-block "Sel. Block")
        (ergoemacs-select-current-line "Sel. Line")
        (ergoemacs-select-text-in-quote "←quote→")
        (ergoemacs-shrink-whitespaces "⌧ white")
        (ergoemacs-switch-to-next-frame "next frame")
        (ergoemacs-switch-to-previous-frame "prev frame")
        (ergoemacs-text-scale-normal-size "Reset Zoom")
        (ergoemacs-toggle-camel-case "tog. camel")
        (ergoemacs-toggle-letter-case "tog. case")
        (ergoemacs-unchorded-alt-modal "Alt+ Mode")
        (ergoemacs-universal-argument "Argument")
        (execute-extended-command "M-x")
        (find-file "Open")
        (flyspell-auto-correct-word "flyspell")
        (forward-char "→ char")
        (forward-paragraph "↓ ¶")
        (forward-word "→ word")
        (goto-line "Goto line")
        (ido-write-file "Save As")
        (indent-for-tab-command "↹Tab")
        (indent-region "indent-region")  ;; Already in CUA
        (insert-parentheses "()")
        (isearch-backward "← isearch")
        (isearch-backward-regexp "← reg isearch")
        (isearch-forward "→ isearch")
        (isearch-forward-regexp "→ reg isearch")
        (keyboard-quit "Stop Command")
        (kill-line "⌦ line")
        (kill-word "⌦ word")
        (left-word  "← word")
        (make-frame-command "New Frame")
        (mark-paragraph "Sel ¶")
        (mark-whole-buffer "Sel All")
        (mc/edit-lines "Edit Lines")
        (mc/mark-next-like-this "Mark Next")
        (menu-bar-open "Menu bar")
        (move-past-close-and-reindent "→) ↹Tab")
        (negative-argument "-Argument")
        (newline-and-indent "Enter↵ Tab↹")
        (next-line "↓ line")
        (pr-interface)
        (previous-line "↑ line")
        (query-replace "rep")
        (query-replace "rep")
        (query-replace-regexp "rep reg")
        (recenter-top-bottom "recenter")
        (redo "↷ redo")
        (revert-buffer "Revert")
        (right-word "→ word")
        (save-buffer "Save")
        (scroll-down "↑ page")
        (scroll-down-command "↑ page")
        (scroll-up "↓ page")
        (scroll-up-command "↓ page")
        (set-mark-command "Set Mark")
        (shell-command "shell cmd")
        (split-window-below "split —")
        (split-window-horizontally "split —")
        (split-window-right "split |")
        (split-window-vertically "split —")
        (switch-to-buffer "Switch Buffer")
        (text-scale-decrease "Zoom Out")
        (text-scale-increase "Zoom In")
        (undo "↶ undo")
        (undo-tree-redo "↷ redo")
        (universal-argument "Argument")
        (vr/query-replace "rep reg")
        (write-file "Save As"))
  "Ergoemacs short command names."
  :group 'ergoemacs-themes
  :type '(repeat :tag "Command abbreviation"
                 (list (sexp :tag "Command")
                       (string :tag "Short Name"))))

(defvar ergoemacs-theme-remove-prefixes
  '("kmacro" "ergoemacs" "help" "w32")
  "When replacing functions, remove the namespaces listed here.")

(defvar ergoemacs-theme-replacements
  '(("view" "🔎")
    ("lookup" "🔎")
    ("view" "🔎")
    ("display" "🔎")
    ("-" " ")
    ("describe" "📖")
    ("about" "📖"))
  "What unicode characters should unknown functions be replaced with?")

(defvar ergoemacs-theme--svg nil)
(defvar ergoemacs-theme--svg-prefixes nil)
(defvar ergoemacs-theme--svg-prefix nil)


(defun ergoemacs-theme--svg-elt-nonabbrev (what)
  "Replace WHAT with ergoemacs abbreviation of function."
  (let (ret)
    (cond
     ((eq what 'ergoemacs-map-undefined) "")
     ((symbolp what)
      (setq ret (replace-regexp-in-string
                 (format "^%s-" (regexp-opt ergoemacs-theme-remove-prefixes t)) ""
                 (format "%s" what)))
      (dolist (v ergoemacs-theme-replacements)
        (setq ret (replace-regexp-in-string (nth 0 v) (nth 1 v) ret)))
      (when (<= 10 (length ret))
        (setq ret (concat (substring ret 0 10) "…")))
      ret)
     (t ""))))

(defun ergoemacs-theme--svg-elt (elt theme layout lay)
  "Handle ELT"
  (ergoemacs-translate--svg-quote
   (let (key binding no-push-p)
     (cond
      ((integerp elt) (nth elt layout))
      ((and (listp elt) (or (integerp (car elt))
                            (stringp (car elt))))
       (if (stringp (car elt))
           (setq key "f#")
         (setq key (nth (car elt) layout)))
       (if (string= key "") ""
         (if (string= key "f#")
             (setq key (aref (read-kbd-macro (concat "<" (downcase (car elt)) ">")) 0))
           (setq key (string-to-char key)))
         (if (eq (nth 1 elt) 'apps)
             (if ergoemacs-theme--svg-prefix
                 (setq key (vector key))
               (setq key (vconcat (or (and (eq system-type 'windows-nt) [apps]) [menu]) (vector key))))
           (setq key (vector (event-convert-list (append (cdr elt) (list key))))))
         (setq no-push-p nil)
         (when (equal key [27])
           (setq no-push-p t))
         (when ergoemacs-theme--svg-prefix
           (setq key (vconcat ergoemacs-theme--svg-prefix key)))
         (setq binding (lookup-key ergoemacs-keymap key))
         (when (integerp binding)
           (setq binding nil))
         (or (and binding
                  (ergoemacs-keymapp binding)
                  (or (and (not no-push-p) (push key ergoemacs-theme--svg-prefixes))
                      no-push-p)
                  "⌨")
             (and binding
                  (setq key (assoc binding ergoemacs-function-short-names))
                  (nth 1 key))
             (and binding
                  (ergoemacs-theme--svg-elt-nonabbrev binding))
             "")))
      ((memq elt '(meta control))
       (concat (ergoemacs-key-description--modifier elt) (format " - Emacs %s" elt)))
      ((memq elt '(meta-shift control-shift))
       (setq elt (intern (replace-regexp-in-string "-shift" "" (symbol-name elt))))
       (concat (ergoemacs-key-description--modifier elt)
               (ergoemacs-key-description--modifier 'shift)
               (format " - Emacs %s shift" elt)))
      ((eq elt 'apps)
       (if ergoemacs-theme--svg-prefix
           "Key without any modifiers"
         "▤ Menu/Apps"))
      ((eq elt 'title)
       (concat theme " (" lay ")"
               (or (and ergoemacs-theme--svg-prefix (concat " for " (ergoemacs-key-description ergoemacs-theme--svg-prefix)))
                    "")))
      (t (setq key (format "%s" elt))
         (when (<= 10 (length key))
           (setq key (concat (substring key 0 10) "…")))
         key)))))

(defun ergoemacs-theme--svg (&optional theme layout full-p reread)
  "Creates SVG based THEME and  LAYOUT"
  (save-excursion
    (let* ((lay (or layout ergoemacs-keyboard-layout))
           (theme (or theme ergoemacs-theme))
           (layout (symbol-value (ergoemacs :layout  lay)))
           (file-dir (expand-file-name "bindings" (expand-file-name "eurgoemacs-extras" user-emacs-directory)))
           (file-name (expand-file-name (concat theme "-" lay "-" (symbol-name (ergoemacs-map--hashkey ergoemacs--start-emacs-state-2)) ".svg") file-dir))
           (reread reread)
           (old-theme ergoemacs-theme)
           (old-layout ergoemacs-keyboard-layout)
           pt ret)
      (if (and file-name (file-exists-p file-name) (not reread) (or (not full-p) ergoemacs-theme--svg-list))
          (progn
            (setq ret (file-expand-wildcards (expand-file-name (concat theme "-" lay "-*-" (symbol-name (ergoemacs-map--hashkey ergoemacs--start-emacs-state-2)) ".svg") file-dir)))
            (push file-name ret)
            ret)
        (unless (and (equal theme old-theme)
                     (equal lay old-layout))
          (setq ergoemacs-theme theme
                ergoemacs-keyboard-layout lay)
          (ergoemacs-mode-reset))
        (unwind-protect
            (progn
              (when (eq reread :svg)
                (setq reread nil))
              (when reread
                (setq ergoemacs-theme--svg nil))
              (unless (and file-dir (file-exists-p file-dir))
                (make-directory file-dir t))
              (unless ergoemacs-theme--svg
                (with-temp-buffer
                  (insert-file-contents (expand-file-name "kbd-ergo.svg" ergoemacs-dir))
                  (goto-char (point-min))
                  (setq pt (point))
                  (while (re-search-forward ">\\([TMCAN][SMCA]?\\|title\\)\\(F?[0-9]+\\|-SPC\\|\\)<" nil t)
                    (push (buffer-substring pt (match-beginning 0)) ergoemacs-theme--svg)
                    (cond
                     ((and (string= "T" (match-string 1)))
                      (push (string-to-number (match-string 2)) ergoemacs-theme--svg))
                     ((and (string= "M" (match-string 1)))
                      (push (list (string-to-number (match-string 2)) 'meta) ergoemacs-theme--svg))
                     ((and (string= "C" (match-string 1)))
                      (push (list (string-to-number (match-string 2)) 'control) ergoemacs-theme--svg))
                     ((and (string= "A" (match-string 1)))
                      (push (list (string-to-number (match-string 2)) 'apps) ergoemacs-theme--svg))
                     ((string= "title" (match-string 1))
                      (push 'title ergoemacs-theme--svg))
                     ((string= "N" (match-string 1))
                      (push (list (match-string 2)) ergoemacs-theme--svg))
                     ((string= "MM" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'meta ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list 32 'meta) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'meta) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'meta) ergoemacs-theme--svg))))
                     ((string= "MS" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'meta-shift ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list 32 'meta 'shift) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'meta 'shift) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'meta 'shift) ergoemacs-theme--svg))))
                     ((string= "CS" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'control-shift ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list 32 'control 'shift) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'control 'shift) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'control 'shift) ergoemacs-theme--svg))))
                     ((string= "CC" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'control ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list 32 'control) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'control) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'control) ergoemacs-theme--svg))))
                     ((string= "AA" (match-string 1))
                      (cond
                       ((string= "" (match-string 2))
                        (push 'apps ergoemacs-theme--svg))
                       ((string= "-SPC" (match-string 2))
                        (push (list 32 'apps) ergoemacs-theme--svg))
                       ((string-match-p "^F" (match-string 2))
                        (push (list (match-string 2) 'apps) ergoemacs-theme--svg))
                       (t
                        (push (list (string-to-number (match-string 2)) 'control) ergoemacs-theme--svg))))
                     (t (push nil ergoemacs-theme--svg)))
                    (setq pt (match-end 0)))
                  (push (buffer-substring pt (point-max)) ergoemacs-theme--svg))
                (setq ergoemacs-theme--svg (reverse ergoemacs-theme--svg)))
              (setq ergoemacs-theme--svg-prefixes nil
                    ergoemacs-theme--svg-prefix nil)
              (with-temp-file file-name
                (dolist (w ergoemacs-theme--svg)
                  (cond
                   ((stringp w)
                    (insert w))
                   (t
                    (insert ">" (ergoemacs-theme--svg-elt w theme layout lay) "<")))))
              (push file-name ret)
              (unless full-p
                (setq ergoemacs-theme--svg-prefixes nil))
              (while ergoemacs-theme--svg-prefixes
                (setq ergoemacs-theme--svg-prefix (pop ergoemacs-theme--svg-prefixes)
                      file-name (expand-file-name (concat ergoemacs-theme "-" lay "-"
                                                          (replace-regexp-in-string "[^A-Za-z0-9-]+" "_" (key-description ergoemacs-theme--svg-prefix))
                                                          "-" (symbol-name (ergoemacs-map--hashkey ergoemacs--start-emacs-state-2)) ".svg") file-dir))
 		(push (list (concat ergoemacs-theme "-" lay "-" (symbol-name (ergoemacs-map--hashkey ergoemacs--start-emacs-state-2)))
			    ergoemacs-theme--svg-prefix file-name) ergoemacs-theme--svg-list)
                (ergoemacs :spinner '("%s→%s" "%s->%s") (ergoemacs-key-description ergoemacs-theme--svg-prefix) file-name)
                (with-temp-file file-name
                  (dolist (w ergoemacs-theme--svg)
                    (cond
                     ((stringp w)
                      (insert w))
                     (t
                      (insert ">" (ergoemacs-theme--svg-elt w theme layout lay) "<")))))
                (push file-name ret)))
          (unless (and (equal theme old-theme)
                       (equal lay old-layout))
            (setq ergoemacs-theme old-theme
                  ergoemacs-keyboard-layout old-layout)
            (ergoemacs-mode-reset)))
        ret))))

(defvar ergoemacs-theme--png nil)
(defvar ergoemacs-theme--png-last nil)
(defun ergoemacs-theme--png--process (&rest _ignore)
  "Process the `ergoemacs-theme--png' list to convert svg files
to png files."
  (when (or (not ergoemacs-theme--png-last)
	    (file-exists-p ergoemacs-theme--png-last)
	    ;; Reset variables
	    (and (message "PNG generation failed. Abort creating png files.")
		 (setq ergoemacs-theme--png nil
		       ergoemacs-theme--png-last nil)))
    (save-excursion
      (let* ((png-info (pop ergoemacs-theme--png))
	     process)
	(if (not png-info)
	    (progn
	      (ergoemacs-command-loop--message "Done creating png files.")
	      ;; FIXME: Update images...
	      )
	  
	  (ergoemacs :spinner "%s" (nth 0 png-info))
	  (setq process (start-process-shell-command
			 "ergoemacs-png-convert" "*ergoemacs-theme-png-convert*"
			 (nth 1 png-info))
		ergoemacs-theme--png-last (nth 2 png-info))
	  (set-process-sentinel process 'ergoemacs-theme--png--process))))))

(defun ergoemacs-theme--png (&optional theme layout full-p reread)
  "Get png file for layout, or create one.
Requires `ergoemacs-inkscape' to be specified."
  (let* ((svg-files (ergoemacs-theme--svg theme layout full-p reread))
         png-file ret)
    (dolist (svg-file svg-files)
      (setq png-file (concat (file-name-sans-extension svg-file) ".png"))
      (if (and png-file (file-exists-p png-file) (not reread)) (push png-file ret)
        (if (and ergoemacs-inkscape (file-readable-p ergoemacs-inkscape))
            (progn
              (push (list (format "%s->%s" (file-name-nondirectory svg-file) (file-name-nondirectory png-file))
                          (format "%s -z -f \"%s\" -e \"%s\"" ergoemacs-inkscape svg-file png-file)
			  png-file) ergoemacs-theme--png)
              (push png-file ret))
          (message "Need inkscape and to specify inkscape location with `ergoemacs-inkscape'.")
          nil)))
    (ergoemacs-theme--png--process)
    ret))

(provide 'ergoemacs-theme-engine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-theme-engine.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
