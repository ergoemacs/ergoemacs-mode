;;; ergoemacs-theme-engine.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

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

(defvar ergoemacs-theme)
(defvar ergoemacs-mode)
(defvar ergoemacs-theme-version)
(defvar ergoemacs-theme-options)

(declare-function ergoemacs-save "ergoemacs-lib")
(declare-function ergoemacs-mode-reset "ergoemacs-lib")
(declare-function ergoemacs-component-struct--component-description "ergoemacs-component")
(declare-function ergoemacs-component-struct--versions "ergoemacs-component")
(declare-function ergoemacs-layouts--menu "ergoemacs-layouts")

(defvar ergoemacs-theme-hash (make-hash-table :test 'equal)
  "Hash of `ergoemacs-mode' themes")

(defun ergoemacs-theme-components (&optional theme)
  "Get a list of components used for the current theme.
This respects `ergoemacs-theme-options'."
  (let* ((theme (or theme (ergoemacs :current-theme)))
         (theme-plist (gethash theme ergoemacs-theme-hash))
         components)
    (if (not theme)
        (error "Could not figure out the theme that you are trying to use...")
      (setq components (reverse (plist-get theme-plist ':components)))
      (dolist (x (reverse (plist-get theme-plist ':optional-on)))
        (let ((a (assoc x ergoemacs-theme-options)))
          (if (not a)
              (push x components)
            (setq a (car (cdr a)))
            (when (or (not a) (eq a 'on))
              (push x components)))))
      (dolist (x (reverse (plist-get theme-plist ':optional-off)))
        (let ((a (assoc x ergoemacs-theme-options)))
          (when a
            (setq a (car (cdr a)))
            (when (eq a 'on)
              (push x components)))))
      (setq components (reverse components)))
    components))

;;;###autoload
(defun ergoemacs-theme-set-version (version)
  "Sets the current themes default VERSION"
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
  (if (eq (type-of option) 'cons)
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
  "Toggles theme OPTION."
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
    (setq ret
          (mapcar
           (lambda(x)
             x)
           (or (and silent
                    (append (gethash "defined-themes" ergoemacs-theme-hash)
                            (gethash "silent-themes" ergoemacs-theme-hash)))
               (gethash "defined-themes" ergoemacs-theme-hash))))
    ret))

(defun ergoemacs-theme-option-enabled-p (option)
  "Determines if OPTION is enabled."
  (let ((plist (gethash (ergoemacs :current-theme) ergoemacs-theme-hash))
        options-on options-off)
    (setq options-on (plist-get plist ':optional-on)
          options-off (plist-get plist ':optional-off))
    (or (and (member option options-on)
             (not (member (list option 'off) ergoemacs-theme-options)))
        (and (member option options-off)
             (member (list option 'on) ergoemacs-theme-options)))))


(defun ergoemacs-theme--menu-options (theme)
  "Gets the options menu for THEME."
  (let ((plist (gethash theme ergoemacs-theme-hash))
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

(defun ergoemacs-theme-get-version ()
  "Gets the current version for the current theme"
  (let ((theme-ver (assoc (ergoemacs :current-theme) ergoemacs-theme-version)))
    (if (not theme-ver) nil
      (car (cdr theme-ver)))))


(defun ergoemacs-theme--version-menu (theme)
  "Gets version menu for THEME"
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
  "Defines menus for current THEME."
  `(keymap
    ,(ergoemacs-layouts--menu)
    (ergoemacs-theme-sep "--")
    (ergoemacs-themes
     menu-item "Themes"
     (keymap
      ,@(mapcar
         (lambda(theme)
           `(,(intern theme) menu-item ,(concat theme " - " (plist-get (gethash theme ergoemacs-theme-hash) ':description))
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
       :enable (condition-case err (interactive-form 'browse-kill-ring)
                 (error nil))
       :button (:radio . (eq ergoemacs-smart-paste 'browse-kill-ring)))))
    (ergoemacs-sep-bash "--")
    (ergoemacs-bash
     menu-item "Make Bash aware of ergoemacs keys"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-bash)))
    (ergoemacs-ahk
     menu-item "Make Windows aware of ergoemacs keys (Requires Autohotkey)"
     (lambda () (interactive)
       (call-interactively 'ergoemacs-gen-ahk)))
    (ergoemacs-sep-menu "--")
    (ergoemacs-cheat
     menu-item "Generate/Open Key binding Cheat Sheet"
     (lambda()
       (interactive)
       (call-interactively 'ergoemacs-display-current-svg)))
    ;; (ergoemacs-menus
    ;;  menu-item "Use Menus"
    ;;  (lambda() (interactive)
    ;;    ;; (ergoemacs-save 'ergoemacs-use-menus (not ergoemacs-use-menus))
    ;;    ;; (if ergoemacs-use-menus
    ;;    ;;     (progn
    ;;    ;;       (require 'ergoemacs-menus)
    ;;    ;;       (ergoemacs-menus-on))
    ;;    ;;   (when (featurep 'ergoemacs-menus)
    ;;    ;;     (ergoemacs-menus-off)))
    ;;    )
    ;;  :button (:radio . ergoemacs-use-menus))
    (ergoemacs-save
     menu-item "Save Settings for Future Sessions"
     (lambda ()
       (interactive)
       (ergoemacs-exit-customize-save-customized)))
    (ergoemacs-customize
     menu-item "Customize ErgoEmacs"
     (lambda ()
       (interactive)
       (customize-group 'ergoemacs-mode)))
    (ergoemacs-mode-exit
     menu-item "Exit ergoemacs-mode"
     (lambda() (interactive) (ergoemacs-mode -1)))))

(provide 'ergoemacs-theme-engine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-theme-engine.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
