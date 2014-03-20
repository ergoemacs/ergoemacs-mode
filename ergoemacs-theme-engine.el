;;; ergoemacs-theme-engine.el --- Engine for ergoemacs-themes
;; 
;; Filename: ergoemacs-theme-engine.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Mar 20 10:41:30 2014 (-0500)
;; Version: 
;; Package-Requires: ()
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(defvar ergoemacs-full-maps '(helm-map)
  "List of keymaps where the full ergoemacs keymap is fully
 installed (ie they use an overriding keymap).")

(defgroup ergoemacs-themes nil
  "Default Ergoemacs Layout"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-theme-options
  '()
  "List of theme options"
  :type '(repeat
          (list
           (sexp :tag "Theme Component")
           (choice
            (const :tag "Force Off" 'off)
            (const :tag "Force On" 'on)
            (const :tag "Let theme decide" nil))))
  :group 'ergoemacs-themes)

(defcustom ergoemacs-function-short-names
  '((backward-char  "← char")
    (forward-char "→ char")
    (previous-line "↑ line")
    (next-line "↓ line")
    (left-word  "← word")
    (right-word "→ word")
    (backward-paragraph "↑ ¶")
    (forward-paragraph "↓ ¶")
    (backward-word "← word")
    (forward-word "→ word")
    (ergoemacs-backward-block "← ¶")
    (ergoemacs-forward-block  "→ ¶")
    (ergoemacs-beginning-of-line-or-what "← line/*")
    (ergoemacs-end-of-line-or-what "→ line/*")
    (scroll-down "↑ page")
    (scroll-down-command "↑ page")
    (scroll-up-command "↓ page")
    (scroll-up "↓ page")
    (ergoemacs-beginning-or-end-of-buffer "↑ Top*")
    (ergoemacs-end-or-beginning-of-buffer "↓ Bottom*")
    (ergoemacs-backward-open-bracket "← bracket")
    (ergoemacs-forward-close-bracket "→ bracket")
    (isearch-forward "→ isearch")
    (isearch-backward "← isearch")
    (recenter-top-bottom "recenter")
    (delete-backward-char "⌫ char")
    (delete-char "⌦ char")
    (backward-kill-word "⌫ word")
    (kill-word "⌦ word")
    (ergoemacs-cut-line-or-region "✂ region")
    (ergoemacs-copy-line-or-region "copy")
    (ergoemacs-paste "paste")
    (ergoemacs-paste-cycle "paste ↑")
    (ergoemacs-copy-all "copy all")
    (ergoemacs-cut-all "✂ all")
    (undo-tree-redo "↷ redo")
    (redo "↷ redo")
    (undo "↶ undo")
    (kill-line "⌦ line")
    (ergoemacs-kill-line-backward "⌫ line")
    (mark-paragraph "Mark Paragraph")
    (ergoemacs-shrink-whitespaces "⌧ white")
    (comment-dwim "cmt dwim")
    (ergoemacs-toggle-camel-case "tog. camel")
    (ergoemacs-toggle-letter-case "tog. case")
    (ergoemacs-call-keyword-completion "↯ compl")
    (flyspell-auto-correct-word "flyspell")
    (ergoemacs-compact-uncompact-block "fill/unfill ¶")
    (set-mark-command "Set Mark")
    (execute-extended-command "M-x")
    (shell-command "shell cmd")
    (ergoemacs-move-cursor-next-pane "next pane")
    (ergoemacs-move-cursor-previous-pane "prev pane")
    (ergoemacs-switch-to-previous-frame "prev frame")
    (ergoemacs-switch-to-next-frame "next frame")
    (query-replace "rep")
    (vr/query-replace "rep reg")
    (query-replace-regexp "rep reg")
    (delete-other-windows "x other pane")
    (delete-window "x pane")
    (split-window-vertically "split |")
    (split-window-right "split |")
    (split-window-horizontally "split —")
    (split-window-below "split —")
    (er/expand-region "←region→")
    (ergoemacs-extend-selection "←region→")
    (er/expand-region "←region→")
    (ergoemacs-extend-selection "←region→")
    (er/mark-outside-quotes "←quote→")
    (ergoemacs-select-text-in-quote "←quote→")
    (ergoemacs-select-current-block "Sel. Block")
    (ergoemacs-select-current-line "Sel. Line")
    (ace-jump-mode "Ace Jump")    (delete-window "x pane")
    (delete-other-windows "x other pane")
    (split-window-vertically "split —")
    (query-replace "rep")
    (ergoemacs-cut-all "✂ all")
    (ergoemacs-copy-all "copy all")
    (execute-extended-command "M-x")
    (execute-extended-command "M-x")
    (indent-region "indent-region")  ;; Already in CUA
    (set-mark-command "Set Mark")
    (mark-whole-buffer "Sel All"))
  "Ergoemacs short command names"
  :group 'ergoemacs-themes
  :type '(repeat :tag "Command abbreviation"
                 (list (sexp :tag "Command")
                       (string :tag "Short Name"))))

(defun ergoemacs--parse-keys-and-body (keys-and-body &optional is-theme)
  "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body.

This is stole directly from ert by Christian Ohler <ohler@gnu.org>

Afterward it was modified for use with `ergoemacs-mode'. In
particular it:
- `define-key' is converted to `ergoemacs-theme-component--define-key' and keymaps are quoted
- `global-set-key' is converted to `ergoemacs-theme-component--global-set-key'
- `global-unset-key' is converted to `ergoemacs-theme-component--global-set-key'
- `global-reset-key' is converted `ergoemacs-theme-component--global-reset-key'
- Allows :version statement expansion
- Adds with-hook syntax or (when -hook)
"
  (let ((extracted-key-accu '())
        (debug-on-error t)
        last-was-version
        plist
        (remaining keys-and-body))
    ;; Allow
    ;; (component name)
    (unless (or (keywordp (first remaining)) (boundp 'skip-first))
      (if (condition-case nil
              (stringp (first remaining))
            (error nil))
          (push `(:name . ,(pop remaining)) extracted-key-accu)
        (push `(:name . ,(symbol-name (pop remaining))) extracted-key-accu))
      (when (memq (type-of (first remaining)) '(symbol cons))
        (pop remaining))
      (when (stringp (first remaining))
        (push `(:description . ,(pop remaining)) extracted-key-accu)))
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
    ;; Now change remaining (define-key keymap key def) to
    ;; (define-key 'keymap key def)
    ;; Also change (with-hook hook-name ) to (let ((ergoemacs-hook 'hook-name)))
    (unless is-theme
      (setq remaining
            (mapcar
             (lambda(elt)
               (cond
                (last-was-version
                 (setq last-was-version nil)
                 (if (stringp elt)
                     `(when (boundp 'component-version) (setq component-version ,elt))
                   `(when (boundp 'component-version) (setq component-version ,(symbol-name elt)))))
                ((condition-case nil
                     (eq elt ':version)
                   (error nil))
                 (setq last-was-version t)
                 nil)
                ((condition-case err
                     (eq (nth 0 elt) 'global-reset-key))
                 `(ergoemacs-theme-component--global-reset-key ,(nth 1 elt)))
                ((condition-case err
                     (eq (nth 0 elt) 'global-unset-key))
                 `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) nil))
                ((condition-case err
                     (eq (nth 0 elt) 'global-set-key))
                 (if (keymapp (nth 2 elt))
                     `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) (quote ,(nth 2 elt)))
                   `(ergoemacs-theme-component--global-set-key ,(nth 1 elt) ,(nth 2 elt))))
                ((condition-case err
                     (eq (nth 0 elt) 'define-key))
                 (if (equal (nth 1 elt) '(current-global-map))
                     (if (keymapp (nth 3 elt))
                         `(ergoemacs-theme-component--global-set-key ,(nth 2 elt) (quote ,(nth 3 elt)))
                       `(ergoemacs-theme-component--global-set-key ,(nth 2 elt) ,(nth 3 elt)))
                   (if (keymapp (nth 3 elt))
                       `(ergoemacs-theme-component--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) (quote ,(nth 3 elt)))
                     `(ergoemacs-theme-component--define-key (quote ,(nth 1 elt)) ,(nth 2 elt) ,(nth 3 elt)))))
                ((or (condition-case err
                         (eq (nth 0 elt) 'with-hook))
                     (and (condition-case err
                              (eq (nth 0 elt) 'when))
                          (condition-case err
                              (string-match "-hook$" (symbol-name (nth 1 elt))))))
                 (let (tmp skip-first)
                   (setq tmp (ergoemacs--parse-keys-and-body (cdr (cdr elt))))
                   `(let ((ergoemacs-hook (quote ,(nth 1 elt)))
                          (ergoemacs-hook-modify-keymap
                           ,(or (plist-get (nth 0 tmp)
                                           ':modify-keymap)
                                (plist-get (nth 0 tmp)
                                           ':modify-map)))
                          (ergoemacs-hook-full-shortcut-map
                           ,(or (plist-get (nth 0 tmp)
                                           ':full-shortcut-keymap)
                                (plist-get (nth 0 tmp)
                                           ':full-shortcut-map)
                                (plist-get (nth 0 tmp)
                                           ':full-map)
                                (plist-get (nth 0 tmp)
                                           ':full-keymap)))
                          (ergoemacs-hook-always ,(plist-get (nth 0 tmp)
                                                             ':always)))
                      ,@(nth 1 tmp)
                      (quote ,(nth 1 elt)) ,(nth 2 elt) ,(nth 3 elt))))
                (t elt)))
             remaining)))
    (setq plist (loop for (key . value) in extracted-key-accu
                      collect key
                      collect value))
    (list plist remaining)))

(defvar ergoemacs-theme-component-hash (make-hash-table :test 'equal))
(defun ergoemacs-theme-component--version-bump ()
  (when (and (boundp 'component-version)
             component-version
             (boundp 'component-version-minor-mode-layout)
             (boundp 'component-version-curr)
             (boundp 'fixed-layout) (boundp 'variable-layout)
             (boundp 'redundant-keys) (boundp 'defined-keys)
             (boundp 'versions)
             (boundp 'just-first-reg)
             (not (equal component-version-curr component-version)))
    ;; Create/Update component-version fixed or variable layouts.
    (when component-version-curr
      (push (list component-version-curr
                  component-version-fixed-layout
                  component-version-variable-layout
                  component-version-redundant-keys
                  component-version-minor-mode-layout) component-version-list))
    (setq component-version-curr component-version)
    (push component-version-curr versions)
    (unless component-version-minor-mode-layout
      (setq component-version-minor-mode-layout (symbol-value 'component-version-minor-mode-layout)))
    (unless component-version-fixed-layout
      (setq component-version-fixed-layout (symbol-value 'fixed-layout)))
    (unless component-version-fixed-layout
      (setq component-version-variable-layout (symbol-value 'variable-layout)))
    (unless component-version-redundant-keys
      (setq component-version-redundant-keys (symbol-value 'redundant-keys)))))

(defun ergoemacs-theme-component--global-reset-key (key)
  "Reset KEY.
will take out KEY from `component-version-redundant-keys'"
  (when (and (boundp 'component-version)
             component-version
             (boundp 'component-version-curr)
             (boundp 'fixed-layout) (boundp 'variable-layout)
             (boundp 'redundant-keys) (boundp 'defined-keys)
             (boundp 'versions)
             (boundp 'component-version-redundant-keys)
             (boundp 'just-first-reg))
    (ergoemacs-theme-component--version-bump)
    (let ((kd (key-description key))
          tmp)
      (setq tmp '())
      (mapc
       (lambda(x)
         (unless (string= x kd)
           (push x tmp)))
       component-version-redundant-keys)
      (setq component-version-redundant-keys tmp))))

(defun ergoemacs-theme-component--global-set-key (key command)
  "Setup ergoemacs theme component internally.
When fixed-layout and variable-layout are bound"
  (cond
   ((and (boundp 'component-version)
         component-version
         (boundp 'component-version-curr)
         (boundp 'fixed-layout) (boundp 'variable-layout)
         (boundp 'redundant-keys) (boundp 'defined-keys)
         (boundp 'versions)
         (boundp 'just-first-reg))
    (ergoemacs-theme-component--version-bump)
    (let* ((kd (key-description key)) cd jf removed
           (variable-p (and (boundp 'variable-reg)
                            variable-reg
                            (condition-case nil
                                (string-match variable-reg kd)
                              (error nil)))))
      (when cd
        (setq cd (car (cdr cd))))
      (if (not command)
          (mapc ;; Remove command from lists.
           (lambda(y)
             (let (tmp '())
               (mapc
                (lambda(x)
                  (unless (equal (nth 0 x) kd)
                    (push x tmp)))
                (symbol-value y))
               (set y tmp)))
           '(component-version-fixed-layout component-version-variable-layout))
        (if (not variable-p)
            (progn ;; Fixed Layout component
              (message "Change/Add Fixed")
              (setq component-version-fixed-layout
                    (mapcar
                     (lambda(x)
                       (if (not (equal (nth 0 x) kd))
                           x
                         (setq removed t)
                         (list kd command cd)))
                     component-version-fixed-layout))
              (unless removed
                (push (list kd command cd) component-version-fixed-layout)))
          ;; (push (list kd command) defined-keys)
          (setq jf (and (boundp 'just-first-reg) just-first-reg
                        (condition-case nil
                            (string-match just-first-reg kd)
                          (error nil))))
          (setq kd (ergoemacs-kbd kd t jf))
          (setq component-version-variable-layout
                (mapcar
                 (lambda(x)
                   (if (not (equal (nth 0 x) kd))
                       x
                     (setq removed t)
                     (list kd command cd jf)))
                 component-version-variable-layout))
          (unless removed
            (push (list kd command cd jf) component-version-variable-layout))))))
   ((and (boundp 'fixed-layout) (boundp 'variable-layout)
         (boundp 'component-version)
         (not component-version)
         (boundp 'redundant-keys) (boundp 'defined-keys))
    (let ((kd (key-description key)) cd jf)
      (if (not command) ; redundant key
          (push kd redundant-keys)
        (setq cd (assoc command ergoemacs-function-short-names)) ; Short key description
        (when cd
          (setq cd (car (cdr cd))))
        (if (not (condition-case nil
                     (string-match variable-reg kd)
                   (error nil)))
            (push (list kd command cd) fixed-layout) ;; Fixed layout component
          (push (list kd command) defined-keys)
          (setq jf (and just-first-reg
                        (condition-case nil
                            (string-match just-first-reg kd)
                          (error nil))))
          (setq kd (ergoemacs-kbd kd t jf))
          (push (list kd command cd jf) variable-layout)))))))

(defun ergoemacs-theme-component--define-key (keymap key def)
  "Setup mode-specific information."
  (when (and (boundp 'fixed-layout) (boundp 'variable-layout))
    (if (memq keymap '(global-map ergoemacs-keymap))
        (if (and (eq keymap 'ergoemacs-keymap) (not def))
            (ergoemacs-theme-component--global-set-key key 'ignore)
          (ergoemacs-theme-component--global-set-key key def))
      (let* ((hook (or
                    (and (boundp 'ergoemacs-hook) ergoemacs-hook)
                    (intern (if (string-match "mode" (symbol-name keymap))
                                (replace-regexp-in-string "mode.*" "mode-hook" (symbol-name keymap))
                              ;; Assume -keymap or -map defines -mode-hook
                              (string-match "(key)?map" "mode-hook" (symbol-name keymap))))))
             (modify-keymap-p
              (and (boundp 'ergoemacs-hook-modify-keymap)
                   ergoemacs-hook-modify-keymap))
             (full-shortcut-p
              (and (boundp 'ergoemacs-hook-full-shortcut-map)
                   ergoemacs-hook-full-shortcut-map))
             (always-run-p (and (boundp 'ergoemacs-hook-always)
                                ergoemacs-hook-always))
             (kd (key-description key))
             (variable-p (and (boundp 'variable-reg)
                              variable-reg
                              (condition-case nil
                                  (string-match variable-reg kd)
                                (error nil))))
             a-key
             jf found-1-p found-2-p)
        (when (boundp 'minor-mode-hook-list)
          (add-to-list 'minor-mode-hook-list hook nil 'eq))
        (when variable-p
          (setq variable-p t)
          (setq jf (and just-first-reg
                        (condition-case nil
                            (string-match just-first-reg kd)
                          (error nil))))
          (setq kd (ergoemacs-kbd kd t jf)))
        (cond
         ((and (boundp 'component-version)
               component-version
               (boundp 'component-version-curr)
               (boundp 'fixed-layout) (boundp 'variable-layout)
               (boundp 'redundant-keys) (boundp 'defined-keys)
               (boundp 'versions)
               (boundp 'just-first-reg))
          (ergoemacs-theme-component--version-bump) ;; Change version information
          )
         ((and (boundp 'fixed-layout) (boundp 'variable-layout)
               (boundp 'component-version)
               (not component-version)
               (boundp 'redundant-keys) (boundp 'defined-keys))
          ;; Keymaps modified are stored as (hook (keymaps))
          ;; Keys are stored as ((hook keymap/t variable-p) ((key def)))
          (setq a-key (list hook (if modify-keymap-p keymap t) variable-p))
          (setq minor-mode-layout
                (mapcar
                 (lambda(elt)
                   (cond
                    ((eq (car elt) hook)
                     (let ((lst (car (cdr elt))))
                       (add-to-list 'lst (if modify-keymap-p keymap t) nil 'eq)
                       (setq found-1-p t)
                       (list hook lst)))
                    ((equal (car elt) a-key)
                     (let ((lst (car (cdr elt))) new-lst)
                       (mapc
                        (lambda(elt-2)
                          (cond
                           ((equal (car elt-2) kd)
                            (setq found-2-p t)
                            (push (list kd def jf) new-lst))
                           (t
                            (push elt-2 new-lst))))
                        lst)
                       (unless found-2-p
                         (push (list kd def) new-lst))
                       (setq found-2-p t)
                       (list a-key new-lst always-run-p full-shortcut-p)))
                    (t
                     elt)))
                 minor-mode-layout))
          (unless found-1-p
            (push (list hook (list (if modify-keymap-p keymap t))) minor-mode-layout))
          (unless found-2-p
            (push (list a-key (list (list kd def)) always-run-p full-shortcut-p) minor-mode-layout))))))))


(defun ergoemacs-theme-component--define-key-in-keymaps (keymap keymap-shortcut key def)
  "Defines KEY in KEYMAP or KEYMAP-SHORTCUT to be DEF.
Similar to `define-key'.

DEF can be:
1. A function; If globally defined, this is defined by
   `ergoemacs-shortcut-remap'
2. A list of functions
3. A keymap
4. A kbd-code that this shortcuts to with `ergoemacs-read'

"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (if (condition-case err
              (stringp (nth 0 def))
            (error nil))
          (progn
            (when (boundp 'shortcut-list)
              (push (list (read-kbd-macro (key-description key) t)
                          `(,(nth 0 def) ,(nth 1 def)))
                    shortcut-list))
            (define-key keymap-shortcut key 'ergoemacs-shortcut))
        (mapc
         (lambda(new-def)
           (unless found
             (when (condition-case err
                       (interactive-form new-def)
                     (error nil))
               (setq found
                     (ergoemacs-define-key keymap key new-def)))))
         def))
      (symbol-value 'found)))
   ((condition-case err
        (interactive-form def)
      (error nil))
    (cond
     ;; only setup on `ergoemacs-shortcut-keymap' when setting up
     ;; ergoemacs default keymap.
     ((memq def '(ergoemacs-ctl-c ergoemacs-ctl-x))
      (define-key keymap-shortcut key def))
     ((and (not (string-match "\\(mouse\\|wheel\\)" (key-description key)))
           (ergoemacs-shortcut-function-binding def))
      (when (boundp 'shortcut-list)
        (push (list (read-kbd-macro (key-description key) t)
                    (list def 'global)) shortcut-list))
      (if (ergoemacs-is-movement-command-p def)
          (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
              (define-key keymap-shortcut key 'ergoemacs-shortcut-movement-no-shift-select)
            (define-key keymap-shortcut key 'ergoemacs-shortcut-movement))
        (define-key keymap-shortcut key 'ergoemacs-shortcut)))
     (t
      (define-key keymap key def)))
    t)
   ((condition-case err
        (keymapp (symbol-value def))
      (error nil))
    (define-key keymap key (symbol-value def))
    t)
   ((condition-case err
        (stringp def)
      (error nil))
    (progn
      (when (boundp 'shortcut-list)
        (push (list (read-kbd-macro (key-description key) t)
                    `(,def nil)) shortcut-list))
      (if (ergoemacs-is-movement-command-p def)
          (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
              (define-key keymap-shortcut key 'ergoemacs-shortcut-movement-no-shift-select)
            (define-key keymap-shortcut key 'ergoemacs-shortcut-movement))
        (define-key keymap-shortcut key 'ergoemacs-shortcut)))
    t)
   (t nil)))

(defcustom ergoemacs-prefer-variable-keybindings t
  "Prefer Variable keybindings over fixed keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-theme-component-get-closest-version (version version-list)
  "Return the closest version to VERSION in VERSION-LIST.
Formatted for use with `ergoemacs-theme-component-hash' it will return ::version or an empty string"
  (if version-list
      (let ((use-version (version-to-list version))
            biggest-version
            biggest-version-list
            smallest-version
            smallest-version-list
            best-version
            best-version-list
            test-version-list
            ret)
        (mapc
         (lambda (v)
           (setq test-version-list (version-to-list v))
           (if (not biggest-version)
               (setq biggest-version v
                     biggest-version-list test-version-list)
             (when (version-list-< biggest-version-list test-version-list)
               (setq biggest-version v
                     biggest-version-list test-version-list)))
           (if (not smallest-version)
               (setq smallest-version v
                     smallest-version-list test-version-list)
             (when (version-list-< test-version-list smallest-version-list)
               (setq smallest-version v
                     smallest-version-list test-version-list)))
           (cond
            ((and (not best-version)
                  (version-list-<= test-version-list use-version))
             (setq best-version v
                   best-version-list test-version-list))
            ((and (version-list-<= best-version-list test-version-list) ;; Better than best 
                  (version-list-<= test-version-list use-version))
             (setq best-version v
                   best-version-list test-version-list))))
         version-list)
        (if (version-list-< biggest-version-list use-version)
            (setq ret "")
          (if best-version
              (setq ret (concat "::" best-version))
            (setq ret (concat "::" smallest-version))))
        (symbol-value 'ret))
    ""))

(defun ergoemacs-theme--install-shortcut-item (key args keymap lookup-keymap
                                                   full-shortcut-map-p)
  (let (fn-lst)
    (cond
     ((condition-case err
          (interactive-form (nth 0 args))
        (error nil))
      (setq fn-lst (ergoemacs-shortcut-remap-list
                    (nth 0 args) lookup-keymap))
      (if fn-lst
          (define-key keymap key (nth 0 (nth 0 fn-lst)))
        (when full-shortcut-map-p
          (define-key keymap key (nth 0 args)))))
     (t
      (define-key keymap key
        `(lambda(&optional arg)
           (interactive "P")
           (ergoemacs-read-key ,(nth 0 args) ',(nth 1 args))))))))

(defun ergoemacs-theme--install-shortcuts-list (shortcut-list keymap lookup-keymap full-shortcut-map-p)
  "Install shortcuts for SHORTCUT-LIST into KEYMAP.
LOOKUP-KEYMAP
FULL-SHORTCUT-MAP-P "
  (mapc
   (lambda(y)
     (let ((key (nth 0 y))
           (args (nth 1 y)))
       (ergoemacs-theme--install-shortcut-item
        key args keymap lookup-keymap
        full-shortcut-map-p)))
   shortcut-list))

(defun ergoemacs-theme-component-keymaps-for-hook (hook component &optional version)
  "Gets the keymaps for COMPONENT and component VERSION for HOOK.
If the COMPONENT has the suffix :fixed, just get the fixed component.
If the COMPONENT has the suffix :variable, just get the variable component.
If the COMPONENT has the suffix ::version, just get the closest specified version.

If COMPONENT is a list, return the composite keymaps of all the
components listed.

The return value is an alist of keymaps needed for this hook.  The format is:
  ((map-name always-modify-p keymap-stub full-map))

map-name is the map name that will be modified
always-modify-p is a flag that will modify the keymap every time the hook is run.
keymaps-stub is the keymap overrides that will be installed.

When map-name is t, it is for a keymap put in `ergoemacs-emulation-mode-map-alist'.

This function does not finalize maps by installing them into the original maps.
"
  (if (eq (type-of component) 'cons)
      (let ((ret nil) ;; List of components.
            already-done-list
            (version version)
            always-modify-p) 
        (mapc
         (lambda(comp)
           (let ((new-ret (ergoemacs-theme-component-keymaps-for-hook hook comp version)))
             (if (not ret)
                 (setq ret new-ret)
               (setq already-done-list '())
               (setq ret
                     (mapcar
                      (lambda(keymap-list)
                        (let ((map-name (nth 0 keymap-list))
                              full-map-p)
                          (setq new-map (assoc map-name new-ret))
                          (if (not new-map)
                              keymap-list
                            ;; Try to decompose keymaps as much as
                            ;; possible.  That way you won't have
                            ;; keymaps like:
                            ;; (keymap (keymap (keymap (keymap...))))
                            ;; For no reason.
                            (push map-name already-done-list)
                            (setq always-modify-p (or (nth 1 keymap-list) (nth 1 new-map)))
                            (setq new-map (nth 2 new-map)
                                  old-map (nth 2 keymap-list)
                                  full-map-p (or (nth 3 keymap-list)
                                                 (nth 3 new-map)))
                            (cond
                             ((and (keymapp (nth 1 new-map)) ; 2 composed.
                                   (keymapp (nth 1 old-map)))
                              (pop new-map)
                              (pop old-map)
                              (setq new-map (append new-map old-map)))
                             ((keymapp (nth 1 old-map))
                              (pop old-map)
                              (setq new-map (append (list new-map) old-map))
                              (push 'keymap new-map))
                             ((keymapp (nth 1 new-map)) ;; New map is composed.
                              (pop new-map)
                              (setq new-map (append new-map (list old-map)))
                              (push 'keymap new-map))
                             (t ;; decomposed maps.
                              (setq new-map (make-composed-keymap (list new-map old-map)))))
                            (list map-name always-modify-p new-map full-map-p))))
                      ret))
               (mapc
                (lambda(keymap-list)
                  (unless (member (nth 0 keymap-list) already-done-list)
                    (push keymap-list ret)))
                new-ret))))
         (reverse component))
        (symbol-value 'ret))
    ;; Single component
    (let ((true-component (replace-regexp-in-string ":\\(fixed\\|variable\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component))))
          (only-variable (string-match ":variable" (or (and (stringp component) component)
                                                       (symbol-name component))))
          (only-fixed (string-match ":fixed" (or (and (stringp component) component)
                                                 (symbol-name component))))
          fixed-maps variable-maps
          (true-version version)
          (version version)
          minor-alist keymap-list shortcut-list
          always-p full-shortcut-map-p ret already-done-list)
      (when (string-match "::\\([0-9.]+\\)$" true-component)
        (setq version (match-string 1 true-component))
        (setq true-component (replace-match "" nil nil true-component)))
      (if (not version)
          (setq version "")
        (setq version (ergoemacs-theme-component-get-closest-version
                       version
                       (gethash (concat true-component ":version")
                                ergoemacs-theme-component-hash))))
      (unless only-variable
        (setq fixed-maps (gethash (concat true-component version ":maps") ergoemacs-theme-component-hash))
        (unless fixed-maps
          ;; Setup fixed fixed-keymap for this component.
          (setq minor-alist
                (gethash (concat true-component version ":minor") ergoemacs-theme-component-hash))
          (when minor-alist
            (setq keymap-list (assoc hook minor-alist))
            (when keymap-list
              (setq keymap-list (car (cdr keymap-list))
                    fixed-maps '())
              (mapc
               (lambda(map-name)
                 (let ((keys (assoc (list hook map-name nil) minor-alist))
                       (map (make-sparse-keymap))
                       always-p)
                   (when keys
                     (setq always-p (nth 2 keys))
                     (setq full-shortcut-map-p (nth 3 keys))
                     (setq keys (nth 1 keys))
                     (mapc
                      (lambda(key-list)
                        (cond
                         ((stringp (nth 1 key-list))
                          (define-key map (read-kbd-macro (nth 0 key-list) t)
                            `(lambda() (interactive) (ergoemacs-read-key ,(nth 1 key-list)))))
                         (t
                          (define-key map (read-kbd-macro (nth 0 key-list) t)
                            (nth 1 key-list)))))
                      keys))
                   (unless (equal map '(keymap))
                     (push (list map-name always-p map full-shortcut-map-p) fixed-maps))))
               keymap-list)
              (unless (equal fixed-maps '())
                (puthash (concat true-component version ":maps") fixed-maps
                         ergoemacs-theme-component-hash))))))

      (unless only-fixed
        (setq variable-maps (gethash (concat true-component version ":" ergoemacs-keyboard-layout ":maps") ergoemacs-theme-component-hash))
        (unless variable-maps
          ;; Setup variable keymaps for this component.
          (setq minor-alist
                (gethash (concat true-component version ":minor") ergoemacs-theme-component-hash))
          (when minor-alist
            (setq keymap-list (assoc hook minor-alist))
            (when keymap-list
              (setq keymap-list (car (cdr keymap-list))
                    variable-maps '())
              (mapc
               (lambda(map-name)
                 (let ((keys (assoc (list hook map-name t) minor-alist))
                       (map (make-sparse-keymap))
                       full-map
                       always-p)
                   (when keys
                     (setq always-p (nth 2 keys))
                     (setq full-shortcut-map-p (nth 3 keys))
                     (setq keys (nth 1 keys))
                     (mapc
                      (lambda(key-list)
                        (cond
                         ((stringp (nth 1 key-list))
                          (define-key map (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list))
                            `(lambda() (interactive) (ergoemacs-read-key ,(nth 1 key-list)))))
                         (t
                          (define-key map (ergoemacs-kbd (nth 0 key-list) nil (nth 3 key-list))
                            (nth 1 key-list)))))
                      keys))
                   (unless (equal map '(keymap))
                     (push (list map-name always-p map full-shortcut-map-p) variable-maps))))
               keymap-list)
              (unless (equal variable-maps '())
                (puthash (concat true-component version ":" ergoemacs-keyboard-layout ":maps")
                         variable-maps ergoemacs-theme-component-hash))))))
      ;; Now variable maps
      (setq already-done-list '())
      (setq ret
            (mapcar
             (lambda(keymap-list)
               (let ((map-name (nth 0 keymap-list))
                     fixed-map composed-map tmp full-shortcut-map-p)
                 (setq fixed-map (assoc map-name fixed-maps))
                 (if (not fixed-map)
                     keymap-list
                   (push map-name already-done-list)
                   (setq full-shortcut-map-p (or (nth 3 keymap-list) (nth 2 fixed-map)))
                   ;; Need to decompose if needed...
                   (cond
                    ((and (keymapp (nth 1 (nth 2 keymap-list)))
                          (keymapp (nth 1 (nth 2 fixed-map))))
                     (setq composed-map (nth 2 keymap-list))
                     (pop composed-map)
                     (setq tmp (nth 2 fixed-map))
                     (pop tmp))
                    ((keymapp (nth 1 (nth 2 keymap-list)))
                     (setq composed-map (nth 2 keymap-list))
                     (pop composed-map)
                     (setq tmp (list (nth 2 fixed-map))))
                    ((keymapp (nth 1 (nth 2 fixed-map)))
                     (setq composed-map (list (nth 2 keymap-list)))
                     (setq tmp (nth 2 fixed-map))
                     (pop tmp))
                    (t
                     (setq composed-map (list (nth 2 keymap-list)))
                     (setq tmp (list (nth 2 fixed-map)))))
                   (setq composed-map
                         (if ergoemacs-prefer-variable-keybindings
                             (append composed-map tmp)
                           (append tmp composed-map)))
                   (push 'keymap composed-map)
                   (list map-name (or (nth 1 keymap-list) (nth 1 fixed-map))
                         composed-map full-shortcut-map-p))))
             variable-maps))
      (mapc
       (lambda(keymap-list)
         (unless (member (nth 0 keymap-list) already-done-list)
           (push keymap-list ret)))
       fixed-maps)
      (symbol-value 'ret))))

(defun ergoemacs-theme-component-keymaps (component &optional version)
  "Gets the keymaps for COMPONENT for component VERSION.
If the COMPONENT has the suffix :fixed, just get the fixed component.
If the COMPONENT has the suffix :variable, just get the variable component.
If the COMPONENT has the suffix ::version, just get the closest specified version.

If COMPONENT is a list, return the composite keymaps of all the
components listed.

Returns list of: read-keymap shortcut-keymap keymap shortcut-list unbind-keymap.
"
  ;;; (let ((tmp (nth 1 (ergoemacs-theme-component-keymaps '(move-char help))))) (message "%s" (substitute-command-keys "\\{tmp}")))
  (if (eq (type-of component) 'cons)
      (let ((ret nil)
            k-l
            (l0 '())
            (l1 '())
            (l2 '())
            (l3 '())
            (l4 '())) ;; List of components.
        (mapc
         (lambda(comp)
           (let ((new-ret (ergoemacs-theme-component-keymaps comp version)))
             (when (and (nth 0 new-ret) (not (equal (nth 0 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 0 new-ret))))
                   (push (nth 0 new-ret) l0) ;; Not a composed keymap.
                 (setq k-l (nth 0 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l0 (append k-l l0))))
             (when (and (nth 1 new-ret) (not (equal (nth 1 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 1 new-ret))))
                   (push (nth 1 new-ret) l1) ;; Not a composed keymap.
                 (setq k-l (nth 1 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l1 (append k-l l1))))
             (when (and (nth 2 new-ret) (not (equal (nth 2 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 2 new-ret))))
                   (push (nth 2 new-ret) l2) ;; Not a composed keymap.
                 (setq k-l (nth 2 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l2 (append k-l l2))))
             (when (nth 3 new-ret)
               (setq l3 (append l3 (nth 3 new-ret))))
             (when (and (nth 4 new-ret) (not (equal (nth 4 new-ret) '(keymap))))
               (if (not (keymapp (nth 1 (nth 4 new-ret))))
                   (push (nth 4 new-ret) l4) ;; Not a composed keymap.
                 (setq k-l (nth 4 new-ret)) ;; Composed keymap.
                 ;; Decompose keymaps.
                 (pop k-l)
                 (setq l4 (append k-l l4))))))
         (reverse component))
        (setq ret
              (list
               (make-composed-keymap l0)
               (make-composed-keymap l1)
               (make-composed-keymap l2)
               l3
               (make-composed-keymap l4))))
    (let (fixed-shortcut
          fixed-read
          fixed-shortcut-list
          unbind
          variable-shortcut
          variable-read
          fixed variable
          key-list
          (no-ergoemacs-advice t)
          (case-fold-search t)
          key
          trans-key input-keys
          cmd cmd-tmp
          (version version)
          (shortcut-list '())
          (true-component (replace-regexp-in-string ":\\(fixed\\|variable\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component))))
          (only-variable (string-match ":variable" (or (and (stringp component) component)
                                                       (symbol-name component))))
          (only-fixed (string-match ":fixed" (or (and (stringp component) component)
                                                 (symbol-name component)))))
      (when (string-match "::\\([0-9.]+\\)$" true-component)
        (setq version (match-string 1 true-component))
        (setq true-component (replace-match "" nil nil true-component)))
      (if (not version)
          (setq version "")
        (setq version (ergoemacs-theme-component-get-closest-version
                       version
                       (gethash (concat true-component ":version")
                                ergoemacs-theme-component-hash))))
      
      ;; (let ((tmp-map (nth 1 (ergoemacs-theme-component-keymaps 'copy))))
      ;;   (substitute-command-keys "\\{tmp-map}"))
      (setq unbind (gethash (concat true-component version ":unbind") ergoemacs-theme-component-hash))
      (unless unbind
        (setq unbind (make-sparse-keymap))
        (mapc
         (lambda(x)
           (define-key unbind (read-kbd-macro x) 'ergoemacs-undefined))
         (gethash (concat true-component version ":redundant") ergoemacs-theme-component-hash))
        (puthash (concat true-component version ":unbind") unbind ergoemacs-theme-component-hash))
      (unless only-variable
        (setq fixed-shortcut (gethash (concat true-component version ":fixed:shortcut") ergoemacs-theme-component-hash))
        (setq fixed-read (gethash (concat true-component version ":fixed:read") ergoemacs-theme-component-hash))
        (setq fixed (gethash (concat true-component version ":fixed:map") ergoemacs-theme-component-hash))
        (setq fixed-shortcut-list (gethash (concat true-component version ":fixed:shortcut:list")
                                           ergoemacs-theme-component-hash))
        (unless (or fixed fixed-shortcut fixed-read fixed-shortcut-list)
          ;; Setup fixed fixed-keymap for this component.
          (setq key-list (gethash (concat true-component version ":fixed") ergoemacs-theme-component-hash))
          (when key-list
            (setq fixed-shortcut (make-sparse-keymap))
            (setq fixed (make-sparse-keymap))
            (setq fixed-read (make-sparse-keymap))
            (mapc
             (lambda(x)
               (when (and (eq 'string (type-of (nth 0 x))))
                 (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))
                 (setq key (read-kbd-macro trans-key))
                 (when (string-match "^\\([^ ]+\\) " (nth 0 x))
                   (add-to-list 'input-keys (match-string 1 (nth 0 x))))
                 (when (ergoemacs-global-changed-p key) ;; Override
                   (define-key ergoemacs-global-override-keymap key
                     (lookup-key (current-global-map) key)))
                 (setq cmd (nth 1 x))
                 (ergoemacs-theme-component--define-key-in-keymaps fixed fixed-shortcut key cmd)))
             key-list)
            (when input-keys
              (mapc
               (lambda(key)
                 (unless (member key ergoemacs-ignored-prefixes)
                   (define-key fixed-read (read-kbd-macro key)
                     `(lambda()
                        (interactive)
                        (ergoemacs-read-key ,key 'normal)))))
               input-keys))
            (setq fixed-shortcut-list shortcut-list
                  input-keys '())
            (setq shortcut-list '())
            (puthash (concat true-component version ":fixed:shortcut") fixed-shortcut
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component version ":fixed:read") fixed-read
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component version ":fixed:map") fixed
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component version ":fixed:shortcut:list") fixed-shortcut-list
                     ergoemacs-theme-component-hash))))

      (unless only-fixed
        (setq variable-shortcut (gethash (concat true-component ":" ergoemacs-keyboard-layout  version ":variable:shortcut") ergoemacs-theme-component-hash))
        (setq variable-read (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:read") ergoemacs-theme-component-hash))
        (setq variable (gethash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:map") ergoemacs-theme-component-hash))
        (setq variable-shortcut-list (gethash (concat true-component version ":variable:shortcut:list")
                                              ergoemacs-theme-component-hash))
        (unless (or variable variable-shortcut variable-read variable-shortcut-list)
          ;; Setup variable variable-keymap for this component.
          (setq key-list (gethash (concat true-component version ":variable") ergoemacs-theme-component-hash))
          (when key-list
            (setq variable-shortcut (make-sparse-keymap))
            (setq variable (make-sparse-keymap))
            (setq variable-read (make-sparse-keymap))
            (mapc
             (lambda(x)
               (when (and (eq 'string (type-of (nth 0 x))))
                 (setq key (ergoemacs-kbd (nth 0 x) nil (nth 3 x)))
                 (when (string-match "^\\([^ ]+\\) " (nth 0 x))
                   (add-to-list 'input-keys (match-string 1 (nth 0 x))))
                 (when (ergoemacs-global-changed-p key) ;; Override
                   (define-key ergoemacs-global-override-keymap key
                     (lookup-key (current-global-map) key)))
                 (setq cmd (nth 1 x))
                 (ergoemacs-theme-component--define-key-in-keymaps variable variable-shortcut key cmd)))
             key-list)
            (when input-keys
              (mapc
               (lambda(key)
                 (unless (member key ergoemacs-ignored-prefixes)
                   (define-key variable-read (read-kbd-macro key)
                     `(lambda()
                        (interactive)
                        (ergoemacs-read-key ,key 'normal)))))
               input-keys))
            (setq variable-shortcut-list shortcut-list
                  input-keys '())
            (setq shortcut-list '())
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:shortcut") variable-shortcut
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:read") variable-read
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:map") variable
                     ergoemacs-theme-component-hash)
            (puthash (concat true-component ":" ergoemacs-keyboard-layout version ":variable:shortcut:list") variable-shortcut-list
                     ergoemacs-theme-component-hash))))
      (mapc
       (lambda(var)
         (when (equal (symbol-value var) '(keymap))
           (set var nil)))
       '(fixed-read
         fixed-shortcut
         fixed
         variable-read
         variable-shortcut
         variable
         unbind))
      (cond
       (only-fixed
        (list fixed-read fixed-shortcut fixed fixed-shortcut-list nil))
       (only-variable
        (list variable-read variable-shortcut variable variable-shortcut-list nil))
       (t
        (list (or (and variable-read fixed-read
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable-read fixed-read)
                                               (list fixed-read variable-read))))
                  variable-read fixed-read)
              (or (and variable-shortcut fixed-shortcut
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable-shortcut fixed-shortcut)
                                               (list fixed-shortcut variable-shortcut))))
                  variable-shortcut fixed-shortcut)
              (or (and variable fixed
                       (make-composed-keymap (if ergoemacs-prefer-variable-keybindings
                                                 (list variable fixed)
                                               (list fixed variable))))
                  variable fixed)
              (if ergoemacs-prefer-variable-keybindings
                  (append variable-shortcut-list fixed-shortcut-list)
                (append fixed-shortcut-list variable-shortcut-list))
              unbind))))))

(defun ergoemacs-theme-hook (hook)
  "Run `ergoemacs-mode' HOOK."
  )

(defun ergoemacs-theme-component-make-hooks (component &optional remove-p)
  "Make ergoemacs-mode hooks for COMPONENT.
COMPONENT may also be a list of components.

When REMOVE-P, remove the created ergoemacs-mode hook functions
from the appropriate startup hooks.  Otherwise the hooks are
added to the appropriate startup hooks.
"
  (if (eq (type-of component) 'cons)
      (mapc
       (lambda(c)
         (ergoemacs-theme-component-make-hooks c remove-p))
       component)
    (let ((true-component (replace-regexp-in-string ":\\(fixed\\|variable\\|:[0-9.]+\\)" ""
                                                    (or (and (stringp component) component)
                                                        (symbol-name component)))))
      (mapc
       (lambda(hook)
         (eval (macroexpand
                `(defun ,(intern (concat "ergoemacs-for-" (symbol-name hook))) ()
                   ,(format "Runs `ergoemacs-theme-hook' for `%s'" (symbol-name hook))
                   (ergoemacs-theme-hook ',hook))))
         (if remove-p
             (eval
              (macroexpand
               `(remove-hook ',hook ',(intern (concat "ergoemacs-for-" (symbol-name hook))))))
           (eval
            (macroexpand
             `(add-hook ',hook ',(intern (concat "ergoemacs-for-" (symbol-name hook))))))))
       (gethash (concat true-component ":minor-list") ergoemacs-theme-component-hash)))))

(defmacro ergoemacs-theme-component (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs--parse-keys-and-body body-and-plist))
    `(let ((name ,(plist-get (nth 0 kb) ':name))
           (desc ,(or (plist-get (nth 0 kb) ':description) ""))
           (layout ,(or (plist-get (nth 0 kb) ':layout) "us"))
           (variable-reg ,(or (plist-get (nth 0 kb) ':variable-reg)
                              (concat "\\(?:^\\|<\\)" (regexp-opt '("M-" "<apps>" "<menu>")))))
           (just-first-reg ,(or (plist-get (nth 0 kb) ':first-is-variable-reg)
                                nil))
           (versions '())
           (component-version nil)
           (component-version-variable-layout nil)
           (component-version-fixed-layout nil)
           (component-version-redundant-keys nil)
           (component-version-minor-mode-layout nil)
           (component-version-curr nil)
           (component-version-list '())
           (defined-keys '())
           (variable-layout '())
           (fixed-layout '())
           (defined-commands '())
           (minor-mode-layout '())
           (minor-mode-hook-list '())
           (redundant-keys '())
           (ergoemacs-translation-from ergoemacs-translation-from)
           (ergoemacs-translation-to ergoemacs-translation-to)
           (ergoemacs-shifted-assoc ergoemacs-shifted-assoc)
           (ergoemacs-needs-translation ergoemacs-needs-translation)
           (ergoemacs-translation-assoc ergoemacs-translation-assoc)
           (ergoemacs-translation-regexp ergoemacs-translation-regexp)
           (case-fold-search nil))
       (when (ad-is-advised 'define-key)
         (ad-disable-advice 'define-key 'around 'ergoemacs-define-key-advice))
       (ergoemacs-setup-translation "us" layout) ; Make sure keys are
                                        ; stored in QWERTY
                                        ; notation.
       ,@(nth 1 kb)
       ;; Finalize version setup
       (when component-version-curr
         (push (list component-version-curr
                     component-version-fixed-layout
                     component-version-variable-layout) component-version-list))
       (puthash (concat name ":plist") ',(nth 0 kb) ergoemacs-theme-component-hash)
       (puthash (concat name ":fixed") (symbol-value 'fixed-layout) ergoemacs-theme-component-hash)
       (puthash (concat name ":variable") (symbol-value 'variable-layout) ergoemacs-theme-component-hash)
       (puthash (concat name ":version") versions ergoemacs-theme-component-hash)
       (puthash (concat name ":redundant") redundant-keys ergoemacs-theme-component-hash)
       (puthash (concat name ":minor") minor-mode-layout ergoemacs-theme-component-hash)
       (puthash (concat name ":minor-list") minor-mode-hook-list ergoemacs-theme-component-hash)
       (mapc
        (lambda(x)
          (let ((ver (nth 0 x))
                (fixed (nth 1 x))
                (var (nth 2 x))
                (red (nth 3 x)))
            (puthash (concat name "::" ver ":fixed") fixed ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":variable") var ergoemacs-theme-component-hash)
            (puthash (concat name "::" ver ":redundant") var ergoemacs-theme-component-hash)))
        component-version-list))))
;;; Theme functions

(defun ergoemacs-theme-components (theme)
  "Get of list of components used for the current theme.
This respects `ergoemacs-theme-options'."
  (let ((theme-plist (gethash (if (stringp theme) theme
                                (symbol-name theme))
                              ergoemacs-theme-hash))
        components)
    (setq components (reverse (plist-get theme-plist ':components)))
    (mapc
     (lambda(x)
       (let ((a (assoc x ergoemacs-theme-options)))
         (if (not a)
             (push x components)
           (setq a (car (cdr a)))
           (when (or (not a) (eq a 'on))
             (push x components)))))
     (reverse (eval (plist-get theme-plist ':optional-on))))
    (mapc
     (lambda(x)
       (let ((a (assoc x ergoemacs-theme-options)))
         (if a
             (setq a (car (cdr a)))
           (when (eq a 'on)
             (push x components)))))
     (reverse (eval (plist-get theme-plist ':optional-off))))
    (setq components (reverse components))
    (symbol-value 'components)))

(defun ergoemacs-theme-make-hooks (theme &optional remove-p)
  "Creates hooks for THEME.

When REMOVE-P, remove the created ergoemacs-mode hook functions
from the appropriate startup hooks.  Otherwise the hooks are
added to the appropriate startup hooks.
"
  (ergoemacs-theme-component-make-hooks (ergoemacs-theme-components theme) remove-p))

(defun ergoemacs-theme-keymaps-for-hook (hook theme &optional version)
  "Gets the keymaps for the HOOK specific to the THEME and VERSION specified.

The return value is an alist of keymaps needed for this hook.  The format is:
  ((map-name always-modify-p keymap-replacement))

map-name is the map name that will be modified
always-modify-p is a flag that will modify the keymap every time the hook is run.
keymaps-stub is the keymap overrides that will be installed.

When map-name is t, it is for a keymap put in `ergoemacs-emulation-mode-map-alist'

Uses `ergoemacs-theme-component-keymaps-for-hook' and `ergoemacs-theme-components'"
  ;;
  (let ((theme-components (ergoemacs-theme-components theme))
        overall-keymaps)
    (setq overall-keymaps (ergoemacs-theme-keymaps theme version))
    ;; 0:read-keymap 1:shortcut-keymap 2:keymap 3:shortcut-list 4:unbind-keymap.
    (mapcar
     (lambda(c)
       (if (eq (nth 0 c) 't)
           (list 't (nth 1 c) (nth 2 c))
         (let ((map-name (nth 0 c))
               (always-modify-p (nth 1 c))
               (base-keymap (nth 2 c))
               (full-keymap-p (nth 3 c))
               (shortcut-map (make-sparse-keymap)) tmp
               orig-map
               final-map)
           (setq orig-map (gethash (concat (symbol-name map-name) ":original-map") ergoemacs-theme-component-hash))
           (unless orig-map
             (setq tmp (intern-soft (concat "ergoemacs-" (symbol-name hook) "-old-keymap")))
             (if (and tmp (symbol-value tmp)) ;; In case old `ergoemacs-mode' already modified it...
                 (setq orig-map (copy-keymap (symbol-value tmp)))
               (setq orig-map (copy-keymap (symbol-value map-name))))
             (puthash (concat (symbol-name map-name) ":original-map") (symbol-value map-name) ergoemacs-theme-component-hash))
           (ergoemacs-theme--install-shortcuts-list
            (nth 3 overall-keymaps) shortcut-map
            orig-map full-keymap-p)
           (message "Full Keymap: %s\n%s" full-keymap-p
                    (nth 3 overall-keymaps))
           (if (not full-keymap-p)
               (setq final-map (make-composed-keymap
                                (if (not (keymapp (nth 1 shortcut-map)))
                                    (append base-keymap (list shortcut-map))
                                  (pop shortcut-map)
                                  (append base-keymap shortcut-map)) ;; orig-map
                                ))
             (if (keymapp (nth 1 base-keymap))
                 (pop base-keymap)
               (setq base-keymap (list base-keymap)))
             (setq base-keymap
                   (if (not (keymapp (nth 1 shortcut-map)))
                       (append base-keymap (list shortcut-map))
                     (pop shortcut-map)
                     (append base-keymap shortcut-map)))
             (when (nth 0 overall-keymaps)
               (setq base-keymap (append (nth 0 overall-keymaps) base-keymap)))
             ;; Set parent to original keymap and compose read-keymap.
             (setq final-map (make-composed-keymap base-keymap ;; orig-map
                                                   )))
           (list map-name always-modify-p final-map))))
     (ergoemacs-theme-component-keymaps-for-hook hook theme-components version))))
(defun ergoemacs-theme-keymaps (theme &optional version)
  "Gets the keymaps for THEME for VERSION.
Returns list of: read-keymap shortcut-keymap keymap shortcut-list.
Uses `ergoemacs-theme-component-keymaps' and `ergoemacs-theme-components'"
  (ergoemacs-theme-component-keymaps (ergoemacs-theme-components theme) version))

(defun ergoemacs-theme-install (theme &optional version)
  "Installs `ergoemacs-theme' THEME into appropriate keymaps."
  (let ((tc (ergoemacs-theme-keymaps theme version)))
    (setq ergoemacs-read-input-keymap (nth 0 tc)
          ergoemacs-shortcut-keymap (nth 1 tc)
          ergoemacs-keymap (nth 2 tc)
          ergoemacs-unbind-keymap (nth 4 tc))
    ;; Reset Shortcut hash.
    (setq ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal))
    (mapc
     (lambda(c)
       (puthash (nth 0 c) (nth 1 c) ergoemacs-command-shortcuts-hash))
     (nth 3 tc))

    (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
    (setq ergoemacs-emulation-mode-map-alist '())
    ;; `ergoemacs-keymap' top in `minor-mode-map-alist'
    (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      (push (cons 'ergoemacs-mode ergoemacs-keymap) minor-mode-map-alist))

    ;; `ergoemacs-unbind-keys' at the bottom in `minor-mode-map-alist'
    (let ((x (assq 'ergoemacs-unbind-keys minor-mode-map-alist)))
      (when x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
      ;; Put at the END of the list.
      (setq minor-mode-map-alist
            (append minor-mode-map-alist
                    (list (cons 'ergoemacs-unbind-keys ergoemacs-unbind-keymap)))))

    ;; `ergoemacs-read-input-keymap', then `ergoemacs-shortcut-keymap'
    ;; in `ergoemacs-emulation-mode-map-alist'
    (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist)
    (push (cons 'ergoemacs-read-input-keys ergoemacs-read-input-keymap) ergoemacs-emulation-mode-map-alist)
    (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
    (set-default 'ergoemacs-mode t)
    (set-default 'ergoemacs-shortcut-keys t)
    (set-default 'ergoemacs-read-input-keys t)
    (set-default 'ergoemacs-unbind-keys t)
    (setq ergoemacs-mode t
          ergoemacs-shortcut-keys t
          ergoemacs-read-input-keys t
          ergoemacs-unbind-keys t)))

(defvar ergoemacs-theme-hash (make-hash-table :test 'equal))
(defmacro ergoemacs-theme (&rest body-and-plist)
  "Define an ergoemacs-theme.
:components -- list of components that this theme uses. These can't be seen or toggled
:optional-on -- list of components that are optional and are on by default
:optional-off -- list of components that are optional and off by default

The rest of the body is an `ergoemacs-theme-component' named THEME-NAME-theme
"
  (declare (doc-string 2)
           (indent 2))
  (let ((kd (make-symbol "body-and-plist"))
        (tmp (make-symbol "tmp")))
    (setq kb (ergoemacs--parse-keys-and-body body-and-plist))
    (setq tmp (eval (plist-get (nth 0 kb) ':components)))
    (push (intern (concat (plist-get (nth 0 kb) ':name) "-theme")) tmp)
    (setq tmp (plist-put (nth 0 kb) ':components tmp))
    (puthash (plist-get (nth 0 kb) ':name) tmp ergoemacs-theme-hash)
    `(ergoemacs-theme-component ,(intern (concat (plist-get (nth 0 kb) ':name) "-theme")) ()
       "Generated theme component"
       ,@(nth 1 kb))))

(provide 'ergoemacs-theme-engine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-theme-engine.el ends here
