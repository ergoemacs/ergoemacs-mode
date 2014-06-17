;;; ergoemacs-macros.el --- Macros for ergoemacs-mode

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
(defmacro ergoemacs-with-ergoemacs (&rest body)
  "With basic `ergoemacs-mode' mode keys.
major-mode, minor-mode, and global keys are ignored."
  `(let ((ergoemacs-mode t)
         (ergoemacs-unbind-keys t)
         (ergoemacs-shortcut-keys t)
         ergoemacs-modal
         ergoemacs-read-input-keys
         (minor-mode-map-alist
          `((ergoemacs-mode ,@ergoemacs-keymap)
            (ergoemacs-unbind-keys ,@ergoemacs-unbind-keymap)))
         (ergoemacs-emulation-mode-map-alist '())
         (ergoemacs-shortcut-emulation-mode-map-alist
          `((ergoemacs-shortcut-keys ,@ergoemacs-shortcut-keymap)))
         (old-global-map (current-global-map))
         (old-local-map (current-local-map))
         (new-local-map (make-sparse-keymap))
         (new-global-map (make-sparse-keymap)))
     (unwind-protect
         (progn
           (use-global-map new-global-map)
           (use-local-map new-local-map)
           ,@body)
       (use-global-map old-global-map)
       (use-local-map old-local-map))))t

;;;###autoload
(defmacro ergoemacs-with-overrides (&rest body)
  "With the `ergoemacs-mode' mode overrides.
The global map is ignored, but major/minor modes keymaps are included."
  `(let (ergoemacs-mode
         ergoemacs-unbind-keys
         ergoemacs-shortcut-keys
         ergoemacs-modal
         ergoemacs-read-input-keys
         (old-global-map (current-global-map))
         (new-global-map (make-sparse-keymap)))
     (unwind-protect
         (progn
           (use-global-map new-global-map)
           ,@body)
       (use-global-map old-global-map))))

;;;###autoload
(defmacro ergoemacs-with-global (&rest body)
  "With global keymap, not ergoemacs keymaps."
  `(ergoemacs-without-emulation
    (let (ergoemacs-mode ergoemacs-unbind-keys)
      ,@body)))

;;;###autoload
(defmacro ergoemacs-with-major-and-minor-modes (&rest body)
  "Without global keymaps and ergoemacs keymaps."
  `(let ((old-global-map (current-global-map))
         (new-global-map (make-sparse-keymap)))
     (unwind-protect
         (progn
           (use-global-map new-global-map)
           (ergoemacs-with-global
            ,@body))
       (use-global-map old-global-map))))

;;;###autoload
(defmacro ergoemacs-without-emulation (&rest body)
  "Without keys defined at `emulation-mode-map-alists'.

Also temporarily remove any changes ergoemacs-mode made to:
- `overriding-terminal-local-map'
- `overriding-local-map'

Will override any ergoemacs changes to the text properties by temporarily
installing the original keymap above the ergoemacs-mode installed keymap.
"
  `(let ((overriding-terminal-local-map overriding-terminal-local-map)
         (overriding-local-map overriding-local-map)
         lookup tmp-overlay override-text-map)
     ;; Remove most of ergoemacs-mode's key bindings
     (ergoemacs-emulations 'remove)
     (unwind-protect
         (progn
           ;; Install override-text-map changes above anything already
           ;; installed.
           (setq tmp-overlay (ergoemacs-remove-shortcuts t))
           ,@body)
       (when tmp-overlay
         (delete-overlay tmp-overlay))
       (when ergoemacs-mode
         (ergoemacs-emulations)))))

;;;###autoload
(defmacro ergoemacs-theme-component (&rest body-and-plist)
  "A component of an ergoemacs-theme."
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist")))
    (setq kb (ergoemacs-theme-component--parse body-and-plist))
    `(puthash ,(plist-get (nth 0 kb) ':name)
              (lambda() ,(plist-get (nth 0 kb) ':description)
                (ergoemacs-theme-component--create-component
                 ',(nth 0 kb)
                 '(lambda () ,@(nth 1 kb)))) ergoemacs-theme-comp-hash)))

;;;###autoload
(defmacro ergoemacs-theme (&rest body-and-plist)
  "Define an ergoemacs-theme.
:components -- list of components that this theme uses. These can't be seen or toggled
:optional-on -- list of components that are optional and are on by default
:optional-off -- list of components that are optional and off by default
:options-menu -- Menu options list
:silent -- If this theme is \"silent\", i.e. doesn't show up in the Themes menu.

The rest of the body is an `ergoemacs-theme-component' named THEME-NAME-theme
"
  (declare (doc-string 2)
           (indent 2))
  (let ((kb (make-symbol "body-and-plist"))
        (tmp (make-symbol "tmp")))
    (setq kb (ergoemacs-theme-component--parse-keys-and-body body-and-plist))
    (setq tmp (eval (plist-get (nth 0 kb) ':components)))
    (push (intern (concat (plist-get (nth 0 kb) ':name) "-theme")) tmp)
    (setq tmp (plist-put (nth 0 kb) ':components tmp))
    (mapc
     (lambda(comp)
       (setq tmp (plist-put (nth 0 kb) comp
                            (eval (plist-get (nth 0 kb) comp)))))
     '(:optional-on :optional-off :options-menu))
    
    `(let (themes silent)
       (setq themes (gethash "defined-themes" ergoemacs-theme-hash)
             silent (gethash "silent-themes" ergoemacs-theme-hash))
       (push ,(plist-get (nth 0 kb) ':name) themes)
       (push ,(plist-get (nth 0 kb) ':name) silent)
       (puthash ,(plist-get (nth 0 kb) ':name) ',tmp ergoemacs-theme-hash)
       (if ,(plist-get (nth 0 kb) ':silent)
           (puthash "silent-themes" silent ergoemacs-theme-hash)
         (puthash "defined-themes" themes ergoemacs-theme-hash))
       (ergoemacs-theme-component ,(intern (concat (plist-get (nth 0 kb) ':name) "-theme")) ()
         ,(format "Generated theme component for %s theme" (concat (plist-get (nth 0 kb) ':name) "-theme"))
         ,@(nth 1 kb)))))

;;;###autoload
(defmacro ergoemacs-deftheme (name desc based-on &rest differences)
  "Creates a theme layout for Ergoemacs keybindings -- Compatability layer.

NAME is the theme name.
DESC is the theme description
BASED-ON is the base name theme that the new theme is based on.

DIFFERENCES are the differences from the layout based on the functions.  These are based on the following functions:

`ergoemacs-key' = defines/replaces variable key with function by (ergoemacs-key QWERTY-KEY FUNCTION DESCRIPTION ONLY-FIRST)
`ergoemacs-fixed-key' = defines/replace fixed key with function by (ergoemacs-fixed-key KEY FUNCTION DESCRIPTION)
"
  (declare (indent 1))
  `(let (silent pl tmp)
     (setq pl (gethash (or ,based-on "standard") ergoemacs-theme-hash))
     (plist-put pl ':name ,(symbol-name name))
     (setq tmp (plist-get pl ':components))
     (push (intern (concat ,(symbol-name name) "-theme")) tmp)
     (setq tmp (plist-put pl ':components tmp))
     (setq silent (gethash "silent-themes" ergoemacs-theme-hash))
     (push ,(symbol-name name) silent)
     (puthash "silent-themes" silent ergoemacs-theme-hash)
     (puthash ,(symbol-name name) tmp ergoemacs-theme-hash)
     (ergoemacs-theme-component ,(intern (concat (symbol-name name) "-theme")) ()
       ,(format "Generated theme component for %s theme" (symbol-name name))
       ,@differences)))
(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
