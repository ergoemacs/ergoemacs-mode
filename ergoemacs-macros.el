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

(defvar ergoemacs-mode)
(declare-function ergoemacs-emulations "ergoemacs-mode.el")
(declare-function ergoemacs-remove-shortcuts "ergoemacs-shortcuts.el")
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


(defmacro ergoemacs-with-global (&rest body)
  "With global keymap, not ergoemacs keymaps."
  `(ergoemacs-without-emulation
    (let (ergoemacs-mode ergoemacs-unbind-keys)
      ,@body)))

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

(provide 'ergoemacs-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-macros.el ends here
