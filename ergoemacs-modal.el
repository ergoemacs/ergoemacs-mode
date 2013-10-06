;;; ergoemacs-modal.el --- Modal Editing commands
;; 
;; Filename: ergoemacs-modal.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:03:23 2013 (-0500)
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(defvar ergoemacs-full-fast-keys-keymap (make-sparse-keymap)
  "Ergoemacs full fast keys keymap")
(defvar ergoemacs-full-alt-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+ keymap.  Alt is removed from all these keys so that no key chord is necessary.")

(defvar ergoemacs-full-alt-shift-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+Shift+ keymap.
Alt+shift is removed from all these keys so that no key chord is
necessary.  Unshifted keys are changed to shifted keys.")

(defun ergoemacs-exit-dummy ()
  "Dummy function for exiting keymaps."
  (interactive))

(defun ergoemacs-setup-fast-keys ()
  "Setup an array listing the fast keys."
  (interactive)
  (ergoemacs-debug-heading "Start ergoemacs-setup-fast-keys")
  (ergoemacs-create-undo-apps-keymap)
  (setq ergoemacs-full-fast-keys-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-shift-keymap (make-sparse-keymap))
  (define-key ergoemacs-full-alt-keymap
    (read-kbd-macro
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>"))
    'ergoemacs-exit-dummy)
  (define-key ergoemacs-full-alt-shift-keymap
    (read-kbd-macro
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>"))
    'ergoemacs-exit-dummy)
  (ergoemacs-debug-heading "Setup Fast/Modal Keys")
  (mapc
   (lambda(var)
     (let* ((key (ergoemacs-kbd (nth 0 var) t))
            (cmd (nth 1 var))
            (stripped-key (replace-regexp-in-string
                           (format "\\<[%s]-"
                                   (if ergoemacs-swap-alt-and-control
                                       "C"
                                     "M"))
                           "" key))
            (new-cmd (nth 1 var)))
       (ergoemacs-debug "Key:%s stripped-key: %s" key stripped-key)
       (when (string-match "^\\([[:ascii:]]\\|SPC\\)$" stripped-key)
         (if (string= (downcase stripped-key) stripped-key)
             (progn
               (define-key ergoemacs-full-alt-keymap (edmacro-parse-keys stripped-key) new-cmd)
               (define-key ergoemacs-full-alt-shift-keymap (edmacro-parse-keys (upcase stripped-key)) new-cmd))
           (define-key ergoemacs-full-alt-shift-keymap (edmacro-parse-keys (downcase stripped-key)) new-cmd)
           (define-key ergoemacs-full-alt-keymap (edmacro-parse-keys stripped-key) new-cmd)))
       (when (member cmd ergoemacs-movement-functions)
         (set (intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
              (make-sparse-keymap))
         (eval `(define-key ,(intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
                  ,(edmacro-parse-keys stripped-key) new-cmd))
         (define-key ergoemacs-full-fast-keys-keymap
           (edmacro-parse-keys stripped-key)
           new-cmd))))
   (symbol-value (ergoemacs-get-variable-layout)))
  (ergoemacs-debug-keymap 'ergoemacs-full-alt-keymap)
  (ergoemacs-debug-keymap 'ergoemacs-full-alt-shift-keymap)
  (ergoemacs-debug-keymap 'ergoemacs-full-fast-keys-keymap)
  (ergoemacs-debug-heading "Stop ergoemacs-setup-fast-keys")
  (ergoemacs-debug-flush))

(defvar ergoemacs-exit-temp-map-var nil)

(defun ergoemacs-minibuffer-setup ()
  "Exit temporary overlay maps."
  (setq ergoemacs-exit-temp-map-var t)
  (setq ergoemacs-shortcut-keys t))

(defun ergoemacs-exit-alt-keys ()
  "Exit alt keys predicate."
  (let (ret cmd)
    (condition-case err
        (progn
          (setq cmd (lookup-key ergoemacs-full-alt-keymap
                                (this-command-keys-vector)))
          (when cmd
            (setq ret t))
          (when (eq cmd 'ergoemacs-exit-dummy)
            (setq ret nil))
          (when ergoemacs-exit-temp-map-var
            (setq ret nil)
            (setq ergoemacs-exit-temp-map-var nil)))
      (error (message "Err %s" err)))
    (unless ret
      (ergoemacs-mode-line) ;; Reset ergoemacs mode line
      (let (message-log-max)
        (message "[Alt+] keys removed from keymap.")))
    (symbol-value 'ret)))

(defun ergoemacs-alt-keys ()
  "Install the alt keymap temporarily"
  (interactive)
  (setq ergoemacs-exit-temp-map-var nil)
  (set-temporary-overlay-map  ergoemacs-full-alt-keymap
                              'ergoemacs-exit-alt-keys)
  (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
   (concat
    " " (replace-regexp-in-string
         "!" "" (ergoemacs-pretty-key "M-!"))))
  (let (message-log-max)
    (message "[Alt+] keys installed to keymap. Press [Menu], [Esc], to exit")))

(defun ergoemacs-exit-alt-shift-keys ()
  "Exit alt-shift keys predicate"
  (let (ret cmd)
    (condition-case err
        (progn
          (setq cmd (lookup-key ergoemacs-full-alt-shift-keymap
                                (this-command-keys-vector)))
          (when cmd
            (setq ret t))
          (when (eq cmd 'ergoemacs-exit-dummy)
            (setq ret nil))
          (when ergoemacs-exit-temp-map-var
            (setq ret nil)
            (setq ergoemacs-exit-temp-map-var nil)))
      (error (message "Err %s" err)))
    (symbol-value 'ret)))

(defun ergoemacs-alt-shift-keys ()
  "Install the alt-shift keymap temporarily"
  (interactive)
  (setq ergoemacs-exit-temp-map-var nil)
  (set-temporary-overlay-map ergoemacs-full-alt-shift-keymap
                             'ergoemacs-exit-alt-shift-keys)
  (message "[Alt+Shift+] keys installed to keymap. Press [Menu], [Esc], to exit"))



(provide 'ergoemacs-modal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-modal.el ends here
