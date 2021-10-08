;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-map.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; Functions for modifying active maps on the fly.
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

(require 'cl-lib)
(eval-when-compile
  (require 'ergoemacs-macros))

(defvar cl-struct-ergoemacs-component-struct-tags)
(defvar ergoemacs-breadcrumb-hash)
(defvar ergoemacs-command-loop--overriding-terminal-local-map)
(defvar ergoemacs-command-loop-type)
(defvar ergoemacs-dir)
(defvar ergoemacs-ignore-prev-global)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-keymap)
(defvar ergoemacs-map--breadcrumb)
(defvar ergoemacs-map--cache-save)
(defvar ergoemacs-map--hash)
(defvar ergoemacs-map-properties--plist-hash)
(defvar ergoemacs-mode)
(defvar ergoemacs-modify-transient-maps)
(defvar ergoemacs-saved-global-map)
(defvar ergoemacs-translation-hash)
(defvar ergoemacs-user-keymap)
(defvar ess-language)
(defvar ergoemacs-mode--fast-p)
(defvar ergoemacs-remap-ignore)
(defvar ergoemacs-component-struct--composed-hook-minibuffer)
(defvar term-raw-map)


(declare-function ergoemacs-warn "ergoemacs-lib")
(declare-function ergoemacs-setcdr "ergoemacs-lib")

(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")

(declare-function ergoemacs-component-struct--get "ergoemacs-component")
(declare-function ergoemacs-component-struct--lookup-hash "ergoemacs-component")
(declare-function ergoemacs-component-struct--lookup-list "ergoemacs-component")
(declare-function ergoemacs-component-struct--minor-mode-map-alist "ergoemacs-component")

(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-map-properties--before-ergoemacs "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--composed-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--composed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--deferred-maps "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--empty-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--get-or-generate-map-key "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--installed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--keymap-value "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--keys "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--label "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--lookup "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--new-command "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--original-user "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--override-maps "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--get "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--revert-original "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--set-map-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--use-local-unbind-list-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--user "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--where-is "ergoemacs-map-properties")

(declare-function ergoemacs-mode--setup-hash-tables "ergoemacs-mode")


(declare-function ergoemacs-translate--apply-key "ergoemacs-translate")
(declare-function ergoemacs-translate--define-key "ergoemacs-translate")
(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")

(declare-function ergoemacs-key-description "ergoemacs-key-description")
(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")
(declare-function ergoemacs-mode-line "ergoemacs-mode")

(declare-function persistent-soft-location-destroy "persistent-soft")

(defvar ergoemacs-map--hashkey nil
  "Current hashkey for theme options keyboard layout and version.")

(defun ergoemacs-map--hashkey (&optional symbol)
  "Generate hashkey for maps.
When SYMBOL is a string/symbol generate a hash-key based on the symbol/string."
  (or (and symbol ergoemacs-map--hashkey
           (intern (format "%s-%s" symbol ergoemacs-map--hashkey)))
      (let* ((val (format "%s" (append (list ergoemacs-keyboard-layout nil) (ergoemacs-theme-components))))
             (md5 (md5 val)))
        (setq ergoemacs-map--hashkey (intern md5)))))


(defvar ergoemacs-map--alist (make-hash-table))
(defvar ergoemacs-map--alist-t (make-hash-table))
(defvar ergoemacs-map--alist-t-o (make-hash-table))

(defvar ergoemacs-map--volatile-symbols '(t helm--minor-mode)
  "List of volatile symbols where breadcrumb is not effective.")

(defun ergoemacs-map--volatile-p (symbol)
  "Is SYMBOL a volatile keymap prefix?

This tests volitile prefixes in any of the Emacs keymap alists
such as `minor-mode-map-alist'.

Volitale map symbols are defined in `ergoemacs-map--volatile-symbols'."
  (catch 'found
    (dolist (v ergoemacs-map--volatile-symbols)
      (when (eq v symbol)
	(throw 'found t)))
    nil))

(defvar ergoemacs-map--alist-atom-symbol-reset-when-volatile nil)
(defun ergoemacs-map--alist-atom (symbol keymap breadcrumb-base &optional original-user)
  "Basic function for addressing Emacs keymap alists.

These alists are typically of the form (SYMBOL . KEYMAP).  This
function assumes these two arguments are sent to this function,
along with the BREADCRUMB-BASE to determine the keymap with
`erogemacs-mode' modifications installed, or removed when
ORIGINAL-USER is non-nil."
  (if (not (and (symbolp symbol) (ergoemacs-keymapp keymap)))
      (cons symbol keymap)
    (let ((tmp)
	  (hash-table (or (and original-user ergoemacs-map--alist-t-o) ergoemacs-map--alist-t))
	  (breadcrumb-add (or (and original-user (format "%s:o" symbol)) (format "%s" symbol))))
      (cond
       ((ergoemacs-map--volatile-p symbol)
	(setq ergoemacs-map--breadcrumb ""
	      tmp (ergoemacs-gethash (cdr keymap) hash-table))
	(when ergoemacs-map--alist-atom-symbol-reset-when-volatile
	  (puthash ergoemacs-map--alist-atom-symbol-reset-when-volatile -1 ergoemacs-map--alist))
	(unless tmp
	  (setq tmp (or (and original-user (ergoemacs keymap :original-user)) (ergoemacs keymap)))
	  (puthash (cdr keymap) tmp hash-table)))
       (t
	(setq ergoemacs-map--breadcrumb (format "%s:%s" breadcrumb-base breadcrumb-add)
	      tmp (or (and original-user (ergoemacs keymap :original-user)) (ergoemacs keymap)))))
      (cons symbol tmp))))

(defun ergoemacs-map--alist (alist &optional symbol)
  "Apply maps for ALIST.

SYMBOL is the symbol where this alist is located and is used to
save the infromationin the `ergoemacs-map--alist' hash."
  (let ((old-breadcrumb ergoemacs-map--breadcrumb)
        breadcrumb-base type old-len)
    (if (and symbol (setq old-len (ergoemacs-gethash symbol ergoemacs-map--alist))
             (= (length alist) old-len)) alist
      (when symbol
        (puthash symbol (length alist) ergoemacs-map--alist)
        (setq breadcrumb-base (format "%s:%s" old-breadcrumb symbol)))
      (setq ergoemacs-map--alist-atom-symbol-reset-when-volatile symbol)
      (prog1
	  (unwind-protect
	      (prog1 (mapcar
		      (lambda(elt)
			(cond
			 ;; ((not (ergoemacs-sv (car elt)))
			 ;;  ;; not enabled, ignore any changes to this map...?
			 ;;  elt)
			 ((eq (car elt) 'ergoemacs-mode) elt)
			 ((and (not (setq type (ergoemacs (cdr elt) :installed-p))) ergoemacs-mode)
			  ;; Install `ergoemacs-mode' into the keymap
			  (ergoemacs-map--alist-atom (car elt) (cdr elt) breadcrumb-base))
			 ((not type)
			  ;; Install `ergoemacs-mode' user protection into the
			  ;; keymap.
			  (ergoemacs-map--alist-atom (car elt) (cdr elt) breadcrumb-base t))
			 ((eq :cond-map type)
			  ;; Don't change conditional maps.  Change in alists...?
			  elt)
			 ((and ergoemacs-mode (eq :protected-p type))
			  ;; Change protection into full ergoemacs-mode installation
			  (ergoemacs-map--alist-atom (car elt) (ergoemacs (cdr elt) :original) breadcrumb-base))
			 ((eq :protected-p type)
			  ;; Already protected.
			  elt)
			 ((and ergoemacs-mode type)
			  ;; Already installed
			  elt)
			 ((and (not ergoemacs-mode) type)
			  (ergoemacs-map--alist-atom (car elt) (ergoemacs (cdr elt) :original-user) breadcrumb-base))))
		      alist)
		(setq ergoemacs-map--breadcrumb old-breadcrumb)))
	(setq ergoemacs-map--alist-atom-symbol-reset-when-volatile nil)))))

(defvar ergoemacs-map--alists (make-hash-table))
(defun ergoemacs-map--alists (alists &optional symbol)
  "Apply maps for ALISTS.

SYMBOL is the symbol where this alist is located and is used to
save the information in the `ergoemacs-map--alist' hash."
  (let (old-len)
    ;; Only modify if the list has changed length.
    (if (and symbol
             (setq old-len (ergoemacs-gethash symbol ergoemacs-map--alist))
             (= (length alists) old-len))
	alists
      (when symbol
        (puthash symbol (length alists) ergoemacs-map--alist)
        (setq ergoemacs-map--breadcrumb (format "%s:%s" ergoemacs-map--breadcrumb symbol)))
      (mapcar
       (lambda(elt)
         (cond
          ((consp elt)
           (ergoemacs-map--alist (list elt)))
	  ((not (boundp elt)))
          (t
           (set elt (ergoemacs-map--alist (symbol-value elt) elt))
           elt)))
       alists))))

(defun ergoemacs-map--emulation-mode-map-alists ()
  "Modify the `emulation-mode-map-alists'."
  (setq ergoemacs-map--breadcrumb ""
        emulation-mode-map-alists (ergoemacs-map--alists emulation-mode-map-alists 'emulation-mode-map-alists)))

(defun ergoemacs-map--minor-mode-overriding-map-alist ()
  "Modify `minor-mode-overriding-map-alist'."
  (setq ergoemacs-map--breadcrumb ""
        minor-mode-overriding-map-alist (ergoemacs-map--alist minor-mode-overriding-map-alist 'minor-mode-overriding-map-alist)))

(defun ergoemacs-map--minor-mode-map-alist (&optional ini)
  "Modify `minor-mode-map-alist'.

When INI is non-nil, and the `ergoemacs-mode' variable is nil,
the conditional maps are added to `minor-mode-map-alist'.  This
condition should only be true in the function
`ergoemacs-map--install'.

When INI is non-nil, and `ergoemacs-mode' variables it non-nil,
the conditional maps are removed from
`minor-mode-map-alist'.  This should only be used in the function
`ergoemacs-map--remove'.

Otherwise, when INI is non-nil, modify any maps in the
`minor-mode-mode-map-alist' list that have not yet applied
ergoemacs-mode keys to them.  The bulk of the modifications are
done in `ergoemacs-map--alist'."
  (let (ret)
    (when (and ini (not ergoemacs-mode))
      (let (new-lst tmp)
        (dolist (elt minor-mode-map-alist)
          (unless (or (eq (car elt) 'ergoemacs-mode)
                      (and
                       (setq tmp (ergoemacs (cdr elt) :map-key))
                       (consp tmp)
                       (eq 'cond-map (car tmp))))
            (push elt new-lst)))
        (setq minor-mode-map-alist (reverse new-lst))))
    
    (setq ergoemacs-map--breadcrumb ""
          ret (ergoemacs-map--alist minor-mode-map-alist 'minor-mode-map-alist))

    (when (and ini ergoemacs-mode ret (not (ignore-errors (eq 'cond-map (car (ergoemacs (cdr (last ret)) :map-key))))))
      (setq ret (append ret (ergoemacs-component-struct--minor-mode-map-alist))))
    (setq minor-mode-map-alist ret)
    ret))

(defvar ergoemacs-menu-order)

(defvar ergoemacs-map--cache--last-breadcrumb "")

(defun ergoemacs-map--cache-- (what &optional save)
  "Get WHAT cache.  If SAVE is non-nil, save cache to WHAT."
  (or (and (not what) save
           (or (not ergoemacs-mode)
               (not (minibufferp))
               (and ergoemacs-mode
                    ;; (ergoemacs-warn "Uncached %s:%s" ergoemacs-mode save)
                    (ergoemacs :spinner "Uncached...")))
           save)
      (let* ((key (ergoemacs-map--hashkey what))
             (val (or save (ergoemacs-gethash key ergoemacs-map--hash))))
        (when (and ergoemacs-mode save)
          (setq ergoemacs-map--cache-save t)
          (cond
           ((not (or (string= ergoemacs-map--breadcrumb "")
                     (string= ergoemacs-map--breadcrumb ergoemacs-map--cache--last-breadcrumb)))
            (ergoemacs :spinner '("⌨→%s" "ergoemacs→%s" "ergoemacs->%s")
                       (replace-regexp-in-string "^:" "" ergoemacs-map--breadcrumb)))
           ((and (string= ergoemacs-map--breadcrumb ""))
            (ergoemacs :spinner '("⌨→%s" "ergoemacs→%s" "ergoemacs->%s")
                       (replace-regexp-in-string "^:" "" (format "%s" what)))))
          (puthash key (copy-tree val t) ergoemacs-map--hash))
        val)))

(defvar ergoemacs-map--unbound-keys nil
  "Unbound keys.")

(defvar ergoemacs-map--mirrored-maps
  '((isearch-mode-map isearch--saved-overriding-local-map))
  "List of mirrored maps (for compatability).")

(defvar ergoemacs-map--modified-maps nil
  "List of maps modified by `ergoemacs-mode'.")

(defvar ergoemacs-map-- (make-hash-table :test 'equal))
(defvar ergoemacs-map--lookup-hash (make-hash-table :test 'equal))

(defvar ergoemacs-read-from-minibuffer-map nil
  "If non-nil, keymap that is being read by the minibuffer.")
(defvar ergoemacs-map--quit-map nil
  "Keymap of quit keys for local keymap.")

(defun ergoemacs-map--install ()
  "Install `ergoemacs-mode' into the appropriate keymaps."
  (interactive)
  (ergoemacs-mode-line))

(defvar ergoemacs-mode)

(defvar ergoemacs-map-undefined-remaps
  '((kill-buffer . ergoemacs-close-current-buffer))
  "Assoc list of ergoemacs-mode equivalent functions.")

(defun ergoemacs-map-undefined ()
  "This key is undefined in `ergoemacs-mode'.

If `ergoemacs-mode' knows what the new key or key sequence that
runs the same command, tell the user."
  (interactive)
  (let ((key (ergoemacs-key-description (this-single-command-keys)))
        (old-key (lookup-key (ergoemacs :global-map) (this-single-command-keys)))
	tmp)
    (cond
     ((and old-key (setq tmp (assoc old-key ergoemacs-map-undefined-remaps)))
      (message "%s is disabled! Use %s in place of %s." key (ergoemacs-key-description (where-is-internal (cdr tmp) ergoemacs-keymap t)) old-key))
     ((and old-key (not (integerp old-key)))
      (message "%s is disabled! Use %s for %s instead." key (ergoemacs-key-description (where-is-internal old-key ergoemacs-keymap t)) old-key))
     (t
      (message "%s is disabled!" key)))))

(autoload 'ergoemacs (expand-file-name "ergoemacs-macros.el" ergoemacs-dir) nil t)
(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
