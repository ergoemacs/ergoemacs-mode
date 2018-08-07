;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

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
(defvar ergoemacs-menu-keymap)
(defvar ergoemacs-mode)
(defvar ergoemacs-modify-transient-maps)
(defvar ergoemacs-saved-global-map)
(defvar ergoemacs-theme)
(defvar ergoemacs-translation-hash)
(defvar ergoemacs-user-keymap)
(defvar ess-language)
(defvar ergoemacs-mode--fast-p)
(defvar ergoemacs-remap-ignore)
(defvar ergoemacs-component-struct--composed-hook-minibuffer)


(declare-function ergoemacs-timing-- "ergoemacs-mode")

(declare-function ergoemacs-menu--filter "ergoemacs-lib")
(declare-function ergoemacs-warn "ergoemacs-lib")
(declare-function ergoemacs-setcdr "ergoemacs-lib")

(declare-function ergoemacs-command-loop--modal "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")

(declare-function ergoemacs-component-struct--create-hooks "ergoemacs-component")
(declare-function ergoemacs-component-struct--get "ergoemacs-component")
(declare-function ergoemacs-component-struct--lookup-hash "ergoemacs-component")
(declare-function ergoemacs-component-struct--lookup-list "ergoemacs-component")
(declare-function ergoemacs-component-struct--minor-mode-map-alist "ergoemacs-component")
(declare-function ergoemacs-component-struct--rm-hooks "ergoemacs-component")
(declare-function ergoemacs-component-struct--translated-list "ergoemacs-component")

(declare-function ergoemacs-command-loop--minibuffer-supported-p "ergoemacs-command-loop")

(declare-function ergoemacs-theme--get-version "ergoemacs-theme-engine")
(declare-function ergoemacs-theme-components "ergoemacs-theme-engine")
(declare-function ergoemacs-theme--menu "ergoemacs-theme-engine")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-map-properties--before-ergoemacs "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--composed-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--composed-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--deferred-maps "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--empty-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--get-or-generate-map-key "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--ignore-global-changes-p "ergoemacs-map-properties")
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
      (let* ((val (format "%s" (append (list ergoemacs-keyboard-layout (ergoemacs :current-version)) (ergoemacs-theme-components))))
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
save the infromationin the `ergoemacs-map--alist' hash."
  (let (old-len)
    ;; Only modify if the list has changed length.
    (if (and symbol
             (setq old-len (ergoemacs-gethash symbol ergoemacs-map--alist))
             (= (length alists) old-len)) alists
      (when symbol
        (puthash symbol (length alists) ergoemacs-map--alist)
        (setq ergoemacs-map--breadcrumb (format "%s:%s" ergoemacs-map--breadcrumb symbol)))
      (mapcar
       (lambda(elt)
         (cond
          ((consp elt)
           (ergoemacs-map--alist (list elt)))
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
(defvar ergoemacs-map--undefined-keys nil
  "List of undefined keys for the global map.")


(defvar ergoemacs-map--cache--last-breadcrumb "")

(defun ergoemacs-map-cache--exists-p (what)
  "Does the hashkey WHAT exist?"
  (let ((key (ergoemacs-map--hashkey what)))
    (ergoemacs-gethash key ergoemacs-map--hash)))

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

(defvar ergoemacs-map--lookup-keymap-key-volitile-maps
  '()
  "Keymaps that are always calculated when they are the `current-local-map'.")

(defun ergoemacs-map--lookup-keymap-key (lookup-keymap)
  "Calculates the cache key based on LOOKUP-KEYMAP."
  (let ((lookup-key (or (and (not (string= "" ergoemacs-map--breadcrumb)) ergoemacs-map--breadcrumb)
                        (ergoemacs lookup-keymap :map-list)))
        mode-hook)
    (setq lookup-key (or (and (stringp lookup-key) (intern lookup-key))
                         (and (consp lookup-key) (car lookup-key))))
    (when (and lookup-key (eq (current-local-map) lookup-keymap))
      (if (memq lookup-key ergoemacs-map--lookup-keymap-key-volitile-maps)
          (setq lookup-key nil)
        (if (minibufferp)
            (setq lookup-key (intern (format "%s-%s" lookup-key (md5 (format "%s" minibuffer-setup-hook)))))
          (when (and (setq mode-hook (intern (format "%s-hook" major-mode))) (boundp mode-hook))
            (setq lookup-key (intern (format "%s-%s" lookup-key (md5 (format "%s" (symbol-value mode-hook))))))))))
    lookup-key))

(defvar ergoemacs-map--unbound-keys nil
  "Unbound keys.")

(defvar ergoemacs-map--mirrored-maps
  '((isearch-mode-map isearch--saved-overriding-local-map))
  "List of mirrored maps (for compatability).")

(defvar ergoemacs-map--modified-maps nil
  "List of maps modified by `ergoemacs-mode'.")

(defvar ergoemacs-map-- (make-hash-table :test 'equal))
(defvar ergoemacs-map--lookup-hash (make-hash-table :test 'equal))

(defun ergoemacs-map--composed-list (lookup-keymap lookup-key only-modify-p use-local-unbind-list-p)
  "Calculate majority of keys for LOOKUP-KEYMAP.

It takes the following arguments:

- LOOKUP-KEY - key for the caching of keymaps.

- LOOKUP-KEYMP -- keymap for any ergoemacs-mode equivalent
  binding lookups.

- ONLY-MODIFY-P -- When the keymap should only be modified,
  lookups should not be performed.

- USE-LOCAL-UNBIND-LIST-P -- When `ergoemacs-mode' equivalent
  keys should be locally unbound.  This is useful for
  `isearch-mode-map' keymap in Emacs 24.4+."
  (when (ergoemacs-keymapp lookup-keymap)
    (unless (ergoemacs lookup-keymap :installed-p)
      (let ((ret (make-sparse-keymap))
            tmp composed-list local-unbind-list bound-keys i key2)
        (ergoemacs-cache (and lookup-key (intern (format "%s-%s-composed-key" lookup-key
                                                         (ergoemacs lookup-keymap :map-key))))
          (unless only-modify-p
            (ergoemacs-timing lookup-keymap
              (ergoemacs-map-keymap
               (lambda(key item)
                 (unless (or (eq item 'ergoemacs-prefix)
                             (consp key)
                             (memq item ergoemacs-remap-ignore))
                   (let ((key (vconcat key)))
                     ;; What are the keys that are changed.
                     (when (setq tmp (ergoemacs-gethash key ergoemacs-map--lookup-hash))
                       (dolist (new-key tmp)
                         ;; (if (and (boundp 'isearch-mode-map) (eq lookup-keymap isearch-mode-map))
                         ;;     (message "Define %s->%s" (key-description new-key) item))
                         ;; Don't use (ergoemacs :define-key), since list contains all variants.
                         (ergoemacs :define-key ret new-key item)
                         (push new-key bound-keys)))
                     ;; Keys where `ergoemacs-mode' dominates
                     (setq i (length key))
                     (when (and (catch 'found-key
                                  (while (> i 0)
                                    (when (setq tmp (ergoemacs-gethash (setq key2 (substring key 0 i)) ergoemacs-map--))
                                      (throw 'found-key t))
                                    (setq i (- i 1))) nil)
                                (not (member key2 bound-keys))
                                (not (member key2 ergoemacs-map--unbound-keys)))
                       (if (not use-local-unbind-list-p)
                           (ergoemacs :define-key ret key2 tmp)
                         (ergoemacs :apply-key key2
                                    (lambda (trans-key)
                                      (unless (member trans-key local-unbind-list)
                                        (push trans-key local-unbind-list))))))
                     ;; Define ergoemacs-mode remapping
                     ;; lookups.
                     (when (setq tmp (ergoemacs-gethash key (ergoemacs global-map :lookup)))
                       (ergoemacs :define-key ret (vector 'ergoemacs-remap tmp) item)))))
               lookup-keymap))
            (ergoemacs ret :label (list (ergoemacs lookup-keymap :key-hash) 'ergoemacs-mode (intern ergoemacs-keyboard-layout))))
          (setq tmp (ergoemacs-component-struct--lookup-list lookup-keymap) composed-list (or (and ret (or (and tmp (append tmp (list ret))) (list ret))) tmp))
          (list composed-list local-unbind-list ret))))))

(defun ergoemacs-map--puthash (key new &optional table)
  "Associate KEY with a list including NEW in TABLE."
  (let* ((hash (or table ergoemacs-map--lookup-hash))
         (lst (ergoemacs-gethash key hash)))
    (if lst
        (unless (member new lst)
          (push new lst))
      (setq lst (list new)))
    (puthash key lst hash)))

(defun ergoemacs-map--remhash (key new &optional table)
  "For the KEY containing NEW, remove that NEW and update TABLE.

If NEW is the only item in the list, remove it from the hash
TABLE."
  (let* ((hash (or table ergoemacs-map--lookup-hash))
         (lst (ergoemacs-gethash key hash))
         new-lst)
    (when lst
      (dolist (elt (reverse new-lst))
        (unless (equal new elt)
          (push elt new-lst)))
      (if new-lst
          (puthash key lst hash)
        (remhash key hash)))))

(defun ergoemacs-map--get-struct-map (struct-map cur-layout &optional lookup-keymap)
  "For component STRUCT-MAP, get the ergoemacs keymap.

When STRUCT-MAP is not a `ergoemacs-component-struct' object,
return nil.  Otherwise, return the keymap based on the STRUCT-MAP
component.

The keyboard layout that is being calculated is CUR-LAYOUT.

When LOOKUP-KEYMAP is nil, the returned map is relative to the
global keymap.  Otherwise, it is relative to LOOKUP-KEYMAP."
  (if (not (ergoemacs-component-struct-p struct-map)) nil
    (let (ret
	  (cur-layout (or cur-layout ergoemacs-keyboard-layout)))
      (cond
       ((and (not lookup-keymap)
             (string= cur-layout (ergoemacs-component-struct-layout struct-map)))
        (setq ret (ergoemacs-component-struct-map struct-map)))
       ((and (not lookup-keymap)
             (setq ret (ergoemacs-gethash
                        (list nil (intern cur-layout))
                        (ergoemacs-component-struct-calculated-layouts struct-map))))
        ret)
       ((not lookup-keymap)
        ;; Overall layout hasn't been calculated.
        (setq ret (ergoemacs-component-struct--get struct-map cur-layout nil)))
       (t
        (error "Cant calculate/lookup keymap")))
      ret)))

(defun ergoemacs-map--get-unbind-list (component-list)
  "Get the list of unbound keys based on COMPONENT-LIST.

COMPONENT-LIST is a list of `ergoemacs-component-struct' items
that will be applied.

This is cached based on the current theme & theme options by
`ergoemacs-cache'."
  (if (not (consp component-list)) nil
    (let (unbind-list)
      (ergoemacs-cache unbind-list
	(dolist (cur-map component-list)
	  (setq unbind-list
		(append unbind-list
			(ergoemacs-component-struct--translated-list
			 cur-map (ergoemacs-component-struct-unbind cur-map)))))
	unbind-list))))

(defun ergoemacs-map--get-undefined-map (component-list)
  "Get a keymap of the ergoemacs-mode unbound keys based on COMPONENT-LIST.

COMPONENT-LIST is a list of `ergoemacs-component-struct' items
that will be applied.

This updates the variable `ergoemacs-map--undefined-keys', and
then defines each key in the `ergoemacs-map--undefined-keys'
vector on the new keymap to be `ergoemacs-map-undefind'."
  (let ((ret (make-sparse-keymap)))
    (dolist (cur-map component-list)
      (dolist (undefined-key (ergoemacs-component-struct-undefined cur-map))
        (unless (member undefined-key ergoemacs-map--undefined-keys)
          (push undefined-key ergoemacs-map--undefined-keys))))
    (dolist (i ergoemacs-map--undefined-keys)
      (define-key ret i #'ergoemacs-map-undefined))
    (ergoemacs ret :label (list (ergoemacs (ergoemacs :global-map) :key-hash) 'ergoemacs-undefined (intern ergoemacs-keyboard-layout)))
    ret))

(defun ergoemacs-map--global-component-keys-lists (component-list menu-bar-list composed-keymap-list &optional layout)
  "Get ergoemacs keys from COMPONENT-LIST.

This puts menu bar components in MENU-BAR-LIST, and other keymaps
in COMPOSED-KEYMAP-LIST.

The LAYOUT represents the keybaord layout that will be translated."
  (let (tmp tmp2)
    (push (lookup-key (ergoemacs :global-map) [menu-bar]) menu-bar-list)
    (dolist (cur-map (reverse component-list))
      (when cur-map
	(setq tmp (ergoemacs-map--get-struct-map cur-map layout))
	(unless (ergoemacs tmp :empty-p)
          (cond
           ((setq tmp2 (lookup-key tmp [menu-bar-list]))
            (push (copy-keymap tmp2) menu-bar-list)
            (setq tmp2 (make-sparse-keymap))
            (map-keymap
             (lambda (event item)
               (unless (eq event 'menu-bar-list)
                 (define-key tmp2 (vector event) item)))
             tmp)
            (unless (ergoemacs tmp2 :empty-p)
              (push tmp2 composed-keymap-list)))
           (t (push tmp composed-keymap-list))))))
    (vector menu-bar-list composed-keymap-list)))

(defun ergoemacs-map--setup-global-ergoemacs-hash (keymap-list unbind-list)
  "Setup `ergoemacs-map--' cache based on KEYMAP-LIST, repsecting UNBIND-LIST."
  (let (tmp)
    (ergoemacs-timing setup-ergoemacs-hash
      (dolist (ret keymap-list)
	(ergoemacs-map-keymap
	 (lambda(key item)
	   (unless (or (eq item 'ergoemacs-prefix)
		       (ignore-errors (eq (aref key 0) 'ergoemacs-labeled))
		       (gethash key ergoemacs-map--)
		       (member key unbind-list))
	     (when (setq tmp (ergoemacs-gethash item (ergoemacs global-map :where-is)))
	       (dolist (old-key tmp)
		 (ergoemacs :apply-key old-key
			    (lambda(trans-old-key)
			      (ergoemacs :apply-key key
					 (lambda(trans-new-key)
					   (unless (or (gethash trans-new-key ergoemacs-map--)
						       (member trans-new-key unbind-list))
					     (puthash trans-new-key item ergoemacs-map--)
					     (ergoemacs-map--puthash trans-old-key trans-new-key))))))))
	     (ergoemacs :apply-key key #'puthash item ergoemacs-map--)))
	 ret)))))

(defun ergoemacs-map--get-global-menu-map (menu-bar-list)
  "Get the global menu-bar from MENU-BAR-LIST."
  (let ((tmp (make-composed-keymap menu-bar-list))
	(i 0)
	alst)
    (dolist (menu ergoemacs-menu-order)
      (push (list menu i) alst)
      (setq i (+ i 1)))
    (setq tmp (sort tmp (lambda(elt1 elt2)
                          (let ((i1 (or (and (eq elt1 'keymap) -1) (assq (car elt1) alst)))
                                (i2 (or (and (eq elt2 'keymap) -1) (assq (car elt2) alst))))
                            (if i1
                                (setq i1 (or (and (integerp i1) i1) (nth 1 i1)))
                              (setq i1 (length ergoemacs-menu-order)))
                            (if i2
                                (setq i2 (or (and (integerp i2) i2) (nth 1 i2)) )
                              (setq i2 (length ergoemacs-menu-order)))
                            (< i1 i2)))))
    tmp))

(defun ergoemacs-map--get-global-unbound-keymap (unbind-list)
  "Create a keymap for UNBIND-LIST to unbind keys."
  (let ((ret (make-sparse-keymap)))
    (dolist (key unbind-list)
      (define-key ret key nil))
    (ergoemacs ret :label (list (ergoemacs (ergoemacs :global-map) :key-hash) 'ergoemacs-unbound (intern ergoemacs-keyboard-layout)))
    ret))


(defun ergoemacs-map--get-global-map (component-list unbind-list layout lookup-key)
  "Get the global map.

- COMPONENT-LIST is the list of ergoemacs components to apply.

- UNBIND-LIST is the keys that `ergoemacs-mode' has unbound.

- LAYOUT represents the keyboard layout to be calculated

- LOOKUP-KEY represents the symbol to cache the calculated
  results."
  ;; The `undefined-key' layer
  (let (tmp
	ret
	menu-bar
	parent
	composed-list
	tmp2)
    (setq tmp (ergoemacs-cache global-menu
                (push (ergoemacs-map--get-undefined-map component-list) composed-list)
                (setq tmp (ergoemacs-map--global-component-keys-lists component-list menu-bar composed-list layout)
                      menu-bar (elt tmp 0)
                      composed-list (elt tmp 1))
                (ergoemacs-map--setup-global-ergoemacs-hash composed-list unbind-list)
                ;; The real `global-map'
                (setq tmp (ergoemacs-map--get-global-menu-map menu-bar)
                      ;; The keys that will be unbound
                      ret (ergoemacs-map--get-global-unbound-keymap unbind-list))
                
                tmp))
    (setq parent (copy-keymap (ergoemacs :global-map))
	  composed-list (ergoemacs-cache global-composed-list composed-list)
	  ret (ergoemacs-cache global-ret ret)
	  ergoemacs-map-- (ergoemacs-cache ergoemacs-map-- ergoemacs-map--)
	  ergoemacs-map--lookup-hash (ergoemacs-cache ergoemacs-map--lookup-hash ergoemacs-map--lookup-hash)
	  ergoemacs-map--undefined-keys (ergoemacs-cache undefined-keys ergoemacs-map--undefined-keys))
    (define-key parent [menu-bar] tmp)
    (set-keymap-parent ret (make-composed-keymap composed-list parent))
    ;; Save hash
    (puthash lookup-key ret ergoemacs-map--hash)
    (puthash (ergoemacs-map--hashkey 'ergoemacs-map--lookup-hash) ergoemacs-map--lookup-hash ergoemacs-map--hash)
    (puthash (ergoemacs-map--hashkey 'ergoemacs-map--undefined-keys) ergoemacs-map--undefined-keys ergoemacs-map--hash)
    
    ;; Get the protecting user keys
    (setq tmp2 (list))
    (unless ergoemacs-ignore-prev-global
      (setq tmp (ergoemacs :user-before))
      (unless (ergoemacs tmp :empty-p)
	(push tmp tmp2)))
    (setq tmp (ergoemacs :user-after))
    (unless (ergoemacs tmp :empty-p)
      (push tmp tmp2))
    (setq tmp (ergoemacs parent :user))
    (when tmp
      (push tmp tmp2))
    (push ergoemacs-user-keymap tmp2)
    (define-key ret [ergoemacs-ignore] 'ergoemacs-command-loop--ignore)
    (setq ret (make-composed-keymap tmp2 ret))
    ret))

(defun ergoemacs-map--adjust-remaps-for-overrides (hook-overrides composed-list keymap &optional deferred-p)
  "Use HOOK-OVERRIDES to adjust COMPOSED-LIST and KEYMAP.

A new list of keymaps will be returned with any [ergoemacs-remap]
keys calculated.

When DEFERRED-P is non-nil, the returned keymap list will have
new overriding keymaps are appended, otherwise the overriding
keymaps are prepended"
  (if (not hook-overrides) composed-list
    ;; Need to fix 'ergoemacs-remaps as well.
    ;; This is done here because hooks can change, and the
    ;; cached map would be invalid.
    (let ((tmp (ergoemacs global-map :keys))
          (tmp2 (make-sparse-keymap))
          (tmp3 (make-composed-keymap hook-overrides)))
      (ergoemacs tmp2 :label '(fix-hook-remaps))
      (ergoemacs-timing calculate-ergoemacs-remap
	(ergoemacs-map-keymap
	 (lambda(key item)
	   (unless (or (eq item 'ergoemacs-prefix)
		       (consp key))
	     (let ((key (vconcat key)))
	       (when (member key tmp)
		 (define-key keymap (vector 'ergoemacs-remap (ergoemacs-gethash key (ergoemacs global-map :lookup))) nil)))))
	 tmp3))
      (unless (ergoemacs tmp2 :empty-p)
	(push tmp2 hook-overrides))
      (if deferred-p
	  (append composed-list hook-overrides)
	(append hook-overrides composed-list)))))

(defun ergoemacs-map--unbound-passthrough (hook-overrides hook-deferred unbind-list local-unbind-list)
  "Create a keymap of the keys that should be visible to Emacs.

- HOOK-OVERRIDES -- overriding keys from hooks.

- HOOK-DEFERRED -- Keys defined in hooks that will can be
  overriden by `ergoemacs-mode'.

- UNBIND-LIST -- List of unbound keys.

- LOCAL-UNBIND-LIST - List of locally unbound keys."
  (let ((unbound-passthrough (make-sparse-keymap))
	tmp tmp3)
    (when (or hook-overrides hook-deferred)
      (setq tmp3 (make-composed-keymap (append hook-overrides hook-deferred))
            tmp (append unbind-list ergoemacs-map--unbound-keys local-unbind-list))
      (ergoemacs-timing calc-passthrough
        (ergoemacs-map-keymap
         (lambda (key item)
           (unless (or (eq item 'ergoemacs-prefix)
                       (consp key))
             (let ((key (vconcat key)))
               (when (member key tmp)
                 (define-key unbound-passthrough key item)))))
         tmp3))
      (ergoemacs unbound-passthrough :label '(unbound-passthrough)))
    unbound-passthrough))

(defun ergoemacs-map--unbound-keymap (lookup-key lookup-keymap unbind-list local-unbind-list)
  "Create unbound keymap.

This is cached with LOOKUP-KEY.

The LOOKUP-KEYMAP is the keymap that will be modified.

The UNBIND-LIST and LOCAL-UNBIND-LIST are the keys that will be
unbound."
  (let ((ret (make-sparse-keymap))
	tmp tmp2 tmp3)
    (ergoemacs-cache (and lookup-key (intern (format "%s-unbound-keymap" lookup-key)))
      ;; Remove keys from lookup-keymap
      (unless lookup-keymap
	(setq tmp2 nil)
	(maphash
	 (lambda(key item)
	   (dolist (key2 item)
	     (when (member key2 (append unbind-list ergoemacs-map--unbound-keys local-unbind-list))
	       (push key tmp2)
	       (when (setq tmp3 (ergoemacs-translate--escape-to-meta key))
		 (push tmp3 tmp2))
	       (when (setq tmp3 (ergoemacs-translate--meta-to-escape key))
		 (push tmp3 tmp2)))))
	 ergoemacs-map--lookup-hash)
	(dolist (key tmp2)
	  (remhash key ergoemacs-map--lookup-hash)))
      (dolist (key (append unbind-list ergoemacs-map--unbound-keys local-unbind-list))
	(unless (equal key [ergoemacs-labeled])
	  (when (not lookup-keymap)
	    (remhash key ergoemacs-map--)
	    (when (setq tmp (ergoemacs-translate--escape-to-meta key))
	      (remhash key ergoemacs-map--lookup-hash)))
	  (if (not lookup-keymap)
	      (define-key ret key nil)
	    (setq tmp (lookup-key lookup-keymap key))
	    (if (or (not tmp) (integerp tmp))
		(define-key ret key nil)
	      (if (member key local-unbind-list)
		  (define-key ret key nil)
		(define-key ret key tmp))))))
      (ergoemacs ret :label (list (ergoemacs lookup-keymap :key-hash) 'ergoemacs-unbound (intern ergoemacs-keyboard-layout)))
      ret)))

(defun ergoemacs-map--set-maps (lookup-keymap final-keymap)
  "Set maps.

LOOKUP-KEYMAP is the lookup-keymap where the keymaps may be set.

FINAL-KEYMAP is the `ergoemacs-mode' modified keymap."
  (let (tmp)
    (when (ergoemacs lookup-keymap :set-map-p)
      (dolist (map (ergoemacs lookup-keymap :map-list))
	(when (eq lookup-keymap overriding-local-map)
	  (setq overriding-local-map final-keymap))
	(when (eq lookup-keymap overriding-terminal-local-map)
	  (setq overriding-terminal-local-map final-keymap))
	(when (eq (default-value map) lookup-keymap)
	  (ergoemacs :spinner '("⌨→%s (default)" "ergoemacs→%s (default)" "ergoemacs->%s (default)") map)
	  (set-default map final-keymap))
	(when (eq (symbol-value map) lookup-keymap)
	  (ergoemacs :spinner '("⌨→%s (local)" "ergoemacs→%s (local)" "ergoemacs->%s (local)") map)
	  (set map final-keymap))
	(when (setq tmp (assoc map ergoemacs-map--mirrored-maps))
	  (dolist (mirror (cdr tmp))
	    (when (and mirror (boundp mirror))
	      (ergoemacs :spinner '("⌨→%s (mirror %s)" "ergoemacs→%s (mirror %s)" "ergoemacs->%s (mirror %s)") map mirror)
	      (set mirror final-keymap)
	      (push mirror ergoemacs-map--modified-maps))))
	(push map ergoemacs-map--modified-maps)))))

(defun ergoemacs-map--lookup-map (keymap unbind-list)
  "Change KEYMAP to insert `ergoemacs-mode' keys.
UNBIND-LIST is the list of keys that `ergoemacs-mode'."
  (ergoemacs keymap :label)
  (let* ((lookup-keymap (ergoemacs keymap :original))
         (use-local-unbind-list-p (ergoemacs lookup-keymap :use-local-unbind-list-p))
         (only-modify-p (ergoemacs lookup-keymap :only-local-modifications-p))
         (lookup-key (ergoemacs-map--lookup-keymap-key lookup-keymap))
         (composed-list (ergoemacs-map--composed-list lookup-keymap lookup-key only-modify-p use-local-unbind-list-p))
         (ret (nth 2 composed-list))
	 (local-unbind-list (nth 1 composed-list))
	 (composed-list (nth 0 composed-list))
	 (parent lookup-keymap)
	 (hook-overrides (ergoemacs lookup-keymap :override-maps))
	 (hook-deferred (ergoemacs lookup-keymap :deferred-maps))
	 unbound-passthrough tmp)
    (setq composed-list (ergoemacs-map--adjust-remaps-for-overrides hook-overrides composed-list ret)
	  composed-list (ergoemacs-map--adjust-remaps-for-overrides hook-deferred composed-list ret t)
 	  unbound-passthrough (ergoemacs-map--unbound-passthrough hook-overrides hook-deferred unbind-list local-unbind-list))
    (cond
     ((and only-modify-p composed-list)
      ;; Get the protecting user keys
      (setq ret (make-composed-keymap composed-list parent)
            tmp (ergoemacs parent :user))
      (when tmp
        (setq ret (make-composed-keymap tmp ret)))
      ret)
     ((and only-modify-p (not composed-list))
      (setq ret parent))
     (t
      ;; The keys that will be unbound
      (setq ret (ergoemacs-map--unbound-keymap lookup-key lookup-keymap unbind-list local-unbind-list))
      (set-keymap-parent ret (make-composed-keymap composed-list parent))
      ;; Put the unbound keys that are passed through the
      ;; `ergoemacs-mode' layer of keys.
      (unless (ergoemacs unbound-passthrough :empty-p)
        (setq ret (make-composed-keymap unbound-passthrough ret)))
      ;; Get the protecting user keys
      (setq tmp (ergoemacs parent :user))
      (when tmp
        (setq ret (make-composed-keymap tmp ret)))
      ;; Set the overall map values too...
      (when (ergoemacs lookup-keymap :set-map-p)
        (dolist (map (ergoemacs lookup-keymap :map-list))
          (when (eq lookup-keymap overriding-local-map)
            (setq overriding-local-map ret))
          (when (eq lookup-keymap overriding-terminal-local-map)
            (setq overriding-terminal-local-map ret))
          (when (eq (default-value map) lookup-keymap)
            (ergoemacs :spinner '("⌨→%s (default)" "ergoemacs→%s (default)" "ergoemacs->%s (default)") map)
            (set-default map ret))
          (when (eq (symbol-value map) lookup-keymap)
            (ergoemacs :spinner '("⌨→%s (local)" "ergoemacs→%s (local)" "ergoemacs->%s (local)") map)
            (set map ret))
          (when (setq tmp (assoc map ergoemacs-map--mirrored-maps))
            (dolist (mirror (cdr tmp))
              (when (and mirror (boundp mirror))
                (ergoemacs :spinner '("⌨→%s (mirror %s)" "ergoemacs→%s (mirror %s)" "ergoemacs->%s (mirror %s)") map mirror)
                (set mirror ret)
                (push mirror ergoemacs-map--modified-maps))))
          (push map ergoemacs-map--modified-maps)))))
    ret))

(defun ergoemacs-map-- (&optional lookup-keymap layout struct-map)
  "Get map looking up changed keys in LOOKUP-KEYMAP based on LAYOUT.

STRUCT-MAP can be a `ergoemacs-component-struct', or a string/symbol of
a calculated or uncalcuated component in
`ergoemacs-component-hash'

STRUCT-MAP can also be a list of `ergoemacs-component-struct' values
or string/symbols that are in `ergoemacs-component-hash'

If missing, STRUCT-MAP represents the current theme compenents, from
`ergoemacs-theme-components'

LAYOUT represents the layout that is used.

LOOKUP-KEYMAP represents what should be calculated/looked up.

If LOOKUP-KEYMAP is a keymap, lookup the ergoemacs-mode
modifications to that keymap."
  (let* ((cur-layout (or layout ergoemacs-keyboard-layout))
         lookup-key
         (struct-map (ergoemacs-component-struct--lookup-hash (or struct-map (ergoemacs-theme-components))))
         unbind-list
         ret
         (lookup-keymap (or (and lookup-keymap (symbolp lookup-keymap)
				 (ergoemacs-sv lookup-keymap))
			    lookup-keymap)))
    (cond
     ((memq 'add-keymap-witness lookup-keymap) ;; Don't translate complete tranisent maps.
      lookup-keymap)
     ((and lookup-keymap (symbolp lookup-keymap) (ergoemacs-gethash lookup-keymap ergoemacs-translation-hash))
      (ergoemacs-command-loop--modal lookup-keymap))
     ((consp (ergoemacs lookup-keymap :map-key)) ;; Ignore already installed.
      lookup-keymap)
     ((and lookup-keymap (ergoemacs lookup-keymap :dont-modify-p))
      lookup-keymap)
     ((and (consp struct-map) ;; Don't do anything with blank keymaps.
           lookup-keymap
	   ;; Blank keymaps are also unlabeled by `ergoemacs-mode', so
	   ;; make sure to use :empty-p t
	   (ergoemacs lookup-keymap :empty-p t))
      lookup-keymap)
     ((and (consp struct-map)
           (progn
             (setq unbind-list (ergoemacs-map--get-unbind-list struct-map)) t))
      (cond
       ((not lookup-keymap)
	(ergoemacs-map--get-global-map struct-map unbind-list cur-layout lookup-key))
       ;; Now create the keymap for a specified `lookup-keymap'
       (lookup-keymap
	(ergoemacs-map--lookup-map lookup-keymap unbind-list))))
     ;; Component keymap
     ((setq ret (ergoemacs-map--get-struct-map struct-map cur-layout lookup-keymap))
      ret)
     (t
      (ergoemacs-warn "Component struct-map isn't a proper argument for `ergoemacs-map'")
      (ergoemacs-warn "\tLookup:%s" lookup-keymap)
      (ergoemacs-warn "\tLayout:%s" layout)
      (ergoemacs-warn "\tMap:%s" struct-map)
      lookup-keymap))))

(defun ergoemacs-map--temporary-map-properties (map)
  "Test if MAP is a transient map that `ergoemacs-mode' does not touch.

This occurs when the keymap is not known to `ergoemacs-mode' and
it is not a composed keymap.

If it is a tranisent map, assign the :dont-modify-p property to t."
  (setq ergoemacs-map--breadcrumb "transient-maps")
  (ergoemacs map :label)
  ;;ergoemacs-modify-transient-maps
  (if (eq (ergoemacs-gethash 'transient-maps ergoemacs-breadcrumb-hash)
	  (ergoemacs (ergoemacs :original map) :key))
      (ergoemacs map :dont-modify-p t)
    (ergoemacs-setcdr (cdr map) (cdr (ergoemacs (ergoemacs :original map))))))


(defvar ergoemacs-map--modify-active-last-overriding-terminal-local-map nil)
(defvar ergoemacs-map--modify-active-last-overriding-local-map nil)
(defvar ergoemacs-map--modify-active-last-char-map nil)
(defvar ergoemacs-map--modify-active-last-local-map nil)
(defvar ergoemacs-map--saved-global-map nil)
(defvar ergoemacs-map--last-global-map nil)
(defvar ergoemacs-read-from-minibuffer-map nil
  "If non-nil, keymap that is being read by the minibuffer.")
(defun ergoemacs-map--modify-active (&optional ini)
  "Modifies Active maps.

When INI is non-nil, add conditional maps to `minor-mode-map-alist'."
  (let ((char-map (get-char-property-and-overlay (point) 'keymap))
        (local-map (get-text-property (point) 'local-map))
        (current-local-map (current-local-map))
        tmp)
    (when (and overriding-terminal-local-map
               (not (eq overriding-terminal-local-map ergoemacs-map--modify-active-last-overriding-terminal-local-map))
               (not (ergoemacs overriding-terminal-local-map :installed-p))
	       (not (memq 'add-keymap-witness overriding-terminal-local-map)))
      ;; (ergoemacs-map--temporary-map-properties overriding-terminal-local-map)
      (setq overriding-terminal-local-map (ergoemacs overriding-terminal-local-map)))
    
    (when (and overriding-local-map
               (not (eq overriding-local-map ergoemacs-map--modify-active-last-overriding-local-map))
               (not (ergoemacs overriding-local-map :installed-p)))
      ;; (ergoemacs-map--temporary-map-properties overriding-local-map)
      (setq overriding-local-map (ergoemacs overriding-local-map)))

    (ergoemacs-save-buffer-state
     (when (and char-map (symbolp char-map))
       (setq char-map (ergoemacs-sv char-map)))
     (when (and (listp char-map)
		(car char-map)
                (not (eq (car char-map) ergoemacs-map--modify-active-last-char-map))
                (not (ergoemacs (car char-map) :installed-p)))
       (cond
        ((cdr char-map)
         ;; Overlay
         (overlay-put (cdr char-map) 'keymap (ergoemacs (car char-map))))
        (t ;; Text Property
         (put-text-property (previous-single-char-property-change (point) 'keymap)
                            (next-single-char-property-change (point) 'keymap)
                            'keymap
                            (ergoemacs (car char-map))))))
     (when (and local-map (symbolp local-map))
       (setq local-map (ergoemacs-sv local-map)))
     (when (and local-map
                (not (eq local-map ergoemacs-map--modify-active-last-local-map))
                (not (ergoemacs local-map :installed-p)))
       (put-text-property (previous-single-char-property-change (point) 'local-map)
                          (next-single-char-property-change (point) 'local-map)
                          'local-map (ergoemacs local-map)))
     ;; Save before any changes happen (ie in calc)
     (when (and (not ergoemacs-map--saved-global-map)
                (ergoemacs :ignore-global-changes-p))
       (setq ergoemacs-map--saved-global-map (copy-keymap global-map)))
     ;; Restore outside of modes that change the global map (calc)
     (when (and ergoemacs-map--saved-global-map
                (not (ergoemacs :ignore-global-changes-p)))
       (setq global-map (copy-keymap ergoemacs-map--saved-global-map)
             ergoemacs-map--saved-global-map nil)
       (use-global-map global-map))
     (when (and (setq tmp (current-global-map))
                (ergoemacs-keymapp tmp)
                (not (eq tmp global-map))
                (or (not ergoemacs-map--last-global-map)
                    (not (eq ergoemacs-map--last-global-map tmp)))
                (not (ergoemacs tmp :installed-p)))
       (use-global-map (ergoemacs tmp))
       (setq ergoemacs-map--last-global-map (current-global-map)))
     (when (and current-local-map (not (ergoemacs current-local-map :installed-p))
                (not (minibufferp)))
       (setq ergoemacs-map--breadcrumb (format "%s" major-mode))
       (when (eq major-mode 'ess-mode)
         (setq ergoemacs-map--breadcrumb (format "ess-mode-%s" ess-language)))
       (use-local-map (ergoemacs current-local-map))
       (setq ergoemacs-map--breadcrumb ""))
     (when (and (minibufferp) ergoemacs-read-from-minibuffer-map)
       (use-local-map (ergoemacs ergoemacs-read-from-minibuffer-map))
       (setq ergoemacs-read-from-minibuffer-map nil
             ergoemacs-map--breadcrumb ""))
     ;; Run deferred "hooks"
     (when (and (minibufferp) ergoemacs-component-struct--composed-hook-minibuffer)
       (dolist (elt (reverse ergoemacs-component-struct--composed-hook-minibuffer))
     	 (when (equal (ergoemacs (symbol-value elt) :map-key)
     		      (ergoemacs ergoemacs-read-from-minibuffer-map :map-key))
     	   (use-local-map (make-composed-keymap (cdr elt) (current-local-map))))
     	 (set (make-local-variable (car elt)) (make-composed-keymap (cdr elt) (symbol-value (car elt)))))
       (setq ergoemacs-component-struct--composed-hook-minibuffer nil)))
    (setq ergoemacs-map--modify-active-last-overriding-terminal-local-map overriding-terminal-local-map
          ergoemacs-map--modify-active-last-overriding-local-map overriding-local-map
          ergoemacs-map--modify-active-last-char-map char-map
          ergoemacs-map--modify-active-last-local-map local-map)
    (ergoemacs-map--emulation-mode-map-alists)
    (ergoemacs-map--minor-mode-map-alist ini)
    (ergoemacs-map--minor-mode-overriding-map-alist)))


(defvar ergoemacs-map--quit-map nil
  "Keymap of quit keys for local keymap.")

(defun ergoemacs-map--install ()
  "Install `ergoemacs-mode' into the appropriate keymaps."
  (interactive)
  (ergoemacs-map--hashkey)
  (ergoemacs-mode-line)
  (define-key ergoemacs-menu-keymap [menu-bar ergoemacs-mode]
    `("ErgoEmacs" . ,(ergoemacs-theme--menu (ergoemacs :current-theme))))

  (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
    (while x
      (setq minor-mode-map-alist (delq x minor-mode-map-alist))
      ;; Multiple menus sometimes happen because of multiple
      ;; ergoemacs-mode variables in minor-mode-map-alist
      (setq x (assq 'ergoemacs-mode minor-mode-map-alist)))
    (push (cons 'ergoemacs-mode ergoemacs-menu-keymap) minor-mode-map-alist))
  
  (setq ergoemacs-map-- (make-hash-table :test 'equal)
        ergoemacs-map--lookup-hash (make-hash-table :test 'equal)
        ergoemacs-keymap (ergoemacs)
        ergoemacs-map--alist (make-hash-table)
        ergoemacs-map--alists (make-hash-table)
        ergoemacs-map--alist-t (make-hash-table)
        ergoemacs-map--alist-t-o (make-hash-table)
        ergoemacs-map--quit-map (make-sparse-keymap)
        ergoemacs-saved-global-map global-map
        global-map ergoemacs-keymap)
  (use-global-map global-map)
  ;; Setup the quit map
  (dolist (key (where-is-internal 'keyboard-quit))
    (when (= 1 (length key))
      (define-key ergoemacs-map--quit-map key 'keyboard-quit)))
  (ergoemacs ergoemacs-map--quit-map :label '(ergoemacs-quit))
  
  ;; Put `ergoemacs-mode' style key shortcuts instead of emacs
  ;; style shortcuts (They need to place the correct shortucts)
  (ergoemacs-menu--filter (lookup-key ergoemacs-keymap [menu-bar]))
  (ergoemacs-map--modify-active t)
  (ergoemacs-component-struct--create-hooks)
  (add-hook 'ergoemacs-post-command-hook #'ergoemacs-map--modify-active))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-map--install)

(defvar ergoemacs-mode)
(defun ergoemacs-map--remove ()
  "Remove `ergoemacs-mode' keybindings."
  (interactive)
  ;; Restore menu-bar
  ;; Not needed; Global map isn't modified...
  (let (ergoemacs-mode)
    (setq ergoemacs-map--alist (make-hash-table)
          ergoemacs-map--alists (make-hash-table)
          global-map (ergoemacs :revert-global-map)
          ergoemacs-saved-global-map  nil)
    (use-global-map global-map)
    (ergoemacs-map--modify-active t)
    (ergoemacs-component-struct--rm-hooks)
    (dolist (map ergoemacs-map--modified-maps)
      (ergoemacs :spinner '("rm ⌨→%s" "rm ergoemacs→%s" "rm ergoemacs->%s") map)
      (set map (ergoemacs (ergoemacs-sv map) :revert-original)))))

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

(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-map--remove)

(autoload 'ergoemacs (expand-file-name "ergoemacs-macros.el" ergoemacs-dir) nil t)
(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
