;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-map.el
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

(defun ergoemacs-map--alist-atom (a b breadcrumb-base &optional original-user)
  (let ((tmp)
        (hash-table (or (and original-user ergoemacs-map--alist-t-o) ergoemacs-map--alist-t))
        (breadcrumb-add (or (and original-user (format "%s:o" a)) (format "%s" a))))
    (cond
     ((eq a t)
      (setq ergoemacs-map--breadcrumb ""
            tmp (ergoemacs-gethash (cdr b) hash-table))
      (unless tmp
        (setq tmp (or (and original-user (ergoemacs b :original-user)) (ergoemacs b)))
        (puthash (cdr b) tmp hash-table)))
     (t
      (setq ergoemacs-map--breadcrumb (format "%s:%s" breadcrumb-base breadcrumb-add)
            tmp (or (and original-user (ergoemacs b :original-user)) (ergoemacs b)))))
    (cons a tmp)))

(defun ergoemacs-map--alist (alist &optional symbol)
  "Apply maps for ALIST."
  (let ((old-breadcrumb ergoemacs-map--breadcrumb)
        breadcrumb-base type old-len)
    (if (and symbol (setq old-len (ergoemacs-gethash symbol ergoemacs-map--alist))
             (= (length alist) old-len)) alist
      (when symbol
        (puthash symbol (length alist) ergoemacs-map--alist)
        (setq breadcrumb-base (format "%s:%s" old-breadcrumb symbol)))
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
        (setq ergoemacs-map--breadcrumb old-breadcrumb)))))

(defvar ergoemacs-map--alists (make-hash-table))
(defun ergoemacs-map--alists (alists &optional symbol)
  "Apply maps for ALISTS"
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
                         (define-key ret new-key item)
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
                       (define-key ret (vector 'ergoemacs-remap tmp) item)))))
               lookup-keymap)
              )
            (ergoemacs ret :label (list (ergoemacs lookup-keymap :key-hash) 'ergoemacs-mode (intern ergoemacs-keyboard-layout))))
          (setq tmp (ergoemacs-component-struct--lookup-list lookup-keymap))
          (setq composed-list (or (and ret (or (and tmp (append tmp (list ret))) (list ret))) tmp))
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

(defun ergoemacs-map-- (&optional lookup-keymap layout map recursive)
  "Get map looking up changed keys in LOOKUP-MAP based on LAYOUT.

MAP can be a `ergoemacs-component-struct', or a string/symbol
of a calculated or uncalcuated component in
`ergoemacs-component-hash'

MAP can also be a list of `ergoemacs-component-struct' values
or string/symbols that are in `ergoemacs-component-hash'

If missing, MAP represents the current theme compenents, from `ergoemacs-theme-components'

LAYOUT represents the layout that is used.

RECURSIVE is an internal argument to make sure that infinite
loops do not occur.

LOOKUP-KEYMAP represents what should be calculated/looked up.

If LOOKUP-KEYMAP is a keymap, lookup the ergoemacs-mode modifications to that keymap.

If LOOKUP-KEYMAP 
"
  (let* ((cur-layout (or layout ergoemacs-keyboard-layout))
         lookup-key
         (map (ergoemacs-component-struct--lookup-hash (or map (ergoemacs-theme-components))))
         unbind-list
         use-local-unbind-list-p
         local-unbind-list
         parent
         composed-list
         tmp
         tmp2
         tmp3
         unbound-passthrough
         ret
         menu-bar
         only-modify-p
         (lookup-keymap lookup-keymap)
         hook-overrides
         hook-deferred)
    (cond
     ((and lookup-keymap (symbolp lookup-keymap) (ergoemacs-gethash lookup-keymap ergoemacs-translation-hash))
      (ergoemacs-command-loop--modal lookup-keymap))
     ((consp (ergoemacs lookup-keymap :map-key)) ;; Ignore already installed.
      lookup-keymap)
     ((and lookup-keymap (ergoemacs lookup-keymap :dont-modify-p))
      lookup-keymap)
     ((ergoemacs-component-struct-p map)
      (let ((ret (cond
                  ((and (not lookup-keymap)
                        (string= cur-layout (ergoemacs-component-struct-layout map)))
                   (ergoemacs-component-struct-map map))
                  ((and (not lookup-keymap)
                        (setq ret (ergoemacs-gethash
                                   (list nil (intern cur-layout))
                                   (ergoemacs-component-struct-calculated-layouts map))))
                   ret)
                  ((not lookup-keymap)
                   ;; Overall layout hasn't been calculated.
                   (ergoemacs-component-struct--get map cur-layout nil))
                  (t
                   (error "Cant calculate/lookup keymap.")))))
        ret))
     ((and (consp map) ;; Don't do anything with blank keymaps.
           lookup-keymap
           (or (equal lookup-keymap (make-sparse-keymap))
               (equal lookup-keymap (make-keymap))))
      lookup-keymap)
     ((and (consp map)
           (progn
             (setq unbind-list
                   (ergoemacs-cache unbind-list
                     (dolist (cur-map map)
                       (setq unbind-list
                             (append unbind-list
                                     (ergoemacs-component-struct--translated-list
                                      cur-map (ergoemacs-component-struct-unbind cur-map)))))
                     unbind-list)) t))
      (cond
       ((not lookup-keymap)
        ;; The `undefined-key' layer
        (setq tmp (ergoemacs-cache global-menu
                    (setq tmp (make-sparse-keymap))
                    (dolist (cur-map map)
                      (dolist (undefined-key
                               (ergoemacs-component-struct-undefined cur-map) ;; (ergoemacs-component-struct--translated-list cur-map )
                               )
                        (unless (member undefined-key ergoemacs-map--undefined-keys)
                          (push undefined-key ergoemacs-map--undefined-keys))))
                    (dolist (i ergoemacs-map--undefined-keys)
                      (define-key tmp i #'ergoemacs-map-undefined))
                    
                    (ergoemacs tmp :label (list (ergoemacs (ergoemacs :global-map) :key-hash) 'ergoemacs-undefined (intern ergoemacs-keyboard-layout)))
                    
                    (push tmp composed-list)
                    
                    (push (lookup-key (ergoemacs :global-map) [menu-bar]) menu-bar)
                    
                    ;; Each ergoemacs theme component
                    (dolist (cur-map (reverse map))
                      (setq tmp (ergoemacs-map-- lookup-keymap layout cur-map t))
                      (unless (ergoemacs tmp :empty-p)
                        (cond
                         ((setq tmp2 (lookup-key tmp [menu-bar]))
                          (push (copy-keymap tmp2) menu-bar)
                          (setq tmp2 (make-sparse-keymap))
                          (map-keymap
                           (lambda (event item)
                             (unless (eq event 'menu-bar)
                               (define-key tmp2 (vector event) item)))
                           tmp)
                          (unless (ergoemacs tmp2 :empty-p)
                            (push tmp2 composed-list)))
                         (t (push tmp composed-list)))))
                    
                    (ergoemacs-timing setup-ergoemacs-hash
                      (dolist (ret composed-list)
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
                         ret)))
                    
                    ;; The real `global-map'
                    (setq tmp (make-composed-keymap menu-bar))

                    ;; The global `menu-bar'
                    (let ((i 0)
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
                                              (< i1 i2))))))
                    
                    
                    ;; The keys that will be unbound
                    (setq ret (make-sparse-keymap))
                    (dolist (key unbind-list)
                      (define-key ret key nil))
                    (ergoemacs ret :label (list (ergoemacs (ergoemacs :global-map) :key-hash) 'ergoemacs-unbound (intern ergoemacs-keyboard-layout)))
                    tmp))
        (setq parent (copy-keymap (ergoemacs :global-map))
              composed-list (ergoemacs-cache global-composed-list composed-list)
              ret (ergoemacs-cache global-ret ret)
              ergoemacs-map-- (ergoemacs-cache ergoemacs-map-- ergoemacs-map--)
              ergoemacs-map--lookup-hash (ergoemacs-cache ergoemacs-map--lookup-hash ergoemacs-map--lookup-hash)
              ergoemacs-map--undefined-keys (ergoemacs-cache undefined-keys ergoemacs-map--undefined-keys))
        (define-key parent [menu-bar] tmp)
        ;; (map-keymap
        ;;  (lambda (event item)
        ;;    (define-key parent (vector menu-bar event) item))
        ;;  tmp)
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
        (setq ret (make-composed-keymap tmp2 ret)))
       
       ;; Now create the keymap for a specified `lookup-keymap'
       (lookup-keymap
        ;; (unless (string= "" ergoemacs-map--breadcrumb)
        ;;   (when (not (ergoemacs lookup-keymap :map-key))
        ;;     (ergoemacs lookup-keymap :label)
        ;;     (puthash (intern ergoemacs-map--breadcrumb) (ergoemacs lookup-keymap :map-key) ergoemacs-breadcrumb-hash)
        ;;     (puthash (ergoemacs lookup-keymap :map-key) (intern ergoemacs-map--breadcrumb) ergoemacs-breadcrumb-hash)))
	(ergoemacs lookup-keymap :label)
        (setq lookup-keymap (ergoemacs lookup-keymap :original)
              hook-overrides (ergoemacs lookup-keymap :override-maps)
              use-local-unbind-list-p (ergoemacs lookup-keymap :use-local-unbind-list-p))
        (setq only-modify-p (ergoemacs lookup-keymap :only-local-modifications-p)
              lookup-key (ergoemacs-map--lookup-keymap-key lookup-keymap)
              composed-list  (ergoemacs-map--composed-list lookup-keymap lookup-key only-modify-p use-local-unbind-list-p)
              ret (nth 2 composed-list)
              local-unbind-list (nth 1 composed-list)
              composed-list (nth 0 composed-list)
              parent (and (not recursive) lookup-keymap))
        (when hook-overrides
          ;; Need to fix 'ergoemacs-remaps as well.
          ;; This is done here because hooks can change, and the
          ;; cached map would be invalid.
          (setq tmp (ergoemacs global-map :keys)
                tmp2 (make-sparse-keymap)
                tmp3 (make-composed-keymap hook-overrides))
          (ergoemacs tmp2 :label '(fix-hook-remaps))
          (ergoemacs-timing calculate-ergoemacs-remap
            (ergoemacs-map-keymap
             (lambda(key item)
               (unless (or (eq item 'ergoemacs-prefix)
                           (consp key))
                 (let ((key (vconcat key)))
                   (when (member key tmp)
                     (define-key ret (vector 'ergoemacs-remap (ergoemacs-gethash key (ergoemacs global-map :lookup))) nil)))))
             tmp3))
          (unless (ergoemacs tmp2 :empty-p)
            (push tmp2 hook-overrides))
          (setq composed-list (append hook-overrides composed-list)))
        (when (setq hook-deferred (ergoemacs lookup-keymap :deferred-maps))
          (setq tmp (ergoemacs global-map :keys)
                tmp2 (make-sparse-keymap))
          (ergoemacs tmp2 :label '(fix-hook-remaps))
          (ergoemacs-timing calc-remaps
            (ergoemacs-map-keymap
             (lambda(key item)
               (unless (or (eq item 'ergoemacs-prefix)
                           (consp key))
                 (let ((key (vconcat key)))
                   (when (member key tmp)
                     (define-key ret (vector 'ergoemacs-remap (ergoemacs-gethash key (ergoemacs global-map :lookup))) item)))))
             (make-composed-keymap hook-deferred)))
          (unless (ergoemacs tmp2 :empty-p)
            (push tmp2 hook-deferred))
          (setq composed-list (append composed-list hook-deferred)))
        (setq unbound-passthrough (make-sparse-keymap))
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
        (cond
         ((and only-modify-p composed-list)
          ;; Get the protecting user keys
          (setq ret (make-composed-keymap composed-list parent))
          (setq tmp (ergoemacs parent :user))
          (when tmp
            (setq ret (make-composed-keymap tmp ret)))
          ret)
         ((and only-modify-p (not composed-list))
          (setq ret parent))
         (t
          ;; The keys that will be unbound
          (setq ret (ergoemacs-cache (and lookup-key (intern (format "%s-unbound-keymap" lookup-key)))
                      (setq ret (make-sparse-keymap))
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
                      ret))
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
              (push map ergoemacs-map--modified-maps)))))))
      ret)
     ((and (not composed-list) parent)
      (unwind-protect
          (progn
            (set-keymap-parent lookup-keymap nil)
            (setq ret (ergoemacs-map-- lookup-keymap layout map t)))
        (set-keymap-parent lookup-keymap parent))
      (setq parent (ergoemacs-map-- parent layout map t))
      (set-keymap-parent ret parent)
      ret)
     (composed-list
      (make-composed-keymap
       (mapcar
        (lambda(x)
          (ergoemacs-map-- x layout map t))
        composed-list)
       (ergoemacs-map-- parent layout map t)))
     (t
      (ergoemacs-warn "Component map isn't a proper argument for `ergoemacs-map'")
      (ergoemacs-warn "\tLookup:%s" lookup-keymap)
      (ergoemacs-warn "\tLayout:%s" layout)
      (ergoemacs-warn "\tMap:%s" map)
      (ergoemacs-warn "\tRecursive:%s" recursive)
      lookup-keymap))))

(defun ergoemacs-map--temporary-map-properties (map)
  "Test if MAP is a transient map that `ergoemacs-mode' does not touch.

This occurs when the keymap is not known to `ergoemacs-mode' and
it is not a composed keymap.

If it is a tranisent map, assign the :dont-modify-p property to t."
  (unless ergoemacs-modify-transient-maps
    (let ((map-key (ergoemacs map :map-key)))
      (unless (or map-key (ergoemacs map :composed-p))
        (ergoemacs map :label '(temporary-map))
        (ergoemacs map :dont-modify-p t)))))


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
               (not (ergoemacs overriding-terminal-local-map :installed-p)))
      (ergoemacs-map--temporary-map-properties overriding-terminal-local-map)
      (setq overriding-terminal-local-map (ergoemacs overriding-terminal-local-map)))
    
    (when (and overriding-local-map
               (not (eq overriding-local-map ergoemacs-map--modify-active-last-overriding-local-map))
               (not (ergoemacs overriding-local-map :installed-p)))
      (ergoemacs-map--temporary-map-properties overriding-local-map)
      (setq overriding-local-map (ergoemacs overriding-local-map)))

    (ergoemacs-save-buffer-state
     (when (and (car char-map)
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
       (ergoemacs-command-loop--minibuffer-supported-p ergoemacs-map--breadcrumb)
       (use-local-map (ergoemacs ergoemacs-read-from-minibuffer-map))
       (setq ergoemacs-read-from-minibuffer-map nil
             ergoemacs-map--breadcrumb "")))
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

(defun ergoemacs-map-undefined ()
  "This key is undefined in `ergoemacs-mode'.

If `ergoemacs-mode' knows what the new key or key sequence that
runs the same command, tell the user."
  (interactive)
  (let ((key (ergoemacs-key-description (this-single-command-keys)))
        (old-key (lookup-key (ergoemacs :global-map) (this-single-command-keys))))
    (cond
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
