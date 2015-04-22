;;; ergoemacs-mapkeymap.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-mapkeymap.el
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

(defvar ergoemacs-map-properties--plist-hash)

(declare-function ergoemacs-map-properties--key-struct "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--keymap-value "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--all-sparse-p "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--put "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--get-or-generate-map-key "ergoemacs-map-properties")

(declare-function ergoemacs-debug "ergoemacs-debug")
(declare-function ergoemacs-debug-heading "ergoemacs-debug")

(declare-function ergoemacs-setcdr "ergoemacs-lib")

(defvar ergoemacs-mapkeymap--current nil)
(defvar ergoemacs-mapkeymap--submaps nil)
(defvar ergoemacs-mapkeymap--prefixes nil)
(defvar ergoemacs-mapkeymap--key nil)
(defvar ergoemacs-mapkeymap--nil nil)
(defvar ergoemacs-mapkeymap--debug nil)
(defvar ergoemacs-mapkeymap--maps nil)


(defun ergoemacs-mapkeymap--key-keymap (key keymap function submaps &optional prefix)
  "Handle KEYMAPs"
  (when function
    (when ergoemacs-mapkeymap--debug
      (ergoemacs-debug "Call (%s %s %s %s)" function key 'ergoemacs-prefix (or prefix t)))
    (funcall function key 'ergoemacs-prefix (or prefix t)))
  (when (and ergoemacs-mapkeymap--key (not prefix))
    ;; (pushnew key ergoemacs-mapkeymap--prefixes :test 'equal)
    (unless (or (member key (append ergoemacs-mapkeymap--prefixes ergoemacs-mapkeymap--nil))
                (not (ergoemacs-keymapp (lookup-key ergoemacs-mapkeymap--current key))))
      (when ergoemacs-mapkeymap--debug
        (ergoemacs-debug "Add prefix %s" key))
      (push key ergoemacs-mapkeymap--prefixes)))
  (if (and submaps (not (eq submaps :prefix)) (ergoemacs-map-properties--key-struct keymap))
      ;; (pushnew (cons key (ergoemacs-map-properties--key-struct keymap))
      ;; ergoemacs-mapkeymap--submaps :test 'equal)
      (unless (member (cons key (ergoemacs-map-properties--key-struct keymap)) (append ergoemacs-mapkeymap--submaps ergoemacs-mapkeymap--nil))
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Add submap %s" (cons key (ergoemacs-map-properties--key-struct keymap)) ))
        (push (cons key (ergoemacs-map-properties--key-struct keymap)) ergoemacs-mapkeymap--submaps))
    (ergoemacs-mapkeymap--loop
     function (ergoemacs-map-properties--keymap-value keymap) submaps key)))

(defun ergoemacs-mapkeymap--key-item (key item function submaps &optional prefix)
  "Process an ITEM for KEY and possibly call FUNCTION, or defer keymap evaluation when SUBMAPS is true.
PREFIX is the current PREFIX for the key code. "
  (cond
   ;; Already ignored keys
   ;; ((member key ergoemacs-mapkeymap--nil))
   
   ;; FIXME -- key ranges... What should be done here...?

   ;; keys defined to be nil, should mask other keys
   ((not item)
    ;; Make nil keys mask other keys

    ;; FIXME do a range too.., but I'm not sure that should be
    ;; supported by emacs full keymaps...?
    (when ergoemacs-mapkeymap--debug
      (ergoemacs-debug "Add key %s to ergoemacs-mapkeymap--nil" (key-description key)))
    (push key ergoemacs-mapkeymap--nil)
    (when function
      (funcall function key nil (or prefix t))))

   ;; Ignore already defined keys
   ((and (vectorp key) (lookup-key ergoemacs-mapkeymap--current key))
    (when ergoemacs-mapkeymap--debug
      (ergoemacs-debug "Ignore %s=%s " (key-description key)
                       (lookup-key ergoemacs-mapkeymap--current key))))

   (;; (key . symbol)
    (or (symbolp item)
        ;; lambda items
        (commandp item)
        ;; (key menu-item . details)
        (ignore-errors
          (eq 'menu-item (nth 0 item)))
        ;; (key "Desc" . symbol)
        (ignore-errors
          (and (stringp (nth 0 item))
               (or (symbolp (nthcdr 1 item))
                   (commandp (nthcdr 1 item)))))
        ;; (key "Desc" "Help" . symbol)
        (ignore-errors
          (and (stringp (nth 0 item))
               (stringp (nth 1 item))
               (or (symbolp (nthcdr 2 item))
                   (commandp (nthcdr 2 item))))))
    (ergoemacs-mapkeymap--define-key key item prefix)
    (when function
      (when ergoemacs-mapkeymap--debug
        (ergoemacs-debug "Call (%s %s %s %s)" function key item (or prefix t))
        (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                         (key-description key)))
      (funcall function key item (or prefix t))))

   ;; (key keymap)
   ((ergoemacs-keymapp item)
    ;; Prefix Key
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-properties--all-sparse-p item)
          (progn
            (when ergoemacs-mapkeymap--debug
              (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                               (key-description key)))
            (ergoemacs-mapkeymap--define-key
             key (make-sparse-keymap) prefix))
        (ergoemacs-mapkeymap--define-key key (make-keymap) prefix)
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Setup prefix %s to be a full keymap"
                           (key-description key)))))
    (ergoemacs-mapkeymap--key-keymap key item function submaps prefix))

   ;; (key "String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (ergoemacs-keymapp (nthcdr 1 item))))
    ;; Install prefix as sparse/full keymap
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-properties--all-sparse-p (nthcdr 1 item))
          (progn
            (when ergoemacs-mapkeymap--debug
              (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                               (key-description key)))
            (ergoemacs-mapkeymap--define-key key
                                             `(,(nth 0 item) ,@(make-sparse-keymap)) prefix))
        (ergoemacs-mapkeymap--define-key key
                                         `(,(nth 0 item) ,@(make-keymap)) prefix)
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Setup prefix %s to be a full keymap"
                           (key-description key)))))
    (ergoemacs-mapkeymap--key-keymap key (nthcdr 1 item) function submaps prefix))
   
   ;; (key "String" "Help-String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (stringp (nth 1 item))
           (ergoemacs-keymapp (nthcdr 2 item))))
    ;; Install prefix as sparse/full keymap
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-properties--all-sparse-p (nthcdr 2 item))
          (progn
            (when ergoemacs-mapkeymap--debug
              (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                               (key-description key)))
            (ergoemacs-mapkeymap--define-key key
                                             `(,(nth 0 item) ,(nth 1 item)
                                               ,@(make-sparse-keymap)) prefix))
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Setup prefix %s to be a full keymap"
                           (key-description key)))
        (ergoemacs-mapkeymap--define-key key
                                         `(,(nth 0 item) ,(nth 1 item) ,@(make-keymap)) prefix)))
    (ergoemacs-mapkeymap--key-keymap key (nthcdr 2 item) function submaps prefix))
   
   ((ignore-errors (string-match-p "^-+$" (format (car item))))
    ;; Separators...
    (ergoemacs-mapkeymap--define-key key item prefix)
    (when function
      (when ergoemacs-mapkeymap--debug
        (ergoemacs-debug "Call (%s %s %s %s)" function key item (or prefix t))
        (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                         (key-description key)))
      (funcall function key item (or prefix t))))
   (t
    (warn "Could not extract\n\tkey:\"%s\" (%s) \n\tItem: \"%s\"" key (key-description key) item))))

(defun ergoemacs-map-force-full-keymap (keymap)
  "Forces KEYMAP to be a full keymap."
  (if (ignore-errors (char-table-p (nth 1 keymap))) keymap
    (progn
      (when ergoemacs-mapkeymap--debug
        (ergoemacs-debug "Forced a full keymap!"))
      (ergoemacs-setcdr keymap (cons (nth 1 (make-keymap)) (cdr keymap))))
    keymap))

(defun ergoemacs-map-set-char-table-range (keymap range value)
  "Sets the KEYMAP's char-table RANGE to VALUE.
If KEYMAP is not a full keymap, make it a full keymap."
  (set-char-table-range
   (nth 1 (ergoemacs-map-force-full-keymap keymap)) range value))

(defun ergoemacs-mapkeymap--define-key (key item &optional prefix)
  "Defines KEY to be ITEM for ergoemacs-mapkeymap--current.
KEY could be a cons for a range if the keymap is a full keymap, otherwise KEY is a vector."
  (cond
   ((vectorp key)
    (when ergoemacs-mapkeymap--debug
      (ergoemacs-debug "Define %s %s" (key-description key) item))
    (ergoemacs-advice--real-define-key
     ergoemacs-mapkeymap--current key item))
   ((consp key) ;; Char table range.
    (when ergoemacs-mapkeymap--debug
      (ergoemacs-debug "Define key range %s to %s" key item))
    (ergoemacs-map-set-char-table-range
     (if prefix (lookup-key ergoemacs-mapkeymap--current prefix)
       ergoemacs-mapkeymap--current) key item))
   (t
    (warn "Did not handle ergoemacs-mapkeymap--define-key %s %s %s" key item prefix))))

(defun ergoemacs-mapkeymap--loop (function keymap submaps &optional prefix)
  "Loops over keys in KEYMAP.
FUNCTION is the function to call.
SUBMAPS indicates if submaps should be deferred.
PREFIX is the prefix key where the map is being examined."
  (let (key)
    (dolist (item keymap)
      ;; (warn "%s %s" prefix item)
      (cond
       ;; Format of keymap
       ((eq item 'keymap)) ;; keymap symbol, ignore
       ((stringp item)) ;; Strings, ignore
       ((ergoemacs-keymapp item)
        ;; composed keymap, extract keys with same prefix
        (ergoemacs-mapkeymap--loop
         function (ergoemacs-map-properties--keymap-value item) submaps prefix))
       ((ignore-errors (char-table-p item))
        (setq key item)
        (while key
          (map-char-table
           (lambda(key-2 value)
             ;; Can be a list (from . to)
             (if (consp key-2)
                 (if (eq (car key-2) (cdr key-2))
                     (ergoemacs-mapkeymap--key-item
                      (or (and prefix (vconcat prefix (vector (car key-2))))
                          (vector (car key-2))) value function submaps prefix)
                   (ergoemacs-mapkeymap--key-item
                    key-2 value function submaps prefix))
               (unless (eq key-2 'ergoemacs-labeled)
                 (ergoemacs-mapkeymap--key-item
                  (or (and prefix (vconcat prefix (vector key-2)))
                      (vector key-2)) value function submaps prefix)))
             (when function
               (funcall function key-2 value nil)))
           key)
          (setq key (char-table-parent key))))
       ((consp item)
        ;; Type
        (let* ((key (or (and prefix (vconcat prefix (vector (car item))))
                        (vector (car item)))))
          (unless (eq (car item) 'ergoemacs-labeled) ;; ignore labels
            (ergoemacs-mapkeymap--key-item
             key (cdr item) function submaps prefix))))
       ;; Ignore [()] keys like:
       ;;(keymap "Select Buffer" [("*scratch*" (nil)...) ...])
       ((vectorp item))
       (t
        (warn "Could not extract Item: \"%s\"" item))))))

(defun ergoemacs-mapkeymap (function keymap &optional submaps)
  "Call FUNCTION for all keys in hash table KEYMAP.
FUNCTION is called with three arguments KEY, VALUE and PREFIX.

When SUBMAPS is non-nil, will map over the whole keymap with the
exception of the submaps.  Once finished mapping over the main
map, map over each submap.  It will also assign properties to the
maps.

When SUBMAPS is :prefix, only extract prefixes of the keymap,
don't recurse to prefix keys

Will return a collapsed keymap without parent"
  (let (ret sub tmp1 tmp2 tmp3)
    (when ergoemacs-mapkeymap--current
      (push (list :current ergoemacs-mapkeymap--current
                  :submaps ergoemacs-mapkeymap--submaps
                  :prefixes ergoemacs-mapkeymap--prefixes
                  :key ergoemacs-mapkeymap--key
                  :nil ergoemacs-mapkeymap--nil)
            ergoemacs-mapkeymap--maps))
    (unwind-protect
        (progn
          (when ergoemacs-mapkeymap--debug
            (ergoemacs-debug-heading "ergoemacs-mapkeymap"))
          (setq ergoemacs-mapkeymap--current
                (if (ergoemacs-map-properties--all-sparse-p keymap)
                    (make-sparse-keymap)
                  (make-keymap))
                ergoemacs-mapkeymap--submaps nil
                ergoemacs-mapkeymap--prefixes nil
                ergoemacs-mapkeymap--key (ergoemacs-map-properties--key-struct keymap) 
                ergoemacs-mapkeymap--nil '())
          (ergoemacs-mapkeymap--loop
           function (ergoemacs-map-properties--keymap-value keymap) submaps)
          (when (and ergoemacs-mapkeymap--prefixes submaps)
            (ergoemacs-map-properties--put keymap :prefixes ergoemacs-mapkeymap--prefixes))
          (setq sub ergoemacs-mapkeymap--submaps
                ret ergoemacs-mapkeymap--current
                ergoemacs-mapkeymap--current nil
                ergoemacs-mapkeymap--nil nil
                ergoemacs-mapkeymap--prefixes nil
                ergoemacs-mapkeymap--submaps nil
                ergoemacs-mapkeymap--key nil)
          (when (and sub submaps)
            (ergoemacs-map-properties--put keymap :submaps sub)
            (dolist (item sub)
              (unless (member (cdr item) tmp1)
                ;; Now map keymap
                (ergoemacs-mapkeymap function (ergoemacs-map-properties--keymap-value (cdr item)) t)
                ;; Make sure submap is assigned
                (setq tmp2 (ergoemacs-map-properties--put (cdr item) :submap-p)
                      tmp3 (cons (car item) (ergoemacs-map-properties--get-or-generate-map-key keymap)))
                (unless (member tmp3 tmp2)
                  (push tmp3 tmp2))
                (ergoemacs-map-properties--put (ergoemacs-map-properties--keymap-value (cdr item)) :submap-p tmp2)
                (push (cdr item) tmp1)))))
      (if (not ergoemacs-mapkeymap--maps)
          (setq ergoemacs-mapkeymap--current nil
                ergoemacs-mapkeymap--nil nil
                ergoemacs-mapkeymap--prefixes nil
                ergoemacs-mapkeymap--submaps nil
                ergoemacs-mapkeymap--key nil)
        (setq tmp1 (pop ergoemacs-mapkeymap--maps)
              ergoemacs-mapkeymap--current (plist-get tmp1 :current)
              ergoemacs-mapkeymap--nil (plist-get tmp1 :nil)
              ergoemacs-mapkeymap--prefixes (plist-get tmp1 :prefixes)
              ergoemacs-mapkeymap--submaps (plist-get tmp1 :submaps)
              ergoemacs-mapkeymap--key (plist-get tmp1 :key))))
    ret))

(provide 'ergoemacs-mapkeymap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-mapkeymap.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
