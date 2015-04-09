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
;; (require 'guide-key nil t)

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(defvar ergoemacs-ignored-prefixes '(;; "C-h" "<f1>"
                                     [27]  [escape]
                                     [remap]
                                     [left-fringe]
                                     [vertical-line]
                                     [vertical-scroll-bar]
                                     [header-line]
                                     [mode-line]
                                     [menu-bar]
                                     [C-down-mouse-2]))

(defvar ergoemacs-original-map-hash (make-hash-table)
  "Hash table of the original maps that `ergoemacs-mode' saves.")

(defun ergoemacs-submap-p (keymap)
  "Returns if this is a submap of another keymap.
If a submap, return a list of the keys and parent map(s)
If not a submap, return nil
"
  (ergoemacs-map-get keymap :submap-p))

(defun ergoemacs-submaps (keymap)
  "Returns the known submaps of this keymap."
  (ergoemacs-map-get keymap :submaps))


(defun ergoemacs-emacs-shift-translate (key-seq &optional modifier prefix)
  "Uses emacs style shift-translation: M-Q becomes M-q.

KEY-SEQ must be a vector.  If there is no need to shift-translate
the key sequence return nil.

Optionally you can change how this function behaves.

Instead of translating the shifted key to the unshifted key, you
can remove another modifier.  For example if you wanted to
convert C-M-a to C-a, you could use 'meta as the MODIFIER
argument to remove the M- modifier.

The PREFIX argument can add a key before the key where the
modifier occurred, such as in `ergoemacs-meta-to-escape'.
"
  (if (not (vectorp key-seq)) nil
    (let ((rev-seq (reverse (append key-seq ())))
          (which-mod (or modifier 'shift))
          modifiers new-mod
          found 
          (seq '()))
      (dolist (event rev-seq)
        (setq modifiers (event-modifiers event))
        (if (not (memq which-mod modifiers)) (push event seq)
          (setq new-mod (list (event-basic-type event)))
          (dolist (mod modifiers)
            (unless (eq which-mod mod)
              (push mod new-mod)))
          (push (event-convert-list new-mod) seq)
          (when prefix
            (push prefix seq))
          (setq found t)))
      (if found (vconcat seq) nil))))

(defun ergoemacs-meta-to-escape (key-seq)
  "Escapes a KEY-SEQ M-q becomes ESC q.
KEY-SEQ must be a vector.  If there is no need to escape the key sequence return nil."
  (ergoemacs-emacs-shift-translate key-seq 'meta 27))

(defcustom ergoemacs-ignore-prev-global t
  "If non-nil, the ergoemacs-mode will ignore previously defined global keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

;; for compatability 
;;;###autoload
(defun ergoemacs-ignore-prev-global ()
  "Ignore previously defined global keys."
  (setq ergoemacs-ignore-prev-global t))

(defvar ergoemacs-map-plist-hash (make-hash-table :test 'equal))

(defvar ergoemacs-global-map nil
  "Original emacs global map before any customizations are made.")

(defun ergoemacs-map-keymap-value (keymap)
  "Return the keymap value of KEYMAP.
KEYMAP can be a symbol, keymap or ergoemacs-mode keymap"
  (let (tmp)
    (or (and (listp keymap) (ergoemacs-keymapp keymap) keymap)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-value keymap))) tmp)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-function keymap))) tmp)
        ;; (ignore-errors (and (setq tmp (gethash keymap ergoemacs-map-plist-hash))
        ;;                     (setq tmp (gethash :map-list tmp))
        ;;                     (symbol-value (car tmp))))
        ;; (ignore-errors (and (setq tmp (plist-get keymap :map-list)) (symbol-value (nth 0 tmp))))
        )))

(defun ergoemacs-map-composed-p (keymap)
  "Determine if the KEYMAP is a composed keymap."
  (and (ignore-errors (eq 'keymap (car keymap)))
       (ignore-errors (eq 'keymap (caadr keymap)))))

;; FIXME: Write test or function
(defun ergoemacs-map-all-sparse-p (keymap)
  "Determines if all components of a KEYMAP are sparse keymaps.
This does not include submaps, which may also be a full keymap."
  (if (not (ergoemacs-keymapp keymap)) t
    (let ((ret t)
          (kv (ergoemacs-map-keymap-value keymap)))
      (cond
       ((ergoemacs-map-composed-p kv)
        (setq ret
              (catch 'found-full
                (dolist (map (ergoemacs-map-composed-list keymap))
                  (unless (ergoemacs-map-all-sparse-p map)
                    (throw 'found-full nil))) t)))
       (t
        (setq ret (not (ignore-errors (char-table-p (nth 1 kv)))))))
      (when ret
        (setq ret (ergoemacs-map-all-sparse-p (keymap-parent keymap))))
      ret)))

(defun ergoemacs-map-composed-list (keymap &optional melt label)
  "Return the list of maps in a composed KEYMAP.
If there are no maps, return nil.
When MELT is true, combine all the keymaps (with the exception of the parent-map)"
  (if (not (ergoemacs-map-composed-p keymap)) nil
    (let ((parent (keymap-parent keymap))
          ret)
      (unwind-protect
          (progn
            (when parent
              (set-keymap-parent keymap nil))
            (dolist (map (reverse (cdr keymap)))
              (when label
                (ergoemacs-map--label map))
              (if melt
                  (setq ret (append (cdr map) ret))
                (push (cons (car map) (cdr map)) ret))))
        (when parent
          (set-keymap-parent keymap parent))
        (when melt
          (setq ret (append '(keymap) ret))))
      ret)))

(defun ergoemacs-map-composed (keymap &optional force)
  "Returns a list of `ergoemacs-mode' map-key for the composed keymap list"
  (let ((composed-list (ergoemacs-map-composed-list keymap nil force)))
    (and composed-list
         (catch 'not-bound
           (mapcar
            (lambda(comp)
              (let ((ret (ergoemacs-map-p comp)))
                (when (and (not force) (not ret))
                  (throw 'not-bound nil))
                ret)) composed-list)))))



(defun ergoemacs-map-p (keymap &optional force)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map.

:map-key is the key of the current map.
:composed is a list of the `ergoemacs-map-p' of each of the composed maps.
:parent is the `ergoemacs-map-p' of the current map
"
  (let* ((keymap (ergoemacs-map-keymap-value keymap))
         (map-key (ergoemacs-map-get keymap :map-key))
         (composed (ergoemacs-map-composed keymap force))
         parent ret)
    (when (and force (not (or map-key composed)))
      (ergoemacs-map--label keymap)
      (setq map-key (ergoemacs-map-get keymap :map-key)
            composed (ergoemacs-map-composed keymap)
            parent (ergoemacs-map-parent keymap)))
    (when map-key
      (setq ret (plist-put ret :map-key map-key)))
    (when composed
      (setq ret (plist-put ret :composed composed)))
    (when (or map-key composed)
      (setq parent (ergoemacs-map-parent keymap t))
      (when parent
        (setq ret (plist-put ret :parent parent))))
    ret))

(defvar ergoemacs-mapkeymap--current nil)
(defvar ergoemacs-mapkeymap--submaps nil)
(defvar ergoemacs-mapkeymap--prefixes nil)
(defvar ergoemacs-mapkeymap--key nil)
(defvar ergoemacs-mapkeymap--nil nil)
(defvar ergoemacs-mapkeymap--debug nil)
(defvar ergoemacs-mapkeymap--maps nil)

(declare-function ergoemacs-debug "ergoemacs-mode")
(declare-function ergoemacs-debug-heading "ergoemacs-mode")
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
  (if (and submaps (not (eq submaps 'prefix)) (ergoemacs-map-p keymap))
      ;; (pushnew (cons key (ergoemacs-map-p keymap))
      ;; ergoemacs-mapkeymap--submaps :test 'equal)
      (unless (member (cons key (ergoemacs-map-p keymap)) (append ergoemacs-mapkeymap--submaps ergoemacs-mapkeymap--nil))
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Add submap %s" (cons key (ergoemacs-map-p keymap)) ))
        (push (cons key (ergoemacs-map-p keymap)) ergoemacs-mapkeymap--submaps))
    (ergoemacs-mapkeymap--loop
     function (ergoemacs-map-keymap-value keymap) submaps key)))

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
   
   ;; (key "String" "Help-String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (stringp (nth 1 item))
           (ergoemacs-keymapp (nthcdr 2 item))))
    ;; Install prefix as sparse/full keymap
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-all-sparse-p (nthcdr 2 item))
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
   
   ;; (key "String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (ergoemacs-keymapp (nthcdr 1 item))))
    ;; Install prefix as sparse/full keymap
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-all-sparse-p (nthcdr 1 item))
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
   
   ;; (key keymap)
   ((ergoemacs-keymapp item)
    ;; Prefix Key
    (unless (lookup-key ergoemacs-mapkeymap--current key)
      (if (ergoemacs-map-all-sparse-p item)
          (progn
            (when ergoemacs-mapkeymap--debug
              (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                               (key-description key)))
            (ergoemacs-mapkeymap--define-key
             key (make-sparse-keymap) prefix))
        (ergoemacs-mapkeymap--define-key
         key (make-keymap) prefix)
        (when ergoemacs-mapkeymap--debug
          (ergoemacs-debug "Setup prefix %s to be a full keymap"
                           (key-description key)))))
    (ergoemacs-mapkeymap--key-keymap key item function submaps prefix))

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
   ((string-match-p "^-+$" (format (car item)))
    ;; Separators...
    (ergoemacs-mapkeymap--define-key key item prefix)
    (when function
      (when ergoemacs-mapkeymap--debug
        (ergoemacs-debug "Call (%s %s %s %s)" function key item (or prefix t))
        (ergoemacs-debug "Setup prefix %s to be a sparse keymap"
                         (key-description key)))
      (funcall function key item (or prefix t)))
    )
   (t
    (warn "Could not extract\n\tkey:\"%s\" \n\tItem: \"%s\"" key item))))

(declare-function ergoemacs-real-define-key "ergoemacs-map.el" (keymap key def) t)
(fset 'ergoemacs-real-define-key (symbol-function 'define-key))

(declare-function ergoemacs-setcdr "ergoemacs-mode.el")

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
    (ergoemacs-real-define-key
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
         function (ergoemacs-map-keymap-value item) submaps prefix))
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

(defun ergoemacs-map-get (keymap property)
  "Gets ergoemacs-mode KEYMAP PROPERTY."
  (cond
   ((eq property :full)
    (ignore-errors (char-table-p (nth 1 (ergoemacs-map-keymap-value keymap)))))
   ((eq property :indirect)
    (ergoemacs-keymapp (symbol-function keymap)))
   ((eq property :map-key)
    ;; Expire any ids that are no longer linked
    (ignore-errors (plist-get (ergoemacs-map-plist keymap) :map-key)))
   ((eq property :map-list)
    (ergoemacs-map--map-list keymap))
   (t
    (ignore-errors
      (gethash property (gethash (ergoemacs-map-p (ergoemacs-map-keymap-value keymap)) ergoemacs-map-plist-hash))))))

(defun ergoemacs-mapkeymap (function keymap &optional submaps)
  "Call FUNCTION for all keys in hash table KEYMAP.
FUNCTION is called with three arguments KEY, VALUE and PREFIX.

When SUBMAPS is non-nil, will map over the whole keymap with the
exception of the submaps.  Once finished mapping over the main
map, map over each submap.  It will also assign properties to the
maps.

When SUBMAPS is 'prefix, only extract prefixes of the keymap,
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
                (if (ergoemacs-map-all-sparse-p keymap)
                    (make-sparse-keymap)
                  (make-keymap))
                ergoemacs-mapkeymap--submaps nil
                ergoemacs-mapkeymap--prefixes nil
                ergoemacs-mapkeymap--key (ergoemacs-map-p keymap) 
                ergoemacs-mapkeymap--nil '())
          (ergoemacs-mapkeymap--loop
           function (ergoemacs-map-keymap-value keymap) submaps)
          (when (and ergoemacs-mapkeymap--prefixes submaps)
            (ergoemacs-map-put keymap :prefixes ergoemacs-mapkeymap--prefixes))
          (setq sub ergoemacs-mapkeymap--submaps
                ret ergoemacs-mapkeymap--current
                ergoemacs-mapkeymap--current nil
                ergoemacs-mapkeymap--nil nil
                ergoemacs-mapkeymap--prefixes nil
                ergoemacs-mapkeymap--submaps nil
                ergoemacs-mapkeymap--key nil)
          (when (and sub submaps)
            (ergoemacs-map-put keymap :submaps sub)
            (dolist (item sub)
              (unless (member (cdr item) tmp1)
                ;; Now map keymap
                (ergoemacs-mapkeymap function (ergoemacs-map-keymap-value (cdr item)) t)
                ;; Make sure submap is assigned
                (setq tmp2 (ergoemacs-map-get (cdr item) :submap-p)
                      tmp3 (cons (car item) (ergoemacs-map--key keymap)))
                (unless (member tmp3 tmp2)
                  (push tmp3 tmp2))
                (ergoemacs-map-put (ergoemacs-map-keymap-value (cdr item)) :submap-p tmp2)
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

(defvar ergoemacs-global-before-ergoemacs (ergoemacs-mapkeymap nil global-map)
  "A single keymap for the keys before `ergoemacs-mode' loads.")

(defun ergoemacs-global-changed-p (key)
  "Determines if the global KEY has changed"
  (let* ((key (or (and (vectorp key) key)
                  (read-kbd-macro (key-description key) t)))
         (after-changed (ergoemacs-map-get global-map :keys-after-changed))
         keymap k1 k2 ret)
    (if (member key after-changed) t
      (setq keymap (or (and ergoemacs-ignore-prev-global ergoemacs-global-before-ergoemacs)
                       ergoemacs-global-map)
            k1 (lookup-key keymap key t)
            k2 (lookup-key global-map key t)
            ret (not (or (eq k1 k2) (and (keymapp k1) (keymapp k2)))))
      (when ret ;; Cache result
        (push key after-changed)
        (ergoemacs-map-put global-map :keys-after-changed after-changed))
      ret)))

(defvar ergoemacs-dir)
(defun ergoemacs-default-global--file ()
  "What is the global key hash file."
  (let* ((file (expand-file-name (format "ergoemacs-global-%s.el" emacs-version)
                                 ergoemacs-dir))
         (extras (expand-file-name "ergoemacs-extras" user-emacs-directory))
         (file2 (expand-file-name (format "ergoemacs-global-%s.el" emacs-version)
                                  extras)))
    (or
     (and (file-readable-p file2) file2)
     (and (file-readable-p file) file)
     (and (file-writable-p file) file)
     file2)))

(defvar ergoemacs-map--label-atoms-maps nil
  "Known bound keymaps")

(defun ergoemacs-default-global--gen ()
  "Generates hash for default emacs maps."
  ;; (setq ergoemacs-map-plist-hash (make-hash-table :test 'equal))
  (ergoemacs-map--label-atoms)
  ;; Pre-calculate map-lists 
  (dolist (map ergoemacs-map--label-atoms-maps)
    (ergoemacs-map-get (ergoemacs-sv map) :map-list))
  (with-temp-file (ergoemacs-default-global--file) 
    (let ((print-level nil)
          (print-length nil)
          keys)
      (goto-char (point-min))
      (insert "(defvar ergoemacs-map-plist-hash)(defvar ergoemacs-global-map)(declare-function ergoemacs-map--label \"ergoemacs-map\")")
      (ergoemacs-mapkeymap
       (lambda (key _item _prefix)
         (cond
          ((vectorp key)
           (push key keys))))
       global-map t)
      (setq ergoemacs-global-map (copy-keymap (ergoemacs-mapkeymap nil global-map)))
      (ergoemacs-map--label ergoemacs-global-map)
      (ergoemacs-map-put ergoemacs-global-map :keys keys)
      ;; (message "Keys: %s\nKey::%s" keys
      ;;          (ergoemacs-map-get ergoemacs-global-map :keys))

      (insert "(setq ergoemacs-map-plist-hash '")
      (prin1 ergoemacs-map-plist-hash (current-buffer))
      (goto-char (point-max))
      (insert ")")
      
      (insert "(setq ergoemacs-global-map '")
      (prin1 ergoemacs-global-map (current-buffer))
      ;; (insert ")(setq ergoemacs-minibuffer-local-map '")
      ;; (ergoemacs-mapkeymap nil minibuffer-local-map)
      ;; (prin1 (ergoemacs-mapkeymap nil minibuffer-local-map t) (current-buffer))

      ;; (insert ")(setq ergoemacs-minibuffer-local-ns-map '")
      ;; (ergoemacs-mapkeymap nil minibuffer-local-ns-map)
      ;; (prin1 (ergoemacs-mapkeymap nil minibuffer-local-ns-map t) (current-buffer))

      ;; (insert ")(setq ergoemacs-minibuffer-local-completion-map '")
      ;; (ergoemacs-mapkeymap nil minibuffer-local-completion-map)
      ;; (prin1 (ergoemacs-mapkeymap nil minibuffer-local-completion-map t) (current-buffer))

      ;; (insert ")(setq ergoemacs-minibuffer-local-must-match-map '")
      ;; (ergoemacs-mapkeymap nil minibuffer-local-must-match-map)
      ;; (prin1 (ergoemacs-mapkeymap nil minibuffer-local-must-match-map t) (current-buffer))

      ;; (insert ")(setq ergoemacs-minibuffer-local-filename-completion-map '")
      ;; (ergoemacs-mapkeymap nil minibuffer-local-filename-completion-map)
      ;; (prin1 (ergoemacs-mapkeymap nil minibuffer-local-filename-completion-map t) (current-buffer))

      (insert ")")

      (message "global-map-list %s" (ergoemacs-map-get global-map :map-list))
      (dolist (map ergoemacs-map--label-atoms-maps)
        (when (ergoemacs-map-p map)
          (insert (format "(when (boundp '%s) (ergoemacs-map--label %s %s))"
                          map map (ergoemacs-map-get (ergoemacs-sv map) :map-key))))))))

(declare-function ergoemacs-emacs-exe "ergoemacs-functions")

(defun ergoemacs-map-default-global ()
  "Loads/Creates the default global map information."
  (ergoemacs-map--label-atoms)
  (if (file-readable-p (ergoemacs-default-global--file))
      (load (ergoemacs-default-global--file))
    (if noninteractive
        (warn "Could not find global map information")
      (let* ((emacs-exe (ergoemacs-emacs-exe))
             (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
             (cmd (format "%s -L %s --batch --load \"ergoemacs-mode\" -Q --eval \"(ergoemacs-default-global--gen) (kill-emacs)\"" emacs-exe default-directory)))
        (message "%s" (shell-command-to-string cmd))
        (ergoemacs-map-default-global)))))

(defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.
"
  (if (not (ergoemacs-keymapp keymap)) nil
    ;; (ergoemacs-extract-keys keymap)
    (when (not (ergoemacs-map-p keymap))
      (ergoemacs-map--label keymap))
    (let ((ret (ergoemacs-map-get keymap :prefixes)) ret2)
      (unless ret
        (ergoemacs-mapkeymap nil keymap 'prefix)
        (setq ret (ergoemacs-map-get keymap :prefixes)))
      (if (and dont-ignore return-vector) ret
        (dolist (a ret)
          (let ((tmp (key-description a)))
            (when (or dont-ignore (not (member a ergoemacs-ignored-prefixes)))
              (if return-vector
                  (push a ret2)
                (push tmp ret2)))))
        ret2))))

(defvar ergoemacs-make-composed-keymap-p t)

(when (not (fboundp 'make-composed-keymap))
  (setq ergoemacs-make-composed-keymap-p nil)
  (defun make-composed-keymap (maps &optional parent)
    "Construct a new keymap composed of MAPS and inheriting from PARENT.

This does not work in emacs 23 or below, but ergoemacs-mode uses
it to create the same structure and flatten them later.

In emacs 24, this is how the function behaves:

When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
    `(keymap
      ,@(if (keymapp maps) (list maps) maps)
      ,@parent)))

(defvar ergoemacs-movement-functions)
(declare-function ergoemacs-translate "ergoemacs-translate")
(defun ergoemacs-flatten-composed-keymap--force-shifted (new-keymap)
  (dolist (move-fn (append ergoemacs-movement-functions
                           '(ergoemacs-shortcut-movement)))
    (dolist (move-key (where-is-internal move-fn new-keymap))
      (let ((trans (plist-get (ergoemacs-translate move-key) ':caps-translated-key)))
        (when (and trans (not (lookup-key new-keymap trans)))
          (define-key new-keymap trans 'ergoemacs-shortcut-movement-force-shift-select)))))
  new-keymap)

(declare-function ergoemacs-setcdr "ergoemacs-mode")

(defun ergoemacs-flatten-composed-keymap (keymap &optional force-shifted)
  "Flattens a composed KEYMAP."
  (if (not (ergoemacs-map-composed-p keymap))
      (if force-shifted
          (ergoemacs-flatten-composed-keymap--force-shifted keymap)
        keymap)
    ;; (let ((parent (keymap-parent keymap))
    ;;       new-keymap)
    ;;   (unwind-protect
    ;;       (progn
    ;;         (set-keymap-parent keymap nil)
    ;;         (setq new-keymap (ergoemacs-mapkeymap nil keymap))
    ;;         (set-keymap-parent keymap parent))
    ;;     (when force-shifted
    ;;       (ergoemacs-flatten-composed-keymap--force-shifted new-keymap))
    ;;     (ergoemacs-setcdr keymap (cdr new-keymap))
    ;;     new-keymap))
    (let ((ret (ergoemacs-mapkeymap nil keymap)))
      (when force-shifted
        (setq ret (ergoemacs-flatten-composed-keymap--force-shifted ret)))
      ret)))

(defvar ergoemacs-map--const-keymaps nil
  "Variable listing constant keymaps.")

(defvar ergoemacs-map--indirect-keymaps (make-hash-table)
  "Variable listing indirect keymaps.")

(defun ergoemacs-map-plist (keymap)
  "Determines if this is an `ergoemacs-mode' KEYMAP.
Returns a plist of fixed keymap properties (not changed by
composing or parent/child relationships)"
  (if (not (ergoemacs-keymapp keymap) ) nil
    (if (ignore-errors (symbol-function keymap))
        (progn (gethash keymap ergoemacs-map--indirect-keymaps))
      (let ((ret (or
                  (ignore-errors ;; (keymap #char-table "Label" (ergoemacs-map-marker) (ergoemacs-map-list))
                    (and (char-table-p (car (cdr keymap)))
                         (stringp (car (cdr (cdr keymap))))
                         (eq (car (car (cdr (cdr (cdr keymap))))) 'ergoemacs-labeled)
                         (funcall (cdr (car (cdr (cdr (cdr keymap))))))))
                  (ignore-errors ;; (keymap #char-table (ergoemacs-map-marker) (ergoemacs-map-list))
                    (and (char-table-p (car (cdr keymap))) 
                         (eq (car (car (cdr (cdr keymap)))) 'ergoemacs-labeled)
                         (funcall (cdr (car (cdr (cdr keymap)))))))
                  (ignore-errors ;; (keymap "label" (ergoemacs-map-marker) (ergoemacs-map-list))
                    (and (stringp (car (cdr keymap))) 
                         (eq (car (car (cdr (cdr keymap)))) 'ergoemacs-labeled)
                         (funcall (cdr (car (cdr (cdr keymap)))))))
                  (ignore-errors ;;(keymap  (ergoemacs-map-marker) (ergoemacs-map-list))
                    (and (eq (car (car (cdr keymap))) 'ergoemacs-labeled)
                         (funcall (cdr (car (cdr keymap))))))))
            (map keymap) parent)
        (unless ret
          (unwind-protect
              (progn
                (setq parent (keymap-parent map))
                (ignore-errors (set-keymap-parent map nil))
                (setq ret (lookup-key map [ergoemacs-labeled]))
                (when ret
                  (setq ret (ignore-errors (funcall ret)))))
            (ignore-errors (set-keymap-parent map parent))))
        (if ret ret
          ;; Now get properties for constant/indirect keymaps
          (catch 'found-map
            (dolist (map ergoemacs-map--const-keymaps)
              (when (eq (cdr map) (cdr keymap))
                (setq ret (car map))
                (throw 'found-map t))))
          ret)))))

(defun ergoemacs-map-put (keymap property value)
  "Set ergoemacs-mode KEYMAP PROPERTY to VALUE."
  (let ((keymap (ergoemacs-map-keymap-value keymap)))
    (cond
     ((not (ergoemacs-keymapp keymap))
      (warn "Trying to put keymap property on non-keymap %s." keymap))
     ((eq property :full)
      (warn "Cannot set the keymap property :full"))
     (t (let ((ret (ergoemacs-map-plist keymap)) tmp)
          (if (and ret (eq property ':map-key))
              (progn
                (setq ret (plist-put ret property value))
                (ergoemacs-map--label keymap value))
            (unless (hash-table-p ergoemacs-map-plist-hash)
              (setq ergoemacs-map-plist-hash (make-hash-table :test 'equal)))
            (setq tmp (gethash (ergoemacs-map-p keymap) ergoemacs-map-plist-hash))
            (unless (hash-table-p tmp)
              (setq tmp (make-hash-table)))
            (puthash property value tmp)
            (puthash (ergoemacs-map-p keymap) tmp ergoemacs-map-plist-hash)))))))

(defun ergoemacs-map-parent (keymap &optional force)
  "Returns a `ergoemacs-mode' map-key for the parent of KEYMAP."
  (let ((parent (keymap-parent keymap)))
    (and parent (ergoemacs-map-p parent force))))



;; (defun ergoemacs-map-boundp (keymap &optional force)
;;   "Returns if the maps linked to the current map are unbound, if it is an `ergoemacs-mode' map.
;; When FORCE is on, figure out if it is bound."
;;   (let ((ret ()(symbol-name (car (ergoemacs-map-get keymap :map-list)))))
;;     (if (not (string= ret "nil"))
;;         (string-match-p "^ergoemacs-unbound-" ret)
;;       (if (not force) nil
;;         (ergoemacs-map--label keymap)
;;         (ergoemacs-map-boundp keymap)))))


(defun ergoemacs-map--map-list (keymap &optional no-hash)
  "Get the list of maps bound to KEYMAP.
KEYMAP can be a keymap or keymap integer key."
  (let (ret
        (map-p (ergoemacs-map-p keymap))
        map-key)
    (cond
     ((and map-p (not no-hash) (setq ret (ergoemacs-map-get keymap :map-list-hash)))
      (setq map-key (ergoemacs-map--key keymap))
      (setq map-p ret)
      (setq ret nil)
      (dolist (map map-p)
        (when (or (eq keymap (ergoemacs-sv map))
                  (= map-key (ergoemacs-map--key (ergoemacs-sv map))))
          (push map ret)))
      (when (not ret);; Check again...
        (setq ret (ergoemacs-map--map-list keymap t)))
      ret)
     ((and map-p (setq map-key (ergoemacs-map--key keymap)) (integerp map-key))
      (setq map-p (ergoemacs-map--key keymap))
      (dolist (map ergoemacs-map--label-atoms-maps)
        (when (or (eq keymap (ergoemacs-sv map))
                  (equal map-p (ergoemacs-map--key map)))
          (push map ret)))
      (ergoemacs-map-put keymap :map-list-hash ret)
      ret)
     ((integerp keymap)
      (dolist (map ergoemacs-map--label-atoms-maps)
        (when (equal keymap (ergoemacs-map--key map))
          (unless map-p
            (setq map-p map))
          (push map ret)))
      (ergoemacs-map-put map-p :map-list-hash ret)
      ret)
     ((and (or map-key (consp keymap))
           (setq ret (or map-key (plist-get keymap :map-key)))
           (integerp ret))
      (ergoemacs-map--map-list ret))
     ((and ret (consp ret) (ignore-errors (setq ret (car (car ret)))))
      (ergoemacs-map--map-list ret)))))

(defun ergoemacs-map--label-map (map)
  "Label MAP"
  (let* (sv)
    (cond 
     ((get map :ergoemacs-labeled)
      t) ;; Already labeled
     ((not (setq sv (ergoemacs-sv map t)))
      nil) ;; Nil
     ((not (ergoemacs-keymapp sv)) ;; Not a keymap
      (put map :ergoemacs-labeled t)
      t)
     ((or (equal sv (make-sparse-keymap)) ;; Empty
          (equal sv (make-keymap)))
      nil)
     (t ;;Label
      (when sv
        (let (omap key)
          (setq key (ergoemacs-map--key sv))
          (ergoemacs-map--label sv key)
          (setq omap (gethash key ergoemacs-original-map-hash))
          ;; Hash should be a copy pointers of the original maps.
          (unless omap
            (puthash key (copy-keymap sv) ergoemacs-original-map-hash))))
      (pushnew map ergoemacs-map--label-atoms-maps)
      (put map :ergoemacs-labeled t)
      t))))

(defun ergoemacs-map--label-atoms (&rest _ignore)
  "Label all the bound keymaps."
  (mapatoms #'ergoemacs-map--label-map))

(defvar ergoemacs-map--unlabeled nil
  "A list of unlabeled keymaps.")

(defun ergoemacs-map--label-unlabeled (&rest _ignore)
  "Label known but unlabeled keymaps."
  (let (new)
    (dolist (map ergoemacs-map--unlabeled)
      (unless (ergoemacs-map--label-map map)
        (push map new)))
    (setq ergoemacs-map--unlabeled new)))



(defun ergoemacs-original-keymap--intern (keymap-label)
  (let ((map-key (plist-get keymap-label :map-key))
        (composed (plist-get keymap-label :composed))
        (parent (plist-get keymap-label :parent))
        tmp
        ret)
    (cond
     (composed
      (dolist (map-label composed)
        (setq tmp (ergoemacs-original-keymap--intern map-label))
        (when (ergoemacs-keymapp tmp) 
          (push tmp ret)))
      (setq tmp (and parent (ergoemacs-original-keymap--intern parent)))
      (setq ret (make-composed-keymap tmp (and (ergoemacs-keymapp tmp) tmp))))
     ((and (setq map-key (gethash map-key ergoemacs-original-map-hash))
           (ergoemacs-keymapp map-key))
      (setq ret (copy-keymap map-key))
      (when parent
        (setq parent (ergoemacs-original-keymap--intern parent))
        (when (ergoemacs-keymapp parent) 
          (set-keymap-parent ret parent)))))
    ret))

(defun ergoemacs-original-keymap (keymap &optional replace)
  "Return a copy of original keymap, or the current keymap."
  (if (not keymap) nil
    (if (symbolp keymap)
        (ergoemacs-original-keymap (ergoemacs-sv keymap) replace)    
      (let ((ret (ergoemacs-map-p keymap)))
        (when ret
          (setq ret (ergoemacs-original-keymap--intern ret))
          (when replace
            (ergoemacs-setcdr keymap (cdr ret))))
        (or ret keymap)))))


(defun ergoemacs-map--key (keymap)
  "Gets the key for the KEYMAP."
  (let ((ret (ergoemacs-map-plist (ergoemacs-map-keymap-value keymap))))
    (or (and ret (plist-get ret :map-key)) (random))))

(defun ergoemacs-map--name (keymap)
  "Gets the first symbol pointing to this KEYMAP (if any)"
  (or (ergoemacs-map-get keymap :map-list)))

(defun ergoemacs-map--label (keymap &optional map-key)
  "Label an `ergoemacs-mode' touched keymap.
MAP-NAME is the identifier of the map name.
The KEYMAP will have the structure
  (keymap optional-char-table \"Optional Label\" (ergoemacs-labeled (lambda nil (plist-of-properties))) true-map)
"
  (if (not (ergoemacs-keymapp keymap) ) nil
    (if (ergoemacs-map-composed-p keymap)
        (cond
         (map-key
          (warn "Will not label a composed map's members to %s" map-key))
         (t
          (dolist (map (ergoemacs-map-composed-list keymap))
            (ergoemacs-map--label map map-key))))
      (let* ((map keymap)
             (map-key (or map-key (ergoemacs-map--key map)))
             char-table
             indirect-p
             old-plist
             (parent (keymap-parent map))
             label tmp1 tmp2)
        (unwind-protect
            (progn
              (ignore-errors (set-keymap-parent map nil))
              (if (ergoemacs-keymapp (symbol-function keymap))
                  (setq indirect-p t ; Indirect keymap
                        old-plist (gethash keymap ergoemacs-map--indirect-keymaps))
                (setq old-plist (lookup-key map [ergoemacs-labeled]))
                (if (eq (car map) 'keymap)
                    (setq map (cdr map))
                  (setq map (list map)))
                (when (ignore-errors (char-table-p (car map)))
                  (setq char-table (pop map)))
                (when (stringp (car map))
                  (setq label (pop map)))
                ;; Drop prior `ergoemacs-mode' labels
                (setq tmp1 '()
                      tmp2 nil)
                (when old-plist
                  (setq old-plist (ignore-errors (funcall old-plist)))
                  (while (not (and (consp tmp2)
                                   (eq (car tmp2) 'ergoemacs-labeled)))
                    (setq tmp2 (pop map))
                    (unless (and (consp tmp2) (equal 'ergoemacs-labeled (car tmp2)))
                      (push tmp2 tmp1)))
                  (while tmp1
                    (push (pop tmp1) map))))
              (setq old-plist (list :map-key map-key))
              (unless indirect-p
                (push (cons 'ergoemacs-labeled
                            `(lambda() (interactive) ',old-plist)) map))
              (unless indirect-p
                (when label
                  (push label map))
                (when char-table
                  (push char-table map))
                (push 'keymap map)))
          (ignore-errors (set-keymap-parent map parent)))
        (if indirect-p
            (puthash keymap old-plist ergoemacs-map--indirect-keymaps)
          (unless (ignore-errors (ergoemacs-setcdr keymap (cdr map)))
            (pushnew (cons old-plist (cdr keymap)) ergoemacs-map--const-keymaps)))
        map))))


(defvar ergoemacs-command-shortcuts-hash)
(defun ergoemacs-map--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-p' of the keymap as well."
  (let (map-key ret)
    (cond
     ((and (ergoemacs-keymapp keymap) (setq map-key (ergoemacs-map--key keymap))
           (integerp map-key))
      (setq ret (gethash map-key ergoemacs-original-map-hash))
      (if (and (not ret) ;; Don't save empty keymaps as original keymaps...
               (or (equal keymap (make-sparse-keymap))
                   (equal keymap (make-keymap)))) keymap
        (unless ret
          (ergoemacs-map--label keymap map-key)
          (puthash map-key (copy-keymap (ergoemacs-map-keymap-value keymap)) ergoemacs-original-map-hash)
          (setq ret (gethash map-key ergoemacs-original-map-hash)))
        ret))
     ((and map-key (ignore-errors (setq map-key (car (car map-key)))))
      (ergoemacs-map--original map-key))
     ((and (consp keymap) (ignore-errors (setq map-key (plist-get keymap :map-key))) (integerp map-key))
      (setq ret (gethash map-key ergoemacs-original-map-hash))
      (setq map-key (plist-get keymap :parent))
      (when map-key
        (set-keymap-parent ret (ergoemacs-map--original map-key)))
      ret)
     ((and map-key (consp map-key) (ignore-errors (setq map-key (car (car map-key)))))
      (ergoemacs-map--original map-key))
     ((and (consp keymap) (ignore-errors (setq map-key (plist-get keymap :composed))))
      (setq ret (plist-get keymap :parent))
      (setq ret (make-composed-keymap
                 (mapcar
                  (lambda(map)
                    (ergoemacs-map--original map))
                  map-key)
                 (and ret (ergoemacs-map--original ret))))
      ret))))

(declare-function ergoemacs-theme--install-shortcut-item "ergoemacs-theme-engine")
(defun ergoemacs-map--install-ergoemacs (map &optional complete)
  "Returns a keymap with `ergoemacs-mode' modifications."
  (cond
   ((symbolp map)
    (ergoemacs-map--install-ergoemacs (ergoemacs-sv map) complete))
   ((ergoemacs-map-p map) map)
   (t
    (let* ((maps (ergoemacs-map--name map))
           (orig-map (copy-keymap (ergoemacs-map--original map)))
           (new-map (copy-keymap map))
           (parent (keymap-parent map)))
      (when parent
        (setq parent (ergoemacs-map--install-ergoemacs parent complete))
        (set-keymap-parent orig-map nil)
        (set-keymap-parent new-map nil))
      ;; Save original maps
      ;; Modify maps.
      (maphash
       (lambda (key args)
         (ergoemacs-theme--install-shortcut-item
          key args new-map orig-map complete))
       ergoemacs-command-shortcuts-hash)
      (setq new-map (cdr new-map))
      ;; Install `read-key' keys
      (dolist (key (ergoemacs-extract-prefixes new-map))
        (push (cons (elt (read-kbd-macro key t) 0)
                    'ergoemacs-read-key-default)
              new-map))
      ;; Install parent map
      (when parent
        (set-keymap-parent new-map parent))
      ;; Install in place
      (ergoemacs-setcdr (ergoemacs-sv map) (cdr new-map))
      (dolist (map-name maps)
        ;; (puthash map-name new-map ergoemacs-modified-map-hash)
        (ergoemacs-setcdr (ergoemacs-sv map-name) (cdr new-map)))
      ;; Return new map
      new-map))))

(defun ergoemacs-define-key--is-global-map (map key)
  "Determines if defining KEY the MAP will change the global-map.
This checks if the map is a submap of the global map.  If so, it
returns a list of keys affected.   Otherwise, it returns nil"
  (let ((submap-p (ergoemacs-submap-p map)) tmp (ret '()))
    (cond
     (submap-p
      (dolist (submap submap-p)
        (setq tmp
              (ergoemacs-define-key--is-global-map
               (ergoemacs-map-keymap-value (cdr submap))
               (or (and (vectorp key) (vconcat (car submap) key))
                   (car submap))))
        (when tmp
          (setq ret (append ret tmp)))))
     ((memq 'global-map (ergoemacs-map-get map :map-list))
      (setq ret (or (and key (list key)) t))))
    ret))

(defun ergoemacs-define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
KEYMAP is a keymap.

KEY is a string or a vector of symbols and characters, representing a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be represented by vectors.
Two types of vector have special meanings:
 [remap COMMAND] remaps any key binding for COMMAND.
 [t] creates a default definition, which applies to any event with no
    other definition in KEYMAP.

DEF is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)

If KEYMAP is a sparse keymap with a binding for KEY, the existing
binding is altered.  If there is no binding for KEY, the new pair
binding KEY to DEF is added at the front of KEYMAP.

When `ergoemacs-mode' has labeled the keymap, then define the key
in the original keymap, and in the override keymap.

"
  ;; (let ((map-plist (ergoemacs-map-p keymap))
  ;;       changes-after-map)
  ;;   (unless map-plist
  ;;     ;; ergoemacs-mode aware keymap.
  ;;     ;; First save changes as changes made after `ergoemacs-mode'
  ;;     (setq changes-after-map (or (ergoemacs-map-get keymap :changes-after-map) (make-sparse-keymap)))
  ;;     (ergoemacs-real-define-key changes-after-map key def)
  ;;     (ergoemacs-map-put keymap :changes-after-map changes-after-map)
  ;;     ;; (ergoemacs-real-define-key keymap key def)
  ;;     )
  ;;   (ergoemacs-real-define-key keymap key def))
  (ergoemacs-real-define-key keymap key def)
  (let ((key-vect (or (and (vectorp key) key)
                      (read-kbd-macro (key-description key) t))))
    (dolist (global-key (ergoemacs-define-key--is-global-map keymap key-vect))
      (ergoemacs-global-set-key-after global-key))))



(declare-function ergoemacs-theme-component--ignore-globally-defined-key "ergoemacs-theme-engine")

(defvar ergoemacs-ignore-advice)
(defun ergoemacs-global-set-key-after (key)
  (if ergoemacs-ignore-advice nil
    (let ((kd (key-description key)) tmp tmp1)
      (unless (or (and (vectorp key)
                       (ignore-errors (memq (elt key 0) '(menu-bar 27 remap))))
                  ;; FIXME: don't unbind for packages that use
                  ;; global-set-key.  Like undo-tree
                  (and (not (vectorp key))
                       (string= "ESC" kd)))
        ;; Let `ergoemacs-mode' know these keys have changed.
        (setq tmp (ergoemacs-map-get global-map :keys-after-changed))
        (setq tmp1 (read-kbd-macro kd t))
        (unless (member tmp1 tmp)
          (push tmp tmp))
        (ergoemacs-map-put global-map :keys-after-changed tmp)
        ;; (pushnew kd ergoemacs-global-changed-cache :test 'equal)
        ;; (setq ergoemacs-global-not-changed-cache (delete kd ergoemacs-global-not-changed-cache))
        ;; Remove the key from `ergoemacs-mode' bindings
        (ergoemacs-theme-component--ignore-globally-defined-key key t)))))


;;; ergoemacs-events
(defvar ergoemacs-event-hash (make-hash-table)
  "Event modifiers not covered by standard emacs")
(defvar ergoemacs-keyboard-layout)
(defun ergoemacs-curr-layout-symbol (&optional layout)
  "Gets the LAYOUT symbol.
If LAYOUT is unspecified, use `ergoemacs-keyboard-layout'."
  (intern (format "ergoemacs-layout-%s" (or layout ergoemacs-keyboard-layout))))

(defun ergoemacs-event-modifier-hash (&optional layout)
  "Gets the event modifier hash for LAYOUT."
  (let* ((layout-symbol (ergoemacs-curr-layout-symbol layout))
         (hash (gethash layout-symbol ergoemacs-event-hash)))
    (if hash hash
      ;; Not present setup modifier hash
      (setq hash (make-hash-table))
      (let ((lay (symbol-value layout-symbol))
            (i 0)
            r1 r2)
        (while (< i 60)
          (unless (or (string= "" (nth i lay))
                      (string= "" (nth (+ i 60) lay)))
            (setq r1 (aref (read-kbd-macro (nth i lay) t) 0)
                  r2 (aref (read-kbd-macro (nth (+ i 60) lay) t) 0))
            (unless (eq (event-basic-type r1) (event-basic-type r2))
              ;; This shifted expression isn't covered by basic emacs.
              (puthash r2 r1 hash)
              (puthash (intern (format "s%s" r1)) r2 hash)))
          (setq i (+ i 1)))
        (puthash layout-symbol hash ergoemacs-event-hash))
      hash)))

(defun ergoemacs-event-modifiers (event &optional layout)
  "Return a list of symbols representing the modifier keys in event EVENT.
This is different than `event-modifiers' in two ways:
- Symbol keys like # will return 'ergoemacs-shift for a QWERTY keyboard.
- Special keys like C-RET will return 'ergoemacs-control
"
  (let ((modifiers (event-modifiers event))
        basic)
    (unless (memq 'shift modifiers)
      ;; Add 'shift for # type events.
      (setq basic (event-basic-type event))
      (when (gethash basic (ergoemacs-event-modifier-hash layout))
        (push 'ergoemacs-shift modifiers)))
    ;; Also add 'ergoemacs-control to C-RET which translates to C-m
    (when (and (integerp event)
               (> event 1000)
               (memq 'control modifiers))
      (unless basic
        (setq basic (event-basic-type event)))
      (when (memq basic '(?m ?i ?\[))
        (push 'ergoemacs-control modifiers)))
    modifiers))

(defun ergoemacs-event-basic-type  (event &optional layout)
  "Return the basic type of the given event (all modifiers removed).
This is different than `event-basic-type' because ?# would return
?3 on a QWERTY LAYOUT."
  (let* ((basic (event-basic-type event))
         (new-basic (gethash basic (ergoemacs-event-modifier-hash layout))))
    (or new-basic basic)))

(defun ergoemacs-event-convert-list (list &optional layout)
  "Convert the event description list EVENT-DESC to an event type.

This is different than `event-convert-list' because:
 -  '(shift ?3) or '(ergoemacs-shift ?3) produces ?# on a QWERTY LAYOUT.
 -  '(ergoemacs-control control ?m) produces C-RET"
  (let ((cur-list list)
        elt
        tmp
        control-p
        new-list)
    (when (memq 'ergoemacs-control cur-list)
      (setq control-p t)
      (dolist (elt (reverse cur-list))
        (unless (equal elt 'ergoemacs-control)
          (push elt new-list)))
      (setq cur-list new-list)
      (setq new-list nil))
    
    (if (not (or (memq 'shift list) (memq 'ergoemacs-shift list)))
        (setq new-list cur-list)
      (while (> (length cur-list) 0)
        (setq elt (pop cur-list))
        (cond
         ((and cur-list (memq elt '(shift ergoemacs-shift))))
         
         ((and (not cur-list)
               (setq tmp (gethash (intern (format "s%s" elt))
                                  (ergoemacs-event-modifier-hash layout))))
          ;; Special case.
          (setq new-list (append new-list (list tmp))))
         ((not cur-list)
          (push 'shift new-list)
          (setq new-list (append new-list (list elt))))
         (t
          (push elt new-list)))))
    (if control-p
        (aref (read-kbd-macro (concat "C-" (key-description (vector (event-convert-list new-list)))) t) 0)
      (event-convert-list new-list))))

;;; Pretty key

(defvar ergoemacs-display-char-list nil
  "List of characters and fonts and if they display or not.")

(require 'descr-text)
(require 'faces)

(defvar ergoemacs-use-unicode-char t
  "Use unicode characters when available.")

(defun ergoemacs-display-char-p (char)
  "Determines if CHAR can be displayed."
  (ignore-errors
    (let* (ret
           (buf (current-buffer))
           (face (font-xlfd-name (face-attribute 'default :font)))
           (found (assoc (list face char window-system) ergoemacs-display-char-list)))
      (if found
          (nth 0 (cdr found))
        (switch-to-buffer (get-buffer-create " *ergoemacs-display-char-p*") t)
        (delete-region (point-min) (point-max))
        (insert char)
        (let ((display (describe-char-display (point-min) (char-after (point-min)))))
          (if (display-graphic-p (selected-frame))
              (if display
                  (setq ret t))
            (if display
                (setq ret t))))
        (switch-to-buffer buf)
        ;; Save it so the user doesn't see the buffer popup very much
        ;; (if at all).
        (push (list (list face char window-system) ret) ergoemacs-display-char-list)
        ret))))

(defun ergoemacs-unicode-char (char alt-char)
  "Uses CHAR if it can be displayed, otherwise use ALT-CHAR.
This assumes `ergoemacs-use-unicode-char' is non-nil.  When
`ergoemacs-use-unicode-char' is nil display ALT-CHAR"
  (if (and ergoemacs-use-unicode-char (ergoemacs-display-char-p char))
      char
    alt-char))

(defcustom ergoemacs-use-ergoemacs-key-descriptions t
  "Use ergoemacs key descriptions (Alt+) instead of emacs key descriptors (M-)"
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-unicode-brackets t
  "Use unicode brackets."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-use-small-symbols nil
  "Use small symbols to represent alt+ ctl+ etc. on windows/linux."
  :type 'boolean
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-capitalize-keys 'with-modifiers
  "Capitalize keys like Ctrl+C.
`ergoemacs-mode' should show Ctrl+Shift+C if you are pressing these keys."
  :type '(choice
          (const :tag "Don't Capitalize Keys" nil)
          (const :tag "Capitalize Keys with modifiers" with-modifiers)
          (const :tag "Capitalize Keys" t))
  :set #'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defvar ergoemacs-use-M-x-p nil)

(defvar ergoemacs-M-x)

(defface ergoemacs-pretty-key
  '((t :inverse-video t :box (:line-width 1 :style released-button) :weight bold))
  "Button Face for a `ergoemacs-mode' pretty key."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-pretty-key-use-face t
  "Use a button face for keys."
  :group 'ergoemacs-mode)

(defun ergoemacs-pretty-key-description--key (key mod)
  "Key description"
  (let ((ret ""))
    (cond
     ((eq key 'deletechar)
      (setq ret "Del"))
     ((memq key '(insert insertchar))
      (setq ret "Ins"))
     ((eq key 'home)
      (setq ret "Home"))
     ((eq key 'end)
      (setq ret "End"))
     ((eq key 32)
      (setq ret "Space"))
     ((eq key 127)
      (setq ret (format "%sBackspace" (ergoemacs-unicode-char "←" "left"))))
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret (format "Tab%s"
                        (ergoemacs-unicode-char "↹" ""))))
     ((eq key 'return)
      (setq ret (format "Enter%s"
                        (ergoemacs-unicode-char "↵" ""))))
     ((memq key '(apps menu))
      (setq ret (ergoemacs-unicode-char "▤" "Menu")))
     ((eq key 'left)
      (setq ret (ergoemacs-unicode-char "←" "left")))
     ((eq key 'right)
      (setq ret (ergoemacs-unicode-char "→" "right")))
     ((eq key 'up)
      (setq ret (ergoemacs-unicode-char "↑" "up")))
     ((eq key 'down)
      (setq ret (ergoemacs-unicode-char "↓" "down")))
     ((eq key 'prior)
      (setq ret "PgUp"))
     ((eq key 'next)
      (setq ret "PgDn"))
     ((integerp key)
      (setq ret (or (and (or (and (eq ergoemacs-capitalize-keys 'with-modifiers)
                                  mod)
                             (eq ergoemacs-capitalize-keys t))
                         (upcase (make-string 1 key)))
                    (make-string 1 key))))
     ((and (symbolp key) (string-match "^f[0-9]+$" (symbol-name key)))
      (setq ret (upcase (symbol-name key))))
     (t
      (setq ret (format "%s" key))))
    (when (and ergoemacs-pretty-key-use-face
               (not ergoemacs-use-small-symbols))
      (add-text-properties 0 (length ret)
                           '(face ergoemacs-pretty-key) ret))
    ret))

(defun ergoemacs-pretty-key-description--modifier (mod)
  "Modifier description"
  (let (ret)
    (cond
     ;; OSX specific key descriptions
     ((and (eq mod 'meta) ergoemacs-use-small-symbols
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%s"
                        (ergoemacs-unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%sCmd+"
                        (ergoemacs-unicode-char "⌘" "+"))))
     ((and (eq mod 'meta) 
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%sOpt+" (ergoemacs-unicode-char "⌥" "+"))))
     ((and (eq mod 'meta) ergoemacs-use-small-symbols
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%s" (ergoemacs-unicode-char "⌥" "+"))))
     ((and ergoemacs-use-small-symbols (eq mod 'shift))
      (setq ret (ergoemacs-unicode-char "⇧" "+")))
     ((and ergoemacs-use-small-symbols (eq mod 'meta))
      (setq ret (ergoemacs-unicode-char "♦" "!")))
     ((and (or (eq system-type 'darwin) ergoemacs-use-small-symbols)
	   (memq mod '(control ergoemacs-control)))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret (format "%sShift+"
                        (ergoemacs-unicode-char "⇧" ""))))
     ((memq mod '(control ergoemacs-control))
      (setq ret "Ctrl+"))
     ((eq mod 'meta)
      (setq ret "Alt+"))
     ((and (eq mod 'super) ergoemacs-use-small-symbols
           (eq system-type 'windows-nt))
      (setq ret (ergoemacs-unicode-char "⊞" "#")))
     ((and (eq mod 'super)
           (eq system-type 'windows-nt))
      (setq ret (format "%sWin+" (ergoemacs-unicode-char "⊞" "#"))))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret))))
    (when (and ergoemacs-pretty-key-use-face
               (not ergoemacs-use-small-symbols))
      (add-text-properties 0 (- (length ret) 1)
                           '(face ergoemacs-pretty-key) ret))
    ret))

(defun ergoemacs-pretty-key-description--ctl (mod)
  "Put in the correct modifiers for special keys"
  (let ((tmp '()))
    (dolist (m mod)
      (cond
       ((eq m 'ergoemacs-control)
        (push 'control tmp))
       ((eq m 'control))
       (t
        (push m tmp))))
    tmp))

(defun ergoemacs-pretty-key-description (kbd &optional layout)
  "Creates Pretty keyboard binding from kbd from M- to Alt+"
  (if (eq kbd (vector)) ""
    (let ((ret "")
          tmp
          mod ev)
      (dolist (key (listify-key-sequence kbd))
        (setq mod (ergoemacs-event-modifiers key)
              ev (ergoemacs-event-basic-type key))
        (cond
         ((and (memq 'control mod) (eq ev ?\[))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'escape))
         ((and (memq 'control mod) (eq ev ?m))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'return))
         ((and (memq 'control mod) (eq ev ?i))
          (setq mod (ergoemacs-pretty-key-description--ctl mod)
                ev 'tab))
         ((memq 'ergoemacs-shift mod)
          (setq tmp '())
          (dolist (m mod)
            (unless (eq m 'ergoemacs-shift)
              (push m tmp)))
          (setq mod tmp
                ev (gethash (intern (format "s%s" ev))
                            (ergoemacs-event-modifier-hash layout)))))
        (setq tmp (format "%s%s%s%s"
                          (or (and ergoemacs-pretty-key-use-face "")
                              (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "【" "["))
                              "[")
                          (mapconcat #'ergoemacs-pretty-key-description--modifier
                                     mod "")
                          (ergoemacs-pretty-key-description--key ev mod)
                          (or (and ergoemacs-pretty-key-use-face "")
                              (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "】" "]"))
                              "]")))
        (when (and ergoemacs-use-small-symbols ergoemacs-pretty-key-use-face)
          (add-text-properties 0 (length tmp)
                               '(face ergoemacs-pretty-key) tmp))
        (setq ret (format "%s%s%s" ret
                          (or (and ergoemacs-pretty-key-use-face " ")
                              (and ergoemacs-use-unicode-brackets "")) tmp)))
      (substring ret (or (and ergoemacs-pretty-key-use-face 1)
                         (and ergoemacs-use-unicode-brackets 0))))))


(defun ergoemacs-pretty-key (code)
  "Creates Pretty keyboard binding from kbd CODE from M- to Alt+"
  (if (not code) ""
    (save-match-data
      (if (string-match "^\\(M-x\\|<execute>\\) " code)
          (if ergoemacs-use-M-x-p
              code
            (replace-match ergoemacs-M-x t t code))
        (ergoemacs-pretty-key-description (read-kbd-macro code t))))))

;;; Translation between layouts
(defvar ergoemacs-translation-hash (make-hash-table))

(defun ergoemacs-get-translation-hash (&optional layout-to layout-from)
  "Gets the translation hash."
  (let* ((to (ergoemacs-curr-layout-symbol (or layout-to ergoemacs-keyboard-layout)))
         (from (ergoemacs-curr-layout-symbol (or layout-from "us")))
         (hash-f (gethash from ergoemacs-translation-hash (make-hash-table)))
         (hash-f-t (gethash to hash-f))
         (i 0)
         hash-t hash-t-f lay-t lay-f r-t r-f)
    (if hash-f-t hash-f-t
      (setq hash-f-t (make-hash-table)
            hash-t (gethash to ergoemacs-translation-hash (make-hash-table))
            hash-t-f (make-hash-table)
            lay-t (symbol-value to)
            lay-f (symbol-value from))
      (while (< i 120)
        (unless (or (string= "" (nth i lay-t))
                    (string= "" (nth i lay-f)))
          (setq r-t (aref (read-kbd-macro (nth i lay-t) t) 0)
                r-f (aref (read-kbd-macro (nth i lay-f) t) 0))
          (puthash r-t r-f hash-t-f)
          (puthash r-f r-t hash-f-t))
        (setq i (+ i 1)))
      (puthash from hash-t-f hash-t)
      (puthash to hash-f-t hash-f)
      (puthash to hash-t ergoemacs-translation-hash)
      (puthash from hash-f ergoemacs-translation-hash)
      hash-f-t)))

(defun ergoemacs-event-translate (event &optional layout-to layout-from basic modifiers)
  "Translate EVENT to the appropriate keyboard layout.
BASIC is the precalculated basic event from `ergoemacs-event-basic-type'
MODIFIERS is the precalculated modifiers from `ergoemacs-event-modifiers'
LAYOUT-TO is the layout to translate to, (default `ergoemacs-keyboard-layout')
LAYOUT-FROM is the layout to translate from, (defualt is \"us\" or QWERTY)"
  (let* ((basic (or basic (ergoemacs-event-basic-type event layout-from)))
         (modifiers (or modifiers (ergoemacs-event-modifiers event layout-from)))
         new-modifiers
         new-event
         (translation-hash (ergoemacs-get-translation-hash layout-to layout-from)))
    (cond
     ((and (eq system-type 'windows-nt) (eq basic 'menu))
      (setq basic 'apps))
     ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
      (setq basic 'menu)))
    (if (memq 'ergoemacs-control modifiers)
        (setq new-event basic
              new-modifiers modifiers)
      (if (or (memq 'shift modifiers)
              (memq 'ergoemacs-shift modifiers))
          (dolist (m modifiers)
            (if (not (memq m '(shift ergoemacs-shift)))
                (push m new-modifiers)
              (setq new-event (ergoemacs-event-convert-list (list m basic) layout-from))
              (setq new-event (or (gethash new-event translation-hash) new-event))))
        (setq new-event (or (gethash basic translation-hash) basic)
              new-modifiers modifiers)))
    (ergoemacs-event-convert-list (append new-modifiers (list new-event)) layout-to)))

(defun ergoemacs-kbd-translate (kbd &optional just-first-keys variable-modifiers variable-prefixes layout-to layout-from)
  "Translates between ergoemacs-mode keyboard layouts.
KBD is the key.

VARIABLE-MODIFIERS are the modifiers that cause translation
between keyboards to occur.

VARIABLE-PREFIXES are the list of prefix keys that are variable.

JUST-FIRST-KEYS is a list of keys where the keyboard translation
stops.  For example when JUST-FIRST-KEYS is [apps ?n] would
translate QWERTY [apps ?n ?n] to colemak [apps ?k ?n] instead of
 [apps ?k ?k]
"
  (let ((var-mod variable-modifiers)
        (var-pre variable-prefixes)
        (ret [])
        (untranslated [])
        (just-first-keys (or (and (vectorp just-first-keys) (list just-first-keys))
                             just-first-keys))
        translated-event
        just-first-p
        translate-prefix-p
        basic modifiers)
    (dolist (event (listify-key-sequence kbd))
      (setq basic (ergoemacs-event-basic-type event layout-from)
            modifiers (ergoemacs-event-modifiers event layout-from))
      (unless translate-prefix-p
        (setq translate-prefix-p (member untranslated var-pre)))
      (when (and just-first-keys (not just-first-p))
        (setq just-first-p (member untranslated just-first-keys)))
      (cond
       ((and (or (catch 'found-modifiers
                   (dolist (m modifiers)
                     (when (memq m var-mod)
                       (throw 'found-modifiers t)))
                   nil)
                 translate-prefix-p)
             (not just-first-p))
        (setq translated-event
              (ergoemacs-event-translate event layout-to layout-from basic modifiers)))
       ((and (eq system-type 'windows-nt) (eq basic 'menu))
        (setq translated-event (ergoemacs-event-convert-list (append modifiers '(apps)))))
       ((and (not (eq system-type 'windows-nt)) (eq basic 'apps))
        (setq translated-event (ergoemacs-event-convert-list (append modifiers '(menu)))))
       (t (setq translated-event event)))
      (setq untranslated (vconcat untranslated (list event))
            ret (vconcat ret (list translated-event))))
    ret))

(defstruct ergoemacs-struct-component-map
  "A basic ergoemacs component map structure."
  (name "default-name")
  (plist '())
  (map (make-sparse-keymap))
  (maps (make-hash-table))
  (cond-maps (make-hash-table))
  (hook-maps (make-hash-table))
  (when-condition nil)
  (hook nil)
  (dynamic-keys '())
  (version nil)
  (versions '())
  (undefined '())
  (unbind '())
  (variables nil)
  (just-first-keys nil :read-only t)
  (variable-modifiers '(meta) :read-only t)
  (variable-prefixes '([apps] [menu] [27]) :read-only t)
  (layout "us" :read-only t)
  (calculated-layouts (make-hash-table :test 'equal))
  (relative-to 'global-map))

(defvar ergoemacs-component-hash (make-hash-table :test 'equal)
  "Hash of ergoemacs-components")
(defvar ergoemacs-struct-define-key--current nil)

(defun ergoemacs-struct--create-component (plist body)
   "PLIST is the component properties
BODY is the body of function."
   (unwind-protect
      (progn
        (setq ergoemacs-struct-define-key--current
              (make-ergoemacs-struct-component-map
               :name (plist-get plist :name)
               :plist plist
               :just-first-keys (or (plist-get plist :just-first-keys) nil)
               :variable-modifiers (or (plist-get plist :variable-modifiers) '(meta))
               :variable-prefixes (or (plist-get plist :variable-prefixes) '([apps] [menu] [27]))
               :layout (or (plist-get plist :layout) "us")))
        (funcall body))
    (puthash (concat (ergoemacs-struct-component-map-name ergoemacs-struct-define-key--current)
                     (and (ergoemacs-struct-component-map-version ergoemacs-struct-define-key--current)
                          (concat "::" (ergoemacs-struct-component-map-version ergoemacs-struct-define-key--current))))
             ergoemacs-struct-define-key--current ergoemacs-component-hash)
    (setq ergoemacs-struct-define-key--current nil)))

(defun ergoemacs-struct-with-hook (when-condition plist body &optional object)
  "How the (when...) conditions in an ergoemacs-mode theme are handled."
  (cond
   ((and (not ergoemacs-struct-define-key--current) (not object)) ;; Old
    (ergoemacs-theme-component--with-hook when-condition plist body))
   (t
    (let ((obj (or object ergoemacs-struct-define-key--current))
          (hook
           (or (and (string-match-p "\\(-hook\\|-mode\\|\\`mark-active\\)\\'" (symbol-name when-condition)) when-condition)
               (and (string-match-p "mode-.*" (symbol-name when-condition))
                    (save-match-data
                      (intern-soft
                       (replace-regexp-in-string
                        "-mode-.*" "mode-hook"
                        (symbol-name when-condition)))))
               (and (string-match-p "(key)?map" (symbol-name when-condition))
                    (save-match-data
                      (intern-soft
                       (replace-regexp-in-string
                        "(key)?map.*" "hook"
                        (symbol-name when-condition)))))))
          new-obj tmp)
      (if (not (ergoemacs-struct-component-map-p obj))
          (error "OBJECT is not an ergoemacs-structure.")
        (setf (ergoemacs-struct-component-map-when-condition obj) when-condition)
        (setf (ergoemacs-struct-component-map-hook obj) hook)
        (funcall body)
        (setf (ergoemacs-struct-component-map-when-condition obj) nil)
        (setf (ergoemacs-struct-component-map-hook obj) nil))))))
   
(defun ergoemacs-struct-get-component-description (component)
  "Gets the description of a COMPONENT.
Allows the component not to be calculated."
  (let* ((comp-name (or (and (symbolp component) (symbol-name component))
                        component))
         (comp (gethash comp-name ergoemacs-component-hash)))
    (cond
     ((functionp comp)
      (replace-regexp-in-string "[\n ]*(fn)[ \n]*\\'" "" (documentation comp t)))
     ((ergoemacs-struct-component-map-p comp)
      (plist-get (ergoemacs-struct-component-map-plist comp) :description))
     (t ""))))

(defun ergoemacs-struct-new-version (version &optional object)
  "Add VERSION to component OBJECT."
  (cond
   ((and (not ergoemacs-struct-define-key--current) (not object)) ;; Old
    (ergoemacs-theme-component--version version))
   (t
    (let ((obj (or object ergoemacs-struct-define-key--current))
          new-obj tmp)
      (if (not (ergoemacs-struct-component-map-p obj))
          (error "OBJECT is not an ergoemacs-structure.")
        (puthash (concat (ergoemacs-struct-component-map-name obj)
                         (and (ergoemacs-struct-component-map-version obj)
                              (concat "::" (ergoemacs-struct-component-map-version obj))))
                 ergoemacs-struct-define-key--current ergoemacs-component-hash)
        ;; Get the base object without version changes
        (setq new-obj (gethash (ergoemacs-struct-component-map-name obj) ergoemacs-component-hash))
        ;; Update all versions to include the new version information.
        (dolist (old-version (ergoemacs-struct-component-map-versions new-obj))
          (setq tmp (gethash (concat (ergoemacs-struct-component-map-name new-obj) "::" old-version) ergoemacs-component-hash))
          (when (ergoemacs-struct-component-map-p tmp)
            (push version (ergoemacs-struct-component-map-versions tmp))))
        (push version (ergoemacs-struct-component-map-versions new-obj))
        ;; Use the last object as the base of the new object
        (setq ergoemacs-struct-define-key--current (copy-ergoemacs-struct-component-map obj))
        (setf (ergoemacs-struct-component-map-version ergoemacs-struct-define-key--current) version))))))

(declare-function 'ergoemacs-theme-define-key "ergoemacs-theme-engine")
(defvar ergoemacs-translations)
(defvar ergoemacs-struct-define-key-- nil)
(defun ergoemacs-struct-define-key--get-def (def)
  "Gets the `ergoemacs-mode' function definition for DEF."
  (cond
   ((and (consp def)
         (= 2 (length def))
         (stringp (nth 0 def))
         (or (not (nth 1 def))
             (gethash (nth 1 def) ergoemacs-translations)))
    `(lambda(&optional arg)
      (interactive "P")
      (ergoemacs-read-key ,(nth 0 def) ',(nth 1 def))))
   ((ergoemacs-keymapp (ergoemacs-sv def))
    (ergoemacs-sv def))
   (t def)))

(defun ergoemacs-struct-refresh-keys (&optional obj)
  "Refreshes the keys in OBJ based on any new interactive functions found."
  (let ((obj (or obj (ergoemacs-theme-components))))
    (if (consp obj)
        (dolist (cur-obj (ergoemacs-get-map--lookup-hash obj))
          (ergoemacs-struct-refresh-keys obj))
      (let* ((obj (ergoemacs-get-map--lookup-hash obj))
             (cur-dynamic (ergoemacs-struct-component-map-dynamic-keys obj))
             new-dynamic keymap key global-map-p cur-map
             fn-lst new-fn-lst new-fn lookup-key cur-layout)
        (dolist (cur-lst cur-dynamic)
          (setq keymap (nth 0 cur-lst)
                key (nth 1 cur-lst)
                fn-lst (nth 2 cur-lst)
                global-map-p (eq keymap 'global-map)
                cur-map (or (and global-map-p (ergoemacs-struct-component-map-map obj))
                            (gethash keymap (ergoemacs-struct-component-map-maps obj)))
                new-fn-lst '())
          (if (catch 'found-fn
                (dolist (fn fn-lst)
                  (if (not (commandp fn t))
                      (push new-fn-lst fn)
                    (setq new-fn fn)
                    (throw 'found-fn nil)))
                t) (push cur-lst new-dynamic)
            (when new-fn-lst ;; For later checks
              (push (list keymap key new-fn-lst) new-dynamic))
            (define-key cur-map key new-fn)
            ;; Now fix cached layouts
            (maphash
             (lambda(key value)
               (setq lookup-key (nth 0 key)
                     cur-layout (nth 1 key))
               (when (or (global-map-p (not lookup-key))
                         (eq looup-key keymap))
                 ;; Update keymap (in place).
                 (define-key value
                   (ergoemacs-kbd-translate
                    key (ergoemacs-struct-component-map-just-first-keys obj)
                    (ergoemacs-struct-component-map-variable-modifiers obj)
                    (ergoemacs-struct-component-map-variable-prefixes obj) cur-layout
                    (ergoemacs-struct-component-map-layout obj)) new-fn)))
             (ergoemacs-struct-component-map-calculated-layouts obj))))
        ;; Update dynamic/deferred keys
        (fset (ergoemacs-struct-component-map-dynamic-keys obj) new-dynamic)))))

(defun ergoemacs-struct-define-key (keymap key def &optional object)
  "Defines KEY to be DEF in KEYMAP for OBJECT.
If not specified, OBJECT is `ergoemacs-struct-define-key--current'."
  (cond
   ((and (not ergoemacs-struct-define-key--current) (not object)) ;; Old
    (ergoemacs-theme-define-key keymap key def))
   (t
    (let ((obj (or object ergoemacs-struct-define-key--current))
          (def (ergoemacs-struct-define-key--get-def def)))
      (if (not (ergoemacs-struct-component-map-p obj))
          (error "OBJECT not a ergoemacs-structure.")
        (let* ((global-map-p (or (eq keymap 'global-map) (eq keymap 'ergoemacs-mode-map)))
               (when-condition (ergoemacs-struct-component-map-when-condition obj))
               (hook (ergoemacs-struct-component-map-hook obj))
               (cur-map (or (and global-map-p (not when-condition) (ergoemacs-struct-component-map-map obj))
                            (and (not when-condition) (gethash keymap (ergoemacs-struct-component-map-maps obj)))
                            (and global-map-p when-condition (gethash when-condition (ergoemacs-struct-component-map-cond-maps obj)))
                            (and when-condition hook (ignore-errors (gethash keymap (gethash hook (ergoemacs-struct-component-map-hook-maps obj)))))))
               fn-lst
               (key (or (and (vectorp key) key)
                        (and (stringp key) (vconcat key)))))
          (cond
           ((and (not cur-map) (not when-condition))
            (pushnew keymap ergoemacs-map--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (ergoemacs-struct-component-map-maps obj)))
           ((and (not cur-map) when-condition global-map-p)
            (setq cur-map (make-sparse-keymap))
            (puthash when-condition cur-map (ergoemacs-struct-component-map-cond-maps obj)))
           ((and (not cur-map) when-condition hook)
            (unless (gethash hook (ergoemacs-struct-component-map-hook-maps obj))
              (puthash hook (make-hash-table) (ergoemacs-struct-component-map-hook-maps obj)))
            (pushnew keymap ergoemacs-map--unlabeled)
            (setq cur-map (make-sparse-keymap))
            (puthash keymap cur-map (gethash hook (ergoemacs-struct-component-map-hook-maps obj)))))
          (cond
           ((and global-map-p (not when-condition) (not def) (lookup-key (ergoemacs-struct-component-map-map obj) key))
            ;; Remove the key from the keymap, do not set it to
            ;; nil; Its as if it was never defined
            (setq ergoemacs-struct-define-key-- (make-sparse-keymap))
            (ergoemacs-mapkeymap
             (lambda (cur-key item prefix)
               (unless (or (eq prefix t) (eq item 'ergoemacs-prefix))
                 (unless (equal key cur-key)
                   (define-key ergoemacs-struct-define-key-- cur-key item))))
             cur-map)
            (setf (ergoemacs-struct-component-map-map obj)
                  (copy-keymap ergoemacs-struct-define-key--))
            (setq ergoemacs-struct-define-key-- nil))
           ((and global-map-p (not when-condition) (not def)) ;; Add to undefined keys
            (unless (member key (ergoemacs-struct-component-map-undefined obj))
              (push key (ergoemacs-struct-component-map-undefined obj))))
           ((and (not when-condition) (lookup-key cur-map key) (not def))
            ;; Remove the key from the keymap.  Do not set it to nil.
            ;; Its as if it was never defined.
            (setq ergoemacs-struct-define-key-- (make-sparse-keymap))
            (ergoemacs-mapkeymap
             (lambda (cur-key item prefix)
               (unless (or (eq prefix t) (eq item 'ergoemacs-prefix))
                 (unless (equal key cur-key)
                   (define-key ergoemacs-struct-define-key-- cur-key item))))
             cur-map)
            ;; Puthash is in place...?
            (puthash keymap (copy-keymap ergoemacs-struct-define-key--) (ergoemacs-struct-component-map-maps obj))
            (setq ergoemacs-struct-define-key-- nil))
           ((and (consp def) (symbolp (nth 1 def))) ;; (fn1 fn2 fn3 fn4)
            (unless (catch 'found-fn
                      (dolist (cur-def def)
                        (if (not (commandp cur-def t))
                            (push cur-def fn-lst)
                          (define-key cur-map key cur-def)
                          (throw 'found-fn t)))
                      nil)
              ;; Not found
              (define-key cur-map key `(lambda() (interactive) (error ,(format "This key is undefined without one of the following functions: %s") fn-lst))))
            (when fn-lst ;; Test for later
              (push (list keymap key fn-lst)
                    (ergoemacs-struct-component-map-dynamic-keys obj))))
           (t
            (define-key cur-map key def)))))))))

(defvar ergoemacs-struct-hash (make-hash-table)
  "Hash table of `ergoemacs-mode' component structures.")

(defvar ergoemacs-get-map--keymap nil)
(defvar ergoemacs-get-map--keymap-extra nil)
(defun ergoemacs-get-map-- (map cur-layout &optional lookup-keymap lookup-key unbind-keys translate-map)
  "Get component MAP and return KEYMAP updating MAP cache.
Optionally, lookup any translations in LOOKUP-KEYMAP, and cache using LOOKUP-KEY. "
  (let* (ret
         ;; (map-list (and lookup-keymap (ergoemacs-map-get lookup-keymap :map-list)))
         (relative-map-name (and lookup-keymap (ergoemacs-struct-component-map-relative-to map)))
         ;; (relative-map-p (and lookup-keymap (not (member relative-map-name map-list))))
         (relative-map (and lookup-keymap
                            (if (eq relative-map-name 'global-map)
                                ergoemacs-global-map
                              (ergoemacs-map--original (symbol-value relative-map-name)))))
         (cmap (or translate-map (ergoemacs-struct-component-map-map map)))
         (just-first-keys (ergoemacs-struct-component-map-just-first-keys map))
         (variable-modifiers (ergoemacs-struct-component-map-variable-modifiers map))
         (variable-prefixes (ergoemacs-struct-component-map-variable-prefixes map))
         (layout-from (ergoemacs-struct-component-map-layout map))
         (hash (ergoemacs-struct-component-map-calculated-layouts map))
         (extra-hash (ergoemacs-struct-component-map-maps map))
         extra-map)
    (setq ergoemacs-get-map--keymap (make-sparse-keymap))
    (ergoemacs-mapkeymap
     (lambda (key item prefix)
       (unless (or (eq prefix t) (eq item 'ergoemacs-prefix))
         (let ((new-key (ergoemacs-kbd-translate
                 key just-first-keys variable-modifiers variable-prefixes cur-layout layout-from))
               (other-command-keys (and relative-map (where-is-internal item relative-map)))
               new-command)
           (when (or (not unbind-keys) ;; Don't add key that is a
                     ;; member of unbind-keys
                     (not (member new-key unbind-keys)))
             (when (or (and (not relative-map) ;; global map
                            (setq new-command item))
                       (and relative-map ;; Relative map w/lookup.
                        (catch 'found-new ;; Define lookup-key's
                          ;; equivalent key
                          (dolist (other-key other-command-keys)
                            (setq new-command (lookup-key lookup-keymap other-key))
                            (when new-command
                              (throw 'found-new t))) nil)))
               (define-key ergoemacs-get-map--keymap new-key new-command))))))
     cmap)
    (if (not (and lookup-keymap
                  (catch 'found-extra
                    ;; If there are exceptions, install them before
                    ;; any lookups.
                    (dolist (map-name (ergoemacs-map-get lookup-keymap :map-list))
                      (setq extra-map (gethash map-name extra-hash))
                      (when extra-map
                        (throw 'found-extra t))) nil)))
        (setq ret (copy-keymap ergoemacs-get-map--keymap))
      (when unbind-keys
        (setq ergoemacs-get-map--keymap-extra (make-sparse-keymap))
        (ergoemacs-mapkeymap
         (lambda (key item prefix)
           (unless (or (eq prefix t) (eq item 'ergoemacs-prefix))
             (when (or (not unbind-keys) ;; Don't add key that is a
                       ;; member of unbind-keys
                       (not (member key unbind-keys)))
               (define-key ergoemacs-get-map--keymap-extra key item))))
         extra-map)
        (setq extra-map ergoemacs-get-map--keymap-extra))
      (setq ret (ergoemacs-mapkeymap nil (make-composed-keymap (list extra-map ergoemacs-get-map--keymap)))))
    (puthash (list lookup-key cur-layout) ret hash)
    (setq ergoemacs-get-map--keymap nil)
    ret))

(defun ergoemacs-struct-component-map-clear-cache (struct-map)
  "Clears STRUCT-MAP's cache of keymaps.

STRUCT-MAP can be a list of `ergoemacs-struct-component-map' structures as well."
  (cond
   ((ergoemacs-struct-component-map-p struct-map)
    (setf (ergoemacs-struct-component-map-calculated-layouts struct-map) (make-hash-table :test 'equal)))
   ((consp struct-map)
    (dolist (cur-map struct-map)
      (ergoemacs-struct-component-map-clear-cache cur-map)))))

(defun ergoemacs-closest-version (version version-list)
  "Return the closest version to VERSION in VERSION-LIST.
Formatted for use with `ergoemacs-theme-component-hash' it will return ::version or an empty string"
  (if (or (not version) (string= "nil" version)) ""
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
          (dolist (v version-list)
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
          (if (version-list-< biggest-version-list use-version)
              (setq ret "")
            (if best-version
                (setq ret (concat "::" best-version))
              (setq ret (concat "::" smallest-version))))
          ret)
      "")))

(defvar ergoemacs-theme-version)
(defun ergoemacs-get-map--lookup-closest (comp)
  "Looks up closest component version from `ergoemacs-component-hash'"
  (if (not (ergoemacs-struct-component-map-p comp)) nil
    (let (versions)
      (cond
       ((not (setq versions (ergoemacs-struct-component-map-versions comp)))
        comp)
       ((string= "" (setq versions (ergoemacs-closest-version ergoemacs-theme-version versions)))
        comp)
       (t
        (ergoemacs-get-map--lookup-hash (concat (ergoemacs-struct-component-map-name comp) versions)))))))

(defun ergoemacs-get-map--lookup-hash (map-or-map-list)
  "Lookup `ergoemacs-component-hash' from MAP-OR-MAP-LIST if necessary.

This takes into consideration any versions defined, and the
closest `ergoemacs-theme-version' calculated from
`ergoemacs-closest-version' by using `ergoemacs-get-map--lookup-closest'"
  (if (consp map-or-map-list)
      (mapcar #'ergoemacs-get-map--lookup-hash map-or-map-list)
    (if (ergoemacs-struct-component-map-p map-or-map-list)
        (ergoemacs-get-map--lookup-closest map-or-map-list)
      (let ((map map-or-map-list)
            ret)
        (when (symbolp map) ;; If map is a symbol, change to string.
          (setq map (symbol-name map)))
        (when (stringp map) ;; If map is a string, get the component from `ergoemacs-component-hash'
          (setq ret (gethash map ergoemacs-component-hash))
          (when (and ret (functionp ret))
            (funcall ret)
            (setq ret (gethash map ergoemacs-component-hash))))
        (ergoemacs-get-map--lookup-closest ret)))))

(defun ergoemacs-get-map--setcdr (map lookup-keymap setcdr-p)
  "Sets the keymap MAP in place when SETCDR-P is non-nil..."
  (cond
   ((not setcdr-p)
    map)
   ((not (ergoemacs-keymapp map))
    map)
   ((ergoemacs-keymapp lookup-keymap)
    (ergoemacs-setcdr lookup-keymap (cdr map))
    map)
   (t map)))

(defun ergoemacs-get-map--minor-mode-map-alist-hash (&optional obj layout)
  "Get `minor-mode-map-alist' additions in hash-table form."
  (let ((obj (ergoemacs-get-map--lookup-hash (or obj (ergoemacs-theme-components))))
        (cur-layout (or layout ergoemacs-keyboard-layout))
        (hash (make-hash-table)))
    (cond
     ((consp obj)
      (dolist (cur-obj obj)
        (maphash
         (lambda(key value)
           (puthash key (append (gethash key hash) value) hash))
         (ergoemacs-get-map--minor-mode-map-alist-hash cur-obj)))
      hash)
     (t
      (maphash
       (lambda(key value)
         ;; Put the translated keymap in a list in the hash.
         (puthash key (list (ergoemacs-get-map-- obj cur-layout nil (list 'cond-map key) nil value)) hash))
       (ergoemacs-struct-component-map-cond-maps obj))
      hash))))

(defun ergoemacs-get-map--minor-mode-map-alist (&optional obj)
  (let (ret map)
    (maphash
     (lambda(key value)
       (setq map (ergoemacs-mapkeymap nil (make-composed-keymap value)))
       (ergoemacs-map--label map (list 'cond-map key ergoemacs-keyboard-layout))
       (push (cons key map) ret))
     (ergoemacs-get-map--minor-mode-map-alist-hash))
    ret))

(defvar ergoemacs-get-map-hash (make-hash-table :test 'equal))
(defun ergoemacs-get-map (&optional lookup-keymap setcdr-p unbind-keys layout map recursive)
  "Get map looking up changed keys in LOOKUP-MAP based on LAYOUT.

MAP can be a `ergoemacs-struct-component-map', or a string/symbol
of a calculated or uncalcuated component in
`ergoemacs-component-hash'

MAP can also be a list of `ergoemacs-struct-component-map' values
or string/symbols that are in `ergoemacs-component-hash'

If missing, MAP represents the current theme compenents, from `ergoemacs-theme-components'

SETCDR-P tells ergoemacs-mode to swap out the keymaps.

LAYOUT represents the layout that is used.

RECURSIVE is an internal argument to make sure that infinite
loops do not occur.
"
  (let ((cur-layout (or layout ergoemacs-keyboard-layout))
        (map (ergoemacs-get-map--lookup-hash (or map (ergoemacs-theme-components))))
        lookup-key
        (lookup-keymap (or (and lookup-keymap (not recursive) (ergoemacs-keymapp lookup-keymap)
                                (ergoemacs-map--original lookup-keymap)) lookup-keymap))
        unbind-list
        parent
        composed-list
        ret)
    (ergoemacs-get-map--setcdr
     (cond
      ((and overriding-terminal-local-map
            (eq overriding-terminal-local-map lookup-keymap))
       ;;
       )
      ((and overriding-local-map
            (eq overriding-local-map lookup-keymap))
       ;;
       )
      ((eq emulation-mode-map-alists lookup-keymap)
       ;; Modify the emulation-mode-map-alists.
       )
      ((eq minor-mode-overriding-map-alist lookup-keymap)
       ;; Modify the `minor-mode-overriding-map-alist'
       )
      ((eq minor-mode-map-alist lookup-keymap)
       ;; Modify the `minor-mode-map-alist' and append conditional
       ;; maps.
       )
      ((eq (current-local-map) lookup-keymap)
       ;; Modify local map.
       )
      ((eq (current-global-map) lookup-keymap)
       ;; Modify global map
       )
      ((ergoemacs-struct-component-map-p map)
       (cond
        ((and (not lookup-keymap)
              (string= cur-layout (ergoemacs-struct-component-map-layout map)))
         (ergoemacs-struct-component-map-map map))
        ((and (not lookup-keymap)
              (setq ret (gethash
                         (list nil cur-layout unbind-keys)
                         (ergoemacs-struct-component-map-calculated-layouts map))))
         ret)
        ((setq ret (gethash
                    (list (and lookup-keymap
                               (setq lookup-key (ergoemacs-map-p lookup-keymap))) cur-layout unbind-keys)
                    (ergoemacs-struct-component-map-calculated-layouts map)))
         ret)
        ((not lookup-keymap)
         ;; Overall layout hasn't been calculated.
         (ergoemacs-get-map-- map cur-layout nil nil unbind-keys))
        ((ergoemacs-keymapp lookup-keymap)
         ;; Layout for lookup keymap hasn't been calculated
         (ergoemacs-get-map-- map cur-layout lookup-keymap lookup-key unbind-keys))
        (t
         (error "Cant calculate/lookup keymap."))))
      ((and (consp map) ;; Don't do anything with blank keymaps.
            lookup-keymap
            (or (equal lookup-keymap (make-sparse-keymap))
                (equal lookup-keymap (make-keymap))))
       lookup-keymap)
      ((and (consp map)
            (setq ret (gethash (list cur-layout map) ergoemacs-get-map-hash)))
       ret)
      ((and (consp map)
            (catch 'all-struct
              (dolist (cur-map map)
                (if (ergoemacs-struct-component-map-p cur-map)
                    (setq unbind-list (append unbind-list (ergoemacs-struct-component-map-unbind cur-map)))
                  (throw 'all-struct nil)))
              t)
            (progn ;; Check for composed keymaps or keymap parents
              (if (not lookup-keymap) t
                (setq parent (keymap-parent lookup-keymap))
                (setq composed-list (and (ergoemacs-map-composed-p lookup-keymap)
                                         (ergoemacs-map-composed-list lookup-keymap)))
                (and (not parent) (not composed-list))))
            (setq ret (make-composed-keymap
                       (append
                        (mapcar
                         (lambda(cur-map)
                           (ergoemacs-get-map lookup-keymap setcdr-p unbind-list layout cur-map))
                         map)
                        (and (not lookup-keymap)
                             (list
                              (let ((undefined-map (make-sparse-keymap)))
                                (dolist (cur-map map)
                                  (dolist (undefined-key (ergoemacs-struct-component-map-undefined cur-map))
                                    (unless (member undefined-key ret)
                                      (define-key undefined-map undefined-key 'ergoemacs-undefined))))
                                undefined-map))))
                       (or (and lookup-keymap (not recursive) (ergoemacs-map--original lookup-keymap))
                           (and (not lookup-keymap) (ergoemacs-map--original global-map))))))
       ;; Decompose (rot) the keymap (so you can label the map)
       (setq ret (ergoemacs-mapkeymap nil ret))
       (ergoemacs-map--label
        ret
        (append (list (list (ergoemacs-map-p lookup-keymap) cur-layout unbind-list))
                (mapcar
                 (lambda(cur-map)
                   (intern (ergoemacs-struct-component-map-name cur-map)))
                 map)))
       (puthash (list cur-layout map) ret ergoemacs-get-map-hash)
       ret)
      ((and (not composed-list) parent)
       (unwind-protect
           (progn
             (set-keymap-parent lookup-keymap nil)
             (setq ret (ergoemacs-get-map lookup-keymap setcdr-p unbind-keys layout map t)))
         (set-keymap-parent lookup-keymap parent))
       (set-keymap-parent ret (ergoemacs-get-map parent setcdr-p unbind-keys layout map t))
       ret)
      (composed-list
       (make-composed-keymap
        (mapcar
         (lambda(x)
           (ergoemacs-get-map x setcdr-p unbind-keys layout map t))
         composed-list)
        (ergoemacs-get-map parent setcdr-p unbind-keys layout map t)))
      (t
       (error "Component map isn't a proper argument")))
     lookup-keymap setcdr-p)))


;;; Change variable values.

(defun ergoemacs-struct-set (symbol newval &optional hook object)
  "Set variables up for components."
  (cond
   ((and (not ergoemacs-struct-define-key--current) (not object)) ;; Old
    (ergoemacs-theme--set symbol newval hook))
   (t
    (let ((obj (or object ergoemacs-struct-define-key--current))
          new-obj tmp)
      (if (not (ergoemacs-struct-component-map-p obj))
          (error "OBJECT is not an ergoemacs-structure.")
        (push (list symbol newval hook) (ergoemacs-struct-component-map-variables obj)))))))

(defun ergoemacs-struct-variables (&optional obj)
  "Get a list of variables for the OBJ."
  (let ((obj (or obj (ergoemacs-theme-components))))
    (cond
     ((consp obj)
      (let (ret)
        (dolist (cur-obj (ergoemacs-get-map--lookup-hash obj))
          (setq ret (append ret (ergoemacs-struct-variables cur-obj))))
        ret))
     (t (ergoemacs-struct-component-map-variables (ergoemacs-get-map--lookup-hash obj))))))

;;;###autoload
(defun ergoemacs-set (variable value &optional force)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value))) ; Otherwise negative integers are the same as nil
         (last-value (ignore-errors (ergoemacs-sv variable))))
    (when (and minor-mode-p (not last-value))
      (setq last-value -1))
    (cond
     ((and minor-mode-p (not (boundp variable)))
      (message "Malformed Minor Mode: %s" variable)
      (unless (get variable 'ergoemacs-save-value)
        (put variable 'ergoemacs-save-value (if new-value nil 1)))
      (funcall variable new-value))
     ((not (equal last-value value))
      (cond
       ((and (custom-variable-p variable) (or force (not (get variable 'save-value))))
        ;; (message "Changed customizable %s" variable)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (customize-set-variable variable new-value)
        (customize-mark-to-save variable)
        (when (and minor-mode-p (not new-value))
          (funcall variable -1)))
       ((or force (equal (ergoemacs-sv variable) (default-value variable)))
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        ;; (message "Changed variable %s" variable)
        (set variable new-value)
        (set-default variable new-value)
        (unless (get variable 'ergoemacs-save-value)
          (put variable 'ergoemacs-save-value (ergoemacs-sv variable)))
        (when minor-mode-p
          (if new-value
              (funcall variable new-value)
            (funcall variable -1))))
       (t
        ;; (message "%s changed outside ergoemacs-mode, respecting." variable)
        )))
     (t
      ;; (message "%s not changed" variable)
      ))))

(defun ergoemacs-reset (variable)
  "Sets VARIABLE to VALUE without disturbing customize or setq.
If FORCE is true, set it even if it changed.
"
  (let* ((minor-mode-p (and (string= "mode" (substring (symbol-name variable) -4))
                            (commandp variable t)))
         (value (get variable 'ergoemacs-save-value))
         (new-value (or (and (not minor-mode-p) value)
                        (and (integerp value) (< 0 value) value)
                        (and (not (integerp value)) value)
                        ;; Otherwise negative integers are the same as nil
                        )))
    (put variable 'ergoemacs-save-value nil)
    (if (and minor-mode-p (not (boundp variable)))
        (funcall variable new-value)
      (if (custom-variable-p variable)
          (progn
            (customize-set-variable variable new-value)
            (customize-mark-to-save variable)
            (when (and minor-mode-p (not new-value))
              (funcall variable -1)))
        (set variable new-value)
        (set-default variable new-value)
        (when minor-mode-p ;; Change minor mode
          (if new-value
              (funcall variable new-value)
            (funcall variable -1)))))))
;;;###autoload
(defun ergoemacs-save (variable value)
  "Set VARIABLE to VALUE and tell customize it needs to be saved."
  (if (not (custom-variable-p variable))
      (set variable value)
    (customize-set-variable variable value)
    (customize-mark-as-set variable)))

(defvar ergoemacs-theme-refresh nil)
(defvar ergoemacs-applied-inits '())

(defun ergoemacs-apply-struct-inits-obj (&optional obj)
  "Apply the initializations from the OBJ."
  (when (eq ergoemacs-theme-refresh t)
    (setq ergoemacs-theme-refresh ergoemacs-applied-inits))
  (let ((obj (or obj (ergoemacs-theme-components))))
    (dolist (init (ergoemacs-struct-variables obj))
      (let ((x (and ergoemacs-theme-refresh (boundp (nth 0 init))
                    (assq (nth 0 init) ergoemacs-theme-refresh))))
        (cond
         ((and x
               (not (nth 2 init))
               (not
                (equal (ergoemacs-sv (nth 0 init))
                       (funcall (nth 1 init)))))
          ;; Values have changed, so reapply.
          (setq ergoemacs-theme-refresh (delq x ergoemacs-theme-refresh)
                x nil))
         ((and x (nth 2 init))
          ;; Reapply hooks
          (setq ergoemacs-theme-refresh (delq x ergoemacs-theme-refresh)
                x nil)))
        (cond
         (x ;; Values have not changed
          (setq ergoemacs-theme-refresh (delq x ergoemacs-theme-refresh)))
         ((not (boundp (nth 0 init))) ;; Do nothing, not bound yet.
          )
         ((assq (nth 0 init) ergoemacs-applied-inits)
          ;; Already applied, Do nothing for now.
          )
         ((nth 2 init)
          ;; Hook
          (let ((add-hook-p (nth 0 (nth 2 init)))
                (append-p (nth 1 (nth 2 init)))
                (local-p (nth 2 (nth 2 init))))
            (if add-hook-p
                (funcall 'add-hook (nth 0 init) (nth 1 init) append-p local-p)
              (funcall 'remove-hook (nth 0 init) (nth 1 init) local-p))
            (push (list (nth 0 init) (nth 1 init)
                        (list (not add-hook-p) append-p local-p))
                  ergoemacs-applied-inits)))
         (t
          ;; (Nth 0 Init)iable state change
          (push (list (nth 0 init) (ergoemacs-sv (nth 0 init)))
                ergoemacs-applied-inits)
          (ergoemacs-set (nth 0 init) (funcall (nth 1 init))))))))
  ;; Now remove things that were not set
  (when ergoemacs-theme-refresh
    (let ((tmp ergoemacs-applied-inits))
      (setq ergoemacs-applied-inits ergoemacs-theme-refresh)
      (setq ergoemacs-theme-refresh nil)
      (unwind-protect
          (ergoemacs-remove-inits)
        (setq ergoemacs-applied-inits tmp)))))

(defun ergoemacs-remove-inits ()
  "Remove the applied initializations of modes and variables.
This assumes the variables are stored in `ergoemacs-applied-inits'"
  (message "Remove Inits %s" ergoemacs-theme-refresh)
  (if ergoemacs-theme-refresh
      (setq ergoemacs-theme-refresh ergoemacs-applied-inits)
    (dolist (init ergoemacs-applied-inits)
      (let ((var (nth 0 init))
            ;; (val (nth 1 init))
            (hook (nth 2 init)))
        (cond
         (hook
          (let ((add-hook-p (nth 0 hook))
                (append-p (nth 1 hook))
                (local-p (nth 2 hook)))
            (if add-hook-p
                (funcall 'add-hook (nth 0 init) (nth 1 init) append-p local-p)
              (funcall 'remove-hook (nth 0 init) (nth 1 init) local-p))))
         (t
          (ergoemacs-reset var))))))
  (setq ergoemacs-applied-inits '()))

;;; Theme variables

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
            (const :tag "Force Off" off)
            (const :tag "Force On" on)
            (const :tag "Let theme decide" nil))))
  :group 'ergoemacs-themes)

(defcustom ergoemacs-theme-version
  '()
  "Each themes set version"
  :type '(repeat
          (string :tag "Theme Component")
          (choice
           (const :tag "Latest Version" nil)
           (string :tag "Version")))
  :group 'ergoemacs-theme)


(defun ergoemacs-struct-versions (&optional obj)
  "Get Versions available for OBJ.
If Object isn't specified assume it is for the current ergoemacs theme."
  (let ((obj (or obj (ergoemacs-theme-components obj))))
    (sort (cond
           ((consp obj)
            (let (ret)
              (dolist (cur-obj (ergoemacs-get-map--lookup-hash obj))
                (dolist (ver (ergoemacs-struct-versions cur-obj))
                  (unless (member ver ret)
                    (push ver ret))))
              ret))
           (t (ergoemacs-struct-component-map-versions (ergoemacs-get-map--lookup-hash obj))))
          'string<)))

(defun ergoemacs-theme-set-version (version)
  "Sets the current themes default VERSION"
  (let (found)
    (setq ergoemacs-theme-version
          (mapcar
           (lambda(elt)
             (if (not (equal (or ergoemacs-theme "standard") (nth 0 elt)))
                 elt
               (setq found t)
               (list (or ergoemacs-theme "standard") version)))
           ergoemacs-theme-version))
    (unless found
      (push (list (or ergoemacs-theme "standard") version) ergoemacs-theme-version))))


(defvar ergoemacs-theme-hash)
(defun ergoemacs-theme-components (&optional theme)
  "Get a list of components used for the current theme.
This respects `ergoemacs-theme-options'."
  (let* ((theme (or theme ergoemacs-theme "standard"))
         (theme-plist (gethash (if (stringp theme) theme
                                 (symbol-name theme))
                               ergoemacs-theme-hash))
         components)
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
    (setq components (reverse components))
    components))

;;;###autoload
(defun ergoemacs-theme-option-off (option &optional no-custom)
  "Turns OPTION off.
Uses `ergoemacs-theme-option-on'."
  (ergoemacs-theme-option-on option no-custom 'off))

(defun ergoemacs-remove (option &optional theme type keep)
  "Removes an OPTION on ergoemacs themes.

Calls `ergoemacs-require' with TYPE defaulting to 'off and
remove defaulting to t.

KEEP can change remove to nil.
"
  (ergoemacs-require option theme (or type 'off) (if keep nil t)))

(defun ergoemacs-require (option &optional theme type remove)
  "Requires an OPTION on ergoemacs themes.

THEME can be a single theme or list of themes to apply the option
to.  If unspecified, it is all themes.

TYPE can be nil, where the option will be turned on by default
but shown as something that can be toggled in the ergoemacs-mode
menu.

TYPE can also be 'required-hidden, where the option is turned on,
and it dosen't show up on the ergoemacs-mode menu.

TYPE can also be 'off, where the option will be included in the
theme, but assumed to be disabled by default.

REMOVE represents when you would remove the OPTION from the
ergoemacs THEME.
"
  (if (eq (type-of option) 'cons)
      (dolist (new-option option)
        (let (ergoemacs-mode)
          (ergoemacs-require new-option theme type)))
    (let ((option-sym
           (or (and (stringp option) (intern option)) option)))
      (dolist (theme (or (and theme (or (and (eq (type-of theme) 'cons) theme) (list theme)))
                         (ergoemacs-get-themes)))
        (let ((theme-plist (gethash (if (stringp theme) theme
                                      (symbol-name theme))
                                    ergoemacs-theme-hash))
              comp on off)
          (setq comp (plist-get theme-plist ':components)
                on (plist-get theme-plist ':optional-on)
                off (plist-get theme-plist ':optional-off))
          (setq comp (delq option-sym comp)
                on (delq option-sym on)
                off (delq option-sym off))
          (cond
           (remove) ;; Don't do anything.
           ((eq type 'required-hidden)
            (push option-sym comp))
           ((eq type 'off)
            (push option-sym off))
           (t
            (push option-sym on)))
          (setq theme-plist (plist-put theme-plist ':components comp))
          (setq theme-plist (plist-put theme-plist ':optional-on on))
          (setq theme-plist (plist-put theme-plist ':optional-off off))
          (puthash (if (stringp theme) theme (symbol-name theme)) theme-plist
                   ergoemacs-theme-hash)))))
  (ergoemacs-theme-option-on option t))

(declare-function ergoemacs-mode "ergoemacs-mode.el")
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
    (ergoemacs-theme-reset)))

(defun ergoemacs-theme-toggle-option (option)
  "Toggles theme OPTION."
  (if (ergoemacs-theme-option-enabled-p option)
      (ergoemacs-theme-option-off option)
    (ergoemacs-theme-option-on option)))

(defun ergoemacs-theme-option-enabled-p (option)
  "Determines if OPTION is enabled."
  (let ((plist (gethash (or ergoemacs-theme "standard") ergoemacs-theme-hash))
        options-on options-off)
    (setq options-on (plist-get plist ':optional-on)
          options-off (plist-get plist ':optional-off))
    (or (and (member option options-on)
             (not (member (list option 'off) ergoemacs-theme-options)))
        (and (member option options-off)
             (member (list option 'on) ergoemacs-theme-options)))))

(defun ergoemacs-keymap-menu-theme-options (theme)
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
              (setq desc (ergoemacs-struct-get-component-description (symbol-name option)))
              (push option menu-options)
              (push
               `(,option
                 menu-item ,desc
                 (lambda()
                   (interactive)
                   (ergoemacs-theme-toggle-option ',option)
                   (customize-mark-as-set 'ergoemacs-theme-options)
                   (ergoemacs-theme-reset))
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
          (let ((desc (ergoemacs-struct-get-component-description (symbol-name option))))
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
                    (ergoemacs-theme-reset))
                  :button (:toggle . (ergoemacs-theme-option-enabled-p ',option)))))
            (sort options-list 'string<)))))))

(defun ergoemacs-keymap-menu-theme-version (theme)
  "Gets version menu for THEME"
  (let ((theme-versions (ergoemacs-theme-versions theme)))
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
            (ergoemacs-theme-reset))
          :button (:radio . (equal (ergoemacs-theme-get-version) nil)))
         ,@(mapcar
            (lambda(version)
              `(,(intern version) menu-item ,version
                (lambda() (interactive)
                  (ergoemacs-theme-set-version ,version)
                  (customize-mark-as-set 'ergoemacs-theme-version)
                  (ergoemacs-theme-reset))
                :button (:radio . (equal (ergoemacs-theme-get-version) ,version))))
            theme-versions))))))

(declare-function ergoemacs-get-layouts-menu "ergoemacs-layouts.el")
(defun ergoemacs-keymap-menu (theme)
  "Defines menus for current THEME."
  `(keymap
    ,(ergoemacs-get-layouts-menu)
    (ergoemacs-theme-sep "--")
    (ergoemacs-themes
     menu-item "Themes"
     (keymap
      ,@(mapcar
         (lambda(theme)
           `(,(intern theme) menu-item ,(concat theme " - " (plist-get (gethash theme ergoemacs-theme-hash) ':description))
             (lambda() (interactive)
               (ergoemacs-save 'ergoemacs-theme ,theme))
             :button (:radio . (string= (or ergoemacs-theme "standard") ,theme))))
         (sort (ergoemacs-get-themes) 'string<))))
    ,(ergoemacs-keymap-menu-theme-options theme)
    ,(ergoemacs-keymap-menu-theme-version theme)
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
    (ergoemacs-menus
     menu-item "Use Menus"
     (lambda() (interactive)
       (ergoemacs-save 'ergoemacs-use-menus (not ergoemacs-use-menus))
       (if ergoemacs-use-menus
           (progn
             (require 'ergoemacs-menus)
             (ergoemacs-menus-on))
         (when (featurep 'ergoemacs-menus)
           (ergoemacs-menus-off))))
     :button (:radio . ergoemacs-use-menus))
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


;; Startup and load functions
(defun ergoemacs-map--label-after-startup ()
  "Labels known unlabeled maps after startup. Also label maps after everything has loaded."
  (ergoemacs-map--label-unlabeled)
  (add-hook 'after-load-functions 'ergoemacs-map--label-unlabeled))
(add-hook 'after-init-hook 'ergoemacs-map--label-after-startup)


(unless init-file-user
  (run-with-idle-timer 0.05 nil 'ergoemacs-map--label-after-startup))

(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
