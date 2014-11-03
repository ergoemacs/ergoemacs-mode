;;; ergoemacs-map.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

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

(defvar ergoemacs-submaps--key nil)
(defvar ergoemacs-submaps--list nil)
(defvar ergoemacs-extract-keys-hash (make-hash-table :test 'equal))
(defvar ergoemacs-extract-keys--hash-1 nil)
(defvar ergoemacs-extract-keys--hash-2 nil)
(defvar ergoemacs-extract-keys--full-map nil)
(defun ergoemacs-extract-keys--puthash (cur-key value)
  (let (tmp)
    (cond
     ((eq (aref cur-key 0) 'ergoemacs-labeled)) ;; Ignore label
     ;; Keymaps
     ;; Indirect maps
     ((ignore-errors (keymapp (symbol-function value)))
      (ergoemacs-map--label (symbol-function value) nil t nil cur-key)
      (ergoemacs-extract-keys (symbol-function value) nil (or cur-key t)))
     ;; Prefix keys
     ((ignore-errors (and (keymapp value) (listp value)))
      (ergoemacs-map--label value nil t nil cur-key)
      (ergoemacs-extract-keys value nil (or cur-key t)))
     ;; Ignore defined keys
     ;; ((gethash cur-key ergoemacs-extract-keys--hash-2))
     
     ;; Command
     ;; ((ignore-errors (and (vectorp cur-key) (not value)))
     ;;  (puthash cur-key 'undefined ergoemacs-extract-keys--hash-2))

     ;; Menu item [tool-bar]
     ((ignore-errors
        (and (vectorp cur-key)
             (eq 'menu-item (cdar value))))
      (puthash (vconcat cur-key (vector (car value))) (cdr value)
               ergoemacs-extract-keys--hash-2))
     ;; Menu ("String" keymap), like ("File" keymap ...)
     ((ignore-errors
        (and (stringp (nth 0 value))
             (eq (nth 1 value) 'keymap)))
      (puthash cur-key (list (nth 0 value) 'keymap)
               ergoemacs-extract-keys--hash-2)
      (setq tmp value)
      (pop tmp)
      (ergoemacs-extract-keys tmp nil (or cur-key t)))
     ;; Command
     ((and (vectorp cur-key)
           (or (commandp value t) ;; Command
               (stringp value) ;; String
               (symbolp value) ;; Symbol
               (and (consp value) (eq (car value) 'menu-item))
               (and (consp value) (stringp (car value))
                    (symbolp (cdr value)))))
      (puthash cur-key value ergoemacs-extract-keys--hash-2)
      (unless (stringp value)
        (setq tmp (gethash value ergoemacs-extract-keys--hash-1))
        (push cur-key tmp)
        (puthash value tmp ergoemacs-extract-keys--hash-1)))
     ;; Only use interactive commands.
     (t
      (warn "Ignorning %s->%s" (or (ignore-errors (key-description cur-key)) (format "err-%s" cur-key)) value))
     )))

(defun ergoemacs-extract-keys--flatten (item)
  "Internal function to create keymap for ITEM."
  (let ((ret (if ergoemacs-extract-keys--full-map
                 (make-keymap)
               (make-sparse-keymap))))
    (maphash
     (lambda(key def)
       (condition-case err
           (if (and (consp key) ergoemacs-extract-keys--full-map)
               (set-char-table-range (nth 1 ret) key def)
             (define-key ret key def))
         (error
          (warn "Error defining %s->%s (%s)" (if (eq key t) "Default" (key-description key)) def err))))
     (nth 1 item))
    ret))

(defun ergoemacs-extract-keys (keymap &optional flatten pre)
  "Create a hash table of functions and their keys from a keymap."
  (let (tmp
        ret)
    (if (and (not ergoemacs-submaps--key)
             (setq tmp (ergoemacs-map-p keymap))
             (setq ret (gethash tmp ergoemacs-extract-keys-hash)))
        (if (not flatten) ret
          (setq ergoemacs-extract-keys--full-map (ergoemacs-map-get keymap :full))
          (prog1
              (ergoemacs-extract-keys--flatten ret)
            (setq ergoemacs-extract-keys--full-map nil)))
      (unless pre
        (ergoemacs-map--label keymap nil t)
        (setq ergoemacs-submaps--key (ergoemacs-map-p keymap)
              ergoemacs-submaps--list '()
              ergoemacs-extract-keys--full-map nil
              ergoemacs-extract-keys--hash-1
              (make-hash-table)
              ergoemacs-extract-keys--hash-2
              (make-hash-table :test 'equal)))
      (if (not (keymapp keymap)) ergoemacs-extract-keys--hash-1
        (dolist (key (reverse keymap))
          (cond
           ((ignore-errors (char-table-p key))
            (setq ergoemacs-extract-keys--full-map t)
            (while key
              (map-char-table
               #'(lambda(key-2 value)
                   ;; Can be a list (from . to)
                   (if (consp key-2)
                       (puthash key-2 value ergoemacs-extract-keys--hash-2)
                     (ergoemacs-extract-keys--puthash (or (and (vectorp pre) (vconcat pre (vector key-2)))
                                                          (vector key-2)) value)))
               key)
              (setq key (char-table-parent key))))
           ((ignore-errors (car key))
            (ergoemacs-extract-keys--puthash
             (or (and (vectorp pre) (integerp (car key)) (vconcat pre (vector (car key))))
                 (and (vectorp pre) (stringp (car key)) (= 1 (length (car key))) (vconcat pre (vector (get-byte 0 (car key)))))
                 (and (vectorp pre) (vconcat pre (vector (car key))))
                 (and (integerp (car key)) (vector (car key)))
                 (and (stringp (car key)) (=  1 (length (car key))) (vector (get-byte 0 (car key))))
                 (vector (car key))) (cdr key)))))
        (unless pre
          (setq ret (list ergoemacs-extract-keys--hash-1 ergoemacs-extract-keys--hash-2))
          (if flatten
              (setq ret (ergoemacs-extract-keys--flatten ret) ret)
            (ergoemacs-map-put keymap :submaps ergoemacs-submaps--list)
            (puthash (ergoemacs-map-p keymap) ret ergoemacs-extract-keys-hash))
          (setq ergoemacs-extract-keys--hash-1 nil
                ergoemacs-extract-keys--hash-2 nil
                ergoemacs-extract-keys--full-map nil
                ergoemacs-submaps--key nil
                ergoemacs-submaps--list nil))
        ret))))

(defun ergoemacs-submap-p (keymap)
  "Returns if this is a submap of another keymap.
If unknown, return 'unknown
If a submap, return a list of the keys and parent map(s)
If not a submap, return nil
"
  
  (let* ((ret (ergoemacs-map-plist keymap)))
    (or (and (not ret) 'unknown)
        (plist-get ret :submap-p))))

(defun ergoemacs-submaps (keymap)
  "Returns the known submaps of this keymap."
  (ergoemacs-map-get keymap :submaps))

(defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector defined)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.

When DEFINED is non-nil, use the list to know previously defined keys.  Also will return a list (prefix-list defined-list)
"
  (if (not (keymapp keymap)) nil
    (let (ret (ret2 '()) (cur-defined (if (eq defined t) nil defined)) tmp)
      (dolist (key keymap)
        (cond
         ((ignore-errors (keymapp key))
          (setq tmp (ergoemacs-extract-prefixes key dont-ignore return-vector (if cur-defined cur-defined t)))
          (dolist (item (reverse (nth 0 tmp)))
            (unless (member item ret)
              (push item ret)))
          (dolist (item (reverse (nth 1 tmp)))
            (unless (member item cur-defined)
              (push item cur-defined))))
         ((ignore-errors (member (vector (car key)) cur-defined))) ;; Ignore already defined keys.
         ((ignore-errors (keymapp (cdr key)))
          (push (vector (car key)) ret))
         ((ignore-errors (char-table-p key))
          (map-char-table
           #'(lambda(key-2 value)
               (if (keymapp value)
                   (push (vector key-2) ret)
                 (push (vector key-2) cur-defined)))
           key))
         ((ignore-errors (car key)) (push (vector (car key)) cur-defined))))
      (if defined
          (list ret cur-defined)
        (if (and dont-ignore return-vector) ret
          (dolist (a ret)
            (let ((tmp (key-description a)))
              (when (or dont-ignore (not (member a ergoemacs-ignored-prefixes)))
                (if return-vector
                    (push a ret2)
                  (push tmp ret2)))))
          ret2)))))

(when (not (fboundp 'make-composed-keymap))
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


(defun ergoemacs-flatten-composed-keymap--define-key (keymap parent &optional pre-vector)
  "Define keys in KEYMAP in PARENT keymap recursively.
PRE-VECTOR is to help define the full key-vector sequence."
  (dolist (item keymap)
    (let ((key (ignore-errors (or (and pre-vector (vconcat pre-vector (vector (car item)))) (vector (car item)))))
          i)
      (cond
       ((eq item 'keymap))
       ((and key (cdr item)
             (ignore-errors (or (symbolp (cdr item)) (commandp (cdr item) t))))
        (setq i (lookup-key parent key))
        (when (integerp i)
          (define-key parent (substring key 0 i) nil))
        (define-key parent key (cdr item)))
       ((and key (equal key [menu-bar]))
        (define-key parent key nil)
        (define-key parent key (cdr item)))
       ((and key (ignore-errors (eq 'keymap (nth 1 item))))
        (ergoemacs-flatten-composed-keymap--define-key (cdr item) parent key))
       ((and key (equal key [keymap]) (keymapp item))
        (ergoemacs-flatten-composed-keymap--define-key item parent pre-vector))
       (t
        ;; (message "This: %s %s %s" pre-vector key item)
        )))))

(defvar ergoemacs-movement-functions)
(defun ergoemacs-flatten-composed-keymap (keymap &optional force-shifted)
  "Flattens a composed KEYMAP.
If it is not a composed KEYMAP, return the keymap as is.

This will also install
`ergoemacs-shortcut-movement-force-shift-select' when
FORCE-SHIFTED is non-nil."
  (if (not (ignore-errors (and (keymapp keymap) (eq (nth 0 (nth 1 keymap)) 'keymap)))) keymap
    (let* (new-keymap
           trans
           (remaining (cdr keymap))
           (keymap-list '()))
      (while (keymapp (car remaining))
        (push (pop remaining) keymap-list)) ;; Should be reversed
      ;; Parent keymap
      (if (keymapp remaining)
          (setq new-keymap (copy-keymap remaining))
        (setq new-keymap (make-sparse-keymap)))
      (dolist (sub-keymap keymap-list)
        (ergoemacs-flatten-composed-keymap--define-key sub-keymap new-keymap))
      (when force-shifted
        (dolist (move-fn (append ergoemacs-movement-functions
                                 '(ergoemacs-shortcut-movement)))
          (dolist (move-key (where-is-internal move-fn new-keymap))
            (setq trans (plist-get (ergoemacs-translate move-key) ':caps-translated-key))
            (when (and trans (not (lookup-key new-keymap trans)))
              (define-key new-keymap trans 'ergoemacs-shortcut-movement-force-shift-select)))))
      (ergoemacs-setcdr keymap (cdr new-keymap))
      keymap)))

(defun ergoemacs-map-plist (keymap)
  "Determines if this is an `ergoemacs-mode' KEYMAP.
Returns a plist of keymap properties"
  (or
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

(defun ergoemacs-map-get (keymap property)
  "Gets ergoemacs-mode KEYMAP PROPERTY."
  (let ((ret (ergoemacs-map-plist keymap)))
    (and ret (plist-get ret property))))

(defun ergoemacs-map-put (keymap property value)
  "Set ergoemacs-mode KEYMAP PROPERTY to VALUE."
  (let ((ret (ergoemacs-map-plist keymap)))
    (when ret
      (setq ret (plist-put ret property value))
      (ergoemacs-map--label keymap nil 'keep nil nil ret))))

(defun ergoemacs-map-p (keymap)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map"
  (ergoemacs-map-get keymap :map-list))



(defun ergoemacs-map--name (keymap)
  "Gets the first symbol pointing to this KEYMAP (if any)"
  (or
   (ergoemacs-map-p keymap)
   (let (ret)
     (unless (or (equal keymap (make-sparse-keymap))
                 (equal keymap (make-keymap)))
       (mapatoms
        (lambda(map)
          (when (and (special-variable-p map) ;; Only save
                     ;; defvar/defcustom, etc variables
                     (ignore-errors (keymapp (ergoemacs-sv map)))
                     (or (eq (ergoemacs-sv map) (ergoemacs-sv keymap))
                         (eq (ergoemacs-sv map) keymap)))
            (push map ret)))))
     (unless ret
       (setq ret (list (intern (concat "ergoemacs-unbound-" (format-time-string "%s-%N"))))))
     ret)))

(defun ergoemacs-map--label (keymap &optional map-name unmodified strip submap-vector replace-plist)
  "Label an `ergoemacs-mode' touched keymap.
UNMODIFIED, labels the keymap as practically untouched.
MAP-NAME is the identifier of the map name.
When STRIP is true, remove all `ergoemacs-mode' labels

The KEYMAP will have the structure

  (keymap optional-char-table \"Optional Label\" (ergoemacs-(un)modified function-for-plist) true-map)

"
  (if (not (keymapp keymap)) nil
    (let* ((map keymap)
           (maps (or map-name (ergoemacs-map--name keymap)))
           (unbound-p (string-match-p  "^ergoemacs-unbound-" (symbol-name (nth 0 maps))))
           (unmodified unmodified)
           char-table
           (old-plist '())
           label tmp1 tmp2)
      (if (eq (car map) 'keymap)
          (setq map (cdr map))
        (setq map (list map)))
      (when (char-table-p (car map))
        (setq char-table (pop map)))
      (when (stringp (car map))
        (setq label (pop map)))
      ;; Drop prior `ergoemacs-mode' labels
      (when (ignore-errors (eq (car (car map)) 'ergoemacs-labeled))
        (setq old-plist (funcall (cdr (car map))))
        (setq unmodified (if (eq unmodified 'keep)
                             (plist-get old-plist :unmodified)
                           unmodified))
        (setq map (cdr map)))
      (when replace-plist
        (setq old-plist replace-plist))
      (when (and ergoemacs-submaps--key (not unbound-p) (vectorp submap-vector))
        (setq tmp1 (plist-get old-plist ':submap-p))
        (pushnew (cons submap-vector ergoemacs-submaps--key) tmp1 :test 'equal)
        (setq old-plist (plist-put old-plist ':submap-p tmp1))
        ;; Add label in original map
        (pushnew (cons submap-vector maps) ergoemacs-submaps--list :test 'equal))
      (unless (or strip
                  (and submap-vector unbound-p))
        (setq old-plist (plist-put old-plist ':label label))
        (setq old-plist (plist-put old-plist ':full (if char-table t nil)))
        (setq old-plist (plist-put old-plist ':map-list maps))
        (setq old-plist (plist-put old-plist ':unmodified unmodified))
        (push (cons 'ergoemacs-labeled
                `(lambda() (interactive) ',old-plist)) map))
      (when label
        (push label map))
      (when char-table
        (push char-table map))
      (push 'keymap map)
      (ergoemacs-setcdr keymap (cdr map))
      map)))

(defvar ergoemacs-original-map-hash)
(defvar ergoemacs-command-shortcuts-hash)
(defun ergoemacs-map--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed."
  (let ((map-name (ergoemacs-map-p keymap)))
    (if (not map-name)
        (let ((maps (ergoemacs-map--name keymap)))
          (ergoemacs-map--label keymap maps t)
          (dolist (map-name maps) ;; Save to original map hash
            (unless (gethash map-name ergoemacs-original-map-hash)
              (puthash map-name (copy-keymap keymap) ergoemacs-original-map-hash)))
          keymap)
      (gethash (car map-name) ergoemacs-original-map-hash))))

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
      (ergoemacs-map--label new-map maps)
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

(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
