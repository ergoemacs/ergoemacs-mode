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
(defvar ergoemacs-submaps-- nil)
(defvar ergoemacs-extract-keys-hash (make-hash-table :test 'equal))
(defvar ergoemacs-extract-keys--hash-1 nil)
(defvar ergoemacs-extract-keys--hash-2 nil)
(defvar ergoemacs-extract-keys--full-map nil)
(defvar ergoemacs-extract-keys--keymap nil)
(defvar ergoemacs-extract-keys--prefixes nil)
(defvar ergoemacs-extract-keys--base-map nil)

(defun ergoemacs-extract-keys--handle-keymap (keymap cur-key compare)
  (ergoemacs-map--label keymap nil t nil cur-key)
  (when ergoemacs-extract-keys--base-map
    (push cur-key ergoemacs-extract-keys--prefixes))
  (unless compare ;; Indicate this is a prefix key
    (puthash cur-key 'ergoemacs-prefix ergoemacs-extract-keys--hash-2))
  (if (ergoemacs-map-p keymap)
      ;; Bound submap, traverse submap later
      (push (ergoemacs-map-p keymap) ergoemacs-submaps--)
    (let ((tmp ergoemacs-extract-keys--base-map))
      (setq ergoemacs-extract-keys--base-map nil)
      (ergoemacs-extract-keys keymap nil (or cur-key t) compare)
      (setq ergoemacs-extract-keys--base-map tmp))))

(defun ergoemacs-extract-keys--puthash (cur-key value compare)
  (let (tmp)
    (cond
     ((eq (aref cur-key 0) 'ergoemacs-labeled)) ;; Ignore label
     ;; Ignore defined keys
     ((gethash cur-key ergoemacs-extract-keys--hash-2))
     ;; Keymaps
     ;; Indirect maps
     ((ignore-errors (keymapp (symbol-function value)))
      (ergoemacs-extract-keys--handle-keymap (symbol-function value) cur-key compare))
     ;; Prefix keys
     ((ignore-errors (and (keymapp value) (listp value)))
      (ergoemacs-extract-keys--handle-keymap value cur-key compare))

     ;; Command
     ((ignore-errors (and (vectorp cur-key) (not value)))
      (puthash cur-key 'ergoemacs-nil ergoemacs-extract-keys--hash-2))

     ;; Menu item [tool-bar]
     ((ignore-errors
        (and (vectorp cur-key)
             (eq 'menu-item (cdar value))))
      (unless compare
        (puthash (vconcat cur-key (vector (car value))) (cdr value)
                 ergoemacs-extract-keys--hash-2)))
     
     ;; Menu ("String" keymap), like ("File" keymap ...)
     ((ignore-errors
        (and (stringp (nth 0 value))
             (eq (nth 1 value) 'keymap)))
      (unless compare
        (puthash cur-key (list (nth 0 value) 'keymap)
                 ergoemacs-extract-keys--hash-2)
        (setq tmp value)
        (pop tmp)
        (let ((tmp2 ergoemacs-extract-keys--base-map))
          (setq ergoemacs-extract-keys--base-map nil)
          (ergoemacs-extract-keys tmp nil (or cur-key t))
          (setq ergoemacs-extract-keys--base-map tmp2))))
     ;; Command
     ((and (vectorp cur-key)
           (or (commandp value t) ;; Command
               (stringp value) ;; String
               (symbolp value) ;; Symbol
               (and (consp value) (eq (car value) 'menu-item))
               (and (consp value) (stringp (car value))
                    (symbolp (cdr value)))))
      (if compare
          (progn
            (setq tmp (gethash ergoemacs-submaps--key ergoemacs-extract-keys-hash))
            (when (hash-table-p (nth 1 tmp))
              (setq tmp (gethash cur-key (nth 1 tmp)))
              (unless (and tmp (eq tmp value))
                (define-key ergoemacs-extract-keys--keymap cur-key value))))
        (puthash cur-key value ergoemacs-extract-keys--hash-2)
        (unless (stringp value)
          (setq tmp (gethash value ergoemacs-extract-keys--hash-1))
          (push cur-key tmp)
          (puthash value tmp ergoemacs-extract-keys--hash-1))))
     (t
      (warn "Ignorning %s->%s" (or (ignore-errors (key-description cur-key)) (format "err-%s" cur-key)) value)))))

(defun ergoemacs-extract-keys--flatten (item submaps &optional keymap prefix)
  "Internal function to create keymap for ITEM."
  (let ((ret (or keymap
                 (if ergoemacs-extract-keys--full-map
                     (make-keymap)
                   (make-sparse-keymap))))
        tmp tmp2)
    (maphash
     (lambda(key def)
       (condition-case err
           (if (and (consp key) ergoemacs-extract-keys--full-map (not prefix))
               (set-char-table-range (nth 1 ret) key def)
             (if (consp key)
                 (progn
                   (message "Ignoring %s from %s to %s" def
                            (car key) (cdr key))
                   ;; (loop for char from (car key) to (cdr key)
                   ;;     do (define-key ret (or (and prefix (vconcat prefix (vector char)))
                   ;;                            (vector char)) def))
                   )
               (define-key ret (or (and prefix (vconcat prefix key)) key) def)))
         (error
          (warn "Error defining %s->%s (%s)" (if (eq key t) "Default" (key-description key)) def err))))
     (nth 1 item))
    (dolist (key submaps)
      (setq tmp (symbol-value (nth 1 key))
            tmp2 ergoemacs-extract-keys--full-map)
      (setq ergoemacs-extract-keys--full-map (ergoemacs-map-get tmp :full))
      (ergoemacs-extract-keys--flatten
       (ergoemacs-extract-keys tmp) (ergoemacs-submaps keymap)
       ret (or (and prefix (vconcat prefix (nth 0 key)))
               (nth 0 key)))
      (setq ergoemacs-extract-keys--full-map tmp2))
    ret))

(defun ergoemacs-extract-keys (keymap &optional flatten pre compare)
  "Create a hash table of functions and their keys from a keymap.

FLATTEN will create a single keymap without submaps, parent maps,
or composed maps.

PRE represents the current prefix (for recursive calls).

COMPARE will compare differences to the current hash.
"
  (let (tmp ret)
    (if (and (not ergoemacs-submaps--key)
             (not compare)
             (setq tmp (ergoemacs-map-p keymap))
             (setq ret (gethash tmp ergoemacs-extract-keys-hash)))
        (if (not flatten) ret
          (setq ergoemacs-extract-keys--full-map (ergoemacs-map-get keymap :full))
          (prog1
              (ergoemacs-extract-keys--flatten ret (ergoemacs-submaps keymap))
            (setq ergoemacs-extract-keys--full-map nil)))
      (unless pre
        (ergoemacs-map--label keymap nil t)
        (setq ergoemacs-submaps--key (ergoemacs-map-p keymap)
              ergoemacs-submaps--list '()
              ergoemacs-extract-keys--full-map nil
              ergoemacs-extract-keys--hash-1
              (make-hash-table)
              ergoemacs-extract-keys--hash-2
              (make-hash-table :test 'equal)
              ergoemacs-extract-keys--prefixes nil
              ergoemacs-extract-keys--base-map t
              ergoemacs-extract-keys--keymap (make-sparse-keymap)))
      (if (not (keymapp keymap)) ergoemacs-extract-keys--hash-1
        (dolist (key keymap)
          (cond
           ((ignore-errors (char-table-p key))
            (setq ergoemacs-extract-keys--full-map t)
            (while key
              (map-char-table
               #'(lambda(key-2 value)
                   ;; Can be a list (from . to)
                   (if (consp key-2)
                       (if (not compare)
                           (puthash key-2 value ergoemacs-extract-keys--hash-2)
                         ;; FIXME -- See if anything changed here...
                         )
                     (ergoemacs-extract-keys--puthash (or (and (vectorp pre) (vconcat pre (vector key-2)))
                                                          (vector key-2)) value compare)))
               key)
              (setq key (char-table-parent key))))
           ((ignore-errors (and (car key)) (eq 'keymap (car key)))
            (warn "Composed keymap."))
           ((ignore-errors (car key))
            (ergoemacs-extract-keys--puthash
             (or (and (vectorp pre) (integerp (car key)) (vconcat pre (vector (car key))))
                 (and (vectorp pre) (stringp (car key)) (= 1 (length (car key))) (vconcat pre (vector (get-byte 0 (car key)))))
                 (and (vectorp pre) (vconcat pre (vector (car key))))
                 (and (integerp (car key)) (vector (car key)))
                 (and (stringp (car key)) (=  1 (length (car key))) (vector (get-byte 0 (car key))))
                 (vector (car key))) (cdr key) compare))))
        (unless pre
          (setq ret (list ergoemacs-extract-keys--hash-1 ergoemacs-extract-keys--hash-2))
          (ergoemacs-map-put keymap :submaps ergoemacs-submaps--list)
          (when compare
            (ergoemacs-map-put keymap :changes-before-map ergoemacs-extract-keys--keymap))
          (when ergoemacs-extract-keys--prefixes
            (ergoemacs-map-put keymap :prefixes ergoemacs-extract-keys--prefixes))
          (if flatten
              (setq ret (ergoemacs-extract-keys--flatten ret (ergoemacs-submaps keymap)))
            (unless compare
              (puthash (ergoemacs-map-p keymap) ret ergoemacs-extract-keys-hash)))
          (setq ergoemacs-extract-keys--hash-1 nil
                ergoemacs-extract-keys--hash-2 nil
                ergoemacs-extract-keys--full-map nil
                ergoemacs-submaps--key nil
                ergoemacs-submaps--list nil
                ergoemacs-extract-keys--base-map nil
                ergoemacs-extract-keys--prefixes nil
                ergoemacs-extract-keys--keymap nil)
          (while ergoemacs-submaps--
            (setq tmp ergoemacs-submaps--)
            (setq ergoemacs-submaps-- nil)
            (dolist (key tmp)
              (ergoemacs-extract-keys (symbol-value (nth 0 key))
                                      nil nil compare))))
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

;;; FIXME: Write tests for this function
(defun ergoemacs-prior-function (key &optional where-is before-ergoemacs keymap)
  "Looks up the original binding of KEY.

If KEYMAP is nil, assumes the keymap is `global-map'.

If BEFORE-ERGOEMACS is non-nil, assume bindings applied before
`ergoemacs-mode' loaded are the original binding.

If WHERE-IS is non-nil, return a list of the keys (in vector format) where this is bound.
"
  (let* ((map (or keymap global-map))
         (hash-lst (ergoemacs-extract-keys map))
         (hash-1 (nth 0 hash-lst))
         (hash-2 (nth 1 hash-lst))
         (new-key (or (and (vectorp key) key)
                      (read-kbd-macro (key-description key) t)))
         (before-ergoemacs-map (and before-ergoemacs (ergoemacs-map-get map :changes-before-map)))
         (prior (or (and (keymapp before-ergoemacs-map) (lookup-key before-ergoemacs-map key))
                    (gethash new-key hash-2))) prefix)
    (when (integerp prior)
      (setq prior nil))
    (if (and prior where-is)
        (setq prior (gethash prior hash-1)))
    (unless prior
      (when (catch 'found-submap
              (dolist (submap (ergoemacs-submaps map))
                (when (boundp (cadr submap))
                  (setq prefix (substring new-key 0 (length (car submap))))
                  (when (equal prefix  (car submap))
                    (setq map (symbol-value (cadr submap)))
                    (setq new-key (substring new-key (length (car submap))))
                    (throw 'found-submap t)))) nil)
        (setq prior (ergoemacs-prior-function new-key where-is after-ergoemacs map))
        (when where-is
          (setq prior (mapcar (lambda(x) (vconcat prefix x)) prior)))))
    prior))

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

(defun ergoemacs-default-global--gen ()
  "Generates hash for default emacs maps."
  (ergoemacs-extract-keys global-map)
  (ergoemacs-extract-keys minibuffer-local-map)
  (ergoemacs-extract-keys minibuffer-local-ns-map)
  (ergoemacs-extract-keys minibuffer-local-completion-map)
  (ergoemacs-extract-keys minibuffer-local-must-match-map)
  (ergoemacs-extract-keys minibuffer-local-filename-completion-map)
  (with-temp-file (ergoemacs-default-global--file) 
    (let ((print-level nil)
          (print-length nil))
      (goto-char (point-min))
      (maphash
       (lambda(key _item)
         (insert (format "(when (boundp '%s) (ergoemacs-map--label %s '%s nil nil nil '"
                         (nth 0 key) (nth 0 key) key))
         (prin1 (ergoemacs-map-plist (symbol-value (nth 0 key))) (current-buffer))
         (insert "))"))
       ergoemacs-extract-keys-hash)
      (insert "(setq ergoemacs-extract-keys-hash ")
      (prin1 ergoemacs-extract-keys-hash (current-buffer))
      (goto-char (point-max))
      (insert ")"))))

(defun ergoemacs-map-default-global ()
  "Loads/Creates the default global map information."
  (if (file-readable-p (ergoemacs-default-global--file))
      (load (ergoemacs-default-global--file))
    (switch-to-buffer-other-window (get-buffer-create "*ergoemacs-get-default-keys*"))
    (let* ((emacs-exe (ergoemacs-emacs-exe))
           (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
           (cmd (format "%s -L %s --load \"ergoemacs-mode\" -Q --batch --eval \"(ergoemacs-default-global--gen)\"" emacs-exe default-directory))
           (process (start-process-shell-command "ergoemacs-global" "*ergoemacs-get-default-keys*" cmd)))
      (set-process-sentinel process 'ergoemacs-map-default-global--finish)))
  ;; Figure differences from default global map
  (ergoemacs-extract-keys global-map nil nil t))

(defun ergoemacs-map-default-global--finish (process change)
  "Run the clean environment"
  (when (string-match "finished" change)
    (kill-buffer (get-buffer-create "*ergoemacs-get-default-keys*"))
    (ergoemacs-map-default-global)))


(defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.
"
  (if (not (keymapp keymap)) nil
    (if (not (ergoemacs-map-p keymap))
        (progn
          (ergoemacs-extract-keys keymap)
          (ergoemacs-extract-prefixes keymap dont-ignore return-vector))
      (let ((ret (ergoemacs-map-get keymap :prefixes)) ret2)
        (if (and dont-ignore return-vector) ret
          (dolist (a ret)
            (let ((tmp (key-description a)))
              (when (or dont-ignore (not (member a ergoemacs-ignored-prefixes)))
                (if return-vector
                    (push a ret2)
                  (push tmp ret2)))))
          ret2)))))

;; (defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector defined)
;;   "Extract prefix commands for KEYMAP.
;; Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

;; When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

;; When RETURN-VECTOR is non-nil, return list of the keys in a vector form.

;; When DEFINED is non-nil, use the list to know previously defined keys.  Also will return a list (prefix-list defined-list)
;; "
;;   (if (not (keymapp keymap)) nil
;;     (let (ret (ret2 '()) (cur-defined (if (eq defined t) nil defined)) tmp)
;;       (dolist (key keymap)
;;         (cond
;;          ((ignore-errors (keymapp key))
;;           (setq tmp (ergoemacs-extract-prefixes key dont-ignore return-vector (if cur-defined cur-defined t)))
;;           (dolist (item (reverse (nth 0 tmp)))
;;             (unless (member item ret)
;;               (push item ret)))
;;           (dolist (item (reverse (nth 1 tmp)))
;;             (unless (member item cur-defined)
;;               (push item cur-defined))))
;;          ((ignore-errors (member (vector (car key)) cur-defined))) ;; Ignore already defined keys.
;;          ((ignore-errors (keymapp (cdr key)))
;;           (push (vector (car key)) ret))
;;          ((ignore-errors (char-table-p key))
;;           (map-char-table
;;            #'(lambda(key-2 value)
;;                (if (keymapp value)
;;                    (push (vector key-2) ret)
;;                  (push (vector key-2) cur-defined)))
;;            key))
;;          ((ignore-errors (car key)) (push (vector (car key)) cur-defined))))
;;       (if defined
;;           (list ret cur-defined)
;;         (if (and dont-ignore return-vector) ret
;;           (dolist (a ret)
;;             (let ((tmp (key-description a)))
;;               (when (or dont-ignore (not (member a ergoemacs-ignored-prefixes)))
;;                 (if return-vector
;;                     (push a ret2)
;;                   (push tmp ret2)))))
;;           ret2)))))

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
  (let ((ret (lookup-key keymap [ergoemacs-labeled])))
    (and ret (funcall ret))))

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

(defun ergoemacs-map-composed-p (keymap)
  "Determine if the KEYMAP is a composed keymap."
  (and (ignore-errors (eq 'keymap (car keymap)))
       (ignore-errors (eq 'keymap (caadr keymap)))))

(defun ergoemacs-map-composed-list (keymap)
  "Return the list of maps in a composed KEYMAP.
If there are no maps, return nil."
  (if (not (ergoemacs-map-composed-p keymap)) nil
    (let ((parent (keymap-parent keymap))
          tmp ret)
      (unwind-protect
          (progn
            (when parent
              (set-keymap-parent keymap nil))
            (dolist (map (cdr keymap))
              (setq tmp '(keymap))
              (ergoemacs-setcdr tmp (cdr map))
              (push tmp ret)))
        (when parent
          (set-keymap-parent keymap parent)))
      ret)))

(defun ergoemacs-map-p (keymap)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map."
  (let ((parent (keymap-parent keymap))
        (composed-list (ergoemacs-map-composed-list keymap)))
    (list (and parent (ergoemacs-map-get parent :map-list))
          (ergoemacs-map-get keymap :map-list)
          (and composed-list (mapcar (lambda(keymap) (ergoemacs-map-get keymap :map-list)) composed-list)))))

(defun ergoemacs-map-boundp (keymap &optional force)
  "Returns if the maps linked to the current map are unbound, if it is an `ergoemacs-mode' map.
When FORCE is on, figure out if it is bound."
  (let ((ret (symbol-name (car (ergoemacs-map-get keymap :map-list)))))
    (if (not (string= ret "nil"))
        (string-match-p "^ergoemacs-unbound-" ret)
      (if (not force) nil
        (ergoemacs-map--label keymap nil t) ;; Assume map is unmodified.
        (ergoemacs-map-boundp keymap)))))

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
           (old-plist (funcall (lookup-key keymap [ergoemacs-labeled]) (cdr (car map))))
           label tmp1)
      (if (eq (car map) 'keymap)
          (setq map (cdr map))
        (setq map (list map)))
      (when (char-table-p (car map))
        (setq char-table (pop map)))
      (when (stringp (car map))
        (setq label (pop map)))
      ;; Drop prior `ergoemacs-mode' labels
      (when (ignore-errors (eq (car (car map)) 'ergoemacs-labeled))
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
      (when label
        (push label map))
      (when char-table
        (push char-table map))
      (push 'keymap map)
      (unless (or strip
                  (and submap-vector unbound-p))
        (setq old-plist (plist-put old-plist ':label label))
        (setq old-plist (plist-put old-plist ':full (if char-table t nil)))
        (setq old-plist (plist-put old-plist ':map-list maps))
        (setq old-plist (plist-put old-plist ':unmodified unmodified))
        (define-key map [ergoemacs-labeled] `(lambda() (interactive) ',old-plist)))
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
