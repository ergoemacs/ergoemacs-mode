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

The MODIFIER to something like 'meta tells which modifier to remove.

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
        (ignore-errors (and (setq tmp (gethash keymap ergoemacs-map-plist-hash))
                            (setq tmp (gethash :map-list tmp))
                            (symbol-value (car tmp))))
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
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map."
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

(defun ergoemacs-mapkeymap--key-keymap (key keymap function submaps &optional prefix)
  "Handle KEYMAPs"
  (when function
    (funcall function key 'ergoemacs-prefix (or prefix t)))
  (when (and ergoemacs-mapkeymap--key (not prefix))
    ;; (pushnew key ergoemacs-mapkeymap--prefixes :test 'equal)
    (add-to-list 'ergoemacs-mapkeymap--prefixes key))
  (if (and submaps (not (eq submaps 'prefix)) (ergoemacs-map-p keymap))
      ;; (pushnew (cons key (ergoemacs-map-p keymap)) ergoemacs-mapkeymap--submaps :test 'equal)
      (add-to-list 'ergoemacs-mapkeymap--submaps (cons key (ergoemacs-map-p keymap)))
      (ergoemacs-mapkeymap--loop
       function (ergoemacs-map-keymap-value keymap) submaps key)))

(defun ergoemacs-mapkeymap--key-item (key item function submaps &optional prefix)
  "Process an ITEM for KEY and possibly call FUNCTION, or defer keymap evaluation when SUBMAPS is true.
PREFIX is the current PREFIX for the key code. "
  (cond
   ;; Already ignored keys
   ((member key ergoemacs-mapkeymap--nil))
   
   ;; Ignore already defined keys
   ((and (vectorp key) (lookup-key ergoemacs-mapkeymap--current key)))

   ;; FIXME -- key ranges... What should be done here...?

   ;; keys defined to be nil, should mask other keys
   ((not item)
    ;; Make nil keys mask other keys

    ;; FIXME do a range too.., but I'm not sure that should be
    ;; supported by emacs full keymaps...?
    (push key ergoemacs-mapkeymap--nil)
    (when function
      (funcall function key nil (or prefix t))))
   
   ;; (key "String" "Help-String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (stringp (nth 1 item))
           (ergoemacs-keymapp (nthcdr 2 item))))
    ;; Install prefix as sparse/full keymap
    (if (ergoemacs-map-all-sparse-p (nthcdr 2 item))
        (ergoemacs-mapkeymap--define-key key
                                         `(,(nth 0 item) ,(nth 1 item)
                                           ,@(make-sparse-keymap)) prefix)
      (ergoemacs-mapkeymap--define-key key
                                       `(,(nth 0 item) ,(nth 1 item) ,@(make-keymap)) prefix))
    (ergoemacs-mapkeymap--key-keymap key (nthcdr 2 item) function submaps prefix))
   
   ;; (key "String" keymap)
   ((ignore-errors
      (and (consp item) (stringp (nth 0 item))
           (ergoemacs-keymapp (nthcdr 1 item))))
    ;; Install prefix as sparse/full keymap
    (if (ergoemacs-map-all-sparse-p (nthcdr 1 item))
        (ergoemacs-mapkeymap--define-key key
                                         `(,(nth 0 item) ,@(make-sparse-keymap)) prefix)
      (ergoemacs-mapkeymap--define-key key
                                       `(,(nth 0 item) ,@(make-keymap)) prefix))
    (ergoemacs-mapkeymap--key-keymap key (nthcdr 1 item) function submaps prefix))
   
   ;; (key keymap)
   ((ergoemacs-keymapp item)
    ;; Prefix Key
    (if (ergoemacs-map-all-sparse-p item)
        (ergoemacs-mapkeymap--define-key
         key (make-sparse-keymap) prefix)
      (ergoemacs-mapkeymap--define-key
       key (make-keymap) prefix))
    (ergoemacs-mapkeymap--key-keymap key item function submaps prefix))
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
      (funcall function key item (or prefix t))))
   
   (t
    (warn "Could not extract\n\tkey:\"%s\" \n\tItem: \"%s\"" key item))))

(declare-function ergoemacs-real-define-key "ergoemacs-map.el" (keymap key def) t)
(fset 'ergoemacs-real-define-key (symbol-function 'define-key))

(defun ergoemacs-map-force-full-keymap (keymap)
  "Forces KEYMAP to be a full keymap."
  (if (ignore-errors (char-table-p (nth 1 keymap))) keymap
    (ergoemacs-setcdr keymap (cons (nth 1 (make-keymap)) (cdr keymap)))
    keymap))

(defun ergoemacs-mapkeymap--define-key (key item &optional prefix)
  "Defines KEY to be ITEM for ergoemacs-mapkeymap--current.
KEY could be a cons for a range if the keymap is a full keymap, otherwise KEY is a vector."
  (cond
   ((vectorp key)
    (ergoemacs-real-define-key
     ergoemacs-mapkeymap--current key item))
   ((consp key) ;; Char table range.
    (set-char-table-range
     (nth 1 (ergoemacs-map-force-full-keymap
             (if prefix (lookup-key ergoemacs-mapkeymap--current prefix)
               ergoemacs-mapkeymap--current))) key item))))

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
    (ignore-errors (plist-get (ergoemacs-map-plist keymap) :map-key)))
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
    (if ergoemacs-mapkeymap--current
        (error "Cannot call `ergoemacs-mapkeymap' while another `ergoemacs-mapkeymap' is being called.\n%s" ergoemacs-mapkeymap--current)
      (unwind-protect
          (progn
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
            (setq sub ergoemacs-mapkeymap--submaps)
            (setq ret ergoemacs-mapkeymap--current)
            (setq ergoemacs-mapkeymap--current nil
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
                  (setq tmp2 (ergoemacs-map-get (cdr item) :submap-p))
                  (setq tmp3 (cons (car item) (ergoemacs-map--key keymap)))
                  (unless (member tmp3 tmp2)
                    (push tmp3 tmp2))
                  (ergoemacs-map-put (ergoemacs-map-keymap-value (cdr item)) :submap-p tmp2)
                  (push (cdr item) tmp1)))))
        (setq ergoemacs-mapkeymap--current nil
              ergoemacs-mapkeymap--nil nil
              ergoemacs-mapkeymap--prefixes nil
              ergoemacs-mapkeymap--submaps nil
              ergoemacs-mapkeymap--key nil)))
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
            k2 (lookup-key global-map key t))
      (setq ret (not (or (eq k1 k2) (and (keymapp k1) (keymapp k2)))))
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

(defun ergoemacs-default-global--gen ()
  "Generates hash for default emacs maps."
  ;; (setq ergoemacs-map-plist-hash (make-hash-table :test 'equal))
  (ergoemacs-map--label-atoms)
  (with-temp-file (ergoemacs-default-global--file) 
    (let ((print-level nil)
          (print-length nil)
          keys
          tmp)
      (goto-char (point-min))
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
      (maphash
       (lambda(key item)
         (message "%s->%s" key (gethash ':map-list item))
         (when (and (hash-table-p item)
                    (setq tmp (gethash :map-list item)))
           (insert (format "(when (boundp '%s) (ergoemacs-map--label %s %s))"
                           (car tmp) (car tmp) (plist-get key :map-key)))))
       ergoemacs-map-plist-hash))))

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
       ((and key (equal key [keymap]) (ignore-errors (keymapp item)))
        (ergoemacs-flatten-composed-keymap--define-key item parent pre-vector))
       (t
        ;; (message "This: %s %s %s" pre-vector key item)
        )))))

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
  "Flattens a composed KEYMAP.
If it is not a composed KEYMAP, return the keymap as is.

This will also install
`ergoemacs-shortcut-movement-force-shift-select' when
FORCE-SHIFTED is non-nil."
  (if (not (ignore-errors (and (keymapp keymap) (eq (nth 0 (nth 1 keymap)) 'keymap)))) keymap
    (let* (new-keymap
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
        (ergoemacs-flatten-composed-keymap--force-shifted new-keymap))
      (ergoemacs-setcdr keymap (cdr new-keymap))
      keymap)))

;; (defun ergoemacs-flatten-composed-keymap (keymap &optional force-shifted)
;;   "Flattens a composed KEYMAP."
;;   (if (not (ergoemacs-map-composed-p keymap))
;;       (if force-shifted
;;           (ergoemacs-flatten-composed-keymap--force-shifted keymap)
;;         keymap)
;;     (let ((parent (keymap-parent keymap))
;;           new-keymap)
;;       (unwind-protect
;;           (progn
;;             (set-keymap-parent keymap nil)
;;             (setq new-keymap (ergoemacs-mapkeymap nil keymap))
;;             (set-keymap-parent keymap parent))
;;         (when force-shifted
;;           (ergoemacs-flatten-composed-keymap--force-shifted new-keymap))
;;         (ergoemacs-setcdr keymap (cdr new-keymap))
;;         new-keymap))))


;; (defun ergoemacs-flatten-composed-keymap (keymap &optional force-shifted)
;;   (if (not (ergoemacs-map-composed-p keymap))
;;       (if force-shifted
;;           (ergoemacs-flatten-composed-keymap--force-shifted keymap)
;;         keymap)
;;     (let ((parent (keymap-parent keymap))
;;           (new-keymap 
;;            (apply 'append
;;                   (mapcar
;;                    (lambda(map)
;;                      (ergoemacs-flatten-composed-keymap map force-shifted))
;;                    (ergoemacs-map-composed-list keymap)))))
;;       (setq new-keymap (append new-keymap parent))
;;       (when force-shifted
;;         (ergoemacs-flatten-composed-keymap--force-shifted new-keymap))
;;       (ergoemacs-setcdr keymap (cdr new-keymap))
;;       new-keymap)))


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




(defun ergoemacs-map--label-atoms (&rest _ignore)
  "Label all the bound keymaps."
  (mapatoms
   (lambda(map)
     (unless (get map :ergoemacs-labeled)
       (let* ((sv (ergoemacs-sv map t))
              omap
              (key (ergoemacs-map--key sv))
              ret)
         (when (ergoemacs-keymapp sv)
           (ergoemacs-map--label sv key)
           (setq ret (ergoemacs-map-get sv :map-list)
                 omap (gethash key ergoemacs-original-map-hash))
           (pushnew map ret)
           ;; Hash should be a copy pointers of the original maps.
           (puthash key (or omap (copy-keymap sv)) ergoemacs-original-map-hash)
           (ergoemacs-map-put sv :map-list ret)
           ;; (message "%s->%s;%s" key ret (ergoemacs-map-get sv :map-list))
           ))
       (unless noninteractive
        (put map :ergoemacs-labeled t))))))


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
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed."
  (let* ((map-key (ergoemacs-map--key keymap))
         (ret (gethash map-key ergoemacs-original-map-hash)))
    (unless ret
      (ergoemacs-map--label keymap map-key)
      (puthash map-key (copy-keymap (ergoemacs-map-keymap-value keymap)) ergoemacs-original-map-hash)
      (setq ret (gethash map-key ergoemacs-original-map-hash)))
    ret))

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



;; (defstruct ergoemacs-map
;;   (map (make-sparse-keymap))
;;   (just-first-keys nil :read-only t)
;;   (variable-modifiers '(meta) :read-only t)
;;   (variable-prefixes '([apps] [menu]) :read-only)
;;   (layout-from "us" :read-only t)
;;   (relative-to 'global-map))

;; (defun ergoemacs-kbd (key &optional just-translate only-first)
;;   "Translates kbd code KEY for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'.
;; If JUST-TRANSLATE is non-nil, just return the KBD code, not the actual emacs key sequence."
;;   (let ((kbd (or (and (vectorp key) key)
;;                  (and (stringp key) (read-kbd-macro key t))))
;;         just-first-keys)
;;     (if (not kbd) nil
;;       (cond
;;        ((and only-first
;;              (> (length kbd) 2)
;;              (memq (aref kbd 0) '(menu apps)))
;;         (setq just-first-keys (substring kbd 0 2)))
;;        ((and only-first
;;              (> (length kbd) 1))
;;         (setq just-first-keys (substring kbd 0 1))))
;;       (setq kbd (ergoemacs-kbd-translate kbd just-first-keys t t ergoemacs-translation-to ergoemacs-translation-from))
;;       (when just-translate
;;         (setq kbd (key-description kbd))))
;;     kbd
;;     ))


(defun ergoemacs-map--label-after-startup ()
  "Labels all maps after startup. Also label maps after everything has loaded."
  (ergoemacs-map--label-atoms)
  (add-hook 'after-load-functions 'ergoemacs-map--label-atoms))
(add-hook 'after-init-hook 'ergoemacs-map--label-after-startup)

(unless init-file-user
  (ergoemacs-map--label-after-startup))

(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
