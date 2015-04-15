;;; ergoemacs-map-properties.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-map-properties.el
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
  (require 'cl)
  (require 'ergoemacs-macros))

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

(defun ergoemacs-map-get (keymap &optional property)
  "Gets ergoemacs-mode KEYMAP PROPERTY.
When PROPERTY is nil, call `ergoemacs-map' for KEYMAP"
  (cond
   ;; ((not property)
   ;;  (ergoemacs-map keymap))
   ((eq property :full)
    (ignore-errors (char-table-p (nth 1 (ergoemacs-map-keymap-value keymap)))))
   ((eq property :indirect)
    (ergoemacs-keymapp (symbol-function keymap)))
   ((or (eq property :map-key) (eq property :key))
    ;; FIXME Expire any ids that are no longer linked
    (ignore-errors (plist-get (ergoemacs-map-plist keymap) :map-key)))
   ((eq property :map-list)
    (ergoemacs-map--map-list keymap))
   ((eq property :prefixes)
    (ergoemacs-extract-prefixes keymap nil t))
   (t
    (ignore-errors
      (gethash property (gethash (ergoemacs-map-p (ergoemacs-map-keymap-value keymap)) ergoemacs-map-plist-hash))))))



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

(defvar ergoemacs-map--const-keymaps nil
  "Variable listing constant keymaps.")

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

(defun ergoemacs-map--original (keymap &optional setcdr-p)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-p' of the keymap as well."
  (let (map-key ret)
    (ergoemacs-get-map--setcdr
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
       ret)) keymap setcdr-p)))

;; Startup and load functions

(defun ergoemacs-map--label-after-startup ()
  "Labels known unlabeled maps after startup. Also label maps after everything has loaded."
  (ergoemacs-map--label-unlabeled)
  (add-hook 'after-load-functions 'ergoemacs-map--label-unlabeled))
(add-hook 'after-init-hook 'ergoemacs-map--label-after-startup)


(unless init-file-user
  (run-with-idle-timer 0.05 nil 'ergoemacs-map--label-after-startup))


(provide 'ergoemacs-map-properties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map-properties.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
