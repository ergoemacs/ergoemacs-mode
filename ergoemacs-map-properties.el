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

(defvar ergoemacs-dir)
(defvar ergoemacs-map-properties--global-map-before-ergoemacs)
(defvar ergoemacs-ignore-prev-global)
(defvar ergoemacs-remap-ignore)

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-setcdr "ergoemacs-lib")

(defvar ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal))

(defun ergoemacs-map-properties--keymap-value (keymap &rest _ignore)
  "Return the keymap value of KEYMAP.
KEYMAP can be a symbol, keymap or ergoemacs-mode keymap"
  (let (tmp)
    (or (and (listp keymap) (ergoemacs-keymapp keymap) keymap)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-value keymap))) tmp)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-function keymap))) tmp)
        ;; (ignore-errors (and (setq tmp (gethash keymap ergoemacs-map-properties--plist-hash))
        ;;                     (setq tmp (gethash :map-list tmp))
        ;;                     (symbol-value (car tmp))))
        ;; (ignore-errors (and (setq tmp (plist-get keymap :map-list)) (symbol-value (nth 0 tmp))))
        )))

(defun ergoemacs-map-properties--composed-p (keymap &rest _ignore)
  "Determine if the KEYMAP is a composed keymap."
  (and (ignore-errors (eq 'keymap (car keymap)))
       (ignore-errors (eq 'keymap (caadr keymap)))))

;; FIXME: Write test or function
(defun ergoemacs-map-properties--all-sparse-p (keymap &rest _ignore)
  "Determines if all components of a KEYMAP are sparse keymaps.
This does not include submaps, which may also be a full keymap."
  (if (not (ergoemacs-keymapp keymap)) t
    (let ((ret t)
          (kv (ergoemacs-map-properties--keymap-value keymap)))
      (cond
       ((ergoemacs-map-properties--composed-p kv)
        (setq ret
              (catch 'found-full
                (dolist (map (ergoemacs-map-properties--composed-list keymap))
                  (unless (ergoemacs-map-properties--all-sparse-p map)
                    (throw 'found-full nil))) t)))
       (t
        (setq ret (not (ignore-errors (char-table-p (nth 1 kv)))))))
      (when ret
        (setq ret (ergoemacs-map-properties--all-sparse-p (keymap-parent keymap))))
      ret)))

(defun ergoemacs-map-properties--composed-list (keymap &optional melt label)
  "Return the list of maps in a composed KEYMAP.
If there are no maps, return nil.
When MELT is true, combine all the keymaps (with the exception of the parent-map)"
  (if (not (ergoemacs-map-properties--composed-p keymap)) nil
    (let ((parent (keymap-parent keymap))
          ret)
      (unwind-protect
          (progn
            (when parent
              (set-keymap-parent keymap nil))
            (dolist (map (reverse (cdr keymap)))
              (when label
                (ergoemacs :label map))
              (if melt
                  (setq ret (append (cdr map) ret))
                (push (cons (car map) (cdr map)) ret))))
        (when parent
          (set-keymap-parent keymap parent))
        (when melt
          (setq ret (append '(keymap) ret))))
      ret)))

(defun ergoemacs-map-properties--current-local-map-p (keymap &rest _ignore)
  "Determines if the KEYMAP is installed in `current-local-map'."
  ;; Make sure they both have labels
  (if (or (not (current-local-map))
          (not (ergoemacs-keymapp keymap))) nil
    (ergoemacs keymap :label)
    (ergoemacs (current-local-map) :label)
    (let ((local-map-key (ergoemacs (current-local-map) :map-key))
          (map-key (ergoemacs keymap :map-key)))
      (when (consp local-map-key)
        (setq local-map-key (plist-get (car local-map-key) :map-key)))
      (when (consp map-key)
        (setq map-key (plist-get (car map-key) :map-key)))
      (= map-key local-map-key))))

(defun ergoemacs-map-properties--composed (keymap &optional force)
  "Returns a list of `ergoemacs-mode' map-key for the composed keymap list"
  (let ((composed-list (ergoemacs-map-properties--composed-list keymap nil force)))
    (and composed-list
         (catch 'not-bound
           (mapcar
            (lambda(comp)
              (let ((ret (ergoemacs-map-properties--key-struct comp)))
                (when (and (not force) (not ret))
                  (throw 'not-bound nil))
                ret)) composed-list)))))

(defvar ergoemacs-map-properties--key-struct (make-hash-table)
  "Key struct hash table.")
(defun ergoemacs-map-properties--key-struct (keymap &optional force)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map.

:map-key is the key of the current map.
:composed is a list of the `ergoemacs-map-properties--key-struct' of each of the composed maps.
:parent is the `ergoemacs-map-properties--key-struct' of the current map.

This will return the keymap structure prior to `ergoemacs-mode' modifications
"
  ;;|-----------+------------+--------------+--------------|
  ;;| Condition | Call Count | Elapsed Time | Average Time |
  ;;|-----------+------------+--------------+--------------|
  ;;| Pre Hash  |     237982 | 100.52800000 | 0.0004224185 |
  ;;| Post Hash |     150045 | 40.600999999 | 0.0002705921 |
  ;;|-----------+------------+--------------+--------------|
  (let* ((keymap (ergoemacs-map-properties--keymap-value keymap))
         (map-key (ergoemacs keymap :map-key))
         (composed (ergoemacs-map-properties--composed keymap force))
         parent
         (hash-key (or (and (not composed) (integerp map-key) map-key)
                       (and composed (not (consp map-key)) (cdr keymap))))
         (ret (or (and (consp map-key) (car map-key))
                  (and hash-key (gethash hash-key ergoemacs-map-properties--key-struct)))))
    (unless ret
      (when (and force (not (or map-key composed)))
        (ergoemacs :label keymap)
        (setq map-key (ergoemacs keymap :map-key)
              composed (ergoemacs-map-properties--composed keymap)
              parent (ergoemacs-map-properties--parent keymap)))
      (when map-key
        (setq ret (plist-put ret :map-key map-key)))
      (when composed
        (setq ret (plist-put ret :composed composed)))
      (when (or map-key composed)
        (setq parent (ergoemacs-map-properties--parent keymap t))
        (when parent
          (setq ret (plist-put ret :parent parent))))
      (puthash hash-key ret ergoemacs-map-properties--key-struct))    
    ret))


(defun ergoemacs-map-properties--key-hash (keymap &optional force)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map.

:map-key is the key of the current map.
:composed is a list of the `ergoemacs-map-properties--key-struct' of each of the composed maps.
:parent is the `ergoemacs-map-properties--key-struct' of the current map.

This will return the keymap structure prior to `ergoemacs-mode' modifications
"
  ;;|-----------+------------+--------------+--------------|
  ;;| Condition | Call Count | Elapsed Time | Average Time |
  ;;|-----------+------------+--------------+--------------|
  ;;| Pre Hash  |     237982 | 100.52800000 | 0.0004224185 |
  ;;| Post Hash |     150045 | 40.600999999 | 0.0002705921 |
  ;;| Hash Key  |      76379 | 15.183000000 | 0.0001987850 |
  ;;|-----------+------------+--------------+--------------|
  (when (ergoemacs-keymapp keymap)
    (let* ((keymap (ergoemacs-map-properties--keymap-value keymap))
           (map-key (ergoemacs keymap :map-key))
           (composed (ergoemacs-map-properties--composed-list keymap force))
           (parent (and composed (keymap-parent keymap)))
           (ret (or (and (consp map-key) (car map-key))
                    (and composed
                         (append
                          (mapcar
                           (lambda(map)
                             (ergoemacs map :map-key))
                           composed)
                          (list (and parent (ergoemacs parent :map-key)))))
                    (and (integerp map-key) (list map-key)))))
      ret)))

(defun ergoemacs-map-properties--default-global-file ()
  "What is the global key hash file."
  (let* ((file (expand-file-name (format "ergoemacs-global-%s-%s.el" emacs-version system-configuration)
                                 ergoemacs-dir))
         (extras (expand-file-name "ergoemacs-extras" user-emacs-directory))
         (file2 (expand-file-name (format "ergoemacs-global-%s-%s.el" emacs-version system-configuration)
                                  extras)))
    (or
     (and (file-readable-p file2) file2)
     (and (file-readable-p file) file)
     (and (file-writable-p file) file)
     file2)))

(defvar ergoemacs-map-properties--label-atoms-maps nil
  "Known bound keymaps")

(defvar ergoemacs-map-properties--const-keymaps nil
  "Variable listing constant keymaps.")

(defun ergoemacs-map-properties--default-global-gen ()
  "Generates hash for default emacs maps."
  ;; (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal))
  (ergoemacs global-map :label most-negative-fixnum) ;; Should be `most-negative-fixnum'
  (ergoemacs-map-properties--label-atoms)
  ;; Pre-calculate map-lists 
  (dolist (map ergoemacs-map-properties--label-atoms-maps)
    (ergoemacs (ergoemacs-sv map) :map-list))
  (with-temp-file (ergoemacs-map-properties--default-global-file) 
    (let ((print-level nil)
          (print-length nil)
          (where-is-hash (make-hash-table))
          tmp
          keys)
      (goto-char (point-min))
      (insert "(defvar ergoemacs-map-properties--plist-hash)(declare-function ergoemacs-map-properties--label \"ergoemacs-map-properties\")")
      (ergoemacs-map-keymap
       (lambda (key item)
         (cond
          ((vectorp key)
           (push key keys)
           (if (setq tmp (gethash item where-is-hash))
               (push key tmp)
             (puthash item (list key) where-is-hash)))))
       global-map)
      (ergoemacs :label global-map)
      (ergoemacs :keys global-map) ;; Should calculate :where-is and :lookup from original map
      
      (insert "(setq ergoemacs-map-properties--plist-hash '")
      (prin1 ergoemacs-map-properties--plist-hash (current-buffer))
      (goto-char (point-max))
      (insert ")")
      
      
      (message "global-map-list %s" (ergoemacs global-map :map-list))
      (dolist (map ergoemacs-map-properties--label-atoms-maps)
        (when (ergoemacs-map-properties--key-struct map)
          (insert (format "(when (boundp '%s) (ergoemacs-map-properties--label %s %s))"
                          map map (ergoemacs (ergoemacs-sv map) :map-key))))))))

(defun ergoemacs-map-properties--protect-global-map ()
  "Protects global map by adding a user-key layer to it"
  (when (and (not noninteractive) (integerp (ergoemacs global-map :map-key)))
    (let ((map (ergoemacs global-map :user)))
      (setq global-map (make-composed-keymap map global-map)))))

(defun ergoemacs-map-properties--get-original-global-map ()
  "Loads/Creates the default global map information."
  (ergoemacs-map-properties--label-atoms)
  (if (file-readable-p (ergoemacs-map-properties--default-global-file))
      (progn
        (load (ergoemacs-map-properties--default-global-file))
        (ergoemacs-map-properties--protect-global-map))
    (if noninteractive
        (warn "Could not find global map information")
      (let* ((emacs-exe (ergoemacs-emacs-exe))
             (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
             (cmd (format "%s -L %s --batch --load \"ergoemacs-mode\" -Q --eval \"(ergoemacs-map-properties--default-global-gen) (kill-emacs)\"" emacs-exe default-directory)))
        (message "%s" (shell-command-to-string cmd))
        (ergoemacs-map-properties--get-original-global-map)))))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-map-properties--get-original-global-map)

(defvar ergoemacs-map-properties--indirect-keymaps (make-hash-table)
  "Variable listing indirect keymaps.")

(defun ergoemacs-map-properties--map-fixed-plist (keymap &rest _ignore)
  "Determines if this is an `ergoemacs-mode' KEYMAP.
Returns a plist of fixed keymap properties (not changed by
composing or parent/child relationships)"
  (if (not (ergoemacs-keymapp keymap) ) nil
    (if (ignore-errors (symbol-function keymap))
        (progn (gethash keymap ergoemacs-map-properties--indirect-keymaps))
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
                (when (char-table-p (car (cdr map)))
                  ;; Drop any full keymap labels
                  (setq map `(keymap ,@(cdr (cdr map)))))
                (setq parent (keymap-parent map))
                (ignore-errors (set-keymap-parent map nil))
                (setq ret (lookup-key map [ergoemacs-labeled]))
                (when ret
                  (setq ret (ignore-errors (funcall ret)))))
            (ignore-errors (set-keymap-parent map parent))))
        (if ret ret
          ;; Now get properties for constant/indirect keymaps
          (catch 'found-map
            (dolist (map ergoemacs-map-properties--const-keymaps)
              (when (eq (cdr map) (cdr keymap))
                (setq ret (car map))
                (throw 'found-map t))))
          ret)))))

(defun ergoemacs-map-properties--put (keymap property value)
  "Set ergoemacs-mode KEYMAP PROPERTY to VALUE."
  (prog1 value
    (if (eq property :label)
        (ergoemacs :label keymap value)
      (let ((keymap (ergoemacs-map-properties--keymap-value keymap)))
        (cond
         ((not (ergoemacs-keymapp keymap))
          (warn "Trying to put keymap property on non-keymap %s." keymap))
         ((eq property :full)
          (warn "Cannot set the keymap property :full"))
         (t (let ((ret (ergoemacs-map-properties--map-fixed-plist keymap)) tmp)
              (if (and ret (eq property ':map-key))
                  (progn
                    (setq ret (plist-put ret property value))
                    (ergoemacs :label keymap value))
                (unless (hash-table-p ergoemacs-map-properties--plist-hash)
                  (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal)))
                (setq tmp (gethash (ergoemacs-map-properties--key-struct keymap) ergoemacs-map-properties--plist-hash))
                (unless (hash-table-p tmp)
                  (setq tmp (make-hash-table)))
                (puthash property value tmp)
                (puthash (ergoemacs-map-properties--key-struct keymap) tmp ergoemacs-map-properties--plist-hash)))))))))

(defun ergoemacs-map-properties--parent (keymap &optional force)
  "Returns a `ergoemacs-mode' map-key for the parent of KEYMAP."
  (let ((parent (keymap-parent keymap)))
    (and parent (ergoemacs-map-properties--key-struct parent force))))

(defun ergoemacs-map-properties--map-list (keymap &optional no-hash)
  "Get the list of maps bound to KEYMAP.
KEYMAP can be a keymap or keymap integer key."
  (if (ergoemacs-keymapp keymap)
      (let* (ret
             (keymap (ergoemacs keymap :original))
             (map-p (ergoemacs keymap :key-hash)))
        (cond
         ((and map-p (not no-hash) (setq ret (ergoemacs keymap :map-list-hash)))
          (ergoemacs-map-properties--get-or-generate-map-key keymap)
          (setq map-p ret)
          (setq ret nil)
          (dolist (map map-p)
            (when (eq keymap (ergoemacs (ergoemacs-sv map) :original))
              (push map ret)))
          (when (not ret);; Check again...
            (setq ret (ergoemacs-map-properties--map-list keymap t)))
          ret)
         (map-p
          (dolist (map ergoemacs-map-properties--label-atoms-maps)
            (when (eq keymap (ergoemacs (ergoemacs-sv map) :original))
              (push map ret)))
          (ergoemacs-map-properties--put keymap :map-list-hash ret)
          ret)))))

(defun ergoemacs-map-properties--label-map (map)
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
        (let (key)
          (setq key (ergoemacs-map-properties--get-or-generate-map-key sv))
          (ergoemacs :label sv key)))
      (pushnew map ergoemacs-map-properties--label-atoms-maps)
      (put map :ergoemacs-labeled t)
      t))))

(defun ergoemacs-map-properties--label-atoms (&rest _ignore)
  "Label all the bound keymaps."
  (mapatoms #'ergoemacs-map-properties--label-map))

(defvar ergoemacs-map-properties--unlabeled nil
  "A list of unlabeled keymaps.")

(defun ergoemacs-map-properties--label-unlabeled (&rest _ignore)
  "Label known but unlabeled keymaps."
  (let (new)
    (dolist (map ergoemacs-map-properties--unlabeled)
      (unless (ergoemacs-map-properties--label-map map)
        (push map new)))
    (setq ergoemacs-map-properties--unlabeled new)))

;; Startup and load functions
(add-hook 'ergoemacs-mode-after-init-emacs 'ergoemacs-map-properties--label-unlabeled)
(add-hook 'ergoemacs-mode-after-load-hook 'ergoemacs-map-properties--label-unlabeled)

(defvar ergoemacs-map-properties--get-or-generate-map-key most-negative-fixnum)
(defun ergoemacs-map-properties--get-or-generate-map-key (keymap &rest _ignore)
  "Gets the key for the KEYMAP."
  (let ((ret (ergoemacs-map-properties--map-fixed-plist (ergoemacs-map-properties--keymap-value keymap))))
    (or (and ret (plist-get ret :map-key))
        (setq ergoemacs-map-properties--get-or-generate-map-key
              (+ 1 ergoemacs-map-properties--get-or-generate-map-key)))))

(defun ergoemacs-map-properties--label (keymap &optional map-key)
  "Label an `ergoemacs-mode' touched keymap.
MAP-NAME is the identifier of the map name.
The KEYMAP will have the structure
  (keymap optional-char-table \"Optional Label\" (ergoemacs-labeled (lambda nil (plist-of-properties))) true-map)
"
  (if (not (ergoemacs-keymapp keymap)) nil
    (if (ergoemacs-map-properties--composed-p keymap)
        (cond
         (map-key
          (warn "Will not label a composed map's members to %s" map-key))
         (t
          (dolist (map (ergoemacs-map-properties--composed-list keymap))
            (ergoemacs-map-properties--label map map-key))))
      (let* ((map keymap)
             (map-key (or map-key (ergoemacs-map-properties--get-or-generate-map-key map)))
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
                        old-plist (gethash keymap ergoemacs-map-properties--indirect-keymaps))
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
            (puthash keymap old-plist ergoemacs-map-properties--indirect-keymaps)
          (unless (ignore-errors (ergoemacs-setcdr keymap (cdr map)))
            (pushnew (cons old-plist (cdr keymap)) ergoemacs-map-properties--const-keymaps)))
        map))))

(defun ergoemacs-map-properties--empty-p (keymap &rest _ignore)
  "Determines if a KEYMAP is empty."
  (catch 'found-key
    (ergoemacs-map-keymap
     (lambda (cur-key item)
       (unless (equal cur-key [ergoemacs-labeled])
         (if (consp cur-key)
             (throw 'found-key nil)
           (unless (eq item 'ergoemacs-prefix) 
             (when item
               (throw 'found-key nil))))))
     keymap) t))

;;ergoemacs-map-properties--label


(defvar ergoemacs-map-properties--user-map-hash (make-hash-table :test 'equal)
  "Hash table of the user maps that `ergoemacs-mode' saves.")

(defun ergoemacs-map-properties--user (keymap &rest _ignore)
  "Gets the user KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-properties--key-struct' of the keymap as well."
  (let ((key (ergoemacs keymap :map-key))
        map)
    (when (integerp key)
      (setq map (gethash (setq key (ergoemacs keymap :key-hash)) ergoemacs-map-properties--user-map-hash))
      (unless map
        (puthash key (make-sparse-keymap) ergoemacs-map-properties--user-map-hash)
        (setq map (gethash key ergoemacs-map-properties--user-map-hash))
        (ergoemacs map :label (list (ergoemacs keymap :key-hash) 'user))))
    map))

(defun ergoemacs-map-properties--calculate-keys-and-where-is-hash (keymap &rest _ignore)
  "Calculates :where-is and :keys properties for KEYMAP."
  (let ((where-is-hash (make-hash-table))
        (lookup-hash (make-hash-table :test 'equal))
        keys tmp)
    (ergoemacs-map-keymap
     (lambda (key item)
       (unless (and (vectorp key) (eq (elt key (- (length key) 1)) 'ergoemacs-labeled))
         (cond
          ((and (vectorp key)
                (commandp item t))
           (push key keys)
           (if (setq tmp (gethash item where-is-hash))
               (push key tmp)
             (puthash item (list key) where-is-hash))
           (puthash key item lookup-hash)))))
     keymap)
    (ergoemacs keymap :extract-keys keys)
    (ergoemacs keymap :extract-where-is where-is-hash)
    (ergoemacs keymap :extract-lookup lookup-hash)))

(defun ergoemacs-map-properties--keys (keymap &rest _ignore)
  "Extract :keys property for KEYMAP."
  (let ((ret (ergoemacs keymap :extract-keys)))
    (unless ret
      (ergoemacs-map-properties--calculate-keys-and-where-is-hash keymap)
      (setq ret (ergoemacs keymap :extract-keys)))
    ret))

(defun ergoemacs-map-properties--where-is (keymap &rest _ignore)
  "Extract :where-is property for KEYMAP."
  (let ((ret (ergoemacs keymap :extract-where-is)))
    (unless ret
      (ergoemacs-map-properties--calculate-keys-and-where-is-hash keymap)
      (setq ret (ergoemacs keymap :extract-where-is)))
    ret))

(defun ergoemacs-map-properties--lookup (keymap &rest _ignore)
  "Extract :lookup property for KEYMAP."
  (let ((ret (ergoemacs keymap :extract-lookup)))
    (unless ret
      (ergoemacs-map-properties--calculate-keys-and-where-is-hash keymap)
      (setq ret (ergoemacs keymap :extract-lookup)))
    ret))

(defun ergoemacs-map-properties--new-command (keymap command &optional relative-map)
  "Get the COMMAND equivalent binding in KEYMAP based on RELATIVE-MAP."
  (and command keymap
       (let* (ret
              (hash-table (ergoemacs (or relative-map global-map) :where-is))
              (cmd-list (gethash command hash-table))
              
              ;; (lookup (ergoemacs keymap :lookup))
              )
         ;; (cond
         ;;  ((equal cmd-list '([]))
         ;;   (setq cmd-list nil))
         ;;  (cmd-list)
         ;;  (t
         ;;   (setq cmd-list (setq cmd-list (where-is-internal 'save-buffer (ergoemacs :original global-map))))
         ;;   (if cmd-list
         ;;       (puthash command cmd-list hash-table)
         ;;     (puthash command '([]) hash-table))))
         (if (not cmd-list) nil
           (catch 'found-new
             (dolist (key cmd-list)
               (when (and (setq ret (lookup-key keymap key t))
                          (or (and (commandp ret t) (not (memq ret ergoemacs-remap-ignore)))
                              (and (integerp ret) (setq ret nil))))
                 (throw 'found-new t))
               (setq ret nil))
             t)
           ret))))

(defun ergoemacs-map-properties--original (keymap &rest _ignore)
  "Gets the original keymap."
  (let ((ret keymap))
    (while (and (or (and (not (ergoemacs ret :map-key)) (ergoemacs ret :label)) t) ;; Apply label if needed.
                (not (integerp (ergoemacs ret :map-key)))
                (setq ret (keymap-parent ret)))
      t)
    ret))

(defun ergoemacs-map-properties--original-user (keymap &rest _ignore)
  "Gets the original keymap with the user protecting layer."
  (make-composed-keymap (ergoemacs keymap :user) (ergoemacs keymap :original)))

(defun ergoemacs-map-properties--installed-p (keymap &rest _ignore)
  "Is `ergoemacs-mode' installed in KEYMAP?
Values returned are:
  :protected-p -- An ergoemacs user keymap is installed on top
  :cond-p -- This is a conditional map (usually found in `minor-mode-map-alist')
  t -- `ergoemacs-mode' has been installed
  nil -- `ergoemacs-mode' has not modified this map.
"
  (when keymap
    (let* ((parent (keymap-parent keymap))
           (key (and parent (ergoemacs keymap :map-key)))
           (ret (and (consp key) 
                     (or (and (eq (nth 1 key) 'user) :protected-p)
                         (and (eq (nth 1 key) 'cond-map) :cond-p)))))
      (cond
       ((eq ret :cond-p) ret)
       ((not ret) nil)
       ((and (setq key (ergoemacs parent :map-key)) (not (consp key))) ret)
       ((eq (nth 1 key) 'ergoemacs-unbound) t)
       ((and (ergoemacs parent :composed-p) (consp key)) t)
       (t ret)))))


(defun ergoemacs-map-properties--sequence (key &rest _ignore)
  "Returns a key sequence from KEY.
This sequence is compatible with `listify-key-sequence'."
  (let (input)
    (cond
     ((not key)) ;; Not specified.
     ((vectorp key) ;; Actual key sequence
      (setq input (listify-key-sequence key)))
     ((consp key) ;; Listified key sequence
      (setq input key))
     ((stringp key) ;; Kbd code
      (setq input (listify-key-sequence (read-kbd-macro key t)))))
    input))

(defun ergoemacs-map-properties--movement-p (command &rest _ignore)
  "Determines if COMMAND is a movement command.
This is done by checking if this is a command that supports shift
selection or cua-mode's movement."
  (let (tmp)
    (prog1 (and (commandp command)
                (or (and (symbolp command) (eq (get command 'CUA) 'move))
                    (setq tmp (let ((intf (ignore-errors (car (cdr (interactive-form command))))))
                                (and intf (stringp intf) (string-match "^[@*]*\\^" intf))))))
      ;; Put 'CUA movement command if it wasn't there.
      (when (and tmp (symbolp command)) 
        (put command 'CUA 'move)))))

(defvar ergoemacs-map-properties--command-loop-functions
  '(ergoemacs-command-loop ergoemacs-read-key)
  "Determine if the function is a command-loop inducing function.")

(defun ergoemacs-map-properties--command-loop-p (command &rest _ignore)
  "Determines if COMMAND induces the `ergoemacs-mode' command loop."
  (memq command ergoemacs-map-properties--command-loop-functions))

(provide 'ergoemacs-map-properties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map-properties.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
