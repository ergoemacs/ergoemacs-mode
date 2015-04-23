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
(defvar ergoemacs-map-properties--original-global-map)
(defvar ergoemacs-map-properties--global-map-before-ergoemacs)
(defvar ergoemacs-ignore-prev-global)

(declare-function ergoemacs-mapkeymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-setcdr "ergoemacs-lib")

(defvar ergoemacs-map-properties--ignored-prefixes '(;; "C-h" "<f1>"
                                     [27]  [escape]
                                     [remap]
                                     [left-fringe]
                                     [vertical-line]
                                     [vertical-scroll-bar]
                                     [header-line]
                                     [mode-line]
                                     [menu-bar]
                                     [C-down-mouse-2]))

(defvar ergoemacs-map-properties--original-map-hash (make-hash-table)
  "Hash table of the original maps that `ergoemacs-mode' saves.")

(defvar ergoemacs-map-properties--user-map-hash (make-hash-table)
  "Hash table of the user maps that `ergoemacs-mode' saves.")

(defvar ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal))

(defun ergoemacs-map-properties--keymap-value (keymap)
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

(defun ergoemacs-map-properties--composed-p (keymap)
  "Determine if the KEYMAP is a composed keymap."
  (and (ignore-errors (eq 'keymap (car keymap)))
       (ignore-errors (eq 'keymap (caadr keymap)))))

;; FIXME: Write test or function
(defun ergoemacs-map-properties--all-sparse-p (keymap)
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

(defun ergoemacs-map-properties--key-struct (keymap &optional force)
  "Returns the maps linked to the current map, if it is an `ergoemacs-mode' map.

:map-key is the key of the current map.
:composed is a list of the `ergoemacs-map-properties--key-struct' of each of the composed maps.
:parent is the `ergoemacs-map-properties--key-struct' of the current map
"
  (let* ((keymap (ergoemacs-map-properties--keymap-value keymap))
         (map-key (ergoemacs keymap :map-key))
         (composed (ergoemacs-map-properties--composed keymap force))
         parent ret)
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
    ret))

(defun ergoemacs-map-properties--default-global-file ()
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

(defvar ergoemacs-map-properties--label-atoms-maps nil
  "Known bound keymaps")

(defvar ergoemacs-map-properties--const-keymaps nil
  "Variable listing constant keymaps.")

(defun ergoemacs-map-properties--default-global-gen ()
  "Generates hash for default emacs maps."
  ;; (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal))
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
      (insert "(defvar ergoemacs-map-properties--plist-hash)(defvar ergoemacs-map-properties--original-global-map)(declare-function ergoemacs-map-properties--label \"ergoemacs-map-properties\")")
      (ergoemacs-mapkeymap
       (lambda (key item _prefix)
         (cond
          ((vectorp key)
           (push key keys)
           (if (setq tmp (gethash item where-is-hash))
               (push key tmp)
             (puthash item (list key) where-is-hash)))))
       global-map)
      (setq ergoemacs-map-properties--original-global-map (copy-keymap (ergoemacs-mapkeymap nil global-map)))
      (ergoemacs :label ergoemacs-map-properties--original-global-map)
      (ergoemacs :keys ergoemacs-map-properties--original-global-map) ;; Should calculate :where-is and :lookup
      ;; (message "Keys: %s\nKey::%s" keys
      ;;          (ergoemacs ergoemacs-map-properties--original-global-map :keys))

      (insert "(setq ergoemacs-map-properties--plist-hash '")
      (prin1 ergoemacs-map-properties--plist-hash (current-buffer))
      (goto-char (point-max))
      (insert ")")
      
      (insert "(setq ergoemacs-map-properties--original-global-map '")
      (prin1 ergoemacs-map-properties--original-global-map (current-buffer))
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

      (message "global-map-list %s" (ergoemacs global-map :map-list))
      (dolist (map ergoemacs-map-properties--label-atoms-maps)
        (when (ergoemacs-map-properties--key-struct map)
          (insert (format "(when (boundp '%s) (ergoemacs-map-properties--label %s %s))"
                          map map (ergoemacs (ergoemacs-sv map) :map-key))))))))

(defun ergoemacs-map-properties--get-original-global-map ()
  "Loads/Creates the default global map information."
  (ergoemacs-map-properties--label-atoms)
  (if (file-readable-p (ergoemacs-map-properties--default-global-file))
      (load (ergoemacs-map-properties--default-global-file))
    (if noninteractive
        (warn "Could not find global map information")
      (let* ((emacs-exe (ergoemacs-emacs-exe))
             (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
             (cmd (format "%s -L %s --batch --load \"ergoemacs-mode\" -Q --eval \"(ergoemacs-map-properties--default-global-gen) (kill-emacs)\"" emacs-exe default-directory)))
        (message "%s" (shell-command-to-string cmd))
        (ergoemacs-map-properties--get-original-global-map)))))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-map-properties--get-original-global-map)

(defun ergoemacs-map-properties--extract-prefixes (keymap &optional dont-ignore return-kbd)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-map-properties--ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-map-properties--ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.
"
  (if (not (ergoemacs-keymapp keymap)) nil
    ;; (ergoemacs-extract-keys keymap)
    (when (not (ergoemacs-map-properties--key-struct keymap))
      (ergoemacs :label keymap))
    (let ((ret (ergoemacs keymap :extract-prefixes)) ret2)
      (unless ret
        (ergoemacs-mapkeymap nil keymap 'prefix)
        (setq ret (ergoemacs keymap :extract-prefixes)))
      (if (and dont-ignore (not return-kbd)) ret
        (dolist (a ret)
          (let ((tmp (key-description a)))
            (when (or dont-ignore (not (member a ergoemacs-map-properties--ignored-prefixes)))
              (if (not return-kbd)
                  (push a ret2)
                (push tmp ret2)))))
        ret2))))

(defvar ergoemacs-map-properties--indirect-keymaps (make-hash-table)
  "Variable listing indirect keymaps.")

(defun ergoemacs-map-properties--map-fixed-plist (keymap)
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
              (puthash (ergoemacs-map-properties--key-struct keymap) tmp ergoemacs-map-properties--plist-hash))))))))

(defun ergoemacs-map-properties--parent (keymap &optional force)
  "Returns a `ergoemacs-mode' map-key for the parent of KEYMAP."
  (let ((parent (keymap-parent keymap)))
    (and parent (ergoemacs-map-properties--key-struct parent force))))

(defun ergoemacs-map-properties--map-list (keymap &optional no-hash)
  "Get the list of maps bound to KEYMAP.
KEYMAP can be a keymap or keymap integer key."
  (let (ret
        (map-p (ergoemacs-map-properties--key-struct keymap))
        map-key)
    (cond
     ((and map-p (not no-hash) (setq ret (ergoemacs keymap :map-list-hash)))
      (setq map-key (ergoemacs-map-properties--get-or-generate-map-key keymap))
      (setq map-p ret)
      (setq ret nil)
      (dolist (map map-p)
        (when (or (eq keymap (ergoemacs-sv map))
                  (= map-key (ergoemacs-map-properties--get-or-generate-map-key (ergoemacs-sv map))))
          (push map ret)))
      (when (not ret);; Check again...
        (setq ret (ergoemacs-map-properties--map-list keymap t)))
      ret)
     ((and map-p (setq map-key (ergoemacs-map-properties--get-or-generate-map-key keymap)) (integerp map-key))
      (setq map-p (ergoemacs-map-properties--get-or-generate-map-key keymap))
      (dolist (map ergoemacs-map-properties--label-atoms-maps)
        (when (or (eq keymap (ergoemacs-sv map))
                  (equal map-p (ergoemacs-map-properties--get-or-generate-map-key map)))
          (push map ret)))
      (ergoemacs-map-properties--put keymap :map-list-hash ret)
      ret)
     ((integerp keymap)
      (dolist (map ergoemacs-map-properties--label-atoms-maps)
        (when (equal keymap (ergoemacs-map-properties--get-or-generate-map-key map))
          (unless map-p
            (setq map-p map))
          (push map ret)))
      (ergoemacs-map-properties--put map-p :map-list-hash ret)
      ret)
     ((and (or map-key (consp keymap))
           (setq ret (or map-key (plist-get keymap :map-key)))
           (integerp ret))
      (ergoemacs-map-properties--map-list ret))
     ((and ret (consp ret) (ignore-errors (setq ret (car (car ret)))))
      (ergoemacs-map-properties--map-list ret)))))

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
        (let (omap key)
          (setq key (ergoemacs-map-properties--get-or-generate-map-key sv))
          (ergoemacs :label sv key)
          (setq omap (gethash key ergoemacs-map-properties--original-map-hash))
          ;; Hash should be a copy pointers of the original maps.
          (unless omap
            (puthash key (copy-keymap sv) ergoemacs-map-properties--original-map-hash))))
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

(defun ergoemacs-map-properties--get-or-generate-map-key (keymap)
  "Gets the key for the KEYMAP."
  (let ((ret (ergoemacs-map-properties--map-fixed-plist (ergoemacs-map-properties--keymap-value keymap))))
    (or (and ret (plist-get ret :map-key)) (random))))

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

(defun ergoemacs-map-properties--empty-p (keymap)
  "Determines if a KEYMAP is empty."
  (catch 'found-key
    (ergoemacs-mapkeymap
     (lambda (cur-key item _prefix)
       (unless (equal cur-key [ergoemacs-labeled])
         (if (consp cur-key)
             (throw 'found-key nil)
           (unless (eq item 'ergoemacs-prefix) 
             (when item
               (throw 'found-key nil))))))
     keymap) t))

;;ergoemacs-map-properties--label
(defun ergoemacs-map-properties--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-properties--key-struct' of the keymap as well."
  (let (map-key ret)
    (cond
     ((and (ergoemacs-keymapp keymap) (setq map-key (ergoemacs-map-properties--get-or-generate-map-key keymap))
           (integerp map-key))
      (setq ret (gethash map-key ergoemacs-map-properties--original-map-hash))
      (if (and (not ret) ;; Don't save empty keymaps as original keymaps...
               (or (equal keymap (make-sparse-keymap))
                   (equal keymap (make-keymap)))) keymap
        (unless ret
          (ergoemacs :label keymap map-key)
          (puthash map-key (copy-keymap (ergoemacs-map-properties--keymap-value keymap)) ergoemacs-map-properties--original-map-hash)
          (setq ret (gethash map-key ergoemacs-map-properties--original-map-hash)))
        ret))
     ((and map-key (ignore-errors (setq map-key (car (car map-key)))))
      (ergoemacs-map-properties--original map-key))
     ((and (consp keymap) (ignore-errors (setq map-key (plist-get keymap :map-key))) (integerp map-key))
      (setq ret (gethash map-key ergoemacs-map-properties--original-map-hash))
      (setq map-key (plist-get keymap :parent))
      (when map-key
        (set-keymap-parent ret (ergoemacs-map-properties--original map-key)))
      ret)
     ((and map-key (consp map-key) (ignore-errors (setq map-key (car (car map-key)))))
      (ergoemacs-map-properties--original map-key))
     ((and (consp keymap) (ignore-errors (setq map-key (plist-get keymap :composed))))
      (setq ret (plist-get keymap :parent))
      (setq ret (make-composed-keymap
                 (mapcar
                  (lambda(map)
                    (ergoemacs-map-properties--original map))
                  map-key)
                 (and ret (ergoemacs-map-properties--original ret))))
      ret))))


(defun ergoemacs-map-properties--user (keymap)
  "Gets the user KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-properties--key-struct' of the keymap as well."
  (let ((key (ergoemacs keymap :map-key))
        map)
    (when (integerp key)
      (setq map (gethash key ergoemacs-map-properties--user-map-hash))
      (unless map
        (puthash key (make-sparse-keymap) ergoemacs-map-properties--user-map-hash)
        (setq map (gethash key ergoemacs-map-properties--user-map-hash))
        (ergoemacs map :label (list (ergoemacs keymap :key-struct) 'user))))
    map))

(defun ergoemacs-map-properties--calculate-keys-and-where-is-hash (keymap)
  "Calculates :where-is and :keys properties for KEYMAP."
  (let ((where-is-hash (make-hash-table))
        (lookup-hash (make-hash-table :test 'equal))
        keys tmp)
    (ergoemacs-mapkeymap
     (lambda (key item _prefix)
       (cond
        ((and (vectorp key)
              (commandp item t))
         (push key keys)
         (if (setq tmp (gethash item where-is-hash))
             (push key tmp)
           (puthash item (list key) where-is-hash))
         (puthash key item lookup-hash))))
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
       (let (ret
             (cmd-list (gethash command (ergoemacs (or relative-map ergoemacs-map-properties--original-global-map) :where-is)))
             ;; (lookup (ergoemacs keymap :lookup))
             )
         (if (not cmd-list) nil
           (catch 'found-new
             (dolist (key cmd-list)
               (when (and (setq ret (lookup-key keymap key))
                          (and (integerp ret) (setq ret nil)))
                 (throw 'found-new t)))
             t)
           ret))))

;; Startup and load functions

(add-hook 'ergoemacs-mode-after-init-emacs 'ergoemacs-map-properties--label-unlabeled)
(add-hook 'ergoemacs-mode-after-load-hook 'ergoemacs-map-properties--label-unlabeled)

(provide 'ergoemacs-map-properties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map-properties.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
