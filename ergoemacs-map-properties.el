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

(require 'package)

(defvar ergoemacs--gzip)
(defvar ergoemacs-map-keymap--load-autoloads-p)
(defvar ergoemacs--original-local-map)
(defvar ergoemacs--system)
(defvar ergoemacs-breadcrumb-hash)
(defvar ergoemacs-dir)
(defvar ergoemacs-directories-where-keys-from-hook-are-deferred)
(defvar ergoemacs-functions-that-always-override-ergoemacs-mode)
(defvar ergoemacs-hooks-that-always-override-ergoemacs-mode)
(defvar ergoemacs-ignore-prev-global)
(defvar ergoemacs-map--breadcrumb)
(defvar ergoemacs-map-properties--after-ergoemacs)
(defvar ergoemacs-map-properties--before-ergoemacs)
(defvar ergoemacs-map-properties--get-or-generate-map-key)
(defvar ergoemacs-map-properties--indirect-keymaps)
(defvar ergoemacs-map-properties--key-struct)
(defvar ergoemacs-map-properties--plist-hash)
(defvar ergoemacs-mode--fast-p)
(defvar ergoemacs-remap-ignore)
(defvar ergoemacs-saved-global-map)
(defvar ergoemacs-map-properties--create-label-function)
(defvar ergoemacs-map-properties--known-maps)
(defvar ergoemacs-map-properties--label-atoms-maps)

(declare-function ergoemacs-command-loop--spinner-display "ergoemacs-command-loop")
(declare-function ergoemacs-timing-- "ergoemacs-mode")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-setcdr "ergoemacs-lib")
(declare-function ergoemacs-warn "ergoemacs-lib")

(declare-function ergoemacs-translate--define-key "ergoemacs-translate")
(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")
(declare-function ergoemacs-key-description "ergoemacs-key-description")

(defun ergoemacs-map-properties--keymap-value (keymap)
  "Return the keymap value of KEYMAP.
KEYMAP can be a symbol, keymap or ergoemacs-mode keymap"
  (let (tmp)
    (or (and (integerp keymap) keymap)
        (and (listp keymap) (ergoemacs-keymapp keymap) keymap)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-value keymap))) tmp)
        (and (symbolp keymap) (ergoemacs-keymapp (setq tmp (symbol-function keymap))) tmp)
        ;; (ignore-errors (and (setq tmp (ergoemacs-gethash keymap ergoemacs-map-properties--plist-hash))
        ;;                     (setq tmp (ergoemacs-gethash :map-list tmp))
        ;;                     (symbol-value (car tmp))))
        ;; (ignore-errors (and (setq tmp (plist-get keymap :map-list)) (symbol-value (nth 0 tmp))))
        )))

(defun ergoemacs-map-properties--composed-p (keymap &rest _ignore)
  "Determine if the KEYMAP is a composed keymap."
  (and (ergoemacs-keymapp keymap)
       (ignore-errors (eq 'keymap (car keymap)))
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
                (dolist (map (ergoemacs keymap :composed-list))
                  (when (ergoemacs-keymapp map)
                    (unless (ergoemacs-map-properties--all-sparse-p map)
                      (throw 'found-full nil)))) t)))
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
              (if (consp map)
                  (if melt
                      (setq ret (append (cdr map) ret))
                    (push (cons (car map) (cdr map)) ret))
                (push map ret))))
        (when parent
          (set-keymap-parent keymap parent))
        (when melt
          (setq ret (append '(keymap) ret))))
      ret)))

(defun ergoemacs-map-properties--composed (keymap &optional force)
  "Returns a list of `ergoemacs-mode' map-key for the composed keymap list"
  (let ((composed-list (ergoemacs-map-properties--composed-list keymap nil force)))
    (and composed-list
         (catch 'not-bound
           (mapcar
            (lambda(comp)
              (when (ergoemacs-keymapp comp)
                (let ((ret (ergoemacs-map-properties--key-struct comp)))
                  (when (and (not force) (not ret))
                    (throw 'not-bound nil))
                  ret))) composed-list)))))


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
                  (and hash-key (ergoemacs-gethash hash-key ergoemacs-map-properties--key-struct)))))
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
  (cond
   ((integerp keymap) (list keymap))
   ((ergoemacs-keymapp keymap)
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
      ret))))

(defun ergoemacs-map-properties--default-global-file (&optional other)
  "What is the global key hash file."
  (let* ((file (expand-file-name (format "ergoemacs-%s-%s.el%s" (or other "global") ergoemacs--system (or (and ergoemacs--gzip ".gz") ""))
                                 ergoemacs-dir))
         (extras (expand-file-name "ergoemacs-extras" user-emacs-directory))
         (file2 (expand-file-name (format "ergoemacs-%s-%s.el%s" (or other "global") ergoemacs--system (or (and ergoemacs--gzip ".gz") ""))
                                  extras)))
    (or
     (and (file-readable-p file2) file2)
     (and (file-readable-p file) file)
     (and (file-writable-p file) file)
     file2)))

(defvar ergoemacs-map-properties--const-keymaps nil
  "Variable listing constant keymaps.")

(defun ergoemacs-map-properties--map-regexp (&optional at-end)
  "Generates a regular expression of all known maps."
  (concat (regexp-opt (mapcar (lambda(x) (symbol-name x)) ergoemacs-map-properties--label-atoms-maps) 'symbols) (or (and at-end "$") "")))

(defun ergoemacs-map-properties--default-global-gen ()
  "Generates hash for default emacs maps."
  ;; (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal))
  (ergoemacs global-map :label most-negative-fixnum) ;; Should be `most-negative-fixnum'
  (ergoemacs-map-properties--label-atoms)
  (with-temp-file (ergoemacs-map-properties--default-global-file) 
    (let ((print-level nil)
          (print-length nil)
          (where-is-hash (make-hash-table))
          tmp
          keys)
      (goto-char (point-min))
      (insert ";; -*- coding: utf-8-emacs -*-\n")
      (insert "(defvar ergoemacs-map-properties--plist-hash)(declare-function ergoemacs-map-properties--label \"ergoemacs-map-properties\")(declare-function ergoemacs-command-loop--spinner-display \"ergoemacs-command-loop\")")
      (ergoemacs-map-keymap
       (lambda (key item)
         (cond
          ((vectorp key)
           (push key keys)
           (if (setq tmp (ergoemacs-gethash item where-is-hash))
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
      (prin1 (ergoemacs-map-properties--create-label-function t)
             (current-buffer)))))

(defun ergoemacs-map-properties--label-echo (keymap-symbol map id)
  "When KEYMAP-SYMBOL is bound, label MAP to ID.

Also let the user know that the labeling was performed."
  (when (boundp keymap-symbol)
    (ergoemacs :spinner '("🎫→%s" "Label→%s" "Label->%s") keymap-symbol)
    (if (ergoemacs map :installed-p)
        (ergoemacs :label (ergoemacs map :original) id)
      (if (ergoemacs map :composed-p)
          (ergoemacs-warn "%s was not labeled to %s since it was a composed keymap.")
        (ergoemacs :label (ergoemacs map :original) id)))))


(defun ergoemacs-map-properties--create-label-function (&optional no-lambda)
  "Create a function to label known keymaps.

When NO-LAMBDA is non-nil, don't prepend the `lambda' specification."
  (let ((ret nil)
        tmp)
    (dolist (map ergoemacs-map-properties--label-atoms-maps)
      (when (ergoemacs-map-properties--key-struct map)
        (setq tmp (find-lisp-object-file-name map 'defvar))
        (unless (or (not tmp) (eq tmp 'C-source)
                    (eq map 'global-map))
          (setq ret
                (append ret
                        `((eval-after-load ,(file-name-sans-extension (file-name-nondirectory tmp))
                            '(when (boundp ',map)
                               (ergoemacs-map-properties--label-echo ',map ,map (ergoemacs (ergoemacs (ergoemacs-sv map) :original) :map-key))))))))))
    (push 'progn ret)
    (or (and no-lambda ret) `(lambda() ,ret))))

(defun ergoemacs-map-properties--before-ergoemacs (&optional after)
  "Get a keymap of keys that changed before or after loading ergoemacs.

By default this gives a keymap of keys changed before
ergoemacs-mode loaded.

When AFTER is non-nil, this is a list of keys that changed after
`ergoemacs-mode' loaded."
  (or (and (not after) ergoemacs-map-properties--before-ergoemacs)
      (and after ergoemacs-map-properties--after-ergoemacs)
      (let ((hash-table (ergoemacs-gethash :extract-lookup (ergoemacs-gethash (list :map-key most-negative-fixnum) ergoemacs-map-properties--plist-hash)))
            (original-global-map (ergoemacs :global-map))
            (before-map (make-sparse-keymap))
            tmp)
        (ergoemacs-timing before-ergoemacs
          (unwind-protect
              (progn
                (setq ergoemacs-map-keymap--load-autoloads-p nil)
                (ergoemacs-map-keymap
                 (lambda (cur-key item)
                   (unless (or (consp cur-key) (eq item 'ergoemacs-prefix))
                     (setq tmp (ergoemacs-gethash cur-key hash-table cur-key))
                     (cond
                      ;; bach mode doesn't save menu-bar or tool-bar information
                      ((memq (elt cur-key 0) '(menu-bar tool-bar iconify-frame make-frame-visible)))
                      ;; batch mode also doesn't save mouse-events
                      ((memq (event-basic-type (elt cur-key 0)) '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-4)))
                      ;; M-O is bound to facemenu keymap by default, except in
                      ;; terminal/batch mode
                      ((and (>= (length cur-key) 2)
                            (eq (elt cur-key 0) 27)
                            (eq (elt cur-key 1) 111)
                            (eq (lookup-key original-global-map [27 111]) 'facemenu-keymap)))
                      ((eq 'ergoemacs-labeled (elt cur-key (- (length cur-key) 1))))
                      ((and (symbolp item) (string-match-p "clipboard" (symbol-name item))))
                      ((and (equal [27 115 104 102] cur-key) (eq item 'hi-lock-find-patterns)))
                      ((and tmp (not (equal tmp item))
                            (or (not after)
                                (not (and ergoemacs-map-properties--before-ergoemacs
                                          (eq item (lookup-key ergoemacs-map-properties--before-ergoemacs cur-key))))))
                       (ergoemacs :define-key before-map cur-key item))
                      ((and (not tmp)
                            (or (not after)
                                (not (and ergoemacs-map-properties--before-ergoemacs
                                          (lookup-key ergoemacs-map-properties--before-ergoemacs cur-key)))))
                       (ergoemacs :define-key before-map cur-key tmp)))))
                 original-global-map t))
            (setq ergoemacs-map-keymap--load-autoloads-p t)))
        (if after
            (progn
              (setq ergoemacs-map-properties--after-ergoemacs before-map)
              (ergoemacs before-map :label)
              (ergoemacs before-map :map-list-hash '(ergoemacs-map-properties--after-ergoemacs)))
          (setq ergoemacs-map-properties--before-ergoemacs before-map)
          (ergoemacs before-map :label)
          (ergoemacs before-map :map-list-hash '(ergoemacs-map-properties--before-ergoemacs)))
        before-map)))

(defvar ergoemacs-map-properties--protect-local nil)
(defun ergoemacs-map-properties--protect-local (hook fn)
  "Protect a local map's modification with information about what hook is running."
  (let ((fn (or (and (symbolp fn) fn) 'lambda)))
    (if (not hook)
        (setq ergoemacs-map-properties--protect-local nil)
      (setq ergoemacs-map-properties--protect-local (list hook fn)))))


(defun ergoemacs-map-properties--modify-run-mode-hooks (&rest hooks)
  "Modify HOOKS to run `ergoemacs-map-properties--protect-local' before hook."
  (let (tmp hook-value)
    (dolist (hook (or (and (consp hooks) hooks) (list hooks)))
      (if (consp hook)
          (dolist (lhook hook)
            (ergoemacs-map-properties--modify-run-mode-hooks lhook))
        (when (and hook (boundp hook) (and (string-match-p "mode-hook" (symbol-name hook))))
          (set hook
               (cond
                ((and (setq hook-value (symbol-value hook))
                      (consp hook-value))
                 (mapcar
                  (lambda(fn)
                    (if (or (eq fn t) (and (setq tmp (documentation fn))
                                           (stringp tmp)
                                           (string-match-p  "^Ergoemacs protect local" tmp)))
                        fn
                      `(lambda() "Ergoemacs protect local"
                         (ergoemacs-map-properties--protect-local ',hook ',fn)
                         (funcall ',fn))))
                  hook-value))
                (t ;; For now do nothing
                 hook-value))))))))


(defun ergoemacs-map-properties--reset-run-mode-hooks (&rest hooks)
  "Reset HOOKS as if `ergoemacs-map-properties--modify-run-mode-hooks' wasn't run."
  (let (tmp hook-value)
    (dolist (hook (or (and (consp hooks) hooks) (list hooks)))
      (if (consp hook)
          (dolist (lhook hook)
            (ergoemacs-map-properties--reset-run-mode-hooks lhook))
        (when (and hook (boundp hook) (and (string-match-p "mode-hook" (symbol-name hook))))
          (set hook
               (cond
                ((and (setq hook-value (symbol-value hook))
                      (consp hook))
                 (mapcar
                  (lambda(fn)
                    (if (or (eq fn t) (and (setq tmp (documentation fn))
                                           (stringp tmp)
                                           (string-match-p  "^Ergoemacs protect local" tmp)
                                           (setq tmp (ignore-errors (car (cdr (nth 1 (nth 4 fn))))))))
                        tmp
                      fn))
                  hook-value))
                (t ;; For now don't touch these.
                 hook-value))))))))


(defvar ergoemacs-map-properties--hook-map-hash (make-hash-table :test 'equal)
  "Hash table of user hook maps that `ergoemacs-mode' saves")

(defun ergoemacs-map-properties--hook-define-key (keymap key def)
  "Save hook-defined keys on separate keymaps.
These keymaps are saved in `ergoemacs-map-properties--hook-map-hash'."
  (ergoemacs keymap :label)
  (let* ((kbd key)
         (key (ergoemacs keymap :map-key))
         key2 tmp map)
    (when (integerp key)
      (setq key2 key
            tmp (ergoemacs-gethash key2 ergoemacs-map-properties--hook-map-hash)
            map (ergoemacs-gethash
                 (setq key `(,(ergoemacs keymap :key-hash) ,@ergoemacs-map-properties--protect-local))
                 ergoemacs-map-properties--hook-map-hash))
      (push key tmp)
      (puthash key2 tmp ergoemacs-map-properties--hook-map-hash)
      (unless map
        (puthash key (make-sparse-keymap) ergoemacs-map-properties--hook-map-hash)
        (setq map (ergoemacs-gethash key ergoemacs-map-properties--hook-map-hash))
        (ergoemacs map :label `(hook-map ,@ergoemacs-map-properties--protect-local)))
      (unwind-protect
          (progn
            (setq tmp ergoemacs-map-properties--protect-local
                  ergoemacs-map-properties--protect-local nil)
            (ergoemacs :define-key map kbd def))
        (setq ergoemacs-map-properties--protect-local nil)))))

(defun ergoemacs-map-properties--override-maps (keymap &rest _ignore)
  "Returns a list of overriding maps based on hooks run."
  (let* ((key (ergoemacs keymap :map-key))
         lst
         ret)
    (when (integerp key)
      (setq lst (ergoemacs-gethash key ergoemacs-map-properties--hook-map-hash))
      (dolist (map-key lst)
        (when (ergoemacs map-key :override-map-p)
          (push (ergoemacs-gethash map-key ergoemacs-map-properties--hook-map-hash) ret))))
    ret))

(defun ergoemacs-map-properties--deferred-maps (keymap &rest _ignore)
  "Returns a list of overriding maps based on hooks run."
  (let* ((key (ergoemacs keymap :map-key))
         lst
         ret)
    (when (integerp key)
      (setq lst (ergoemacs-gethash key ergoemacs-map-properties--hook-map-hash))
      (dolist (map-key lst)
        (when (not (ergoemacs map-key :override-map-p))
          (push (ergoemacs-gethash map-key ergoemacs-map-properties--hook-map-hash) ret))))
    ret))


(defvar ergoemacs-map-properties--override-map-hash (make-hash-table)
  "Hash Table of defined/undefined keys")

(defvar ergoemacs-map-properties--deferred-hooks-directory-regexp
  (concat "\\`" (regexp-opt (append package-directory-list
                                    (list package-user-dir)
                                    (list (file-name-directory (locate-library "abbrev")))
                                    ergoemacs-directories-where-keys-from-hook-are-deferred) t))
  "Regular experssion of libraries where maps are deferred")

(defun ergoemacs-map-properties--override-map-p (keymap)
  "Determine if KEYMAP should override `ergoemacs-mode' keys."
  (let ((key (or (and (ergoemacs-keymapp keymap) (ergoemacs keymap :map-key)) keymap))
        tmp)
    (and (consp key)
         (or (eq (car key) 'hook-map)
             (and (consp (car key)) (integerp (car (car key)))))
         (or (memq (nth 1 key) ergoemacs-hooks-that-always-override-ergoemacs-mode)
             (memq (nth 2 key) ergoemacs-functions-that-always-override-ergoemacs-mode)
             (progn
               (setq tmp (ergoemacs-gethash (nth 2 key) ergoemacs-map-properties--override-map-hash))
               (if tmp
                   (if (eq tmp :override-p) t nil)
                 (if (not (functionp (nth 2 key))) nil
                   (if (string-match-p ergoemacs-map-properties--deferred-hooks-directory-regexp (find-lisp-object-file-name (nth 2 key) (symbol-function (nth 2 key))))
                       (progn
                         (puthash (nth 2 key) :deferred-p ergoemacs-map-properties--override-map-hash)
                         nil)
                     (puthash (nth 2 key) :override-p ergoemacs-map-properties--override-map-hash)
                     t))))))))
(defun ergoemacs-map-properties--protect-global-map ()
  "Protects global map by adding a user-key layer to it"
  (when (and (or (not noninteractive) (file-readable-p (ergoemacs-map-properties--default-global-file)))
             (integerp (ergoemacs global-map :map-key)))
    (let ((user-map (ergoemacs global-map :user)))
      (ergoemacs :user-before)
      (setq global-map (make-composed-keymap user-map global-map)))))

(defun ergoemacs-map-properties--get-original-global-map ()
  "Loads/Creates the default global map information."
  (if ergoemacs-mode--fast-p
      (ergoemacs-map-properties--label-atoms)
    (ergoemacs-timing get-original-global-map
      (ergoemacs-map-properties--label-atoms)
      (if (file-readable-p (ergoemacs-map-properties--default-global-file))
          (progn
            (unless ergoemacs-mode--fast-p
              (load (ergoemacs-map-properties--default-global-file)))
            (when (functionp ergoemacs-map-properties--create-label-function)
              (ergoemacs-timing ergoemacs-map-properties--create-label-function
                (funcall ergoemacs-map-properties--create-label-function)))
            (ergoemacs-map-properties--protect-global-map))
        (if noninteractive
            (ergoemacs-warn "Could not find global map information")
          (ergoemacs-timing ergoemacs-create-global
            (let* ((emacs-exe (ergoemacs-emacs-exe))
                   (default-directory (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))))
                   (cmd (format "%s -L %s --batch --load \"ergoemacs-mode\" -Q --eval \"(ergoemacs-map-properties--default-global-gen) (kill-emacs)\"" emacs-exe default-directory)))
              (message "%s" (shell-command-to-string cmd))
              (ergoemacs-map-properties--get-original-global-map))))))))

(add-hook 'ergoemacs-mode-intialize-hook 'ergoemacs-map-properties--get-original-global-map)

(defun ergoemacs-map-properties--map-fixed-plist (keymap &rest _ignore)
  "Determines if this is an `ergoemacs-mode' KEYMAP.
Returns a plist of fixed keymap properties (not changed by
composing or parent/child relationships)"
  (if (not (ergoemacs-keymapp keymap) ) nil
    (if (ignore-errors (symbol-function keymap))
        (progn (ergoemacs-gethash keymap ergoemacs-map-properties--indirect-keymaps))
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

(defun ergoemacs-map-properties--get (keymap property)
  "Get ergoemacs-mode KEYMAP PROPERTY."
  (let ((ret (ergoemacs-map-properties--map-fixed-plist keymap)) tmp)
    (unless (and ergoemacs-map-properties--plist-hash (hash-table-p ergoemacs-map-properties--plist-hash))
      (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal)))
    (setq tmp (ergoemacs-gethash (ergoemacs-map-properties--key-struct keymap) ergoemacs-map-properties--plist-hash))
    (if (not (and tmp (hash-table-p tmp)))
	(setq ret nil)
      (setq ret (ergoemacs-gethash property tmp)))
    ret))

(defun ergoemacs-map-properties--put (keymap property value)
  "Set ergoemacs-mode KEYMAP PROPERTY to VALUE."
  (prog1 value
    (if (eq property :label)
        (ergoemacs :label keymap value)
      (let ((keymap (ergoemacs-map-properties--keymap-value keymap)))
        (cond
         ((not (ergoemacs-keymapp keymap))
          (error "Trying to put keymap property on non-keymap %s." keymap))
         ((eq property :full)
          (ergoemacs-warn "Cannot set the keymap property :full"))
         (t (let ((ret (ergoemacs-map-properties--map-fixed-plist keymap)) tmp)
              (if (and ret (eq property :map-key))
                  (progn
                    (setq ret (plist-put ret property value))
                    (ergoemacs :label keymap value))
                (unless (and ergoemacs-map-properties--plist-hash (hash-table-p ergoemacs-map-properties--plist-hash))
                  (setq ergoemacs-map-properties--plist-hash (make-hash-table :test 'equal)))
                (setq tmp (ergoemacs-gethash (ergoemacs-map-properties--key-struct keymap) ergoemacs-map-properties--plist-hash))
                (unless (and tmp (hash-table-p tmp))
                  (setq tmp (make-hash-table)))
                (puthash property value tmp)
                (puthash (ergoemacs-map-properties--key-struct keymap) tmp ergoemacs-map-properties--plist-hash)))))))))

(defun ergoemacs-map-properties--parent (keymap &optional force)
  "Returns a `ergoemacs-mode' map-key for the parent of KEYMAP."
  (if (not (ergoemacs-keymapp keymap)) nil
    (let ((parent (keymap-parent keymap)))
      (and parent (ergoemacs-map-properties--key-struct parent force)))))

(defun ergoemacs-map-properties--map-list (keymap)
  "Get the list of maps bound to KEYMAP.
KEYMAP can be a keymap or integer indicating the keympap id."
  (cond
   ((ergoemacs-keymapp keymap)
    (ergoemacs (ergoemacs keymap :original) :map-list-hash))
   ((integerp keymap)
    (let (ret)
      (dolist (map ergoemacs-map-properties--label-atoms-maps)
	(when (and (boundp map)
		   (eq (ergoemacs map :map-key) keymap))
	  (push map ret)))
      ret))
   (t
    ;; (error "Need a proper keymap.")
    nil)))

(defun ergoemacs-map-properties--label-map (map &optional label-empty-p)
  "Label MAP"
  (let* (sv map-list-hash)
    (cond 
     ((get map :ergoemacs-labeled)
      t) ;; Already labeled
     ((not (setq sv (ergoemacs-sv map t)))
      nil) ;; Nil
     ((not (ergoemacs-keymapp sv)) ;; Not a keymap
      (put map :ergoemacs-labeled t)
      t)
     ((and (not label-empty-p)
	   (or (equal sv (make-sparse-keymap)) ;; Empty
	       (equal sv (make-keymap))))
      nil)
     ((ergoemacs sv :installed-p) ;; Already modified.
      (put map :ergoemacs-labeled t)
      (setq map-list-hash (ergoemacs sv :map-list-hash))
      (push map map-list-hash)
      (ergoemacs sv :map-list-hash map-list-hash))
     ;; ((ergoemacs sv :composed-p) ;; Already modified.
     ;;  (ergoemacs-warn "Composed map %s not labeled." map))
     (t ;;Label
      (when sv
        (let (key)
          (setq key (ergoemacs-map-properties--get-or-generate-map-key sv))
          (ergoemacs :label sv key)))
      (pushnew map ergoemacs-map-properties--label-atoms-maps)
      (setq map-list-hash (ergoemacs sv :map-list-hash))
      (push map map-list-hash)
      (ergoemacs sv :map-list-hash map-list-hash)
      (put map :ergoemacs-labeled t)
      t))))

(defun ergoemacs-map-properties--label-atoms (&rest _ignore)
  "Label all the bound keymaps."
  (mapatoms #'ergoemacs-map-properties--label-map))

;; Startup and load functions
;;(add-hook 'ergoemacs-mode-after-init-emacs 'ergoemacs-map-properties--label-unlabeled)
;;(add-hook 'ergoemacs-mode-after-load-hook 'ergoemacs-map-properties--label-unlabeled)

(defvar ergoemacs-map-properties--unlabeled-p t
  "Is the map unlabeled?")

(defun ergoemacs-map-properties--get-or-generate-map-key (keymap)
  "Gets the key for the KEYMAP.
If unlabeled, set `ergoemacs-map-properties--unlabeled-p' to nil,
otherwise it is set to t."
  (setq ergoemacs-map-properties--unlabeled-p t)
  (let ((ret (ergoemacs-map-properties--map-fixed-plist (ergoemacs-map-properties--keymap-value keymap))))
    (unless (and ret (setq ret (plist-get ret :map-key)))
      (setq ergoemacs-map-properties--unlabeled-p nil
	    ret (or (prog1 (and ergoemacs-map--breadcrumb (not (string= "" ergoemacs-map--breadcrumb))
				(ergoemacs-gethash (intern ergoemacs-map--breadcrumb) ergoemacs-breadcrumb-hash)))
                    (prog1 (setq ergoemacs-map-properties--get-or-generate-map-key
				 (+ 1 ergoemacs-map-properties--get-or-generate-map-key))
		      (when (and ergoemacs-map--breadcrumb (not (string= "" ergoemacs-map--breadcrumb)))
			(puthash (intern ergoemacs-map--breadcrumb) ergoemacs-map-properties--get-or-generate-map-key ergoemacs-breadcrumb-hash))))))
    ret))

(defun ergoemacs-map-properties--label (keymap &optional map-key struct)
  "Label an `ergoemacs-mode' touched KEYMAP.
MAP-KEY is the identifier of the map name.
STRUCT is the keymap structure for the current map."
  (if (not (ergoemacs-keymapp keymap)) nil
    (if (ergoemacs-map-properties--composed-p keymap)
        (cond
         (map-key
          (error "Will not label a composed map's members to %s" map-key))
         (t
          (let ((parent (keymap-parent keymap))
		(breadcrumb-base ergoemacs-map--breadcrumb)
		(struct (or struct (ergoemacs-gethash map-key ergoemacs-map-properties--key-struct)))
		(comp (plist-get struct :composed))
		(comp-list (ergoemacs-map-properties--composed-list keymap))
		(i 0))
	    (unless (= (length comp) (length comp-list))
	      (setq comp nil))
	    (dolist (map comp-list)
	      (when (and breadcrumb-base (not (string= breadcrumb-base "")))
		(setq ergoemacs-map--breadcrumb (concat breadcrumb-base "-" (number-to-string i))
		      i (+ 1 i)))
	      (if comp
		  (ergoemacs :label map nil (pop comp))
		(ergoemacs :label map)))
	    (when parent
	      (when (and breadcrumb-base (not (string= breadcrumb-base "")))
                (setq ergoemacs-map--breadcrumb (concat breadcrumb-base "-parent")))
              (ergoemacs :label parent nil (plist-get struct :parent)))
	    (setq ergoemacs-map--breadcrumb breadcrumb-base))))
      (let* ((map keymap)
             (map-key (or map-key
			  (plist-get struct :map-key)
			  (ergoemacs-map-properties--get-or-generate-map-key map)))
             char-table
             indirect-p
             old-plist
	     (breadcrumb-base ergoemacs-map--breadcrumb)
             (parent (keymap-parent map))
	     (struct (or struct (ergoemacs-gethash map-key ergoemacs-map-properties--key-struct)))
	     unlabeled-p
             label tmp1 tmp2)
        (unwind-protect
            (progn
              (ignore-errors (set-keymap-parent map nil))
              (if (ergoemacs-keymapp (symbol-function keymap))
                  (setq indirect-p t ; Indirect keymap
                        old-plist (ergoemacs-gethash keymap ergoemacs-map-properties--indirect-keymaps))
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
	  (setq unlabeled-p ergoemacs-map-properties--unlabeled-p)
          (when parent
	    (when (and breadcrumb-base (not (string= breadcrumb-base "")))
              (setq ergoemacs-map--breadcrumb (concat breadcrumb-base "-parent")))
            (ergoemacs :label parent nil (plist-get struct :parent))
	    (set-keymap-parent map parent)
	    (setq ergoemacs-map--breadcrumb breadcrumb-base)))
        (if indirect-p
            (puthash keymap old-plist ergoemacs-map-properties--indirect-keymaps)
          (unless (ignore-errors (ergoemacs-setcdr keymap (cdr map)))
            (pushnew (cons old-plist (cdr keymap)) ergoemacs-map-properties--const-keymaps)))
 	(when (and unlabeled-p (not (consp map-key)))
          (setq tmp1 nil)
          (dolist (known-map-sym ergoemacs-map-properties--known-maps)
            (if (not (and (boundp known-map-sym)
                          (eq (ergoemacs-sv known-map-sym) keymap)))
                (push known-map-sym tmp1)
              (setq tmp2 (ergoemacs map :map-list-hash))
	      (pushnew known-map-sym tmp2)
	      (pushnew known-map-sym ergoemacs-map-properties--label-atoms-maps)
	      (ergoemacs map :map-list-hash tmp2)))
	  (setq ergoemacs-map-properties--known-maps tmp1))
        map))))

(defun ergoemacs-map-properties--empty-p (keymap &rest _ignore)
  "Determines if a KEYMAP is empty."
  (catch 'found-key
    (ergoemacs-timing empty-p
      (ergoemacs-map-keymap
       (lambda (cur-key item)
         (unless (equal cur-key [ergoemacs-labeled])
           (if (consp cur-key)
               (throw 'found-key nil)
             (unless (eq item 'ergoemacs-prefix)
               (when item
                 (throw 'found-key nil))))))
       keymap)) t))

;;ergoemacs-map-properties--label


(defvar ergoemacs-map-properties--user-map-hash (make-hash-table :test 'equal)
  "Hash table of the user maps that `ergoemacs-mode' saves.")

(defun ergoemacs-map-properties--user (keymap &rest _ignore)
  "Gets the user KEYMAP with `ergoemacs-mode' identifiers installed.
KEYMAP can be an `ergoemacs-map-properties--key-struct' of the keymap as well."
  (let ((key (ergoemacs keymap :map-key))
        original map)
    (cond
     ((not key)
      (ergoemacs :label keymap)
      (setq key (ergoemacs keymap :map-key))
      (puthash key (make-sparse-keymap) ergoemacs-map-properties--user-map-hash)
      (setq map (ergoemacs-gethash key ergoemacs-map-properties--user-map-hash))
      (ergoemacs map :label (list (ergoemacs keymap :key-hash) 'user)))
     ((not (integerp key))
      (setq original (ergoemacs keymap :original)
            key (ergoemacs original :map-key))
      (if (integerp key)
          (setq map (ergoemacs original :user))
        (setq map (ergoemacs-gethash key ergoemacs-map-properties--user-map-hash))
        (unless map
          (puthash key (make-sparse-keymap) ergoemacs-map-properties--user-map-hash)
          (setq map (ergoemacs-gethash key ergoemacs-map-properties--user-map-hash))
          (ergoemacs map :label (list key 'user)))))
     (t
      (setq map (ergoemacs-gethash (setq key (ergoemacs keymap :key-hash)) ergoemacs-map-properties--user-map-hash))
      (unless map
        (puthash key (make-sparse-keymap) ergoemacs-map-properties--user-map-hash)
        (setq map (ergoemacs-gethash key ergoemacs-map-properties--user-map-hash))
        (ergoemacs map :label (list (ergoemacs keymap :key-hash) 'user)))))
    map))

(defun ergoemacs-map-properties--calculate-keys-and-where-is-hash (keymap &rest _ignore)
  "Calculates :where-is and :keys properties for KEYMAP."
  (let ((where-is-hash (make-hash-table))
        (lookup-hash (make-hash-table :test 'equal))
        keys tmp)
    (ergoemacs-timing where-is-hash
      (ergoemacs-map-keymap
       (lambda (key item)
         (unless (and (vectorp key) (eq (elt key (- (length key) 1)) 'ergoemacs-labeled))
           (cond
            ((and (vectorp key)
                  (commandp item t))
             (push key keys)
             (if (setq tmp (ergoemacs-gethash item where-is-hash))
                 (push key tmp)
               (puthash item (list key) where-is-hash))
             (puthash key item lookup-hash)))))
       keymap))
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
              (hash-table (ergoemacs (or relative-map ergoemacs-saved-global-map global-map) :where-is))
              (cmd-list (ergoemacs-gethash command hash-table)))
         (if (not cmd-list) nil
           (catch 'found-new
             (dolist (key cmd-list)
               (when (and (setq ret (lookup-key keymap key t))
                          (or (and (commandp ret t) (not (memq ret ergoemacs-remap-ignore)))
                              (and (integerp ret) (setq ret nil))))
                 (throw 'found-new t))
               (setq ret nil)) t) ret))))

(defun ergoemacs-map-properties--revert-original (keymap &rest type)
  "Revert KEYMAP.

This is the same as calling `ergoemacs-map-properties--original'
with the TYPE set to :setcdr.

You may specify TYPE to be :flatten to return a flattend copy of
the original instead."
  (ergoemacs-map-properties--original keymap (or type :setcdr)))
(defun ergoemacs-map-properties--original (keymap &optional type)
  "Get original KEYMAP.

When TYPE is :flatten, the return keymap is not composed and has
no parents.

When TYPE is :setcdr, the function modifies any submaps so they
are original keymaps as well as modifying KEYMAP to be the
original keymap.

When TYPE is nil, return the original keymap, but any sub keymaps
are not original, and the keymap may be composed or have a parent
keymap."
  (if (not (ergoemacs-keymapp keymap)) nil
    (let ((ret keymap))
      (while (and (or (and (not (ergoemacs ret :map-key))
                           ;; Apply label if needed.
                           (ergoemacs ret :label)) t)
                  (not (integerp (ergoemacs ret :map-key)))
                  (setq ret (keymap-parent ret)))
        t)
      (cond
       ((eq type :flatten)
        (ergoemacs-timing flatten-original
          (ergoemacs-map-keymap nil ret t)))
       ((eq type :submaps)
        (ergoemacs-setcdr (cdr ret)
                          (cdr (ergoemacs-timing flatten-setcdr
                                 (ergoemacs-map-keymap nil ret :setcdr))))
        ret)
       (t ret)))))

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
  (when (and keymap (ergoemacs-keymapp keymap))
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

(defun ergoemacs-map-properties--key-lessp (key1 key2)
  "Compares KEY1 and KEY2, and determines if KEY1 is \"less than\" key2.
Used for sorting keys in displays."
  (cond
   ((and (not key1) (not key2)) t)
   ((and key1 (not key2)) nil)
   ((and key2 (not key1)) t)
   (t
    (let* ((seq1 (or (and (or (stringp key1) (vectorp key1))
                          (listify-key-sequence 
                           (or (ergoemacs-translate--escape-to-meta
                                (vconcat key1))
                               (vconcat key1))))
                     key1))
           (seq2 (or (and (or (stringp key2) (vectorp key2))
                          (listify-key-sequence 
                           (or (ergoemacs-translate--escape-to-meta
                                (vconcat key2))
                               (vconcat key2))))
                     key2) )
           (c1 (car seq1))
           (c2 (car seq2))
           (e1 (event-basic-type c1))
           (e2 (event-basic-type c2))
           (m1 (event-modifiers c1))
           (m2 (event-modifiers c2)))
      (cond
       ;; Modifier lengths are different
       ((< (length m1) (length m2)) t)
       ((> (length m1) (length m2)) nil)

       ;; meta first
       ((and (not (memq 'meta m1)) (memq 'meta m2)) t)
       ((and (memq 'meta m1) (not (memq 'meta m2))) nil)

       ;; control next
       ((and (not (memq 'control m1)) (memq 'control m2)) t)
       ((and (memq 'control m1) (not (memq 'control m2))) nil)

       ;; shift next
       ((and (not (memq 'shift m1)) (memq 'shift m2)) t)
       ((and (memq 'shift m1) (not (memq 'shift m2))) nil)

       ;; hyper
       ((and (not (memq 'hyper m1)) (memq 'hyper m2)) t)
       ((and (memq 'hyper m1) (not (memq 'hyper m2))) nil)

       ;; super
       ((and (not (memq 'super m1)) (memq 'super m2)) t)
       ((and (memq 'super m1) (not (memq 'super m2))) nil)

       ;; 
       ((and (integerp e1) (symbolp e2)) t)
       ((and (symbolp e1) (integerp e2)) nil)

       ((or (and (symbolp e1) (symbolp e2)
                 (string= (symbol-name e1) (symbol-name e2)))
            (and (integerp e1) (integerp e2) (= e1 e2)))
        (ergoemacs-map-properties--key-lessp (cdr seq1) (cdr seq2)))

       ((and (symbolp e1) (symbolp e2)
             (string= "f" (substring (symbol-name e1) 0 1))
             (string= "f" (substring (symbol-name e2) 0 1)))
        (< (string-to-number (substring (symbol-name e1) 1)) (string-to-number (substring (symbol-name e2) 1))))
       
       ((and (symbolp e1) (symbolp e2))
        (string-lessp (ergoemacs-key-description (vector e1)) (ergoemacs-key-description (vector e2))))
       
       ((and (integerp e1) (integerp e2))
        (< e1 e2))
       (t t))))))


(defvar ergoemacs-map-properties--use-local-unbind-list '(isearch-mode-map)
  "List of maps that will unbind ergoemacs-mode keys instead of using them directly.")

(defun ergoemacs-map-properties--use-local-unbind-list-p (keymap &rest _ignore)
  "Determines if ergoemacs-mode keys should be unbound in KEYMAP.
Looks in `ergoemacs-use-local-unbind-list' to determine what maps will unbind ergoemacs keys.

 This is useful in supporting isearch in emacs 24.4+."
  (when (ergoemacs-keymapp keymap)
    
    (let ((local-unbind-list-p (ergoemacs keymap :use-local-unbind-list-key)))
      (cond
       ((eq local-unbind-list-p 'no) nil)
       (local-unbind-list-p local-unbind-list-p)
       (ergoemacs-map-properties--use-local-unbind-list
        (let ((map-list (ergoemacs keymap :map-list)))
          (prog1 (catch 'found-use-local
                   (dolist (map map-list)
                     (when (memq map ergoemacs-map-properties--use-local-unbind-list)
                       (setq local-unbind-list-p t)
                       (throw 'found-use-local t)))
                   (setq local-unbind-list-p 'no)
                   nil)
            (ergoemacs keymap :use-local-unbind-list-key local-unbind-list-p))))
       (t nil)))))

(defvar ergoemacs-map-properties--set-map-list '(isearch-mode-map)
  "List of maps that assign the map values to ergoemacs-mode's
modification, instead of modifying them in the current active
maps.")

(defun ergoemacs-map-properties--set-map-p (keymap &rest _ignore)
  "Determines if ergoemacs-mode should make sure to set the original keymap to the caluclated value.

 This is useful in supporting isearch in emacs 24.4+."
  (when (ergoemacs-keymapp keymap)

    (let ((set-map-p (ergoemacs keymap :use-set-map-key)))
      (cond
       ((eq set-map-p 'no) nil)
       (set-map-p set-map-p)
       (ergoemacs-map-properties--set-map-list
        (let ((map-list (ergoemacs keymap :map-list)))
          (prog1 (catch 'found-use-local
                   (dolist (map map-list)
                     (when (memq map ergoemacs-map-properties--set-map-list)
                       (setq set-map-p t)
                       (throw 'found-use-local t)))
                   (setq set-map-p 'no)
                   nil)
            (ergoemacs keymap :use-set-map-key set-map-p))))
       (t nil)))))

(defvar ergoemacs-map-properties--major-modes-that-modify-global-keymap
  '(calc-mode calc-trail-mode calc-edit-mode)
  "List of major modes that modify the global map.")

(defvar ergoemacs-map-properties--ignore-global-changes-p nil
  "When set, `ergoemacs-mode' may ignore changes in the `global-map'")

(defun ergoemacs-map-properties--ignore-global-changes-p ()
  "Determines if `ergoemacs-mode' ignores the changes in the `global-map'."
  (or ergoemacs-map-properties--ignore-global-changes-p
      (memq major-mode ergoemacs-map-properties--major-modes-that-modify-global-keymap)))

(provide 'ergoemacs-map-properties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map-properties.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
