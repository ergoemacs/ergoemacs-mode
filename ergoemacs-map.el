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
(defvar ergoemacs-original-map-hash (make-hash-table)
  "Hash table of the original maps that `ergoemacs-mode' saves.")
(defvar ergoemacs-extract-keys-hash (make-hash-table :test 'equal))
(defvar ergoemacs-extract-keys--hash-1 nil)
(defvar ergoemacs-extract-keys--hash-2 nil)
(defvar ergoemacs-extract-keys--full-map nil)
(defvar ergoemacs-extract-keys--keymap nil)
(defvar ergoemacs-extract-keys--before-keys nil)
(defvar ergoemacs-extract-keys--prefixes nil)
(defvar ergoemacs-extract-keys--base-map nil)

(defun ergoemacs-extract-keys--handle-keymap (keymap cur-key compare)
  (ergoemacs-map--label keymap nil nil cur-key)
  (when ergoemacs-extract-keys--base-map
    (push cur-key ergoemacs-extract-keys--prefixes))
  (unless compare ;; Indicate this is a prefix key
    (puthash cur-key 'ergoemacs-prefix ergoemacs-extract-keys--hash-2))
  (if (ergoemacs-map-p keymap)
      ;; Bound submap, traverse submap later
      (push (plist-get (ergoemacs-map-p keymap) :map-list) ergoemacs-submaps--)
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
             (unless (eq key 'ranges)
               (if (consp key)
                   (progn
                     (message "Ignoring %s from %s to %s" def
                              (car key) (cdr key))
                     ;; (loop for char from (car key) to (cdr key)
                     ;;     do (define-key ret (or (and prefix (vconcat prefix (vector char)))
                     ;;                            (vector char)) def))
                     )
                 (when (vectorp key)
                   (cond
                    ((eq def 'ergoemacs-nil)
                     (define-key ret (or (and prefix (vconcat prefix key)) key) nil))
                    ((not (eq def 'ergoemacs-prefix))
                     (define-key ret (or (and prefix (vconcat prefix key)) key) def)))))))
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

(defun ergoemacs-extract-keys--loop (keymap flatten pre compare)
  (let ((parent-map (keymap-parent keymap)) composed-map-list tmp)
    (cond
     ((ergoemacs-map-composed-p keymap)
      ;; Extract composed maps first
      (setq composed-map-list (ergoemacs-map-composed-list keymap))
      (dolist (map composed-map-list)
        (ergoemacs-extract-keys--loop map flatten pre compare))
      ;; Extract parent map next
      (when parent-map
        (ergoemacs-extract-keys--loop parent-map flatten pre compare)))
     (parent-map
      ;; Extract mp then parent map
      (unwind-protect
          (progn
            (set-keymap-parent keymap nil)
            (ergoemacs-extract-keys--loop keymap flatten pre compare))
        (set-keymap-parent keymap parent-map))
      (ergoemacs-extract-keys--loop parent-map flatten pre compare))
     (t (dolist (key keymap) ;; Extract simple map
          (cond
           ((ignore-errors (char-table-p key))
            (setq ergoemacs-extract-keys--full-map t)
            (while key
              (map-char-table
               #'(lambda(key-2 value)
                   ;; Can be a list (from . to)
                   (if (consp key-2)
                       (if (not compare)
                           (progn
                             (puthash (ergoemacs-copy-list key-2) value ergoemacs-extract-keys--hash-2)
                             (setq tmp (gethash 'ranges ergoemacs-extract-keys--hash-2) )
                             (push (ergoemacs-copy-list key-2) tmp)
                             (puthash 'ranges tmp ergoemacs-extract-keys--hash-2))
                         ;; FIXME -- See if anything changed here...
                         )
                     (ergoemacs-extract-keys--puthash (or (and (vectorp pre) (vconcat pre (vector key-2)))
                                                          (vector key-2)) value compare)))
               key)
              (setq key (char-table-parent key))))
           ((ignore-errors (car key))
            (ergoemacs-extract-keys--puthash
             (or (and (vectorp pre) (integerp (car key)) (vconcat pre (vector (car key))))
                 (and (vectorp pre) (stringp (car key)) (= 1 (length (car key))) (vconcat pre (vector (get-byte 0 (car key)))))
                 (and (vectorp pre) (vconcat pre (vector (car key))))
                 (and (integerp (car key)) (vector (car key)))
                 (and (stringp (car key)) (=  1 (length (car key))) (vector (get-byte 0 (car key))))
                 (vector (car key))) (cdr key) compare))))))))

(defun ergoemacs-extract-keys (keymap &optional flatten pre compare)
  "Create a hash table of functions and their keys from a keymap.

FLATTEN will create a single keymap without submaps, parent maps,
or composed maps.

PRE represents the current prefix (for recursive calls).

COMPARE will compare differences to the current hash.
"
  (if (not (keymapp keymap)) nil
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
          (unless compare
            ;; Label any parent maps or composed maps.
            (when (ignore-errors (keymapp keymap))
              (setq tmp (keymap-parent keymap))
              (when tmp (ergoemacs-extract-keys tmp))
              (setq tmp (ergoemacs-map-composed-list keymap))
              (when tmp
                (dolist (map tmp)
                  (ergoemacs-extract-keys map)))))
          (ergoemacs-map--label keymap)
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
        (if (not (ignore-errors (keymapp keymap))) ergoemacs-extract-keys--hash-1
          (ergoemacs-extract-keys--loop keymap flatten pre compare)
          (unless pre
            (puthash :submaps ergoemacs-submaps--list ergoemacs-extract-keys--hash-2)
            (when ergoemacs-extract-keys--prefixes
              (puthash :prefixes ergoemacs-extract-keys--prefixes ergoemacs-extract-keys--hash-2))
            (when compare
              (puthash :changes-before-map ergoemacs-extract-keys--keymap ergoemacs-extract-keys--hash-2))
            (setq ret (list ergoemacs-extract-keys--hash-1 ergoemacs-extract-keys--hash-2))
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
          ret)))))

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
          basic modifiers new-mod
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

(defun ergoemacs-meta-to-escape (key-seq &optional modifier prefix)
  "Escapes a KEY-SEQ M-q becomes ESC q.
KEY-SEQ must be a vector.  If there is no need to escape the key sequence return nil."
  (ergoemacs-emacs-shift-translate key-seq 'meta 27))

(defun ergoemacs-key-submap (key map &optional not-recursive)
  "If KEY defines a key on a submap MAP, return the submap map name and key on it."
  (let (new-key
        prefix
        map-name
        (new-key (or (and (vectorp key) key)
                     (read-kbd-macro (key-description key) t)))
        ret)
    (when (catch 'found-submap
            (dolist (submap (ergoemacs-submaps map))
              (when (boundp (cadr submap))
                (when (equal new-key (car submap))
                  (setq prefix new-key)
                  (setq map (cadr submap))
                  (setq new-key nil)
                  (throw 'found-submap t))
                (when (> (length new-key) (length (car submap)))
                  (setq prefix (substring new-key 0 (length (car submap))))
                  (when (equal prefix  (car submap))
                    (setq map (cadr submap))
                    (setq new-key (substring new-key (length (car submap))))
                    (throw 'found-submap t))))) nil)
      (when (and new-key (not not-recursive))
        (setq ret (ergoemacs-key-submap new-key (symbol-value map))))
      (if ret
          (setq ret (list (vconcat prefix (nth 0 ret)) (or (nth 1 ret)
                                                           (list prefix new-key map)) (nth 2 ret)))
        (setq ret (list prefix new-key map))))
    ret))

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
         (prior (or (and (ignore-errors (keymapp before-ergoemacs-map)) (lookup-key before-ergoemacs-map key))
                    (gethash new-key hash-2)))
         tmp
         range 
         prefix)
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
                    (setq tmp (substring new-key (length (car submap))))
                    (throw 'found-submap t)))) nil)
        (setq prior (ergoemacs-prior-function tmp where-is before-ergoemacs map))
        (when where-is
          (setq prior (mapcar (lambda(x) (vconcat prefix x)) prior)))))
    
    (when (and (not prior) (= 1 (length new-key))
               (setq range (gethash 'ranges hash-2)))
      ;; Possibly a range?
      (catch 'found-range
        (dolist (r range)
          (when (and (integerp (aref new-key 0))
                     (>= (aref new-key 0) (car r))
                     (<= (aref new-key 0) (cdr r)))
            (setq prior (gethash r hash-2))
            (throw 'found-range t))) t))
    (unless prior
      ;; Instead of sequences like M-q, try ESC q
      (setq tmp (ergoemacs-meta-to-escape new-key))
      (when tmp
        (setq prior (ergoemacs-prior-function tmp where-is before-ergoemacs keymap))
        (when (eq prior 'self-insert-command)
          (setq prior nil))))
    prior))

(defcustom ergoemacs-ignore-prev-global t
  "If non-nil, the ergoemacs-mode will ignore previously defined global keybindings."
  :type 'boolean
  :group 'ergoemacs-mode)

;; for compatability 
;;;###autoload
(defun ergoemacs-ignore-prev-global ()
  "Ignore previously defined global keys."
  (setq ergoemacs-ignore-prev-global t))

(defun ergoemacs-global-changed-p (key)
  "Determines if the global KEY has changed"
  (let* ((key (or (and (vectorp key) key)
                  (read-kbd-macro (key-description key) t)))
         (after-changed (ergoemacs-map-get global-map :keys-after-changed))
         current prior)
    (if (member key after-changed) t
      (setq current (lookup-key global-map key)
            prior (ergoemacs-prior-function key nil (not ergoemacs-ignore-prev-global)))
      (unless (eq current prior)
        (when (keymapp current)
          (setq current 'ergoemacs-prefix)))
      (not (eq current prior)))))

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
  (ergoemacs-map--label-atoms)
  (ergoemacs-extract-keys global-map)
  (ergoemacs-extract-keys minibuffer-local-map)
  (ergoemacs-extract-keys minibuffer-local-ns-map)
  (ergoemacs-extract-keys minibuffer-local-completion-map)
  (ergoemacs-extract-keys minibuffer-local-must-match-map)
  (ergoemacs-extract-keys minibuffer-local-filename-completion-map)
  (with-temp-file (ergoemacs-default-global--file) 
    (let ((print-level nil)
          (print-length nil)
          tmp)
      (goto-char (point-min))
      (maphash
       (lambda(key _item)
         (setq tmp (plist-get key :map-list))
         (insert (format "(when (boundp '%s) (ergoemacs-map--label %s '%s nil nil '"
                         (nth 0 tmp) (nth 0 tmp) tmp))
         (prin1 (ergoemacs-map-plist (symbol-value (nth 0 tmp))) (current-buffer))
         (insert "))"))
       ergoemacs-extract-keys-hash)
      (insert "(setq ergoemacs-extract-keys-hash ")
      (prin1 ergoemacs-extract-keys-hash (current-buffer))
      (goto-char (point-max))
      (insert ")"))))

(defun ergoemacs-map--label-after-startup ()
  "Labels all maps after startup. Also label maps after everything has loaded."
  (ergoemacs-map--label-atoms)
  (add-hook 'after-load-functions 'ergoemacs-map--label-atoms))

(add-hook 'after-init-hook 'ergoemacs-map--label-after-startup)

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
        (ergoemacs-map-default-global)
        (ergoemacs-extract-keys global-map nil nil t)))))

(defun ergoemacs-extract-prefixes (keymap &optional dont-ignore return-vector)
  "Extract prefix commands for KEYMAP.
Ignores command sequences starting with `ergoemacs-ignored-prefixes'.

When DONT-IGNORE is non-nil, don't ignore sequences starting with `ergoemacs-ignored-prefixes'.

When RETURN-VECTOR is non-nil, return list of the keys in a vector form.
"
  (if (not (ignore-errors (keymapp keymap))) nil
    (ergoemacs-extract-keys keymap)
    (if (not (ergoemacs-map-p keymap))
        (warn "Can't identify keymap's prefixes")
      (let ((ret (ergoemacs-map-get keymap :prefixes)) ret2)
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
       ((and key (equal key [keymap]) (ignore-errors (keymapp item)))
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
Returns a plist of fixed keymap properties (not changed by
composing or parent/child relationships)"
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
      ;; Now get properties for constant keymaps
      (catch 'found-map
        (dolist (map ergoemacs-map--const-keymaps)
          (when (eq (cdr map) (cdr keymap))
            (setq ret (car map))
            (throw 'found-map t))))
      ret)))

(defun ergoemacs-map-get (keymap property)
  "Gets ergoemacs-mode KEYMAP PROPERTY."
  (cond
   ((eq property :full)
    (ignore-errors (char-table-p (nth 1 keymap))))
   (t (let ((ret (ergoemacs-map-plist keymap)))
        (or (and ret (or (plist-get ret property)
                         (gethash property (nth 1 (ergoemacs-extract-keys keymap)))))
            (and (not (eq property :map-list))
                 (gethash (ergoemacs-map-p keymap) ergoemacs-extract-keys-hash)
                 (gethash property (nth 1 (gethash (ergoemacs-map-p keymap) ergoemacs-extract-keys-hash)))))))))

(defun ergoemacs-map-put (keymap property value)
  "Set ergoemacs-mode KEYMAP PROPERTY to VALUE."
  (cond
   ((eq property :full)
    (warn "Cannot set the keymap property :full"))
   (t (let ((ret (ergoemacs-map-plist keymap)) tmp)
        (if (and ret (member property '(:submap-p :map-list)))
            (progn
              (setq ret (plist-put ret property value))
              (ergoemacs-map--label keymap nil nil nil ret))
          (puthash property value (nth 1 (ergoemacs-extract-keys keymap))))))))

(defun ergoemacs-map-composed-p (keymap)
  "Determine if the KEYMAP is a composed keymap."
  (and (ignore-errors (eq 'keymap (car keymap)))
       (ignore-errors (eq 'keymap (caadr keymap)))))

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

(defun ergoemacs-map-parent (keymap &optional force)
  "Returns a `ergoemacs-mode' map-list for the parent of KEYMAP."
  (let ((parent (keymap-parent keymap)))
    (and parent (ergoemacs-map-p parent force))))

(defun ergoemacs-map-composed (keymap &optional force)
  "Returns a list of `ergoemacs-mode' map-list for the composed keymap list"
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
  (let ((map-list (ergoemacs-map-get keymap :map-list))
        (composed (ergoemacs-map-composed keymap force))
        parent ret)
    (when (and force (not (or map-list composed)))
      (ergoemacs-map--label keymap)
      (setq map-list (ergoemacs-map-get keymap :map-list)
            composed (ergoemacs-map-composed keymap)
            parent (ergoemacs-map-parent keymap)))
    (when map-list
      (setq ret (plist-put ret :map-list map-list)))
    (when composed
      (setq ret (plist-put ret :composed composed)))
    (when (or map-list composed)
      (setq parent (ergoemacs-map-parent keymap t))
      (when parent
        (setq ret (plist-put ret :parent parent))))
    ret))

(defun ergoemacs-map-boundp (keymap &optional force)
  "Returns if the maps linked to the current map are unbound, if it is an `ergoemacs-mode' map.
When FORCE is on, figure out if it is bound."
  (let ((ret (symbol-name (car (ergoemacs-map-get keymap :map-list)))))
    (if (not (string= ret "nil"))
        (string-match-p "^ergoemacs-unbound-" ret)
      (if (not force) nil
        (ergoemacs-map--label keymap)
        (ergoemacs-map-boundp keymap)))))

(defvar ergoemacs-map--const-keymaps nil
  "Variable listing constant keymaps.")

(defun ergoemacs-map--label-atoms (&rest _ignore)
  "Label all the bound keymaps.
Also make a hash table of all original maps (linked based on :map-list)"
  (mapatoms
   (lambda(map)
     (let ((sv (ergoemacs-sv map t))
           omap
           ret)
       (when (keymapp sv)
         (setq ret (ergoemacs-map-get sv :map-list)
               omap (gethash ret ergoemacs-original-map-hash))
         ;; (when
         ;;   (remhash ret ergoemacs-original-map-hash))
         (if (and ret (string-match-p "^ergoemacs-unbound-" (symbol-name (nth 0 ret))))
             (setq ret '()))
         (pushnew map ret)
         ;; Hash should be a copy pointers of the original maps.
         (puthash ret (or omap (copy-keymap sv)) ergoemacs-original-map-hash)
         (ergoemacs-map--label sv ret))))))

(defun ergoemacs-original-keymap--intern (keymap-label)
  (let ((map-list (plist-get keymap-label :map-list))
        (composed (plist-get keymap-label :composed))
        (parent (plist-get keymap-label :parent))
        tmp
        ret)
    (cond
     (composed
      (dolist (map-label composed)
        (setq tmp (ergoemacs-original-keymap--intern map-label))
        (when (keymapp tmp)
          (push tmp ret)))
      (setq tmp (and parent (ergoemacs-original-keymap--intern parent)))
      (setq ret (make-composed-keymap tmp (and (keymapp tmp) tmp))))
     ((and (setq map-list (gethash map-list ergoemacs-original-map-hash))
           (keymapp map-list))
      (setq ret (copy-keymap map-list))
      (when parent
        (setq parent (ergoemacs-original-keymap--intern parent))
        (when (keymapp parent)
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

(defun ergoemacs-map--name (keymap)
  "Gets the first symbol pointing to this KEYMAP (if any)"
  (or
   (let ((ret (ergoemacs-map-p keymap))) (and ret (plist-get ret :map-list)))
   (let (ret)
     (unless ret
       (setq ret (list (intern (concat "ergoemacs-unbound-" (format-time-string "%s-%N"))))))
     ret)))

(defun ergoemacs-map--label (keymap &optional map-name strip submap-vector replace-plist)
  "Label an `ergoemacs-mode' touched keymap.
MAP-NAME is the identifier of the map name.
When STRIP is true, remove all `ergoemacs-mode' labels
The KEYMAP will have the structure
  (keymap optional-char-table \"Optional Label\" (ergoemacs-labeled (lambda nil (plist-of-properties))) true-map)
"
  (if (not (keymapp keymap)) nil
    (if (ergoemacs-map-composed-p keymap)
        (cond
         (map-name
          (warn "Will not label a composed map's members to %s" map-name))
         (replace-plist
          (warn "Will not update a plist for composed maps' members."))
         (t
          (dolist (map (ergoemacs-map-composed-list keymap))
            (ergoemacs-map--label map nil strip submap-vector nil))))
      (let* ((map keymap)
             (maps (or map-name (ergoemacs-map--name keymap)))
             (unbound-p (string-match-p  "^ergoemacs-unbound-" (symbol-name (nth 0 maps))))
             char-table
             old-plist
             (parent (keymap-parent map))
             label tmp1 tmp2)
        (unwind-protect
            (progn
              (ignore-errors (set-keymap-parent map nil))
              (setq old-plist (lookup-key map [ergoemacs-labeled]))
              (if (eq (car map) 'keymap)
                  (setq map (cdr map))
                (setq map (list map)))
              (when (char-table-p (car map))
                (setq char-table (pop map)))
              (when (stringp (car map))
                (setq label (pop map)))
              ;; Drop prior `ergoemacs-mode' labels
              (setq tmp1 '()
                    tmp2 nil)
              (when old-plist
                (setq old-plist (ignore-errors (funcall old-plist)))
                (while (not (eq (car tmp2) 'ergoemacs-labeled))
                  (setq tmp2 (pop map))
                  (unless (equal 'ergoemacs-labeled (car tmp2))
                    (push tmp2 tmp1)))
                (while tmp1
                  (push (pop tmp1) map)))
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
                (setq old-plist (plist-put old-plist ':map-list maps))
                (push (cons 'ergoemacs-labeled
                            `(lambda() (interactive) ',old-plist)) map))
              (when label
                (push label map))
              (when char-table
                (push char-table map))
              (push 'keymap map))
          (ignore-errors (set-keymap-parent map parent)))
        (unless (ignore-errors (ergoemacs-setcdr keymap (cdr map)))
          (pushnew (cons old-plist (cdr keymap)) ergoemacs-map--const-keymaps))
        map))))


(defvar ergoemacs-command-shortcuts-hash)
(defun ergoemacs-map--original (keymap)
  "Gets the original KEYMAP with `ergoemacs-mode' identifiers installed."
  (let ((map-name (ergoemacs-map-p keymap)))
    (if (not map-name)
        (let ((maps (ergoemacs-map--name keymap)))
          (ergoemacs-map--label keymap maps)
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

(defun ergoemacs-define-key--is-global-map (map key)
  "Determines if defining KEY the MAP will change the global-map.
This checks if the map is a submap of the global map.  If so, it
returns a list of keys affected.   Otherwise, it returns nil"
  (let ((submap-p (ergoemacs-submap-p map)) tmp (ret '()))
    (cond
     ((and submap-p (not (eq submap-p 'unknown)))
      (dolist (submap submap-p)
        (setq tmp
              (ergoemacs-define-key--is-global-map
               (symbol-value (nth 0 (plist-get (cdr submap) :map-list)))
               (or (and (vectorp key) (vconcat (car submap) key))
                   (car submap))))
        (when tmp
          (setq ret (append ret tmp)))))
     ((memq 'global-map (plist-get (ergoemacs-map-p map) :map-list))
      (setq ret (or (and key (list key)) t))))
    ret))


(declare-function ergoemacs-real-define-key "ergoemacs-map.el" (file form) t)
(fset 'ergoemacs-real-define-key (symbol-function 'define-key))

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
  (let ((key-vect (or (and (vectorp key) key)
                      (read-kbd-macro (key-description key) t)))
        (labeled-map-p (ergoemacs-map-p keymap)))
    (cond
     (labeled-map-p
      (ergoemacs-real-define-key (ergoemacs-original-keymap keymap) key def) ;; Should done in place..?
      (ergoemacs-real-define-key keymap key def))
     (t
      (ergoemacs-real-define-key keymap key def)))
    (dolist (global-key (ergoemacs-define-key--is-global-map keymap key-vect))
      (ergoemacs-global-set-key-after global-key))))

(defun ergoemacs-global-set-key-after (key)
  (if ergoemacs-ignore-advice nil
    (let ((kd (key-description key)) tmp)
      (unless (or (and (vectorp key)
                       (ignore-errors (memq (elt key 0) '(menu-bar 27 remap))))
                  ;; FIXME: don't unbind for packages that use
                  ;; global-set-key.  Like undo-tree
                  (and (not (vectorp key))
                       (string= "ESC" kd)))
        ;; Let `ergoemacs-mode' know these keys have changed.
        (setq tmp (ergoemacs-map-get global-map :keys-after-changed))
        (pushnew (read-kbd-macro kd t) tmp :test 'equal)
        (ergoemacs-map-put global-map :keys-after-changed tmp)
        ;; (pushnew kd ergoemacs-global-changed-cache :test 'equal)
        ;; (setq ergoemacs-global-not-changed-cache (delete kd ergoemacs-global-not-changed-cache))
        ;; Remove the key from `ergoemacs-mode' bindings
        (ergoemacs-theme-component--ignore-globally-defined-key key t)))))


;;; ergoemacs-events
(defvar ergoemacs-event-hash (make-hash-table)
  "Event modifiers not covered by standard emacs")

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
    (unless (and (integerp event)
                 (< event 1000)
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

(defvar ergoemacs-use-M-x-p nil)

(defvar ergoemacs-M-x)

(defface ergoemacs-pretty-key
  '((t :inverse-video t :box (:line-width 1 :style released-button) :weight bold))
  "Button Face for a `ergoemacs-mode' pretty key."
  :group 'ergoemacs-mode)

(defcustom ergoemacs-pretty-key-use-face t
  "Use a button face for keys."
  :group 'ergoemacs-mode)

(defun ergoemacs-pretty-key-description--key (key)
  "Key description"
  (let ((ret ""))
    (cond
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret (format "%sTab"
                        (ergoemacs-unicode-char "↹" ""))))
     ((eq key 'return)
      (setq ret (format "Enter%s"
                        (ergoemacs-unicode-char "⏎" ""))))
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
      (setq ret (make-string 1 key)))
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
                        (ergoemacs-unicode-char "⌘" "")))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret)))
     ((and (eq mod 'meta) ergoemacs-use-small-symbols
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%s"
                        (ergoemacs-unicode-char "⌥" "+"))))
     ((and (eq mod 'meta)
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%sOpt+"
                        (ergoemacs-unicode-char "⌥" "")))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret)))
     ((and ergoemacs-use-small-symbols (eq mod 'shift))
      (setq ret (ergoemacs-unicode-char "⇧" "+")))
     ((and ergoemacs-use-small-symbols (eq mod 'meta))
      (setq ret (ergoemacs-unicode-char "♦" "!")))
     ((and ergoemacs-use-small-symbols (eq mod 'control))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret (format "%sShift+"
                        (ergoemacs-unicode-char "⇧" "")))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret)))
     ((eq mod 'control)
      (setq ret "Ctrl+")
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret)))
     ((eq mod 'meta)
      (setq ret "Alt+")
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret)))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-pretty-key-use-face
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-pretty-key) ret))))
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
                          (ergoemacs-pretty-key-description--key ev)
                          (or (and ergoemacs-pretty-key-use-face "")
                              (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "】" "]"))
                              "]")))
        (when (and ergoemacs-use-small-symbols ergoemacs-pretty-key-use-face)
          (add-text-properties 0 (length tmp)
                               '(face ergoemacs-pretty-key) tmp))
        (setq ret (format "%s %s" ret tmp)))
      (substring ret 1))))


(defun ergoemacs-pretty-key (code)
  "Creates Pretty keyboard binding from kbd CODE from M- to Alt+"
  (if (not code) ""
    (save-match-data
      (if (string-match "^\\(M-x\\|<execute>\\) " code)
          (if ergoemacs-use-M-x-p
              code
            (replace-match ergoemacs-M-x t t code))
        (ergoemacs-pretty-key-description (read-kbd-macro code t))))))



(provide 'ergoemacs-map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-map.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
