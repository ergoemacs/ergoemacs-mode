;;; ergoemacs-key-description.el --- Ergoemacs map interface -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-key-description.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Sep 28 20:10:56 2013 (-0500)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;; Ergoemacs-mode key description library.
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

(require 'cl-lib)

(eval-when-compile
  (require 'ergoemacs-macros))

(require 'descr-text)
(require 'faces)
(require 'help-mode)

(defvar ergoemacs-use-unicode-symbols)
(defvar ergoemacs-display-unicode-characters)
(defvar ergoemacs-display-capitalize-keys)
(defvar ergoemacs-display-key-use-face-p)
(defvar ergoemacs-display-small-symbols-for-key-modifiers)
(defvar ergoemacs-display-use-unicode-brackets-around-keys)
(defvar ergoemacs-display-without-brackets nil
  "Display the key without brackets.")

(declare-function ergoemacs-timing-- "ergoemacs-mode")

(declare-function ergoemacs-translate--escape-to-meta "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifiers "ergoemacs-translate")
(declare-function ergoemacs-translate--event-basic-type "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifier-hash "ergoemacs-translate")

(declare-function ergoemacs-map-properties--composed-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-fixed-plist "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--map-list "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-lessp "ergoemacs-map-properties")
(declare-function ergoemacs-map-properties--key-hash "ergoemacs-map-properties")

(declare-function ergoemacs-map--cache-- "ergoemacs-map")

(declare-function ergoemacs-component--help-link "ergoemacs-component")

(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")
(declare-function ergoemacs-advice--real-substitute-command-keys "C")

(defvar ergoemacs-key-description--display-char-cache nil
  "List of characters and fonts and if they display or not.")

(defun ergoemacs-key-description--display-char-p (char)
  "Determines if CHAR can be displayed."
  (cond
   ((= 1 (length char))
    (ignore-errors
      (let* (ret
             (buf (current-buffer))
             (face (font-xlfd-name (face-attribute 'default :font)))
             (found (assoc (list face char window-system) ergoemacs-key-description--display-char-cache)))
        (if found
            (nth 0 (cdr found))
          (switch-to-buffer (get-buffer-create " *ergoemacs-display-char-p*") t)
          (delete-region (point-min) (point-max))
          (insert char)
          (let ((display (describe-char-display (point-min) (char-after (point-min)))))
            (if (display-graphic-p (selected-frame))
                (if display
                    (setq ret (font-at (point-min))))
              (if display
                  (setq ret (font-at (point-min))))))
          (switch-to-buffer buf)
          ;; Save it so the user doesn't see the buffer popup very much
          ;; (if at all).
          (push (list (list face char window-system) ret) ergoemacs-key-description--display-char-cache)
          ret))))
   ((stringp char)
    (catch 'does-not-display
      (dolist (c (mapcar (lambda(x) (make-string 1 x)) (vconcat char)))
        (when (not (ergoemacs-key-description--display-char-p c))
          (throw 'does-not-display nil)))
      t))
   (t nil)))

(defun ergoemacs-key-description--unicode-char--internal (char alt-char)
  "Return CHAR if it can be displayed, otherwise use ALT-CHAR.
This assumes `ergoemacs-display-unicode-characters' is non-nil.  When
`ergoemacs-display-unicode-characters' is nil display ALT-CHAR"
  (if (and ergoemacs-display-unicode-characters
           (ergoemacs-key-description--display-char-p char))
      char
    alt-char))

(defun ergoemacs-key-description--unicode-char (&rest chars)
  "Return the first dispalyable character in CHARS.
This uses `ergoemacs-key-description--unicode-char--internal'"
  (if ergoemacs-use-unicode-symbols
      (let* ((char-list chars)
             (test-char (pop char-list))
             tmp
             (next-char test-char))
        (catch 'found-char
          (while char-list
            (setq next-char (pop char-list)
                  tmp (ergoemacs-key-description--unicode-char--internal test-char next-char))
            (if (string= tmp next-char)
                (setq test-char next-char)
              (throw 'found-char tmp)))
          next-char))
    (car (last chars))))

(defun ergoemacs-key-description--key (key mod)
  "Key description.
KEY is the fundamental event of a key.
MOD ar the modifiers applied to the key."
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
      (setq ret (format "%sBackspace" (ergoemacs :unicode-or-alt "←" ""))))
     ((eq key 'escape)
      (setq ret "Esc"))
     ((eq key 'tab)
      (setq ret (format "Tab%s"
                        (ergoemacs :unicode-or-alt "↹" ""))))
     ((eq key 'return)
      (setq ret (format "Enter%s"
                        (ergoemacs :unicode-or-alt "↵" ""))))
     ((memq key '(apps menu))
      (setq ret (ergoemacs :unicode-or-alt "▤" "Menu")))
     ((eq key 'left)
      (setq ret (ergoemacs :unicode-or-alt "←" "left")))
     ((eq key 'right)
      (setq ret (ergoemacs :unicode-or-alt "→" "right")))
     ((eq key 'up)
      (setq ret (ergoemacs :unicode-or-alt "↑" "up")))
     ((eq key 'down)
      (setq ret (ergoemacs :unicode-or-alt "↓" "down")))
     ((eq key 'prior)
      (setq ret "PgUp"))
     ((eq key 'next)
      (setq ret "PgDn"))
     ((eq key 'remap)
      (setq ret (ergoemacs :unicode-or-alt "➩" "remap")))
     ((eq key 'ergoemacs-timeout)
      (setq ret (ergoemacs :unicode-or-alt "⌚" "ergoemacs-timeout")))
     ((integerp key)
      (setq ret (or (and (or (and (eq ergoemacs-display-capitalize-keys 'with-modifiers)
                                  mod)
                             (eq ergoemacs-display-capitalize-keys t))
                         (upcase (make-string 1 key)))
                    (make-string 1 key))))
     ((and (symbolp key) (string-match "^f[0-9]+$" (symbol-name key)))
      (setq ret (upcase (symbol-name key))))
     (t
      (setq ret (format "%s" key))))
    (when (and ergoemacs-display-key-use-face-p
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (length ret)
                           '(face ergoemacs-display-key-face) ret))
    ret))

(defun ergoemacs-key-description--modifier (mod)
  "Modifier MOD description."
  (let (ret)
    (cond
     ;; OSX specific key descriptions
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%s"
                        (ergoemacs :unicode-or-alt "⌘" "+"))))
     ((and (eq mod 'meta)
           (eq system-type 'darwin)
           (or (and (boundp 'mac-command-modifier)
                    (eq mac-command-modifier 'meta))
               (and (boundp 'ns-command-modifier)
                    (eq ns-command-modifier 'meta))))
      (setq ret (format "%sCmd+"
                        (ergoemacs :unicode-or-alt "⌘" "+"))))
     ((and (eq mod 'meta)
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%sOpt+" (ergoemacs :unicode-or-alt "⌥" "+"))))
     ((and (eq mod 'meta) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'darwin)
           (or (and (boundp 'mac-alternate-modifier)
                    (eq mac-alternate-modifier 'meta))
               (and (boundp 'ns-alternate-modifier)
                    (eq ns-alternate-modifier 'meta))))
      (setq ret (format "%s" (ergoemacs :unicode-or-alt "⌥" "+"))))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'shift))
      (setq ret (ergoemacs :unicode-or-alt "⇧" "+")))
     ((and ergoemacs-display-small-symbols-for-key-modifiers (eq mod 'meta))
      (setq ret (ergoemacs :unicode-or-alt "♦" "!")))
     ((and (or (eq system-type 'darwin) ergoemacs-display-small-symbols-for-key-modifiers)
           (memq mod '(control ergoemacs-control)))
      (setq ret "^"))
     ((eq mod 'shift)
      (setq ret (format "%sShift+"
                        (ergoemacs :unicode-or-alt "⇧" ""))))
     ((memq mod '(control ergoemacs-control))
      (setq ret (format "%sCtrl+"
                        (or (and (eq 'windows-nt system-type)
                                 (ergoemacs :unicode "✲" ""))
                            (and (eq 'gnu/linux system-type)
                                 (ergoemacs :unicode "⎈" ""))
                            ""))))
     ((eq mod 'meta)
      (setq ret "Alt+"))
     ((and (eq mod 'super) ergoemacs-display-small-symbols-for-key-modifiers
           (eq system-type 'windows-nt))
      (setq ret (ergoemacs :unicode-or-alt "⊞" "#")))
     ((and (eq mod 'super)
           (eq system-type 'windows-nt))
      (setq ret (format "%sWin+" (ergoemacs :unicode-or-alt "⊞" "#"))))
     (t
      (setq ret (format "%s+" mod))
      (when ergoemacs-display-key-use-face-p
        (add-text-properties 0 (- (length ret) 1)
                             '(face ergoemacs-display-key-face) ret))))
    (when (and ergoemacs-display-key-use-face-p
               (not ergoemacs-display-small-symbols-for-key-modifiers))
      (add-text-properties 0 (- (length ret) 1)
                           '(face ergoemacs-display-key-face) ret))
    ret))

(defun ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers (mod)
  "Change `ergoemacs-mode' special modifiers in MOD to the Emacs modifiers."
  (let ((tmp '()))
    (dolist (m mod)
      (cond
       ((eq m 'ergoemacs-control)
        (push 'control tmp))
       ((eq m 'control))
       (t
        (push m tmp))))
    tmp))

(defun ergoemacs-key-description--menu (kbd &optional layout)
  "Create pretty keyboard bindings for menus.
KBD is the keyboard code, LAYOUT is the keyboard layout."
  (let ((ergoemacs-display-without-brackets t)
        (ergoemacs-display-key-use-face-p nil)
        (ergoemacs-display-small-symbols-for-key-modifiers nil))
    (ergoemacs-key-description kbd layout)))


(defun ergoemacs-key-description (kbd &optional layout)
  "Create Pretty keyboard binding from kbd from M- to Alt+.

KBD is the keyboard code.  LAYOUT is the layout that is used."
  (if (not kbd) ""
    (let ((kbd (or (ergoemacs-translate--escape-to-meta kbd)
                   (and (stringp kbd) (vconcat kbd)) kbd)))
      (if (eq kbd (vector)) ""
        (let ((ret "")
              tmp
              mod ev)
          (dolist (key (listify-key-sequence kbd))
            (setq mod (ergoemacs-translate--event-modifiers key)
                  ev (ergoemacs-translate--event-basic-type key))
            (cond
             ((and (memq 'control mod) (eq ev ?\[))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'escape))
             ((and (memq 'control mod) (eq ev ?m))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'return))
             ((and (memq 'control mod) (eq ev ?i))
              (setq mod (ergoemacs-key-description--add-emacs-modifiers-for-ergoemacs-modifiers mod)
                    ev 'tab))
             ((memq 'ergoemacs-shift mod)
              (setq tmp '())
              (dolist (m mod)
                (unless (eq m 'ergoemacs-shift)
                  (push m tmp)))
              (setq mod tmp
                    ev (ergoemacs-gethash (intern (format "s%s" ev))
                                          (ergoemacs-translate--event-modifier-hash layout)))))
	    (when (memq 'ergoemacs-gui mod)
	      (setq tmp '())
              (dolist (m mod)
                (unless (eq m 'ergoemacs-gui)
                  (push m tmp)))
	      (setq mod tmp))
            (setq tmp (format "%s%s%s%s"
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face-p) "")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys (ergoemacs :unicode-or-alt "【" "["))
                                  "[")
                              (mapconcat #'ergoemacs-key-description--modifier
                                         mod "")
                              (ergoemacs-key-description--key ev mod)
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face-p) "")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys (ergoemacs :unicode-or-alt "】" "]"))
                                  "]")))
            (when (and ergoemacs-display-small-symbols-for-key-modifiers ergoemacs-display-key-use-face-p)
              (add-text-properties 0 (length tmp)
                                   '(face ergoemacs-display-key-face) tmp))
            (setq ret (format "%s%s%s" ret
                              (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face-p) " ")
                                  (and ergoemacs-display-use-unicode-brackets-around-keys "")
                                  " ") tmp)))
          (substring ret (or (and (or ergoemacs-display-without-brackets ergoemacs-display-key-use-face-p) 1)
                             (and ergoemacs-display-use-unicode-brackets-around-keys 0)
                             1)))))))

(defun ergoemacs-key-description-kbd (code)
  "Create `ergoemacs-mode' style description of kbd macro CODE."
  (if (not code) ""
    (save-match-data
      (ergoemacs-key-description (read-kbd-macro code t)))))

(defvar ergoemacs-describe-keymap--ignore
  (append '(again
            begin
            compose-last-chars
            copy
            cut
            delete
            delete-frame
            deleteline
            execute
            find
            header-line
            help
            iconify-frame
            insertline
            language-change
            left-fringe
            lwindow
            make-frame-visible
            menu-bar
            mode-line
            mouse-movement
            open
            paste
            redo
            ;; remap
            right-fringe
            rwindow
            select-window
            switch-frame
            tool-bar
            undo
            vertical-line
            vertical-scroll-bar
            XF86Back
            XF86Forward)
          (if (eq system-type 'windows-nt) '(menu) '(apps)))
  "Ignored prefixes of keymaps.")

(defvar ergoemacs-describe-keymap--column-widths '(18 . 40)
  "Column widths for key description tables.")

(defun ergoemacs-key-description--keymap-item-2 (item)
  "Get the description of ITEM for the table."
  (cond
   ((or (vectorp item) (stringp item))
    (ergoemacs-key-description item))
   ((listp item)
    (cond
     ((eq (car item) 'lambda) (cons nil (ergoemacs :unicode-or-alt "λ" "lambda")))
     ((eq (car item) 'closure) (cons nil "#<closure>"))
     ((eq (car item) 'keymap) (cons nil "#<keymap>"))
     (t (format "%s" item))))
   ((symbolp item)
    (if (ignore-errors (commandp item t))
        (cons 'help-function (format "%s" item))
      (cons nil (format "%s" item))))
   (t (cons nil (format"#<byte compiled %s>" (ergoemacs :unicode-or-alt "λ" "lambda"))))))

(defun ergoemacs-key-description--keymap-blame (key map)
  "Find the source of KEY in MAP."
  (let (composed-list parent ret tmp)
    (cond
     ((not map) (setq ret ""))
     ((not key) (setq ret ""))
     ((ergoemacs-keymapp map)
      (setq composed-list (ergoemacs :composed-list map)
            parent (keymap-parent map))
      (catch 'found-source
        (when composed-list
          (dolist (cur-map composed-list)
            (when (and (ergoemacs-keymapp cur-map)
                       (setq tmp (lookup-key cur-map key))
                       (not (integerp tmp)))
              (setq ret (ergoemacs cur-map :map-key))
              (throw 'found-source t)))))
      (when (and parent (not composed-list))
        (unwind-protect
            (progn
              (set-keymap-parent map nil)
              (when (and (setq tmp (lookup-key map key)) (not (integerp tmp)))
                (setq ret (ergoemacs map :map-key))))
          (set-keymap-parent map parent)))
      (when (and (not parent) (not composed-list) (setq tmp (lookup-key map key)) (not (integerp tmp)))
        (setq ret (ergoemacs map :map-key)))
      (when (and (not ret) parent)
        (setq ret (ergoemacs-key-description--keymap-blame key parent)))))
    (cond
     ((and ret (integerp ret))
      (if (and (setq tmp (ergoemacs ret :map-list))
               (setq tmp (nth 0 tmp)))
          (setq ret (cons 'help-variable (format "%s" tmp)))
        (setq ret (cons nil "?"))))
     ((and ret (consp ret) (consp (cdr ret)))
      (setq ret (cons 'ergoemacs-component-help (nth 1 ret)))))
    ret))

(defun ergoemacs-key-description--setup-xrefs ()
  "Setup cross refecnes in help buffer."
  (ergoemacs-component--help-link))

(add-hook 'temp-buffer-show-hook 'ergoemacs-key-description--setup-xrefs)

(defun ergoemacs-key-description--keymap-item (&optional elt keymap help)
  "Get keymap description for ELT based on KEYMAP.

When HELP is non-nil, assume this is a help buffer and insert the keymap item.

Otherwise return the value."
  (let* ((column-widths ergoemacs-describe-keymap--column-widths)
         (last-column (- 80 (+ (car column-widths) (cdr column-widths) 3)))
         (kd (or (and (consp elt) (ergoemacs-key-description (car elt)))
                 (and (eq elt t) (make-string (- (car column-widths) 2) ?-))
                 "Key"))
         (item (or (and (consp elt) (ergoemacs-key-description--keymap-item-2 (cdr elt)))
                   (and (eq elt t) (make-string (- (cdr column-widths) 2) ?-))
                   "Command"))
         (key-item (format "%s%s%s" kd (make-string (max 1 (- (car column-widths) (length kd))) ? )
                           (or (and (consp item) (cdr item)) item)))
         (src (or (and (consp elt) (ergoemacs-key-description--keymap-blame (car elt) keymap))
                  (and (eq elt t) (make-string (- last-column 2) ?-))
                  "Source"))
         type item-type)
    (when (consp src)
      (setq type (car src)
            src (format "%s" (cdr src))))
    (when (consp item)
      (setq item-type (car item)
            item (format "%s" (cdr item))))
    (if (not help)
        (format "%s%s%s" key-item  (make-string (max 1 (- (+ (car column-widths) (cdr column-widths)) (length key-item))) ? ) src)
      (insert key-item)
      (when (and item-type (looking-back (regexp-quote item) nil))
        (help-xref-button 0 item-type (intern (match-string 0))))
      (insert (make-string (max 1 (- (+ (car column-widths) (cdr column-widths)) (length key-item))) ? ))
      (insert src)
      (when (and type (looking-back (regexp-quote src) nil))
        (help-xref-button 0 type (intern (match-string 0)))))))

(defun ergoemacs-key-description--keymap (map &optional help)
  "Describes the keymap MAP.

When HELP is non-nil, insert and add help cross-refences."
  (let ((map (or (and (symbolp map) (symbol-value map))
                 (and (consp map) (eq (car map) 'keymap) map)))
        ret)
    (ergoemacs-timing describe-keymap
      (setq ret
            (ergoemacs-cache (intern (format "describe-keymap-ret%s" (mapconcat (lambda(x) (number-to-string x)) (ergoemacs  map :key-hash) "_")))
              (ergoemacs-map-keymap
               (lambda (cur-key item)
                 (unless (eq item 'ergoemacs-prefix)
                   (cond
                    ((consp cur-key))
                    ((memq (elt cur-key 0) ergoemacs-describe-keymap--ignore))
                    ((consp item))
                    ((not item))
                    (t
                     (push (cons cur-key item) ret)))))
               map)
              ret)))
    (setq ret (append (list nil t) (sort ret (lambda(e1 e2) (ergoemacs :key-lessp (car e1) (car e2))))))
    (if help
        (insert (ergoemacs-cache (intern (format "describe-keymap-help%s" (mapconcat (lambda(x) (number-to-string x)) (ergoemacs  map :key-hash) "_")))
                  (with-temp-buffer
                    (dolist (x ret)
                      (ergoemacs-key-description--keymap-item x map t)
                      (insert "\n"))
                    (buffer-string))))
      (ergoemacs-cache (intern (format "describe-keymap%s" (mapconcat (lambda(x) (number-to-string x)) (ergoemacs  map :key-hash) "_")))
        (concat "\n" (mapconcat (lambda(x) (ergoemacs-key-description--keymap-item x map)) ret "\n"))))))

(defun ergoemacs-key-description--substitute-command-keys (string)
  "Substitute key descriptions for command names in STRING.
A replacement for `substitute-command-keys'."
  (if (not (stringp string)) nil
    (let (quote-p
	  slash-p
          command-p
          command
          mapvar-p
          mapvar
          map-p
          current-map
          tmp
          rep-item
          cur-item
          ret)
      (dolist (elt (append (vconcat string) nil))
        (cond
	 ((and (boundp 'text-quoting-style) (memq elt '(8216 96))) ;; Left quote
          (cond
	   (quote-p
	    (setq ret (concat ret (make-string 1 elt))
		  quote-p nil))
	   ((or (eq text-quoting-style 'curve)
		(eq text-quoting-style nil))
	    (setq ret (concat ret (make-string 1 8216))))
	   ((eq text-quoting-style 'straight)
	    (setq ret (concat ret "'")))
	   ((eq text-quoting-style 'grave)
            (setq ret (concat ret "`")))))
         ((and (boundp 'text-quoting-style) (memq elt '(8217 39))) ;; Right quote
	  (cond
           (quote-p
            (setq ret (concat ret (make-string 1 elt))
                  quote-p nil))
           ((or (eq text-quoting-style 'curve)
                (eq text-quoting-style nil))
            (setq ret (concat ret (make-string 1 8217))))
           ((eq text-quoting-style 'straight)
            (setq ret (concat ret "'")))
           ((eq text-quoting-style 'grave)
            (setq ret (concat ret "'")))))
	 ;;\
         ((= ?\\ elt)
          (setq slash-p t
                mapvar nil
                command nil))
         ;;\[
         ((and slash-p (= 91 elt))
          (if quote-p
	      (setq ret (concat ret "\\[")
		    slash-p nil
		    quote-p nil)
	    (setq command-p t
		  slash-p   nil)))
         ;;\{
         ((and slash-p (= 123 elt))
          (if quote-p
	      (setq ret (concat ret "\\{")
		    slash-p nil
                    quote-p nil)
	    (setq slash-p  nil
		  mapvar-p t)))
         ;;\<
         ((and slash-p (= ?< elt))
          (if quote-p
	      (setq ret (concat ret "\\<")
		    slash-p nil
                    quote-p nil)
	    (setq slash-p  nil
		  map-p t)))
	 ;;\=
	 ((and slash-p (= ?= elt))
	  (if quote-p
	      (setq ret (concat ret "\\=")
		    slash-p nil
                    quote-p nil)
	    (setq quote-p t
		  slash-p nil)))
         ;;\<...>
         ((and map-p (= ?> elt))
          (setq map-p nil
                current-map (intern mapvar)))
         (map-p
          (setq mapvar (or (and mapvar (concat mapvar (make-string 1 elt)))
                           (make-string 1 elt))))
         ;;\{...}
         ((and mapvar-p (= 125 elt))
          (setq mapvar-p nil
                mapvar (intern mapvar)
                rep-item "")
          (and
           (boundp mapvar)
           (setq rep-item (ergoemacs-key-description--keymap mapvar)))
          (setq ret (concat ret rep-item)))
         (mapvar-p
          (setq mapvar (or (and mapvar (concat mapvar (make-string 1 elt)))
                            (make-string 1 elt))))
         ;;\[...]
         ((and command-p (= 93 elt))
          ;; Completed, look for command.
          (setq command-p nil
                command (intern command)
                rep-item command
                cur-item (where-is-internal command (symbol-value current-map) t))
          (if cur-item
              (setq rep-item (ergoemacs-key-description cur-item))
            (setq rep-item
                  (format "%s %s" (or (and (setq tmp (where-is-internal 'execute-extended-command nil t)) (ergoemacs-key-description tmp)) "M-x")
                          rep-item)))
          (setq ret (concat ret rep-item)))
         (command-p
          (setq command (or (and command (concat command (make-string 1 elt)))
                            (make-string 1 elt))))
	 (t
	  (when slash-p
	    (setq slash-p nil
		  ret (concat ret "\\")))
	  (setq quote-p nil
		command-p nil
		mapvar-p nil
		map-p nil
		ret (concat ret (make-string 1 elt))))))
      ret)
    ))

(defalias 'ergoemacs-substitute-command-keys 'ergoemacs-key-description--substitute-command-keys)

(provide 'ergoemacs-key-description)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-key-description.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
