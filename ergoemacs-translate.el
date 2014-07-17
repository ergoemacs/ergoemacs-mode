;;; ergoemacs-translate.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

;; Filename: ergoemacs-translate.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:08:09 2013 (-0500)
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


;;; ergoemacs pretty keys
(defvar ergoemacs-shortcut-emulation-mode-map-alist)
(defvar ergoemacs-no-shortcut-emulation-mode-map-alist)
(defvar ergoemacs-read-input-keys)
(defvar ergoemacs-shortcut-keys)
(defvar ergoemacs-no-shortcut-keys)
(defvar ergoemacs-modal)
(defvar ergoemacs-unbind-keys)
(defvar ergoemacs-emulation-mode-map-alist)
(defvar ergoemacs-mode)
(defvar ergoemacs-ignore-advice)
(defvar ergoemacs-read-local-emulation-mode-map-alist)

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

;; FIXME: invalidate/fix cache when changing.
(defcustom ergoemacs-use-small-symbols nil
  "Use small symbols to represent alt+ ctl+ etc. on windows/linux."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-use-M-x-p nil)

(defvar ergoemacs-M-x)
(defun ergoemacs-pretty-key (code)
  "Creates Pretty keyboard binding from kbd CODE from M- to Alt+"
  (if (not code) ""
    (save-match-data
      (if (string-match "^\\(M-x\\|<execute>\\) " code)
          (if ergoemacs-use-M-x-p
              code
            (replace-match ergoemacs-M-x t t code))
        (let* ((ob (or (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "【" "[")) "["))
               (cb (or (and ergoemacs-use-unicode-brackets (ergoemacs-unicode-char "】" "]")) "]"))
               (ret (concat ob (replace-regexp-in-string
                                " +$" "" (replace-regexp-in-string "^ +" "" code)) cb))
               (case-fold-search nil)
               (pt 0))
          (when ergoemacs-use-ergoemacs-key-descriptions
            (while (string-match "<f\\([0-9]+\\)>" ret pt)
              (setq ret (replace-match "<F\\1>" t nil ret)
                    pt (match-end 0)))
            (setq pt 0)
            (while (string-match "\\(-[A-Z]\\)\\>\\([^-]\\|$\\)" ret pt)
              (setq pt (+ (match-end 0) 2)
                    ret (replace-match (format "-S%s%s" (downcase (match-string 1 ret)) (match-string 2 ret)) t t ret)))
            (setq pt 0)
            (while (string-match "\\_<[A-Z]\\_>" ret pt)
              (setq pt (+ (match-end 0) 2)
                    ret (replace-match (format "S-%s" (match-string 0 ret)) t t ret)))
            (setq pt 0)
            (while (string-match "\\(S-\\)\\{2,\\}" ret pt)
              (setq pt (+ (match-beginning 0) 2)
                    ret (replace-match "S-" t t ret)))
            (setq pt 0)
            (while (string-match "<\\(\\(?:C-\\|S-\\|M-\\)?[A-Za-z0-9]+\\)>" ret pt)
              (setq pt (- (match-end 0) 2)
                    ret (replace-match (match-string 1 ret) t t ret)))
            (dolist (args `(("\\<M-" ,(if (eq system-type 'darwin)
                                          (cond
                                           ((or (and (boundp 'mac-command-modifier)
                                                     (eq mac-command-modifier 'meta))
                                                (and (boundp 'ns-command-modifier)
                                                     (eq ns-command-modifier 'meta)))
                                            (format "%sCmd+"
                                                    (ergoemacs-unicode-char "⌘" "")))
                                           ((or (and (boundp 'mac-alternate-modifier)
                                                     (eq mac-alternate-modifier 'meta))
                                                (and (boundp 'ns-alternate-modifier)
                                                     (eq ns-alternate-modifier 'meta)))
                                            (format "%sOpt+"
                                                    (ergoemacs-unicode-char "⌥" "")))
                                           (t "Alt+"))
                                        "Alt+"))
                            ("\\<C-" "Ctrl+")
                            ("\\<S-" ,(format "%sShift+"
                                              (ergoemacs-unicode-char "⇧" "")))
                            ("\\<\\(RET\\|[Rr]eturn\\)\\>" ,(format "Enter%s"
                                                                    (ergoemacs-unicode-char "⏎" "")))
                            ("\\<TAB\\>" ,(format "%sTab"
                                                  (ergoemacs-unicode-char "↹" "")))
                            ("\\_<\\(menu\\|apps\\)\\_>" ,(format "%s"
                                                                  (ergoemacs-unicode-char "▤" "Menu")))
                            ("\\_<prior\\_>" "PgUp")
                            ("\\_<next\\_>" "PgDn")
                            ("\\_<left\\_>" ,(ergoemacs-unicode-char "←" "left"))
                            ("\\_<right\\_>" ,(ergoemacs-unicode-char "→" "right"))
                            ("\\_<up\\_>" ,(ergoemacs-unicode-char "↑" "up"))
                            ("\\_<down\\_>" ,(ergoemacs-unicode-char "↓" "down"))

                            ("+left\\_>"  ,(concat "+" (ergoemacs-unicode-char "←" "left")))
                            ("+right\\_>" ,(concat "+" (ergoemacs-unicode-char "→" "right")))
                            ("+up\\_>"    ,(concat "+" (ergoemacs-unicode-char "↑" "up")))
                            ("+down\\_>"  ,(concat "+" (ergoemacs-unicode-char "↓" "down")))
                            (" +" ,(concat cb ob))
                            (,(regexp-quote (concat cb " +" ob)) ,(concat cb ob))))
              (setq pt 0)
              (while (string-match (nth 0 args) ret pt)
                (setq pt (+ (length (nth 1 args)) (match-beginning 0))
                      ret (replace-match (nth 1 args) t t ret))))
            (setq pt 0)
            (while (string-match "[+]\\([[:lower:]]\\)\\(】\\|\\]\\)" ret pt)
              (setq ret (replace-match (upcase (match-string 0 ret)) t t ret)
                    pt (match-end 0)))
            (setq pt 0)
            (cond
             ((and (eq system-type 'darwin)
                   (string= "⇧" (ergoemacs-unicode-char "⇧" ""))
                   (string= "⌘" (ergoemacs-unicode-char "⌘" ""))
                   (string= "⌥" (ergoemacs-unicode-char "⌥" "")))
              (dolist (args `((".Opt[+]"  "⌥")
                              (".Cmd[+]" "⌘")
                              (".Shift[+]" "⇧")
                              (".Ctr?l[+]" "^")))
                (setq pt 0)
                (while (string-match (nth 0 args) ret pt)
                  (setq pt (+ (length (nth 1 args)) (match-beginning 0))
                        ret (replace-match (nth 1 args) t t ret)))))
             ((and ergoemacs-use-small-symbols
                   (string= "⇧" (ergoemacs-unicode-char "⇧" ""))
                   (string= "♦" (ergoemacs-unicode-char "♦" "")))
              (dolist (args `((".Alt[+]"  "♦")
                              (".Shift[+]" "⇧")
                              (".Ctr?l[+]" "^")))
                (setq pt 0)
                (while (string-match (nth 0 args) ret pt)
                  (setq pt (+ (length (nth 1 args)) (match-beginning 0))
                        ret (replace-match (nth 1 args) t t ret)))))))
          ret)))))



;;; Actual Translations
(defvar ergoemacs-translation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (read-kbd-macro "<deletechar>") (read-kbd-macro "DEL"))
    map)
  "Map of defined translations that are applied if the original key wasn't found.")

(defvar ergoemacs-needs-translation nil
  "Tells if ergoemacs keybindings need a translation")

(defvar ergoemacs-translation-from nil
  "Translation from keyboard layout")

(defvar ergoemacs-translation-to nil
  "Translation to keyboard layout")

(defvar ergoemacs-translation-assoc nil
  "Translation alist")

(defvar ergoemacs-translation-regexp nil
  "Translation regular expression")

(defvar ergoemacs-unshifted-regexp nil
  "Unshifted regular expression.")

(defvar ergoemacs-shifted-regexp nil
  "Shifted regular expression.")

(defvar ergoemacs-shifted-assoc nil
  "Translation alist.")

(defvar ergoemacs-universal-fns
  '(universal-argument ergoemacs-universal-argument)
  "Universal argument functions")
(defvar ergoemacs-translate-hash (make-hash-table :test 'equal))
(defvar ergoemacs-translations (make-hash-table :test 'equal))
(defvar ergoemacs-translation-text (make-hash-table :test 'equal))

(defun ergoemacs-reset-translations ()
  "Resets translations."
  (setq ergoemacs-translate-hash (make-hash-table :test 'equal))
  (setq ergoemacs-translations (make-hash-table :test 'equal))
  (setq ergoemacs-translation-text (make-hash-table :test 'equal))
  (setq ergoemacs-universal-fns '(universal-argument ergoemacs-universal-argument)))

(defun ergoemacs-local-map (type &optional modal)
  "Gets local keymap for TYPE, or returns nil.
If MODAL is true, get the modal override map."
  (let ((map (intern-soft (concat "ergoemacs-" (symbol-name type) (if modal "-modal-map" "-translation-local-map")))))
    (if (not map) nil
      (symbol-value map))))

(defun ergoemacs-translation (&rest arg-plist)
  "Add or modifies an ergoemacs-translation.

The argument ARG-PLIST should be a plist with the following properties:

:name -- name of translation, should be a symbol
:text -- Text to display while completing this translation
:keymap -- Local Keymap for translation
:keymap-modal -- Modal keymap for overrides.
:modal-always -- If the modal state is always on, regardless of
                 the values of  `ergoemacs-modal-ignored-buffers',
                `ergoemacs-modal-emacs-state-modes' `minibufferp'
The following arguments allow the keyboard presses to be translated:
 - :alt
 - :ctl
 - :shift
 - :alt-ctl
 - :alt-shift
 - :ctl-shift
 - :alt-ctl-shift
 - :unchorded (no modifiers)

This will be called by `ergoemacs-translate'.

The translations plists are stored in `ergoemacs-translations'.
The keymap translation text is stored in `ergoemacs-translation-text'

This also creates functions:
- ergoemacs-NAME-universal-argument
- ergoemacs-NAME-digit-argument
- ergoemacs-NAME-negative-argument
- ergoemacs-NAME-modal
"
  ;; Take off all the `ergoemacs-translate' hashes
  ;; (setq ergoemacs-translate-hash (make-hash-table :test 'equal))
  (let ((ret-plist arg-plist)
        (keymap (plist-get arg-plist ':keymap))
        (keymap-modal (plist-get arg-plist ':keymap-modal))
        tmp
        (trans-text '())
        (pretty-trans '())
        (key-text '())
        (key-pretty '()))
    (setq ret-plist (plist-put ret-plist ':keymap-modal nil))
    (setq ret-plist (plist-put ret-plist ':keymap nil))

    (eval (macroexpand
           `(defvar ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-modal-map"))
              ',keymap-modal
              ,(concat "Ergoemacs modal override map for "
                      (symbol-name (plist-get arg-plist ':name))
                      " translation.
This keymap is made in `ergoemacs-translation'"))))

    (eval (macroexpand
           `(defvar ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-translation-local-map"))
              ',keymap
              ,(concat "Ergoemacs translation local map for "
                      (symbol-name (plist-get arg-plist ':name))
                      " translation setup.
This keymap is made in `ergoemacs-translation'"))))
    
    ;; Create the universal argument functions.
    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-universal-argument")) ()
              ,(concat "Ergoemacs universal argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-universal-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-universal-argument ',(plist-get arg-plist ':name)))))
    (add-to-list 'ergoemacs-universal-fns
                 (intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-universal-argument")))

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-digit-argument")) ()
              ,(concat "Ergoemacs digit argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-digit-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-digit-argument ',(plist-get arg-plist ':name)))))

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-negative-argument")) ()
              ,(concat "Ergoemacs digit argument, with "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation setup.
This is called through `ergoemacs-negative-argument'.
This function is made in `ergoemacs-translation'")
              (interactive)
              (ergoemacs-negative-argument ',(plist-get arg-plist ':name)))))

    (eval (macroexpand
           `(defun ,(intern (concat "ergoemacs-" (symbol-name (plist-get arg-plist ':name)) "-modal")) ()
              ,(concat "Toggle modal "
                       (symbol-name (plist-get arg-plist ':name))
                       " translation.
This function is made in `ergoemacs-translation' and calls `ergoemacs-modal-toggle'.")
              (interactive)
              (ergoemacs-modal-toggle ',(plist-get arg-plist ':name)))))
    
    
    ;; Now put the translation text together as a list.
    (dolist (x '((:alt "M-")
                 (:ctl "C-")
                 (:shift "S-")
                 (:alt-ctl "M-C-")
                 (:alt-shift "M-S-")
                 (:ctl-shift "C-S-")
                 (:alt-ctl-shift "C-M-S-")))
      (let ((trans (plist-get arg-plist (nth 0 x)))
            (orig (nth 1 x))
            case-fold-search)
        (when trans
          (push (concat orig (ergoemacs-unicode-char "→" "->") trans)
                trans-text)
          (push (concat
                 (replace-regexp-in-string
                  "[Qq]" ""
                  (ergoemacs-pretty-key (concat orig "q")))
                 (ergoemacs-unicode-char "→" "->")
                 (replace-regexp-in-string
                  "[Qq]" ""
                  (ergoemacs-pretty-key (concat trans "q"))))
                pretty-trans))))
    ;; Now get keys that change the next key's behavior
    (when keymap
      (dolist (x '((ergoemacs-read-key-next-key-is-alt "M-")
                   (ergoemacs-read-key-next-key-is-ctl "C-")
                   (ergoemacs-read-key-next-key-is-alt-ctl "C-M-")
                   (ergoemacs-read-key-next-key-is-quoted "")))
        (let ((key (where-is-internal (nth 0 x) keymap t))
              (trans (nth 1 x)))
          (when key
            (setq key (key-description key))
            (push (concat key (ergoemacs-unicode-char "→" "->") trans)
                  key-text)
            (push (concat
                   (ergoemacs-pretty-key key)
                   (ergoemacs-unicode-char "→" "->")
                   (replace-regexp-in-string
                    "[Qq]" ""
                    (ergoemacs-pretty-key (concat trans "q"))))
                  key-pretty)))))
    (setq tmp '("" ""))
    (when (plist-get arg-plist ':unchorded)
      (setq tmp (list (plist-get arg-plist ':unchorded)
                      (replace-regexp-in-string
                       "[Qq]" ""
                       (ergoemacs-pretty-key
                        (concat (plist-get arg-plist ':unchorded) "q"))))))
    (puthash (plist-get arg-plist ':name)
             (list trans-text pretty-trans
                   key-text key-pretty tmp (or (plist-get arg-plist ':text) ""))
             ergoemacs-translation-text)
    ;; Replace/Add translation
    (puthash (plist-get arg-plist ':name) ret-plist
             ergoemacs-translations)))

;; Reset translations in case this is re-sourced
(ergoemacs-reset-translations)

;; Add ergoemacs-mode default translations
(ergoemacs-translation
 :name 'normal
 :keymap (let ((map (make-sparse-keymap))
               (ergoemacs-ignore-advice t))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "C-h") 'ergoemacs-read-key-help)
           map))

(ergoemacs-translation
 :name 'ctl-to-alt
 :text (format "<Ctl%sAlt> " (ergoemacs-unicode-char "↔" " to "))
 :alt "C-"
 :ctl "M-"
 :modal-color "blue"
 :modal-always t
 :keymap (let ((map (make-sparse-keymap))
               (ergoemacs-ignore-advice t))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "M-h") 'ergoemacs-read-key-help)
           (define-key map (if (eq system-type 'windows-nt) [M-apps] [M-menu]) 'ergoemacs-read-key-next-key-is-quoted)
           (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-next-key-is-ctl)
           (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-next-key-is-alt)
           
           ;; (define-key map "G" 'ergoemacs-read-key-next-key-is-quoted)
           ;; (define-key map "g" 'ergoemacs-read-key-next-key-is-alt)
           map))

(ergoemacs-translation
 :name 'unchorded
 :text "<Ctl+>"
 :unchorded "C-"
 :alt ""
 :ctl "M-"
 :keymap (let ((map (make-sparse-keymap))
               (ergoemacs-ignore-advice t))
           (define-key map [f1] 'ergoemacs-read-key-help)
           (define-key map (read-kbd-macro "SPC") 'ergoemacs-read-key-next-key-is-quoted)
           (define-key map (read-kbd-macro "M-SPC") 'ergoemacs-read-key-next-key-is-alt-ctl)
           (define-key map "g" 'ergoemacs-read-key-next-key-is-alt)
           (define-key map "G" 'ergoemacs-read-key-next-key-is-alt-ctl)
           map))

(ergoemacs-translation
 :name 'unchorded-alt
 :text "<Alt+>"
 :unchorded "M-"
 :shift "M-S-"
 :alt "M-S-"
 :modal-color "red"
 :keymap-modal (let ((map (make-sparse-keymap))
                     (ergoemacs-ignore-advice t))
                 (define-key map (read-kbd-macro "<return>") 'ergoemacs-unchorded-alt-modal)
                 (define-key map (read-kbd-macro "RET") 'ergoemacs-unchorded-alt-modal)
                 map))

(defun ergoemacs-translate-shifted (kbd)
  "Translates anything with S- and no C- in it to an upper-case character.
Translates C-A into C-S-a."
  (if (not kbd) nil
    (let ((ret kbd)
          case-fold-search)
      (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\)" ret)
        (if (string-match "C-" ret)
            (when (and (string-match "\\(.\\)$" ret)
                       (string= (upcase (match-string 1 ret))
                                (match-string 1 ret))
                       (not (string= (downcase (match-string 1 ret))
                                     (match-string 1 ret))))
              (setq ret
                    (replace-match
                     (concat "S-" (downcase (match-string 1 ret))) t t ret)))
          (when (string-match "^\\(.*\\)S-\\(.*\\)\\(.\\)$" ret)
            (setq ret (concat (match-string 1 ret)
                              (match-string 2 ret)
                              (upcase (match-string 3 ret)))))))
      ret)))

(defun ergoemacs-shift-translate-install (trans-plist ret-plist)
  "Install shift translation"
  (let (shift-translated
        (name (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated")))
        (k (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated-key")))
        (p (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-shift-translated-pretty")))
        (key (plist-get ret-plist (intern (concat ":" (symbol-name (plist-get trans-plist ':name))))))
        (ret ret-plist))
    (unless (stringp key)
      (setq key (key-description key)))
    (setq shift-translated key)
    (cond
     ((string-match "S-" key)
      (setq shift-translated (replace-regexp-in-string "S-" "" key t)))
     ((string-match "-\\(.\\)$" key)
      (setq shift-translated
            (replace-match
             (concat "-"
                     (downcase (match-string 1 key))) t t key))))
    (unless (string= shift-translated key)
      (setq ret (plist-put ret name shift-translated))
      (setq ret (plist-put ret k (read-kbd-macro shift-translated t)))
      (setq ret (plist-put ret p (ergoemacs-pretty-key shift-translated))))
    ret))

(defun ergoemacs-translation-install (trans-plist orig-key ret-plist)
  "Installs the translation.
TRANS-PLIST is the plist defining the translation.
ORIG-KEY is the original kbd-code
RET-PLIST is  the plist that the translation will be installed into.

Should install
 - :translation-name kbd-code
 - :translation-name-key [key-vector]
 - :translation-name-pretty ergoemacs-pretty-key

If the command can be shift translated, then the following
properties are also added:

 - :translation-name-shift-translated kbd
 - :translation-name-shift-translated-key [key-vector]
 - :translation-name-shift-translated-pretty ergoemacs-pretty-key
"
  (let ((name (intern (concat ":" (symbol-name (plist-get trans-plist ':name)))))
         (key (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-key")))
         (pretty (intern (concat ":" (symbol-name (plist-get trans-plist ':name)) "-pretty")))
         (ret ret-plist)
         case-fold-search
         new-key)
    (cond
     ;; Trifecta
     ((string-match "C-M-S-" orig-key)
      (if (plist-get trans-plist ':alt-ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Double keys
     ((string-match "C-S-" orig-key)
      (if (plist-get trans-plist ':ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "C-M-" orig-key)
      (if (plist-get trans-plist ':alt-ctl)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-S-" orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Emacs saves these as M-A instead of M-S-a 
     ((string-match (format "M%s" ergoemacs-shifted-regexp) orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key
                  (concat (plist-get trans-plist ':alt-shift)
                          (cdr (assoc (match-string 2 orig-key) ergoemacs-shifted-assoc))))
        (setq new-key orig-key)))
     ;; Single Key combinations
     ((string-match "C-" orig-key)
      (if (plist-get trans-plist ':ctl)
          (setq new-key
                (concat (plist-get trans-plist ':ctl)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-" orig-key)
      (if (plist-get trans-plist ':alt)
          (setq new-key
                (concat (plist-get trans-plist ':alt)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match ergoemacs-shifted-regexp orig-key)
      (if (plist-get trans-plist ':shift)
          (setq new-key
                (concat (plist-get trans-plist ':shift)
                        (cdr (assoc (match-string 2 orig-key) ergoemacs-shifted-assoc))))
        (setq new-key orig-key)))
     ;; Unchorded
     ((plist-get trans-plist ':unchorded)
      (setq new-key
            (concat (plist-get trans-plist ':unchorded) orig-key)))
     (t
      (setq new-key orig-key)))
    (setq new-key (ergoemacs-translate-shifted new-key))
    (setq ret (plist-put ret name new-key))
    (setq ret (plist-put ret key (read-kbd-macro new-key t)))
    (setq ret (plist-put ret pretty (ergoemacs-pretty-key new-key)))
    (setq ret (ergoemacs-shift-translate-install trans-plist ret))
    ret))

(defun ergoemacs-translate (key)
  "Translates KEY and returns a plist of the translations.

:shift-translated
    S-a    -> a
    M-S-a  -> M-a
    C-S-a  -> C-a
    Anything without shift is nil.

All other translations are defined in `ergoemacs-translations'.

There are also :XXX-key and :XXX-pretty for actual key-strokes
and `ergoemacs-pretty-key' descriptions.

"
  (let* ((ret (gethash key ergoemacs-translate-hash))
         (orig-key key)
         case-fold-search
         only-key
         shift-translated
         (ergoemacs-use-ergoemacs-key-descriptions t)
         shifted-key
         unshifted-key)
    (if ret ret
      (unless (stringp key)
        (setq key (key-description key)
              orig-key key))
      (cond
       ((string-match "\\(^<.+>$\\|SPC\\|DEL\\|ESC\\|RET\\|TAB\\)" key)
        (setq only-key (replace-regexp-in-string "[CMS]-" "" key t))
        (if (string-match "S-" key)
            (setq shifted-key (replace-match "" t nil key))
          (setq shifted-key (concat "S-" key))))
       (t
        (setq only-key (replace-regexp-in-string "^.*\\(.\\)$" "\\1" key t)
              shifted-key (assoc only-key ergoemacs-shifted-assoc))
        (when shifted-key
          (setq shifted-key (cdr shifted-key)))))
      (when (and (string-match "\\([A-Z]\\)$" key)
                 (not (string-match "\\<\\(SPC\\|DEL\\|ESC\\|RET\\|TAB\\)\\>" key)))
        (setq key
              (replace-match
               (concat "S-" (downcase (match-string 1 key))) t t key)))
      (when shifted-key
        (setq unshifted-key only-key)
        (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\)" shifted-key)
          (when (string-match "[A-Z]" shifted-key)
            (setq shifted-key (concat "S-" (downcase shifted-key))))
          (when (string-match "[A-Z]" unshifted-key)
            (setq unshifted-key (concat "S-" (downcase unshifted-key))))))
      (when (string-match "S-" key)
        (setq shift-translated (replace-regexp-in-string "S-" "" key t)))
      
      (if shift-translated
          (progn
            (setq ret (plist-put ret ':shift-translated (ergoemacs-translate-shifted shift-translated)))
            (setq ret (plist-put ret ':shift-translated-key (read-kbd-macro (ergoemacs-translate-shifted shift-translated) t)))
            (setq ret (plist-put ret ':shift-translated-pretty (ergoemacs-pretty-key shift-translated))))
        (setq ret (plist-put ret ':shift-translated nil))
        (setq ret (plist-put ret ':shift-translated-key nil))
        (setq ret (plist-put ret ':shift-translated-pretty nil)))
      
      (when shifted-key
        (setq ret (plist-put ret ':shifted (ergoemacs-translate-shifted shifted-key)))
        (setq ret (plist-put ret ':shifted-key (read-kbd-macro (ergoemacs-translate-shifted shifted-key) t)))
        (setq ret (plist-put ret ':shifted-pretty (ergoemacs-pretty-key shifted-key))))
      (when unshifted-key
        (setq ret (plist-put ret ':unshifted (ergoemacs-translate-shifted unshifted-key)))
        (setq ret (plist-put ret ':unshifted-key (read-kbd-macro (ergoemacs-translate-shifted unshifted-key) t)))
        (setq ret (plist-put ret ':unshifted-pretty (ergoemacs-pretty-key unshifted-key))))
      (setq ret (plist-put ret ':ctl (ergoemacs-translate-shifted
                                      (concat "C-" unshifted-key))))
      (setq ret (plist-put ret ':ctl-key (read-kbd-macro (plist-get ret ':ctl) t)))
      (setq ret (plist-put ret ':ctl-pretty (ergoemacs-pretty-key (plist-get ret ':ctl))))

      (setq ret (plist-put ret ':raw (ergoemacs-translate-shifted
                                      (replace-regexp-in-string
                                       "\\<[CSMS]-" "" key))))
      (setq ret (plist-put ret ':raw-key  (read-kbd-macro (plist-get ret ':raw) t)))
      (setq ret (plist-put ret ':raw-pretty (ergoemacs-pretty-key
                                             (plist-get ret ':raw))))
      (if (assoc (plist-get ret ':raw) ergoemacs-shifted-assoc)
          (progn
            (setq ret (plist-put ret ':raw-shift
                                 (ergoemacs-translate-shifted
                                  (replace-regexp-in-string
                                   "\\<[CSM]-" ""
                                   (cdr (assoc (plist-get ret ':raw) ergoemacs-shifted-assoc))))))
            (setq ret (plist-put ret ':raw-shift-key
                                 (read-kbd-macro (plist-get ret ':raw-shift) t)))
            (setq ret (plist-put ret ':raw-shift-pretty
                                 (ergoemacs-pretty-key
                                  (plist-get ret ':raw-shift)))))
        (setq ret (plist-put ret ':raw-shift nil))
        (setq ret (plist-put ret ':raw-shift-key nil))
        (setq ret (plist-put ret ':raw-shift-pretty nil)))
      
      (setq ret (plist-put ret ':alt (ergoemacs-translate-shifted
                                      (concat "M-" unshifted-key))))
      (setq ret (plist-put ret ':alt-key (read-kbd-macro (plist-get ret ':alt) t)))
      (setq ret (plist-put ret ':alt-pretty (ergoemacs-pretty-key (plist-get ret ':alt))))
      
      (when unshifted-key
        (setq ret (plist-put ret ':alt-ctl (ergoemacs-translate-shifted
                                            (concat "M-C-" unshifted-key))))
        (setq ret (plist-put ret ':alt-ctl-key (read-kbd-macro (plist-get ret ':alt-ctl) t)))
        (setq ret (plist-put ret ':alt-ctl-pretty (ergoemacs-pretty-key (plist-get ret ':alt-ctl)))))

      (when shifted-key
        (setq ret (plist-put ret ':ctl-shift (ergoemacs-translate-shifted
                                              (concat "C-" shifted-key))))
        (setq ret (plist-put ret ':ctl-shift-key (read-kbd-macro (plist-get ret ':ctl-shift) t)))
        (setq ret (plist-put ret ':ctl-shift-pretty (ergoemacs-pretty-key (plist-get ret ':ctl-shift))))
        (setq ret (plist-put ret ':alt-shift (ergoemacs-translate-shifted
                                              (concat "M-" shifted-key))))
        (setq ret (plist-put ret ':alt-shift-key (read-kbd-macro (plist-get ret ':alt-shift) t)))
        (setq ret (plist-put ret ':alt-shift-pretty (ergoemacs-pretty-key (plist-get ret ':alt-shift))))
        (setq ret (plist-put ret ':alt-ctl-shift (ergoemacs-translate-shifted
                                                  (concat "M-C-" shifted-key))))
        (setq ret (plist-put ret ':alt-ctl-shift-key (read-kbd-macro (plist-get ret ':alt-ctl-shift) t)))
        (setq ret (plist-put ret ':alt-ctl-shift-pretty (ergoemacs-pretty-key (plist-get ret ':alt-ctl-shift)))))
      (maphash
       (lambda(_ignore plist)
         (setq ret (ergoemacs-translation-install plist orig-key ret)))
       ergoemacs-translations)
      (puthash orig-key ret ergoemacs-translate-hash)
      (puthash key ret ergoemacs-translate-hash)
      ret)))

(defun ergoemacs-setup-translation (layout &optional base-layout)
  "Setup translation from BASE-LAYOUT to LAYOUT."
  (let ((orig-base (or base-layout "us"))
        lay shifted-list unshifted-list base
        len i)
    (setq lay (symbol-value (intern (concat "ergoemacs-layout-" layout))))
    (setq base (symbol-value (intern (concat "ergoemacs-layout-" orig-base))))
    
    (setq len (length base))
    (setq i 0)
    (while (< i 60)
      (unless (or (string= "" (nth i lay))
                  (string= "" (nth (+ i 60) lay)))
        ;; Add to list is incompatible with lexical scoping.  However
        ;; this use is OK since `ergoemacs-shifted-assoc' is defined
        ;; in a defvar statement.
        (add-to-list 'ergoemacs-shifted-assoc
                     `(,(nth i lay) . ,(nth (+ i 60) lay)))
        (add-to-list 'ergoemacs-shifted-assoc
                     `(,(nth (+ i 60) lay) . ,(nth i lay)))
        (pushnew (nth i lay)
                 unshifted-list
                 :test 'equal)
        (pushnew (nth (+ i 60) lay)
                 shifted-list
                 :test 'equal))
      (setq i (+ i 1)))
    (setq ergoemacs-shifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt shifted-list nil)))
    (setq ergoemacs-unshifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt unshifted-list nil)))
    (unless (and (string= layout ergoemacs-translation-to)
                 (string= orig-base ergoemacs-translation-from))
      (if (equal layout orig-base)
          (progn
            (setq ergoemacs-translation-from orig-base)
            (setq ergoemacs-translation-to layout)
            (setq ergoemacs-needs-translation nil)
            (setq ergoemacs-translation-assoc nil)
            (setq ergoemacs-translation-regexp nil))
        (setq ergoemacs-translation-from orig-base)
        (setq ergoemacs-translation-to layout)
        (setq ergoemacs-needs-translation t)
        (setq ergoemacs-translation-assoc nil)
        (setq len (length base))
        (setq i 0)
        (while (< i len)
          (unless (or (string= "" (nth i base))
                      (string= "" (nth i lay)))
            (add-to-list 'ergoemacs-translation-assoc
                         `(,(nth i base) . ,(nth i lay))))
          (setq i (+ i 1)))
        (setq ergoemacs-translation-regexp
              (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                      (regexp-opt (mapcar (lambda(x) (nth 0 x))
                                          ergoemacs-translation-assoc) nil)))))
    ;; Pre-cache the translations...?  Takes too long to load :(
    (when nil
      (dolist (char (append lay '("<f1>"  "<S-f1>"
                                  "<f2>"  "<S-f2>"
                                  "<f3>"  "<S-f3>"
                                  "<f4>"  "<S-f4>"
                                  "<f5>"  "<S-f5>"
                                  "<f6>"  "<S-f6>"
                                  "<f7>"  "<S-f7>"
                                  "<f8>"  "<S-f8>"
                                  "<f9>"  "<S-f9>"
                                  "<f10>" "<S-f10>"
                                  "<f11>" "<S-f11>"
                                  "<f12>" "<S-f12>"
                                  "SPC" "RET" "ESC" "DEL" "TAB"
                                  "<home>" "<S-home>"
                                  "<next>" "<S-next>"
                                  "<prior>" "<S-prior>"
                                  "<end>" "<S-end>"
                                  "<insert>" "<S-insert>"
                                  "<deletechar>" "<S-deletechar>")))
        (unless (string= "" char)
          (ergoemacs-translate char)
          (ergoemacs-translate (concat "C-" char))
          (ergoemacs-translate (concat "M-" char))
          (ergoemacs-translate (concat "M-C-" char)))))))

(declare-function ergoemacs-mode-line "ergoemacs-mode.el")
(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  ;; Set appropriate mode-line indicator
  (ergoemacs-mode-line))

(defvar ergoemacs-kbd-hash (make-hash-table :test 'equal))
;; This is called so frequently make a hash-table of the results.

(defun ergoemacs-kbd (key &optional just-translate only-first)
  "Translates kbd code KEY for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'.
If JUST-TRANSLATE is non-nil, just return the KBD code, not the actual emacs key sequence.
"
  (save-match-data
    (if (not key)
        nil
      (let ((new-key (gethash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to)
                              ergoemacs-kbd-hash)))
        (if new-key
            new-key
          (setq new-key key)
          (cond
           ((eq system-type 'windows-nt)
            (setq new-key (replace-regexp-in-string "<menu>" "<apps>" new-key)))
           (t
            (setq new-key (replace-regexp-in-string "<apps>" "<menu>" new-key))))
          ;; Translate Alt+ Ctl+ or Ctrl+ to M- and C-
          (setq new-key (replace-regexp-in-string "[Aa][Ll][Tt][+]" "M-" new-key))
          (setq new-key (replace-regexp-in-string "[Cc][Tt][Rr]?[Ll][+]" "C-" new-key))
          (when ergoemacs-needs-translation
            (setq new-key
                  (with-temp-buffer
                    (insert new-key)
                    (goto-char (point-min))
                    (when (re-search-forward ergoemacs-translation-regexp nil t)
                      (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                      (skip-chars-backward " "))
                    (when (not only-first)
                      (while (re-search-forward ergoemacs-translation-regexp nil t)
                        (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                        (skip-chars-backward " ")))
                    (buffer-string))))
          (let ((case-fold-search t))
            (setq new-key (replace-regexp-in-string
                           ">>" ">"
                           (replace-regexp-in-string
                            "<<" "<"
                            (replace-regexp-in-string
                             "\\<\\(ENTER\\|enter\\)\\>" "<return>" new-key t t)))))
          (if (not just-translate)
              (or (ignore-errors (read-kbd-macro new-key t))
                  (read-kbd-macro (encode-coding-string new-key locale-coding-system) t))
            (puthash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to) new-key
                     ergoemacs-kbd-hash)
            new-key))))))

(defcustom ergoemacs-change-fixed-layout-to-variable-layout nil
  "Change the fixed layout to variable layout keys.
For example, on dvorak, change C-j to C-c (copy/command)."
  :type 'boolean
  :set 'ergoemacs-set-default
  :initialize #'custom-initialize-default
  :group 'ergoemacs-mode)

(defun ergoemacs-get-kbd-translation (pre-kbd-code)
  "This allows a translation from the listed kbd-code and the true kbd code."
  (let ((ret (replace-regexp-in-string
              "[Cc]\\(?:on\\)?tro?l[+-]" "C-"
              (replace-regexp-in-string
               "[Aa]lt[+-]" "M-" pre-kbd-code))))
    ret))

(defvar ergoemacs-keymap)
(defvar ergoemacs-unbind-keymap)
(defvar ergoemacs-shortcut-keymap)
(defvar ergoemacs-no-shortcut-keymap)
(defvar ergoemacs-command-shortcuts-hash)
(defun ergoemacs-key-fn-lookup (function &optional use-apps)
  "Looks up the key binding for FUNCTION based on.
Based on `ergoemacs-with-ergoemacs'"
  (ergoemacs-with-ergoemacs
   (let ((ret (where-is-internal function)))
     (maphash
      (lambda (key val)
        (let ((fn (nth 0 val)))
          (when (eq fn function)
            (push key ret))))
      ergoemacs-command-shortcuts-hash)
     (if (not use-apps)
         (while (and ret (eq (elt (nth 0 ret) 0) 'apps))
           (setq ret (cdr ret)))
       (while (and ret (not (eq (elt (nth 0 ret) 0) 'apps)))
         (setq ret (cdr ret))))
     (setq ret (nth 0 ret))
     ret)))

(provide 'ergoemacs-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-translate.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
