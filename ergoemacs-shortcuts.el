;;; ergoemacs-shortcuts.el --- Ergoemacs shortcuts interface
;;
;; Filename: ergoemacs-shortcuts.el
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defcustom ergoemacs-translate-keys t
  "When translating extracted keymaps, attempt to translate to
the best match."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-setup-keys-for-keymap---internal (keymap key def)
  "Defines KEY in KEYMAP to be DEF"
  (cond
   ((eq 'cons (type-of def))
    (let (found)
      (if (condition-case err
              (stringp (nth 0 def))
            (error nil))
          (if (and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
              (progn
                (puthash (read-kbd-macro (key-description key) t)
                         `(,(nth 0 def) ,(nth 1 def))
                         ergoemacs-command-shortcuts-hash)
                (define-key ergoemacs-shortcut-keymap key
                  'ergoemacs-shortcut))
            (unless (lookup-key keymap key)
              (define-key keymap key
                `(lambda(&optional arg)
                   (interactive "P")
                   (setq this-command last-command) ; Don't record this command.
                   ;; (setq prefix-arg current-prefix-arg)
                   (ergoemacs-shortcut-internal ,(nth 0 def) ',(nth 1 def))))))
        (mapc
         (lambda(new-def)
           (unless found
             (when (condition-case err
                       (interactive-form new-def)
                     (error nil))
               (setq found
                     (ergoemacs-setup-keys-for-keymap---internal keymap key new-def)))))
         def))
      (symbol-value 'found)))
   ((condition-case err
        (interactive-form def)
      (error nil))
    (cond
     ;; only setup on `ergoemacs-shortcut-keymap' when setting up
     ;; ergoemacs default keymap.
     ((and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap
           (memq def '(ergoemacs-ctl-c ergoemacs-ctl-x)))
      (define-key ergoemacs-shortcut-keymap key def))
     ((and (not (string-match "\\(mouse\\|wheel\\)" (key-description key)))
           (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap
           (or (remove-if '(lambda(x) (eq 'menu-bar (elt x 0))) ; Ignore
                                        ; menu-bar
                                        ; functions
                          (where-is-internal def (current-global-map)))
               (gethash def ergoemacs-where-is-global-hash)))
      
      (puthash (read-kbd-macro (key-description key) t)
               (list def 'global) ergoemacs-command-shortcuts-hash)
      (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut))     
     ((or (and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
          (not (lookup-key keymap key)))
      (define-key keymap key def)))
    t)
   ((condition-case err
        (keymapp (symbol-value def))
      (error nil))
    (define-key keymap key (symbol-value def))
    t)
   ((condition-case err
        (stringp def)
      (error nil))
    (if (and (boundp 'setup-ergoemacs-keymap) setup-ergoemacs-keymap)
        (progn
          (puthash (read-kbd-macro (key-description key) t)
                   `(,def nil)
                   ergoemacs-command-shortcuts-hash)
          (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut))
      (unless (lookup-key keymap key)
        (define-key keymap key
          `(lambda(&optional arg)
             (interactive "P")
             (setq this-command last-command) ; Don't record this command.
             ;; (setq prefix-arg current-prefix-arg)
             (ergoemacs-shortcut-internal ,def)))))
    
    t)
   (t nil)))

(defmacro ergoemacs-setup-keys-for-keymap (keymap)
  "Setups ergoemacs keys for a specific keymap"
  `(condition-case err
       (let ((no-ergoemacs-advice t)
             (case-fold-search t)
             key
             trans-key
             cmd cmd-tmp)
         (ergoemacs-debug-heading ,(format "Setup keys for %s" (symbol-name keymap)))
         (if (eq ',keymap 'ergoemacs-keymap)
             (setq ,keymap (make-sparse-keymap))
           (ergoemacs-debug "Theme: %s" ergoemacs-theme))
         ;; Fixed layout keys
         (ergoemacs-debug-heading "Setup Fixed Keys")
         (mapc
          (lambda(x)
            (when (and (eq 'string (type-of (nth 0 x))))
              (setq trans-key (ergoemacs-get-kbd-translation (nth 0 x)))              
              (if ergoemacs-change-fixed-layout-to-variable-layout
                  (progn ;; Change to the fixed keyboard layout.
                    (setq key (ergoemacs-kbd trans-key)))
                (condition-case err
                    (setq key (read-kbd-macro trans-key t))
                  (error
                   (setq key (read-kbd-macro
                              (encode-coding-string
                               trans-key
                               locale-coding-system) t)))))
              (if (eq ',keymap 'ergoemacs-keymap)
                  (ergoemacs-debug "Fixed %s: %s -> %s %s (%s)"
                                   (nth 0 x) trans-key cmd
                                   key (key-description key)))
              (if (ergoemacs-global-changed-p trans-key)
                  (progn
                    (ergoemacs-debug "!!!Fixed %s has changed globally." trans-key)
                    (ergoemacs-setup-keys-for-keymap---internal ,keymap key (lookup-key (current-global-map) key)))
                (setq cmd (nth 1 x))
                (when (not (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd))
                  (ergoemacs-debug "Key %s->%s not setup." key cmd)))))
          (symbol-value (ergoemacs-get-fixed-layout)))
         (ergoemacs-debug-heading "Setup Variable Layout Keys")
         ;; Variable Layout Keys
         (mapc
          (lambda(x)
            (when (and (eq 'string (type-of (nth 0 x))))
              (setq trans-key
                    (ergoemacs-get-kbd-translation (nth 0 x)))
              (setq key (ergoemacs-kbd trans-key nil (nth 3 x)))
              (if (ergoemacs-global-changed-p trans-key t)
                  (progn
                    (ergoemacs-debug "!!!Variable %s (%s) has changed globally."
                                     trans-key (ergoemacs-kbd trans-key t (nth 3 x))))
                ;; Add M-O and M-o handling for globally defined M-O and
                ;; M-o.
                ;; Only works if ergoemacs-mode is on...
                (setq cmd (nth 1 x))
                
                (if (and ergoemacs-fix-M-O
                         (string= (ergoemacs-kbd trans-key t t) "M-O"))
                    (progn
                      ;; Add shortcut if available
                      (cond
                       ((or (remove-if '(lambda(x) (eq 'menu-bar (elt x 0))) ; Ignore
                                        ; menu-bar
                                        ; functions
                                       (where-is-internal cmd (current-global-map)))
                            (gethash cmd ergoemacs-where-is-global-hash))
                        (puthash (read-kbd-macro (key-description key) t)
                                 (list cmd 'global) ergoemacs-command-shortcuts-hash)
                        (define-key ergoemacs-shortcut-keymap key 'ergoemacs-M-O)
                        (define-key ergoemacs-M-O-keymap [timeout] 'ergoemacs-shortcut))
                       (t
                        (define-key ,keymap key  'ergoemacs-M-O)
                        (define-key ergoemacs-M-O-keymap [timeout] cmd)))
                      (if (eq ',keymap 'ergoemacs-keymap)
                          (ergoemacs-debug "Variable %s: %s (%s) -> %s %s via ergoemacs-M-O"
                                           (nth 0 x) trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
                  (if (and ergoemacs-fix-M-O
                           (string= (ergoemacs-kbd trans-key t t) "M-o"))
                      (progn
                        (cond  ;; Use shortcut if available.
                         ((or (remove-if '(lambda(x) (eq 'menu-bar (elt x 0))) ; Ignore
                                        ; menu-bar
                                        ; functions
                                         (where-is-internal cmd (current-global-map)))
                              (gethash cmd ergoemacs-where-is-global-hash))
                          (puthash (read-kbd-macro (key-description key) t)
                                   (list cmd 'global) ergoemacs-command-shortcuts-hash)
                          (define-key ergoemacs-shortcut-keymap key 'ergoemacs-M-o)
                          (define-key ergoemacs-M-o-keymap [timeout] 'ergoemacs-shortcut))
                         (t
                          (define-key ,keymap key  'ergoemacs-M-o)
                          (define-key ergoemacs-M-o-keymap [timeout] cmd)))
                        (if (eq ',keymap 'ergoemacs-keymap)
                            (ergoemacs-debug "Variable: %s (%s) -> %s %s via ergoemacs-M-o" trans-key
                                             (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)))
                    (when cmd
                      (ergoemacs-setup-keys-for-keymap---internal ,keymap key cmd)
                      (if (eq ',keymap 'ergoemacs-keymap)
                          (ergoemacs-debug "Variable: %s (%s) -> %s %s" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key))))))))
          (symbol-value (ergoemacs-get-variable-layout)))
         (when ergoemacs-fix-M-O
           (let ((M-O (lookup-key ,keymap (read-kbd-macro "M-O")))
                 (g-M-O (lookup-key global-map (read-kbd-macro "M-O")))
                 (M-o (lookup-key ,keymap (read-kbd-macro "M-o")))
                 (g-M-o (lookup-key global-map (read-kbd-macro "M-o"))))
             (ergoemacs-debug "M-O %s; Global M-O: %s; M-o %s; Global M-o: %s" M-O g-M-O M-o g-M-o)
             (when (and (not (functionp M-O))
                        (functionp g-M-O))
               (ergoemacs-debug "Fixed M-O")
               (define-key ,keymap (read-kbd-macro "M-O") 'ergoemacs-M-O)
               (define-key ergoemacs-M-O-keymap [timeout] g-M-O))
             (when (and (not (functionp M-o))
                        (functionp g-M-o))
               (ergoemacs-debug "Fixed M-o")
               (define-key ,keymap (read-kbd-macro "M-o") 'ergoemacs-M-o)
               (define-key ergoemacs-M-o-keymap [timeout] g-M-o))))
         (ergoemacs-debug-keymap ',keymap))
     (error
      (ergoemacs-debug "Error: %s" err)
      (ergoemacs-debug-flush))))

(defun ergoemacs-setup-keys-for-layout (layout &optional base-layout)
  "Setup keys based on a particular LAYOUT. All the keys are based on QWERTY layout."
  (ergoemacs-setup-translation layout base-layout)
  ;; Reset shortcuts layer.
  (setq ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal))
  (let ((setup-ergoemacs-keymap t))
    (ergoemacs-setup-keys-for-keymap ergoemacs-keymap))
  (ergoemacs-setup-fast-keys)
  (let ((x (assq 'ergoemacs-mode minor-mode-map-alist)))
    ;; Install keymap
    (if x
        (setq minor-mode-map-alist (delq x minor-mode-map-alist)))
    (add-to-list 'minor-mode-map-alist
                 `(ergoemacs-mode  ,(symbol-value 'ergoemacs-keymap))))
  (easy-menu-define ergoemacs-menu ergoemacs-keymap
    "ErgoEmacs menu"
    `("ErgoEmacs"
      ,(ergoemacs-get-layouts-menu)
      ,(ergoemacs-get-themes-menu)
      "--"
      ("Ctrl+C and Ctrl+X behavior"
       ["Ctrl+C and Ctrl+X are for Emacs Commands"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x))
        :style radio
        :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-C-c-and-C-x)]
       ["Ctrl+C and Ctrl+X are only Copy/Cut"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut))
        :style radio
        :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)]
       ["Ctrl+C and Ctrl+X are both Emacs Commands & Copy/Cut"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-handle-ctl-c-or-ctl-x 'both))
        :style radio
        :selected (eq ergoemacs-handle-ctl-c-or-ctl-x 'both)]
       ["Customize Ctrl+C and Ctrl+X Cut/Copy Timeout"
        (lambda() (interactive)
          (customize-variable 'ergoemacs-ctl-c-or-ctl-x-delay))])
      ("Paste behavior"
       ["Repeating Paste pastes multiple times"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-smart-paste nil))
        :style radio
        :selected (eq ergoemacs-smart-paste 'nil)]
       ["Repeating Paste cycles through previous pastes"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-smart-paste t))
        :style radio
        :selected (eq ergoemacs-smart-paste 't)]
       ["Repeating Paste starts browse-kill-ring"
        (lambda()
          (interactive)
          (set-default 'ergoemacs-smart-paste 'browse-kill-ring))
        :style radio
        :enable (condition-case err (interactive-form 'browse-kill-ring)
                  (error nil))
        :selected (eq ergoemacs-smart-paste 'browse-kill-ring)])
      "--"
      ["Make Bash aware of ergoemacs keys"
       (lambda () (interactive)
         (call-interactively 'ergoemacs-bash)) t]
      "--"
      ["Use Menus"
       (lambda() (interactive)
         (setq ergoemacs-use-menus (not ergoemacs-use-menus))
         (if ergoemacs-use-menus
             (progn
               (require 'ergoemacs-menus)
               (ergoemacs-menus-on))
           (when (featurep 'ergoemacs-menus)
             (ergoemacs-menus-off))))
       :style toggle :selected (symbol-value 'ergoemacs-use-menus)]
      "--"
      ;; ["Generate Documentation"
      ;;  (lambda()
      ;;    (interactive)
      ;;    (call-interactively 'ergoemacs-extras)) t]
      ["Customize Ergoemacs"
       (lambda ()
         (interactive)
         (customize-group 'ergoemacs-mode)) t]
      ["Save Settings for Future Sessions"
       (lambda ()
         (interactive)
         (customize-save-variable 'ergoemacs-smart-paste ergoemacs-smart-paste)
         (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
         (customize-save-variable 'ergoemacs-theme ergoemacs-theme)
         (customize-save-variable 'ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
         (customize-save-variable 'ergoemacs-ctl-c-or-ctl-x-delay ergoemacs-ctl-c-or-ctl-x-delay)
         (customize-save-variable 'ergoemacs-handle-ctl-c-or-ctl-x ergoemacs-handle-ctl-c-or-ctl-x)
         (customize-save-variable 'ergoemacs-use-menus ergoemacs-use-menus)
         (customize-save-customized)) t]
      ["Exit ErgoEmacs"
       (lambda ()
         (interactive)
         (ergoemacs-mode -1)) t]))
  
  (let ((existing (assq 'ergoemacs-mode minor-mode-map-alist)))
    (if existing
        (setcdr existing ergoemacs-keymap)
      (push (cons 'ergoemacs-mode ergoemacs-keymap) minor-mode-map-alist)))
  (ergoemacs-mode-line)
  ;; Set appropriate mode-line indicator
  (ergoemacs-setup-backward-compatability))

(defvar ergoemacs-extract-map-hash (make-hash-table :test 'equal))
(defmacro ergoemacs-extract-maps (keymap &optional prefix)
  "Extracts maps."
  `(save-excursion
     (let ((deactivate-mark nil)
           (buf (current-buffer))
           (normal '())
           (translations '())
           (prefixes '())
           (bound-regexp "")
           (tmp "")
           (fn nil)
           (new-key nil)
           (start-time (float-time))
           (last-time nil)
           (cur-prefix (or ,prefix "C-x"))
           (hashkey "")
           (prefix-regexp ""))
       (ergoemacs-debug-heading "Extracting maps for %s" cur-prefix)
       (with-temp-buffer
         (let (ergoemacs-shortcut-keys)
           (describe-buffer-bindings buf (read-kbd-macro cur-prefix)))
         (goto-char (point-min))
         (while (re-search-forward (format "%s \\(.*?\\)[ \t]\\{2,\\}\\(.+\\)$" cur-prefix) nil t)
           (setq new-key (match-string 1))
           (setq fn (match-string 2))
           (unless (string-match " " new-key)
             (cond
              ((save-match-data
                 (string-match "[ \t]+[?][?]$" (match-string 0)))
               (ergoemacs-debug "Anonymous function for %s" new-key)
               (let (ergoemacs-shortcut-keys)
                 (setq fn (key-binding (read-kbd-macro new-key)))
                 (add-to-list 'normal (list new-key fn))))
              ((save-match-data
                 (string-match "Prefix Command$" (match-string 0)))
               (unless (string-match "ESC" new-key)
                 (ergoemacs-debug "Prefix: %s" new-key)
                 (add-to-list 'prefixes new-key)))
              (t
               (condition-case err
                   (with-temp-buffer
                     (insert "(if (condition-case err (keymapp '" fn
                             ") (error nil)) (unless (string-match \"ESC\" \"" new-key
                             "\") (add-to-list 'prefixes \"" new-key
                             "\") (ergoemacs-debug \"Prefix (keymap): %s\" new-key)) (add-to-list 'normal '(\""
                             new-key "\" " fn ")) (ergoemacs-debug \"Normal: %s -> %s\" new-key fn))")
                     (eval-buffer)
                     (when ergoemacs-translate-keys
                       (cond
                        ((string-match "\\( \\|^\\)C-\\([a-zA-Z'0-9{}/,.`]\\)$" new-key)
                         (add-to-list 'translations
                                      (list (replace-match "\\1\\2" t nil new-key)
                                            fn)))
                        ((string-match "\\( \\|^\\)\\([a-zA-Z'0-9{}/,.`]\\)$" new-key)
                         (add-to-list 'translations
                                      (list (replace-match "\\1C-\\2" t nil new-key)
                                            fn))))))
                 (error
                  (setq fn nil))))))))
       (ergoemacs-debug-heading "Building keymap")
       (setq hashkey (md5 (format "%s;%s;%s" cur-prefix normal prefixes)))
       (setq ,keymap (gethash hashkey ergoemacs-extract-map-hash))
       (unless ,keymap
         (setq ,keymap (make-keymap))
         (mapc
          (lambda(x)
            (let* ((normal (nth 0 x))
                   (ctl-to-alt
                    (replace-regexp-in-string
                     "\\<W-" "M-"
                     (replace-regexp-in-string
                      "\\<M-" "C-"
                      (replace-regexp-in-string "\\<C-" "W-" normal))))
                   (unchorded
                    (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
              (if (not (functionp (nth 1 x)))
                  (when (string-match "^C-x 8" cur-prefix)
                    (ergoemacs-debug "Not a function AND C-x 8, assuming translation.")
                    (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal (nth 1 x))
                    
                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Normal> %s %s" cur-prefix normal))
                      (read-kbd-macro (format "%s" (nth 1 x))))
                    
                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Ctl%sAlt> %s %s" 
                               (ergoemacs-unicode-char "↔" " to ")
                               cur-prefix ctl-to-alt))
                      (read-kbd-macro (format "%s" (nth 1 x))))
                    (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                     (ergoemacs-unicode-char "↔" " to ")
                                     cur-prefix ctl-to-alt (nth 1 x))

                    (define-key local-function-key-map
                      (read-kbd-macro
                       (format "<Unchorded> %s %s" cur-prefix unchorded))
                      (read-kbd-macro (format "%s" (nth 1 x))))

                    (ergoemacs-debug "<Unchorded> %s %s => %s"
                                     cur-prefix unchorded (nth 1 x)))
                (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal (nth 1 x))
                (define-key ,keymap
                  (read-kbd-macro (format "<Normal> %s %s" cur-prefix normal))
                  `(lambda(&optional arg)
                     (interactive "P")
                     (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',(nth 1 x))))
                (define-key ,keymap
                  (read-kbd-macro
                   (format "<Ctl%sAlt> %s %s" 
                           (ergoemacs-unicode-char "↔" " to ")
                           cur-prefix ctl-to-alt))
                  `(lambda(&optional arg)
                     (interactive "P")
                     (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',(nth 1 x))))
                (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                 (ergoemacs-unicode-char "↔" " to ")
                                 cur-prefix ctl-to-alt (nth 1 x))
                
                (define-key ,keymap
                  (read-kbd-macro
                   (format "<Unchorded> %s %s" cur-prefix unchorded))
                  `(lambda(&optional arg)
                     (interactive "P")
                     (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',(nth 1 x))))
                (ergoemacs-debug "<Unchorded> %s %s => %s"
                                 cur-prefix unchorded (nth 1 x)))))
          normal)
         (ergoemacs-debug-heading "Adding Prefixes")
         ;; Now add prefixes.
         (mapc
          (lambda(x)
            (let ((new (replace-regexp-in-string
                        "\\<W-" "M-"
                        (replace-regexp-in-string
                         "\\<M-" "C-"
                         (replace-regexp-in-string "\\<C-" "W-" x)))))

              (condition-case err
                  (define-key ,keymap
                    (read-kbd-macro (format "<Normal> %s %s" cur-prefix x))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'normal)))
                (error nil))

              (condition-case err
                  (define-key ,keymap 
                    (read-kbd-macro
                     (format "<Ctl%sAlt> %s %s" 
                             (ergoemacs-unicode-char "↔" " to ")
                             cur-prefix new))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'ctl-to-alt)))
                (error nil))
              
              (setq new
                    (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" new))))
              
              (condition-case err
                  (define-key ,keymap 
                    (read-kbd-macro
                     (format "<Unchorded> %s %s"
                             cur-prefix new))
                    `(lambda(&optional arg)
                       (interactive "P")
                       (ergoemacs-menu-send-prefix ,cur-prefix ,x 'unchorded)))
                (error nil))))
          prefixes)
         
         (ergoemacs-debug-heading "Translating keys")
         
         ;;
         (when ergoemacs-translate-keys
           (setq bound-regexp
                 (format "^%s$"
                         (regexp-opt
                          (append
                           (mapcar (lambda(x) (nth 0 x))
                                   normal) prefixes) t)))
           (ergoemacs-debug-heading "Translating keys for %s" cur-prefix)
           (mapc
            (lambda(x)
              (if (string-match bound-regexp (nth 0 x))
                  (ergoemacs-debug "Assume %s is already defined" x)
                (ergoemacs-debug "Testing %s; %s" x (functionp (intern (nth 1 x))))
                (when (functionp (intern (nth 1 x)))    
                  (let* ((fn (intern (nth 1 x)))
                         (normal (nth 0 x))
                         (ctl-to-alt
                          (replace-regexp-in-string
                           "\\<W-" "M-"
                           (replace-regexp-in-string
                            "\\<M-" "C-"
                            (replace-regexp-in-string "\\<C-" "W-" normal))))
                         (unchorded
                          (replace-regexp-in-string
                           "\\<W-" ""
                           (replace-regexp-in-string
                            "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                            (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
                    (let ((curr-kbd (format "<Normal> %s %s" cur-prefix normal)))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (define-key ,keymap
                        (read-kbd-macro curr-kbd)
                        `(lambda(&optional arg)
                           (interactive "P")
                           (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',fn)))
                      (condition-case err
                          (ergoemacs-debug "<Normal> %s %s => %s" cur-prefix normal fn)
                        (error (ergoemacs-debug "%s" err)))
                      (setq curr-kbd
                            (format "<Ctl%sAlt> %s %s" 
                                    (ergoemacs-unicode-char "↔" " to ")
                                    cur-prefix ctl-to-alt))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (condition-case err
                          (define-key ,keymap
                            (read-kbd-macro curr-kbd)
                            `(lambda(&optional arg)
                               (interactive "P")
                               (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',fn)))
                        (error (ergoemacs-debug "%s" err)))
                      (ergoemacs-debug "<Ctl%sAlt> %s %s => %s"
                                       (ergoemacs-unicode-char "↔" " to ")
                                       cur-prefix ctl-to-alt fn)
                      (setq curr-kbd (format "<Unchorded> %s %s" cur-prefix unchorded))
                      (ergoemacs-debug "\tcurr-kbd: %s" curr-kbd)
                      (condition-case err
                          (define-key ,keymap
                            (read-kbd-macro curr-kbd)
                            `(lambda(&optional arg)
                               (interactive "P")
                               (ergoemacs-send-fn ,(concat cur-prefix " " normal) ',fn)))
                        (error (ergoemacs-debug "%s" err)))
                      (ergoemacs-debug "<Unchorded> %s %s => %s"
                                       cur-prefix unchorded fn))))))
            translations))
         (ergoemacs-debug-heading "Adding swap")
         
         ;; Now add root level swap.
         (ergoemacs-debug "Root: %s <%s>" cur-prefix (if (eq system-type 'windows-nt) "apps" "menu"))
         
         (condition-case err
             (define-key ,keymap
               (read-kbd-macro (format "<Normal> %s <%s>" cur-prefix
                                       (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'normal)))
           (error nil))

         (condition-case err
             (define-key ,keymap
               (read-kbd-macro (format "<Normal> %s <exit>" cur-prefix))
               'ignore)
           (error nil))
         
         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Ctl%sAlt> %s <%s>" 
                        (ergoemacs-unicode-char "↔" " to ")
                        cur-prefix
                        (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'ctl-to-alt)))
           (error nil))

         (condition-case err
             (define-key ,keymap
               (read-kbd-macro
                (format "<Ctl%sAlt> %s <exit>"
                        (ergoemacs-unicode-char "↔" " to ")
                        cur-prefix)) 'ignore)
           (error nil))

         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Unchorded> %s <%s>"
                        cur-prefix
                        (if (eq system-type 'windows-nt) "apps" "menu")))
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-menu-swap ,cur-prefix "" 'unchorded)))
           (error nil))

         (condition-case err
             (define-key ,keymap 
               (read-kbd-macro
                (format "<Unchorded> %s <exit>"
                        cur-prefix)) `ignore)
           (error nil))
         (puthash hashkey ,keymap ergoemacs-extract-map-hash))
       (ergoemacs-debug-flush))))

(defvar ergoemacs-send-fn-keys-fns '(ergoemacs-undefined ergoemacs-shortcut)
  "List of functions where `unread-command-events' are sent with `ergoemacs-send-fn'.")

(defun ergoemacs-send-fn (key fn &optional message)
  "Sends the function."
  (setq ergoemacs-shortcut-send-key nil
        ergoemacs-shortcut-send-fn nil
        ergoemacs-shortcut-send-timer nil)
  (setq ergoemacs-shortcut-send-fn (or (command-remapping fn (point)) fn))
  (setq this-command ergoemacs-shortcut-send-fn)
  (cond
   ((memq ergoemacs-shortcut-send-fn ergoemacs-send-fn-keys-fns)
    (let ((old-unread (listify-key-sequence (this-command-keys)))
          new-unread)
      ;; Force emacs to send the correct keys.  Workaround for emacs
      ;; bug.
      (eval
       (macroexpand
        `(flet
             ((this-command-keys () (if (equal this-command ',ergoemacs-shortcut-send-fn) ,(read-kbd-macro key t) (funcall ,(symbol-function 'this-command-keys))))
              (this-single-command-keys () (if (equal this-command ',ergoemacs-shortcut-send-fn) ,(read-kbd-macro key t) (funcall ,(symbol-function 'this-single-command-keys))))
              (this-command-keys-vector () (if (equal this-command ',ergoemacs-shortcut-send-fn) ,(read-kbd-macro key t) (funcall ,(symbol-function 'this-command-keys-vector)))))
           (setq new-unread (listify-key-sequence (this-command-keys)))
           (call-interactively ergoemacs-shortcut-send-fn nil ,(read-kbd-macro key t)))))
      ;; Some commands, like isearch, put commands in
      ;; `unread-command-events'; Try to handle these.
      (when (and unread-command-events
                 (equal unread-command-events new-unread))
        (setq unread-command-events old-unread))))
   (t
    (call-interactively ergoemacs-shortcut-send-fn))))

(defun ergoemacs-menu-send-prefix (prefix-key untranslated-key type)
  "Extracts maps for PREFIX-KEY UNTRANSLATED-KEY of TYPE."
  (setq this-command last-command) ; Don't record this command.
  ;; (setq prefix-arg current-prefix-arg)
  (ergoemacs-shortcut-internal (format "%s %s" prefix-key untranslated-key) type))

(defun ergoemacs-menu-swap (prefix-key untranslated-key type)
  "Swaps what <menu> key translation is in effect"
  (let* (deactivate-mark
         (new-type nil)
         (new-key nil)
         (kbd-code nil)
         (normal untranslated-key)
         (ctl-to-alt (replace-regexp-in-string
                      "\\<W-" "M-"
                      (replace-regexp-in-string
                       "\\<M-" "C-"
                       (replace-regexp-in-string "\\<C-" "W-" normal))))
         (unchorded (replace-regexp-in-string
                     "\\<W-" ""
                     (replace-regexp-in-string
                      "\\(^\\| \\)\\([^-]\\)\\( \\|$\\)" "\\1M-\\2\\3"
                      (replace-regexp-in-string "\\<M-" "W-" ctl-to-alt)))))
    (cond
     ((member ergoemacs-first-extracted-variant '(ctl-to-alt normal))
      (cond
       ((eq type 'ctl-to-alt)
        (setq new-type 'unchorded))
       ((eq type 'unchorded)
        (setq new-type 'normal))
       ((eq type 'normal)
        (setq new-type 'ctl-to-alt))))
     ((equal ergoemacs-first-extracted-variant 'unchorded)
      (cond
       ((eq type 'ctl-to-alt)
        (setq new-type 'normal))
       ((eq type 'unchorded)
        (setq new-type 'ctl-to-alt))
       ((eq type 'normal)
        (setq new-type 'unchorded)))))
    (setq kbd-code
          (cond
           ((eq new-type 'normal)
            (format "<Normal> %s %s" prefix-key normal))
           ((eq new-type 'ctl-to-alt)
            (format "<Ctl%sAlt> %s %s"
                    (ergoemacs-unicode-char "↔" " to ")
                    prefix-key
                    ctl-to-alt))
           ((eq new-type 'unchorded)
            (format "<Unchorded> %s %s" prefix-key
                    unchorded))))
    (setq new-key (listify-key-sequence (read-kbd-macro kbd-code)))
    (setq this-command last-command) ; Don't record this command.
    (setq prefix-arg current-prefix-arg) ; Need to send prefix argument since sending unread command events
    (set-temporary-overlay-map ergoemacs-current-extracted-map)
    (reset-this-command-lengths)
    (setq unread-command-events (append new-key unread-command-events))
    
    (save-match-data
      (when (string-match "<\\(.*?\\)> \\(.*\\)" kbd-code)
        (let (message-log-max)
          (message "%s%s"
                   (if current-prefix-arg (format "%s " current-prefix-arg) "")
                   (replace-regexp-in-string "<Normal> +" ""
                                             (format "<%s> %s" (match-string 1 kbd-code)
                                                     (ergoemacs-pretty-key (match-string 2 kbd-code))))))))))

(defvar ergoemacs-prefer-shortcuts t ;; Prefer shortcuts.
  "Prefer shortcuts")

(defvar ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal)
  "List of command shortcuts.")

(defvar ergoemacs-repeat-shortcut-keymap (make-sparse-keymap)
  "Keymap for repeating often used shortcuts like C-c C-c.")

(defvar ergoemacs-repeat-shortcut-msg ""
  "Message for repeating keyboard shortcuts like C-c C-c")

(defun ergoemacs-shortcut-timeout ()
  (let (message-log-max)
    (unless (current-message)
      (message ergoemacs-repeat-shortcut-msg)))
  (set-temporary-overlay-map ergoemacs-repeat-shortcut-keymap))

(defvar ergoemacs-current-extracted-map nil
  "Current extracted map for `ergoemacs-shortcut' defined functions")

(defvar ergoemacs-first-extracted-variant nil
  "Current extracted variant")

(defcustom ergoemacs-shortcut-ignored-functions
  '(undo-tree-visualize)
  "Ignored functions for `ergoemacs-shortcut'."
  :group 'ergoemacs-mode
  :type '(repeat
          (symbol :tag "Function to ignore:")))

(defmacro ergoemacs-with-global (&rest body)
  "With global keymap, not ergoemacs keymaps.
In theory `ergoemacs-mode' and `ergoemacs-unbind-keys' should
also be nil, but this is used in `ergoemacs-shortcut-internal'
which allows a shortcut to ergoemacs keys as well, so these
variables are handled in `ergoemacs-shortcut-internal'.
"
  `(let (overriding-terminal-local-map
         overriding-local-map
         ergoemacs-overlays overlay overlay-keymap
         keymap-begin keymap-end
         (overlays (overlays-at (point))))
     ;; Remove most of ergoemacs-mode's key bindings
     (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
     ;; Sigh.  Now remove overriding overlay and text-map key bindings
     ;; that have been altered by ergoemacs-mode. If this does not
     ;; happen, and press Ctrl+v will lookup the emacs equivalent
     ;; command of Ctrl+y which is ergoemacs' redo.  This only occurs
     ;; in things that add an overlay, such a smart-parens mode.
     (ergoemacs-remove-shortcuts)
     (unwind-protect
         (progn
           ,@body)
       (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
       ;; The shortcuts will be put back in post command hook.
       ;; Putting them back here will end up in an infinite loop. 
       ;;(ergoemacs-install-shortcuts-up)
       )))

(defun ergoemacs-shortcut (&optional arg)
  "Shortcut for other key/function.
Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for `this-command-keys-vector'.  The
workhorse of this function is in `ergoemacs-shortcut-internal'.

This is only performed it `ergoemacs-mode' has not defined some
work-around for a particular key in `ergoemacs-emulation-mode-map-alist'
"
  (interactive "P")
  (setq ergoemacs-shortcut-send-key nil
        ergoemacs-shortcut-send-fn nil
        ergoemacs-shortcut-send-timer nil)
  (let (cmd1 cmd2)
    (let (ergoemacs-shortcut-keys ergoemacs-shortcut-override-mode)
      (setq cmd1 (key-binding (this-single-command-keys)))
      (ergoemacs-with-global 
       (setq cmd2 (key-binding (this-single-command-keys)))))
    (if (not (equal cmd1 cmd2))
        (progn
          (setq ergoemacs-shortcut-send-key (key-description (this-command-keys-vector))
                ergoemacs-shortcut-send-fn cmd1))
      (setq args (gethash (this-command-keys-vector)
                          ergoemacs-command-shortcuts-hash))
      (unless args
        ;; Take care of vectors and universal arguments
        (setq args
              (gethash (read-kbd-macro
                        (key-description (this-single-command-keys)) t)
                       ergoemacs-command-shortcuts-hash)))
      (if (not args)
          (progn
            ;; Remove reference to `ergoemacs-shortcut'
            (when (featurep 'keyfreq)
              (when keyfreq-mode
                (let ((command 'ergoemacs-shortcut) count)
                  (setq count (gethash (cons major-mode command) keyfreq-table))
                  (remhash (cons major-mode command) keyfreq-table)
                  ;; Add `ergoemacs-undefined' to counter.
                  (setq command 'ergoemacs-undefined)
                  (setq count (gethash (cons major-mode command) keyfreq-table))
                  (puthash (cons major-mode command) (if count (+ count 1) 1)
                           keyfreq-table))))
            (ergoemacs-undefined))
        (when (featurep 'keyfreq)
          (when keyfreq-mode
            (let ((command 'ergoemacs-shortcut) count)
              (setq count (gethash (cons major-mode command) keyfreq-table))
              (remhash (cons major-mode command) keyfreq-table))))
        (setq this-command last-command)
        ;; (setq prefix-arg current-prefix-arg)
        (if (condition-case err
                (interactive-form (nth 0 args))
              (error nil))
            (progn
              (setq ergoemacs-shortcut-send-fn (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args)))))
          (setq ergoemacs-shortcut-send-fn (macroexpand `(ergoemacs-shortcut-internal ,(nth 0 args) ',(nth 1 args))))))))
  ;; Get out of the nesting and let bindings...
  (cond
   (ergoemacs-shortcut-send-key
    (ergoemacs-send-fn ergoemacs-shortcut-send-key ergoemacs-shortcut-send-fn))
   (ergoemacs-shortcut-send-fn
    (eval ergoemacs-shortcut-send-fn))))

(defun ergoemacs-quit-key-sequence ()
  (interactive)
  (beep)
  (message "Quit key sequence."))

(defvar ergoemacs-shortcut-send-key nil)
(defvar ergoemacs-shortcut-send-fn nil)
(defvar ergoemacs-shortcut-send-timer nil)
(defun ergoemacs-shortcut-internal (key &optional chorded repeat keymap-key timeout timeout-fn)
  "Ergoemacs Shortcut.

KEY is the keyboard shortcut.

CHORDED is a variable that alters to keymap to allow unchorded
Key sequences.  Also if CHORDED is 'global, then make this a
shortcut to a global command.

If CHORDED is nil, the NAME command will just issue the KEY sequence.

If CHORDED is 'unchorded or the NAME command will translate the control
bindings to be unchorded.  For example:

For example for the C-x map,

Original Key   Translated Key  Function
C-k C-n     -> k n             (kmacro-cycle-ring-next)
C-k a       -> k M-a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

If CHORDED is 'ctl-to-alt or the NAME command will translate the control
bindings to be unchorded.  For example:

C-k C-n     -> M-k M-n         (kmacro-cycle-ring-next)
C-k a       -> M-k a           (kmacro-add-counter)
C-k M-a     -> k C-a           not defined
C-k S-a     -> k S-a           not defined

When REPEAT is a variable name, then an easy repeat is setup for
the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows
Ctrl+c Ctrl+c to be repeated by m.

When KEYMAP-KEY is non-nil, define the KEYMAP-KEY on the
`ergoemacs-shortcut-override-keymap'

When `override-text-map' is bound and defined only look up based
on that key.

When KEY is a function, lookup the corresponding binding of that
function if it is bound globally.  For example
`beginning-of-line' becomes `org-beginning-of-line' in `org-mode'
"
  (setq ergoemacs-shortcut-send-key nil
        ergoemacs-shortcut-send-fn nil
        ergoemacs-shortcut-send-timer nil)
  (let (ergoemacs-mode
        ergoemacs-unbind-keys
        case-fold-search binding fn-ergo
        fn fn-lst new-fn fn-override
        do-it key-seq next-key new-key-seq new-cmd
        shared-do-it
        (ctl-c-keys (key-description (this-single-command-keys))))
    (cond
     ((condition-case err ;; This is a function (possibly global)
          (interactive-form key)
        (error nil))
      ;; Lookup ergoemacs key bindings.
      (mapc
       (lambda(cur-key)
         (setq new-fn (lookup-key ergoemacs-keymap cur-key))
         (unless new-fn
           (setq new-fn (gethash (read-kbd-macro
                                   (key-description cur-key) t)
                                  ergoemacs-command-shortcuts-hash))
           (when new-fn
             (setq new-fn (car new-fn))))
         (when new-fn
           (push new-fn fn-ergo)))
       (or
        (remove-if
         '(lambda(x)
            (or (eq 'menu-bar (elt x 0)))) ; Ignore menu-bar functions
         (where-is-internal key (current-global-map)))
        (gethash key ergoemacs-where-is-global-hash)))
      
      (setq new-fn nil)
      (ergoemacs-with-global
          (mapc
           (lambda(cur-key)
             (unless (string-match "\\(s-\\|A-\\|H-\\)"
                                   (key-description cur-key))
               (setq binding
                     (if (and keymap-key (boundp 'ergoemacs-orig-keymap)
                              ergoemacs-orig-keymap)
                         (lookup-key ergoemacs-orig-keymap cur-key t)
                       (key-binding cur-key t nil (point))))
               (setq new-fn (intern-soft (format "erogemacs-%s" binding)))
               ;; Dont bind to shortcut maps... causes infinite recursion
               ;; of that function calls `ergoemacs-shortcut-internal'
               (when (and new-fn (not (eq ergoemacs-this-command new-fn))
                          (condition-case err
                              (interactive-form new-fn)
                            (error nil)))
                 ;; When a lookup finds org-metadown and there is a
                 ;; function ergoemacs-org-metadown, use the
                 ;; ergoemacs-org-metadown instead.
                 (setq fn-override
                       (list new-fn
                             (read-kbd-macro
                              (key-description cur-key) t))))
               (unless (or (eq binding key)
                           ;; No infinite lookups.
                           (eq ergoemacs-this-command binding)
                           ;; No shortcuts to ergoemacs from function.
                           (eq binding fn-ergo)
                           (memq binding
                                 (append ergoemacs-shortcut-ignored-functions
                                         '(ergoemacs-undefined
                                           ergoemacs-shortcut)
                                         fn-ergo)))
                 (add-to-list 'fn-lst (list binding
                                            (read-kbd-macro
                                             (key-description cur-key) t))))))
           (or
            (remove-if
             '(lambda(x)
                (or (eq 'menu-bar (elt x 0)))) ; Ignore menu-bar functions
             (where-is-internal key (current-global-map)))
            (gethash key ergoemacs-where-is-global-hash))))
      (cond
       (fn-override
        (set fn fn-override))
       (fn-lst
        ;; FIXME: If new functions exist, give the user the option to use
        ;; these functions

        ;; For now, just use the first function.
        (setq fn (nth 0 fn-lst)))
       (t  ; Could not find another function, just use the function
           ; passed to `ergoemacs-shortcut'
        (ergoemacs-with-global
            (setq fn (list key
                       (read-kbd-macro
                        (key-description
                         (or (where-is-internal
                              key (current-global-map) t)
                             (this-command-keys))) t))))))
      (setq shared-do-it t))
     ((or (not chorded)
          (memq chorded '(repeat repeat-global global-repeat global))) ;; lookup keybinding for the function keys.
      (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
      (ergoemacs-with-global
          (setq fn (list (key-binding (read-kbd-macro key))
                         (read-kbd-macro key t)))
        (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist))
      (setq shared-do-it t))
    (keymap-key ;; extract key prefixes.
     )
    (t ;; key prefix
     (setq ergoemacs-push-M-O-timeout nil) ;; Cancel timeouts
     (setq this-command last-command) ; Don't record this command.
     ;; (setq prefix-arg current-prefix-arg)
     (setq ergoemacs-mode t ergoemacs-unbind-keys t
           key-seq nil
           key-type (cond
                     ((eq chorded 'unchorded)
                      "Unchorded")
                     ((eq chorded 'ctl-to-alt)
                      (format "Ctl%sAlt"
                              (ergoemacs-unicode-char "↔" " to ")))
                     (t "Normal")))
     (let  (deactivate-mark)
       (eval (macroexpand '(ergoemacs-extract-maps ergoemacs-current-extracted-map key)))
       (setq ergoemacs-first-extracted-variant chorded)
       (setq key-seq
             (read-kbd-macro
              (format "<%s> %s" key-type key)))
       (setq key-type (concat "<" key-type "> "))
       (when (string= key-type "<Normal> ")
         (setq key-type ""))
       
       (let (message-log-max)
         (message (concat
                   (if current-prefix-arg
                       (format "%s " current-prefix-arg)
                     "")
                   (format "%s%s " key-type
                           (ergoemacs-pretty-key key)))))
       (let* ((next-key (if timeout
                            (with-timeout (timeout nil)
                              (eval (macroexpand `(key-description [,(read-key)]))))
                          (eval (macroexpand `(key-description [,(read-key)])))))
              (new-key-seq (if next-key (read-kbd-macro (concat (key-description key-seq) " " next-key)) nil))
              (new-cmd (if next-key (lookup-key ergoemacs-current-extracted-map new-key-seq) nil)))
         (cond
          ((and (not next-key) timeout timeout-fn)
           (let (message-log-max)
             (message ""))
           (call-interactively timeout-fn))
          ((and (not next-key) timeout)
           (message "Exiting."))
          ((condition-case err
               (interactive-form new-cmd)
             (error nil))
           (setq ergoemacs-shortcut-send-key (concat key " " next-key))
           (setq ergoemacs-shortcut-send-fn new-cmd))
          ((and (string-match "\\(M-[oO]\\|ESC\\|<escape>\\|M-\\[\\)"
                              next-key)
                (string-match "\\(M-[oO]\\|ESC\\|<escape>\\|M-\\[\\)"
                              (key-description (ergoemacs-key-fn-lookup 'keyboard-quit))))
           ;; Keep translations; Send quit.
           (setq ergoemacs-push-M-O-timeout t)
           (define-key ergoemacs-current-extracted-map
             (read-kbd-macro (concat (key-description key-seq)
                                     " " next-key " <timeout>"))
             'ergoemacs-quit-key-sequence)
           (set-temporary-overlay-map ergoemacs-current-extracted-map)
           (setq key-seq (listify-key-sequence new-key-seq))
           (reset-this-command-lengths)
           (setq unread-command-events
                 (append key-seq unread-command-events))
           (setq ergoemacs-M-O-timer (run-with-timer ergoemacs-M-O-delay nil #'ergoemacs-M-O-timeout)))
          ((equal 'keyboard-quit (key-binding (read-kbd-macro next-key)))
           (ergoemacs-quit-key-sequence))
          ;; Allow prefixes to be picked up if they were not already
          ;; defined.  This is done by a temporary keymap and waiting.
          ((or ; When the key-sequence is a keymap or a prefix for a
                                        ; translation map, exit command with
                                        ; unread-command-events taking care of any translations.
            (condition-case err
                (keymapp new-cmd)
              (error nil))
            (string-match "\\(M-[oO]\\|ESC\\|<escape>\\|M-\\[\\)" next-key))
           (set-temporary-overlay-map ergoemacs-current-extracted-map)
           (setq key-seq (listify-key-sequence new-key-seq))
           (reset-this-command-lengths)
           (setq unread-command-events
                 (append key-seq unread-command-events)))
          ((lookup-key (current-global-map)
                       (read-kbd-macro (concat key " " next-key)))
           ;; Fallback if the extracted map didn't pick up the global
           ;; binding...  
           (ergoemacs-debug "WARNING: Did not extract the correct map.")
           (call-interactively (lookup-key (current-global-map)
                                           (read-kbd-macro (concat key " " next-key)))))
          (t
           (setq ergoemacs-shortcut-send-key (concat key " " next-key))
           (setq ergoemacs-shortcut-send-fn 'ergoemacs-undefined)))))))
    (when shared-do-it
      (if (not fn)
       (unless keymap-key
         (let (message-log-max)
           (message "%s is not defined." (ergoemacs-pretty-key key))))
     (unless keymap-key
       (setq this-command (nth 0 fn)) ; Don't record this command.
       ;; (setq prefix-arg current-prefix-arg)
       )
     (if (condition-case err
             (interactive-form (nth 0 fn))
           (error nil))
         (if keymap-key
             (progn
               (setq do-it
                     (or (not (boundp 'ergoemacs-orig-keymap))
                         (and (boundp 'ergoemacs-orig-keymap)
                              (not ergoemacs-orig-keymap))
                         ;; Overwrite local mode's maps (should issue
                         ;; warning?)
                         (condition-case err
                             (interactive-form
                              (lookup-key ergoemacs-shortcut-override-keymap keymap-key))
                           (error nil))
                         ;; Add key if it changed.
                         (not (eq key (nth 0 fn)))))
               (when  do-it
                 (ergoemacs-debug "Shortcut %s to %s %s" (key-description keymap-key)
                                  (nth 0 fn) (nth 1 fn))
                 (cond
                  ((and (boundp 'ergoemacs-orig-keymap) ergoemacs-orig-keymap)
                   (if (not (memq (nth 0 fn) ergoemacs-send-fn-keys-fns))
                       (define-key ergoemacs-shortcut-override-keymap
                         keymap-key (nth 0 fn))
                     (eval
                      (macroexpand
                       `(defun ,(intern (format "%s-ergoemacs-%s"
                                                (nth 0 fn)
                                                (md5 (key-description (nth 1 fn))))) (&optional arg)
                                                ,(format "Run `%s' or what is remapped to by `command-remapping'.
It also tells the function that you pressed %s, and after run it
sets `this-command' to `%s'. The hook
`ergoemacs-pre-command-hook' tries to set `this-command'  to
`%s' as well."
                                                         (nth 0 fn) (key-description (nth 1 fn))
                                                         (nth 0 fn) (nth 0 fn))
                                                (interactive "P")
                                                (ergoemacs-send-fn
                                                 ,(key-description (nth 1 fn))
                                                 ',(nth 0 fn)))))
                     (define-key ergoemacs-shortcut-override-keymap
                       keymap-key (intern (format "%s-ergoemacs-%s"
                                                  (nth 0 fn)
                                                  (md5 (key-description (nth 1 fn))))))
                     ;; Store override keymap for quickly figuring out
                     ;; what keys are bound where.
                     (define-key ergoemacs-shortcut-override-keymap
                       (read-kbd-macro (format "<override> %s" (key-description keymap-key)))
                       (nth 0 fn))))
                  (t
                   (define-key ergoemacs-shortcut-override-keymap
                     keymap-key (nth 0 fn)))))) 
           (unless (boundp 'keyfreq-no-record)
             (when (featurep 'keyfreq)
               (when keyfreq-mode ;; took out variable count.
                 (when (and (condition-case err
                                (interactive-form (nth 0 fn))
                              (error nil))
                            (condition-case err
                                (symbolp (nth 0 fn))
                              (error nil)))
                   ;; Add function name to count
                   (setq count
                         (gethash (cons major-mode (nth 0 fn))
                                  keyfreq-table))
                   (puthash (cons major-mode (nth 0 fn)) (if count (+ count 1) 1)
                            keyfreq-table)))))
           (setq ergoemacs-shortcut-send-key (key-description (nth 1 fn)))
           (setq ergoemacs-shortcut-send-fn (nth 0 fn))
           ;; repeat only works with a function.
           (when (and repeat
                      (or (not chorded)
                          (not (eq chorded 'global))))
             (when  (string-match "[A-Za-z]$" ctl-c-keys)
               (setq ctl-c-keys (match-string 0 ctl-c-keys))
               (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
               (define-key ergoemacs-repeat-shortcut-keymap
                 (read-kbd-macro ctl-c-keys)
                 `(lambda(&optional arg)
                    (interactive "P")
                    (ergoemacs-send-fn
                     ,(key-description (nth 1 fn))
                     ',(nth 0 fn))))
               (setq ergoemacs-repeat-shortcut-msg
                     (format  "Repeat %s with %s"
                              (ergoemacs-pretty-key key)
                              (ergoemacs-pretty-key ctl-c-keys)))
               ;; Allow time to process the unread command events before
               ;; installing temporary keymap
               (setq ergoemacs-shortcut-send-timer t))))
       ;; Not a function, probably a keymap
       (if keymap-key
           (progn
             ;; (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys) (symbol-value fn))
             )
         (setq prefix-arg current-prefix-arg)
         (setq unread-command-events
               (append
                (listify-key-sequence (read-kbd-macro key))
                unread-command-events))
         (reset-this-command-lengths))))))
  (when (and (not unread-command-events)
             ergoemacs-shortcut-send-key ergoemacs-shortcut-send-fn)
    (cond
     ((memq ergoemacs-shortcut-send-fn ergoemacs-send-fn-keys-fns)
      (ergoemacs-send-fn ergoemacs-shortcut-send-key ergoemacs-shortcut-send-fn))
     (t
      (setq this-command (or (command-remapping
                              ergoemacs-shortcut-send-fn (point))
                             ergoemacs-shortcut-send-fn))
      (call-interactively this-command)))
    
    (when ergoemacs-shortcut-send-timer
      (setq ergoemacs-M-O-timer
            (run-with-timer ergoemacs-M-O-delay nil
                            #'ergoemacs-shortcut-timeout)))))

(defcustom ergoemacs-repeat-ctl-c-ctl-c t
  "Repeat C-c C-c"
  :group 'ergoemacs-mode
  :type 'boolean)

(defun ergoemacs-ctl-c-ctl-c (&optional arg)
  "Ergoemacs C-c C-c. If `ergoemacs-repeat-ctl-c-ctl-c', repeat the command"
  (interactive "P")
  (setq this-command last-command) ; Don't record this command.
  ;; (setq prefix-arg current-prefix-arg)
  (ergoemacs-shortcut-internal "C-c C-c" 'repeat-global ergoemacs-repeat-ctl-c-ctl-c))

(defun ergoemacs-install-shortcuts-map (&optional map dont-complete)
  "Returns a keymap with shortcuts installed.
If MAP is defined, use a copy of that keymap as a basis for the shortcuts.
If MAP is nil, base this on a sparse keymap."
  (let ((ergoemacs-shortcut-override-keymap
         (or map
             (make-sparse-keymap)))
        (ergoemacs-orig-keymap
         (if map
             (copy-keymap map) nil)))
    (maphash
     (lambda(key args)
       (if (condition-case err
               (interactive-form (nth 0 args))
             (error nil))
           (eval (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args) nil ,key)))
         (define-key ergoemacs-shortcut-override-keymap
           key #'(lambda(&optional arg)
                   (interactive "P")
                   (let (overriding-terminal-local-map
                         overriding-local-map)
                     ;; (setq prefix-arg current-prefix-arg)
                     (condition-case err
                         (call-interactively 'ergoemacs-shortcut)
                       (error (beep) (message "%s" err))))))))
     ergoemacs-command-shortcuts-hash)
    ;; Now install the rest of the ergoemacs-mode keys
    (unless dont-complete
      (ergoemacs-setup-keys-for-keymap ergoemacs-shortcut-override-keymap)
      ;; Remove bindings for C-c and C-x so that the extract keyboard
      ;; macro will work correctly on links.  (Issue #121)
      (define-key ergoemacs-shortcut-override-keymap (read-kbd-macro "C-c") nil)
      (define-key ergoemacs-shortcut-override-keymap (read-kbd-macro "C-x") nil))
    ergoemacs-shortcut-override-keymap))

(defvar ergoemacs-describe-keybindings-functions
  '(describe-package
    describe-bindings
    describe-key
    where-is
    describe-key-briefly
    describe-function
    describe-variable
    ergoemacs-describe-major-mode)
  "Functions that describe keys.
Setup C-c and C-x keys to be described properly.")

(defvar ergoemacs-show-true-bindings nil
  "Show the true bindings.  Otherwise, show what the bindings translate to...")

(define-minor-mode ergoemacs-shortcut-override-mode
  "Lookup the functions for `ergoemacs-mode' shortcut keys and pretend they are currently bound."
  nil
  :lighter ""
  :global t
  :group 'ergoemacs-mode
  (if ergoemacs-shortcut-override-mode
      (progn
        (let ((x (assq 'ergoemacs-shortcut-override-mode
                       ergoemacs-emulation-mode-map-alist)))
          (when x
            (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
          ;; Remove shortcuts.
          (setq x (assq 'ergoemacs-shortcut-keys ergoemacs-emulation-mode-map-alist))
          (when x
            (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
          ;; Create keymap
          (ergoemacs-debug-heading "Turn on `ergoemacs-shortcut-override-mode'")
          (setq ergoemacs-shortcut-override-keymap (ergoemacs-install-shortcuts-map))
          ;; Add M-O and M-o key-bindings; Pretend they are the actual
          ;; bindings instead of the M-O and M-o work-rounds.
          (when (eq (key-binding (read-kbd-macro "M-O"))
                    'ergoemacs-M-O)
            (define-key ergoemacs-shortcut-override-keymap
              (read-kbd-macro "M-O")
              (lookup-key ergoemacs-M-O-keymap [timeout])))
          
          (when (eq (key-binding (read-kbd-macro "M-o"))
                    'ergoemacs-M-o)
            (define-key ergoemacs-shortcut-override-keymap
              (read-kbd-macro "M-o")
              (lookup-key ergoemacs-M-o-keymap [timeout])))
          
          (ergoemacs-debug-keymap 'ergoemacs-shortcut-override-keymap)
          (push (cons 'ergoemacs-shortcut-override-mode
                      ergoemacs-shortcut-override-keymap)
                ergoemacs-emulation-mode-map-alist)
          
          (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s" (mapcar (lambda(x) (nth 0 x)) ergoemacs-emulation-mode-map-alist))
          (ergoemacs-debug-heading "Finish `ergoemacs-shortcut-override-mode'")
          (ergoemacs-debug-flush)))
    ;; Add back shortcuts.
    (let ((x (assq 'ergoemacs-shortcut-keys ergoemacs-emulation-mode-map-alist)))
      (when x
        (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
      (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist))))

(defun ergoemacs-remove-shortcuts ()
  "Removes ergoemacs shortcuts from keymaps."
  (let (hashkey lookup override-text-map override orig-map)
    (cond
     (overriding-terminal-local-map
      (when (eq (lookup-key
                 overriding-terminal-local-map
                 (read-kbd-macro "<ergoemacs>")) 'ignore)
        (setq hashkey (md5 (format "override-terminal-orig:%s" overriding-terminal-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          (setq overriding-terminal-local-map lookup)
          (ergoemacs-debug-heading "Remove ergoemacs from `overriding-terminal-local-map'")
          ;; Save old map.
          (ergoemacs-debug-keymap 'overriding-terminal-local-map))))
     (overriding-local-map
      (when (eq (lookup-key overriding-local-map
                            (read-kbd-macro "<ergoemacs>")) 'ignore)
        (setq hashkey (md5 (format "override-local-orig:%s" overriding-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          (ergoemacs-debug-heading "Remove ergoemacs from `overriding-local-map'")
          (setq overriding-local-map lookup)
          (ergoemacs-debug-keymap 'overriding-local-map))))
     ((progn
        (setq override-text-map (get-char-property (point) 'keymap))
        (and (keymapp override-text-map)
             (eq (lookup-key override-text-map
                             (read-kbd-macro "<ergoemacs>"))
                 'ignore)))
      (let ((overlays (overlays-at (point)))
            found)
        (while overlays
          (let* ((overlay (car overlays))
                 (overlay-keymap (overlay-get overlay 'keymap)))
            (if (not (equal overlay-keymap override-text-map))
                (setq overlays (cdr overlays))
              (setq found overlay)
              (setq overlays nil))))
        (setq hashkey (md5 (format "char-map-orig:%s" override-text-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (when lookup
          (ergoemacs-debug-heading "Remove ergoemacs from (get-char-property (point) 'keymap)")
          (setq override-text-map lookup)
          (if found
              (overlay-put found 'keymap override-text-map)
            (ergoemacs-debug "Put into text properties")
            (put-text-property
             (previous-single-property-change (point) 'keymap)
             (next-single-property-change (point) 'keymap)
             'keymap override-text-map))
          (ergoemacs-debug-keymap 'override-text-map)))))))

(defun ergoemacs-install-shortcuts-up ()
  "Installs ergoemacs shortcuts into overriding keymaps.
The keymaps are:
- `overriding-terminal-local-map'
- `overriding-local-map'
- overlays with :keymap property
- text property with :keymap property."
  (let (hashkey lookup override-text-map override orig-map)
    (cond
     (overriding-terminal-local-map
      (when (not
             (eq (lookup-key
                  overriding-terminal-local-map
                  (read-kbd-macro "<ergoemacs>"))
                 'ignore))
        (ergoemacs-debug-heading "Install shortcuts into overriding-terminal-local-map")
        (setq hashkey (md5 (format "override-terminal:%s" overriding-terminal-local-map)))
        (setq orig-map (copy-keymap overriding-terminal-local-map))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq overriding-terminal-local-map lookup)
          (ergoemacs-install-shortcuts-map overriding-terminal-local-map)
          (define-key overriding-terminal-local-map
            (read-kbd-macro  "<ergoemacs>") 'ignore)
          (puthash hashkey overriding-terminal-local-map ergoemacs-extract-map-hash)
          ;; Save old map.
          (setq hashkey (md5 (format "override-terminal-orig:%s" overriding-terminal-local-map)))
          (puthash hashkey orig-map ergoemacs-extract-map-hash))
        (ergoemacs-debug-keymap 'overriding-terminal-local-map)))
     (overriding-local-map
      (when  (not (eq (lookup-key overriding-local-map
                                  (read-kbd-macro "<ergoemacs>"))
                      'ignore))
        (ergoemacs-debug-heading "Install shortcuts into overriding-local-map")
        (setq hashkey (md5 (format "override-local:%s" overriding-local-map)))
        (setq orig-map (copy-keymap overriding-local-map))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq overriding-local-map lookup)
          (ergoemacs-install-shortcuts-map overriding-local-map)
          (define-key overriding-local-map
            (read-kbd-macro "<ergoemacs>") 'ignore)
          (puthash hashkey overriding-local-map ergoemacs-extract-map-hash)
          ;; Save old map.
          (setq hashkey (md5 (format "override-local-orig:%s" overriding-local-map)))
          (puthash hashkey orig-map ergoemacs-extract-map-hash))
        (ergoemacs-debug-keymap 'overriding-local-map)))
     ((progn
        (setq override-text-map (get-char-property (point) 'keymap))
        (and (keymapp override-text-map)
             (not (eq (lookup-key override-text-map
                                  (read-kbd-macro "<ergoemacs>"))
                      'ignore))))
      (ergoemacs-debug-heading "Install shortcuts into (get-char-property (point) 'keymap)")
      (let ((overlays (overlays-at (point)))
            found)
        (while overlays
          (let* ((overlay (car overlays))
                 (overlay-keymap (overlay-get overlay 'keymap)))
            (if (not (equal overlay-keymap override-text-map))
                (setq overlays (cdr overlays))
              (setq found overlay)
              (setq overlays nil))))
        (setq hashkey (md5 (format "char-map:%s" override-text-map)))
        (setq orig-map (copy-keymap override-text-map))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq override-text-map lookup)
          (ergoemacs-install-shortcuts-map override-text-map)
          (define-key override-text-map
            (read-kbd-macro "<ergoemacs>") 'ignore)
          (puthash hashkey override-text-map ergoemacs-extract-map-hash)
          ;; Save old map.
          (setq hashkey (md5 (format "char-map-orig:%s" override-text-map)))
          (puthash hashkey orig-map ergoemacs-extract-map-hash))
        (if found
            (overlay-put found 'keymap override-text-map)
          ;; Overlay not found; change text property
          (ergoemacs-debug "Put into text properties")
          (put-text-property
           (previous-single-property-change (point) 'keymap)
           (next-single-property-change (point) 'keymap)
           'keymap
           override-text-map))
        (ergoemacs-debug-keymap 'override-text-map))))))

(provide 'ergoemacs-shortcuts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-shortcuts.el ends here
