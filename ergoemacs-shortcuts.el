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

(defun ergoemacs-send-fn (key fn &optional message)
  "Sends the function."
  (let ((cmd fn))
    (setq cmd (or (command-remapping cmd (point)) cmd))
    (setq this-command cmd)
    (setq prefix-arg current-prefix-arg)
    ;; (let (message-log-max)
    ;;   (message "%s %s: %s" (ergoemacs-pretty-key key) cmd))
    (call-interactively cmd nil (read-kbd-macro (format "%s" key) t))))

(defun ergoemacs-menu-send-prefix (prefix-key untranslated-key type)
  "Extracts maps for PREFIX-KEY UNTRANSLATED-KEY of TYPE."
  (setq this-command last-command) ; Don't record this command.
  (setq prefix-arg current-prefix-arg)
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
    (setq prefix-arg current-prefix-arg)
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

(defun ergoemacs-shortcut (&optional arg)
  "Shortcut for other key/function.
Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for `this-command-keys-vector'.  The
workhorse of this function is in `ergoemacs-shortcut-internal'."
  (interactive "P")
  (let ((args (gethash (this-command-keys-vector)
                       ergoemacs-command-shortcuts-hash)))
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
      (setq prefix-arg current-prefix-arg)
      (if (interactive-form (nth 0 args))
          (eval (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args))))
        (eval (macroexpand `(ergoemacs-shortcut-internal ,(nth 0 args) ',(nth 1 args))))))))

(defun ergoemacs-shortcut-internal (key &optional chorded repeat keymap-key)
  "Ergoemacs Shortcut.

KEY is the keyboard shortcut.

CHORDED is a variable that alters to keymap to allow unchorded
key sequences.  Also if CHORDED is 'global, then make this a
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

When REPEAT is a variable name, then an easy repeat is setup for the command.

For example if you bind <apps> m to Ctrl+c Ctrl+c, this allows Ctrl+c Ctrl+c to be repeated by m.

When KEYMAP-KEY is non-nil, define the KEYMAP-KEY on the `ergoemacs-shortcut-override-keymap'

When `override-text-map' is bound and defined only look up based
on that key.
"
  (cond
   ((or (not chorded)
        (condition-case err
            (interactive-form key)
          (error nil))
        (memq chorded '(repeat repeat-global global-repeat global)))
    ;; A single function for the key shortcut.
    (let ((ctl-c-keys (key-description (this-command-keys))))
      (let* ((ergoemacs-mode
              (if (condition-case err
                      (interactive-form key)
                    (error nil)) nil
                (or (eq chorded 'repeat)
                    (not chorded))))
             (ergoemacs-unbind-keys ergoemacs-mode)
             (minor (intern-soft (format "ergoemacs-%s-hook-mode" major-mode)))
             old-minor ergoemacs-shortcut-keys
             fn fn-lst new-fn fn-override)
        ;; Temporarily unbind ergoemacs-major-mode-hook-mode
        (when minor
          (setq old-minor (symbol-value minor))
          (set minor nil))
        (cond
         ((condition-case err
              (interactive-form key)
            (error nil))
          ;; FIXME:  Overlays that are installed/removed based on
          ;; pre-command-hook status can be disrupted.  This is the
          ;; case in `auto-complete-mode'.  Therefore,
          ;; `ergoemacs-mode' currently does not translate these keys
          ;; correctly :(

          ;; I tried a post-command-hook setting up a temporary
          ;; overlay map with the actual command.  It was too slow...

          ;; Currently fixed by a hook :)
          
          ;; Lookup function on non-ergoemacs keymaps.
          (let (ergoemacs-mode ergoemacs-unbind-keys)
            (mapc
             (lambda(cur-key)
               (unless (let (case-fold-search)
                         ;; only use when M- C- are used
                         (string-match "\\(s-\\|A-\\|H-\\)"
                                       (key-description cur-key)))
                 (let ((binding
                        (if (and keymap-key (boundp 'ergoemacs-orig-keymap)
                                 ergoemacs-orig-keymap)
                            (lookup-key ergoemacs-orig-keymap cur-key t)
                          (key-binding cur-key t nil (point)))))
                   (setq new-fn (intern-soft (format "erogemacs-%s" binding)))
                   (when (and new-fn (condition-case err
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
                               (memq binding
                                     ergoemacs-shortcut-ignored-functions))
                     (add-to-list 'fn-lst (list binding
                                                (read-kbd-macro
                                                 (key-description cur-key) t)))))))
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
            ;; FIXME: If new functions exist, have user option to use
            ;; these functions

            ;; For now, just use the first function.
            (setq fn (nth 0 fn-lst)))
           (t  ; Could not find another function, just use the
               ; function passed to `ergoemacs-shortcut'
            (setq fn (list key
                           (read-kbd-macro
                            (key-description
                             (or (where-is-internal
                                  key (current-global-map) t)
                                 (this-command-keys))) t))))))
         (t ;; lookup keybinding for the function keys.          
          (setq fn (list (key-binding (read-kbd-macro key))
                         (read-kbd-macro key t)))))
        (if (not fn)
            (unless keymap-key
              (let (message-log-max)
                (message "%s is not defined." (ergoemacs-pretty-key key))))
          (unless keymap-key
            (setq this-command (nth 0 fn)) ; Don't record this command.
            (setq prefix-arg current-prefix-arg))
          (if (condition-case err
                  (interactive-form (nth 0 fn))
                (error nil))
              (if keymap-key
                  (let ((do-it
                         (or (not (boundp 'ergoemacs-orig-keymap))
                             (and (boundp 'ergoemacs-orig-keymap) (not ergoemacs-orig-keymap))
                             ;; Overwrite local mode's maps (should issue
                             ;; warning?)
                             (condition-case err
                                 (interactive-form
                                  (lookup-key ergoemacs-shortcut-override-keymap keymap-key))
                               (error nil))
                             ;; Add key if it changed.
                             (not (eq key (nth 0 fn))))))
                    (when  do-it
                        (ergoemacs-debug "Shortcut %s to %s %s" (key-description keymap-key)
                                       (nth 0 fn) (nth 1 fn))
                        (cond
                         ((and (boundp 'ergoemacs-orig-keymap) ergoemacs-orig-keymap)
                        (eval
                         (macroexpand
                          `(defun ,(intern (format "%s-ergoemacs" (nth 0 fn)))
                             (&optional arg)
                             ,(format "Run `%s' or what is remapped to by `command-remapping'.
It also tells the function that you pressed %s, and after run it
sets `this-command' to `%s'. Also after
`ergoemacs-pre-command-hook' `this-command' should be set to
`%s'"
                                      (nth 0 fn) (key-description (nth 1 fn))
                                      (nth 0 fn) (nth 0 fn))
                             (interactive "P")
                             (ergoemacs-send-fn
                              ,(key-description (nth 1 fn))
                              ',(nth 0 fn)))))
                        (define-key ergoemacs-shortcut-override-keymap
                          keymap-key (intern (format "%s-ergoemacs" (nth 0 fn))))
                        
                        ;; Store override keymap for quickly figuring out
                        ;; what keys are bound where.
                        (define-key ergoemacs-shortcut-override-keymap
                          (read-kbd-macro (format "<override> %s" (key-description keymap-key)))
                          (nth 0 fn)))
                       (t
                        (define-key ergoemacs-shortcut-override-keymap
                          keymap-key (nth 0 fn))))))
                (unless (boundp 'keyfreq-no-record)
                  (when (featurep 'keyfreq)
                    (when keyfreq-mode
                      (let ((command fn) count)
                        ;; Add function name to to counter.
                        (setq count (gethash (cons major-mode command)
                                             keyfreq-table))
                        (puthash (cons major-mode command) (if count (+ count 1) 1)
                                 keyfreq-table)))))
                (condition-case err
                    (let ((ergoemacs-mode t))
                      (call-interactively (or (command-remapping (nth 0 fn) (point)) (nth 0 fn))
                                          nil (nth 1 fn)))
                  (error (beep) (message "%s" err)))
                ;; repeat only works with a function.
                (when (and repeat
                           (or (not chorded)
                               (not (eq chorded 'global))))
                  (when  (string-match "[A-Za-z]$" ctl-c-keys)
                    (setq ctl-c-keys (match-string 0 ctl-c-keys))
                    (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
                    (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys)
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
                    (setq ergoemacs-M-O-timer
                          (run-with-timer ergoemacs-M-O-delay nil
                                          #'ergoemacs-shortcut-timeout)))))
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
              (reset-this-command-lengths))))
        (when minor
          (set minor old-minor)))))
   (keymap-key ;; extract key prefixes.
    )
   (t ;; key prefix
    (setq ergoemacs-push-M-O-timeout nil) ;; Cancel timeouts
    (setq this-command last-command) ; Don't record this command.
    (setq prefix-arg current-prefix-arg)
    (let (key-seq
          (key-type
           (cond
            ((eq chorded 'unchorded)
             "Unchorded")
            ((eq chorded 'ctl-to-alt)
             (format "Ctl%sAlt"
                     (ergoemacs-unicode-char "↔" " to ")))
            (t "Normal")))
          deactivate-mark)
      (eval (macroexpand '(ergoemacs-extract-maps ergoemacs-current-extracted-map key)))
      (set-temporary-overlay-map ergoemacs-current-extracted-map)
      (setq ergoemacs-first-extracted-variant chorded)
      (setq key-seq
            (read-kbd-macro
             (format "<%s> %s" key-type key)))
      (setq key-seq (listify-key-sequence key-seq))
      (reset-this-command-lengths)
      (setq unread-command-events
            (append key-seq unread-command-events))
      (setq key-type (concat "<" key-type "> "))
      (when (string= key-type "<Normal> ")
        (setq key-type ""))
      (let (message-log-max)
        (message (concat
                  (if current-prefix-arg
                      (format "%s " current-prefix-arg)
                    "")
                  (format "%s%s " key-type
                          (ergoemacs-pretty-key key)))))))))

(defcustom ergoemacs-repeat-ctl-c-ctl-c t
  "Repeat C-c C-c"
  :group 'ergoemacs-mode
  :type 'boolean)

(defun ergoemacs-ctl-c-ctl-c (&optional arg)
  "Ergoemacs C-c C-c. If `ergoemacs-repeat-ctl-c-ctl-c', repeat the command"
  (interactive "P")
  (setq this-command last-command) ; Don't record this command.
  (setq prefix-arg current-prefix-arg)
  (ergoemacs-shortcut-internal "C-c C-c" 'repeat-global ergoemacs-repeat-ctl-c-ctl-c))

(defun ergoemacs-install-shortcuts-map (&optional map)
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
       (if (interactive-form (nth 0 args))
           (eval (macroexpand `(ergoemacs-shortcut-internal ',(nth 0 args) ',(nth 1 args) nil ,key)))
         (eval (macroexpand `(ergoemacs-shortcut-internal ,(nth 0 args) ',(nth 1 args) nil ,key)))))
     ergoemacs-command-shortcuts-hash)
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
  "Lookup the functions for `ergoemacs-mode' shortcut keys."
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
          (ergoemacs-debug-flush)))))

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
