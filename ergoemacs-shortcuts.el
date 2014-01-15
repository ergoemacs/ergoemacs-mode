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
(defmacro ergoemacs-setup-keys-for-keymap (keymap)
  "Setups ergoemacs keys for a specific keymap"
  `(condition-case err
       (let ((no-ergoemacs-advice t)
             (case-fold-search t)
             key
             trans-key input-keys
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
              (when (eq ',keymap 'ergoemacs-keymap)
                (ergoemacs-debug "Fixed %s: %s -> %s %s (%s)"
                                 (nth 0 x) trans-key cmd
                                 key (key-description key))
                (when (string-match "^\\([^ ]+\\) " (nth 0 x))
                  (add-to-list 'input-keys (match-string 1 (nth 0 x)))))
              (if (ergoemacs-global-changed-p trans-key)
                  (progn
                    (ergoemacs-debug "!!!Fixed %s has changed globally." trans-key)
                    (ergoemacs-define-key ,keymap key (lookup-key (current-global-map) key)))
                (setq cmd (nth 1 x))
                (when (not (ergoemacs-define-key ,keymap key cmd))
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
                       ((ergoemacs-shortcut-function-binding cmd)
                        (puthash (read-kbd-macro (key-description key) t)
                                 (list cmd 'global) ergoemacs-command-shortcuts-hash)
                        (define-key ergoemacs-shortcut-keymap key 'ergoemacs-M-O)
                        (if (ergoemacs-is-movement-command-p cmd)
                            (define-key ergoemacs-M-O-keymap [timeout] 'ergoemacs-shortcut-movement-no-shift-select)
                          (define-key ergoemacs-M-O-keymap [timeout] 'ergoemacs-shortcut)))
                       (t
                        (define-key ,keymap key  'ergoemacs-M-O)
                        (define-key ergoemacs-M-O-keymap [timeout] cmd)))
                      (when (eq ',keymap 'ergoemacs-keymap)
                        (ergoemacs-debug
                         "Variable %s: %s (%s) -> %s %s via ergoemacs-M-O"
                         (nth 0 x) trans-key
                         (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)
                        (when (string-match "^\\([^ ]+\\) " (ergoemacs-kbd trans-key t (nth 3 x)))
                          (add-to-list
                           'input-keys
                           (match-string 1 (ergoemacs-kbd trans-key t (nth 3 x)))))))
                  (if (and ergoemacs-fix-M-O
                           (string= (ergoemacs-kbd trans-key t t) "M-o"))
                      (progn
                        (cond  ;; Use shortcut if available.
                         ((ergoemacs-shortcut-function-binding cmd)
                          (puthash (read-kbd-macro (key-description key) t)
                                   (list cmd 'global) ergoemacs-command-shortcuts-hash)
                          (define-key ergoemacs-shortcut-keymap key 'ergoemacs-M-o)
                          (if (ergoemacs-is-movement-command-p cmd)
                              (define-key ergoemacs-M-o-keymap [timeout] 'ergoemacs-shortcut-movement)
                            (define-key ergoemacs-M-o-keymap [timeout] 'ergoemacs-shortcut)))
                         (t
                          (define-key ,keymap key  'ergoemacs-M-o)
                          (define-key ergoemacs-M-o-keymap [timeout] cmd)))
                        (when (eq ',keymap 'ergoemacs-keymap)
                          (ergoemacs-debug "Variable: %s (%s) -> %s %s via ergoemacs-M-o" trans-key
                                           (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)
                          (when (string-match "^\\([^ ]+\\) " (ergoemacs-kbd trans-key t (nth 3 x)))
                            (add-to-list
                             'input-keys
                             (match-string 1 (ergoemacs-kbd trans-key t (nth 3 x)))))))
                    (when cmd
                      (ergoemacs-define-key ,keymap key cmd)
                      (when (eq ',keymap 'ergoemacs-keymap)
                        (ergoemacs-debug "Variable: %s (%s) -> %s %s" trans-key (ergoemacs-kbd trans-key t (nth 3 x)) cmd key)
                        (when (string-match "^\\([^ ]+\\) " (ergoemacs-kbd trans-key t (nth 3 x)))
                          (add-to-list
                           'input-keys
                           (match-string 1 (ergoemacs-kbd trans-key t (nth 3 x))))))))))))
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
         (ergoemacs-debug-keymap ',keymap)
         (when input-keys
           (ergoemacs-debug-heading "Setup Read Input Layer")
           (setq ergoemacs-read-input-keymap (make-sparse-keymap))
           (mapc
            (lambda(key)
              (unless (member key ergoemacs-ignored-prefixes)
                (define-key ergoemacs-read-input-keymap
                  (read-kbd-macro key)
                  `(lambda()
                     (interactive)
                     (ergoemacs-read ,key 'normal)))))
            input-keys)
           (ergoemacs-debug-keymap 'ergoemacs-read-input-keymap)))
     (error
      (ergoemacs-debug "Error: %s" err)
      (ergoemacs-debug-flush))))

(defmacro ergoemacs-with-overrides (&rest body)
  "With the `ergoemacs-mode' mode overrides.
The global map is ignored, but major/minor modes keymaps are included."
  `(let (ergoemacs-mode
         ergoemacs-unbind-keys
         ergoemacs-shortcut-keys
         ergoemacs-modal
         ergoemacs-read-input-keys
         (old-global-map (current-global-map))
         (new-global-map (make-sparse-keymap)))
     (unwind-protect
         (progn
           (use-global-map new-global-map)
           ,@body)
       (use-global-map old-global-map))))

(defmacro ergoemacs-with-global (&rest body)
  "With global keymap, not ergoemacs keymaps."
  `(ergoemacs-without-emulation
    (let (ergoemacs-mode ergoemacs-unbind-keys)
      ,@body)))

(defmacro ergoemacs-with-major-and-minor-modes (&rest body)
  "Without global keymaps and ergoemacs keymaps."
  `(let ((old-global-map (current-global-map))
         (new-global-map (make-sparse-keymap)))
     (unwind-protect
         (progn
           (use-global-map new-global-map)
           (ergoemacs-with-global
            ,@body))
       (use-global-map old-global-map))))

(defmacro ergoemacs-without-emulation (&rest body)
  "Without keys defined at `ergoemacs-emulation-mode-map-alist'.
Also turns off other things like `overriding-terminal-local-map'
and `overriding-local-map'"
  `(let (overriding-terminal-local-map
         overriding-local-map
         ergoemacs-overlays overlay overlay-keymap
         keymap-begin keymap-end
         (overlays (overlays-at (point))))
     ;; Remove most of ergoemacs-mode's key bindings
     (remove-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist)
     (unwind-protect
         (progn
           ;; Sigh.  Now remove overriding overlay and text-map key
           ;; bindings that have been altered by ergoemacs-mode. If
           ;; this does not happen, and press Ctrl+v will lookup the
           ;; emacs equivalent command of Ctrl+y which is ergoemacs'
           ;; redo.  This only occurs in things that add an overlay,
           ;; such a smart-parens mode.
           (ergoemacs-remove-shortcuts)
           ,@body)
       (when ergoemacs-mode
         (add-hook 'emulation-mode-map-alists 'ergoemacs-emulation-mode-map-alist))
       ;; The shortcuts will be put back in post command hook.
       ;; Putting them back here will end up in an infinite loop. 
       ;;(ergoemacs-install-shortcuts-up)
       )))

(defcustom ergoemacs-translate-keys t
  "When translation is enabled, when a command is not defined
look for the command with or without modifiers."
  :type 'boolean
  :group 'ergoemacs-mode)

(defvar ergoemacs-first-variant nil
  "First variant of `ergoemacs-read' key.")
(defvar ergoemacs-describe-key nil)
(defun ergoemacs-describe-key ()
  "Ergoemacs replacement for `describe-key'
Uses `ergoemacs-read'"
  (interactive)
  (setq ergoemacs-describe-key t)
  (ergoemacs-read nil 'normal))

(defvar ergoemacs-single-command-keys nil)
(defvar ergoemacs-mark-active nil)

(defcustom ergoemacs-unchorded-g-is-special t
  "When in unchorded key mode, the key g has special meaning.
g is equivalent to pressing Alt+.
G is equivalent to pessing Alt+Ctrl+."
  :type 'boolean
  :group 'ergoemacs-mode)

(defcustom ergoemacs-unchorded-escape-kbd "SPC"
  "Kbd description for escaping for unchorded sequences.
If equal \"\", then there are no escaped kbd combinations."
  :type 'string
  :group 'ergoemacs-mode)

(defvar ergoemacs-translate-hash (make-hash-table :test 'equal))
(defun ergoemacs-translate (key)
  "Translates KEY and returns a plist of the translations.

:raw
   No translation

:ctl-to-alt
     M-a -> C-a
     C-a -> M-a

:ctl-to-alt-shift
     M-a   -> C-a
     C-a   -> M-a

     S-a   -> M-a
     M-S-a -> C-M-a
     C-S-a -> C-M-a

:unchorded
     a   -> C-a
     M-a -> a
     C-a -> M-a

:unchorded-shift
     a   -> C-a
     M-a -> a
     C-a -> M-a

     S-a   -> M-a
     M-S-a -> C-M-a
     C-S-a -> C-M-a

:shift-translated
    S-a    -> a
    M-S-a  -> M-a
    C-S-a  -> C-a
    Anything without shift is nil.

There are also :XXX-key and :XXX-pretty for actual key-strokes
and `ergoemacs-pretty-key' descriptions.

"
  (let* ((ret (gethash key ergoemacs-command-shortcuts-hash))
         (orig-key key)
         tmp
         case-fold-search
         (key key)
         only-key
         shift-translated
         (ergoemacs-use-ergoemacs-key-descriptions t)
         unshifted-key)
    (or ret
        (progn
          (unless (stringp key)
            (setq key (key-description key))
            (setq orig-key key))
          (cond
           ((string-match "^<.+>$" key)
            (setq only-key (replace-regexp-in-string "[CMS]-" "" key))
            (if (string-match "S-" key)
                (setq shifted-key nil)
              (setq shifted-key (replace-regexp-in-string "<" "<S-" only-key t t))))
           (t
            (setq only-key (replace-regexp-in-string "^.*\\(.\\)$" "\\1" key t)
                  shifted-key (assoc only-key ergoemacs-shifted-assoc))
            (when shifted-key
              (setq shifted-key (cdr shifted-key)))))
          (when (string-match "\\([A-Z]\\)$" key)
            (setq key
                  (replace-match
                   (concat "S-" (downcase (match-string 1 key))) t t key)))
          (when shifted-key
            (setq unshifted-key only-key)
            (unless (string-match "^<.+>$" shifted-key)
              (when (string-match "[A-Z]" shifted-key)
                (setq shifted-key (concat "S-" (downcase shifted-key))))
              (when (string-match "[A-Z]" unshifted-key)
                (setq unshifted-key (concat "S-" (downcase unshifted-key))))))
          (when (string-match "S-" key)
            (setq shift-translated (replace-regexp-in-string "S-" "" key)))
          (put 'ret ':raw key)
          (put 'ret ':raw-key (read-kbd-macro key))
          (put 'ret ':raw-pretty (ergoemacs-pretty-key key))
          
          (if shift-translated
              (progn
                (put 'ret ':shift-translated shift-translated)
                (put 'ret ':shift-translated-key (read-kbd-macro shift-translated))
                (put 'ret ':shift-translated-pretty (ergoemacs-pretty-key shift-translated)))
            (put 'ret ':shift-translated nil)
            (put 'ret ':shift-translated-key nil)
            (put 'ret ':shift-translated-pretty nil))
          
          (put 'ret ':shifted shifted-key)
          (put 'ret ':shifted-key (read-kbd-macro shifted-key))
          (put 'ret ':shifted-pretty (ergoemacs-pretty-key shifted-key))
          
          (put 'ret ':unshifted unshifted-key)
          (put 'ret ':unshifted-key (read-kbd-macro unshifted-key))
          (put 'ret ':unshifted-pretty (ergoemacs-pretty-key unshifted-key))
          
          ;; M-a -> C-a
          ;; C-a -> M-a
          (put 'ret ':ctl-to-alt (replace-regexp-in-string
                                  "\\(^\\|-\\)W-" "\\1M-"
                                  (replace-regexp-in-string
                                   "\\(^\\|-\\)M-" "\\1C-"
                                   (replace-regexp-in-string
                                    "\\(^\\|-\\)C-" "\\1W-" key t) t) t))
          (put 'ret ':ctl-to-alt-key (read-kbd-macro (get 'ret ':ctl-to-alt)))
          (put 'ret ':ctl-to-alt-pretty (ergoemacs-pretty-key (get 'ret ':ctl-to-alt)))
          
          (put 'ret ':ctl-to-alt-shift
               (replace-regexp-in-string
                "M-M-" "C-M-"
                (replace-regexp-in-string
                 "S-" "M-" (get 'ret ':ctl-to-alt) t) t))
          (put 'ret ':ctl-to-alt-shift-key (read-kbd-macro (get 'ret ':ctl-to-alt-shift)))
          (put 'ret ':ctl-to-alt-shift-pretty (ergoemacs-pretty-key (get 'ret ':ctl-to-alt-shift)))
          
          
          (if (string-match "\\(^\\|-\\)[MC]-" (get 'ret ':ctl-to-alt))
              (put 'ret ':unchorded (get 'ret ':ctl-to-alt))
            (put 'ret ':unchorded (concat "W-" (get 'ret ':ctl-to-alt))))
          (put 'ret ':unchorded
               (replace-regexp-in-string
                "W-" "C-"
                (replace-regexp-in-string
                 "C-" "" (get 'ret ':unchorded) t) t))
          (put 'ret ':unchorded-key (read-kbd-macro (get 'ret ':unchorded)))
          (put 'ret ':unchorded-pretty (ergoemacs-pretty-key (get 'ret ':unchorded)))
          
          ;; Now get unchorded shift
          (if (not shift-translated)
              (put 'ret ':unchorded-shift (get 'ret ':unchorded))
            ;; S-a   -> M-a
            ;; M-S-a -> C-M-a
            ;; C-S-a -> C-M-a
            (put 'ret ':unchorded-shift
                 (replace-regexp-in-string
                  "M-M-" "C-M-"
                  (replace-regexp-in-string
                   "S-" "M-" (get 'ret ':ctl-to-alt) t) t)))
          (put 'ret ':unchorded-shift-key (read-kbd-macro (get 'ret ':unchorded-shift)))
          (put 'ret ':unchorded-shift-pretty (ergoemacs-pretty-key (get 'ret ':unchorded-shift)))
          
          (put 'ret ':ctl (concat "C-" unshifted-key))
          (put 'ret ':ctl-key (read-kbd-macro (get 'ret ':ctl)))
          (put 'ret ':ctl-pretty (ergoemacs-pretty-key (get 'ret ':ctl)))
          
          (put 'ret ':alt (concat "M-" unshifted-key))
          (put 'ret ':alt-key (read-kbd-macro (get 'ret ':alt)))
          (put 'ret ':alt-pretty (ergoemacs-pretty-key (get 'ret ':alt)))
          
          (put 'ret ':alt-ctl (concat "M-C-" unshifted-key))
          (put 'ret ':alt-ctl-key (read-kbd-macro (get 'ret ':alt-ctl)))
          (put 'ret ':alt-ctl-pretty (ergoemacs-pretty-key (get 'ret ':alt-ctl)))
          
          (put 'ret ':ctl-shift (concat "C-" shifted-key))
          (put 'ret ':ctl-shift-key (read-kbd-macro (get 'ret ':ctl-shift)))
          (put 'ret ':ctl-shift-pretty (ergoemacs-pretty-key (get 'ret ':ctl-shift)))
          
          (put 'ret ':alt-shift (concat "M-" shifted-key))
          (put 'ret ':alt-shift-key (read-kbd-macro (get 'ret ':alt-shift)))
          (put 'ret ':alt-shift-pretty (ergoemacs-pretty-key (get 'ret ':alt-shift)))
          
          (put 'ret ':alt-ctl-shift (concat "M-C-" shifted-key))
          (put 'ret ':alt-ctl-shift-key (read-kbd-macro (get 'ret ':alt-ctl-shift)))
          (put 'ret ':alt-ctl-shift-pretty (ergoemacs-pretty-key (get 'ret ':alt-ctl-shift)))
          (puthash orig-key (symbol-plist 'ret) ergoemacs-translate-hash)
          (puthash (read-kbd-macro orig-key) (symbol-plist 'ret) ergoemacs-translate-hash)
          (symbol-plist 'ret)))))

(defun ergoemacs-to-sequence (key)
  "Returns a key sequence from KEY.
This sequence is compatible with `listify-key-sequence'."
  (let (input)
    (cond
     ((not key)) ;; Not specified.
     ((eq (type-of key) 'vector) ;; Actual key sequence
      (setq input (listify-key-sequence key)))
     ((eq (type-of key) 'cons) ;; Listified key sequence
      (setq input key))
     ((eq (type-of key) 'string) ;; Kbd code
      (setq input (listify-key-sequence (read-kbd-macro key t)))))
    (symbol-value 'input)))

(defun ergoemacs-read (&optional key type)
  "Read keyboard input and execute command.
The KEY is the keyboard input where the reading begins.  If nil,
read the whole keymap.

TYPE is the keyboard translation type.
It can be: 'ctl-to-alt 'unchorded 'normal"
  (let (next-key
        (key key)
        (real-type type)
        type
        ctl-to-alt
        unchorded
        fn fn-key
        ergoemacs-read-input-keys
        ergoemacs-shortcut-keys
        ergoemacs-shortcut-override-mode
        test-key new-type tmp hash
        input
        (continue-read t)
        (ctl-to-alt-text
         (replace-regexp-in-string "[qQ]" "" (ergoemacs-pretty-key "C-q")))
        message-log-max)
    (setq input (ergoemacs-to-sequence key)
          key nil)
    (while continue-read 
      (when (and (not input) real-type)
        (setq type real-type)
        (setq real-type nil))
      (setq continue-read nil)
      (unless (or (minibufferp) input)
        (message "%s%s%s%s%s"
                 (if ergoemacs-describe-key
                     "Describe key: " "")
                 (if current-prefix-arg
                     (format "%s " current-prefix-arg)
                   "")
                 (cond
                  ((eq type 'ctl-to-alt)
                   (format "<Ctl%sAlt> " 
                           (ergoemacs-unicode-char "↔" " to ")))
                  ((eq type 'unchorded)
                   "<Unchorded> ")
                  (t
                   ""))
                 (if key (ergoemacs-pretty-key key)
                   "")
                 (if (eq type 'unchorded)
                     ctl-to-alt-text "")))
      (setq next-key (key-description (vector (or (pop input) (read-key)))))
      (when (and (or
                  (and ergoemacs-unchorded-g-is-special
                       (or (string= next-key "g") (string= next-key "G")))
                  (string= next-key ergoemacs-unchorded-escape-kbd))
                 (eq type 'unchorded))
        (unless (or (minibufferp) input)
          (message "%s%s%s%s%s"
                   (if ergoemacs-describe-key
                       "Describe key: " "")
                   (if current-prefix-arg
                       (format "%s " current-prefix-arg)
                     "")
                   "<Unchorded> "
                   (if key (ergoemacs-pretty-key key)
                     "")
                   (replace-regexp-in-string
                    "q" ""
                    (cond
                     ((string= next-key "g")
                      (ergoemacs-pretty-key "M-q"))
                     ((string= next-key "G")
                      (ergoemacs-pretty-key "M-C-q"))
                     (t "")))))
        (setq next-key (concat
                        (cond
                         ((string= next-key "g") "M-")
                         ((string= next-key "G") "M-C-")
                         (t ""))
                        (key-description (vector (or (pop input) (read-key)))))))
      (when (member next-key '("M-o" "M-O" "M-[" "ESC"))
        
        )
      ;; M-a ->
      ;; C-a -> M-a
      (setq ctl-to-alt
            (replace-regexp-in-string
             "\\(^\\|-\\)W-" "\\1M-"
             (replace-regexp-in-string
              "\\(^\\|-\\)M-" "\\1C-"
              (replace-regexp-in-string
               "\\(^\\|-\\)C-" "\\1W-" next-key))))
      ;; a   -> C-a
      ;; M-a -> a
      ;; C-a -> M-a
      (if (string-match "\\(^\\|-\\)[MC]-" ctl-to-alt)
          (setq unchorded ctl-to-alt)
        (setq unchorded (concat "W-" ctl-to-alt)))
      (setq unchorded
            (replace-regexp-in-string
             "W-" "C-"
             (replace-regexp-in-string
              "C-" "" unchorded)))
      (setq fn-key (cond
                    ((eq type 'ctl-to-alt) ctl-to-alt)
                    ((eq type 'unchorded) unchorded)
                    (t next-key)))
      (when (string= next-key "ESC")
        (setq next-key "<escape>"))
      ;; Next key is apps/menu
      (cond
       ((and (not key)
             (lookup-key
              ergoemacs-read-input-keymap (read-kbd-macro next-key)))
        (setq key next-key
              type 'normal
              ergoemacs-shortcut-keys t
              continue-read t))
       ((string-match "<\\(menu\\|apps\\)>" next-key)
        ;; Swap translation
        (cond
         ((equal ergoemacs-first-variant 'unchorded)
          (cond
           ((eq type 'ctl-to-alt)
            (setq new-type 'normal))
           ((eq type 'unchorded)
            (setq new-type 'ctl-to-alt))
           ((eq type 'normal)
            (setq new-type 'unchorded))))
         ((equal ergoemacs-first-variant 'ctl-to-alt)
          (cond
           ((eq type 'ctl-to-alt)
            (setq new-type 'unchorded))
           ((eq type 'unchorded)
            (setq new-type 'normal))
           ((eq type 'normal)
            (setq new-type 'ctl-to-alt))))
         (t
          (cond
           ((eq type 'normal)
            (setq new-type 'unchorded))
           ((eq type 'unchorded)
            (setq new-type 'ctl-to-alt))
           ((eq type 'ctl-to-alt)
            (setq new-type 'normal)))))
        (setq type new-type
              continue-read t))
       ((progn
          (setq tmp (lookup-key input-decode-map (read-kbd-macro (if key (concat key " " fn-key) fn-key))))
          (when (and tmp (integerp tmp))
            (setq tmp nil))
          (unless tmp
            (setq tmp (lookup-key local-function-key-map (read-kbd-macro (if key (concat key " " fn-key) fn-key))))
            (when (and tmp (integerp tmp))
              (setq tmp nil))
            (unless tmp
              (setq tmp (lookup-key key-translation-map (read-kbd-macro (if key (concat key " " fn-key) fn-key))))
              (when (and tmp (integerp tmp))
                (setq tmp nil))))
          tmp)
        ;; Should use emacs key translation.
        (cond
         ((keymapp tmp)
          (setq key (if key (concat key " " fn-key) fn-key)
                ergoemacs-shortcut-keys nil
                continue-read t))
         ((and ergoemacs-describe-key (vectorp tmp))
          (message "%s translates to %s"
                   (ergoemacs-pretty-key
                    (if key (concat key " " fn-key) fn-key))
                   (ergoemacs-pretty-key (key-description tmp)))
          (setq ergoemacs-describe-key nil))
         ((vectorp tmp)
          (setq last-input-event tmp)
          (setq prefix-arg current-prefix-arg)
          (setq unread-command-events (append (listify-key-sequence tmp) unread-command-events))
          (reset-this-command-lengths))))
       ((string= next-key
                 (key-description (ergoemacs-key-fn-lookup 'keyboard-quit)))
        (unless (minibufferp)
          (message "%s%s%s Canceled with %s"
                   (if ergoemacs-describe-key
                       "Help for: " "")
                   (cond
                    ((eq type 'ctl-to-alt)
                     (format "<Ctl%sAlt> " 
                             (ergoemacs-unicode-char "↔" " to ")))
                    ((eq type 'unchorded)
                     "<Unchorded> ")
                    (t
                     ""))
                   (if key (ergoemacs-pretty-key key) "")
                   (ergoemacs-pretty-key next-key)))
        (setq ergoemacs-describe-key nil))
       ((progn
          (setq fn (key-binding (read-kbd-macro (if key (concat key " " fn-key) fn-key))))
          (condition-case err
              (interactive-form fn)
            nil))
        (let* ((new-key (if key (concat key " " fn-key) fn-key))
               (new-key-vector (read-kbd-macro new-key t)))
          (setq hash (gethash new-key-vector ergoemacs-command-shortcuts-hash))
          (cond
           ((and (eq fn 'ergoemacs-shortcut) hash
                 (eq 'string (type-of (nth 0 hash)))
                 (memq (nth 1 hash) '(ctl-to-alt unchorded normal)))
            (setq key (nth 0 hash)
                  type (nth 1 hash)
                  ergoemacs-shortcut-keys nil
                  continue-read t))
           ((and (eq fn 'ergoemacs-shortcut) hash
                 (eq 'string (type-of (nth 0 hash))))
            (setq ergoemacs-mark-active mark-active)
            (setq ergoemacs-first-variant nil)
            (setq ergoemacs-single-command-keys new-key-vector)
            (setq prefix-arg current-prefix-arg)
            (setq unread-command-events (append (listify-key-sequence (read-kbd-macro (nth 0 hash))) unread-command-events)))
           ((and (memq fn '(ergoemacs-shortcut ergoemacs-shortcut-movement ergoemacs-shortcut-movement-no-shift-select))
                 (condition-case err
                     (interactive-form (nth 0 hash))
                   (error nil)))
            (ergoemacs-shortcut-remap (nth 0 hash)))
           ((and ergoemacs-shortcut-keys (not ergoemacs-describe-key))
            ;; There is some issue with these key.  Read-key thinks it
            ;; is in a minibuffer, so the recurive minibuffer error is
            ;; raised unless these are put into unread-command-events.
            (setq ergoemacs-mark-active mark-active)
            (setq ergoemacs-first-variant nil)
            (setq ergoemacs-single-command-keys new-key-vector)
            (setq prefix-arg current-prefix-arg)
            (setq unread-command-events (append (listify-key-sequence new-key-vector) unread-command-events)))
           (t
            (setq ergoemacs-first-variant nil)
            (setq fn (or (command-remapping fn (point)) fn))
            (setq ergoemacs-single-command-keys new-key-vector)
            (ergoemacs-send-fn (if key (concat key " " fn-key) fn-key) fn)))))
       (fn ;; not complete.
        (setq key (if key (concat key " " fn-key) fn-key) 
              continue-read t))
       ;; Now try to translate...
       ((and ergoemacs-translate-keys
             (progn
               ;; Look at key without C- in it.
               (when (string-match "C-" fn-key)
                 (setq test-key (replace-match "" t t fn-key))
                 (setq fn (key-binding
                           (read-kbd-macro (if key (concat key " " test-key) test-key)))))
               ;; Look at key without M- in it.
               (when (and (not fn) (string-match "M-" fn-key))
                 (setq test-key (replace-match "" t t fn-key))
                 (setq fn (key-binding
                           (read-kbd-macro (if key (concat key " " test-key) test-key)))))
               ;; Try key with C- in it.
               (unless (or fn (string-match "C-" fn-key))
                 (setq test-key (concat "C-" fn-key))
                 (setq fn (key-binding
                           (read-kbd-macro (if key (concat key " " test-key) test-key)))))
               ;; Try key with M- in it
               (unless (or fn (string-match "M-" fn-key))
                 (setq test-key (concat "M-" fn-key))
                 (setq fn (key-binding
                           (read-kbd-macro (if key (concat key " " test-key) test-key)))))
               (condition-case err
                   (interactive-form fn)
                 (error nil))))
        (setq fn (or (command-remapping fn (point)) fn))
        (setq ergoemacs-shortcut-keys nil)
        (cond
         ((eq fn 'ergoemacs-undefined)
          (setq ergoemacs-single-command-keys (read-kbd-macro (if key (concat key " " next-key) next-key) t)))
         (t
          (setq ergoemacs-single-command-keys (read-kbd-macro (if key (concat key " " test-key) test-key) t))))
        (setq hash (gethash ergoemacs-single-command-keys ergoemacs-command-shortcuts-hash))
        (cond
         ((and (eq fn 'ergoemacs-shortcut) hash
               (eq 'string (type-of (nth 0 hash)))
               (memq (nth 1 hash) '(ctl-to-alt unchorded normal)))
          (setq ergoemacs-single-command-keys nil)
          (setq key (nth 0 hash)
                type (nth 1 hash)
                ergoemacs-shortcut-keys nil
                continue-read t))
         ((and (eq fn 'ergoemacs-shortcut) hash
               (eq 'string (type-of (nth 0 hash))))
          (setq ergoemacs-mark-active mark-active)
          (setq ergoemacs-first-variant nil)
          (setq ergoemacs-single-command-keys new-key-vector)
          (setq prefix-arg current-prefix-arg)
          (setq unread-command-events (append (listify-key-sequence (read-kbd-macro (nth 0 hash))) unread-command-events))
          (setq ergoemacs-single-command-keys nil))
         ((and (memq fn '(ergoemacs-shortcut ergoemacs-shortcut-movement ergoemacs-shortcut-movement-no-shift-select))
               (condition-case err
                   (interactive-form (nth 0 hash))
                 (error nil)))
          (setq ergoemacs-single-command-keys nil)
          (ergoemacs-shortcut-remap (nth 0 hash)))
         (t
          (ergoemacs-send-fn (if key (concat key " " test-key) test-key) fn))))
       (fn
        (setq key (if key (concat key " " test-key) test-key)
              ergoemacs-shortcut-keys t
              continue-read t))
       (t
        (beep)
        (unless (minibufferp)
          (message "%s%s %s is undefined!"
                   (if ergoemacs-describe-key
                       "Describe Key: " "")
                   (if key (ergoemacs-pretty-key key) "")
                   (ergoemacs-pretty-key
                    (cond
                     ((and (eq type 'unchorded)
                           (not (string-match "M-" next-key)))
                      (concat "C-" next-key))
                     (t next-key)))))
        (setq ergoemacs-describe-key nil)))))
  (when ergoemacs-single-command-keys 
    (setq ergoemacs-read-input-keys nil)))

(defun ergoemacs-define-key (keymap key def)
  "Defines KEY in KEYMAP to be DEF.
Similar to `define-key'.

DEF can be:
1. A function; If globally defined, this is defined by `ergoemacs-shortcut-remap'
2. A list of functions
3. A keymap
4. A kbd-code that this shortcuts to with `ergoemacs-read'

"
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
              (cond
               ((condition-case err
                    (interactive-form (nth 0 def))
                  (error nil))
                (define-key keymap key
                  `(lambda(&optional arg)
                     (interactive "P")
                     (setq this-command last-command) ; Don't record this command.
                     ;; (setq prefix-arg current-prefix-arg)
                     (ergoemacs-shortcut-remap ,(nth 0 def)))))
               (t
                (define-key keymap key
                `(lambda(&optional arg)
                   (interactive "P")
                   (setq this-command last-command) ; Don't record this command.
                   ;; (setq prefix-arg current-prefix-arg)
                   (ergoemacs-read ,(nth 0 def) ',(nth 1 def))))))))
        (mapc
         (lambda(new-def)
           (unless found
             (when (condition-case err
                       (interactive-form new-def)
                     (error nil))
               (setq found
                     (ergoemacs-define-key keymap key new-def)))))
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
           (ergoemacs-shortcut-function-binding def))
      (puthash (read-kbd-macro (key-description key) t)
               (list def 'global) ergoemacs-command-shortcuts-hash)
      (if (ergoemacs-is-movement-command-p def)
          (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
              (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut-movement-no-shift-select)
            (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut-movement))
        (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut)))     
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
          (if (ergoemacs-is-movement-command-p def)
              (if (let (case-fold-search) (string-match "\\(S-\\|[A-Z]$\\)" (key-description key)))
                  (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut-movement-no-shift-select)
                (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut-movement))
            (define-key ergoemacs-shortcut-keymap key 'ergoemacs-shortcut)))
      (unless (lookup-key keymap key)
        (define-key keymap key
          `(lambda(&optional arg)
             (interactive "P")
             (setq this-command last-command) ; Don't record this command.
             ;; (setq prefix-arg current-prefix-arg)
             (ergoemacs-read ,def)))))
    
    t)
   (t nil)))

(defvar ergoemacs-ignored-prefixes '("C-h" "<f1>" "C-x" "C-c" "ESC" "<escape>"))

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

(defvar ergoemacs-send-fn-keys-fns '(ergoemacs-undefined ergoemacs-shortcut)
  "List of functions where `unread-command-events' are sent with `ergoemacs-send-fn'.")

(defun ergoemacs-send-fn (key fn &optional message)
  "Sends the function."
  (setq ergoemacs-shortcut-send-key nil
        ergoemacs-shortcut-send-fn nil
        ergoemacs-shortcut-send-timer nil)
  (setq ergoemacs-shortcut-send-fn (or (command-remapping fn (point)) fn))
  (cond
   ((and ergoemacs-describe-key ergoemacs-shortcut-send-fn
         (or ergoemacs-show-true-bindings
             (and (not ergoemacs-show-true-bindings)
                  (not (memq ergoemacs-shortcut-send-fn '(ergoemacs-shortcut ergoemacs-shortcut-movement ergoemacs-shortcut-movement-no-shift-select))))))
    (let ((desc-fn ergoemacs-shortcut-send-fn))
      (ergoemacs-shortcut-override-mode 1)
      (describe-function desc-fn)
      (ergoemacs-shortcut-override-mode -1))
    (setq ergoemacs-describe-key nil))
   ((memq ergoemacs-shortcut-send-fn ergoemacs-send-fn-keys-fns)
    (let ((old-unread (listify-key-sequence (or ergoemacs-single-command-keys (this-command-keys))))
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
           (ergoemacs-with-global
            (call-interactively ergoemacs-shortcut-send-fn nil ,(read-kbd-macro key t))))))
      ;; Some commands, like isearch, put commands in
      ;; `unread-command-events'; Try to handle these.
      (when (and unread-command-events
                 (equal unread-command-events new-unread))
        (setq unread-command-events old-unread))))
   (t
    (ergoemacs-with-global
     (call-interactively ergoemacs-shortcut-send-fn)))))

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

(defcustom ergoemacs-shortcuts-do-not-lookup
  '(execute-extended-command)
  "Functions that `ergoemacs-mode' does not lookup equivalent key-bindings for. "
  :group 'ergoemacs-mode
  :type '(repeat
          (symbol :tag "Function to call literally:")))

(defun ergoemacs-get-override-function (keys)
  "See if KEYS has an overriding function.
If so, return the function, otherwise return nil.

First it looks up ergoemacs user commands.  These are defined in the key
<ergoemacs-user> KEYS

If there is no ergoemacs user commands, look in override map.
This is done by looking up the function for KEYS with
`ergoemacs-with-overrides'.

If the overriding function is found make sure it isn't the key
defined in the major/minor modes (by
`ergoemacs-with-major-and-minor-modes'). "
  (let ((override (key-binding (read-kbd-macro (format "<ergoemacs-user> %s" (key-description keys)))))
        cmd1 cmd2)
    (unless (condition-case err
              (interactive-form override)
            (error nil))
      (setq override nil))
    (unless override
      (setq cmd1 (ergoemacs-with-overrides
                  (key-binding keys)))
      (when (condition-case err
                  (interactive-form cmd1)
              (error nil))
        (setq cmd2 (ergoemacs-with-major-and-minor-modes
                    (key-binding keys)))
        (unless (eq cmd1 cmd2)
          (setq override cmd1))))
    override))

(defun ergoemacs-shortcut---internal ()
  (let* ((keys (or ergoemacs-single-command-keys (this-single-command-keys)))
         (args (gethash keys ergoemacs-command-shortcuts-hash))
         (one (nth 0 args)) tmp override)
    (unless args
      (setq keys (read-kbd-macro (key-description keys) t))
      (setq args (gethash keys ergoemacs-command-shortcuts-hash))
      (setq one (nth 0 args)))
    (setq override (ergoemacs-get-override-function keys))
    (cond
     (override
      (call-interactively override))
     ((condition-case err
          (interactive-form one)
        (error nil))
      (ergoemacs-shortcut-remap one))
     ((and (eq 'string (type-of one))
           (progn
             (setq tmp (key-binding (read-kbd-macro one) t nil (point)))
             tmp))
      (cond
       ((string-match "self-insert" (symbol-name tmp))
        (setq ergoemacs-mark-active mark-active)
        (setq prefix-arg current-prefix-arg)
        (setq unread-command-events (append (read-kbd-macro one t) unread-command-events))
        (reset-this-command-lengths))
       ((memq tmp ergoemacs-send-fn-keys-fns)
        (ergoemacs-send-fn (key-descripiton keys) tmp))
       (t
        (setq this-command (or (command-remapping tmp (point)) tmp))
        (if (not ergoemacs-describe-key)
            (progn
              (call-interactively this-command))
          (ergoemacs-shortcut-override-mode 1)
          (describe-function this-command)
          (ergoemacs-shortcut-override-mode -1)
          (setq ergoemacs-describe-key nil)))))
     ((eq 'string (type-of one))
      (ergoemacs-read one (nth 1 args)))
     (t 
      (message "Keys: %s; Args: %s; One: %s" keys args one)))
    (setq ergoemacs-single-command-keys nil)))

(defun ergoemacs-shortcut-movement (&optional opt-args)
  "Shortcut for other key/function for movement keys.

This function is `cua-mode' aware for movement and supports
`shift-select-mode'.

Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for
`ergoemacs-single-command-keys' or `this-single-command-keys'."
  (interactive "^P")
  (ergoemacs-shortcut-movement-no-shift-select opt-args))

(defun ergoemacs-shortcut-movement-no-shift-select (&optional opt-args)
  "Shortcut for other key/function in movement keys without shift-selection support.

Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for
`ergoemacs-single-command-keys' or `this-single-command-keys'.
"
  (interactive "P")
  (ergoemacs-shortcut---internal))
(put 'ergoemacs-shortcut-movement 'CUA 'move)

(defun ergoemacs-shortcut (&optional opt-args)
  "Shortcut for other key/function for non-movement keys.
Calls the function shortcut key defined in
`ergoemacs-command-shortcuts-hash' for `ergoemacs-single-command-keys' or `this-single-command-keys'."
  (interactive "P")
  (ergoemacs-shortcut---internal))

(defvar ergoemacs-shortcut-send-key nil)
(defvar ergoemacs-shortcut-send-fn nil)
(defvar ergoemacs-shortcut-send-timer nil)

(defvar ergoemacs-debug-shortcuts nil)

(defun ergoemacs-shortcut-function-binding (function &optional dont-ignore-menu)
  "Determine the global bindings for FUNCTION.

This also considers archaic emacs bindings by looking at
`ergoemacs-where-is-global-hash' (ie bindings that are no longer
in effect)."
  (or
   (if dont-ignore-menu
       (where-is-internal function (current-global-map))
     (remove-if
      '(lambda(x)
         (or (eq 'menu-bar (elt x 0)))) ; Ignore menu-bar functions
      (where-is-internal function (current-global-map))))
   (gethash function ergoemacs-where-is-global-hash)))

(defcustom ergoemacs-use-function-remapping t
  "Uses function remapping.
For example in `org-agenda-mode' the standard key for
save-buffer (C-x C-s) `org-save-all-org-buffers'"
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-shortcut-remap-list
  (function
   &optional keymap
   ignore-desc dont-swap-for-ergoemacs-functions dont-ignore-commands)
  "Determine the possible current remapping of FUNCTION.

It based on the key bindings bound to the default emacs keys
For the corresponding FUNCTION.

It returns a list of (remapped-function key key-description).

If KEYMAP is non-nil, lookup the binding based on that keymap
instead of the keymap with ergoemacs disabled.

For example, in `org-agenda-mode' C-x C-s runs
`org-save-all-org-buffers' instead of `save-buffer'.

Therefore:

 (ergoemacs-shortcut-remap-list 'save-buffer)

returns

 ((org-save-all-org-buffers [24 19] \"C-x C-s\"))

in `org-agenda-mode'.

If you wish to disable this remapping, you can set
`ergoemacs-use-function-remapping' to nil.

When IGNORE-DESC is t, then ignore key description filters.  When
IGNORE-DESC is nil, then ignore keys that match \"[sAH]-\".  When
IGNORE-DESC is a regular expression, then ignore keys that match
the regular expression.

By default, if an interactive function ergoemacs-X exists, use
that instead of the remapped function.  This can be turned off by
changing DONT-SWAP-FOR-ERGOEMACS-FUNCTIONS.

This command also ignores anything that remaps to FUNCTION,
`ergoemacs-this-command' and
`ergoemacs-shortcut-ignored-functions'.  This can be turned off
by setting changed by setting DONT-IGNORE-COMMANDS to t.

When KEYMAP is defined, `ergoemacs-this-command' is not included
in the ignored commands.

Also this ignores anything that is changed in the global keymap.

If <ergoemacs-user> key is defined, ignore this functional remap.

<ergoemacs-user> key is defined with the `define-key' advice when
running `run-mode-hooks'.  It is a work-around to try to respect
user-defined keys.
"
  (if (not ergoemacs-use-function-remapping) nil
    (let ((key-bindings-lst (ergoemacs-shortcut-function-binding function))
          case-fold-search
          (old-global-map (current-global-map))
          (new-global-map (make-sparse-keymap))
          ret ret2)
      (unwind-protect
          (progn
            (use-global-map new-global-map)
            (when key-bindings-lst
              (mapc
               (lambda(key)
                 (let* (fn
                        (key-desc (key-description key))
                        (user-key
                         (read-kbd-macro
                          (concat "<ergoemacs-user> " key-desc)))
                        fn2)
                   (cond
                    (keymap
                     (setq fn (lookup-key keymap key t))
                     (if (eq fn (lookup-key keymap user-key))
                         (setq fn nil)
                       (unless (condition-case err
                                   (interactive-form fn)
                                 (error nil))
                         (setq fn nil))))
                    (t
                     (ergoemacs-with-global
                      (setq fn (key-binding key t nil (point)))
                      (if (eq fn (key-binding user-key t nil (point)))
                          (setq fn nil)))))
                   (when fn
                     (cond
                      ((eq ignore-desc t))
                      ((eq (type-of ignore-desc) 'string)
                       (when (string-match ignore-desc key-desc)
                         (setq fn nil)))
                      (t
                       (when (string-match "[sAH]-" key-desc)
                         (setq fn nil))))
                     (when fn
                       (unless dont-swap-for-ergoemacs-functions
                         (setq fn2 (condition-case err
                                       (intern-soft (concat "ergoemacs-" (symbol-name fn)))
                                     (error nil)))
                         (when (and fn2 (not (interactive-form fn2)))
                           (setq fn2 nil)))
                       (when (memq fn (append
                                       `(,function ,(if keymap nil ergoemacs-this-command))
                                       ergoemacs-shortcut-ignored-functions))
                         (setq fn nil))
                       (when (and fn2
                                  (memq fn2 (append
                                             `(,function ,(if keymap nil ergoemacs-this-command))
                                             ergoemacs-shortcut-ignored-functions)))
                         (setq fn2 nil))
                       (cond
                        (fn2
                         (push (list fn2 key key-desc) ret2))
                        (fn (push (list fn key key-desc) ret)))))))
               key-bindings-lst)))
        (use-global-map old-global-map))
      (when ret2
        (setq ret (append ret2 ret)))
      (symbol-value 'ret))))

(defun ergoemacs-shortcut-remap (function &optional keys)
  "Runs the FUNCTION or whatever `ergoemacs-shortcut-remap-list' returns.
Will use KEYS or `this-single-command-keys', if cannot find the
original key binding.
"
  (let ((send-keys (or keys (key-description (this-single-command-keys))))
        (fn-lst (ergoemacs-shortcut-remap-list function))
        (fn function)
        send-fn)
    (when fn-lst
      (setq send-keys (nth 2 (nth 0 fn-lst)))
      (setq fn (nth 0 (nth 0 fn-lst))))
    (setq send-fn (memq ergoemacs-shortcut-send-fn ergoemacs-send-fn-keys-fns))
    (cond
     (send-fn
      (ergoemacs-send-fn send-keys fn))
     (t
      (setq this-command (or (command-remapping fn (point)) fn))
      (if (not ergoemacs-describe-key)
          (call-interactively this-command)
        (ergoemacs-shortcut-override-mode 1)
        (describe-function this-command)
        (ergoemacs-shortcut-override-mode -1)
        (setq ergoemacs-describe-key nil))))))

(defun ergoemacs-install-shortcuts-map (&optional map dont-complete)
  "Returns a keymap with shortcuts installed.
If MAP is defined, use a copy of that keymap as a basis for the shortcuts.
If MAP is nil, base this on a sparse keymap."
  (let ((ergoemacs-shortcut-override-keymap
         (or map
             (make-sparse-keymap)))
        (ergoemacs-orig-keymap
         (if map
             (copy-keymap map) nil))
        fn-lst)
    (maphash
     (lambda(key args)
       (cond
        ((condition-case err
                 (interactive-form (nth 0 args))
               (error nil))
         (setq fn-lst (ergoemacs-shortcut-remap-list
                       (nth 0 args) ergoemacs-orig-keymap))
         (if fn-lst
             (define-key ergoemacs-shortcut-override-keymap key
               (nth 0 (nth 0 fn-lst)))
           (unless dont-complete
             (define-key ergoemacs-shortcut-override-keymap key
               (nth 0 args)))))
        (t
         (let ((hash (gethash key ergoemacs-command-shortcuts-hash)))
           (cond
            ((not hash) ;; Shouldn't get here
             (define-key ergoemacs-shortcut-override-keymap
               key #'(lambda(&optional arg)
                       (interactive "P")
                       (let (overriding-terminal-local-map
                             overriding-local-map)
                         ;; (setq prefix-arg current-prefix-arg)
                         (condition-case err
                             (call-interactively 'ergoemacs-shortcut)
                           (error (beep) (message "%s" err))))))) 
            ((condition-case err
                 (interactive-form (nth 0 hash))
               (error nil))
             (define-key ergoemacs-shortcut-override-keymap
               key (nth 0 hash)))
            (t
             (define-key ergoemacs-shortcut-override-keymap key
               `(lambda(&optional arg)
                  (interactive "P")
                  (ergoemacs-read ,(nth 0 hash) ',(nth 1 hash))))))))))
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
  (let ((inhibit-read-only t)
        hashkey lookup override-text-map override orig-map)
    (cond
     ((and overriding-terminal-local-map
           (eq saved-overriding-map t))
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
            (when (and (previous-single-property-change (point) 'keymap)
                       (next-single-property-change (point) 'keymap))
              (ergoemacs-debug "Put into text properties")
              (put-text-property
             (previous-single-property-change (point) 'keymap)
             (next-single-property-change (point) 'keymap)
             'keymap override-text-map)))
          (ergoemacs-debug-keymap 'override-text-map)))))))

(defun ergoemacs-install-shortcuts-up ()
  "Installs ergoemacs shortcuts into overriding keymaps.
The keymaps are:
- `overriding-terminal-local-map'
- `overriding-local-map'
- overlays with :keymap property
- text property with :keymap property."
  (let ((inhibit-read-only t)
        hashkey lookup override-text-map override orig-map)
    (cond
     ((and overriding-terminal-local-map
           (eq saved-overriding-map t))
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
          (ergoemacs-install-shortcuts-map override-text-map t)
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
