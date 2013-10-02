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

(defvar ergoemacs-prefer-shortcuts t ;; Prefer shortcuts.
  "Prefer shortcuts")

(defvar ergoemacs-command-shortcuts-hash (make-hash-table :test 'equal)
  "List of command shortcuts.")

(defvar ergoemacs-repeat-shortcut-keymap (make-sparse-keymap)
  "Keymap for repeating often used shortcuts like C-c C-c.")

(defvar ergoemacs-repeat-shortcut-msg ""
  "Message for repeating keyboard shortcuts like C-c C-c")

(defun ergoemacs-shortcut-timeout ()
  (message ergoemacs-repeat-shortcut-msg)
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
      (let (ergoemacs-shortcut-keys
            ergoemacs-unbind-keys
            (minor (intern-soft (format "ergoemacs-%s-hook-mode" major-mode)))
            old-minor
            ;; if chorded is undefined, shortcut is to ergoemacs-keys
            ;; if chorded doesn't have global in it it acts on
            ;; ergoemacs-keys.
            ;; otherwise the shortcut is acting on non-ergoemacs keys.
            (ergoemacs-mode
             (or (eq chorded 'repeat)
                 (not chorded)))
            fn fn-lst new-fn fn-override)
        (when (condition-case err
                  (interactive-form key)
                (error nil))
          (setq ergoemacs-mode nil))
        (setq ergoemacs-unbind-keys ergoemacs-mode)
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
          (setq ergoemacs-mode nil)
          (setq ergoemacs-unbind-keys nil)
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
            (gethash key ergoemacs-where-is-global-hash)))
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
              (message "%s is not defined." (ergoemacs-pretty-key key)))
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
                             (let ((cmd ',(nth 0 fn)))
                               (setq cmd (or (command-remapping cmd (point)) cmd))
                               (setq prefix-arg current-prefix-arg)
                               (setq this-command cmd)
                               (call-interactively cmd nil ,(nth 1 fn))))))
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
                    (call-interactively (or (command-remapping (nth 0 fn) (point)) (nth 0 fn))
                                        nil (nth 1 fn))
                  (error (beep) (message "%s" err)))
                ;; repeat only works with a function.
                (when (and repeat
                           (or (not chorded)
                               (not (eq chorded 'global))))
                  (when  (string-match "[A-Za-z]$" ctl-c-keys)
                    (setq ctl-c-keys (match-string 0 ctl-c-keys))
                    (setq ergoemacs-repeat-shortcut-keymap (make-keymap))
                    (define-key ergoemacs-repeat-shortcut-keymap (read-kbd-macro ctl-c-keys) fn)
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
      (princ (concat
              (if current-prefix-arg
                  (format "%s " current-prefix-arg)
                "")
              (format "%s%s " key-type
                      (ergoemacs-pretty-key key))))))))

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

(defun ergoemacs-install-shortcuts-up ()
  "Installs ergoemacs shortcuts into overriding keymaps.
The keymaps are:
- `overriding-terminal-local-map'
- `overriding-local-map'
- overlays with :keymap property
- text property with :keymap property."
  (let (hashkey lookup override-text-map)
    (cond
     (overriding-terminal-local-map
      (when (not
             (eq (lookup-key
                  overriding-terminal-local-map
                  (read-kbd-macro "<ergoemacs>"))
                 'ignore))
        (ergoemacs-debug-heading "Install shortcuts into overriding-terminal-local-map")
        (setq hashkey (md5 (format "override-terminal:%s" overriding-terminal-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq overriding-terminal-local-map lookup)
          (ergoemacs-install-shortcuts-map overriding-terminal-local-map)
          (define-key overriding-terminal-local-map
            (read-kbd-macro  "<ergoemacs>") 'ignore)
          (puthash hashkey overriding-terminal-local-map ergoemacs-extract-map-hash))
        (ergoemacs-debug-keymap 'overriding-terminal-local-map)))
     (overriding-local-map
      (when  (not (eq (lookup-key overriding-local-map
                                  (read-kbd-macro "<ergoemacs>"))
                      'ignore))
        (ergoemacs-debug-heading "Install shortcuts into overriding-local-map")
        (setq hashkey (md5 (format "override-local:%s" overriding-local-map)))
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq overriding-local-map lookup)
          (ergoemacs-install-shortcuts-map overriding-local-map)
          (define-key overriding-local-map
            (read-kbd-macro "<ergoemacs>") 'ignore)
          (puthash hashkey overriding-local-map ergoemacs-extract-map-hash))
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
        (setq lookup (gethash hashkey ergoemacs-extract-map-hash))
        (if lookup
            (setq override-text-map lookup)
          (ergoemacs-install-shortcuts-map override-text-map)
          (define-key override-text-map
            (read-kbd-macro "<ergoemacs>") 'ignore)
          (puthash hashkey override-text-map ergoemacs-extract-map-hash))
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
