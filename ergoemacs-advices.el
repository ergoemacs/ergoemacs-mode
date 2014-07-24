;;; ergoemacs-advices.el --- advices for ErgoEmacs -*- lexical-binding: t -*-

;; Copyright © 2013, 2014  Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Keywords: convenience

;; ErgoEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; ErgoEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ErgoEmacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;; Todo:

;; 

;;; Code:

(eval-when-compile 
  (require 'cl)
  (require 'ergoemacs-macros))

(defvar ergoemacs-advices '()
  "List of advices to enable and disable when ergoemacs is running.")

(defvar ergoemacs-run-mode-hooks nil)
(defmacro ergoemacs-define-overrides (&rest body)
  "Force the define-keys to work"
  `(let ((ergoemacs-run-mode-hooks t))
     ,@body))
(defvar ergoemacs-mode)
(defvar ergoemacs-hook-functions)
(defadvice add-hook (around ergoemacs-add-hook-advice (hook function &optional append  local) activate)
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'
If `pre-command-hook' is used and `ergoemacs-mode' is enabled add to `ergoemacs-pre-command-hook' instead."
  (cond
   ((and ergoemacs-mode (eq hook 'pre-command-hook)
         (memq hook ergoemacs-hook-functions))
    (add-hook 'ergoemacs-pre-command-hook function append local))
   (t
    ad-do-it)))

(defadvice remove-hook (around ergoemacs-remove-hook-advice (hook function &optional local) activate)
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'.
If `pre-command-hook' is used and `ergoemacs-mode' is remove from `ergoemacs-pre-command-hook' instead."
  (cond
   ((and ergoemacs-mode (eq hook 'pre-command-hook)
         (memq hook ergoemacs-hook-functions))
    (remove-hook 'ergoemacs-pre-command-hook function local))
   (t
    ad-do-it)))
(defadvice describe-buffer-bindings (around ergoemacs-describe-buffer-bindings activate)
  "Describes buffer bindings without `ergoemacs-read-input-keys' enabled"
  (let (ergoemacs-read-input-keys
        ergoemacs-read-shortcut-keys
        ergoemacs-read-no-shortcut-keys)
    ad-do-it))
(defvar ergoemacs-global-map)
(defadvice define-key (around ergoemacs-define-key-advice (keymap key def) activate)
  "This does the right thing when modifying `ergoemacs-keymap'.
Also adds keymap-flag for user-defined keys run with `run-mode-hooks'."
  (let ((is-global-p (equal keymap (current-global-map)))
        (is-local-p (equal keymap (current-local-map)))
        ergoemacs-local-map)
    (when (and is-local-p (not ergoemacs-local-emulation-mode-map-alist))
      (set (make-local-variable 'ergoemacs-local-emulation-mode-map-alist) '()))
    ;; (when is-local-p
    ;;   (setq ergoemacs-local-map
    ;;         (cdr (car ergoemacs-local-emulation-mode-map-alist)))
    ;;   (unless (ignore-errors (keymapp ergoemacs-local-map))
    ;;     (setq ergoemacs-local-map (make-sparse-keymap)))
    ;;   (define-key ergoemacs-local-map key def)
    ;;   (setq ergoemacs-local-emulation-mode-map-alist
    ;;         (list (cons 'ergoemacs-mode (make-sparse-keymap)))))
    (if (and ergoemacs-run-mode-hooks
             (not (equal keymap ergoemacs-global-map))
             (not (equal keymap ergoemacs-keymap)))
        (let ((ergoemacs-run-mode-hooks nil)
              (new-key (read-kbd-macro
                        (format "<ergoemacs-user> %s"
                                (key-description key)))))
          (define-key keymap new-key def)))
    ad-do-it
    (when is-global-p
      (let ((vk key))
        (ergoemacs-global-set-key-after key)
        (unless (vectorp vk) ;; Do vector def too.
          (setq vk (read-kbd-macro (key-description key) t))
          (ergoemacs-global-set-key-after vk))))))

(defvar ergoemacs-global-override-rm-keys '())
;;; Advices enabled or disabled with ergoemacs-mode
(defvar ergoemacs-ignore-advice nil)

(declare-function ergoemacs-theme-component--ignore-globally-defined-key
                  "ergoemacs-theme-engine.el")
(defvar ergoemacs-global-changed-cache)
(defvar ergoemacs-global-not-changed-cache)
(defun ergoemacs-global-set-key-after (key)
  (if ergoemacs-ignore-advice nil
    (let ((kd (key-description key)))
      (unless (or (and (vectorp key)
                       (memq (elt key 0) '(menu-bar 27 remap)))
                  (and (not (vectorp key))
                       (string= "ESC" kd)))
        ;; Let `ergoemacs-mode' know these keys have changed.
        (pushnew kd ergoemacs-global-changed-cache :test 'equal)
        (setq ergoemacs-global-not-changed-cache (delete kd ergoemacs-global-not-changed-cache))
        ;; Remove the key from `ergoemacs-mode' bindings
        (ergoemacs-theme-component--ignore-globally-defined-key key t)))))

(defvar ergoemacs-use-M-x-p)
(eval-after-load "helm"
  '(progn
     (defadvice helm-M-x-transformer (around ergoemacs-helm-M-x-transformer activate)
       "Make ``helm-M-x' work correctly with `ergoemacs-mode' pretty keys"
       (let ((ergoemacs-use-M-x-p t))
         ad-do-it))))


(defadvice cua-mode (around ergoemacs-activate-only-selection-mode (arg) activate)
  "When `ergoemacs-mode' is enabled, enable `cua-selection-mode' instead of plain `cua-mode'."
  (when ergoemacs-mode
    ;; Do NOT allow cua-mode to do the C-x and C-c
    ;; hack, `ergoemacs-mode' needs to do a similar hack to allow
    ;; backspace in key sequences.
    (setq-default cua-enable-cua-keys nil))
  ad-do-it
  (require 'cua-rect)
  ;; Reset `cua--keymap-alist' -- make it compatible with
  ;; `ergoemacs-mode'
  (setq cua--rectangle-keymap (make-sparse-keymap))
  (setq cua--rectangle-initialized nil)
  (if ergoemacs-mode
      (setq cua--rectangle-modifier-key ergoemacs-cua-rect-modifier)
    (setq cua--rectangle-modifier-key 'meta))
  (cua--init-rectangles)
  (setq cua--keymap-alist
        (progn
          (cua--init-rectangles)
          `((cua--ena-prefix-override-keymap . ,cua--prefix-override-keymap)
            (cua--ena-prefix-repeat-keymap . ,cua--prefix-repeat-keymap)
            (cua--ena-cua-keys-keymap . ,cua--cua-keys-keymap)
            (cua--ena-global-mark-keymap . ,cua--global-mark-keymap)
            (cua--rectangle . ,cua--rectangle-keymap)
            (cua--ena-region-keymap . ,cua--region-keymap)
            (cua-mode . ,cua-global-keymap))))
  (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
    (customize-mark-as-set 'cua-enable-cua-keys)))

(defadvice icicle-mode (around ergoemacs-icicle-play (arg) activate)
  "Allow `ergoemacs-mode' to play nicely with `icicle-mode'."
  (let ((oee ergoemacs-mode))
    (when oee ;; Remove key bindings
      (ergoemacs-mode -1))
    ad-do-it
    (when oee ;; Add them back.  Now icy-mode should play nice.
      (ergoemacs-mode 1))))

(defadvice guide-key/close-guide-buffer (around ergoemacs-guide-key/close-guide-buffer activate)
  "Don't close guide buffer when reading ergoemacs-mode keys."
  (unless (and ergoemacs-mode ergoemacs-read-key
               ergoemacs-read-key-last-help
               (equal ergoemacs-read-key-last-help ergoemacs-read-key))
    ad-do-it))

(defadvice guide-key/format-guide-buffer (around ergoemacs-guide-key/format-guide-buffer-advice (key-seq &optional regexp) activate)
  "Format keys in `ergoemacs-mode' style"
  (when ergoemacs-mode
    (goto-char (point-min))
    (while (re-search-forward (format"^%s \\([^ \t]+\\)" (regexp-quote (key-description key-seq))) nil t)
      (replace-match (concat (key-description key-seq) " " (substring (ergoemacs-pretty-key (match-string 1)) 1 -1)) t t)))
  ad-do-it
  (when (and ergoemacs-mode ergoemacs-use-unicode-brackets)
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\|  \\)\\[" nil t)
      (replace-match (format "\\1%s" (ergoemacs-unicode-char "【" "[")))
      (skip-chars-forward "^\\]")
      (when (looking-at "\\]")
        (replace-match (ergoemacs-unicode-char "】" "]"))))))

(defcustom ergoemacs-helm-expand-user-dirs 't
  "Expand user directories under helm.
This makes helm behave more like `ido-find-file'"
  :group 'ergoemacs-mode
  :type 'boolean)

(eval-after-load "helm-files"
  '(progn
     (defadvice helm-ff-auto-expand-to-home-or-root (around ergoemacs-helm-ido-user-dirs activate)
      "Allow `helm-find-files' to expand user directories.
For example ~ergoemacs/ would expand to /usr/ergoemacs or
whatever that points to...

This require `ergoemacs-mode' to be enabled as well as
`ergoemacs-helm-expand-user-dirs' to be true.
"
      (cond
       ((and ergoemacs-helm-expand-user-dirs
             ergoemacs-mode
             (helm-file-completion-source-p)
             (string-match "/\\(~[^/]*/\\)$" helm-pattern)
             (with-current-buffer (window-buffer (minibuffer-window)) (eolp))
             (not (string-match helm-ff-url-regexp helm-pattern)))
        (let ((input (match-string 1 helm-pattern)))
          (if (file-directory-p input)
              (setq helm-ff-default-directory
                    (setq input (file-name-as-directory input)))
            (setq helm-ff-default-directory (file-name-as-directory
                                             (file-name-directory input))))
          (with-helm-window
            (helm-set-pattern input)
            (helm-check-minibuffer-input))))
       (t
        ad-do-it)))))


(defadvice run-mode-hooks (around ergoemacs-run-hooks activate)
  "`ergoemacs-mode' run-hooks advice helps user define keys properly.
This assumes any key defined while running a hook is a user-defined hook."
  (let ((ergoemacs-run-mode-hooks t))
    ad-do-it))

(defadvice fancy-startup-tail (around ergoemacs-fancy-startup-tail activate)
  "Modify fancy startup to use ergoemacs-mode keys"
  (if (not ergoemacs-mode) (progn ad-do-it)
    (unless concise
      (fancy-splash-insert
       :face 'variable-pitch
       "\nTo start...     "
       :link `("Open a File"
               ,(lambda (_button) (call-interactively 'find-file))
               "Specify a new file's name, to edit the file")
       "     "
       :link `("Open Home Directory"
               ,(lambda (_button) (dired "~"))
               "Open your home directory, to operate on its files")
       "     "
       :link `("Customize Startup"
               ,(lambda (_button) (customize-group 'initialization))
               "Change initialization settings including this screen")
       "\n"))
    (fancy-splash-insert
     :face 'variable-pitch "To quit a partially entered command, type "
     :face 'default (substitute-command-keys "\\[keyboard-quit]")
     :face 'variable-pitch ".\n")
    (fancy-splash-insert :face `(variable-pitch font-lock-builtin-face)
                         "\nThis is "
                         (emacs-version)
                         "\n"
                         :face '(variable-pitch (:height 0.8))
                         emacs-copyright
                         "\n")
    (and auto-save-list-file-prefix
         ;; Don't signal an error if the
         ;; directory for auto-save-list files
         ;; does not yet exist.
         (file-directory-p (file-name-directory
                            auto-save-list-file-prefix))
         (directory-files
          (file-name-directory auto-save-list-file-prefix)
          nil
          (concat "\\`"
                  (regexp-quote (file-name-nondirectory
                                 auto-save-list-file-prefix)))
          t)
         (fancy-splash-insert :face '(variable-pitch font-lock-comment-face)
                              "\nIf an Emacs session crashed recently, "
                              "type "
                              :face '(fixed-pitch font-lock-comment-face)
                              (substitute-command-keys "\\[recover-session]")
                              :face '(variable-pitch font-lock-comment-face)
                              "\nto recover"
                              " the files you were editing."))
    (when concise
      (fancy-splash-insert
       :face 'variable-pitch "\n"
       :link `("Dismiss this startup screen"
               ,(lambda (_button)
                  (when startup-screen-inhibit-startup-screen
                    (customize-set-variable 'inhibit-startup-screen t)
                    (customize-mark-to-save 'inhibit-startup-screen)
                    (custom-save-all))
                  (let ((w (get-buffer-window "*GNU Emacs*")))
                    (and w (not (one-window-p)) (delete-window w)))
                  (kill-buffer "*GNU Emacs*")))
       "  ")
      (when (or user-init-file custom-file)
        (let ((checked (create-image "checked.xpm"
                                     nil nil :ascent 'center))
              (unchecked (create-image "unchecked.xpm"
                                       nil nil :ascent 'center)))
          (insert-button
           " "
           :on-glyph checked
           :off-glyph unchecked
           'checked nil 'display unchecked 'follow-link t
           'action (lambda (button)
                     (if (overlay-get button 'checked)
                         (progn (overlay-put button 'checked nil)
                                (overlay-put button 'display
                                             (overlay-get button :off-glyph))
                                (setq startup-screen-inhibit-startup-screen
                                      nil))
                       (overlay-put button 'checked t)
                       (overlay-put button 'display
                                    (overlay-get button :on-glyph))
                       (setq startup-screen-inhibit-startup-screen t)))))
        (fancy-splash-insert :face '(variable-pitch (:height 0.9))
                             " Never show it again.")))))


(defadvice easy-menu-filter-return (around ergoemacs-easy-menu-filter-return activate)
  "Makes sure that the keys are correct in the menu."
  (let ((ret (ignore-errors ad-do-it)))
    (when ergoemacs-mode
      (require 'ergoemacs-menus)
      (ergoemacs-preprocess-menu-keybindings ret))
    ret))


;;; Unfortunately, the advice route doesn't seem to work for these
;;; functions :(

;;; key-description
(declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
(declare-function ergoemacs-real-key-description
                  "ergoemacs-advices.el" (keys &optional prefix) t)
(fset 'ergoemacs-real-key-description (symbol-function 'key-description))
(defvar ergoemacs-key-description-commands '(describe-function))
(defun ergoemacs-key-description (keys &optional prefix)
  "Allows `describe-function' to show the `ergoemacs-pretty-key' bindings.
Uses `ergoemacs-real-key-description'."
  (let ((ret (ergoemacs-real-key-description keys prefix)))
    (when (and ergoemacs-mode
               (memq this-command ergoemacs-key-description-commands))
      (setq ret (ergoemacs-pretty-key ret)))
    ret))

(declare-function ergoemacs-real-substitute-command-keys "ergoemacs-advices.el" (string) t)
(fset 'ergoemacs-real-substitute-command-keys (symbol-function 'substitute-command-keys))

(defvar ergoemacs-original-keys-to-shortcut-keys-regexp)
(defvar ergoemacs-original-keys-to-shortcut-keys)
(defvar ergoemacs-unbind-keys)
(defvar ergoemacs-read-input-keys)
(defvar ergoemacs-repeat-keys)
(defvar ergoemacs-shortcut-keys)
(defvar ergoemacs-modal)
(defvar ergoemacs-no-shortcut-keys)
(defvar ergoemacs-use-unicode-brackets)
(declare-function ergoemacs-emulations "ergoemacs-mode.el")
(declare-function ergoemacs-remove-shortcuts "ergoemacs-shortcuts.el")
(defun ergoemacs-substitute-command (string &optional map)
  "Substitutes command STRING within MAP or currently bound keys."
  (save-match-data
    (let* ((test (ergoemacs-with-global
                  (ergoemacs-real-substitute-command-keys
                   (or (and map (concat map string)) string))))
           (test-vect (read-kbd-macro test t))
           (test-hash (gethash test-vect ergoemacs-original-keys-to-shortcut-keys)))
      (if test-hash
          (progn
            (setq test (ergoemacs-real-key-description (nth 0 test-hash)))
            (ergoemacs-pretty-key test))
        (let (ergoemacs-modal
              ;; (ergoemacs-shortcut-keys (not map))
              ergoemacs-shortcut-keys
              (ergoemacs-no-shortcut-keys (not map))
              ergoemacs-repeat-keys
              ergoemacs-read-input-keys)
          (ergoemacs-pretty-key
           (ergoemacs-real-substitute-command-keys
            (or (and map (concat map string)) string))))))))

(defun ergoemacs-substitute-map--1 (string)
  (substring
   (replace-regexp-in-string
    "`\\(binding\\|Prefix Command\\|-------\\)'" "\\1"
    (replace-regexp-in-string
     "---|\n|-" "---|"
     (replace-regexp-in-string
      "^|'[ \t]*|$" "|-"
      (replace-regexp-in-string
       "' |\n.*(that binding is.*\n|'" "' (shadowed)"
       (replace-regexp-in-string
        "^" "|"
        (replace-regexp-in-string
         "$" "' |"
         (replace-regexp-in-string
          "\\([ \t]\\{2,\\}\\|\t\\)" "\\1 | `"
          string))))))) 0 -2))

(declare-function ergoemacs-unicode-char "ergoemacs-translate.el")
(defun ergoemacs-substitute-map (string &optional function)
  (save-match-data
    (let* (ret
           ergoemacs-use-unicode-brackets
           (max1 0) (max2 0)
           (function (or function 'ergoemacs-real-substitute-command-keys))
           (test (ergoemacs-with-global
                  (funcall function string)))
           (shortcut-list '()))
      (while (string-match (format "^%s.*$"ergoemacs-original-keys-to-shortcut-keys-regexp) test)
        (push (match-string 0 test) shortcut-list)
        (setq test
              (replace-match "" nil nil test)))
      (let (ergoemacs-modal ergoemacs-repeat-keys ergoemacs-read-input-keys
                            ergoemacs-shortcut-keys ergoemacs-no-shortcut-keys)
        (setq test (funcall function string))
        (when (string-match ".*\n.*\n" test)
          (setq ret (ergoemacs-substitute-map--1
                     (concat (match-string 0 test)
                             (mapconcat (lambda(x) x) shortcut-list "\n")
                             (replace-match "" nil nil test))))))
      (with-temp-buffer
        (insert ret)
        (goto-char (point-min))
        (while (re-search-forward ".*\\(ergoemacs-shortcut\\|Prefix Command\\).*" nil t)
          (delete-region (point-at-bol) (point-at-eol))
          (when (looking-at "\n+")
            (replace-match "")))
        (while (search-forward "`??'" nil t)
          (replace-match (concat " " (ergoemacs-unicode-char "λ" "?") "  ") t t))
        (goto-char (point-min))
        (forward-line 2)
        (while (re-search-forward "^|\\(.*?\\)[ \t]+|" nil t)
          (setq test (ergoemacs-pretty-key (match-string 1)))
          (replace-match (format "| %s |" test) t t)
          (setq max1 (max max1 (length test))
                max2 (max max2 (length (buffer-substring (point) (point-at-eol))))))
        (setq test (concat "|"
                           (make-string (+ max1 2) ?-)
                           "+"
                           (make-string (max 0 (- max2 1)) ?-)
                           "|"))
        (goto-char (point-min))
        (insert test "\n")
        (goto-char (point-max))
        (insert "\n" test "\n\n")
        (goto-char (point-min))
        (while (re-search-forward "|-.*\\(\n|-.*\\)*" nil t)
          (replace-match test t t))
        (goto-char (point-min))
        (while (re-search-forward "^| *\\(.*?[^ ]\\) +| *\\(.*?[^ ]\\) +|$" nil t)
          (replace-match (format "| \\1%s | \\2%s |"
                                 (make-string (max 0 (- max1 (length (match-string 1)))) ? )
                                 (make-string (max 0 (- max2 (+ 3 (length (match-string 2))))) ? )) t))
        (setq ret (buffer-string)))
      ret)))


(defvar ergoemacs-mode)
(defun ergoemacs-substitute-command-keys (string)
  "Substitute key descriptions for command names in STRING.
`ergoemacs-mode' replacement for substitute-command-keys.

Actual substitute-command-keys is always in
`ergoemacs-real-substitute-command-keys'.

Each substring of the form \\=\\[COMMAND] is replaced by either a
keystroke sequence that invokes COMMAND, or \"M-x COMMAND\" if COMMAND
is not on any keys.

Each substring of the form \\=\\\{MAPVAR} is replaced by a summary of
the value of MAPVAR as a keymap.  This summary is similar to the one
produced by `describe-bindings'.  The summary ends in two newlines
 (used by the helper function `help-make-xrefs' to find the end of the
      summary).

Each substring of the form \\=\\<MAPVAR> specifies the use of MAPVAR
as the keymap for future \\=\\[COMMAND] substrings.
\\=\\= quotes the following character and is discarded;
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.

Return the original STRING if no substitutions are made.
Otherwise, return a new string, without any text properties."
  (save-match-data
    (if (not string) nil
      (let (ret mapvar (pt 0) tmp)
        (if (not ergoemacs-mode)
            (setq ret (ergoemacs-real-substitute-command-keys string))
          (while (string-match "\\(\\(?:\\\\=\\)?\\)\\\\\\(\\[\\|<\\|{\\)\\(.*?\\)\\(\\]\\|>\\|}\\)" string pt)
            (cond
             ((string-match-p "\\\\=" (match-string 1 string)) ; Quoted
              (setq pt (+ (length (match-string 2 string))
                          (length (match-string 3 string))
                          (length (match-string 4 string))
                          (match-beginning 0)))
              (setq string (replace-match "\\\\\\2\\3\\4" t nil string)))
             ((and (string-match-p "<" (match-string 2 string)) ; Choose Map \<>
                   (string-match-p ">" (match-string 4 string)))
              (setq mapvar (concat "\\<" (match-string 3 string) ">"))
              (setq string (replace-match "" nil nil string))
              (setq pt (match-beginning 0)))
             ((and (string-match-p "{" (match-string 2 string)) ; Entire map
                   (string-match-p "}" (match-string 4 string)))
              (setq tmp (ergoemacs-substitute-map (match-string 0 string)))
              (setq string (replace-match tmp t t string))
              (setq pt (+ (length tmp) (match-beginning 0))))
             ((and (string-match-p "\\[" (match-string 2 string)) ; command
                   (string-match-p "\\]" (match-string 4 string)))
              (setq tmp (ergoemacs-substitute-command (match-string 0 string) mapvar))
              (setq string (replace-match tmp t t string))
              (setq pt (+ (length tmp) (match-beginning 0))))))
	  (setq pt 0 ret string)
	  (while (string-match "\\\\=" ret pt)
	    (setq ret (replace-match "" nil t ret))
	    (setq pt (match-beginning 0))
	    (when (string=  "\\=" (substring ret pt (min (+ pt 2) (length ret))))
	      (setq pt (+ pt 2))))
          (setq pt 0)
          (while (string-match "\\<C-u\\>" ret pt)
            (setq string
                  (ergoemacs-substitute-command "\\[universal-argument]" "\\<global-map>"))
            (setq ret (replace-match string t t ret))
            (setq pt (+ (length string) (match-beginning 0))))
	  (when (not ergoemacs-use-M-x-p)
	    (setq ret (replace-regexp-in-string "\\(\\<M-x\\|<execute>\\) " (ergoemacs-substitute-command "\\[execute-extended-command] " "\\<global-map>")
						ret t t))))
        ret))))

(declare-function ergoemacs-real-completing-read "ergoemacs-advices.el"
                  (prompt collection &optional
                          predicate require-match
                          initial-input hist def inherit-input-method) t)
(fset 'ergoemacs-real-completing-read (symbol-function 'completing-read))
(defun ergoemacs-completing-read (prompt collection &optional
                                         predicate require-match
                                         initial-input hist def inherit-input-method)
  "Ergoemacs replacement of `completing-read'.
Allows `execute-extended-command' to show the proper keys.
The real command is always `ergoemacs-real-completing-read'.
"
  (ergoemacs-real-completing-read
   (substitute-command-keys
    (replace-regexp-in-string "\\<M-x " "\\[execute-extended-command] " prompt t t))
   collection predicate require-match
   initial-input hist def inherit-input-method))

(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
(fset 'ergoemacs-real-key-binding (symbol-function 'key-binding))
(defun ergoemacs-key-binding (key &optional accept-default no-remap position)
  "Return the binding for command KEY in the without `ergoemacs-mode' enabled.
Uses `ergoemacs-real-key-binding' to get the key-binding."
  (ergoemacs-with-global
   (ergoemacs-real-key-binding key accept-default no-remap position)))



(defun ergoemacs-enable-c-advices (&optional disable)
  "Enabling advices for C code and complex changes to functions.
DISABLE when non-nil.
Assumes ergoemacs-real-FUNCTION and ergoemacs-FUNCTION as the two functions to toggle"
  (dolist (ad '(completing-read substitute-command-keys key-binding key-description))
    (cond
     (disable
      (fset ad (symbol-function (intern (concat "ergoemacs-real-" (symbol-name ad))))))
     (t
      (fset ad (symbol-function (intern (concat "ergoemacs-" (symbol-name ad)))))))))
(provide 'ergoemacs-advices)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advices.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
