;;; ergoemacs-test.el --- tests for ErgoEmacs issues

;; Copyright © 2013-2021 Free Software Foundation, Inc.

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
  (require 'cl-lib)
  (require 'ergoemacs-macros))

(declare-function ergoemacs-translate--keymap "ergoemacs-translate")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")

(defvar ergoemacs-map--)
(defvar ergoemacs-layout-us)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-command-loop-type)
(defvar ergoemacs-dir)
(defvar ergoemacs-mode)
(defvar dired-sort-map)
(defvar dired-mode-map)

(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")
(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-mode "ergoemacs-mode")

(declare-function ergoemacs-command-loop--mouse-command-drop-first "ergoemacs-command-loop")

(declare-function ergoemacs-copy-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-cut-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-eshell-here "ergoemacs-functions")
(declare-function ergoemacs-paste "ergoemacs-functions")

(declare-function ergoemacs-translate--quail-to-ergoemacs "ergoemacs-translate")
(declare-function ergoemacs-translate-layout "ergoemacs-translate")

(require 'ert)
(require 'elp)
;;; Not sure why `cl-gensym' is called, probably from `ert'/`elp'?
;; Suppress: "the function `cl-gensym' might not be defined at
;; runtime" warning.
(autoload 'cl-gensym "cl-macs.el")
(defvar ergoemacs-test-lorem-ipsum
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defun ergoemacs-test-fast ()
  "Fast test of ergoemacs-mode (doesn't include keyboard startup issues)."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (not (tag :slow))))
  (call-interactively 'elp-results))

(defun ergoemacs-test-search ()
  "Test search functionality in ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :search)))
  (call-interactively 'elp-results))

(defun ergoemacs-test-map-keymap ()
  "Test search functionality in ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :map-keymap)))
  (call-interactively 'elp-results))

(defun ergoemacs-test-calc ()
  "Test for calc."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :calc)))
  (call-interactively 'elp-results))

(defun ergoemacs-test-no-calc ()
  "Test for calc."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (not (tag :calc))))
  (call-interactively 'elp-results))

(defun ergoemacs-test-shift-select ()
  "Shift-selection test for ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :shift-select)))
  (call-interactively 'elp-results))

(defun ergoemacs-test-translate ()
  "Translation test for ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :translate)))
  (call-interactively 'elp-results))

(defun ergoemacs-test-interactive ()
  "Interactive test for ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :interactive)))
  (call-interactively 'elp-results))

;;;###autoload
(defun ergoemacs-test ()
  "Test ergoemacs issues."
  (interactive)
  (let ((ret t)
        (test))
    (elp-instrument-package "ergoemacs-")
    (ert "^ergoemacs-test-")
    (call-interactively 'elp-results)))

(ert-deftest ergoemacs-test-copy-paste-cut-line-or-region ()
  "Issue #68.
kill-ring function name is used and such doesn't exist. It errs when
not using cua or cutting line. I think kill-region is what is meant."
  (let ((old-c cua-mode)
        (ret t))
    (cua-mode -1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (delete-region (point-min) (point-max))
      (insert ergoemacs-test-lorem-ipsum)
      (condition-case _err
          (ergoemacs-cut-line-or-region)
        (error (setq ret nil)))
      (kill-buffer (current-buffer)))
    (when old-c
      (cua-mode 1))
    (should ret)
    )
  )

;;; Functionality Test

(ert-deftest ergoemacs-test-function-bol-or-what ()
  "Test beginning of line functionality."
  (let ((ergoemacs-end-of-comment-line t)
        (ergoemacs-back-to-indentation t))
    (with-temp-buffer
      (emacs-lisp-mode) ; Turn on ergoemacs-mode 
      (insert "(progn\n  (ergoemacs-mode 1)) ; Turn on ergoemacs-mode")
      (goto-char (point-max))
      (call-interactively 'ergoemacs-beginning-of-line-or-what)
      (should (string= "Turn on ergoemacs-mode"
                       (buffer-substring (point) (point-at-eol))))
      (call-interactively 'ergoemacs-beginning-of-line-or-what)
      (should (string= " ; Turn on ergoemacs-mode"
                       (buffer-substring (point) (point-at-eol))))
      (call-interactively 'ergoemacs-beginning-of-line-or-what)
      (should (string= "(ergoemacs-mode 1)) ; Turn on ergoemacs-mode"
                       (buffer-substring (point) (point-at-eol))))
      (call-interactively 'ergoemacs-beginning-of-line-or-what)
      (should (string= "  (ergoemacs-mode 1)) ; Turn on ergoemacs-mode"
                       (buffer-substring (point) (point-at-eol)))))))


(ert-deftest ergoemacs-test-function-eol-or-what ()
  "Test beginning of line functionality."
  (let ((ergoemacs-end-of-comment-line t)
        (ergoemacs-back-to-indentation t))
    (with-temp-buffer
      (emacs-lisp-mode) ; Turn on ergoemacs-mode
      (insert "(progn\n  (ergoemacs-mode 1)) ; Turn on ergoemacs-mode")
      (goto-char (point-max))
      (beginning-of-line)
      
      (call-interactively 'ergoemacs-end-of-line-or-what)
      (should (string= " ; Turn on ergoemacs-mode"
                       (buffer-substring (point) (point-at-eol))))
      (call-interactively 'ergoemacs-end-of-line-or-what)
      (should (= (point) (point-at-eol))))))

(ert-deftest ergoemacs-test-function-unbind-commands-active ()
  "Make sure the unbound keys work"
  (should (eq 'undefined (key-binding (read-kbd-macro "C-x C-s")))))

;;; Org-mode

;;; Calc

;;; Command Loop

(ert-deftest ergoemacs-test-command-loop-C-x-8-! ()
  "Test that unicode translations work.
See Issue #138."
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (execute-kbd-macro (kbd "C-x 8 !"))
    (should (string= "¡" (buffer-string)))
    (kill-buffer (current-buffer))))

(ert-deftest ergoemacs-test-command-loop-C-x-8-A ()
  "Test that unicode translations work.
See Issue #138."
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (execute-kbd-macro (kbd "C-x 8 \" A"))
    (should (string= "Ä" (buffer-string)))
    (kill-buffer (current-buffer))))

;;; Global map tests.
(defun ergoemacs-test-global-key-set-before (&optional after key ergoemacs ignore-prev-global delete-def)
  "Test the global key set before ergoemacs-mode is loaded."
  :tags '(:slow)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (ret nil)
         (sk nil)
         (test-key (or key "M-k"))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (setq sk
          (format "(%s '(lambda() (interactive) (with-temp-file \"%s\" (insert \"Ok\"))))"
                  (cond
                   ((eq ergoemacs 'define-key)
                    (format "define-key global-map (kbd \"%s\") " test-key))
                   (t
                    (format "global-set-key (kbd \"%s\") " test-key)))
                  w-file))
    (with-temp-file temp-file
      (if (boundp 'wait-for-me)
          (insert "(setq debug-on-error t)")
        (insert "(condition-case err (progn "))
      (unless after
        (when delete-def
          (insert (format "(global-set-key (kbd \"%s\") nil)" delete-def)))
        (insert sk))
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (unless ignore-prev-global
        (insert "(setq ergoemacs-ignore-prev-global nil)"))
      (insert "(require 'ergoemacs-mode)(setq ergoemacs-mode--start-p t)(ergoemacs-mode 1)")
      (insert
       (format
        "(setq ergoemacs-test-macro (edmacro-parse-keys \"%s\" t))"
        test-key))
      (when after
        (when delete-def
          (insert (format "(global-set-key (kbd \"%s\") nil)" delete-def)))
        (insert sk))
      (insert "(execute-kbd-macro ergoemacs-test-macro)")
      (insert (format "(if (file-exists-p \"%s\") (message \"Passed\") (message \"Failed\"))" w-file))
      (unless (boundp 'wait-for-me)
        (insert ") (error (message \"Error %s\" err)))")
        (insert "(kill-emacs)")))
    (message
     "%s"
     (shell-command-to-string
      (format "%s %s -Q -l %s" emacs-exe (if (boundp 'wait-for-me) "" "--batch") temp-file)))
    (delete-file temp-file)
    (when (file-exists-p w-file)
      (setq ret 't)
      (delete-file w-file))
    ret))

(ert-deftest ergoemacs-test-global-key-set-after-220 ()
  "Test global C-c b"
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-c b") t)))

(defvar ergoemacs-component-hash)

(ert-deftest ergoemacs-test-global-key-set-apps-220-before ()
  "Test global C-c b"
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before nil "C-c b") t)))



(ert-deftest ergoemacs-test-issue-243 ()
  "Allow globally set keys like C-c C-c M-x to work globally while local commands like C-c C-c will work correctly. "
  :tags '(:slow)
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(condition-case err (progn ")
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (insert "(setq ergoemacs-command-loop-type nil)")
      (insert "(require 'ergoemacs-mode)(require 'ergoemacs-test)(setq ergoemacs-mode--start-p t)(ergoemacs-mode 1)")
      (insert "(global-set-key (kbd \"C-c C-c M-x\") 'execute-extended-command)")
      (insert (format "(define-key ergoemacs-test-major-mode-map (kbd \"C-c C-c\") #'(lambda() (interactive (with-temp-file \"%s\" (insert \"Ok\")))))" w-file))
      (insert
       "(setq ergoemacs-test-macro (edmacro-parse-keys \"C-c C-c\" t))(ergoemacs-test-major-mode)")
      (insert "(with-timeout (0.5 nil) (execute-kbd-macro ergoemacs-test-macro))")
      (insert (format "(if (file-exists-p \"%s\") (message \"Passed\") (message \"Failed\"))" w-file))
      (insert ") (error (message \"Error %s\" err)))")
      (unless (boundp 'wait-for-me)
        (insert "(kill-emacs)")))
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "" "--batch")
                      temp-file)))
    (delete-file temp-file)
    (should (file-exists-p w-file))
    (when (file-exists-p w-file)
      (delete-file w-file))))

;; Issue 437
;;
;; Can override an ergoemacs binding when loading the new mode.  For
;; example, this code changes M-left to M-right.
;;
;; (add-hook 'org-mode-hook
;;   (lambda ()
;;     (define-key org-mode-map (kbd "<M-left>") 'org-metaright)
;;     ))

;;; Not sure why this doesn't actually use `ergoemacs-test-major-mode-map'.
(define-derived-mode ergoemacs-test-major-mode fundamental-mode "ET"
  "Major mode for testing some issues with `ergoemacs-mode'.
\\{ergoemacs-test-major-mode-map}")

(define-key ergoemacs-test-major-mode-map (kbd "C-s") 'search-forward)
(define-key ergoemacs-test-major-mode-map (kbd "<f6>") 'search-forward)
(define-key ergoemacs-test-major-mode-map (kbd "M-s a") 'isearch-forward)
(define-key ergoemacs-test-major-mode-map (kbd "M-s b") 'isearch-backward)

(let ((ergoemacs-is-user-defined-map-change-p t))
  (add-hook 'ergoemacs-test-major-mode-hook
            '(lambda()
               (interactive)
               (define-key ergoemacs-test-major-mode-map (kbd "C-w") 'ergoemacs-close-current-buffer))))

(ert-deftest ergoemacs-test-issue-349 ()
  "Unbind <f6>"
  :tags '(:slow :interactive)
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(condition-case err (progn ")
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (insert "(setq ergoemacs-command-loop-type nil)")
      (insert "(require 'ergoemacs-mode)(require 'ergoemacs-test)(setq ergoemacs-mode--start-p t)(ergoemacs-mode 1)")
      (insert (format "(define-key ergoemacs-test-major-mode-map (kbd \"<f6>\") #'(lambda() (interactive (with-temp-file \"%s\" (insert \"Ok\")))))" w-file))
      (insert "(global-unset-key (kbd \"<f6>\"))")
      (insert
       "(setq ergoemacs-test-macro (edmacro-parse-keys \"<f6>\" t))(ergoemacs-test-major-mode)(run-hooks 'post-command-hook)")
      (insert "(with-timeout (0.5 nil) (execute-kbd-macro ergoemacs-test-macro))")
      (insert (format "(if (file-exists-p \"%s\") (message \"Passed\") (message \"Failed\"))" w-file))
      (insert ") (error (message \"Error %s\" err)))")
      (unless (boundp 'wait-for-me)
        (insert "(kill-emacs)")))
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "" "--batch")
                      temp-file)))
    (delete-file temp-file)
    (should (file-exists-p w-file))
    (when (file-exists-p w-file)
      (delete-file w-file))))

(ert-deftest ergoemacs-test-ignore-ctl-w ()
  "Keep user-defined C-w in major-mode `ergoemacs-test-major-mode'.
Part of addressing Issue #147."
  (let (ret
        (ergoemacs-use-function-remapping t))
    (with-temp-buffer
      (ergoemacs-test-major-mode)
      (when (not (current-local-map))
        (use-local-map ergoemacs-test-major-mode-map))
      (should (eq (key-binding (kbd "C-w")) 'ergoemacs-close-current-buffer))
      ;; The user-defined C-w should not affect kill-region remaps.
      (should (not (eq (key-binding [ergoemacs-remap kill-region]) 'ergoemacs-close-current-buffer))))))

(ert-deftest ergoemacs-test-dired-sort-files ()
  "Test Issue #340"
  (add-hook 'dired-mode-hook (lambda ()
                               (interactive)
                               (make-local-variable  'dired-sort-map)
                               (setq dired-sort-map (make-sparse-keymap))
                               (define-key dired-mode-map "s" dired-sort-map)
                               (define-key dired-sort-map "s"
                                 '(lambda () "sort by Size"
                                    (interactive) (dired-sort-other (concat dired-listing-switches "-AlS --si --time-style long-iso"))))
                               (define-key dired-sort-map "."
                                 '(lambda () "sort by eXtension"
                                    (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
                               (define-key dired-sort-map "t"
                                 '(lambda () "sort by Time"
                                    (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
                               (define-key dired-sort-map "n"
                                 '(lambda () "sort by Name"
                                    (interactive) (dired-sort-other (concat dired-listing-switches ""))))
                               ;; Use "|", not "r".
                               (define-key dired-mode-map "|" 'dired-sort-menu-toggle-reverse)
                               ))
  (dired ergoemacs-dir)
  (should (equal (key-binding (kbd "s s")) '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "-AlS --si --time-style long-iso")))))
  (should (equal (key-binding (kbd "s .")) '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X")))))
  (should (equal (key-binding (kbd "s t")) '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t")))))
  (should (equal (key-binding (kbd "s n")) '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches "")))))
  (should (equal (key-binding (kbd "|")) 'dired-sort-menu-toggle-reverse))
  (kill-buffer (current-buffer))
  (remove-hook 'dired-mode-hook (lambda ()
    (interactive)
    (make-local-variable  'dired-sort-map)
    (setq dired-sort-map (make-sparse-keymap))
    (define-key dired-mode-map "s" dired-sort-map)
    (define-key dired-sort-map "s"
      '(lambda () "sort by Size"
         (interactive) (dired-sort-other (concat dired-listing-switches "-AlS --si --time-style long-iso"))))
    (define-key dired-sort-map "."
      '(lambda () "sort by eXtension"
         (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
    (define-key dired-sort-map "t"
      '(lambda () "sort by Time"
         (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
    (define-key dired-sort-map "n"
      '(lambda () "sort by Name"
         (interactive) (dired-sort-other (concat dired-listing-switches ""))))
    ;; Use "|", not "r".
    (define-key dired-mode-map "|" 'dired-sort-menu-toggle-reverse)
    )))


(ert-deftest ergoemacs-test-quail-translations ()
  "Test if quail to ergoemacs-mode translations work."
  :tags '(:translate)
  (should (equal ergoemacs-layout-us (ergoemacs-translate--quail-to-ergoemacs (ergoemacs-translate-layout 'us :quail)))))

(ert-deftest ergoemacs-test-translations ()
  "Test ergoemacs-mode translations"
  :tags '(:translate)
  (should (string= "A" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0))))))
  (should (string= "M-A" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0) :unchorded-alt)))))
  (should (string= "C-S-a" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "A" t) 0) :unchorded-ctl)))))
  (should (string= "M-A" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-S-a" t) 0) :ctl-to-alt)))))

  ;; DEL = ^?, doesn't seem to have the issues that RET, ESC, and TAB has.
  (should (string= "DEL" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "DEL" t) 0) :ctl-to-alt)))))
  
  (should (string= "C-DEL" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-DEL" t) 0) :ctl-to-alt)))))
  
  (should (string= "M-DEL" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-DEL" t) 0) :ctl-to-alt)))))

  ;; RET = ^M
  (should (string= "RET" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "RET" t) 0) :ctl-to-alt)))))

  (should (string= "M-RET" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-RET" t) 0) :ctl-to-alt)))))

  (should (string= "C-RET" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-RET" t) 0) :ctl-to-alt)))))

  ;; ESC = ^[
  (should (string= "ESC" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "ESC" t) 0) :ctl-to-alt)))))

  (should (string= "C-ESC" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-ESC" t) 0) :ctl-to-alt)))))

  (should (string= "M-ESC" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-ESC" t) 0) :ctl-to-alt)))))

  ;; TAB = ^i
  (should (string= "TAB" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "TAB" t) 0) :ctl-to-alt)))))

  (should (string= "C-TAB" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-TAB" t) 0) :ctl-to-alt)))))
  
  (should (string= (key-description (kbd "M-TAB")) (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "C-TAB" t) 0) :ctl-to-alt)))))

  (cl-letf (((symbol-function 'display-graphic-p) (lambda(&rest _ignore) t)))
    ;; Test M-i -> ^i -> TAB
    (should (string= "<C-i>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-i" t) 0) :ctl-to-alt)))))
    
    ;; Test M-[ -> ^[ -> ESC
    (should (string= "<C-[>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-[" t) 0) :ctl-to-alt)))))
    
    ;; Test M-m -> ^m -> RET
    (should (string= "<C-m>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-m" t) 0) :ctl-to-alt))))))

  (cl-letf (((symbol-function 'display-graphic-p) (lambda(&rest _ignore) nil)))
    ;; Test M-i -> ^i -> TAB
    (should (string= "TAB" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-i" t) 0) :ctl-to-alt)))))
    
    ;; Test M-[ -> ^[ -> ESC
    (should (string= "ESC" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-[" t) 0) :ctl-to-alt)))))
    
    ;; Test M-m -> ^m -> RET
    (should (string= "RET" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-m" t) 0) :ctl-to-alt)))))))

(ert-deftest ergoemacs-test-input-methods ()
  "Make sure that `ergoemacs-mode' works with input methods."
  :tags '(:translate)
  (ergoemacs-test-layout
   :layout "colemak"
   :macro "arst"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (set-input-method "greek")
     (message "%s" current-input-method)
     (ergoemacs-command-loop--internal "arst")
     (should (string= "αρστ" (buffer-string)))
     (quail-set-keyboard-layout "colemak")
     (delete-region (point-min) (point-max))
     (ergoemacs-command-loop--internal "arst")
     (quail-set-keyboard-layout "standard")
     (should (string= "ασδφ" (buffer-string)))
     (set-input-method nil)
     (kill-buffer (current-buffer)))))

(ert-deftest ergoemacs-test-table-insert ()
  "Tests that table can insert without hanging emacs."
  :tags '(:table)
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (table-insert 1 2)
    (execute-kbd-macro (kbd "abc <tab> abc <tab>"))
    (should (string= (buffer-string) "+-----+
|abc  |
+-----+
|abc  |
+-----+
"))
    (kill-buffer (current-buffer))))

;; File variables
(ert-deftest ergoemacs-test-mouse-command-list-changes ()
  "Part of test for Sub issue described in #351"
  (should (equal '(&rest arg) (ergoemacs-command-loop--mouse-command-drop-first '(&rest arg) t)))
  (should (equal '(arg) (ergoemacs-command-loop--mouse-command-drop-first '(&rest arg))))
  (should (equal 'arg (ergoemacs-command-loop--mouse-command-drop-first '(&rest arg) :rest)))

  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&rest arg) :drop-rest)))
  
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg) t)))
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg))))
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg) :rest)))
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg) :drop-rest)))

  (should (equal '(&optional arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2) t)))
  (should (equal '(arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2))))
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2) :rest)))
  (should (equal '(arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2) :drop-rest)))

  (should (equal '(&rest arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 &rest arg2) t)))
  (should (equal '(arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 &rest arg2))))
  (should (equal 'arg2 (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 &rest arg2) :rest)))
  (should (equal nil (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 &rest arg2) :drop-rest)))

  (should (equal '(&optional arg2 &rest arg3) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2 &rest arg3) t)))
  (should (equal '(arg2 arg3) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2 &rest arg3))))
  (should (equal 'arg3 (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2 &rest arg3) :rest)))
  (should (equal '(arg2) (ergoemacs-command-loop--mouse-command-drop-first '(&optional arg1 arg2 &rest arg3) :drop-rest))))

;;; minibuffer tests...
;;; Related to: http://emacs.stackexchange.com/questions/10393/how-can-i-answer-a-minibuffer-prompt-from-elisp

(defmacro ergoemacs-minibuffer-key-bindings (minibuffer-call &rest keys)
  "Setup minibuffer with MINIBUFFER-CALL, and lookep KEYS."
  `(catch 'found-key
       (minibuffer-with-setup-hook
	   (lambda ()
	     (run-with-timer
	      0.05 nil
	      (lambda()
		(throw 'found-key (mapcar (lambda(key) (if (consp key)
                                                           (key-binding (eval key t))
                                                         (key-binding key)))
					  ',keys)))))
	 ,minibuffer-call)
       nil))

(provide 'ergoemacs-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
