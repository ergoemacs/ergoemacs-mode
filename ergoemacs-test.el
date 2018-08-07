;;; ergoemacs-test.el --- tests for ErgoEmacs issues

;; Copyright © 2013-2015 Free Software Foundation, Inc.

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

(declare-function ergoemacs-translate--keymap "ergoemacs-translate")
(declare-function ergoemacs-mode-reset "ergoemacs-mode")

(defvar ergoemacs-translate--parent-map)
(defvar ergoemacs-map--)
(defvar ergoemacs-layout-us)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme)
(defvar ergoemacs-command-loop-type)
(defvar ergoemacs-dir)
(defvar ergoemacs-mode)
(defvar dired-sort-map)
(defvar dired-mode-map)
(defvar cl-struct-ergoemacs-component-struct-tags)

(declare-function ergoemacs-key-description "ergoemacs-key-description")


(declare-function ergoemacs-translate--meta-to-escape "ergoemacs-translate")
(declare-function ergoemacs-map-keymap "ergoemacs-mapkeymap")

(declare-function ergoemacs-mode "ergoemacs-mode")

(declare-function ergoemacs-command-loop "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--internal "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--mouse-command-drop-first "ergoemacs-command-loop")
(declare-function ergoemacs-command-loop--read-key-sequence "ergoemacs-command-loop")

(declare-function ergoemacs-component-struct--lookup-hash "ergoemacs-compononent")

(declare-function ergoemacs-copy-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-cut-line-or-region "ergoemacs-functions")
(declare-function ergoemacs-emacs-exe "ergoemacs-functions")
(declare-function ergoemacs-eshell-here "ergoemacs-functions")
(declare-function ergoemacs-paste "ergoemacs-functions")

(declare-function ergoemacs-map-- "ergoemacs-map")
(declare-function ergoemacs-map--modify-active "ergoemacs-map")

(declare-function ergoemacs-require "ergoemacs-lib")

(declare-function ergoemacs-theme--get-version "ergoemacs-theme")
(declare-function ergoemacs-theme-set-version "ergoemacs-theme")

(declare-function ergoemacs-translate--event-mods "ergoemacs-translate")
(declare-function ergoemacs-translate--quail-to-ergoemacs "ergoemacs-translate")
(declare-function ergoemacs-translate-layout "ergoemacs-translate")
(declare-function ergoemacs-translate--get "ergoemacs-translate")
(declare-function ergoemacs-unchorded-alt-modal "ergoemacs-translate")
(declare-function ergoemacs-translate--event-modifiers "ergoemacs-translate")

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

(defun ergoemacs-test-require-input ()
  "Run tests that require input."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :require-input)))
  (call-interactively 'elp-results))

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

(defun ergoemacs-test-copy ()
  "Copy/Paste test for ergoemacs-mode."
  (interactive)
  (elp-instrument-package "ergoemacs-")
  (ert '(and "ergoemacs-" (tag :copy)))
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
    (ert '(and "^ergoemacs-test-" (not (tag :require-input))))
    (call-interactively 'elp-results)))

;; Test isearch

(ert-deftest ergoemacs-test-isearch-C-f-backspace ()
  "Test Backspace in `isearch-mode'"
  :tags '(:search :interactive)
  ;; Google Code Issue #145
  (ergoemacs-test-layout
   :layout "colemak"
   :macro "C-f a r s C-f <backspace> M-n"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (insert "aars1\nars2\nars3\nars4")
     (goto-char (point-min))
     (execute-kbd-macro macro)
     (when (looking-at ".*")
       (should (string= "s1" (match-string 0))))
     (kill-buffer (current-buffer)))))

(ert-deftest ergoemacs-test-isearch-C-f ()
  "C-f doesn't work in isearch-mode."
  :tags '(:search :interactive)
  ;; Google Code Issue #119
  (ergoemacs-test-layout
   :layout "colemak"
   :cua t
   :macro "C-f ars C-f C-f"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (insert "aars1\nars2\nars3\nars4")
     (goto-char (point-min))
     (execute-kbd-macro macro)
     (when (looking-at ".*")
       (should (string= "3" (match-string 0))))
     (kill-buffer (current-buffer)))))

(ert-deftest ergoemacs-test-isearch-exit-C-s ()
  "Make sure C-s works outside of `ergoemacs-mode'. Issue #361."
  :tags '(:search)
  (ergoemacs-mode -1)
  (should (eq (lookup-key isearch-mode-map (kbd "C-s")) 'isearch-repeat-forward))
  (should (eq (lookup-key isearch-mode-map (kbd "C-r")) 'isearch-repeat-backward))
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (insert "aars1\nars2\nars3\nars4")
    (goto-char (point-min))
    (execute-kbd-macro (read-kbd-macro "C-s ars C-s C-s"))
    (when (looking-at ".*")
      (should (string= "3" (match-string 0))))
    (kill-buffer (current-buffer)))
  (ergoemacs-mode 1))

(ert-deftest ergoemacs-test-isearch-in-eshell ()
  "Test Issue #322."
  :tags '(:search :calc)
  (ergoemacs-test-layout
   :layout "us"
   (ergoemacs-eshell-here)
   (should (eq 'isearch-forward (key-binding (kbd "C-f"))))
   (should (eq 'isearch-forward (key-binding (kbd "M-y"))))
   (kill-buffer (current-buffer))))

(ert-deftest ergoemacs-test-isearch-works-with-region ()
  "With vanilla Emacs, when mark is active and even some region is
already selected, isearch-ing would expand or shrink selection.
Currently ergoemacs-mode discards selection as soon as isearch key is
pressed. Reproducible with ergoemacs-clean.
Issue #186."
  :tags '(:search)
  (let ((ret t))
    (ergoemacs-test-layout
     :macro "C-f lab"
     :layout "colemak"
     :cua t
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (mark-word)
       (execute-kbd-macro macro)
       (setq ret mark-active)
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-isearch-exits-with-ergoemacs-movement-keys ()
  "Tests if isearch exits the search with movement keys.
Tests issue #347"
  :tags '(:search)
  (ergoemacs-test-layout
   :macro "C-f ars M-e"
   :layout "colemak"
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (insert "aars1\nars2\nars3\nars4")
    (goto-char (point-min))
    (execute-kbd-macro macro)
    (should (not isearch-mode))
    (when isearch-mode
      (isearch-mode -1))
    (kill-buffer (current-buffer)))))

;; Not sure why; Fixed but test dosesn't work
;; (ert-deftest ergoemacs-test-isearch-isearch-backward-repeats-backward ()
;;   "Tests backward isearch remapping is correct."
;;   :tags '(:search)
;;   (ergoemacs-test-layout
;;    :macro "M-j ars M-j M-i"
;;    :layout "colemak"
;;    :theme "reduction"
;;    (save-excursion
;;      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
;;      (delete-region (point-min) (point-max))
;;      (insert "aars1\nars2\nars3\nars4")
;;      (goto-char (point-max))
;;      (execute-kbd-macro macro)
;;      (should (looking-at "rs3")))))

;;; Shift Selection
(ert-deftest ergoemacs-test-shift-select-move-no-mark ()
  "Tests another shifted selection"
  :tags '(:shift-select)
  (let ((ret t))
    (ergoemacs-test-layout
     :macro "M-H"
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (goto-char (point-min))
       (insert ";;")
       (execute-kbd-macro macro)
       (setq ret (not mark-active)) ;;  Shouldn't be selected
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shift-select-cua-move-keep-mark ()
  "Test the shifted selection bug."
  :tags '(:shift-select)
  (let (ret)
    (ergoemacs-test-layout
     :macro "M-SPC M-h M-I"
     :layout "colemak"
     :cua t
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test-shifted-move*"))
       (delete-region (point-min) (point-max))
       (insert ";;;;")
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (setq ret mark-active) ;; Should be selected.
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shift-select-reduction ()
  "Test that shift selection works properly in reduction."
  :tags '(:shift-select :calc :interactive)
  (ergoemacs-test-layout
   :theme "reduction"
   :layout "colemak"
   :macro "M-E M-E M-x"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (insert ergoemacs-test-lorem-ipsum)
     (goto-char (point-min))
     (execute-kbd-macro macro)
     (should (= (point) (point-min)))
     (kill-buffer (current-buffer)))))


(ert-deftest ergoemacs-test-shift-select-subword ()
  "Test for mark working with shift-selection of `subword-forward'."
  :tags '(:shift-select)
  (let (ret)
    (ergoemacs-test-layout
     :macro "M-Y M-x"
     :theme "reduction"
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (subword-mode 1)
       (goto-char (point-max))
       (beginning-of-line)
       (execute-kbd-macro macro)
       (when (looking-at " in culpa qui")
         (setq ret t))
       (kill-buffer (current-buffer))))))

;;; Copy/Paste


(ert-deftest ergoemacs-test-copy-paste-issue-184 ()
  "Issue #184; Not replace the \"selected all\" by paste."
  :tags '(:copy :interactive)
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (ergoemacs-test-layout
     :macro "C-v"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (push-mark)
       (end-of-line)
       (ergoemacs-copy-line-or-region)
       (push-mark (point))
       (push-mark (point-max) nil t)
       (goto-char (point-min))
       ;; Make sure the `pre-command-hook' and `post-command-hook' is
       ;; run by calling the macro.
       (execute-kbd-macro macro) 
       ;; (ergoemacs-paste)
       (should (string= "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed\n"
                        (buffer-string)))
       (kill-buffer (current-buffer))))))

(ert-deftest ergoemacs-test-copy-paste-issue-184-paste-should-clear-mark ()
  "Issue #186.
Selected mark would not be cleared after paste."
  :tags '(:copy)
  (ergoemacs-test-layout
   (let ((ergoemacs-handle-ctl-c-or-ctl-x 'both))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (push-mark)
       (end-of-line)
       (ergoemacs-copy-line-or-region)
       (push-mark (point))
       (push-mark (point-max) nil t)
       (goto-char (point-min))
       (ergoemacs-paste)
       (should (or deactivate-mark (not mark-active)))
       (kill-buffer (current-buffer))))))


(ert-deftest ergoemacs-test-copy-paste-cut-line-or-region ()
  "Issue #68.
kill-ring function name is used and such doesn't exist. It errs when
not using cua or cutting line. I think kill-region is what is meant."
  (ergoemacs-test-layout
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
     (should ret))))


(ert-deftest ergoemacs-test-copy-paste-issue-130-cut ()
  "Attempts to test Issue #130 -- Cut"
  :tags '(:copy :interactive)
  (ergoemacs-test-layout
   (let ((ret t)
         (ergoemacs-ctl-c-or-ctl-x-delay 0.1)
         (ergoemacs-handle-ctl-c-or-ctl-x 'both))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (push-mark (point))
       (push-mark (point-max) nil t)
       (goto-char (point-min))
       (ergoemacs-command-loop--internal "C-x <ergoemacs-timeout>")
       (setq ret (string= "" (buffer-string)))
       (kill-buffer (current-buffer)))
     (should ret))))

(ert-deftest ergoemacs-test-copy-paste-issue-130-copy ()
  "Attempts to test Issue #130 -- Copy"
  :tags '(:copy :interactive)
  (ergoemacs-test-layout
   (let ((ergoemacs-ctl-c-or-ctl-x-delay 0.1)
         (ergoemacs-handle-ctl-c-or-ctl-x 'both)
         (txt "Text\n123"))
     (with-temp-buffer
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert txt)
       (push-mark (point))
       (push-mark (point-max) nil t)
       ;; (message "Region Active: %s" transient-mark-mode)
       (setq last-command nil
             this-command nil)
       (goto-char (point-min))
       (ergoemacs-command-loop--internal "C-c <ergoemacs-timeout>")
       (goto-char (point-max))
       (ergoemacs-paste)
       (should (string= (concat txt txt)
                        (buffer-string)))
       (kill-buffer (current-buffer))))))

(ert-deftest ergoemacs-test-copy-paste-apps-cut ()
  "Tests <apps> x on QWERTY cutting a region, not just a line."
  :tags '(:copy :interactive)
  (let (ret)
    (ergoemacs-test-layout
     :macro (format "<%s> x"
                    (if (eq system-type 'windows-nt)
                        "apps" "menu"))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (push-mark (point))
       (push-mark (point-max) nil t)
       (goto-char (point-min))
       (ergoemacs-command-loop--internal macro)
       (setq ret (string= "" (buffer-string)))
       (kill-buffer (current-buffer))))
    (should ret)))

;; [1 apps 99 22]

(ert-deftest ergoemacs-test-copy-paste-apps-copy ()
  "Tests <apps> c on QWERTY copying a region, not just a line."
  :tags '(:copy :interactive)
  ;; :tags '(:interactive)
  (ergoemacs-test-layout
   :macro (format "C-a <%s> c C-v"
                  (if (eq system-type 'windows-nt)
                      "apps" "menu"))
   (let ((test-string "1\n2\n3\n4"))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert test-string)
       (execute-kbd-macro macro)
       (should (string= (concat test-string test-string)
                        (buffer-string)))
       (kill-buffer (current-buffer))))))

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
  (should (eq 'ergoemacs-map-undefined (key-binding (read-kbd-macro "C-x C-s")))))

(ert-deftest ergoemacs-test-function-M-e-only-one-char-issue-306 ()
  "Tests Issue #306."
  :tags '(:calc)
  (let ((ergoemacs-test-fn t)
        (ergoemacs-read-input-keys nil))
    (ergoemacs-test-layout
     :layout "us"
     :theme "lvl2"
     :macro "M-e"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (fundamental-mode)
       (should (or (eq (key-binding (kbd "M-e")) 'backward-kill-word)
                   (eq (key-binding (kbd "M-e")) (command-remapping 'backward-kill-word (point)))))
       (setq ergoemacs-test-fn nil)
       (goto-char (point-max))
       (execute-kbd-macro macro)
       (should (string= "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut
enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est " (buffer-string)))
       (kill-buffer (current-buffer))))))


(ert-deftest ergoemacs-test-function-issue-305-variables-set-to-nil ()
  "Test Issue #305.
When calling `ergoemacs-refresh' variable values should be preserved."
  (ergoemacs-mode-reset)
  (should (eq t shift-select-mode)))

;;; Grep


(ert-deftest ergoemacs-test-grep-issue-293 ()
  "Test Issue #293.
Unable to use M-ijkl in a grep buffer."
  (ergoemacs-test-layout
   :layout "colemak"
   :macro "M-e M-e M-e M-i"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (insert "-*- mode: grep; default-directory: \"~src/ergoemacs-mode/\" -*-
Grep started at Fri Aug 22 08:30:37
grep -nH -e ergoemacs-mode ergoemacs-mode.el
ergoemacs-mode.el:1:;;; ergoemacs-mode.el --- Emacs mode based on common modern interface and ergonomics. -*- lexical-binding: t -*-
ergoemacs-mode.el:949:;;; ergoemacs-mode.el ends here
Grep finished (matches found) at Fri Aug 22 08:30:37
")
     (grep-mode)
     ;; (ergoemacs-map--modify-active)
     (goto-char (point-min))
     (execute-kbd-macro macro)
     (should (string= (buffer-substring (point) (+ 16 (point)))
                      "rgoemacs-mode.el"))
     (kill-buffer (current-buffer)))))

;;; Org-mode

(ert-deftest ergoemacs-test-org-C-a ()
  "Test beginning of line in standard ergoemacs-mode/org-mode."
  (ergoemacs-test-layout
   :layout "colemak"
   :macro "M-m"
   (let (ret)
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert "abc\n* TODO Fix org C-a issue")
       (org-mode)
       (goto-char (point-max))
       (execute-kbd-macro macro)
       (ignore-errors
         (should (string= (buffer-substring (point) (point-at-eol))
                          "Fix org C-a issue")))
       (kill-buffer (current-buffer))))))

(ert-deftest ergoemacs-test-org-respect-keys-issue-304 ()
  "Tests Issue #304.
`org-mode' should respect the keys used."
  (let ((ergoemacs-test-fn t))
    (ergoemacs-test-layout
     :layout "us"
     :theme "standard"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (org-mode)
       (ergoemacs-map--modify-active)
       (should (eq (key-binding (kbd "<M-right>")) 'ergoemacs-org-metaright))
       (should (eq (key-binding (kbd "<M-left>")) 'ergoemacs-org-metaleft))
       (should (eq (key-binding (kbd "<M-up>")) 'ergoemacs-org-metaup))
       (should (eq (key-binding (kbd "<M-down>")) 'ergoemacs-org-metadown))
       (kill-buffer (current-buffer))))))


;;; Calc

(ert-deftest ergoemacs-test-calc-300 ()
  "Test Calc undo"
  :tags '(:calc :interactive)
  (let ((ergoemacs-test-fn t))
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (call-interactively 'calc)
     (unwind-protect
         (should (eq (key-binding (kbd "C-z")) (or (command-remapping 'calc-undo (point)) 'calc-undo)))
       (call-interactively 'calc-quit)))))

(ert-deftest ergoemacs-test-calc-fries-ergoemacs-mode ()
  "After calc has entered some numbers, it fries ergoemacs-mode."
  :tags '(:calc :interactive)
  (let ((ergoemacs-test-fn t))
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (call-interactively 'calc)
     (execute-kbd-macro "1 1 +")
     (call-interactively 'calc-quit)
     (should (eq (key-binding (kbd "M-u")) 'previous-line)))))

;;; Modal

(ert-deftest ergoemacs-test-modal-preserve-mark ()
  "Issue #101.
Test next and prior translation."
  (with-temp-buffer
    (insert ergoemacs-test-lorem-ipsum)
    (goto-char (point-min))
    (ergoemacs-translate--get :unchorded-alt) ;; Load translation
    (ergoemacs-unchorded-alt-modal)
    (set-mark (point))
    (forward-char 3)
    (ergoemacs-unchorded-alt-modal)
    (should mark-active)))

(ert-deftest ergoemacs-test-modal-alt-mode-horizontal-position ()
  "Tests Issue #213"
  (let (ret)
    (ergoemacs-test-layout
     :layout "colemak"
     :macro "i u u"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-max))
       (beginning-of-line)
       (ergoemacs-translate--get :unchorded-alt)
       (ergoemacs-unchorded-alt-modal)
       (execute-kbd-macro macro)
       (looking-at ".*? ")
       (ignore-errors (should (string= (match-string 0) "eprehenderit ")))
       (ergoemacs-unchorded-alt-modal)
       (kill-buffer (current-buffer))))))



;;; Command Loop

(ert-deftest ergoemacs-test-command-loop-apps-e-t-_ ()
  "Test that colemak <apps> e t sends _.
Should test for Issue #143."
  (ergoemacs-test-layout
   :theme "reduction"
   :layout "colemak"
   (save-excursion
     (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
     (delete-region (point-min) (point-max))
     (with-timeout (0.5 nil)
       (ergoemacs-command-loop--internal (format "<%s> e t"
                                       (if (eq system-type 'windows-nt)
                                           "apps" "menu"))))
     (should (string= "_" (buffer-string)))
     (kill-buffer (current-buffer)))))


(ert-deftest ergoemacs-test-command-loop-C-x-8-! ()
  "Test that unicode translations work.
See Issue #138."
  (save-excursion
    (unless ergoemacs-mode
      (ergoemacs-mode))
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (ergoemacs-command-loop--internal "C-x 8 !")
    (should (string= "¡" (buffer-string)))
    (kill-buffer (current-buffer))))

(ert-deftest ergoemacs-test-command-loop-C-x-8-A ()
  "Test that unicode translations work.
See Issue #138."
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (unless ergoemacs-mode
      (ergoemacs-mode))
    (delete-region (point-min) (point-max))
    (ergoemacs-command-loop--internal "C-x 8 \" A")
    (should (string= "Ä" (buffer-string)))
    (kill-buffer (current-buffer))))

(ert-deftest ergoemacs-test-command-loop-overlay-paren ()
  "Test that overlays will send the appropriate parenthesis"
  (let (ret
        tmp (tmp-key (make-sparse-keymap)) overlays)
    (ergoemacs-test-layout
     :layout "colemak"
     :macro (format "M-i <%s> e e"
                    (if (eq system-type 'windows-nt)
                        "apps" "menu"))
     (define-key tmp-key [ergoemacs-test] 'ignore)
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       ;; Put in dummy overlay
       (while (re-search-forward "[A-Za-z]+" nil t)
         (setq tmp (make-overlay (match-beginning 0) (match-end 0)))
         (overlay-put tmp 'keymap tmp-key)
         (push tmp overlays))
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (when (looking-at ")")
         (setq ret t))
       (dolist (x overlays)
         (delete-overlay x))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-command-loop-shortcut ()
  "Test that shortcuts don't eat or duplicate key-strokes. (Issue #141)"
  :tags '(:interactive)
  (let (ret)
    (ergoemacs-test-layout
     :macro (format "<%s> e e M-u"
                    (if (eq system-type 'windows-nt)
                        "apps" "menu"))
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-max))
       (beginning-of-line)
       (execute-kbd-macro macro)
       (looking-at ".*")
       (when (looking-at "ulla pariatur.")
         (setq ret t))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-command-loop-overlay ()
  "Test for mark working with overlays.
Should test issue #142"
  :tags '(:interactive)
  (let (ret
        tmp (tmp-key (make-sparse-keymap))
        overlays)
    (ergoemacs-test-layout
     :macro "M-SPC M-y M-x"
     :layout "colemak"
     (define-key tmp-key [ergoemacs-test] 'ignore)
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       ;; Put in dummy overlay
       (while (re-search-forward "[A-Za-z]+" nil t)
         (setq tmp (make-overlay (match-beginning 0) (match-end 0)))
         (overlay-put tmp 'keymap tmp-key)
         (push tmp overlays))
       (goto-char (point-max))
       (beginning-of-line)
       (execute-kbd-macro macro)
       (when (looking-at " in culpa qui")
         (setq ret t))
       (dolist (x overlays)
         (delete-overlay x))
       (kill-buffer (current-buffer)))
     (should (equal ret t)))))

(ert-deftest ergoemacs-test-command-loop-reduction-M-o-works ()
  "Test Ergoemacs M-o works correctly (Issue #171)."
  ;; (let ((ergoemacs-test-fn t))
  ;;   (ergoemacs-test-layout
  ;;    :theme "reduction"
  ;;    :layout "colemak"
  ;;    (with-timeout (0.2 nil) (ergoemacs-read-key "M-o"))
  ;;    (message "Test FN: %s" ergoemacs-test-fn)
  ;;    (should (eq ergoemacs-test-fn (or (command-remapping 'execute-extended-command (point)) 'execute-extended-command)))))
  )

(ert-deftest ergoemacs-test-terminal-M-O-fight ()
  "Tests Issue #188"
  (let ((old-map (copy-keymap input-decode-map))
        (ret nil))
    (ergoemacs-test-layout
     (setq input-decode-map (make-sparse-keymap))
     ;; Setup input decode map just like `xterm' for some common keys.
     (define-key input-decode-map "\eOA" [up])
     (define-key input-decode-map "\eOB" [down])
     (define-key input-decode-map "\eOC" [right])
     (define-key input-decode-map "\eOD" [left])
     (define-key input-decode-map "\eOF" [end])
     (define-key input-decode-map "\eOH" [home])
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-max))
       (beginning-of-line)
       (with-timeout (0.2 nil)
         (ergoemacs-command-loop--internal "M-O A")) ; by looking at `ergoemacs-read-key' this seems to be translating correctly, but... it doesn't run in this context.
       (message "Decode: %s" (lookup-key input-decode-map (kbd "M-O A")))
       (setq ret (looking-at "nulla pariatur. Excepteur sint occaecat cupidatat non proident,"))
       (kill-buffer (current-buffer)))
     (setq input-decode-map (copy-keymap old-map)))
    (should ret)))
;;; Key inheritance 

(ert-deftest ergoemacs-key-inheitance-alt-up-and-down ()
  "Test M-up and M-down keys make sure they are moving lines"
  (ergoemacs-require 'move-and-transpose-lines)
  (ergoemacs-mode-reset)
  (should (eq (key-binding [\M-up]) 'ergoemacs-move-text-up))
  (should (eq (key-binding [\M-down]) 'ergoemacs-move-text-down)))

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
      (insert "(setq ergoemacs-theme nil)")
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

(ert-deftest ergoemacs-test-global-key-set-before-1 ()
  "Test global set key before ergoemacs-mode loads."
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before) t)))

(ert-deftest ergoemacs-test-global-key-set-before-2 ()
  "Test global set key before ergoemacs-mode loads (define-key)."
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before nil nil 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-after ()
  "Test global set key after ergoemacs loads."
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after) t)))

(ert-deftest ergoemacs-test-global-key-set-after-2 ()
  "Test global set key after ergoemacs loads (define-key)."
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after nil 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-before ()
  "Test setting <apps> m c before loading."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-before-2 ()
  "Test setting <apps> m c before loading (define-key)."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-m-semi-before ()
  "Test setting M-; before loading."
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before nil "M-;") t)))

(ert-deftest ergoemacs-test-global-key-set-m-semi-after ()
  "Test setting M-; before loading."
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before t "M-;") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-before ()
  "Test setting <apps> before loading."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>")) t)))


(ert-deftest ergoemacs-test-global-key-set-apps-before-2 ()
  "Test setting <apps> before loading (define-key)."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>") 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-before ()
  "Test setting <apps> m before loading."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-before-2 ()
  "Test setting <apps> m before loading (define-key)."
  :tags '(:slow :interactive)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-after ()
  "Test setting <apps> m after loading"
  :tags '(:slow)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") nil nil "<menu>") t)))


(ert-deftest ergoemacs-test-global-key-set-apps-m-after-2 ()
  "Test setting <apps> m after loading (define-key)"
  :tags '(:slow)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after ()
  "Test setting <apps> m c after loading."
  :tags '(:slow)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after-2 ()
  "Test setting <apps> m c after loading (define-key)."
  :tags '(:slow)
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") 'define-key nil "<menu>") t)))


(ert-deftest ergoemacs-test-global-key-set-after-220 ()
  "Test global C-c b"
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-c b") t)))


(ert-deftest ergoemacs-test-global-key-set-after-397 ()
  "Test global C-SPC"
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-SPC") t)))

(ert-deftest ergoemacs-test-397-test-3 ()
  "Test M-s is switch pane."
  :tags '(:slow)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(eval-when-compile (load (expand-file-name \"ergoemacs-macros\")) (require 'cl))"
              (or (and (boundp 'wait-for-me)
                       "(setq debug-on-error t debug-on-quit t)") "")
	      "(setq ergoemacs-theme nil)"
	      "(setq ergoemacs-keyboard-layout \"us\")"
              "(ergoemacs-mode 1)\n"
	      "(global-set-key (kbd \"C-SPC\") 'set-mark-command)\n"
              "(when (eq (key-binding (kbd \"M-s\")) 'ergoemacs-move-cursor-next-pane)\n"
              "(with-temp-file \"" w-file "\")\n"
              "   (message \"Passed\")"
              "  (insert \"Found\"))\n"
              (or (and (boundp 'wait-for-me) "")
                  "(kill-emacs)")))
    (byte-compile-file temp-file)
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -L %s -l %s -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "-debug-init" "--batch")
                      (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))
                      (expand-file-name (file-name-sans-extension (locate-library "ergoemacs-mode")))
                      temp-file)))
    (should (file-exists-p w-file))
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when  (file-exists-p (concat temp-file "c"))
      (delete-file (concat temp-file "c")))
    (when (file-exists-p w-file)
      (delete-file w-file))))


(ergoemacs-package icicles
    :ensure t)

(ert-deftest ergoemacs-test-397-test-4 ()
  "Test M-s is switch pane."
  :tags '(:slow :icicles :interactive)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(add-to-list 'load-path \"" (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))) "\")"
	      "(add-to-list 'load-path \"" (expand-file-name (file-name-directory (locate-library "icicles"))) "\")"
       "(eval-when-compile (require 'ergoemacs-macros) (require 'cl))"
              (or (and (boundp 'wait-for-me)
                       "(setq debug-on-error t debug-on-quit t)") "")
	      "(setq ergoemacs-theme nil)"
	      "(setq ergoemacs-keyboard-layout \"us\")"
	      "(require 'icicles)\n"
	      "(require 'ergoemacs-mode)\n"
              "(ergoemacs-mode 1)\n"
	      "(setq icicle-search-key-prefix (kbd \"C-f\"))\n"
	      "(icy-mode 1)\n"
              "(when (eq (key-binding (kbd \"M-s\")) 'ergoemacs-move-cursor-next-pane)\n"
              "(with-temp-file \"" w-file "\")\n"
              "   (message \"Passed\")"
              "  (insert \"Found\"))\n"
              (or (and (boundp 'wait-for-me) "")
                  "(kill-emacs)")))
    (byte-compile-file temp-file)
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "-debug-init" "--batch")                      
                      temp-file)))
    (should (file-exists-p w-file))
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when  (file-exists-p (concat temp-file "c"))
      (delete-file (concat temp-file "c")))
    (when (file-exists-p w-file)
      (delete-file w-file))))

(ert-deftest ergoemacs-test-397-test-2 ()
  "Test that defining C-SPC after ergoemacs-mode loads will give `set-mark-command'."
  :tags '(:slow)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(eval-when-compile (require 'ergoemacs-macros) (require 'cl))"
              (or (and (boundp 'wait-for-me)
                       "(setq debug-on-error t debug-on-quit t)") "")
	      "(setq ergoemacs-theme nil)"
	      "(setq ergoemacs-keyboard-layout \"us\")"
              "(ergoemacs-mode 1)\n"
	      "(global-set-key (kbd \"C-SPC\") 'set-mark-command)\n"
              "(when (eq (key-binding (kbd \"C-SPC\")) 'set-mark-command)\n"
              ;; "           (eq (key-binding (kbd \"M-s\")) 'ergoemacs-move-cursor-next-pane))\n"
              "(with-temp-file \"" w-file "\")\n"
              "   (message \"Passed\")"
              "  (insert \"Found\"))\n"
              (or (and (boundp 'wait-for-me) "")
                  "(kill-emacs)")))
    (byte-compile-file temp-file)
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -L %s -l %s -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "-debug-init" "--batch")
                      (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))
                      (expand-file-name (file-name-sans-extension (locate-library "ergoemacs-mode")))
                      temp-file)))
    (should (file-exists-p w-file))
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when  (file-exists-p (concat temp-file "c"))
      (delete-file (concat temp-file "c")))
    (when (file-exists-p w-file)
      (delete-file w-file))))

(ert-deftest ergoemacs-test-global-key-set-apps-220-before ()
  "Test global C-c b"
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before nil "C-c b") t)))

(ert-deftest ergoemacs-test-global-key-set-M-t-after ()
  "Test global M-t"
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after "M-t") t)))


(ert-deftest ergoemacs-test-global-key-set-C-d-after ()
  "Test global C-d"
  :tags '(:slow)
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-d") t)))

(ert-deftest ergoemacs-test-global-key-set-C-d-before ()
  "Test global C-d"
  :tags '(:slow :interactive)
  (should (equal (ergoemacs-test-global-key-set-before nil "C-d") t)))

(ert-deftest ergoemacs-test-issue-243 ()
  "Allow globally set keys like C-c C-c M-x to work globally while local commands like C-c C-c will work correctly. "
  :tags '(:slow)
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(condition-case err (progn ")
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-theme nil)")
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

(ert-deftest ergoemacs-test-ergoemacs-theme-component-351 ()
  "Test `ergoemacs-theme-compenent' before load"
  :tags '(:slow)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(eval-when-compile (require 'ergoemacs-macros) (require 'cl))"
              (or (and (boundp 'wait-for-me)
                       "(setq debug-on-error t debug-on-quit t)") "")
              "(ergoemacs-theme-component my-theme01 ()\n"
              "\"My ergoemacs-mode theme component\"\n"
              "(global-set-key (kbd \"C-x 1\") 'delete-other-windows)\n"
              "(global-set-key (kbd \"C-x 2\") 'split-window-vertically)\n"
              "(global-set-key (kbd \"C-x 3\") 'split-window-horizontally)\n"
              ";; more code ... \n"
              "(global-set-key '[f2] 'save-buffer)\n)\n"
              "(add-hook 'ergoemacs-mode-hook\n"
              "(lambda ()\n"
              "  (ergoemacs-require 'my-theme01)))\n"
              "(setq ergoemacs-mode--start-p t)"
              "(ergoemacs-mode 1)\n"
              "(when (and (eq (key-binding (kbd \"C-x 1\")) 'delete-other-windows)\n"
              "           (eq (key-binding (kbd \"C-x 2\")) 'split-window-vertically)\n"
              "           (eq (key-binding (kbd \"C-x 3\")) 'split-window-horizontally))\n"
              "(with-temp-file \"" w-file "\")\n"
              "   (message \"Passed\")"
              "  (insert \"Found\"))\n"
              (or (and (boundp 'wait-for-me) "")
                  "(kill-emacs)")))
    (byte-compile-file temp-file)
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -L %s -l %s -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "-debug-init" "--batch")
                      (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))
                      (expand-file-name (file-name-sans-extension (locate-library "ergoemacs-mode")))
                      temp-file)))
    (should (file-exists-p w-file))
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when  (file-exists-p (concat temp-file "c"))
      (delete-file (concat temp-file "c")))
    (when (file-exists-p w-file)
      (delete-file w-file))))


(ert-deftest ergoemacs-test-M-J-347 ()
  "Test keys that are not working in #347."
  :tags '(:slow :interactive)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(eval-when-compile (require 'ergoemacs-macros) (require 'cl))"
              "(setq ergoemacs-keyboard-layout \"sw\")\n"
              "(require 'ergoemacs-mode)\n"
              "(message \"Binding 1: %s\" (key-binding (kbd \"M-J\")))\n"
              "(global-unset-key (kbd \"M-J\"))\n"
              "(message \"Binding 2: %s\" (key-binding (kbd \"M-J\")))\n"
              "(global-set-key (kbd \"M-J\") 'beginning-of-buffer)\n"
              "(message \"Binding 3: %s\" (key-binding (kbd \"M-J\")))\n"
              "(setq ergoemacs-mode--start-p t)(ergoemacs-mode 1)"
              "(message \"Binding 4: %s\" (key-binding (kbd \"M-J\")))\n"
              "(when (eq (key-binding (kbd \"M-J\")) 'beginning-of-buffer)\n"
              "(with-temp-file \"" w-file "\")\n"
              "   (message \"Passed\")"
              "  (insert \"Found\"))\n"
              (or (and (boundp 'wait-for-me) "")
                  "(kill-emacs)")))
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -L %s -l %s -l %s"
                      emacs-exe (if (boundp 'wait-for-me) "" "--batch")
                      (expand-file-name (file-name-directory (locate-library "ergoemacs-mode")))
                      (expand-file-name (file-name-sans-extension (locate-library "ergoemacs-mode")))
                      temp-file)))
    (should (file-exists-p w-file))
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when (file-exists-p w-file)
      (delete-file w-file))))

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
      (insert "(setq ergoemacs-theme nil)")
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
      (ergoemacs-map--modify-active)
      (should (eq (key-binding (kbd "C-w")) 'ergoemacs-close-current-buffer))
      ;; The user-defined C-w should not affect kill-region remaps.
      (should (not (eq (key-binding [ergoemacs-remap kill-region]) 'ergoemacs-close-current-buffer))))))

(ert-deftest ergoemacs-test-keep-ctl-s ()
  "Keep mode-defined C-s in major-mode `ergoemacs-test-major-mode'.
Part of addressing Issue #147."
  :tags '(:interactive)
  (ergoemacs-test-layout
   (let (ret
         (ergoemacs-use-function-remapping t))
     (with-temp-buffer
       (ergoemacs-test-major-mode)
       (when (not (current-local-map))
         (use-local-map ergoemacs-test-major-mode-map))
       (ergoemacs-map--modify-active)
       (should (eq (key-binding (kbd "C-s")) 'save-buffer))
       (should (eq (key-binding [ergoemacs-remap isearch-forward]) 'search-forward))))))


(ert-deftest ergoemacs-test-keep-alt-s ()
  "Keep mode-defined M-s in major-mode `ergoemacs-test-major-mode'.
Tests Issue #372."
  :tags '(:interactive)
  (ergoemacs-test-layout
   :layout "us"
   :theme "reduction"
   (let (ret
         (ergoemacs-use-function-remapping t))
     (with-temp-buffer
       (ergoemacs-test-major-mode)
       (when (not (current-local-map))
         (use-local-map ergoemacs-test-major-mode-map))
       (ergoemacs-map--modify-active)
       (should (eq (key-binding (kbd "M-s")) 'ergoemacs-move-cursor-next-pane))))))

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
  (ergoemacs-map--modify-active)
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

  (letf (((symbol-function 'display-graphic-p) (lambda(&rest _ignore) t)))
    ;; Test M-i -> ^i -> TAB
    (should (string= "<C-i>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-i" t) 0) :ctl-to-alt)))))
    
    ;; Test M-[ -> ^[ -> ESC
    (should (string= "<C-[>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-[" t) 0) :ctl-to-alt)))))
    
    ;; Test M-m -> ^m -> RET
    (should (string= "<C-m>" (key-description (vector (ergoemacs-translate--event-mods (elt (read-kbd-macro "M-m" t) 0) :ctl-to-alt))))))

  (letf (((symbol-function 'display-graphic-p) (lambda(&rest _ignore) nil)))
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

(ert-deftest ergoemacs-test-translate-bound ()
  "Make sure that bound keys are put in the `ergoemacs-map--'
hash appropriaetly."
  :tags '(:translate)
  (ergoemacs-test-layout
   :layout "colemak"
   :theme "reduction"
   (should (equal (ergoemacs-gethash (read-kbd-macro "M-r" t) ergoemacs-map--)
                  (ergoemacs-gethash (ergoemacs-translate--meta-to-escape (read-kbd-macro "M-r" t)) ergoemacs-map--)))))

(ert-deftest ergoemacs-test-table-insert ()
  "Tests that table can insert without hanging emacs."
  :tags '(:table)
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (table-insert 1 2)
    (ergoemacs-command-loop--internal "abc <tab> abc <tab>")
    (should (string= (buffer-string) "+-----+
|abc  |
+-----+
|abc  |
+-----+
"))
    (kill-buffer (current-buffer))))

;; File variables
(ert-deftest ergoemacs-test-component-location ()
  "Make sure that the locations are correct."
  (should (string= "ergoemacs-themes" (file-name-sans-extension (file-name-nondirectory (plist-get (ergoemacs-component-struct-plist (ergoemacs-component-struct--lookup-hash "standard-fixed")) :file))))))

;; multiple cursors
;; (ert-deftest ergoemacs-test-mc-mark-next ()
;;   "Test Issue #342."
;;   :tags '(:mc)
;;   :expected-result :failed
;;   (ergoemacs-test-layout
;;    :layout "colemak"
;;    :theme "reduction"
;;    :macro "M-SPC M-y M-* n o t SPC f o o <return>"
;;    (save-excursion
;;      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
;;      (delete-region (point-min) (point-max))
;;      (emacs-lisp-mode)
;;      (insert "foo bar bar bar foo bar")
;;      (goto-char (point-min))
;;      (execute-kbd-macro macro)
;;      (should (string= (buffer-string) "not foo bar bar bar not foo bar"))
;;      (kill-buffer (current-buffer)))))

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


(ert-deftest ergoemacs-test-map-keymap-prefix ()
  "Tests mapping multiple keymaps defining a prefix."
  :tags '(:map-keymap)
  (let ((parent (make-sparse-keymap))
        (map (make-sparse-keymap))
        list)
    (define-key parent [27 ?a] 'ignore)
    (define-key map [27 ?b] 'ignore)
    (set-keymap-parent map parent)
    (ergoemacs-map-keymap
     (lambda(key item)
       (unless (or (eq item 'ergoemacs-prefix)
                   (consp key)
                   (equal key [ergoemacs-labeled]))
         (push key list)))
     map)
    (should (member [27 ?a] list))
    (should (member [27 ?b] list))
    list))

(ert-deftest ergoemacs-test-issue-379 ()
  "Test infinite recursive event-modifiers."
  :tags '(:translate)
  (should (equal (ignore-errors (ergoemacs-specials (ergoemacs-translate--event-modifiers 134217755))) '(control meta))))

(ert-deftest ergoemacs-test-temp-map-issue ()
  "Test temporary map issue."
  (if (version-list-< (version-to-list "24.4") (version-to-list emacs-version))
      (ergoemacs-test-layout
       :layout "colemak"
       :theme "reduction"
       :macro "M-8 M-SPC M-SPC M-i"
       (save-excursion
	 (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
	 (delete-region (point-min) (point-max))
	 (insert ergoemacs-test-lorem-ipsum)
	 (goto-char (point-max))
	 (beginning-of-line)
	 (execute-kbd-macro macro)
	 (should (eq (key-binding (kbd "8")) 'self-insert-command))
	 (kill-buffer (current-buffer)))))
  (should t))

(ert-deftest ergoemacs-test--swap-translation-386 ()
  "Test thes swapping of the translations."
  :tags '(:translate :interactive)
  (should (eq (lookup-key (ergoemacs ergoemacs-translate--parent-map) (or (and (eq system-type 'windows-nt) [apps]) [menu])) 'ergoemacs-command-loop--swap-translation)))

(ert-deftest ergoemacs-test-407 ()
  "Test M-s is switch pane."
  :tags '(:require-input :interactive)
  (let* ((emacs-exe (ergoemacs-emacs-exe))
         (w-file (expand-file-name "global-test" ergoemacs-dir))
         (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(add-to-list 'load-path \"" (expand-file-name (file-name-directory (locate-library "ergoemacs-mode"))) "\")"
	      "(add-to-list 'load-path \"" (expand-file-name (file-name-directory (locate-library "icicles"))) "\")"
       "(eval-when-compile (require 'ergoemacs-macros) (require 'cl))"
              (or (and (boundp 'wait-for-me)
                       "(setq debug-on-error t debug-on-quit t)") "")
	      "(setq ergoemacs-theme nil)"
	      "(setq ergoemacs-keyboard-layout \"us\")"
	      "(require 'ergoemacs-mode)\n"
              "(ergoemacs-mode 1)\n"
	      "(require 'icicles)\n"
	      "(icy-mode 1)\n"
              "(ergoemacs-theme-component reclaim-C-f ()\n"
              "  \"We need to give at least one sequence to reclaim C-f from isearch and get the new icicle-search-key-prefix picked up.\"\n"
	      "(global-set-key (kbd \"C-f .\") 'isearch-forward-symbol-at-point))"
              "(ergoemacs-require 'reclaim-C-f)"
              "(setq icicle-search-key-prefix (kbd \"C-f\"))"
	      "(ergoemacs-package smart-mode-line :ensure t (sml/setup))"
              "(ergoemacs-package srefactor :ensure t)"
              "(ergoemacs-package virtualenvwrapper :ensure t)"
	      "(defun test-freeze ()\n"
	      "(interactive)\n"
	      "(yes-or-no-p \"Are you sure you want to remove this file? \"))"
	      "(global-set-key (kbd \"C-1\") 'test-freeze)"
	      "(insert \"Try C-1 to see if emacs freezes.\\nThen try M-a test-freeze.\\nM-a calc, do something and then exit with q it should exit\nMake sure M-o goes forward word in icy ergoemacs-mode.\")"
              ;; (or (and (boundp 'wait-for-me) "")
              ;;     "(kill-emacs)")
	      ))
    (byte-compile-file temp-file)
    (message "%s"
             (shell-command-to-string
              (format "%s %s -Q -l %s"
                      emacs-exe "-debug-init"                      
                      temp-file)))
    
    (when  (file-exists-p temp-file)
      (delete-file temp-file))
    (when  (file-exists-p (concat temp-file "c"))
      (delete-file (concat temp-file "c")))))

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
                                                           (key-binding (eval key))
                                                         (key-binding key))) ',keys)))))
	 ,minibuffer-call) nil))

(ert-deftest ergoemacs-test-icy-407-minibuffer ()
  "Test minibuffer keybindings for `icy-mode'.
[f11] = `previous-history-element'
[f12] = `next-history-element'
M-s   = `ergoemacs-move-cursor-next-pane'
M-r   = `kill-word'"
  :tags '(:icy-mode :interactive)
  (icy-mode 1)
  (let ((keys))
(ergoemacs-test-layout
 :layout "us"
 :theme "standard"
 (should (equal (ergoemacs-minibuffer-key-bindings
		 (call-interactively 'icicle-execute-extended-command)
		 [f11] [f12] (read-kbd-macro "M-o") (read-kbd-macro "M-s") (read-kbd-macro "M-r"))
		'(previous-history-element next-history-element forward-word ergoemacs-move-cursor-next-pane kill-word)))))
(icy-mode -1))

(provide 'ergoemacs-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
