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

(declare-function ergoemacs-mode-reset "ergoemacs-mode.el")

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme)
(defvar ergoemacs-command-loop-type)

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

(defvar ergoemacs-test-fast nil)
(defun ergoemacs-test-fast ()
  "Fast test of ergoemacs-mode (doesn't include keyboard startup issues)."
  (interactive)
  (unwind-protect
      (progn
        (setq ergoemacs-test-fast t)
        (call-interactively 'ergoemacs-test))
    (setq ergoemacs-test-fast nil)))

;;;###autoload
(defun ergoemacs-test ()
  "Test ergoemacs issues."
  (interactive)
  (let ((ret t)
        (test))
    (elp-instrument-package "ergoemacs-")
    (ert "^ergoemacs-test-")
    (call-interactively 'elp-results)))

;; Test isearch

(ert-deftest ergoemacs-test-isearch-C-f-backspace ()
  "Test Backspace in `isearch-mode'"
  ;; Google Code Issue #145
  (let ((ret t))
    (ergoemacs-test-layout
     :layout "colemak"
     :macro "C-f ars C-f <backspace> M-n"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert "aars1\nars2\nars3\nars4")
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (when (looking-at ".*")
         (unless (string= "s1" (match-string 0))
           (setq ret nil)))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-isearch-C-f ()
  "C-f doesn't work in isearch-mode."
  ;; Google Code Issue #119
  (let ((ret t))
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
         (unless (string= "3" (match-string 0))
           (setq ret nil)))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-isearch-works-with-region ()
  "With vanilla Emacs, when mark is active and even some region is
already selected, isearch-ing would expand or shrink selection.
Currently ergoemacs-mode discards selection as soon as isearch key is
pressed. Reproducible with ergoemacs-clean.
Issue #186."
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

;;; Shift Selection

(ert-deftest ergoemacs-test-shift-select-move-no-mark ()
  "Tests another shifted selection "
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
  (let (ret)
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (delete-region (point-min) (point-max))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (execute-kbd-macro (edmacro-parse-keys "M-E M-E" t))
       (call-interactively 'ergoemacs-cut-line-or-region)
       (setq ret (= (point) (point-min)))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))


(ert-deftest ergoemacs-test-shift-select-subword ()
  "Test for mark working with shift-selection of `subword-forward'."
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
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (push-mark)
      (end-of-line)
      (ergoemacs-copy-line-or-region)
      (push-mark (point))
      (push-mark (point-max) nil t)
      (goto-char (point-min))
      (ergoemacs-paste)
      (setq ret (or deactivate-mark (not mark-active))))
    (should ret)))
    

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
    (should ret)))


(ert-deftest ergoemacs-test-copy-paste-issue-130-cut ()
  "Attempts to test Issue #130 -- Cut"
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
      (with-timeout (0.15 nil)
        (ergoemacs-command-loop "C-x"))
      (setq ret (string= "" (buffer-string)))
      (kill-buffer (current-buffer)))
    (should ret)))

(ert-deftest ergoemacs-test-copy-paste-issue-130-copy ()
  "Attempts to test Issue #130 -- Copy"
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
      (with-timeout (0.15 nil)
        (ergoemacs-command-loop "C-c"))
      (goto-char (point-max))
      (ergoemacs-paste)
      (should (string= (concat txt txt)
                       (buffer-string)))
      (kill-buffer (current-buffer)))))

(ert-deftest ergoemacs-test-copy-paste-apps-cut ()
  "Tests <apps> x on QWERTY cutting a region, not just a line."
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
       (execute-kbd-macro macro)
       (setq ret (string= "" (buffer-string)))
       (kill-buffer (current-buffer))))
    (should ret)))

(ert-deftest ergoemacs-test-copy-paste-apps-copy ()
  "Tests <apps> c on QWERTY copying a region, not just a line."
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


;; Not sure why this doesn't work
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
  (let ((ergoemacs-test-fn t))
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (call-interactively 'calc)
     (unwind-protect
         (should (eq (key-binding (kbd "C-z")) (or (command-remapping 'calc-undo (point)) 'calc-undo)))
       (call-interactively 'calc-quit)))))

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
  :expected-result (if noninteractive :failed :passed) ;; Not sure why.
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
       (setq ret (looking-at "eprehenderit"))
       (ergoemacs-unchorded-alt-modal)
       (kill-buffer (current-buffer))))
    (should ret)))



;;; Command Loop

(ert-deftest ergoemacs-test-command-loop-apps-e-t-_ ()
  "Test that colemak <apps> e t sends _.
Should test for Issue #143."
  ;; (let (unread-command-events)
  ;;   (ergoemacs-test-layout
  ;;    :theme "reduction"
  ;;    :layout "colemak"
  ;;    (ergoemacs-read-key (format "<%s> e t"
  ;;                                (if (eq system-type 'windows-nt)
  ;;                                    "apps" "menu")))
  ;;    (should (equal (listify-key-sequence (read-kbd-macro "_"))
  ;;                   unread-command-events))))
  )


(ert-deftest ergoemacs-test-command-loop-C-x-8-! ()
  "Test that unicode translations work.
See Issue #138."
  (let (ergoemacs-command-loop-type)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (delete-region (point-min) (point-max))
      (with-timeout (1 nil)
        (ergoemacs-command-loop "C-x 8 !"))
      (should (string= "¡" (buffer-string)))
      (kill-buffer (current-buffer)))))

(ert-deftest ergoemacs-test-command-loop-C-x-8-A ()
  "Test that unicode translations work.
See Issue #138."
  (save-excursion
    (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
    (delete-region (point-min) (point-max))
    (with-timeout (1 nil)
      (ergoemacs-command-loop "C-x 8 \" A"))
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
  :expected-result (if noninteractive :failed :passed)
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
  :expected-result (if noninteractive :passed :failed)
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
         (ergoemacs-read-key "M-O A")) ; by looking at `ergoemacs-read-key' this seems to be translating correctly, but... it doesn't run in this context.
       (message "Decode: %s" (lookup-key input-decode-map (kbd "M-O A")))
       (setq ret (looking-at "nulla pariatur. Excepteur sint occaecat cupidatat non proident,"))
       (kill-buffer (current-buffer)))
     (setq input-decode-map (copy-keymap old-map)))
    (should ret)))

(provide 'ergoemacs-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
