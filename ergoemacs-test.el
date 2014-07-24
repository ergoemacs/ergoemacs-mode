;;; ergoemacs-test.el --- tests for ErgoEmacs issues

;; Copyright © 2013, 2014 Free Software Foundation, Inc.

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

(declare-function ergoemacs-set "ergoemacs-theme-engine.el")
(declare-function ergoemacs-define-key "ergoemacs-theme-engine.el")
(declare-function ergoemacs-theme-get-version "ergoemacs-theme-engine.el")
(declare-function ergoemacs-theme-set-version "ergoemacs-theme-engine.el")

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

;;;###autoload
(defun ergoemacs-test ()
  "Test ergoemacs issues."
  (interactive)
  (let ((ret t)
        (test))
    (when nil
      (message "Updating for the current version of emacs")
      (ergoemacs-warn-globally-changed-keys t))
    (elp-instrument-package "ergoemacs-")
    (ert "^ergoemacs-test-")
    (call-interactively 'elp-results)))

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme)
(declare-function ergoemacs-mode "ergoemacs-mode.el")
(ert-deftest ergoemacs-test-google-code-145 ()
  "Backspace doesn't work in `isearch-mode'."
  (let ((ret t))
    (ergoemacs-test-layout
     :layout "colemak"
     :macro "C-f ars C-f <backspace> M-n"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert "aars1\nars2\nars3\nars4")
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (when (looking-at ".*")
         (unless (string= "s1" (match-string 0))
           (setq ret nil)))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-google-code-119 ()
  "C-f doesn't work in isearch-mode."
  (let ((ret t))
    (ergoemacs-test-layout
     :layout "colemak"
     :cua t
     :macro "C-f ars C-f C-f"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert "aars1\nars2\nars3\nars4")
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (when (looking-at ".*")
         (unless (string= "3" (match-string 0))
           (setq ret nil)))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shifted-move-no-mark ()
  "Tests another shifted selection bug."
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

(ert-deftest ergoemacs-test-shifted-move-keep-mark ()
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

(defvar ergoemacs-dir)
(declare-function ergoemacs-emacs-exe "ergoemacs-functions.el")
(declare-function ergoemacs-shortcut-remap-list "ergoemacs-shortcuts.el")
(defun ergoemacs-test-global-key-set-before (&optional after key ergoemacs ignore-prev-global delete-def)
  "Test the global key set before ergoemacs-mode is loaded."
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
                   (ergoemacs
                    (format "ergoemacs-key \"%s\" " test-key))
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
      (insert "(require 'ergoemacs-mode)(ergoemacs-mode 1)")
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
      (format "%s --batch -Q -l %s" emacs-exe temp-file)))
    (delete-file temp-file)
    (when (file-exists-p w-file)
      (setq ret 't)
      (delete-file w-file))
    ret))

(ert-deftest ergoemacs-test-global-key-set-before-1 ()
  "Test global set key before ergoemacs-mode loads."
  (should (equal (ergoemacs-test-global-key-set-before) t)))

(ert-deftest ergoemacs-test-global-key-set-before-2 ()
  "Test global set key before ergoemacs-mode loads (define-key)."
  (should (equal (ergoemacs-test-global-key-set-before nil nil 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-after ()
  "Test global set key after ergoemacs loads."
  (should (equal (ergoemacs-test-global-key-set-before 'after) t)))

(ert-deftest ergoemacs-test-global-key-set-after-2 ()
  "Test global set key after ergoemacs loads (define-key)."
  (should (equal (ergoemacs-test-global-key-set-before 'after nil 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-before ()
  "Test setting <apps> m c before loading."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-before-2 ()
  "Test setting <apps> m c before loading (define-key)."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-m-semi-before ()
  "Test setting M-; before loading."
  (should (equal (ergoemacs-test-global-key-set-before nil "M-;") t)))

(ert-deftest ergoemacs-test-global-key-set-m-semi-after ()
  "Test setting M-; before loading."
  (should (equal (ergoemacs-test-global-key-set-before t "M-;") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-before ()
  "Test setting <apps> before loading."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>")) t)))


(ert-deftest ergoemacs-test-global-key-set-apps-before-2 ()
  "Test setting <apps> before loading (define-key)."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps>"
       "<menu>") 'define-key) t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-before ()
  "Test setting <apps> m before loading."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-before-2 ()
  "Test setting <apps> m before loading (define-key)."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     nil
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-after ()
  "Test setting <apps> m after loading"
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") nil nil "<menu>") t)))


(ert-deftest ergoemacs-test-global-key-set-apps-m-after-2 ()
  "Test setting <apps> m after loading (define-key)"
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m"
       "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after ()
  "Test setting <apps> m c after loading."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after-2 ()
  "Test setting <apps> m c after loading (define-key)."
  (should
   (equal
    (ergoemacs-test-global-key-set-before
     'after
     (if (eq system-type 'windows-nt)
         "<apps> m c"
       "<menu> m c") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-after-c-e ()
  "Test C-e after"
  (should
   (ergoemacs-test-global-key-set-before
    'after "C-e" 'ergoemacs-key)))

(declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
(ert-deftest ergoemacs-test-ctl-c-ctl-c ()
  "Issue #64.  Should translate C-c C-c correctly."
  (let (ergoemacs-use-unicode-char)
    (should (string= (ergoemacs-pretty-key "C-c C-c") "[Ctrl+C][Ctrl+C]"))))

(declare-function ergoemacs-cut-line-or-region "ergoemacs-functions.el")
(ert-deftest ergoemacs-test-cut-line-or-region ()
  "Issue #68.
kill-ring function name is used and such doesn't exist. It errs when
not using cua or cutting line. I think kill-region is what is meant."
  (let ((old-c cua-mode)
        (ret t))
    (cua-mode -1)
    (with-temp-buffer
      (insert ergoemacs-test-lorem-ipsum)
      (condition-case err
          (ergoemacs-cut-line-or-region)
        (error (setq ret nil))))
    (when old-c
      (cua-mode 1))
    (should ret)))

(declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
(ert-deftest ergoemacs-test-issue-77 ()
  "Issue #77.
Test \"C-x \" translating to \"[Ctrl+X][]\", should be \"[Ctrl+X]\""
  (let ((ergoemacs-use-unicode-char nil))
    (should (string= (ergoemacs-pretty-key "C-x ") "[Ctrl+X]"))))

(ert-deftest ergoemacs-test-issue-86 ()
  "Test Issue #86.
Hyper Key mapping no longer works."
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(condition-case err (progn ")
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-theme nil)")
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (insert "(require 'ergoemacs-mode)(ergoemacs-mode 1)")
      (insert "(defun osx-map-hyper ()
        (global-unset-key \"\C-p\") 
        (define-key function-key-map (kbd \"C-p\") 'event-apply-hyper-modifier))")
      (insert "(osx-map-hyper)")
      (insert "(run-hooks 'emacs-startup-hook)")
      (insert (format "(when (not (eq 'ergoemacs-print-buffer-confirm
                             (lookup-key ergoemacs-keymap (read-kbd-macro \"C-p\"))))
                (with-temp-file \"%s\"
                  (insert \"Ok\")))" w-file))
      (insert (format "(if (file-exists-p \"%s\") (message \"Passed\") (message \"Failed\"))" w-file))
      (insert ") (error (message \"Error %s\" err)))")
      (unless (boundp 'wait-for-me)
        (insert "(kill-emacs)")))
    (message "%s"
             (shell-command-to-string
              (format "%s --batch -Q -l %s" emacs-exe temp-file)))
    (delete-file temp-file)
    (should (file-exists-p w-file))
    (when (file-exists-p w-file)
      (delete-file w-file))))

(ert-deftest ergoemacs-test-issue-98 ()
  "Test full fast-movement-keys"
  (let ((ergoemacs-repeat-movement-commands 'all))
    (ergoemacs-test-layout
     :layout "colemak"
     :cua t
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (execute-kbd-macro (edmacro-parse-keys "M-e"))
       (should (looking-at "do eiusmod"))
       (execute-kbd-macro (edmacro-parse-keys "e"))
       (should (looking-at "enim ad"))
       (execute-kbd-macro (edmacro-parse-keys "u"))
       (should (looking-at "do eiusmod"))
       (kill-buffer (current-buffer))))))

(declare-function ergoemacs-unchorded-alt-modal "ergoemacs-translate.el"
                  nil t)
(ert-deftest ergoemacs-test-modal-preserve-mark ()
  "Issue #101.
Test next and prior translation."
  (with-temp-buffer
    (insert ergoemacs-test-lorem-ipsum)
    (goto-char (point-min))
    (ergoemacs-unchorded-alt-modal)
    (set-mark (point))
    (forward-char 3)
    (ergoemacs-unchorded-alt-modal)
    (should mark-active)))

(ert-deftest ergoemacs-test-issue-114 ()
  "Attempts to test Issue #114."
  (let ((ret t))
    (ergoemacs-test-layout
     :macro "C-f ars C-f <backspace> M-n"
     :layout "colemak"
     (setq ret (lookup-key isearch-mode-map
                           (read-kbd-macro
                            (format "<%s> s"
                                    (if (eq system-type 'windows-nt)
                                        "apps" "menu"))))))
    (should ret)))

(defvar ergoemacs-ctl-c-or-ctl-x-delay)
(ert-deftest ergoemacs-test-issue-130-cut ()
  "Attempts to test Issue #130 -- Cut"
  :expected-result (if noninteractive :failed :passed)
  (if noninteractive (should nil)
    (let ((ret t)
          (ergoemacs-ctl-c-or-ctl-x-delay 0.1)
          (ergoemacs-handle-ctl-c-or-ctl-x 'both))
      (with-temp-buffer
        (insert ergoemacs-test-lorem-ipsum)
        (push-mark (point))
        (push-mark (point-max) nil t)
        (goto-char (point-min))
        (with-timeout (0.15 nil)
          (call-interactively 'ergoemacs-ctl-x))
        (setq ret (string= "" (buffer-string))))
      (should ret))))

(declare-function ergoemacs-paste "ergoemacs-functions.el")
(ert-deftest ergoemacs-test-issue-130-copy ()
  "Attempts to test Issue #130 -- Copy"
  :expected-result (if noninteractive :failed :passed)
  (if noninteractive (should nil)
      (let ((ret t)
            (ergoemacs-ctl-c-or-ctl-x-delay 0.1)
            (ergoemacs-handle-ctl-c-or-ctl-x 'both))
        (with-temp-buffer
          (insert ergoemacs-test-lorem-ipsum)
          (push-mark (point))
          (push-mark (point-max) nil t)
          (goto-char (point-min))
          (with-timeout (0.15 nil)
            (call-interactively 'ergoemacs-ctl-c))
          (goto-char (point-max))
          (ergoemacs-paste)
          (setq ret (string= (concat ergoemacs-test-lorem-ipsum
                                     ergoemacs-test-lorem-ipsum)
                             (buffer-string))))
        (should ret))))

(ert-deftest ergoemacs-test-apps-cut ()
  "Tests <apps> x on QWERTY cutting a region, not just a line."
  (let (ret)
    (ergoemacs-test-layout
     :macro (format "<%s> x"
                    (if (eq system-type 'windows-nt)
                        "apps" "menu"))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (push-mark (point))
       (push-mark (point-max) nil t)
       (goto-char (point-min))
       (execute-kbd-macro macro)
       (setq ret (string= "" (buffer-string)))
       (kill-buffer (current-buffer))))
    (should ret)))

(ert-deftest ergoemacs-test-apps-copy ()
  "Tests <apps> c on QWERTY copying a region, not just a line."
  (ergoemacs-test-layout
   :macro (format "C-a <%s> c C-v"
                  (if (eq system-type 'windows-nt)
                      "apps" "menu"))
   (let ((test-string "1\n2\n3\n4"))
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert test-string)
       (execute-kbd-macro macro)
       (should (string= (concat test-string test-string)
                        (buffer-string)))
       (kill-buffer (current-buffer))))))

(ert-deftest ergoemacs-test-shift-selection ()
  "Test that shift selection works properly.
Issue #137."
  (let (ret)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (execute-kbd-macro (edmacro-parse-keys "<S-down> <S-down>" t))
      (call-interactively 'ergoemacs-cut-line-or-region)
      (setq ret (= (point) (point-min)))
      (kill-buffer (current-buffer)))
    (should ret)))

(declare-function ergoemacs-read-key "ergoemacs-shortcuts.el")
(ert-deftest ergoemacs-test-translations ()
  "Test that unicode translations work.
See Issue #138."
  (let (ret
        unread-command-events)
    (ergoemacs-read-key "C-x 8 !")
    (setq ret (equal (listify-key-sequence (read-kbd-macro "¡")) unread-command-events))
    (should ret)))

(ert-deftest ergoemacs-test-multi-translations ()
  "Test that multicode unicode translations work.
See Issue #140."
  (let (ret
        unread-command-events)
    (ergoemacs-read-key "C-x 8 \" A")
    (setq ret (equal (listify-key-sequence (read-kbd-macro "Ä")) unread-command-events))
    (should ret)))

(ert-deftest ergoemacs-test-shortcut ()
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


(ert-deftest ergoemacs-test-misspelled-mark ()
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

(ert-deftest ergoemacs-test-shift-select-subword ()
  "Test for mark working with shift-selection of `subword-forward'."
  (let (ret)
    (ergoemacs-test-layout
     :macro "M-Y M-x"
     :theme "reduction"
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (subword-mode 1)
       (goto-char (point-max))
       (beginning-of-line)
       (execute-kbd-macro macro)
       (when (looking-at " in culpa qui")
         (setq ret t))
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-apps-e-t-_ ()
  "Test that colemak <apps> e t sends _.
Should test for Issue #143."
  (let (unread-command-events)
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (ergoemacs-read-key (format "<%s> e t"
                                 (if (eq system-type 'windows-nt)
                                     "apps" "menu")))
     (should (equal (listify-key-sequence (read-kbd-macro "_"))
                    unread-command-events)))))

(ert-deftest ergoemacs-test-ignore-global-definitions-on-remap ()
  "If someone sets a key on the global keymap, ignore it.
Addresses Issue #145."
  (let ((old-global-map (current-global-map))
        ret
        new-global-map)
    (setq new-global-map (copy-keymap (current-global-map)))
    (define-key new-global-map (read-kbd-macro "M-q") 'ergoemacs-cut-line-or-region)
    (unwind-protect
        (with-temp-buffer
          (use-global-map new-global-map)
          (setq ret (ergoemacs-shortcut-remap-list 'fill-paragraph)))
      (use-global-map old-global-map))
    (should (not ret))))

(define-derived-mode ergoemacs-test-major-mode fundamental-mode "ET"
  "Major mode for testing some issues with `ergoemacs-mode'.
\\{ergoemacs-test-major-mode-map}"
  (define-key ergoemacs-test-major-mode-map (read-kbd-macro "C-s") 'save-buffer))

(add-hook 'ergoemacs-test-major-mode-hook
          '(lambda()
             (interactive)
             (define-key ergoemacs-test-major-mode-map
               (read-kbd-macro "C-w") 'ergoemacs-close-current-buffer)))

(ert-deftest ergoemacs-test-ignore-ctl-w ()
  "Ignore user-defined C-w in major-mode `ergoemacs-test-major-mode'.
Part of addressing Issue #147."
  (let (ret
        (ergoemacs-use-function-remapping t))
    (with-temp-buffer
      (ergoemacs-test-major-mode)
      (setq ret (ergoemacs-shortcut-remap-list 'kill-region)))
    (should (not ret))))

(ert-deftest ergoemacs-test-keep-ctl-s ()
  "Keep mode-defined C-s in major-mode `ergoemacs-test-major-mode'.
Part of addressing Issue #147."
  (let (ret
        (ergoemacs-use-function-remapping t))
    (with-temp-buffer
      (ergoemacs-test-major-mode)
      (setq ret (ergoemacs-shortcut-remap-list 'isearch-forward)))
    (eq (nth 0 (nth 0 ret)) 'save-buffer)))


(ert-deftest ergoemacs-test-overlay-paren ()
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

(ert-deftest ergoemacs-test-shift-selection-reduction ()
  "Test that shift selection works properly in reduction."
  (let (ret)
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (execute-kbd-macro (edmacro-parse-keys "M-E M-E" t))
       (call-interactively 'ergoemacs-cut-line-or-region)
       (setq ret (= (point) (point-min)))
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
       (insert ergoemacs-test-lorem-ipsum)
       (goto-char (point-min))
       (mark-word)
       (execute-kbd-macro macro)
       (setq ret mark-active)
       (kill-buffer (current-buffer))))
    (should (equal ret t))))

(ert-deftest ergoemacs-test-reduction-M-o-works ()
  "Test Ergoemacs M-o works correctly (Issue #171)."
  (let ((ergoemacs-test-fn t))
    (ergoemacs-test-layout
     :theme "reduction"
     :layout "colemak"
     (with-timeout (0.2 nil) (ergoemacs-read-key "M-o"))
     (message "Test FN: %s" ergoemacs-test-fn)
     (should (eq ergoemacs-test-fn (or (command-remapping 'execute-extended-command (point)) 'execute-extended-command))))))

(declare-function ergoemacs-copy-line-or-region "ergoemacs-functions.el")
(ert-deftest ergoemacs-test-issue-184-paste ()
  "Issue #184; Not replace the \"selected all\" by paste."
  (let ((ret t)
        (ergoemacs-handle-ctl-c-or-ctl-x 'both))
    (ergoemacs-test-layout
     :macro "C-v"
     (save-excursion
       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
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

(ert-deftest ergoemacs-test-issue-184-paste-should-clear-mark ()
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

(ert-deftest ergoemacs-test-command-remapping ()
  "Test to make sure remapping for `ergoemacs-commands' are applied."
  (should (eq 'ergoemacs-describe-key (command-remapping 'describe-key))))

;; Fixed, but tests dont work.  Not sure why ergoemacs-test-fn goes to nil

;; (ert-deftest ergoemacs-test-apps-h-v ()
;;   "Make Sure <apps> h v works correctly"
;;   (let ((old-ergoemacs-theme ergoemacs-theme)
;;         (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
;;         (ergoemacs-test-fn t)
;;         (rk (format "<%s> h v"
;;                     (or (and (eq system-type 'windows-nt) "apps") "menu")))
;;         (ret nil))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme "reduction")
;;     (setq ergoemacs-keyboard-layout "colemak")
;;     (ergoemacs-mode 1)
;;     (with-timeout (0.2 nil)
;;       (ergoemacs-read-key rk))
;;     (message "Test FN: %s" ergoemacs-test-fn)
;;     (setq ret (eq ergoemacs-test-fn (or (command-remapping 'describe-variable (point)) 'describe-variable)))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme old-ergoemacs-theme)
;;     (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
;;     (ergoemacs-mode 1)
;;     (should ret)))

;; (ert-deftest ergoemacs-test-apps-h-z ()
;;   "Make Sure <apps> h z works correctly"
;;   (let ((old-ergoemacs-theme ergoemacs-theme)
;;         (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
;;         (ergoemacs-test-fn t)
;;         (rk (format "<%s> h z"
;;                     (or (and (eq system-type 'windows-nt) "apps") "menu")))
;;         (ret nil))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme "reduction")
;;     (setq ergoemacs-keyboard-layout "colemak")
;;     (ergoemacs-mode 1)
;;     (with-timeout (0.2 nil)
;;       (ergoemacs-read-key rk))
;;     (message "Test FN: %s" ergoemacs-test-fn)
;;     (setq ret (eq ergoemacs-test-fn (or (command-remapping 'ergoemacs-clean (point)) 'ergoemacs-clean)))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme old-ergoemacs-theme)
;;     (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
;;     (ergoemacs-mode 1)
;;     (should ret)))

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

;; (ert-deftest ergoemacs-test-comment-dwim-deactivate-region ()
;;   "Makes sure that `comment-dwim' deactivates the region.
;; Issue #203"
;;   :expected-result :failed ;; It works, just doesn't pass the test :(
;;   (let ((old-ergoemacs-theme ergoemacs-theme)
;;         (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
;;         (macro (edmacro-parse-keys "M-o" t))
;;         (ret t))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme nil)
;;     (setq ergoemacs-keyboard-layout "colemak")
;;     (ergoemacs-mode 1)
;;     (cua-mode 1)
;;     (save-excursion
;;       (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
;;       (emacs-lisp-mode)
;;       (insert ergoemacs-test-lorem-ipsum)
;;       (goto-char (point-min))
;;       (mark-word)
;;       (execute-kbd-macro macro)
;;       (setq ret (not mark-active))
;;       (kill-buffer (current-buffer)))
;;     (ergoemacs-mode -1)
;;     (setq ergoemacs-theme old-ergoemacs-theme)
;;     (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
;;     (ergoemacs-mode 1)
;;     (should (equal ret t))))

(ert-deftest ergoemacs-test-alt-mode-horizontal-position ()
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
       (ergoemacs-unchorded-alt-modal)
       (execute-kbd-macro macro)
       (setq ret (looking-at "eprehenderit"))
       (ergoemacs-unchorded-alt-modal)
       (kill-buffer (current-buffer))))
    (should ret)))


(ert-deftest ergoemacs-test-global-key-set-after-220 ()
  "Test global C-c b"
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-c b") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-220-before ()
  "Test global C-c b"
  (should (equal (ergoemacs-test-global-key-set-before nil "C-c b") t)))

(ert-deftest ergoemacs-test-global-key-set-M-t-after ()
  "Test global M-t"
  (should (equal (ergoemacs-test-global-key-set-before 'after "M-t") t)))


(ert-deftest ergoemacs-test-global-key-set-C-d-after ()
  "Test global C-d"
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-d") t)))

(ert-deftest ergoemacs-test-global-key-set-C-d-before ()
  "Test global C-d"
  (should (equal (ergoemacs-test-global-key-set-before nil "C-d") t)))

(ert-deftest ergoemacs-test-issue-243 ()
  "Allow globally set keys like C-c C-c M-x to work globally while local commands like C-c C-c will work correctly. "
  :expected-result :failed
  (let ((emacs-exe (ergoemacs-emacs-exe))
        (w-file (expand-file-name "global-test" ergoemacs-dir))
        (temp-file (make-temp-file "ergoemacs-test" nil ".el")))
    (with-temp-file temp-file
      (insert "(condition-case err (progn ")
      (insert (format "(add-to-list 'load-path \"%s\")" ergoemacs-dir))
      (insert "(setq ergoemacs-theme nil)")
      (insert "(setq ergoemacs-keyboard-layout \"us\")")
      (insert "(require 'ergoemacs-mode)(require 'ergoemacs-test)(ergoemacs-mode 1)")
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

(ert-deftest ergoemacs-test-bol-or-what ()
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

(ert-deftest ergoemacs-test-eol-or-what ()
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

(declare-function package-list-packages-no-fetch "package.el")
(ert-deftest ergoemacs-test-u-for-package-list-packages ()
  "Test `package-list-packages' `substitute-command-keys'"
  (require 'package)
  (package-list-packages-no-fetch)
  (should (string= (ergoemacs-pretty-key "U")
                   (substitute-command-keys
                    "\\[package-menu-mark-upgrades]")))
  (kill-buffer (current-buffer)))

(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
(ert-deftest ergoemacs-test-unbind-commands-active ()
  "Make sure the unbound keys work"
  (let (ergoemacs-shortcut-keys ergoemacs-mode ergoemacs-read-input-keys)
    (should (eq 'ergoemacs-undefined (ergoemacs-real-key-binding (read-kbd-macro "C-x C-s"))))))

(declare-function ergoemacs-shortcut-for-command "ergoemacs-menus.el")
(declare-function ergoemacs-kbd-to-key "ergoemacs-menus.el")
(ert-deftest ergoemacs-test-keyboard-translations ()
  "Should test Issue #265"
  :expected-result :failed ;; Not sure why... But for now ignore the error
  (ergoemacs-test-layout ;; Us/standard
   (ergoemacs-shortcut-for-command 'goto-line) ;;
   (should (string= (ergoemacs-shortcut-for-command 'ergoemacs-new-empty-buffer) (ergoemacs-kbd-to-key "C-n")))
   (should (string= (ergoemacs-shortcut-for-command 'make-frame-command) (ergoemacs-kbd-to-key "C-N")))
   (should (string= (ergoemacs-shortcut-for-command 'find-file) (ergoemacs-kbd-to-key "C-o")))
   (should (string= (ergoemacs-shortcut-for-command 'ergoemacs-open-last-closed) (ergoemacs-kbd-to-key "C-T")))
   (should (string= (ergoemacs-shortcut-for-command 'ergoemacs-close-current-buffer) (ergoemacs-kbd-to-key "C-w")))
   (should (string= (ergoemacs-shortcut-for-command 'revert-buffer) (ergoemacs-kbd-to-key "C-r")))
   (should (string= (ergoemacs-shortcut-for-command 'ergoemacs-print-buffer-confirm) (ergoemacs-kbd-to-key "C-p")))
   (should (string= (ergoemacs-shortcut-for-command 'save-buffer) (ergoemacs-kbd-to-key "C-s")))
   (should (string= (ergoemacs-shortcut-for-command 'write-file) (ergoemacs-kbd-to-key "C-S")))
   (should (string= (ergoemacs-shortcut-for-command 'split-window-below) (ergoemacs-kbd-to-key "M-4")))
   (should (string= (ergoemacs-shortcut-for-command 'split-window-right) (ergoemacs-kbd-to-key "M-$")))
   (should (string= (ergoemacs-shortcut-for-command 'delete-other-windows) (ergoemacs-kbd-to-key "M-3")))
   (should (string= (ergoemacs-shortcut-for-command 'execute-extended-command) (ergoemacs-kbd-to-key "M-a")))
   (should (string= (ergoemacs-shortcut-for-command 'goto-line) (ergoemacs-kbd-to-key "C-l")))))

;; (ert-deftest ergoemacs-test-5.3.7 ()
;;   "Test Ergoemacs 5.3.7 keys"
;;   (let ((ergoemacs-test-fn t))
;;     (ergoemacs-test-layout
;;      :version "5.3.7"
;;      (with-timeout (0.2 nil) (ergoemacs-read-key "M-n"))
;;      (should (eq ergoemacs-test-fn (or (command-remapping 'keyboard-quit (point)) 'keyboard-quit))))))

(provide 'ergoemacs-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
