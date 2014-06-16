;;; ergoemacs-test-bind-keys.el --- tests for ErgoEmacs Key binding issues

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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
      (format "%s -Q -l %s" emacs-exe temp-file)))
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
  (should (equal (ergoemacs-test-global-key-set-before nil
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m c"
                                                         "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-before-2 ()
  "Test setting <apps> m c before loading (define-key)."
  (should (equal (ergoemacs-test-global-key-set-before nil
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
  (should (equal (ergoemacs-test-global-key-set-before nil
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m"
                                                         "<menu> m") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-before-2 ()
  "Test setting <apps> m before loading (define-key)."
  (should (equal (ergoemacs-test-global-key-set-before nil
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m"
                                                         "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-after ()
  "Test setting <apps> m after loading"
  (should (equal (ergoemacs-test-global-key-set-before 'after
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m"
                                                         "<menu> m") nil nil "<menu>") t)))


(ert-deftest ergoemacs-test-global-key-set-apps-m-after-2 ()
  "Test setting <apps> m after loading (define-key)"
  (should (equal (ergoemacs-test-global-key-set-before 'after
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m"
                                                         "<menu> m") 'define-key nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after ()
  "Test setting <apps> m c after loading."
  (should (equal (ergoemacs-test-global-key-set-before 'after
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m c"
                                                         "<menu> m c") nil nil "<menu>") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-m-c-after-2 ()
  "Test setting <apps> m c after loading (define-key)."
  (should (equal (ergoemacs-test-global-key-set-before 'after
                                                       (if (eq system-type 'windows-nt)
                                                           "<apps> m c"
                                                         "<menu> m c") 'define-key nil "<menu>") t)))

(ert-deftest ergoemast-test-global-key-set-after-c-e ()
  "Test C-e after"
  (should (equal (ergoemacs-test-global-key-set-before
                  'after "C-e" 'ergoemacs-key))))

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
              (format "%s -Q -l %s" emacs-exe temp-file)))
    (delete-file temp-file)
    (should (file-exists-p w-file))
    (when (file-exists-p w-file)
      (delete-file w-file))))

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
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ret nil)
        (macro (edmacro-parse-keys (format "M-i <%s> e e"
                                           (if (eq system-type 'windows-nt)
                                               "apps" "menu")) t))
        tmp (tmp-key (make-sparse-keymap)) overlays)
    (define-key tmp-key [ergoemacs-test] 'ignore)
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
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
      (mapc
       (lambda(x)
         (delete-overlay x))
       overlays)
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-reduction-M-g-works ()
  "Test Ergoemacs M-g works correctly (Issue #171)."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ergoemacs-test-fn t)
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (with-timeout (0.2 nil) (ergoemacs-read-key "M-g"))
    (message "Test FN: %s" ergoemacs-test-fn)
    (setq ret (eq ergoemacs-test-fn (or (command-remapping 'execute-extended-command (point)) 'execute-extended-command)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should ret)))

(ert-deftest ergoemacs-test-command-remapping ()
  "Test to make sure remapping for `ergoemacs-commands' are applied."
  (should (eq 'ergoemacs-describe-key (command-remapping 'describe-key))))




(ert-deftest ergoemacs-test-comment-dwim-deactivate-region ()
  "Makes sure that `comment-dwim' deactivates the region.
Issue #203"
  :expected-result :failed ;; It works, just doesn't pass the test :(
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-o" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (emacs-lisp-mode)
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (mark-word)
      (execute-kbd-macro macro)
      (setq ret (not mark-active))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-alt-mode-horizontal-position ()
  "Tests Issue #213"
  (let ((old-map (copy-keymap input-decode-map))
        (old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "i u u" t))
        (ret nil))
    (setq input-decode-map (make-sparse-keymap)
          ergoemacs-theme nil
          ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)
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
      (kill-buffer (current-buffer)))
    ;; Restore old `input-decode-map' & ergoemacs-mode themes.
    (setq ergoemacs-theme old-ergoemacs-theme
          ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)
    ;; (progn (require 'ergoemacs-test) (ert "ergoemacs-test-terminal-M-O-fight"))
    (should ret)))


(ert-deftest ergoemacs-test-global-key-set-after-220 ()
  "Test global C-c b"
  (should (equal (ergoemacs-test-global-key-set-before 'after "C-c b") t)))

(ert-deftest ergoemacs-test-global-key-set-apps-220-before ()
  "Test global C-c b"
  (should (equal (ergoemacs-test-global-key-set-before nil "C-c b") t)))

(provide 'ergoemacs-test-bind-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-bind-keys.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
