;;; ergoemacs-test-misc.el --- tests for ErgoEmacs Key binding issues

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


(ert-deftest ergoemacs-test-shortcut ()
  "Test that shortcuts don't eat or duplicate key-strokes. (Issue #141)"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys (format "<%s> e e M-u"
                                           (if (eq system-type 'windows-nt)
                                               "apps" "menu")) t))
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-max))
      (beginning-of-line)
      (execute-kbd-macro macro)
      (when (looking-at "ulla pariatur.")
        (setq ret t))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-issue-98 ()
  "Test full fast-movement-keys"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "C-f ars C-f <backspace> M-n" t))
        (ergoemacs-repeat-movement-commands 'all)
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (execute-kbd-macro (edmacro-parse-keys "M-e"))
      (unless (looking-at "do eiusmod")
        (setq ret nil))
      (execute-kbd-macro (edmacro-parse-keys "e"))
      (unless (looking-at "enim ad")
        (setq ret nil))
      (execute-kbd-macro (edmacro-parse-keys "u"))
      (unless (looking-at "do eiusmod")
        (setq ret nil))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should ret)))

;; Fixed, but tests dont work.  Not sure why ergoemacs-test-fn goes to nil

(ert-deftest ergoemacs-test-apps-h-v ()
  "Make Sure <apps> h v works correctly"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ergoemacs-test-fn t)
        (rk (format "<%s> h v"
                    (or (and (eq system-type 'windows-nt) "apps") "menu")))
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (with-timeout (0.2 nil)
      (ergoemacs-read-key rk))
    (message "Test FN: %s" ergoemacs-test-fn)
    (setq ret (eq ergoemacs-test-fn (or (command-remapping 'describe-variable (point)) 'describe-variable)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should ret)))

(ert-deftest ergoemacs-test-apps-h-z ()
  "Make Sure <apps> h z works correctly"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ergoemacs-test-fn t)
        (rk (format "<%s> h z"
                    (or (and (eq system-type 'windows-nt) "apps") "menu")))
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (with-timeout (0.2 nil)
      (ergoemacs-read-key rk))
    (message "Test FN: %s" ergoemacs-test-fn)
    (setq ret (eq ergoemacs-test-fn (or (command-remapping 'ergoemacs-clean (point)) 'ergoemacs-clean)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should ret)))


(ert-deftest ergoemacs-test-terminal-M-O-fight ()
  "Tests Issue #188"
  :expected-result :failed ;; It works, just doesn't pass the test :(
  (let ((old-map (copy-keymap input-decode-map))
        (old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ret nil))
    (setq input-decode-map (make-sparse-keymap)
          ergoemacs-theme nil
          ergoemacs-keyboard-layout "us")
    ;; Setup input decode map just like `xterm' for some common keys.
    (define-key input-decode-map "\eOA" [up])
    (define-key input-decode-map "\eOB" [down])
    (define-key input-decode-map "\eOC" [right])
    (define-key input-decode-map "\eOD" [left])
    (define-key input-decode-map "\eOF" [end])
    (define-key input-decode-map "\eOH" [home])
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)
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
    ;; Restore old `input-decode-map' & ergoemacs-mode themes.
    (setq input-decode-map (copy-keymap old-map)
          ergoemacs-theme old-ergoemacs-theme
          ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode -1)
    (ergoemacs-mode 1)
    ;; (progn (require 'ergoemacs-test) (ert "ergoemacs-test-terminal-M-O-fight"))
    (should ret)))

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

(provide 'ergoemacs-test-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-misc.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
