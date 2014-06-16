;;; ergoemacs-test-selection.el --- tests for ErgoEmacs Key binding issues

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

(ert-deftest ergoemacs-test-shifted-move-no-mark ()
  "Tests another shifted selection bug."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-H" t))
        (ret t))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert ";;")
      (execute-kbd-macro macro)
      (setq ret (not mark-active)) ;;  Shouldn't be selected
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shifted-move-keep-mark ()
  "Test the shifted selection bug."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-SPC M-h M-I" t))
        (ret))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (cua-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test-shifted-move*"))
      (delete-region (point-min) (point-max))
      (insert ";;;;")
      (goto-char (point-min))
      (execute-kbd-macro macro)
      (setq ret mark-active) ;; Should be selected.
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-modal-preserve-mark ()
  "Issue #101.
Presevre mark with modal translations."
  (with-temp-buffer
    (insert ergoemacs-test-lorem-ipsum)
    (goto-char (point-min))
    (ergoemacs-unchorded-alt-modal)
    (set-mark (point))
    (forward-char 3)
    (ergoemacs-unchorded-alt-modal)
    (should mark-active)))

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

(ert-deftest ergoemacs-test-misspelled-mark ()
  "Test for mark working with overlays.
Should test issue #142"
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-SPC M-y M-x" t))
        (ret nil)
        tmp (tmp-key (make-sparse-keymap))
        overlays)
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
      (goto-char (point-max))
      (beginning-of-line)
      (execute-kbd-macro macro)
      (when (looking-at " in culpa qui")
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

(ert-deftest ergoemacs-test-shift-select-subword ()
  "Test for mark working with shift-selection of `subword-forward'."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (macro (edmacro-parse-keys "M-Y M-x" t))
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (subword-mode 1)
      (goto-char (point-max))
      (beginning-of-line)
      (execute-kbd-macro macro)
      (when (looking-at " in culpa qui")
        (setq ret t))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(ert-deftest ergoemacs-test-shift-selection-reduction ()
  "Test that shift selection works properly in reduction."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (save-excursion
      (switch-to-buffer (get-buffer-create "*ergoemacs-test*"))
      (insert ergoemacs-test-lorem-ipsum)
      (goto-char (point-min))
      (execute-kbd-macro (edmacro-parse-keys "M-E M-E" t))
      (call-interactively 'ergoemacs-cut-line-or-region)
      (setq ret (= (point) (point-min)))
      (kill-buffer (current-buffer)))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))

(provide 'ergoemacs-test-selection)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-selection.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
