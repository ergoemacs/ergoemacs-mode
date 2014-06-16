;;; ergoemacs-test-translations.el --- tests for ErgoEmacs translations

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

(ert-deftest ergoemacs-test-helm-M-x ()
  "Issue #65.  helm-M-x should not be helm-[Alt+X]."
  (let (ergoemacs-use-unicode-char)
    (should (string= (ergoemacs-pretty-key-rep "helm-M-x test") "helm-M-x test"))))

(ert-deftest ergoemacs-test-ctl-c-ctl-c ()
  "Issue #64.  Should translate C-c C-c correctly."
  (let (ergoemacs-use-unicode-char)
    (should (string= (ergoemacs-pretty-key-rep "C-c C-c") "[Ctl+C] [Ctl+C]"))))

(ert-deftest ergoemacs-test-next-and-prior-translation ()
  "Issue #70.
Test next and prior translation."
  (should (string= (ergoemacs-pretty-key-rep "Test next and prior translation")
                   "Test next and prior translation")))

(ert-deftest ergoemacs-test-issue-77 ()
  "Issue #77.
Test \"C-x \" translating to \"[Ctl+X][]\", should be \"[Ctl+X]\""
  (let ((ergoemacs-use-unicode-char nil))
    (should (string= (ergoemacs-pretty-key "C-x ") "[Ctl+X]"))))

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

(ert-deftest ergoemacs-test-apps-e-t-_ ()
  "Test that colemak <apps> e t sends _.
Should test for Issue #143."
  (let ((old-ergoemacs-theme ergoemacs-theme)
        (old-ergoemacs-keyboard-layout ergoemacs-keyboard-layout)
        unread-command-events
        (ret nil))
    (ergoemacs-mode -1)
    (setq ergoemacs-theme "reduction")
    (setq ergoemacs-keyboard-layout "colemak")
    (ergoemacs-mode 1)
    (ergoemacs-read-key (format "<%s> e t"
                                (if (eq system-type 'windows-nt)
                                    "apps" "menu")))
    (setq ret (equal (listify-key-sequence (read-kbd-macro "_"))
                     unread-command-events))
    (message "%s;%s" (listify-key-sequence (read-kbd-macro "_"))
             unread-command-events)
    (ergoemacs-mode -1)
    (setq ergoemacs-theme old-ergoemacs-theme)
    (setq ergoemacs-keyboard-layout old-ergoemacs-keyboard-layout)
    (ergoemacs-mode 1)
    (should (equal ret t))))



(provide 'ergoemacs-test-translations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-test-translations.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
