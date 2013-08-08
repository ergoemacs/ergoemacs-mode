;;; ergoemacs-advices.el.el --- advices for ErgoEmacs

;; Copyright (C) 2013 Matthew L. Fidler

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

(defadvice define-key (around ergoemacs-define-key-advice (keymap key def))
  "This does the right thing when modifying `ergoemacs-keymap'."
  (if (and (equal keymap 'ergoemacs-keymap)
           (or (not (boundp 'no-ergoemacs-advice))
               (and (boundp 'no-ergoemacs-advice) (not no-ergoemacs-advice))))
      (progn
        (message "Define Key Advice %s %s" key def)
        (let ((found))
          (set (ergoemacs-get-fixed-layout)
               (mapcar
                (lambda(x)
                  (if (not (or (and (type-of (nth 0 x) 'string)
                                    (string= (key-description
                                              (condition-case err
                                                  (read-kbd-macro (nth 0 x))
                                                (error
                                                 (read-kbd-macro (encode-coding-string (nth 0 x) locale-coding-system)))))
                                             (key-description key)))
                               (and (not (type-of (nth 0 x) 'string))
                                    (string= (key-description (nth 0 x)) (key-description key)))))
                      x
                    (setq found t)
                    `(,(nth 0 x) ,command "")))
                (symbol-value (ergoemacs-get-fixed-layout))))
          (unless found
            (set (ergoemacs-get-variable-layout)
                 (mapcar
                  (lambda(x)
                    (if (not (and (type-of (nth 0 x) 'string)
                                  (string= (key-description (ergoemacs-kbd (nth 0 x) nil (nth 3 x))) (key-description key))))
                        x
                      (setq found t)
                      ;; Assume the complete sequence is translated?
                      `(,(nth 0 x) ,command "")))
                  (symbol-value (ergoemacs-get-variable-layout)))))
          (unless found
            (add-to-list (ergoemacs-get-fixed-layout) `(,(key-description key) ,command ""))))
        (message "Only changed ergoemacs-keybinding for current theme, %s" (or ergoemacs-theme "which happens to be the default key-binding"))
        (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
          (let ((no-ergoemacs-advice t))
            (define-key ergoemacs-keymap key def))))
    ad-do-it))
(ad-activate 'define-key)


(defadvice cua--pre-command-handler (around ergoemacs-fix-shifted-commands activate)
  "Fixes shifted movement problems."
  (let ((do-it t)
        (case-fold-search nil)
        (send-timeout nil))
    (condition-case nil
        (progn
          ;; Fix shifted commands.
          (when (and (string-match "\\(^\\|-\\)M-" (key-descrtion (this-single-command-keys))) ;; Alt command
                     (or (eq (get this-command 'CUA) 'move)
                         (memq this-command ergoemacs-movement-functions)))
            (setq do-it nil))
          ;; Fix Issue 139.  However may introduce an issue when you
          ;; want to issue C-c commands quickly...
          (when (and mark-active (string-match "^C-\\(c\\|x\\)" (key-description (this-single-command-keys))))
            (setq do-it t)
            (setq send-timeout t)))
      (error nil))
    (when cua--rectangle
      (setq do-it t))
    (when do-it
      ad-do-it)
    (when send-timeout
      (setq unread-command-events
            (cons 'timeout unread-command-events)))))

;;; Advices enabled or disabled with ergoemacs-mode


(defadvice global-set-key (around ergoemacs-global-set-key-advice (key command))
  "This let you use `global-set-key' as usual when `ergoemacs-mode' is enabled."
  ad-do-it
  (add-to-list 'ergoemacs-do-not-restore-list (key-description key))
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (if (string-match "<\\(apps\\|menu\\)>" (key-description key))
      (let ((no-ergoemacs-advice t))
        (define-key ergoemacs-keymap key nil);; Take care of prefix
        ;; commands.
        (define-key ergoemacs-keymap key command))
    (if (and ergoemacs-fix-M-O
             (string= "M-O" (key-description key)))
        (let ((no-ergoemacs-advice t))
          (define-key ergoemacs-keymap key 'ergoemacs-M-O)
          (define-key ergoemacs-M-O-keymap [timeout] command))
      (if (and ergoemacs-fix-M-O
               (string= "M-o" (key-description key)))
          (let ((no-ergoemacs-advice t))
            (define-key ergoemacs-keymap key 'ergoemacs-M-o)
            (define-key ergoemacs-M-o-keymap [timeout] command)))
      (let ((no-ergoemacs-advice t))
        (define-key ergoemacs-keymap key nil)))))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-set-key-advice)

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use `global-unset-key' as usual when `ergoemacs-mode' is enabled."
  ;; the global-unset-key will remove the key from ergoemacs as well.
  ad-do-it
  (add-to-list 'ergoemacs-do-not-restore-list (key-description key))
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (let ((no-ergoemacs-advice t))
    (define-key ergoemacs-keymap key nil)))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-unset-key-advice)

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use `local-set-key' as usual when `ergoemacs-mode' is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-set-key-advice)

(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use `local-unset-key' as usual when `ergoemacs-mode' is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-unset-key-advice)


;; I don't think this works...
(defadvice helm-persistent-help-string (around ergoemacs-helm-persistent-help-string-advice activate)
  "Formats the help string by pretty printing it."
  (let ((ret ad-do-it))
    (when ergoemacs-mode
      (setq ret (ergoemacs-pretty-key-rep ret)))
    (symbol-value 'ret)))

(ad-activate 'helm-persistent-help-string)





(provide 'ergoemacs-advices)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advices.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
