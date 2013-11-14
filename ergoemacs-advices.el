;;; ergoemacs-advices.el --- advices for ErgoEmacs

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
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (let ((no-ergoemacs-advice t))
    (when (lookup-key ergoemacs-unbind-keymap key)
      (define-key ergoemacs-unbind-keymap key nil)
      (unless (string-match "^C-[xc]" (key-description key))
        (define-key ergoemacs-shortcut-keymap key nil))))
  (let ((x (assq 'ergoemacs-shortcut-keys ergoemacs-emulation-mode-map-alist)))
    (when x
      (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist)))
    (push (cons 'ergoemacs-shortcut-keys ergoemacs-shortcut-keymap) ergoemacs-emulation-mode-map-alist))
  (if (string-match "<\\(apps\\|menu\\)>" (key-description key))
      (let ((no-ergoemacs-advice t))
        (when command
          ;; Make prefixes possible
          (when (integerp (lookup-key ergoemacs-keymap key))
            (let ((key-as-vector (read-kbd-macro (key-description key) t))
                  (prefix-vector (make-vector (lookup-key ergoemacs-keymap key) nil))
                  (i 0))
              (while (< i (length prefix-vector))
                (aset prefix-vector i (elt key-as-vector i))
                (setq i (+ 1 i)))
              (define-key ergoemacs-keymap prefix-vector nil)))
          ;; Take care of prefix
          (when (lookup-key ergoemacs-keymap key)
            (define-key ergoemacs-keymap key nil))
          (when (lookup-key ergoemacs-shortcut-keymap key)
            (define-key ergoemacs-shortcut-keymap key nil))
          ;; commands.
          (define-key ergoemacs-keymap key command)))
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
        (condition-case err
            (define-key ergoemacs-keymap key nil)
          (error (ergoemacs-debug "Key %s not found in erogemacs-keymap (probably a shortcut).  Did not remove it from the map." (key-description key))))))))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-set-key-advice)

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use `global-unset-key' as usual when `ergoemacs-mode' is enabled."
  ;; the global-unset-key will remove the key from ergoemacs as well.
  ad-do-it
  (add-to-list 'ergoemacs-global-changed-cache (key-description key))
  (when ergoemacs-global-not-changed-cache
    (delete (key-description key) ergoemacs-global-not-changed-cache))
  (let ((no-ergoemacs-advice t))
    (define-key ergoemacs-keymap key nil)))

(add-to-list 'ergoemacs-advices 'ergoemacs-global-unset-key-advice)

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use `local-set-key' as usual when `ergoemacs-mode' is enabled."
  (if (and (fboundp 'ergoemacs-mode) ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-set-key-advice)

(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use `local-unset-key' as usual when `ergoemacs-mode' is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

(add-to-list 'ergoemacs-advices 'ergoemacs-local-unset-key-advice)

(eval-after-load "helm"
  '(progn
     ;; (defadvice helm-M-x (around ergoemacs-helm-M-x-keys)
;;        "Translates Helm M-x keys to ergoemacs style bindings."
;;        (flet ((helm-M-x-transformer
;;                (candidates sources)
;;                "filtered-candidate-transformer to show bindings in emacs commands.
;; Show global bindings and local bindings according to current `major-mode'."
;;                (with-helm-current-buffer
;;                  (loop with local-map = (helm-M-x-current-mode-map-alist)
;;                        for cand in candidates
;;                        for local-key  = (car (rassq cand local-map))
;;                        for key        = (substitute-command-keys (format "\\[%s]" cand))
;;                        collect
;;                        (cons (cond ((and (string-match "^M-x" key) local-key)
;;                                     (format "%s (%s)"
;;                                             cand (propertize
;;                                                   (if (and ergoemacs-use-ergoemacs-key-descriptions ergoemacs-mode)
;;                                                       (ergoemacs-pretty-key local-key)
;;                                                     local-key)
;;                                                   'face 'helm-M-x-key)))
;;                                    ((string-match "^M-x" key) cand)
;;                                    (t (format "%s (%s)"
;;                                               cand (propertize
;;                                                     (if (and ergoemacs-use-ergoemacs-key-descriptions ergoemacs-mode)
;;                                                         (ergoemacs-pretty-key key)
;;                                                       key)
;;                                                     'face 'helm-M-x-key))))
;;                              cand) into ls
;;                              finally return
;;                              (sort ls #'helm-command-M-x-sort-fn)))))
;;          ad-do-it))

     ;; (ad-activate 'helm-M-x)
     ))


(defadvice cua-mode (around ergoemacs-activate-only-selection-mode (arg))
  "When `ergoemacs-mode' is enabled, enable `cua-selection-mode' instead of plain `cua-mode'."
  (when ergoemacs-mode
    (setq-default cua-enable-cua-keys nil))
  ad-do-it
  (when ergoemacs-mode
    (customize-mark-as-set 'cua-enable-cua-keys)))

(ad-activate 'cua-mode)

(defadvice icicle-mode (around ergoemacs-icicle-play (arg))
  "Allow `ergoemacs-mode' to play nicely with `icicle-mode'."
  (let ((oee ergoemacs-mode))
    (when oee ;; Remove key bindings
      (ergoemacs-mode -1))
    ad-do-it
    (when oee ;; Add them back.  Now icy-mode should play nice.
      (ergoemacs-mode 1))))

(ad-activate 'icicle-mode)

(defadvice smex (around ergoemacs-change-text)
  (let ((smex-prompt-string smex-prompt-string))
    (when (and ergoemacs-mode ergoemacs-change-smex-meta-x)
      (setq smex-prompt-string
            (ergoemacs-pretty-key
             (key-description
              (this-single-command-keys)))))
    ad-do-it))

(ad-activate 'smex)


(provide 'ergoemacs-advices)
;;;;;;;;;;;;;;;;;;;;;;;;`';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advices.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
