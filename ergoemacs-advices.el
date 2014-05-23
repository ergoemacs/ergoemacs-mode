;;; ergoemacs-advices.el --- advices for ErgoEmacs

;; Copyright (C) 2013, 2014  Free Software Foundation, Inc.

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
(defvar ergoemacs-advices '()
  "List of advices to enable and disable when ergoemacs is running.")

(defvar ergoemacs-dir
  (file-name-directory
   (or
    load-file-name
    (buffer-file-name)))
  "Ergoemacs directory.")
(add-to-list 'load-path ergoemacs-dir)
(require 'ergoemacs-shortcuts)
(require 'ergoemacs-unbind)

(defmacro ergoemacs-define-overrides (&rest body)
  "Force the define-keys to work"
  `(let ((ergoemacs-run-mode-hooks t))
     ,@body))

(defadvice add-hook (around ergoemacs-add-hook-advice (hook function &optional append  local) activate)
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'
If `pre-command-hook' is used and `ergoemacs-mode' is enabled add to `ergoemacs-pre-command-hook' instead."
  (cond
   ((and (boundp 'ergoemacs-mode)
         ergoemacs-mode (eq hook 'pre-command-hook)
         (boundp 'ergoemacs-hook-functions)
         (memq hook ergoemacs-hook-functions))
    (add-hook 'ergoemacs-pre-command-hook function append local))
   (t
    ad-do-it)))

(defadvice remove-hook (around ergoemacs-remove-hook-advice (hook function &optional local) activate)
  "Advice to allow `this-command' to be set correctly before running `pre-command-hook'.
If `pre-command-hook' is used and `ergoemacs-mode' is remove from `ergoemacs-pre-command-hook' instead."
  (cond
   ((and (boundp 'ergoemacs-mode)
         ergoemacs-mode (eq hook 'pre-command-hook)
         (boundp 'ergoemacs-hook-functions)
         (memq hook ergoemacs-hook-functions))
    (remove-hook 'ergoemacs-pre-command-hook function local))
   (t
    ad-do-it)))

(defadvice define-key (around ergoemacs-define-key-advice (keymap key def) activate)
  "This does the right thing when modifying `ergoemacs-keymap'.
Also adds keymap-flag for user-defined keys run with `run-mode-hooks'."
  (let ((is-global-p (equal keymap (current-global-map))))
    (if (and (boundp 'ergoemacs-run-mode-hooks) ergoemacs-run-mode-hooks
             (not (equal keymap (current-global-map)))
             (not (equal keymap ergoemacs-keymap)))
        (let ((ergoemacs-run-mode-hooks nil)
              (new-key (read-kbd-macro
                        (format "<ergoemacs-user> %s"
                                (key-description key)))))
          (define-key keymap new-key def)))
    ad-do-it
    (when is-global-p
      (let ((vk key))
        (ergoemacs-global-set-key-after key)
        (unless (vectorp vk) ;; Do vector def too.
          (setq vk (read-kbd-macro (key-description key) t))
          (ergoemacs-global-set-key-after vk))))))

(defvar ergoemacs-global-override-rm-keys '())
;;; Advices enabled or disabled with ergoemacs-mode
(defvar ergoemacs-ignore-advice nil)
(defun ergoemacs-global-set-key-after (key)
  (if ergoemacs-ignore-advice nil
    (unless (or (and (vectorp key)
                     (memq (elt key 0) '(menu-bar 27 remap)))
                (and (not (vectorp key))
                     (string= "ESC" (key-description key))))
      (let ((ergoemacs-ignore-advice t))
        (add-to-list 'ergoemacs-global-changed-cache (key-description key))
        (when ergoemacs-global-not-changed-cache
          (delete (key-description key) ergoemacs-global-not-changed-cache))
        (add-to-list 'ergoemacs-global-override-rm-keys key)
        (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
          (ergoemacs-theme-remove-key-list (list key) t))))))

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command) activate)
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


(defadvice cua-mode (around ergoemacs-activate-only-selection-mode (arg) activate)
  "When `ergoemacs-mode' is enabled, enable `cua-selection-mode' instead of plain `cua-mode'."
  (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
    (setq-default cua-enable-cua-keys nil))
  ad-do-it
  (when (and (boundp 'ergoemacs-mode) ergoemacs-mode)
    (customize-mark-as-set 'cua-enable-cua-keys)))

(defadvice icicle-mode (around ergoemacs-icicle-play (arg) activate)
  "Allow `ergoemacs-mode' to play nicely with `icicle-mode'."
  (let ((oee (and (boundp 'ergoemacs-mode) ergoemacs-mode)))
    (when oee ;; Remove key bindings
      (ergoemacs-mode -1))
    ad-do-it
    (when oee ;; Add them back.  Now icy-mode should play nice.
      (ergoemacs-mode 1))))

(defcustom ergoemacs-helm-expand-user-dirs 't
  "Expand user directories under helm.
This makes helm behave more like `ido-find-file'"
  :group 'ergoemacs-mode
  :type 'boolean)

(eval-after-load "helm-files"
  '(progn
     (defadvice helm-ff-auto-expand-to-home-or-root (around ergoemacs-helm-ido-user-dirs activate)
      "Allow `helm-find-files' to expand user directories.
For example ~ergoemacs/ would expand to /usr/ergoemacs or
whatever that points to...

This require `ergoemacs-mode' to be enabled as well as
`ergoemacs-helm-expand-user-dirs' to be true.
"
      (cond
       ((and ergoemacs-helm-expand-user-dirs
             (boundp 'ergoemacs-mode)
             ergoemacs-mode
             (helm-file-completion-source-p)
             (string-match "/\\(~[^/]*/\\)$" helm-pattern)
             (with-current-buffer (window-buffer (minibuffer-window)) (eolp))
             (not (string-match helm-ff-url-regexp helm-pattern)))
        (let ((input (match-string 1 helm-pattern)))
          (if (file-directory-p input)
              (setq helm-ff-default-directory
                    (setq input (file-name-as-directory input)))
            (setq helm-ff-default-directory (file-name-as-directory
                                             (file-name-directory input))))
          (with-helm-window
            (helm-set-pattern input)
            (helm-check-minibuffer-input))))
       (t
        ad-do-it)))))


(defadvice run-mode-hooks (around ergoemacs-run-hooks activate)
  "`ergoemacs-mode' run-hooks advice helps user define keys properly.
This assumes any key defined while running a hook is a user-defined hook."
  (let ((ergoemacs-run-mode-hooks t))
    ad-do-it))

(defadvice turn-on-undo-tree-mode (around ergoemacs-undo-tree-mode activate)
  "Make `ergoemacs-mode' and undo-tree compatible."
  (ergoemacs-with-global
   ad-do-it))




(provide 'ergoemacs-advices)
;;;;;;;;;;;;;;;;;;;;;;;;`';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-advices.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
