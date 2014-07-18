;;; ergoemacs-modal.el --- Modal Editing commands -*- lexical-binding: t -*-

;; Copyright © 2013-2014  Free Software Foundation, Inc.

;; Filename: ergoemacs-modal.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:03:23 2013 (-0500)
;; Version:
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL:
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar ergoemacs-handle-ctl-c-or-ctl-x)
(defgroup ergoemacs-modal nil
  "Modal ergoemacs"
  :group 'ergoemacs-mode)

(defcustom ergoemacs-modal-ignored-buffers
  '("^ \\*load\\*" "^[*]e?shell[*]" "^[*]R.*[*]$")
  "Regular expression of bufferst that should come up in
ErgoEmacs state, regardless of if a modal state is currently
enabled."
  :type '(repeat string)
  :group 'ergoemacs-modal)

(defvar ergoemacs-default-cursor nil
  "The default cursor color.
This should be reset every time that the modal cursor changes color.  Otherwise this will be nil
A color string as passed to `set-cursor-color'.")

(defcustom ergoemacs-modal-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-diff-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-reflog-mode
    magit-show-branches-mode
    magit-branch-manager-mode ;; New name for magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    magit-wazzup-mode
    mh-folder-mode
    monky-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    shell-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in ErgoEmacs state, regardless of if a
modal state is currently enabled."
  :type  '(repeat symbol)
  :group 'ergoemacs-modal)

(defvar ergoemacs-modal-list '())
(defvar ergoemacs-translations)
(defvar ergoemacs-modal-ignored-keymap
  (let ((ret (make-sparse-keymap)))
    (dolist (char '("<f1>" 
                    "<f2>" 
                    "<f3>" 
                    "<f4>" 
                    "<f5>" 
                    "<f6>" 
                    "<f7>" 
                    "<f8>" 
                    "<f9>" 
                    "<f10>"
                    "<f11>"
                    "<f12>"
                    "<apps>" "<menu>"
                    "RET" "ESC" "DEL" "TAB"
                    "<home>" 
                    "<next>" 
                    "<prior>"
                    "<end>"
                    "<insert>"
                    "<deletechar>"))
      (dolist (mod '("" "C-" "C-S-" "M-" "M-S-" "C-M-" "C-M-S-"))
        (let ((key (read-kbd-macro (concat mod char))))
          (unless (lookup-key ret key)
            (define-key ret key 'ergoemacs-modal-default)))))
    ret)
  "`ergoemacs-mode' keys to ignore the modal translation.
Typically function keys")

(defvar ergoemacs-single-command-keys)
(defun ergoemacs-modal-p ()
  "Determine if the command should be modal.
If so return the hash of translation values."
  (if (not ergoemacs-modal-list) nil
    (let* ((type (nth 0 ergoemacs-modal-list))
           (hash (gethash type ergoemacs-translations))
           (always (plist-get hash ':modal-always))
           (ret hash))
      (cond
       ((and (minibufferp)
             (not always))
        (setq ret nil))
       ((and (not always)
             (memq major-mode ergoemacs-modal-emacs-state-modes))
        (setq ret nil))
       ((and (not always)
             (catch 'match-modal
               (dolist (reg ergoemacs-modal-ignored-buffers)
                 (when (string-match reg (buffer-name))
                   (throw 'match-modal t)))
               nil))
        (setq ret nil))
       ((and (not always)
             (lookup-key ergoemacs-modal-ignored-keymap
                         (or ergoemacs-single-command-keys (this-single-command-keys))))
        (setq ret nil)))
      ret)))

(defvar ergoemacs-translation-text)
(declare-function ergoemacs-read-key "ergoemacs-shortcuts.el")
(declare-function ergoemacs-mode-line "ergoemacs-mode.el")

(defun ergoemacs-modal--internal ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
the translation type defined by `ergoemacs-modal-list' as long as it should."
  (let* ((type (nth 0 ergoemacs-modal-list))
         (hash (gethash type ergoemacs-translations))
         tmp)
    (when (not (ergoemacs-modal-p))
      (setq type nil))
    ;; Actual call
    (ergoemacs-read-key
     (or ergoemacs-single-command-keys (this-single-command-keys))
     type
     type)
    ;; Fix cursor color and mode-line
    (save-excursion
      (let (deactivate-mark)
        (cond
         ((ergoemacs-modal-p)
          (setq tmp (plist-get hash ':modal-color))
          (if tmp
              (set-cursor-color tmp)
            (when ergoemacs-default-cursor
              (set-cursor-color ergoemacs-default-cursor)))
          (setq tmp (if ergoemacs-modal-list (gethash (nth 0 ergoemacs-modal-list) ergoemacs-translation-text) nil))
          (if tmp
              (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
               (concat " " (nth 5 tmp)))
            (ergoemacs-mode-line)))
         (t
          (when ergoemacs-default-cursor
            (set-cursor-color ergoemacs-default-cursor))
          (ergoemacs-mode-line)))))))

(defun ergoemacs-modal-default ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
the translation type defined by `ergoemacs-modal-list' as long as it should."
  (interactive)
  (ergoemacs-modal--internal))

(defun ergoemacs-modal-movement ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
the translation type defined by `ergoemacs-modal-list' as long as it should."
  (interactive "^")
  (ergoemacs-modal--internal))
(put 'ergoemacs-modal-movement 'CUA 'move) ;; Fake movement command


(defun ergoemacs-modal-movement-no-shift-select ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with
the translation type defined by `ergoemacs-modal-list' as long as it should."
  (interactive)
  (ergoemacs-modal--internal))

(defvar ergoemacs-modal-save nil)
(defvar ergoemacs-modal nil
  "If ergoemacs modal and what translation is active.")

(defvar ergoemacs-modal-keymap nil
  "`ergoemacs-mode' modal keymap.  Attempts to capture ALL keystrokes.")

(defvar ergoemacs-modal-base-keymap nil
  "`ergoemacs-mode' modal keymap.  Attempts to capture ALL keystrokes.")

(declare-function ergoemacs-translate-shifted "ergoemacs-translate.el")
(declare-function ergoemacs-get-layouts "ergoemacs-layouts.el")
(declare-function ergoemacs-local-map "ergoemacs-translate.el")
(defun ergoemacs-modal-base-keymap  (&optional map)
  "Returns the ergoemacs-modal keymap"
  (if ergoemacs-modal-base-keymap
      (if map
          (make-composed-keymap (list map ergoemacs-modal-base-keymap))
        ergoemacs-modal-base-keymap)
    (let ((ret (make-sparse-keymap)))
      (dolist (lay (ergoemacs-get-layouts))
        (dolist (char (symbol-value (intern (concat "ergoemacs-layout-" lay))))
          (unless (string= char "")
            (dolist (mod '("" "C-" "M-" "C-M-"))
              (let ((key (read-kbd-macro
                          (ergoemacs-translate-shifted
                           (concat mod char)))))
                (unless (lookup-key ret key)
                  (define-key ret key 'ergoemacs-modal-default)))))))
      (dolist (char '("<f1>" 
                      "<f2>" 
                      "<f3>" 
                      "<f4>" 
                      "<f5>" 
                      "<f6>" 
                      "<f7>" 
                      "<f8>" 
                      "<f9>" 
                      "<f10>"
                      "<f11>"
                      "<f12>"
                      "<apps>" "<menu>"
                      "SPC" "RET" "ESC" "DEL" "TAB"
                      "<home>" 
                      "<next>" 
                      "<prior>"
                      "<end>"
                      "<insert>"
                      "<deletechar>"))
        (dolist (mod '("" "C-" "C-S-" "M-" "M-S-" "C-M-" "C-M-S-"))
          (let ((key (read-kbd-macro (concat mod char))))
            (unless (lookup-key ret key)
              (define-key ret key 'ergoemacs-modal-default)))))
      (setq ergoemacs-modal-base-keymap ret))
    (ergoemacs-modal-base-keymap map)))

(defvar ergoemacs-modal-emulation-mode-map-alist)
(defvar ergoemacs-ignore-advice)
(declare-function ergoemacs-flatten-composed-keymap "ergoemacs-mode.el")
(defun ergoemacs-modal-toggle (type)
  "Toggle ergoemacs command modes."
  (let* ((help-list (gethash type ergoemacs-translation-text))
         (type type)
         tmp
         (ergoemacs-ignore-advice t))
    (cond
     ((or (not ergoemacs-modal-list) ;; First time to turn on
          (not (eq (nth 0 ergoemacs-modal-list) type)) ;; New modal 
          )
      (push type ergoemacs-modal-list)
      (setq ergoemacs-modal-keymap
            (make-composed-keymap
             (list (ergoemacs-local-map type t)
                   (ergoemacs-modal-base-keymap))))
      (setq ergoemacs-modal-emulation-mode-map-alist
            `((ergoemacs-modal ,@ergoemacs-modal-keymap)))
      (set-default 'ergoemacs-modal type)
      (setq ergoemacs-modal type)
      (unless ergoemacs-default-cursor
        (setq ergoemacs-default-cursor
              (or (frame-parameter nil 'cursor-color) "black")))
      (let ((hash (gethash type ergoemacs-translations))
            tmp)
        (when hash
          (setq tmp (plist-get hash ':modal-color))
          (when tmp
            (set-cursor-color tmp))))
      (if help-list
          (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
           (concat " " (nth 5 help-list)))
        (ergoemacs-mode-line))
      (let (message-log-max)
        (if help-list
            (message "%s command mode installed" (nth 5 help-list)))))
     (t ;; Turn off.
      (setq tmp (pop ergoemacs-modal-list))
      (when (eq tmp type)
        (if (not ergoemacs-modal-list)
            (setq type nil)
          (setq type (nth 0 ergoemacs-modal-list))))
      (if type
          (progn ;; Turn off current modal, turn on last modal.
            (setq help-list (gethash type ergoemacs-translation-text))
            (setq ergoemacs-modal-keymap
                  (ergoemacs-flatten-composed-keymap
                   (make-composed-keymap
                    (list (ergoemacs-local-map type t)
                          (ergoemacs-modal-base-keymap)))))
            (setq ergoemacs-modal-emulation-mode-map-alist
                  `((ergoemacs-modal ,@ergoemacs-modal-keymap)))
            (set-default 'ergoemacs-modal type)
            (setq ergoemacs-modal type)
            (unless ergoemacs-default-cursor
              (setq ergoemacs-default-cursor
                    (or (frame-parameter nil 'cursor-color) "black")))
            (let ((hash (gethash type ergoemacs-translations))
                  tmp)
              (when hash
                (setq tmp (plist-get hash ':modal-color))
                (when tmp
                  (set-cursor-color tmp))))
            (if help-list
                (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
                 (concat " " (nth 5 help-list)))
              (ergoemacs-mode-line))
            (let (message-log-max)
              (if help-list
                  (message "%s command mode resumed." (nth 5 help-list)))))
        ;; Turn of ergoemacs-modal
        (when ergoemacs-default-cursor
          (set-cursor-color ergoemacs-default-cursor))
        (let (message-log-max)
          (message "Full %s command mode removed." (if help-list (nth 5 help-list) "")))
        (set-default 'ergoemacs-modal nil)
        (setq ergoemacs-modal nil)
        (setq ergoemacs-modal-save nil)
        (ergoemacs-mode-line))))))

(provide 'ergoemacs-modal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-modal.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
