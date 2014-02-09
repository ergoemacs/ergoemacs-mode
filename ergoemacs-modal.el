;;; ergoemacs-modal.el --- Modal Editing commands
;; 
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

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

(defvar ergoemacs-exit-temp-map-var nil)

(defun ergoemacs-minibuffer-setup ()
  "Exit temporary overlay maps."
  ;; (setq ergoemacs-exit-temp-map-var t)
  (set (make-local-variable 'ergoemacs-modal) nil)
  (when (and ergoemacs-modal-list
             (let ((hash (gethash (nth 0 ergoemacs-modal-list) ergoemacs-translations)))
               (and hash
                    (plist-get hash ':modal-always))))
    (setq ergoemacs-modal (nth 0 ergoemacs-modal-list)))
  (ergoemacs-debug-heading "ergoemacs-minibuffer-setup")
  (ergoemacs-debug "emulation-mode-map-alists: %s" emulation-mode-map-alists)
  (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    ergoemacs-emulation-mode-map-alist))
  (ergoemacs-debug "minor-mode-map-alist: %s"
                   (mapcar
                    (lambda(x) (nth 0 x))
                    minor-mode-map-alist))
  ;; (setq ergoemacs-shortcut-keys t)
  (ergoemacs-debug "ergoemacs-shortcut-keys: %s" ergoemacs-shortcut-keys)
  (ergoemacs-debug "ergoemacs-shortcut-override-mode: %s" ergoemacs-shortcut-override-mode)
  (ergoemacs-debug "ergoemacs-mode: %s" ergoemacs-mode)
  (ergoemacs-debug "ergoemacs-unbind-keys: %s" ergoemacs-unbind-keys))

(defun ergoemacs-modal-default ()
  "The default command for `ergoemacs-mode' modal.
It sends `this-single-command-keys' to `ergoemacs-read-key' with the translation type defined by `ergoemacs-modal'"
  (interactive)
  (ergoemacs-read-key
   (or ergoemacs-single-command-keys (this-single-command-keys))
   (nth 0 ergoemacs-modal-list)
   (nth 0 ergoemacs-modal-list)))

(defvar ergoemacs-modal-save nil)
(defvar ergoemacs-modal nil
  "If ergoemacs modal and what translation is active.")

(defvar ergoemacs-modal-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'ergoemacs-modal-default)
    map))

(defun ergoemacs-modal-mouse-keymap ()
  "Returns the mouse-bindings keymap"
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (other)
       (mapc
        (lambda(mod)
          (mapc
           (lambda(event)
             (mapc
              (lambda(num)
                (let* ((key (read-kbd-macro (concat other "<" mod event "mouse-" num ">")))
                       (def (key-binding key)))
                  (when def
                    (define-key map key def))))
              '("1" "2" "3" "4" "5" "6" "7")))
           '("" "drag-" "down-" "double-" "triple-")))
        '("" "C-" "C-S-" "M-" "M-S-" "C-M-" "C-M-S-" "S-")))
     '("" "<mode-line>" "<header-line> " "<left-fringe> " "<right-fringe> " "<vertical-line> " "<vertical-scroll-bar> "))
    (symbol-value 'map)))

(defvar ergoemacs-modal-list '())
(defun ergoemacs-modal-toggle (type)
  "Toggle ergoemacs command modes."
  (let* ((x (assq 'ergoemacs-modal ergoemacs-emulation-mode-map-alist))
         (help-list (gethash type ergoemacs-translation-text))
         keymap
         (type type)
         tmp
         (no-ergoemacs-advice t))
    (setq ergoemacs-emulation-mode-map-alist
          (delq x ergoemacs-emulation-mode-map-alist))
    (cond
     ((or (not ergoemacs-modal-list) ;; First time to turn on
          (not (eq (nth 0 ergoemacs-modal-list) type)) ;; New modal 
          )
      (push type ergoemacs-modal-list)
      (setq keymap (make-composed-keymap
                    (list (ergoemacs-modal-mouse-keymap)
                          (ergoemacs-local-map type t)
                          ergoemacs-modal-keymap)))
      (push (cons 'ergoemacs-modal keymap)
            ergoemacs-emulation-mode-map-alist)
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
            (setq keymap
                  (make-composed-keymap
                   (list (ergoemacs-modal-mouse-keymap)
                         (ergoemacs-local-map type t)
                         ergoemacs-modal-keymap)))
            (push (cons 'ergoemacs-modal keymap)
                  ergoemacs-emulation-mode-map-alist)
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
