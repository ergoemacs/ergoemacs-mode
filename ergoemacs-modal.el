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
(defvar ergoemacs-full-fast-keys-keymap (make-sparse-keymap)
  "Ergoemacs full fast keys keymap")

(defvar ergoemacs-full-alt-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+ keymap.  Alt is removed from all these keys so that no key chord is necessary.")

(defvar ergoemacs-full-alt-shift-keymap (make-sparse-keymap)
  "Ergoemacs full Alt+Shift+ keymap.
Alt+shift is removed from all these keys so that no key chord is
necessary.  Unshifted keys are changed to shifted keys.")

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

(defcustom ergoemacs-modal-cursor ;; Adapted from evil-mode
  "red"
  "The default cursor.
A color string as passed to `set-cursor-color'."
  :type 'string
  :group 'ergoemacs-modal)

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

(defcustom ergoemacs-modal-translated-shifted-to-alt-commands nil
  "Translate Shifted Commands to Alt+ commands.  For example, on QWERTY ! becomes M-1."
  :type 'boolean
  :group 'ergoemacs-modal)

(defun ergoemacs-setup-fast-keys ()
  "Setup an array listing the fast keys."
  (interactive)
  (ergoemacs-debug-heading "Start ergoemacs-setup-fast-keys")
  (ergoemacs-create-undo-apps-keymap)
  (setq ergoemacs-full-fast-keys-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-keymap (make-sparse-keymap))
  (setq ergoemacs-full-alt-shift-keymap (make-sparse-keymap))
  (define-key ergoemacs-full-alt-keymap
    (read-kbd-macro "RET")
    'ergoemacs-toggle-full-alt)
  (define-key ergoemacs-full-alt-shift-keymap
    (read-kbd-macro "RET")
    'ergoemacs-toggle-full-alt-shift)
  (ergoemacs-debug-heading "Setup Fast/Modal Keys")
  (mapc
   (lambda(var)
     (let* ((key (ergoemacs-kbd (nth 0 var) t))
            (cmd (nth 1 var))
            (stripped-key (replace-regexp-in-string
                           (format "\\<[%s]-"
                                   (if ergoemacs-swap-alt-and-control
                                       "C"
                                     "M"))
                           "" key))
            (new-cmd (nth 1 var)) tmp)
       (when (eq 'cons (type-of cmd))
         (mapc
          (lambda(fn)
            (when (and (not tmp)
                       (condition-case err
                           (interactive-form fn)
                         (error nil)))
              (setq tmp fn)))
          (reverse cmd))
         (setq cmd tmp)
         (setq new-cmd tmp)
         (setq tmp nil))
       (ergoemacs-debug "Key:%s stripped-key: %s" key stripped-key)
       (when (string-match "^\\([[:ascii:]]\\|SPC\\)$" stripped-key)
         (eval
          (macroexpand
           `(defun ,(intern (format "%s-ergoemacs" (symbol-name cmd))) (&optional arg)
              ,(format "Run `%s' or whatever this mode remaps the command to be using `ergoemacs-shortcut-internal'." (symbol-name cmd))
              (interactive "P")
              (setq this-command ',cmd)
              ;; (setq prefix-arg current-prefix-arg)
              (ergoemacs-shortcut-internal ',cmd))))
         (setq new-cmd (intern (format "%s-ergoemacs" (symbol-name cmd))))
         (ergoemacs-debug "Created %s" new-cmd)
         (ergoemacs-debug "Unshifted regular expression: %s" ergoemacs-unshifted-regexp)
         (if (save-match-data
               (let (case-fold-search)
                 (string-match ergoemacs-unshifted-regexp stripped-key)))
             (progn ;; Lower case
               (define-key ergoemacs-full-alt-keymap
                 (read-kbd-macro stripped-key) new-cmd)
               (define-key ergoemacs-full-alt-keymap
                 (read-kbd-macro (concat "<override> " stripped-key))
                 cmd)
               (setq tmp (assoc stripped-key ergoemacs-shifted-assoc))
               (when tmp
                 ;; M-lower case key for shifted map.
                 (define-key ergoemacs-full-alt-shift-keymap
                   (read-kbd-macro (concat "M-" stripped-key))
                   new-cmd)
                 (define-key ergoemacs-full-alt-keymap
                   (read-kbd-macro (concat "<override> M-" stripped-key))
                   cmd)
                 ;; Upper case for shifted map
                 (define-key ergoemacs-full-alt-shift-keymap
                   (read-kbd-macro (cdr tmp)) new-cmd)
                 (define-key ergoemacs-full-alt-keymap
                   (read-kbd-macro (concat "<override> " (cdr tmp)))
                   cmd)))
           ;; Upper case
           (setq tmp (assoc stripped-key ergoemacs-shifted-assoc))
           (when tmp
             ;; Install lower case on shifted map.
             (define-key ergoemacs-full-alt-shift-keymap
               (read-kbd-macro (cdr tmp)) new-cmd)
             (define-key ergoemacs-full-alt-shift-keymap
               (read-kbd-macro (concat "<override> " (cdr tmp)))
               cmd)
             ;; Install M-lower for alt map.
             (define-key ergoemacs-full-alt-keymap
               (read-kbd-macro (concat "M-" (cdr tmp))) new-cmd)
             (define-key ergoemacs-full-alt-keymap
               (read-kbd-macro (concat "<override> M-" (cdr tmp)))
               cmd))
           (define-key ergoemacs-full-alt-keymap
             (read-kbd-macro stripped-key) new-cmd)
           (define-key ergoemacs-full-alt-keymap
             (read-kbd-macro (concat "<override> M-" stripped-key))
             cmd)))
       (when (member cmd ergoemacs-movement-functions)
         (set (intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
              (make-sparse-keymap))
         (eval `(define-key ,(intern (concat "ergoemacs-fast-" (symbol-name cmd) "-keymap"))
                  ,(read-kbd-macro stripped-key) new-cmd))
         (define-key ergoemacs-full-fast-keys-keymap
           (read-kbd-macro stripped-key)
           new-cmd))))
   (symbol-value (ergoemacs-get-variable-layout)))
  (mapc
   (lambda(key)
     (unless (string= "" key)
       (unless (lookup-key ergoemacs-full-alt-keymap (read-kbd-macro key))
         (define-key ergoemacs-full-alt-keymap
           (read-kbd-macro key)
           '(lambda() (interactive)
              (let (message-log-max)
                (message "[Alt+] keymap active.")))))
       (unless (lookup-key ergoemacs-full-alt-shift-keymap (read-kbd-macro key))
         (define-key ergoemacs-full-alt-shift-keymap
           (read-kbd-macro key)
           '(lambda() (interactive)
              (let (message-log-max)
                (message "[Alt+Shift] keymap active.")))))))
   (symbol-value (intern (concat "ergoemacs-layout-" (or ergoemacs-keyboard-layout "us")))))
  
  (ergoemacs-debug-keymap 'ergoemacs-full-alt-keymap)
  (ergoemacs-debug-keymap 'ergoemacs-full-alt-shift-keymap)
  (ergoemacs-debug-keymap 'ergoemacs-full-fast-keys-keymap)
  (ergoemacs-debug-heading "Stop ergoemacs-setup-fast-keys")
  (ergoemacs-debug-flush))

(defvar ergoemacs-exit-temp-map-var nil)

(defun ergoemacs-minibuffer-setup ()
  "Exit temporary overlay maps."
  ;; (setq ergoemacs-exit-temp-map-var t)
  (set (make-local-variable 'ergoemacs-modal) nil)
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


(defvar ergoemacs-modal nil
  "Weather modal ergoemacs is active.")

(defun ergoemacs-modal-toggle (mode-text keymap exit-fn)
  "Toggle ergoemacs command modes."
  (let ((alt mode-text)
        (x (assq 'ergoemacs-modal
                 ergoemacs-emulation-mode-map-alist)))
    (if x
        (progn
          (setq ergoemacs-emulation-mode-map-alist (delq x ergoemacs-emulation-mode-map-alist))
          (set-cursor-color ergoemacs-default-cursor)
          (let (message-log-max)
            (message "Full %s command mode removed." alt))
          (set-default 'ergoemacs-modal nil)
          (setq ergoemacs-modal nil)
          (ergoemacs-mode-line))
      ;; Turn on full alt command mode.
      (push (cons 'ergoemacs-modal
                  keymap)
            ergoemacs-emulation-mode-map-alist)
      (set-default 'ergoemacs-modal mode-text)
      (setq ergoemacs-modal mode-text)
      (ergoemacs-mode-line ;; Indicate Alt+ in mode-line
       (concat " " mode-text))
      (unless ergoemacs-default-cursor
        (setq ergoemacs-default-cursor
              (or (frame-parameter nil 'cursor-color) "black"))
        (set-cursor-color ergoemacs-modal-cursor))
      (let (message-log-max)
        (message "%s command move installed. Exit by %s"
               alt
               (mapconcat
                (lambda(key)
                  (ergoemacs-pretty-key (key-description key)))
                (where-is-internal exit-fn keymap)
                ", "))))
    (ergoemacs-debug "ergoemacs-emulation-mode-map-alist: %s" (mapcar (lambda(x) (nth 0 x)) ergoemacs-emulation-mode-map-alist))))

(defun ergoemacs-toggle-full-alt ()
  "Toggles full Alt+ keymap"
  (interactive)
  (let (deactivate-mark)
    (ergoemacs-debug-heading "Start `ergoemacs-toggle-full-alt'")
    (ergoemacs-modal-toggle
     (replace-regexp-in-string
      "!" "" (ergoemacs-pretty-key "M-!")) ergoemacs-full-alt-keymap 'ergoemacs-toggle-full-alt)
    (ergoemacs-debug-heading "Finish `ergoemacs-toggle-full-alt'")
    (ergoemacs-debug-flush)))

(defun ergoemacs-toggle-full-alt-shift ()
  "Toggles full Alt+Shift+ keymap"
  (interactive)
  (let (deactivate-mark)
    (ergoemacs-debug-heading "Start `ergoemacs-toggle-full-alt-shift'")
    (ergoemacs-modal-toggle
     (replace-regexp-in-string
      "~" "" (ergoemacs-pretty-key "M-S-~")) ergoemacs-full-alt-shift-keymap 'ergoemacs-toggle-full-alt-shift)
    (ergoemacs-debug-heading "Finish `ergoemacs-toggle-full-alt-shift'")
    (ergoemacs-debug-flush)))

(provide 'ergoemacs-modal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-modal.el ends here
