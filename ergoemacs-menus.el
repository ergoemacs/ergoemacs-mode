;;; ergoemacs-menus.el --- toggle ErgoEmacs-style menus -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

;; Maintainer: Matthew L. Fidler
;; Authors: Xah Lee,  Matthew Fidler, Xah Lee, Drew Adams
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
(eval-when-compile
  (require 'cl))

(defvar ergoemacs-xah-emacs-lisp-tutorial-url
  "http://ergoemacs.org/emacs/elisp.html")

(defvar ergoemacs-mode-web-page-url
  "http://ergoemacs.github.io/")

(declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
(defvar ergoemacs-use-unicode-char)
(defun ergoemacs-kbd-to-key (key)
  "Convert key Emacs key code to ergoemacs-key-code."
  (let ((ergoemacs-use-unicode-char nil))
    (replace-regexp-in-string
     "\\(\\[\\|\\]\\)" ""
     (replace-regexp-in-string "\\]\\[" " "(ergoemacs-pretty-key key)))))

(defun ergoemacs-shortcut-for-menu-item (item)
  (if (and (>= (safe-length item) 4)
           (symbolp (car item))
           (eq (cadr item) 'menu-item)
           (stringp (caddr item))
           (symbolp (cadddr item))
           (not (keymapp (cadddr item))))
      ;; Look if this item already has a :keys property
      (if (catch 'found-keys
            (dolist (i item)
              (when (eq i ':keys)
                (throw 'found-keys t))) nil)
          nil
        (ergoemacs-shortcut-for-command (cadddr item)))
    nil))

(defun ergoemacs-preprocess-menu-keybindings (menu)
  "Put `ergoemacs-mode' key bindings on menus."
  (if (not (ignore-errors (keymapp menu)))
      (progn
        (message "Invalid menu in ergoemacs-preprocess-menu-keybindings %s" menu)
        menu)
    (when (symbolp menu)
      (setq menu (symbol-value menu)))
    ;; For each element in the menu
    (setcdr menu
            (mapcar (lambda (item)
                      (let ((key (ergoemacs-shortcut-for-menu-item item)))
                        (if key
                            (append item (cons :keys (cons key nil)))
                          item)))
                    (cdr menu)))
    ;; Recurse sub menu items
    (dolist (x (cdr menu))
      (when (and (consp x)
                 (consp (cdr x))
                 (consp (cdr (cdr x)))
                 (consp (cdr (cdr (cdr x))))
                 (eq (car (cdr x)) 'menu-item)
                 (keymapp (car (cdr (cdr (cdr x))))))
                                        ;(message "Submenu: %s" (car (cdr (cdr x))))
        (ergoemacs-preprocess-menu-keybindings (car (cdr (cdr (cdr x)))))))))

(defvar ergoemacs-handle-ctl-c-or-ctl-x)
(defvar ergoemacs-no-shortcut-keys)
(defvar ergoemacs-shortcut-keys)
(defvar ergoemacs-read-input-keys)
(defun ergoemacs-shortcut-for-command (cmd)
  "Figures out ergoemacs-mode menu's preferred key-binding for CMD."
  (cond
   ((and (memq ergoemacs-handle-ctl-c-or-ctl-x '(only-copy-cut both))
         (eq cmd 'ergoemacs-cut-line-or-region)) (ergoemacs-kbd-to-key "C-x") )
   ((and (memq ergoemacs-handle-ctl-c-or-ctl-x '(only-copy-cut both))
         (eq cmd 'ergoemacs-copy-line-or-region)) (ergoemacs-kbd-to-key "C-c"))
   (t
    (let ((key (key-description
                (let (ergoemacs-read-input-keys
                      ergoemacs-shortcut-keys
                      (ergoemacs-no-shortcut-keys t)
                      (min-len 1000)
                      (ret ""))
                  (dolist (item (where-is-internal cmd nil nil nil t))
                    (cond
                     ((and (< (length item) min-len) item
                           (not (memq (elt item 0) '(menu-bar remap again redo cut copy paste help))))
                      (setq ret item)
                      (setq min-len (length item)))
                     ((and item (= (length item) min-len)
                           (string-match-p "^C-.$" (key-description item)))
                      (setq ret item))
                     ((and item (= (length item) min-len) (not (eq (elt item 0) 'help))
                           (string-match-p "\\<[Ff]1\\>" (key-description item)))
                      (setq ret item))))
                  ret))))
      (ergoemacs-kbd-to-key key)))))


(defvar ergoemacs-menu-bar-old-file-menu (lookup-key global-map [menu-bar file]))

(defvar ergoemacs-menu-bar-file-menu nil)


;; (defvar ergoemacs-excluded-major-modes '()
;;   )

(defcustom ergoemacs-excluded-major-modes
  '(conf-colon-mode
    conf-xdefaults-mode conf-space-mode conf-javaprop-mode
    conf-ppd-mode mail-mode
    ebrowse-tree-mode diff-mode fundamental-mode emacs-lisp-byte-code-mode
    R-transcript-mode S-transcript-mode XLS-mode tar-mode
    git-commit-mode git-rebase-mode image-mode
    archive-mode ses-mode)
  "List of major modes excluded from ergoemacs' Languages menu."
  :type '(repeat (symbol :tag "Excluded Major Mode"))
  :group 'ergoemacs-mode)

(defcustom ergoemacs-mode-names
  '((conf-mode "Settings")
    (ses-mode "Emacs Spreadsheet")
    (m2-mode "Modula-2")
    (snmpv2-mode "SNMPv2 MIBs")
    (snmp-mode "SKMP MIBs"))
  "Menu name for ergoemacs' Languages menu."
  :type '(repeat
          (list
           (symbol :tag "Major Mode Name")
           (text :tag "Alternative Description:")))
  :group 'ergoemacs-mode)


(defun ergoemacs-get-major-mode-name (mode)
  "Gets the MODE language name.
Tries to get the value from `ergoemacs-mode-names'.  If not guess the language name."
  (let ((ret (assoc mode ergoemacs-mode-names)))
    (if (not ret)
        (setq ret (replace-regexp-in-string
                   "-" " "
                   (replace-regexp-in-string
                    "-mode" ""
                    (symbol-name mode))))
      (setq ret (car (cdr ret))))
    (setq ret (concat (upcase (substring ret 0 1))
                      (substring ret 1)))
    ret))

;; `Languages'  
(defun ergoemacs-get-major-modes ()
  "Gets a list of language modes known to `ergoemacs-mode'.
This gets all major modes known from the variables:
-  `interpreter-mode-alist';
-  `magic-mode-alist'
-  `magic-fallback-mode-alist'
-  `auto-mode-alist'

All other modes are assumed to be minor modes or unimportant.
"
  ;; Get known major modes
  (let ((ret '())
        all dups cur-lst current-letter
        added-modes
        (modes '()))
    (dolist (elt (append
                  interpreter-mode-alist
                  magic-mode-alist
                  magic-fallback-mode-alist
                  auto-mode-alist))
      (unless (memq (cdr elt) modes)
        (when (and (functionp (cdr elt))
                   (ignore-errors (string-match "-mode$" (symbol-name (cdr elt)))))
          (unless (or (memq (cdr elt) ergoemacs-excluded-major-modes)
                      (member (downcase (symbol-name (cdr elt))) added-modes))
            (let* ((name (ergoemacs-get-major-mode-name (cdr elt)))
                   (first (upcase (substring name 0 1))))
              (if (member first all)
                  (unless (member first dups)
                    (push first dups))
                (push first all))
              (push (list (cdr elt) 'menu-item
                          name
                          (cdr elt)) ret))
            (push (downcase (symbol-name (cdr elt))) added-modes)
            (push (cdr elt) modes)))))
    (setq modes (sort ret (lambda(x1 x2) (string< (downcase (nth 2 x2))
                                             (downcase (nth 2 x1))))))
    (setq ret '())
    (dolist (elt modes)
      (let ((this-letter (upcase (substring (nth 2 elt) 0 1))))
        (cond
         ((not (member this-letter dups))
          ;; not duplicated -- add prior list and push current element.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (push elt ret)
          (setq current-letter this-letter)
          (setq cur-lst nil))
         ((not (equal this-letter current-letter))
          ;; duplicated, but not last letter.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (setq cur-lst nil)
          (setq current-letter this-letter)
          (push elt cur-lst))
         (t
          ;; duplicated and last letter
          (push elt cur-lst)))))
    (when cur-lst
      (push `(,(intern current-letter) menu-item ,current-letter
              (keymap ,@cur-lst)) ret))
    ;; Now create nested menu.
    `(keymap ,@ret
             (separator1 menu-item "--")
             (package menu-item  "Manage Packages" list-packages))))

;;; `File' menu
(declare-function pr-update-menus "printing.el")
(defun ergoemacs-menu-bar-file-menu ()
  "Creates Ergoemacs File Menu"
  (unless ergoemacs-menu-bar-file-menu
    (setq ergoemacs-menu-bar-file-menu
          `(keymap
            (new-file menu-item "New" ergoemacs-new-empty-buffer)
            (make-frame menu-item "New Frame" make-frame-command)
            (open-file menu-item "Open..." find-file)
            (open-directory menu-item "Open Containing Folder"
                            (keymap
                             ;; FIXME add open in cmd/iTerm/xterm, etc
                             (open-directory-in-dired menu-item "In Dired" dired-jump)
                             (open-directory-in-desktop
                              menu-item  ,(cond
                                           ((eq system-type 'windows-nt) "In Explorer")
                                           ((eq system-type 'darwin) "In Finder")
                                           (t "In File Manager"))
                              ergoemacs-open-in-desktop)
                             (sep1 menu-item "--")
                             (open-eshell-here menu-item "In Emacs Shell" ergoemacs-eshell-here)
                             (open-shell-here menu-item ,(if (eq system-type 'windows-nt) "In Command Prompt" "In Shell") ergoemacs-shell-here)
                             ,(if (eq system-type 'windows-nt) '(powershell-here menu-item "In PowerShell" ergoemacs-powershell-here :enable (fboundp 'powershell)))
                             ))
            ;; FIXME -- Somehow put open last closed in recentf menu; It
            ;; seems to fit there the best
            ;; (open-last-closed menu-item "Open last closed" ergoemacs-open-last-closed)
            (kill-buffer menu-item "Close" ergoemacs-close-current-buffer)
            (separator1 menu-item "--")
            (save-buffer menu-item "Save" save-buffer)
            (write-file menu-item "Save As..." write-nfile)
            (revert-buffer menu-item "Revert to Saved" revert-buffer)
            (print-buffer menu-item "Print" ergoemacs-print-buffer-confirm)
            ;; (ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
            (separator4 menu-item "--")
            (split-window-below menu-item "Split Window"
                                split-window-below)
            (split-window-right menu-item "Split Window right"
                                split-window-right)
            (one-window menu-item "Unsplit Window"
                        delete-other-windows)
            (separator5 menu-item "--")
            (execute-command menu-item "Execute Command" execute-extended-command)
            (repeat-earlier-command menu-item "Repeat Earlier Command"
                                    repeat-complex-command)
            (separator6 menu-item "--")
            (exit-emacs menu-item "Quit" save-buffers-kill-emacs)
            "File"))
    (define-key global-map [menu-bar file] (cons "File" ergoemacs-menu-bar-file-menu))
    (require 'printing nil t)
    (when (featurep 'printing)
      (pr-update-menus t))
    (setq ergoemacs-menu-bar-file-menu (lookup-key global-map [menu-bar file])))
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-file-menu))

;;; `Edit' Menu
(defvar ergoemacs-menu-bar-old-edit-menu (lookup-key global-map [menu-bar edit]))

(defvar ergoemacs-menu-bar-edit-menu
  '(keymap
    (undo menu-item "Undo" undo
          :enable (and
                   (not buffer-read-only)
                   (not
                    (eq t buffer-undo-list))
                   (if
                       (eq last-command 'undo)
                       (listp pending-undo-list)
                     (consp buffer-undo-list)))
          :help "Undo last operation")
    (redo menu-item "Redo" ergoemacs-redo
          :enable (and
                   (not buffer-read-only)
                   (not (eq t buffer-undo-list))
                   (or
                    (not (and (boundp 'undo-tree-mode) undo-tree-mode))
                    (and (and (boundp 'undo-tree-mode) undo-tree-mode)
                            (null (undo-tree-node-next (undo-tree-current buffer-undo-tree)))))))
    (redo-sep menu-item "--")
    (cut menu-item "Cut" ergoemacs-cut-line-or-region
         :help "Delete text in Line/region and copy it to the clipboard"
         :enable (or (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                     (region-active-p)))
    (copy menu-item "Copy" ergoemacs-copy-line-or-region
          :help "Copy text in line/region to the clipboard"
          :enable (or (eq ergoemacs-handle-ctl-c-or-ctl-x 'only-copy-cut)
                      (region-active-p)))
    (paste menu-item "Paste" ergoemacs-paste
           :help "Paste text from clipboard")
    (paste-from-menu menu-item "Paste from Kill Menu" yank-menu
                     :enable (and
                              (cdr yank-menu)
                              (not buffer-read-only))
                     :help "Choose a string from the kill ring and paste it")
    (clear menu-item "Clear" delete-region
           :enable (and mark-active (not buffer-read-only))
           :help "Delete the text in region between mark and current position")
    (mark-whole-buffer menu-item "Select All" mark-whole-buffer
                       :help "Mark the whole buffer for a subsequent cut/copy")
    (separator-search menu-item "--")
    (blank-operations menu-item "Blank/Whitespace Operations"
                      (keymap
                       (trim-trailing-space menu-item
                                            "Trim Trailing Space"
                                            delete-trailing-whitespace
                                            :help "Trim Trailing spaces on each line")
                       (separator-tabify menu-item "--")
                       (tabify-region menu-item
                                      "Change multiple spaces to tabs (Tabify)"
                                      (lambda() (interactive)
                                        (if mark-active
                                            (tabify (region-beginning)
                                                    (region-end))
                                          (tabify (point-min) (point-max))))
                                      :help "Convert multiple spaces in the nonempty region to tabs when possible"
                                      :enable  (not buffer-read-only))
                       (untabify menu-item
                                 "Change Tabs To Spaces (Untabify)"
                                 (lambda() (interactive)
                                   (if mark-active
                                       (untabify (region-beginning)
                                                 (region-end))
                                     (untabify (point-min) (point-max))))
                                 :help "Convert all tabs in the nonempty region or buffer to multiple spaces"
                                 :enable (not buffer-read-only))))
    (copy-to-clipboard menu-item "Copy File/Path to Clipboard"
                       (keymap
                        (copy-full-path menu-item
                                        "Current Full File Path to Clipboard"
                                        ergoemacs-copy-full-path
                                        :enable (buffer-file-name))
                        (copy-file-name menu-item
                                        "Current File Name to Clipboard"
                                        ergoemacs-copy-file-name
                                        :enable (buffer-file-name))
                        (copy-dir-path menu-item
                                       "Current Dir. Path to Clipboard"
                                       ergoemacs-copy-dir-path
                                       :enable (buffer-file-name))))
    (convert-case-to menu-item "Convert Case To"
                     (keymap
                      (capitalize-region menu-item
                                         "Capitalize" capitalize-region
                                         :help "Capitalize (initial caps) words in the nonempty region"
                                         :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                      (downcase-region menu-item
                                       "downcase" downcase-region
                                       :help "Make words in the nonempty region lower-case"
                                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                      (upcase-region menu-item "UPCASE" upcase-region
                                     :help "Make words in the nonempty region upper-case"
                                     :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
                      (toggle-case-region menu-item "Toggle Capitalization/Case"
                                          ergoemacs-toggle-letter-case
                                          :enable (not buffer-read-only))
                      (toggle-camel menu-item "Toggle CamelCase to camel_case"
                                    ergoemacs-toggle-camel-case
                                    :enable (not buffer-read-only))))
    
    (eol-conversion menu-item "EOL Conversion"
                    (keymap
                     (windows menu-item
                              "Windows/DOS"
                              (lambda() (interactive)
                                (ergoemacs-eol-conversion 'dos))
                              :enable (not (ergoemacs-eol-p 'dos)))
                     (unix menu-item
                           "Unix/OSX"
                           (lambda() (interactive)
                             (ergoemacs-eol-conversion 'unix))
                           :enable (not (ergoemacs-eol-p 'unix)))
                     (mac menu-item
                          "Old Mac"
                          (lambda() (interactive)
                            (ergoemacs-eol-conversion 'mac))
                          :enable (not (ergoemacs-eol-p 'mac)))))
    ;; Taken/Adapted from menu+ by Drew Adams.
    ;; (region menu-item "Region"
    ;;         (keymap
    ;;          (unaccent-region
    ;;           menu-item "Unaccent" unaccent-region ; Defined in `unaccent'.
    ;;                     :help "Replace accented chars in the nonempty region by unaccented chars"
    ;;                     :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
    ;;          (separator-chars menu-item "--")
    
    
    ;;         (comment-region menu-item
    ;;                         "(Un)Comment" comment-region
    ;;                         :help "Comment or uncomment each line in the nonempty region"
    ;;                         :enable (and comment-start  (not buffer-read-only)  mark-active
    ;;                                    (> (region-end) (region-beginning))))
    ;;         (center-region menu-item
    ;;                        "Center" center-region
    ;;                       :help "Center each nonblank line that starts in the nonempty region"
    ;;                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
    ;;         ;; (indent-rigidly-region menu-item "Rigid Indent"
    ;;         ;;                        indent-rigidly
    ;;         ;;               :help "Indent each line that starts in the nonempty region"
    ;;         ;;               :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
    ;;         (indent-region menu-item "Column/Mode Indent" indent-region
    ;;                       :help "Indent each nonblank line in the nonempty region"
    ;;                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))

    ;;         (separator-indent menu-item "--")
    ;;         (abbrevs-region "Expand Abbrevs..."
    ;;                         expand-region-abbrevs
    ;;                         :help "Expand each abbrev in the nonempty region (with confirmation)"
    ;;                         :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
    
    ;;         (macro-region menu-item
    ;;                       "Exec Keyboard Macro" apply-macro-to-region-lines
    ;;                       :help "Run keyboard macro at start of each line in the nonempty region"
    ;;                       :enable (and last-kbd-macro mark-active  (not buffer-read-only)
    ;;                                    (> (region-end) (region-beginning))))))
    ;; Taken/Adapted from menu+ by Drew Adams.
    (sort menu-item "Sort"
          (keymap
           (regexp-fields menu-item
                          "Regexp Fields" sort-regexp-fields
                          :help "Sort the nonempty region lexicographically"
                          :enable (and last-kbd-macro
                                       (not buffer-read-only)
                                       mark-active
                                       (> (region-end) (region-beginning))))
           (pages menu-item
                  "Pages" sort-pages
                  :help "Sort pages in the nonempty region alphabetically"
                  :enable (and last-kbd-macro
                               (not buffer-read-only)
                               mark-active
                               (> (region-end) (region-beginning))))
           (sort-paragraphs menu-item
                            "Paragraphs" sort-paragraphs
                            :help "Sort paragraphs in the nonempty region alphabetically"
                            :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
           (sort-numeric-fields menu-item
                                "Numeric Field" sort-numeric-fields
                                :help "Sort lines in the nonempty region numerically by the Nth field"
                                :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
           (sort-fields menu-item
                        "Field" sort-fields
                        :help "Sort lines in the nonempty region lexicographically by the Nth field"
                        :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
           (sort-columns menu-item
                         "Columns" sort-columns
                         :help "Sort lines in the nonempty region alphabetically, by a certain range of columns"
                         :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
           (sort-lines menu-item
                       "Lines" sort-lines
                       :help "Sort lines in the nonempty region alphabetically"
                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
           (reverse-region menu-item "Reverse" reverse-region
                           :help "Reverse the order of the selected lines"
                           :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))))
    
    
    ;; (search menu-item "Search"
    ;;         (keymap
    ;;          (search-forward menu-item "Text..." search-forward)
    ;;          (separator-repeat-search menu-item "--")
    ;;          (tags-srch menu-item "Search Tagged Files..." tags-search
    ;;                     :help "Search for a regexp in all tagged files")
    ;;          (tags-continue menu-item "Continue Tags Search" tags-loop-continue
    ;;                         :help "Continue last tags search operation")
    ;;          "Search"))
    
    ;; (i-search menu-item "Incremental Search"
    ;;           (keymap
    ;;            (isearch-forward menu-item "Forward String..." isearch-forward
    ;;                             :help "Search forward for a string as you type it")
    ;;            (isearch-backward menu-item "Backward String..." isearch-backward
    ;;                              :help "Search backwards for a string as you type it")
    ;;            (isearch-forward-regexp menu-item "Forward Regexp..." isearch-forward-regexp
    ;;                                    :help "Search forward for a regular expression as you type it")
    ;;            (isearch-backward-regexp menu-item "Backward Regexp..." isearch-backward-regexp
    ;;                                     :help "Search backwards for a regular expression as you type it")
    ;;            "Incremental Search"))
    
    ;; (replace menu-item "Replace"
    ;;          (keymap
    ;;           (query-replace menu-item "Replace String..." query-replace
    ;;                          :enable (not buffer-read-only)
    ;;                          :help "Replace string interactively, ask about each occurrence")
    ;;           (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp
    ;;                                 :enable (not buffer-read-only)
    ;;                                 :help "Replace regular expression interactively, ask about each occurrence")
    ;;           (separator-replace-tags menu-item "--")
    ;;           (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace
    ;;                      :help "Interactively replace a regexp in all tagged files")
    ;;           (tags-repl-continue menu-item "Continue Replace" tags-loop-continue
    ;;                               :help "Continue last tags replace operation")
    ;;           "Replace"))
    
    ;; (goto menu-item "Go To"
    ;;       (keymap
    ;;        (go-to-line menu-item "Goto Line..." goto-line
    ;;                    :help "Read a line number and go to that line")
    ;;        (separator-tags menu-item "--")
    ;;        (find-tag menu-item "Find Tag..." find-tag
    ;;                  :help "Find definition of function or variable")
    ;;        (find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window
    ;;                         :help "Find function/variable definition in another window")
    ;;        (next-tag menu-item "Find Next Tag" menu-bar-next-tag
    ;;                  :enable (and
    ;;                           (boundp 'tags-location-ring)
    ;;                           (not
    ;;                            (ring-empty-p tags-location-ring)))
    ;;                  :help "Find next function/variable matching last tag name")
    ;;        (next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window
    ;;                         :enable (and
    ;;                                  (boundp 'tags-location-ring)
    ;;                                  (not
    ;;                                   (ring-empty-p tags-location-ring)))
    ;;                         :help "Find next function/variable matching last tag name in another window")
    ;;        (apropos-tags menu-item "Tags Apropos..." tags-apropos
    ;;                      :help "Find function/variables whose names match regexp")
    ;;        (separator-tag-file menu-item "--")
    ;;        (set-tags-name menu-item "Set Tags File Name..." visit-tags-table
    ;;                       :help "Tell Tags commands which tag table file to use")
    ;;        "Go To"))
    
    ;; (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
    (separator-bookmark menu-item "--")
    (fill menu-item "Fill" fill-region
          :enable (and mark-active
                       (not buffer-read-only))
          :help "Fill text in region to fit between left and right margin")
    (props menu-item "Text Properties" facemenu-menu)
    "Edit"))

;;; `Search' menu
(defvar ergoemacs-menu-bar-search-menu
  '(keymap
    (isearch-forward menu-item "String Forward..." isearch-forward
                     :help "Search forward for a string as you type it")
    (isearch-backward menu-item "    Backward..." isearch-backward
                      :help "Search backwards for a string as you type it")
    (re-isearch-forward menu-item "Regexp Forward..." isearch-forward-regexp
                        :help "Search forward for a regular expression as you type it")
    (re-isearch-backward menu-item "    Backward..." isearch-backward-regexp
                         :help "Search backwards for a regular expression as you type it")
    (separator-repeat-search menu-item "--" )
    (repeat-forward menu-item "Repeat Forward" nonincremental-repeat-search-forward
                    :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
                                (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
                    :help "Repeat last search forward")
    (repeat-backward menu-item "    Repeat Backward" nonincremental-repeat-search-backward
                     :enable (or (and (memq menu-bar-last-search-type '(string word)) search-ring)
                                 (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
                     :help "Repeat last search forward")
    (separator-isearch menu-item "--")
    (i-search menu-item "String Search"
              (keymap
               (search-forward menu-item "Forward String..." search-forward)
               (search-backward menu-item "    Backward..." search-backward)
               (search-forward-regexp menu-item "Forward Regexp..." re-search-forward)
               (search-backward-regexp menu-item "    Backward..." re-search-backward)
               "String Search"))
    
    (replace menu-item "Replace"
             (keymap
              (query-replace menu-item "Replace String..." query-replace
                             :enable (not buffer-read-only)
                             :help "Replace string interactively, ask about each occurrence")
              (query-replace-regexp menu-item "Replace Regexp..." query-replace-regexp
                                    :enable (not buffer-read-only)
                                    :help "Replace regular expression interactively, ask about each occurrence")
              (separator-replace-tags menu-item "--")
              (tags-repl menu-item "Replace in Tagged Files..." tags-query-replace
                         :help "Interactively replace a regexp in all tagged files")
              (tags-repl-continue menu-item "Continue Replace" tags-loop-continue
                                  :help "Continue last tags replace operation")
              "Replace"))
    (grep menu-item "Grep..." grep
          :enable (executable-find "grep"))
    (occur menu-item "Occurrences in buffer..." occur
           :help "Show Lines in a buffer that match a regular expression")
    (moccur menu-item "Occurrences in all buffers..." multi-occur
            :help "Show Lines in all buffers that match a regular expression")
    (separator-go-to menu-item "--" )
    
    (goto menu-item "Go To"
          (keymap
           (go-to-line menu-item "Goto Line..." goto-line
                       :help "Read a line number and go to that line")
           (separator-tags menu-item "--")
           (find-tag menu-item "Find Tag..." find-tag
                     :help "Find definition of function or variable")
           (find-tag-otherw menu-item "Find Tag in Other Window..." find-tag-other-window
                            :help "Find function/variable definition in another window")
           (next-tag menu-item "Find Next Tag" menu-bar-next-tag
                     :enable (and
                              (boundp 'tags-location-ring)
                              (not
                               (ring-empty-p tags-location-ring)))
                     :help "Find next function/variable matching last tag name")
           (next-tag-otherw menu-item "Next Tag in Other Window" menu-bar-next-tag-other-window
                            :enable (and
                                     (boundp 'tags-location-ring)
                                     (not
                                      (ring-empty-p tags-location-ring)))
                            :help "Find next function/variable matching last tag name in another window")
           (apropos-tags menu-item "Tags Apropos..." tags-apropos
                         :help "Find function/variables whose names match regexp")
           (separator-tag-file menu-item "--")
           (set-tags-name menu-item "Set Tags File Name..." visit-tags-table
                          :help "Tell Tags commands which tag table file to use")
           "Go To")
          (separator-packages))
    
    (bookmark menu-item "Bookmarks" menu-bar-bookmark-map)
    "Search"))

;;; `View' menu
(defvar package-archives)
(defvar tabbar-mode)
(declare-function tabbar-install-faces "tabbar-ruler.el")
(declare-function tabbar-mode "tabbar-mode.el")
(declare-function package-refresh-contents "package.el")
(declare-function package-initialize "package.el")
(defun ergoemacs-menu-tabbar-toggle ()
  "Enables/Disables (and installs if not present) a tab-bar for emacs."
  (interactive)
  (require 'package nil t)
  (if (not (fboundp 'tabbar-mode))
      (let ((package-archives '(("melpa" . "http://melpa.milkbox.net/packages/"))))
        (require 'tabbar-ruler nil t)
        (if (fboundp 'tabbar-install-faces)
            (tabbar-install-faces)
          (when (fboundp 'package-install)
            (package-refresh-contents)
            (package-initialize)
            (package-install 'tabbar-ruler)
            (require 'tabbar-ruler nil t)
            (tabbar-install-faces))))
    (if (not (featurep 'tabbar-ruler))
        (require 'tabbar-ruler nil t)
      (if tabbar-mode
          (tabbar-mode -1)
        (tabbar-mode 1)))))

(defvar ergoemacs-menu-bar-view-menu
  `(keymap
    (menu-font-size menu-item "Zoom"
                    (keymap
                     (zoom-in menu-item "Zoom In" text-scale-increase)
                     (zoom-out menu-item "Zoom Out" text-scale-decrease)
                     (zoom-reset menu-item "Zoom Reset" ergoemacs-text-scale-normal-size)))
    
    (menu-set-font menu-item "Set Default Font..." menu-set-font :visible
                   (display-multi-font-p)
                   :help "Select a default font")
    
    ,(when (fboundp 'customize-themes)
       '(color-theme menu-item "Customize Color Themes" customize-themes
                     :help "Customize Emacs Themes."))
    
    (separator-font-size menu-item "--")

    (highlight-current-line menu-item "Highlight Current Line" global-hl-line-mode
                            :help "Have the cursor line always Highlighted"
                            :button (:toggle . (and (boundp 'global-hl-line-mode)
                                                    global-hl-line-mode)))

    (paren-mode menu-item "Highlight Matching Parentheses" show-paren-mode
                :button (:toggle . show-paren-mode))

    (ruler-mode menu-item "Ruler Mode" ruler-mode
                :button (:toggle . ruler-mode))

    (blink-cursor menu-item "Cursor Blink" blink-cursor-mode
                  :button (:toggle . blink-cursor-mode))

    ;; (auto-pair menu-item "Insert Matching Parentheses/Bracket"
    ;;            (lambda() (interactive)
    ;;              (cond
    ;;               ((fboundp 'smartparens-mode)
    ;;                (smartparens-mode))
    ;;               ((fboundp 'autopair-global-mode)
    ;;                (autopair-global-mode))
    ;;               (t (electric-pair-mode))))
    ;;            :button (:toggle . 
    ;;                             (or
    ;;                              (and (boundp 'smartparens-mode) smartparens-mode)
    ;;                              (and (boundp 'auto-indent-mode) auto-indent-mode)
    ;;                              electric-pair-mode)))
    
    (tabbar-mode menu-item "Tabbar" ergoemacs-menu-tabbar-toggle
                 :button (:toggle . (and (boundp 'tabbar-mode)
                                         tabbar-mode)))
    
    
    ;; (showhide-tool-bar menu-item "Tool-bar" tool-bar-mode :help "Turn tool-bar on/off"
    ;;                    :button (:toggle . tool-bar-mode))
    
    ;; (menu-bar-mode menu-item "Menu-bar" toggle-menu-bar-mode-from-frame :help "Turn menu-bar on/off" :button
    ;;                (:toggle menu-bar-positive-p
    ;;                         (frame-parameter
    ;;                          (menu-bar-frame-for-menubar)
    ;;                          'menu-bar-lines))
    ;;                :keys "")

    ;; (showhide-tooltip-mode menu-item "Tooltips" tooltip-mode :help "Turn tooltips on/off" :visible
    ;;                        (and
    ;;                         (display-graphic-p)
    ;;                         (fboundp 'x-show-tip))
    ;;                        :button
    ;;                        (:toggle . tooltip-mode)
    ;;                        :keys "")
    (separator-speedbar menu-item "--")
    ;; (showhide-scroll-bar)
    ;; (showhide-fringe)

    (showhide-speedbar menu-item "Speedbar" speedbar-frame-mode :help "Display a Speedbar quick-navigation frame" :button
                       (:toggle and
                                (boundp 'speedbar-frame)
                                (frame-live-p
                                 speedbar-frame)
                                (frame-visible-p
                                 speedbar-frame)))
    ;; (datetime-separator)
    ;; (showhide-date-time)
    (linecolumn-separator "--")
    (line-number-mode menu-item "Line Numbers" line-number-mode :help "Show the current line number in the mode line" :button
                      (:toggle and
                               (default-boundp 'line-number-mode)
                               (default-value 'line-number-mode)))
    (global-whitespace-mode menu-item "Show/Hide whitespaces" global-whitespace-mode :button
                            (:toggle . global-whitespace-mode))
    (global-linum-mode menu-item "Show/Hide line numbers in margin" global-linum-mode :button
                       (:toggle . global-linum-mode))))


;;; `Help' menus

(defvar ergoemacs-menu-bar-old-help-menu (lookup-key global-map [menu-bar help-menu]))

(defvar ergoemacs-menu-bar-help-menu
  `(keymap
    ;; Adapted from Menu-bar+
    (whoops menu-item "Whoops!?"
            (keymap
             (what-did-i-do
              menu-item "What did I do !?"
              view-lossage
              :help "Display last 100 input keystrokes")
             (exit-recurive-edit
              menu-item "Exit Recursive Editing"
              top-level
              :help "Exit all Recursive editing Levels")
             (keyboard-quit
              menu-item "Cancel Current Action"
              keyboard-quit
              :help "Quit any operation in progress")))
    
    (help-for-help menu-item "Help for Help..."
                   help-for-help
                   :help "Emacs main help command")
    (separator-1 menu-item  "--")
    (apropos menu-item "Apropos (Find matching)"
             (keymap
              (commands menu-item "Commands..."
                        apropos-command
                        :help "Find commands matching a regular expression")
              (user-options menu-item "User Options..."
                            ergoemacs-apropos-user-options
                            :help "Find user options matching a regular expression")
              (all-vars menu-item "All Variables..."
                        apropos-variable
                        :help "Find a variable that matches a regular expression")
              (var-values menu-item "Variable Values..."
                          apropos-value
                          :help "Find variable values that matches a regular expression.")
              (symbols menu-item "Symbols..."
                       apropos
                       :help "Find functions/variables that match a regular expression.")
              (symbol-desc menu-item "Symbol Descriptions (Doc)..."
                           apropos-documentation
                           :help "Find functions/variables whose documentation match a regular expression")
              (tags menu-item "Tags..."
                    tags-apropos
                    :help "Find Tags Matching Regular Expression")))
    (describe menu-item "Describe"
              (keymap
               (function menu-item "Function..."
                         describe-function
                         :help "Describe command or other function")
               (variable menu-item "Variable..."
                         describe-variable
                         :help "Describe an emacs user option or other variable.")
               (face menu-item "Face..."
                     describe-face
                     :help "Describe a face")
               (key menu-item "Key..."
                    describe-key
                    :help "Describe a command bound to a key")
               (input menu-item "Input Method..."
                      describe-input-method)
               (coding menu-item "Coding System..."
                       describe-coding-system)
               (separator-curr-modes menu-item "--")
               (curr-major-mode menu-item "Current Major Mode"
                           ergoemacs-describe-major-mode
                           :help "Describe this buffers major and minor modes.")
               (curr-modes menu-item "Current Modes"
                           (lambda() (interactive) (call-interactively 'describe-mode))
                           :help "Describe this buffers major and minor modes.")
               (curr-keys menu-item "Current Key Bindings"
                          describe-bindings
                          :help "List all key-bindings with brief descriptions.")
               (curr-syntax menu-item "Current Syntax Table"
                            describe-syntax
                            :help "Describe the syntax specifications in the current syntax table")))
    (learn-more menu-item "Learn More"
                (keymap
                 (emacs menu-item"Emacs"
                        (keymap
                         (manual menu-item
                                 "Manual"
                                 info-emacs-manual)
                         (command-desc menu-item
                                       "    Command Description..."
                                       Info-goto-emacs-command-node
                                       :help "Show emacs manual section about a command")
                         ;; Useless for ergoemacs...
                         ;; (key-desc menu-item
                         ;;           "    Key Description..."
                         ;;           Info-goto-emacs-key-command-node
                         ;;           :help "Show Emacs manual
                         ;;           section that describes a key
                         ;;           sequence.")
                         (index menu-item
                                "    Index..."
                                emacs-index-search
                                :help "Lookup topic in Emacs manual")
                         (glossary menu-item
                                   "    Glossary"
                                   search-emacs-glossary)
                         (separator-emacs menu-item "--")
                         (faq menu-item
                              "FAQ"
                              view-emacs-FAQ
                              :help "Read frequently asked questions about Emacs (with answers)")
                         (whats-new menu-item
                                    "What's new"
                                    view-emacs-news
                                    :help "New features of emacs")
                         (known-problems menu-item
                                         "Known problems"
                                         view-emacs-problems
                                         :help "Known problems of this Emacs version.")))
                 (emacs-lisp menu-item "Emacs Lisp"
                             (keymap
                              (xah-lisp menu-item
                                        "Xah Emacs Lisp Tutorial"
                                        (lambda() (interactive)
                                          (browse-url ergoemacs-xah-emacs-lisp-tutorial-url))
                                        :help "Read Emacs Lisp Tutorial")
                              
                              (intro menu-item
                                     "Intro to Elisp"
                                     (lambda() (interactive)
                                       (info "eintr"))
                                     :help "Read introduction to Emacs Lisp")
                              (manual menu-item
                                      "Manual"
                                      (lambda() (interactive) (info "elisp"))
                                      :help "Read Emacs Lisp reference Manual")
                              (index menu-item
                                     "    Index..."
                                     elisp-index-search
                                     :help "Lookup topic in emacs lisp manual")
                              (elisp-separator menu-item "--")
                              (locate-library menu-item "Locate Library"
                                              locate-library
                                              :help "Locate lisp library")))
                 (last-accessed-info menu-item "Last Accessed Manual (Info)"
                                     info
                                     :help "Open Info, at the last doc place visited.")
                 (info-dir menu-item "All Manuals (`Info')"
                           Info-directory
                           :help "Open a list of all the info docs.")
                 (man-dir menu-item "Unix Man Pages..."
                          woman
                          :help "Unix Manual entries (with WoMan)")))
    (separator-2 menu-item "--")

    (eroemacs-current-keybindings menu-item
                                  "Current Ergoemacs Keybindings"
                                  ergoemacs-display-current-svg)
    
    (ergoemacs-mode-web-page menu-item
                             "Ergoemacs-mode web-page"
                             (lambda() (interactive)
                               (browse-url ergoemacs-mode-web-page-url))
                             :help "Online help about ergoemacs.")
    
    (separator-3 menu-item "--")
    (send-bug-report menu-item "Send Emacs Bug Report"
                     report-emacs-bug
                     :help "Report an emacs bug.")
    
    (emacs-web-page menu-item "Emacs Web Page"
                    (lambda() (interactive)
                      (browse-url "http://www.gnu.org/software/emacs/"))
                    :help "Emacs Web Page")

    (separator-licence menu-item "--")
    (license menu-item "License"
             describe-copying)
    ,(if (eq system-type 'darwin) "Help" "?")))

;; Preprocess menu keybindings...
(defvar recentf-mode)
(declare-function recentf-show-menu "recentf.el")
(defun ergoemacs-menus-on ()
  "Turn on ergoemacs menus instead of emacs menus."
  (interactive)
  (ergoemacs-menu-bar-file-menu )
  (define-key global-map [menu-bar file] (cons "File" ergoemacs-menu-bar-file-menu))
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-edit-menu)
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-search-menu)
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-view-menu)
  (ergoemacs-preprocess-menu-keybindings ergoemacs-menu-bar-help-menu)
  ;; Remove help menu
  (define-key global-map [menu-bar help-menu]
    (cons (if (eq system-type 'darwin) "Help" "?") ergoemacs-menu-bar-help-menu))
  ;; Add the Print options
  (define-key global-map [menu-bar edit] (cons "Edit" ergoemacs-menu-bar-edit-menu))
  (define-key-after global-map [menu-bar search] (cons "Search" ergoemacs-menu-bar-search-menu)
    'edit)
  (define-key-after global-map [menu-bar view] (cons "View" ergoemacs-menu-bar-view-menu)
    'search)
  (define-key-after global-map [menu-bar languages]
    (cons "Languages" (ergoemacs-get-major-modes)) 'view)
  (when recentf-mode
    (recentf-show-menu)))

(defun ergoemacs-menus-off ()
  "Turn off ergoemacs menus instead of emacs menus"
  (interactive)
  (define-key global-map [menu-bar file] (cons "File" ergoemacs-menu-bar-old-file-menu))
  (when recentf-mode
    (recentf-show-menu))
  (define-key global-map [menu-bar edit] (cons "Edit" ergoemacs-menu-bar-old-edit-menu))
  (define-key global-map [menu-bar search] nil)
  (define-key global-map [menu-bar view] nil)
  (define-key global-map [menu-bar languages] nil)
  (define-key global-map [menu-bar help-menu]
    ;; FIXME: paren mismatch
    (cons "Help" ergoemacs-menu-bar-old-help-menu)))

;;(ergoemacs-menus-on)
(provide 'ergoemacs-menus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-menus.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
