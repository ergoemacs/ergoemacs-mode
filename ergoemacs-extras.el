;;; ergoemacs-extras.el --- generate extras for ErgoEmacs

;; Copyright © 2013, 2014 Free Software Foundation, Inc.

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


(eval-when-compile (require 'cl))


(defgroup ergoemacs-extras nil
  "Documentation and script generation"
  :group 'ergoemacs-mode)

;;; Keyboard Settings

;; SVG heavily modified from
;; http://en.wikipedia.org/wiki/File:KB_United_Kingdom.svg

;; Color scheme chose from color brewer.
(defun ergoemacs-gen-svg-quote (char)
  ;; Derived from `describe-char'
  (let* ((case-fold-search nil))
    (save-match-data
      (cond
       ((string= char "")
        " ")
       ((string= char ">")
        "&gt;")
       ((string= char ">")
        "&lt;")
       ((string= char "\"")
        "&quot;")
       ((string-match "[a-zA-Z0-9]" char)
        char)
       (t
        (format "&#x%04X;"
                (encode-char
                 (with-temp-buffer
                   (insert char)
                   (char-before)) 'unicode)))))))

(defvar keyfreq-file)
(defun ergoemacs-fix-keyfreq ()
  "Fixes ergoemacs induced keyfreq bug..."
  (interactive)
  (when (featurep 'keyfreq)
    (with-temp-buffer
      (insert-file-contents keyfreq-file)
      (goto-char (point-min))
      (while (re-search-forward " \\[.*?\\]" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward " \".*?\"" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "(\\_<\\([/A-Za-z0-9.-]*?\\)\\_> *\\_<\\([/A-Za-z0-9.-]*?\\)\\_>)" nil t)
        (replace-match "(\\1 . \\2)"))
      (goto-char (point-min))
      (while (re-search-forward "(lambda" nil t)
        (goto-char (match-beginning 0))
        (when (re-search-backward "((" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point)
                         (save-excursion
                           (forward-sexp 1)
                           (point)))))
      (write-file keyfreq-file))))

(defvar ergoemacs-window-tab-switching
  '(ergoemacs-switch-to-previous-frame
    ergoemacs-switch-to-next-frame
    ergoemacs-previous-user-buffer
    split-window-horizontally
    delete-window
    delete-other-windows
    split-window-vertically
    ergoemacs-next-user-buffer)
  "Window/Tab switching functions.")

(defvar ergoemacs-deletion-functions)
(defvar ergoemacs-movement-functions)
(declare-function ergoemacs-mode "ergoemacs-mode.el")
(declare-function ergoemacs-get-fixed-layout "ergoemacs-theme-engine.el")
(declare-function ergoemacs-get-variable-layout "ergoemacs-theme-engine.el")
(defun ergoemacs-get-html-key-table ()
  "Gets the key table for the current layout."
  (ergoemacs-mode -1)
  (ergoemacs-mode 1)
  (let ((case-fold-search nil)
        (shortcut-type "")
        (short-desc "")
        (standard-key-regexp
         (format "^%s\t"
                 (regexp-opt '("C-n" "C-o" "C-O" "C-S-o" "C-w" "C-s" "C-S" "C-S-s" "C-p" "C-z" "C-Z" "C-S-z"
                               "C-y"
                               "C-f" "C-a" "C-T") 'paren)))
        (tab-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar
                   (lambda(x)
                     (format "%s" x))
                   ergoemacs-window-tab-switching) 'paren)))
        (deletion-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar
                   (lambda(x)
                     (format "%s" x))
                   ergoemacs-deletion-functions) 'paren)))
        (movement-regexp
         (format "^%s---ergo$"
                 (regexp-opt
                  (mapcar (lambda(x)
                            (format "%s" x))
                          ergoemacs-movement-functions) 'paren)))
        (standard-shortcuts-regexp
         (regexp-opt (mapcar
                      (lambda(x) (nth 0 x))
                      (symbol-value (ergoemacs-get-fixed-layout))) 'paren))
        (ergo-shortcuts-regexp
         (regexp-opt (mapcar
                      (lambda(x) (nth 0 x))
                      (symbol-value (ergoemacs-get-variable-layout))) 'paren)))
    (with-temp-buffer
      (describe-buffer-bindings (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward (format "^%s\\(  +\\|\t\\)" standard-shortcuts-regexp) nil t)
        (end-of-line)
        (insert "---standard"))
      (goto-char (point-min))
      (while (re-search-forward standard-key-regexp nil t)
        (end-of-line)
        (insert "---cua"))
      (goto-char (point-min))
      (while (re-search-forward "\\(<apps>\\|<menu>\\)" nil t)
        (end-of-line)
        (insert "---apps"))
      (goto-char (point-min))
      (while (re-search-forward (format "^%s\\(  +\\|\t\\)" ergo-shortcuts-regexp) nil t)
        (end-of-line)
        (insert "---ergo"))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-max))
      (insert "\n")
      (goto-char (point-min))
      (while (re-search-forward "<\\([CMS]-\\)" nil t)
        (replace-match "\\1<")
        (backward-char 1))
      (goto-char (point-min))
      (while (re-search-forward "-\\([A-Z]\\)\\>" nil t)
        (unless (save-match-data (looking-at "-"))
          (replace-match (format "S-%s" (downcase (match-string 1))) t nil)))
      (goto-char (point-min))
      (while (re-search-forward ".*:.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "key[ \t]+binding[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "-+[ \t]+-+[ \t]*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward ".*\\(Prefix Command\\|remap\\|XF86\\).*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward ".*\\(mouse\\|kp\\|mute\\|dead\\|A\\|self-insert\\|wheel\\|iso\\|/ergoemacs\\)-.*\n" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "^\\w+$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n"))
      (goto-char (point-min))
      (while (re-search-forward "^[^\t\n]+$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\(  +\\|\t\\)" nil t)
         (goto-char (match-beginning 0))
        (save-restriction
          (narrow-to-region (point-at-bol) (point))
          (goto-char (point-min))
          (while (search-forward "<" nil t)
            (replace-match "<kbd>")
            (when (search-forward ">" nil t)
              (replace-match "</kbd>")))
          (goto-char (point-min))
          (while (re-search-forward "S-" nil t)
            (replace-match "<kbd>⇧Shift</kbd>+" t))
          (goto-char (point-min))
          (while (re-search-forward "C-" nil t)
            (replace-match "<kbd>Ctrl</kbd>+" t t))
          (goto-char (point-min))
          (while (re-search-forward "M-" nil t)
            (replace-match "<kbd>Alt</kbd>+" t t))
          (goto-char (point-min))
          (while (re-search-forward "\\<\\(.\\)\\>" nil t)
            (replace-match "<kbd>\\1</kbd>" t))
          (goto-char (point-min))
          (while (re-search-forward "[+]\\(.\\)\\( \\|$\\)" nil t)
            (replace-match "+<kbd>\\1</kbd>\\2" t))
          (goto-char (point-min))
          (while (re-search-forward " \\(.\\)\\( \\|$\\)" nil t)
            (replace-match " <kbd>\\1</kbd>\\2" t))
          (goto-char (point-min))
          (while (re-search-forward " \\([^>]\\)$" nil t)
            (replace-match " <kbd>\\1</kbd>" t))
          (goto-char (point-min))
          (insert "【")
          (goto-char (point-max))
          (insert "】"))
        (when (looking-at "[ \t]*")
          (replace-match "</td><td>"))
        (beginning-of-line)
        (insert "<tr><td>")
        (end-of-line)
        (insert "</td></tr>"))
      (goto-char (point-min))
      (while (re-search-forward ".*【】.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^.*<td>\\(.*\\)</td></tr>" nil t)
        (if (= 1 (length (match-string 1)))
            (replace-match "")
          (if (intern-soft (save-match-data
                             (replace-regexp-in-string
                              "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" ""
                              (match-string 1))))
              (progn
                (setq shortcut-type "Unclassified")
                (setq short-desc "")
                (dolist (x `(,@(symbol-value (ergoemacs-get-variable-layout))
                             ,@(symbol-value (ergoemacs-get-fixed-layout))))
                  ;; (message "%s" (nth 1 x))
                  ;; (message "%s" (nth 1 (nth 0 x)))
                  (when (string= (format "%s" (nth 1 x))
                                 (save-match-data
                                   (replace-regexp-in-string
                                    "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" "" (match-string 1))))
                    (setq shortcut-type
                          (if (save-match-data (string-match "---apps" (match-string 1)))
                              "Ergonomic Unchorded"
                            (if (save-match-data (string-match "---standard---cua" (match-string 1)))
                                "Standard"
                              (if (save-match-data (string-match "---standard" (match-string 1)))
                                  "Fixed"
                                (if (save-match-data (string-match "---ergo" (match-string 1)))
                                    (if (save-match-data (string-match movement-regexp (match-string 1)))
                                        "Ergonomic Movement"
                                      (if (save-match-data
                                            (string-match deletion-regexp (match-string 1)))
                                          "Ergonomic Deletion"
                                        (if (save-match-data
                                              (string-match tab-regexp (match-string 1)))
                                            "Ergonomic Window/Frame"
                                          "Ergonomic Misc")))
                                  "Duplicate")))))
                    (when (nth 2 x)
                      (setq short-desc (nth 2 x)))))
                (save-restriction
                  (narrow-to-region (point-at-bol) (point-at-eol))
                  (when (re-search-backward "---\\(standard\\(---cua\\)?\\|ergo\\|apps\\)" nil t)
                    (replace-match "")))
                (beginning-of-line)
                (search-forward "<tr>" nil t)
                (insert "<td>")
                (insert shortcut-type)
                (insert "</td>")
                (when (search-forward "</td>")
                  (insert "<td>")
                  (insert short-desc)
                  (insert "</td>"))
                (end-of-line)
                ;; (message "%s" (match-string 1))
                )
            (replace-match ""))))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n"))
      
      (sort-lines nil (point-min) (point-max))
      (buffer-string))))

(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-theme)
(declare-function ergoemacs-get-layouts "ergoemacs-layouts.el")
(declare-function ergoemacs-set-default "ergoemacs-mode.el")
(declare-function ergoemacs-get-themes "ergoemacs-theme-engine.el")

(defun ergoemacs-get-html-key-tables ()
  "Get key table and convert to HTML"
  (let* ((lay (ergoemacs-get-layouts))
         (saved-layout ergoemacs-keyboard-layout)
         (tbl "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta name=\"keywords\" content=\"\" />
    <meta name=\"description\" content=\"\" />
    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
    <title></title>
    <link rel=\"stylesheet\" href=\"../../style/basic.css\" />
    <link rel=\"stylesheet\" href=\"../../style/keys.css\" />
    <script language=\"javascript\" type=\"text/javascript\" src=\"../../stlye/js/tablefilter.js\"></script>
   </head>
  <body>
    <table id=\"table_keys\"><tr><th>Type</th>><th>Key</th><th>Short Desc</th><th>Emacs Function</th></tr>")
         (extra-dir)
         (curr-dir)
         (saved-theme (or ergoemacs-theme "standard")))
    (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
    (when (not (file-exists-p extra-dir))
      (make-directory extra-dir t))
    (dolist (x lay)
      (message "Generate Kbd table for %s" x)
      (ergoemacs-set-default 'ergoemacs-theme nil)
      (ergoemacs-set-default 'ergoemacs-keyboard-layout x)
      (ergoemacs-mode 1)
      (setq curr-dir (expand-file-name "ergo-layouts" extra-dir))
      (when (not (file-exists-p curr-dir))
        (make-directory curr-dir t))
      (with-temp-file (expand-file-name (format "ergoemacs-layout-%s.html" x) curr-dir)
        (insert tbl)
        (insert (ergoemacs-get-html-key-table))
        (insert "</table><script language=\"javascript\" type=\"text/javascript\">
  //<![CDATA[
        var table_keys_Props =      {
                                                        col_0: \"select\", on_change: true, display_all_text: \" [ Show all ] \", rows_counter: true, alternate_rows: true
                                                };
        setFilterGrid( \"table_keys\",table_keys_Props );
//]]>
</script></body></html>"))
      (dolist (y (sort (ergoemacs-get-themes) 'string<))
        (ignore-errors
          (ergoemacs-set-default 'ergoemacs-theme y)
          (ergoemacs-mode 1)
          (message "\tTheme %s" y)
          (setq curr-dir (expand-file-name y extra-dir))
          (when (not (file-exists-p curr-dir))
            (make-directory curr-dir t))
          (with-temp-file (expand-file-name (format "ergoemacs-layout-%s.html" x) curr-dir)
            (insert tbl)
            (insert (ergoemacs-get-html-key-table))
            (insert "</table><script language=\"javascript\" type=\"text/javascript\">
  //<![CDATA[
        var table_keys_Props =      {
                                                        col_0: \"select\"
                                                };
        setFilterGrid( \"table_keys\",table_keys_Props );
//]]>
</script></html>")))))
    (message "Setting theme back to %s" saved-theme)
    (ergoemacs-set-default 'ergoemacs-theme saved-theme)
    (ergoemacs-set-default 'ergoemacs-keyboard-layout saved-layout)
    (ergoemacs-mode 1)
    t))

;; (defvar ergoemacs-fixed-layout)
;; (declare-function ergoemacs-pretty-key "ergoemacs-translate.el")
;; (defun ergoemacs-ghpages-standard-keys ()
;;   "Generate org-mode table for standard keys."
;;   (require 'cl)
;;   (with-temp-buffer
;;     (insert (mapconcat
;;              #'(lambda(var)
;;                  (let* ((key (ergoemacs-pretty-key (nth 0 var)))
;;                         (command (condition-case nil
;;                                      (symbol-name (nth 1 var))
;;                                    (error "")))
;;                         (desc (nth 2 var))
;;                         emacs-key)
;;                    (setq desc
;;                          (replace-regexp-in-string "[+]" "\\\\+" desc))
;;                    (setq emacs-key (condition-case nil
;;                                        (key-description
;;                                         (where-is-internal
;;                                          (nth 1 var)
;;                                          (current-global-map) t))
;;                                      (error "NA")))
;;                    (if (string= emacs-key "")
;;                        (setq emacs-key "NA")
;;                      (setq emacs-key (ergoemacs-pretty-key emacs-key)))
;;                    (concat "| " desc " | " key  " | "
;;                            emacs-key " | =" command "= |")))
;;              (remove-if #'(lambda(x) (not (nth 2 x)))
;;                         ergoemacs-fixed-layout) "\n"))
;;     (sort-lines nil (point-min) (point-max))
;;     (goto-char (point-min))
;;     (insert "|-\n| Standard Name | Ergoemacs Key | Emacs Key | Emacs Command Name |\n|-")
;;     (goto-char (point-max))
;;     (insert "\n|-\n")
;;     (buffer-string)))

(defvar ergoemacs-dir)
;;;###autoload
(defun ergoemacs-ghpages ()
  "Generate github pages with o-blog."
  (interactive)
  (let ((o-blog (expand-file-name (file-name-directory (locate-library "o-blog"))))
        (htmlize (expand-file-name (file-name-directory (locate-library "htmlize"))))
        (emacs-exe (invocation-name))
        (emacs-dir (invocation-directory))
        (full-exe nil))
    (setq full-exe (expand-file-name emacs-exe emacs-dir))
    (if current-prefix-arg
        (shell-command (format "%s -Q --batch -L \"%s\" -L \"%s\" -L \"%s\" -l \"htmlize\" -l \"o-blog\" -l \"ergoemacs-mode\" -l \"ergoemacs-extras\" --eval \"(ergoemacs-publish-blog 1)\" &"
                               full-exe o-blog htmlize ergoemacs-dir))
      (shell-command (format "%s -Q --batch -L \"%s\" -L \"%s\" -L \"%s\" -l \"htmlize\" -l \"o-blog\" -l \"ergoemacs-mode\" -l \"ergoemacs-extras\" --funcall ergoemacs-publish-blog &"
                               full-exe o-blog htmlize ergoemacs-dir)))))

(defun ergoemacs-publish-blog (&optional generate-all-layouts)
  "Internal function for generating o-blog website"
  (when noninteractive
    (setq user-emacs-directory (expand-file-name "out" ergoemacs-dir))
    (when generate-all-layouts
      (message "Generating all layouts")
      (ergoemacs-get-html-key-tables)
      (ergoemacs-extras))
    (find-file (expand-file-name "web.org" ergoemacs-dir))
    (call-interactively 'org-publish-blog)
    (find-file (expand-file-name "out/key-setup.html" ergoemacs-dir))
    (goto-char (point-min))
    (when (re-search-forward "<pre" nil t)
      (insert " id=\"dot_emacs\""))
    ;; Now add [K]ey type headers.
    (let (current-class)
      (dolist (file (let ((default-directory (expand-file-name "out" ergoemacs-dir)))
                      (file-expand-wildcards "*.html" t)))
        (find-file file)
        (setq current-class "dark")
        (goto-char (point-min))
        (while (re-search-forward "\\<\\(Ctr?l\\|Alt\\|\\(?:.\\|&.*?;\\)? ?Shift\\|Shift\\)[+]\\(.\\)" nil t)
          (replace-match "<kbd class=\"dark\">\\1</kbd>+\\2")
          (backward-char)
          (while (looking-at "\\(Ctr?l\\|Alt\\|\\(?:.\\|&.*?;\\)? ?Shift\\|Shift\\)[+]\\(.\\)")
            (replace-match "<kbd class=\"dark\">\\1</kbd>+\\2")
            (backward-char))
          (when (looking-at "\\(.\\)")
            (replace-match "<kbd class=\"dark\">\\1</kbd>")))
        (goto-char (point-min))
        (while (re-search-forward ">Shift</kbd>" nil t)
          (replace-match ">⇧ Shift</kbd>"))
        (goto-char (point-min))
        (let (p1 p2)
          (while (re-search-forward "@@html:" nil t)
            (replace-match "")
            (setq p1 (point))
            (when (search-forward "@@" nil t)
              (replace-match "")
              (setq p2 (point))
              (goto-char p1)
              (while (search-forward "&lt;" p2 t)
                (replace-match "<"))
              (goto-char p1)
              (while (search-forward "&gt;" p2 t)
                (replace-match ">")))))
        (goto-char (point-min))
        (while (re-search-forward "\\(<h[1234].*?>\\)" nil t)
          (re-search-forward "\\=<a.*?>" nil t)
          (when (looking-at "About")
            (goto-char (point-max)) ;; Not displaying properly.  Remove.
            (setq current-class "light"))
          (when (looking-at "\\([A-Za-z0-9]\\)")
            (replace-match (format "<kbd class=\"%s\">\\1</kbd>" current-class) t)))
        (save-buffer (current-buffer))))
    (save-buffer (current-buffer))))

(defun ergoemacs-o-blog-html ()
  "Gets HTML needed for o-blog variant page."
  (let ((extra-dir (expand-file-name "out/ergoemacs-extras" ergoemacs-dir)))
    (when (not (file-exists-p extra-dir))
      (make-directory extra-dir t))
    (ergoemacs-get-html-select)))

(declare-function ergoemacs-emacs-exe "ergoemacs-functions.el")
(defun ergoemacs-get-html ()
  "Gets a HTML description of ErgoEmacs"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-get-html)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (if (not (file-exists-p (expand-file-name "Readme.org" ergoemacs-dir)))
        (message "Not creating HTML information file.  Readme.org was not found.")
      
      (ergoemacs-get-html-key-tables))))

(defun ergoemacs-get-html-select ()
  "Gets the list of all known layouts and the documentation associated with the layouts."
  (let ((lays (sort (ergoemacs-get-layouts) 'string<)))
    (concat
     "<script type=\"text/javascript\">
function change_layout() {
  var select = document.getElementById('select_layout');
  var selection = select.selectedIndex;
  var img = select.options[selection].value;
  select = document.getElementById('select_theme');
  selection = select.selectedIndex;
  var dir = select.options[selection].value;  
  document.getElementById('ergo_image').src = \"ergoemacs-extras/\" + dir + \"/ergoemacs-layout-\" + img + \".png\";
  if (dir == \"kbd-layouts\"){
    dir = \"ergo-layouts\";
  } else {
    var dir2 = dir;
    if (dir2 == \"ergo-layouts\"){
       dir2 = \"nil\";
    } else {
       dir2 = '<span style=\"font-style: italic;\">\"'+dir2+'\"</span>';
    }
    document.getElementById('dot_emacs').innerHTML = '(setq ergoemacs-theme '+dir2+')\\n(setq ergoemacs-keyboard-layout <span style=\"font-style: italic;\">\"' + img + '\"</span>)\\n(require \\'ergoemacs-mode)\\n(ergoemacs-mode 1)';
    }
  document.getElementByTagName('ergo_kbd').src = \"ergoemacs-extras/\" + dir + \"/ergoemacs-layout-\" + img + \".html\";
}
    </script><form><strong>Layout:</strong><select onchange=\"change_layout()\" id=\"select_layout\">\n"
     (mapconcat
      (lambda(lay)
        (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
               (alias (condition-case nil
                          (indirect-variable variable)
                        (error variable)))
               (is-alias nil)
               (doc nil))
          (setq doc (or (documentation-property variable 'variable-documentation)
                        (progn
                          (setq is-alias t)
                          (documentation-property alias 'variable-documentation))))
          (concat "<option value=\"" lay "\"" (if (string= lay "us") " selected" "")
                  ">" doc "(" lay ")"
                  (if is-alias ", alias" "")
                  "</option>")))
      lays "\n")
     "\n</select><br/><strong>Theme:</strong><select onchange=\"change_layout()\" id=\"select_theme\"><option value=\"kbd-layouts\">Keyboard Layout</option><option value=\"ergo-layouts\" selected>Standard</option>"
     (let ((lays (sort (ergoemacs-get-themes) 'string<)))
             (mapconcat
              (lambda(lay)
                (let* ((variable (intern (concat "ergoemacs-" lay "-theme")))
                       (alias (condition-case nil
                                  (indirect-variable variable)
                                (error variable)))
                       (is-alias nil)
                       (doc nil))
                  (setq doc (or (documentation-property variable 'group-documentation)
                                (progn
                                  (setq is-alias t)
                                  (documentation-property alias 'group-documentation))))
                  (concat "<option value=\"" lay "\">" lay " - "
                          (replace-regexp-in-string
                           ">" "&lt;"
                           (replace-regexp-in-string "<" "&gt;" doc)) "</option>")))
              lays "\n"))
     "</select></form><br /><image id=\"ergo_image\" src=\"ergoemacs-extras/ergo-layouts/ergoemacs-layout-us.png\" /><br /><iframe id=\"ergo_kbd\" src=\"ergoemacs-extras/ergo-layouts/ergoemacs-layout-us.html\"
  width=\"100%\" height=\"400\"></iframe>")))

(defun ergoemacs-trans-mac-osx (key &optional swap-option-and-control)
  "Translates Emacs kbd code KEY to Mac OS X DefaultKeyBinding.dict"
  (let ((ret key)
        (case-fold-search t))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\<M-" nil t)
        (replace-match (if swap-option-and-control "^" "~") nil t))
      (goto-char (point-min))
      (while (re-search-forward "\\<C-" nil t)
        (replace-match (if swap-option-and-control "~" "^") nil t))
      (setq ret (buffer-string)))
    ret))

(declare-function ergoemacs-setup-keys-for-layout "ergoemacs-translate.el")
(declare-function ergoemacs-kbd "ergoemacs-translate.el")
(defun ergoemacs-gen-mac-osx (layout &optional file-name extra swap-opiton-and-control)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "os_x_qwerty.dict.txt"))
        (xtra (or extra "os_x_opt_meta"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      ;; Translate keys
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout ".dict. txt") extra-dir))
      (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name fn dir))
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (dolist (x (symbol-value (ergoemacs-get-variable-layout)))
          (let ((from (nth 0 x))
                from-reg
                (to nil))
            (setq to (ergoemacs-kbd from t))
            (if (string= from to) nil
              
              (setq from (ergoemacs-trans-mac-osx from t))
              (setq to (ergoemacs-trans-mac-osx to swap-opiton-and-control))
              (setq from-reg (regexp-quote from))
              (goto-char (point-min))
              (when (re-search-forward from-reg nil t)
                (replace-match to t t)))))
        (goto-char (point-min))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-mac-osx-dicts (&optional layouts)
  "Generate Mac OS X dictionaries for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (dolist (x lay)
      (message "Generate Mac Dictionary for %s" x)
      (ergoemacs-gen-mac-osx x)
      (ergoemacs-gen-mac-osx x nil "os_x_opt-ctl" t))))

(defun ergoemacs-trans-bash (key)
  "Translate Emacs kbd code KEY to bash kbd code"
  (let ((ret key)
        (case-fold-search nil))
    (with-temp-buffer
      (insert ret)
      (goto-char (point-min))
      (while (re-search-forward "\\([MSC]-\\)" nil t)
        (replace-match "\\\\\\1"))
      (setq ret (buffer-string)))
    ret))

;;;###autoload
(defun ergoemacs-bash ()
  "Generates `~/.inputrc' to use ergoemacs-keys in bash.  This is
based on ergoemacs' current theme and layout."
  (interactive)
  (when (file-exists-p "~/.inputrc")
    (if (yes-or-no-p "Would you like to overwrite ~/.inputrc ?")
        (delete-file "~/.inputrc")
      (error "Cannot generate ergoemacs ~/.inputrc")))
  (ergoemacs-gen-bash ergoemacs-keyboard-layout "~/.inputrc")
  (message "~/.inputrc set to ergoemacs keys."))

(defun ergoemacs-gen-bash (layout &optional file-name extra)
  "Generates an Autohotkey Script for Ergoemacs Keybindings.
Currently only supports two modifier plus key.
FILE-NAME is the input file.  Or if FILE-NAME = ~/.inputrc, generate to the ~/.inputrc using bash-us.txt

EXTRA is the extra directory used to gerenate the bash ~/.inputrc
"
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "bash-us.txt"))
        (xtra (or extra "bash"))
        file
        txt
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (i 0))
    
    ;; ergoemacs-variable-layout
    (if (not lay)
        (message "Layout %s not found" layout)
      (ergoemacs-setup-keys-for-layout layout)
      (if (and fn (string= fn "~/.inputrc"))
          (setq file "~/.inputrc")
        (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
        (if (not (file-exists-p extra-dir))
            (make-directory extra-dir t))
        (setq extra-dir (expand-file-name xtra extra-dir))
        (if (not (file-exists-p extra-dir))
            (make-directory extra-dir t))
        
        ;; Translate keys
        (setq file (expand-file-name
                    (concat "ergoemacs-layout-" layout ".txt") extra-dir)))
      
      (with-temp-file file
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (if (and fn (string= fn "~/.inputrc"))
                                  (expand-file-name "bash-us.txt" dir)
                                (expand-file-name fn dir)) )
        (goto-char (point-min))
        (when (re-search-forward "QWERTY")
          (replace-match layout))
        (dolist (x (symbol-value (ergoemacs-get-variable-layout)))
          (let ((from (nth 0 x))
                from-reg
                (to nil))
            (setq to (ergoemacs-kbd from t))
            (if (string= from to) nil
              
              (setq from (ergoemacs-trans-bash from))
              (setq to (ergoemacs-trans-bash to))
              (setq from-reg (regexp-quote from))
              (goto-char (point-min))
              (when (re-search-forward from-reg nil t)
                (replace-match to t t)))))
        (goto-char (point-min))
        (while (re-search-forward "\"\\\\M-" nil t)
          (replace-match "\"\\e" t t))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)))))

(defun ergoemacs-bashs (&optional layouts)
  "Generate BASH scripts for all the defined layouts."
  (interactive)
  (let ((lay (or layouts (ergoemacs-get-layouts))))
    (dolist (x lay)
      (message "Generate bash for %s" x)
      (ergoemacs-gen-bash x))))

(defun ergoemacs-trans-ahk (key &optional number)
  "Translates Emacs kbd code KEY to ahk kbd code. "
  (let ((ret key)
        (mod-code 0)
        (case-fold-search nil))
    (while (string-match "-\\([A-Z]\\)\\($\\| \\)" ret)
      (setq ret (replace-match
                 (concat "-S-"
                         (downcase (match-string 1 ret))
                         (match-string 2 ret))
                 t t ret)))
    (while (string-match "M-" ret)
      (setq mod-code (+ mod-code 2))
      (setq ret (replace-match (if number "" "!") t t ret)))
    (while (string-match "S-" ret)
      (setq mod-code (+ mod-code 8))
      (setq ret (replace-match (if number "" "+") t t ret)))
    (while (string-match "C-" ret)
      (setq mod-code (+ mod-code 4))
      (setq ret (replace-match (if number "" "^") t t ret)))
    (if (and number (= 1 (length ret)))
        (setq ret (format "%s%s" (string-to-char ret) (number-to-string mod-code))))
    ret))

(defun ergoemacs-get-layouts-ahk-ini ()
  "Gets the easymenu entry for ergoemacs-layouts."
  (let ((lay-ini "")
        (trans-keys '())
        (i 0))
    (with-temp-buffer
      (insert "[Layouts]\n")
      (dolist (lay (ergoemacs-get-layouts))
        (let* ((variable (intern (concat "ergoemacs-layout-" lay)))
               (alias (condition-case nil
                          (indirect-variable variable)
                        (error variable)))
               (is-alias nil)
               (doc nil))
          (setq doc (or (documentation-property variable 'variable-documentation)
                        (progn
                          (setq is-alias t)
                          (documentation-property alias 'variable-documentation))))
          (insert lay)
          (insert "=")
          (insert doc)
          (insert "\n")
          (setq i 1)
          (setq lay-ini (format "%s\n[%s]" lay-ini lay))
          (dolist (x (symbol-value variable))
            (unless (string-match "\\(<apps>\\|<menu>\\)" x) ;; Currently take out
              ;; <apps> mapping.  Needs some work.
              (let ((key (format "%s" (string-to-char x))))
                (push `(,x ,key) trans-keys)
                (setq lay-ini (format "%s\n%s=%s" lay-ini i key))))
            (setq i (+ i 1)))))
      (goto-char (point-max))
      (insert lay-ini)
      (insert "\n")
      (buffer-string))))

(defvar ergoemacs-theme-hash)
(defun ergoemacs-get-themes-ahk-ini ()
  "Gets the list of all known themes and the documentation associated with the themes."
  (with-temp-buffer
    (insert "[Themes]\n")
    (let ((lays (sort (ergoemacs-get-themes) 'string<)))
      (dolist (lay lays)
        (insert lay "="
                (plist-get (gethash lay ergoemacs-theme-hash) ':description) "\n")))
    (buffer-string)))

(defun ergoemacs-get-ahk-keys-ini ()
  "Get ahk keys for all themes/ahk combinations and put into INI file."
  (let ((re "")
        (lst '()))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ \n]*\\):" nil t)
        (push (match-string 1) lst)))
    (setq re (format "^%s$" (regexp-opt lst 't)))
    (with-temp-buffer
      (let ((old-lay (or ergoemacs-theme "standard")))
        (dolist (z (ergoemacs-get-themes))
          (ergoemacs-set-default 'ergoemacs-theme z)
          (dolist (x (ergoemacs-get-layouts))
            (ergoemacs-setup-keys-for-layout x)
            (insert (concat "[" x "-" z "]\n"))
            (message "Generating AHK ini for %s %s" x z)
            (dolist (y (symbol-value (ergoemacs-get-variable-layout)))
              (when (string-match re (format "%s" (nth 1 y)))
                (let ((trans (ergoemacs-trans-ahk (ergoemacs-kbd (nth 0 y) t (nth 3 y)) t)))
                  (when (string-match "^[0-9]+$" trans)
                    (insert (symbol-name (nth 1 y)) "=" trans "\n")))))))
        (ergoemacs-setup-keys-for-layout ergoemacs-keyboard-layout)
        (ergoemacs-set-default 'ergoemacs-theme old-lay))
      (buffer-string))))

(defun ergoemacs-gen-ahk (&optional extra)
  "Generates autohotkey for all layouts and themes"
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-gen-ahk)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (let ((xtra (or extra "ahk"))
          (extra-dir)
          file-temp)
      (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq file-temp (expand-file-name "ergoemacs.ini" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert (ergoemacs-get-layouts-ahk-ini))
        (insert (ergoemacs-get-themes-ahk-ini))
        (insert (ergoemacs-get-ahk-keys-ini)))
      (setq file-temp (expand-file-name "ergoemacs.ahk" extra-dir))
      (with-temp-file file-temp
        (set-buffer-file-coding-system 'utf-8)
        (insert-file-contents (expand-file-name "ahk-us.ahk" ergoemacs-dir)))
      (message "Generated ergoemacs.ahk")
      (when (executable-find "ahk2exe")
        (shell-command (format "ahk2exe /in %s" file-temp))
        (message "Generated ergoemacs.exe")))))

(defvar ergoemacs-mode)
;;;###autoload
(defun ergoemacs-extras ( &optional layouts)
  "Generate layout diagram, and other scripts for system-wide ErgoEmacs keybinding.

The following are generated:
• SVG Diagram for ErgoEmacs command layouts in SVG format.
• Bash 〔.inputrc〕 code.
• Mac OS X 〔DefaultKeyBinding.dict〕 code.
• AutoHotkey script for Microsoft Windows.

Files are generated in the dir 〔ergoemacs-extras〕 at `user-emacs-directory'."
  (interactive)
  (if (called-interactively-p 'any)
      (progn
        (shell-command (format "%s -Q --batch -l %s/ergoemacs-mode %s/ergoemacs-extras --eval \"(ergoemacs-extras)\" &"
                               (ergoemacs-emacs-exe)
                               ergoemacs-dir ergoemacs-dir)))
    (unless ergoemacs-mode
      (ergoemacs-mode 1))
    (ergoemacs-svgs layouts)
    (ergoemacs-gen-ahk)
    (ergoemacs-bashs layouts)
    (ergoemacs-mac-osx-dicts layouts)
    ;; (ergoemacs-get-html)
    ;; (find-file (expand-file-name "ergoemacs-extras" user-emacs-directory))
    ))

;;FIXME
(defvar ergoemacs-select)
(defvar ergoemacs-html-table)
(defun ergoemacs-keyfreq-gen-img (file prefix text shift lay cmd-freq-ergo)
  "Ergoemacs keyfreq generate image."
  (let (ret i tmp)
    (with-temp-file file
      (insert-file-contents (expand-file-name "kbd-ergo.svg" ergoemacs-dir))
      ;; Change all text to black
      (goto-char (point-min))
      (while (re-search-forward "<text" nil t)
        (when (re-search-forward "fill:#.*?;" nil t)
          (replace-match "fill:#000000;")))
      ;; Change the letters to A, B, C, D, and E
      (goto-char (point-min))
      (while (re-search-forward ">\\([MCA]\\)\\([0-9]+\\)<" nil t)
        (cond
         ((string= "A" (match-string 1))
          (replace-match ">E\\2<" t))
         ((and (string= "C" (match-string 1))
               (<= 60 (string-to-number (match-string 2))))
          (replace-match (format ">C%s<" (- (string-to-number (match-string 2)) 60))))
         ((and (string= "M" (match-string 1))
               (<= 60 (string-to-number (match-string 2))))
          (replace-match (format ">A%s<" (- (string-to-number (match-string 2)) 60))))
         ((string= "M" (match-string 1))
          (replace-match ">B\\2<"))
         ((string= "C" (match-string 1))
          (replace-match ">D\\2<"))))
      
      ;; Now add the layout information.
      (setq i 0)
      ;; (setq cmd-freq '())
      (while (< i (length lay))
        (goto-char (point-min))
        (when (search-forward (format ">%s<" i) nil t)
          (if (string= "" (nth (+ (if shift 60 0) i) lay))
              (replace-match "><")
            (replace-match (format ">%s<"
                                   (ergoemacs-gen-svg-quote
                                    (upcase (nth
                                             (+ (if shift 60 0) i)
                                             lay)))) t t)))
        (setq tmp (assoc (format "%s%s" prefix
                                 (nth (+ (if shift 60 0) i) lay))
                         cmd-freq-ergo))
        (if (not tmp)
            (progn
              ;; Try to figure out if this is a
              ;; prefix, or not...
              (setq tmp (all-completions
                         (format "%s%s " prefix (nth (+ (if shift 60 0) i) lay))
                         cmd-freq-ergo))
              (if tmp
                  (progn
                    (setq ret t)
                    (goto-char (point-min))
                    (while (re-search-forward (format ">A%s<" i) nil t)
                      (replace-match ">♦<"))
                    
                    (goto-char (point-min))
                    (while (re-search-forward (format ">[BCDE]%s<" i) nil t)
                      (replace-match "><"))
                    
                    (goto-char (point-min))
                    (when (re-search-forward (format "id=\"key%s\"" i) nil t)
                      (when (re-search-backward "fill:.*?;" nil t)
                        (replace-match "fill:#00FFFF;")))
                    (let ((new-file
                           (replace-regexp-in-string
                            ".svg"
                            (format "-%s%s.svg"
                                    (if shift "S-" "")
                                    (nth (+ (if shift 60 0) i) lay))
                            file)))
                      
                      (if (ergoemacs-keyfreq-gen-img new-file
                                   (format "%s%s "
                                           prefix
                                           (nth (+ (if shift 60 0) i) lay))
                                   (format "%s%s" text
                                           (nth (+ (if shift 60 0) i) lay)) nil lay cmd-freq-ergo)
                          (progn
                            (setq ergoemacs-select
                                  (format "%s<option value=\"%s\">%s</option>"
                                          ergoemacs-select
                                          (file-name-nondirectory new-file)
                                          (format "%s%s" text
                                                  (nth (+ (if shift 60 0) i) lay)))))
                        (delete-file new-file))

                      (setq new-file
                            (replace-regexp-in-string
                             ".svg" "-S.svg"
                             new-file))
                      (if (ergoemacs-keyfreq-gen-img new-file
                                   (format "%s%s "
                                           prefix
                                           (nth (+ (if shift 60 0) i) lay))
                                   (format "%s%s ⇧Shift+" text
                                           (nth (+ (if shift 60 0) i) lay)) t lay cmd-freq-ergo)
                          (progn
                            (setq ergoemacs-select
                                  (format "%s<option value=\"%s\">%s</option>"
                                          ergoemacs-select
                                          (file-name-nondirectory new-file)
                                          (format "%s%s ⇧Shift+" text
                                                  (nth (+ (if shift 60 0) i) lay)))))
                        (delete-file new-file))))
                (goto-char (point-min))
                (while (re-search-forward (format ">[ABCDE]%s<" i) nil t)
                  (replace-match "><"))
                (goto-char (point-min))
                (when (re-search-forward (format "id=\"key%s\"" i) nil t)
                  (when (re-search-backward "fill:.*?;" nil t)
                    (replace-match "fill:#FFFF00;")))))
          (setq ret t)
          (goto-char (point-min))
          (push `(,(nth 2 tmp) ,(format "<tr><td style=\"background-color: %s\">%s</td><td style=\"background-color: %s\"><input type=\"text\" value=\"%s\"></td><td style=\"background-color: %s\">%s</td></tr>"
                                        (nth 6 tmp) (nth 2 tmp)
                                        (nth 6 tmp) (nth 1 tmp)
                                        (nth 6 tmp) (nth 4 tmp))) ergoemacs-html-table)
          (when (search-forward (format "id=\"key%s\"" i) nil t)
            (when (re-search-backward "fill:.*?;" nil t)
              (replace-match (format "fill:%s;" (nth 6 tmp)))))
          (goto-char (point-min))
          (when (search-forward (format ">A%s<" i) nil t)
            (replace-match (format ">%s<" (nth 1 tmp)) t t))
          (goto-char (point-min))
          (when (search-forward (format ">B%s<" i) nil t)
            (replace-match (format ">N: %s<" (nth 2 tmp)) t t))
          (goto-char (point-min))
          (when (search-forward (format ">C%s<" i) nil t)
            (replace-match (format ">NC: %s<" (length (nth 3 tmp))) t t))
          (goto-char (point-min))
          (when (search-forward (format ">D%s<" i) nil t)
            (replace-match (format ">Cmd: %s<" (nth 4 tmp)) t t))
          (goto-char (point-min))
          (when (search-forward (format ">E%s<" i) nil t)
            (replace-match (format ">Tot: %s<" (nth 5 tmp)) t t)))
        (setq i (+ i 1)))
      ;; Now lookup prefix-SPC
      (setq tmp (assoc (format "%s%sSPC" prefix (if shift "S-" ""))
                       cmd-freq-ergo))
      (if (not tmp)
          (progn
            (goto-char (point-min))
            (while (re-search-forward ">\\(MS\\|MM\\|CS\\|CC\\|AA\\)-SPC<" nil t)
              (replace-match "><"))
            (goto-char (point-min))
            (when (re-search-forward "id=\"keySPC\"" nil t)
              (when (re-search-backward "fill:.*?;" nil t)
                (replace-match "fill:#FFFF00;"))))
        ;;"id=\"keySPC\"")
        (setq ret t)
        (goto-char (point-min))
        (when (search-forward ">MS-SPC<" nil t)
          (replace-match (format ">%s<" (nth 1 tmp)) t t))
        (goto-char (point-min))
        (when (search-forward ">MM-SPC<" nil t)
          (replace-match (format ">N: %s<" (nth 2 tmp)) t t))
        (goto-char (point-min))
        (when (search-forward ">CS-SPC<" nil t)
          (replace-match (format ">NC: %s<" (length (nth 3 tmp))) t t))
        (goto-char (point-min))
        (when (search-forward ">CC-SPC<" nil t)
          (replace-match (format ">Cmd: %s<" (nth 4 tmp)) t t))
        (goto-char (point-min))
        (when (search-forward ">AA-SPC<" nil t)
          (replace-match (format ">Tot: %s<" (nth 5 tmp)) t t)))
      (goto-char (point-min))
      (when (search-forward ">title<" nil t)
        (replace-match (format ">Frequency Heatmap for %s<" text)))
      (goto-char (point-min))
      (when (search-forward ">MS<" nil t)
        (replace-match ">N: Number of times called<" t))
      (goto-char (point-min))
      (when (search-forward ">MM<" nil t)
        (replace-match ">NC: Number of commands for this key<" t))
      (goto-char (point-min))
      (when (search-forward ">CS<" nil t)
        (replace-match ">Cmd: % of emacs keyboard commands<" t))
      (goto-char (point-min))
      (when (search-forward ">CC<" nil t)
        (replace-match ">Tot: % of typing<" t))
      (goto-char (point-min))
      (while (re-search-forward ">\\(AA\\)<" nil t)
        (replace-match "><" t)))
    ret))

(declare-function ergoemacs-real-key-binding "ergoemacs-advices.el" (key &optional accept-default no-remap position) t)
(defun ergoemacs-keyfreq-calc-ergo (x list var-layout cmd-n total-n)
  "Calculate keyfreq based on ergoemacs positions."
  (let ((a (assoc (nth 1 x) (cdr list)))
        curr-cmd
        (cmds '())
        (num 0))
    (when a
      (setq num (+ num (cdr a)))
      (push (car a) cmds))
    ;; Now lookup key based on the currently installed
    ;; minor modes

    ;; For example with subword-mode, backward-word
    ;; becomes subword-backward-word
    (setq curr-cmd
          (ergoemacs-real-key-binding (if var-layout
                           (ergoemacs-kbd (nth 0 x) nil (nth 3 x))
                         (read-kbd-macro (nth 0 x))) t))
    
    (unless (memq curr-cmd cmds)
      (setq a (assoc curr-cmd (cdr list)))
      (when a
        (setq num (+ num (cdr a)))
        (push (car a) cmds)))
    ;; Also lookup based on any compatibility fixes with
    ;; made by ergoemacs.
    ;; (mapc
    ;;  (lambda(minor-list)
    ;;    (when (eq (type-of (nth 1 minor-list)) 'cons)
    ;;      (mapc
    ;;       (lambda(translation-list)
    ;;         (when (eq (nth 1 x) (nth 1 translation-list))
    ;;           (setq a (assoc (nth 1 translation-list) (cdr list)))
    ;;           (when a
    ;;             (setq num (+ num (cdr a)))
    ;;             (push (car a) cmds))))
    ;;       (nth 1 minor-list))))
    ;;  (symbol-value (ergoemacs-get-minor-mode-layout)))
    (list (if var-layout
              (ergoemacs-kbd (nth 0 x) t (nth 3 x))
            (nth 0 x)) (nth 2 x)  num cmds
            (format "%6.2f%%" (/ (* 1e2 num) cmd-n))
            (format "%6.2f%%" (/ (* 1e2 num) total-n)))))

(defvar keyfreq-table)
(declare-function keyfreq-table-load "keyfreq.el")
(declare-function keyfreq-list "keyfreq.el")
(declare-function keyfreq-groups-major-modes "keyfreq.el")
;;;###autoload
(defun ergoemacs-keyfreq-image ()
  "Create heatmap keyfreq images, based on the current layout."
  (interactive)
  (if (not (featurep 'keyfreq))
      (error "This requires the package `keyfreq'")
    (message "Calculating key frequencies based on key-position")
    (let ((table (copy-hash-table keyfreq-table))
          list
          file
          (total-n 0)
          (cmd-n 0)
          (i 0)
          i2
          cmd-freq-ergo
          tmp
          (ergoemacs-select "")
          (ergoemacs-html-table '())
          (lay (or (intern-soft (format "ergoemacs-layout-%s"
                                        ergoemacs-keyboard-layout))
                   'ergoemacs-layout-us))
          var-layout)
      (setq lay (symbol-value lay))
      ;; Merge with the values in .emacs.keyfreq file
      (keyfreq-table-load table)
      (setq list (keyfreq-list (keyfreq-groups-major-modes table) 'no-sort))
      (dolist (x (cdr list))
        (setq total-n (+ total-n (cdr x)))
         (unless (string-match "self-insert" (symbol-name (car x)))
           (setq cmd-n (+ cmd-n (cdr x)))))

      ;; Get the frequencies for all the ergoemacs commands
      (setq cmd-freq-ergo
            (mapcar
             (lambda(x)
               (ergoemacs-keyfreq-calc-ergo x list var-layout cmd-n total-n))
             (append
              (symbol-value (ergoemacs-get-fixed-layout)))))
      
      (setq cmd-freq-ergo
            (append cmd-freq-ergo
                    (mapcar
                     (lambda(x)
                       (ergoemacs-keyfreq-calc-ergo x list var-layout cmd-n total-n))
                     (append
                      (symbol-value (ergoemacs-get-variable-layout))))))
      
      (setq cmd-freq-ergo (sort cmd-freq-ergo #'(lambda(x y) (< (nth 2 x) (nth 2 y)))))

      ;; Consolidated color calculation
      (setq i2 (/ (* 1e0 (length cmd-freq-ergo)) 2.0))
      (setq i 0)
      
      (setq cmd-freq-ergo
            (mapcar
             (lambda(x)
               (let (tmp color)
                 (cond
                  ((< i i2)
                   (setq tmp (* 255e0 (/ (* 1e0 i) (* 1e0 i2))))
                   (setq color (format "#%02X%02Xff" tmp tmp)))
                  (t
                   (setq tmp (* 255e0 (- 1e0 (/ (- (* 1e0 i) (* 1e0 i2)) (* 1e0 i2)) )))
                   (setq color (format "#ff%02X%02X" tmp tmp))))
                 (setq i (+ i 1))
                 (append x (list color))))
             cmd-freq-ergo))
      
      (let (extra-dir)
        (setq extra-dir (expand-file-name "ergoemacs-extras" user-emacs-directory))
        (if (not (file-exists-p extra-dir))
            (make-directory extra-dir t))
        (setq file (expand-file-name  "keyfreq-alt-map.svg" extra-dir))
        (when (ergoemacs-keyfreq-gen-img file "M-" "Alt+" nil lay cmd-freq-ergo)
          (setq ergoemacs-select
                (format "%s<option value=\"%s\">Alt+</option>"
                        ergoemacs-select
                        (file-name-nondirectory file))))
        (message "Generated Alt+ frequency heatmap")
        
        (setq file (expand-file-name  "keyfreq-alt-shift-map.svg" extra-dir))
        (when (ergoemacs-keyfreq-gen-img file "M-" "Alt+⇧Shift+" t lay cmd-freq-ergo)
          (setq ergoemacs-select
                (format "%s<option value=\"%s\">Alt+⇧Shift+</option>"
                        ergoemacs-select
                        (file-name-nondirectory file))))
        (message "Generated Alt+⇧Shift+ frequency heatmap")
        
        (setq file (expand-file-name  "keyfreq-ctrl-map.svg" extra-dir))
        (when (ergoemacs-keyfreq-gen-img file "C-" "Ctrl+" nil lay cmd-freq-ergo)
          (setq ergoemacs-select
                (format "%s<option value=\"%s\">Ctrl+</option>"
                        ergoemacs-select
                        (file-name-nondirectory file))))
        (message "Generated Ctrl+ frequency heatmap")
        
        (setq file (expand-file-name  "keyfreq-ctrl-shift-map.svg" extra-dir))
        (when (ergoemacs-keyfreq-gen-img file "C-" "Ctrl+⇧Shift+" t lay cmd-freq-ergo)
          (setq ergoemacs-select
                (format "%s<option value=\"%s\">Ctrl+⇧Shift+</option>"
                        ergoemacs-select
                        (file-name-nondirectory file))))
        (message "Generated Ctrl+⇧Shift+ frequency heatmap")

        (setq file (expand-file-name  "keyfreq-menu-map.svg" extra-dir))
        (when (ergoemacs-keyfreq-gen-img file (if (eq system-type 'windows-nt)
                                                  "<apps> "
                                                "<menu> ") "▤ Menu/Apps " nil lay cmd-freq-ergo)
          (setq ergoemacs-select
                (format "%s<option value=\"%s\">▤ Menu/Apps</option>"
                        ergoemacs-select
                        (file-name-nondirectory file))))
        (message "Generated ▤ Menu/Apps")
        (setq ergoemacs-html-table (sort ergoemacs-html-table (lambda(x y) (>= (nth 0 x) (nth 0 y)))))
        
        (setq ergoemacs-select (format "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta name=\"keywords\" content=\"\" />
    <meta name=\"description\" content=\"\" />
    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
    <title>Keyboard based key frequency</title>
<script type=\"text/javascript\">
function change_keyfreq_layout() {
  var select = document.getElementById('keyfreq');
  var selection = select.selectedIndex;
  var img = select.options[selection].value;
  document.getElementById('keyfreq_img').src =  img;
}
    </script><body><form><b>Keyboard Modifiers:</b>&nbsp;&nbsp;<select id=\"keyfreq\" onchange=\"change_keyfreq_layout()\">%s</select></form><image id=\"keyfreq_img\" src=\"keyfreq-alt-map.svg\"/><form><table>"
                                       ergoemacs-select))
        (with-temp-file (expand-file-name "keyfreq.html"
                                          (expand-file-name "ergoemacs-extras" user-emacs-directory))
          (insert ergoemacs-select)
          (mapc
           (lambda(x)
             (insert (nth 1 x)))
                 ergoemacs-html-table)
          (insert "</table></form></body></html>")))
      )))

;; Allow the SVN prefixes to be specified by the following:
(defvar ergoemacs-svn-prefixes
      '(("M-"      "M" "MS" "Alt/⌘+⇧Shift+" "M-S-"     nil)
        ("M-"      "M" "MM" "Alt/⌘+"         "M-"       nil)
        ("C-"      "C" "CS" "Ctrl+⇧Shift+"  "C-S-"     nil)
        ("C-"      "C" "CC" "Ctrl+"          "C-"      nil)
        ("<apps> " "A" "AA" "▤ Menu/Apps "  "<apps> " nil)))

;;; Format of list -- (0) Emacs prefix (1) svg prefix (2) Final/Symbol
;;; prefix (3) Final text legend (4) Keyboard lookup (5) Treat
;;; variable-layout as fixed-layout

(defvar ergoemacs-translation-from)
(defvar ergoemacs-inkscape)
(defvar ergoemacs-convert)
(declare-function ergoemacs-theme-component-map-list-md5 "ergoemacs-theme-engine.el")
(defvar ergoemacs-theme--object)
(defun ergoemacs-gen-svg (layout &optional file-name extra is-prefix)
  "Generates a SVG picture of the layout
FILE-NAME represents the SVG template
EXTRA represents an extra file representation.
IS-PREFIX tell ergoemacs if this is a prefix diagram."
  (let ((dir ergoemacs-dir)
        (extra-dir)
        (fn (or file-name "kbd.svg"))
        (xtra (or extra "kbd-layouts"))
        file
        txt
        (prefix-lst '())
        (prefix-num 0)
        prefix-fixed
        (lay
         (intern-soft
          (concat "ergoemacs-layout-" layout)))
        (fix (mapcar
              (lambda(x)
                `(,(if (condition-case nil
                           (string-match "-S-\\([a-z]\\)\\>" (nth 0 x))
                         (error nil))
                       (replace-match (format "-%s" (upcase (match-string 1 (nth 0 x)))) t t (nth 0 x))
                     (nth 0 x))  ,(nth 1 x) ,(nth 2 x)))
              `(,@(symbol-value (ergoemacs-get-fixed-layout))
                ,@(if cua-mode
                      `(("C-c" nil "Copy")
                        ("C-v" nil "Paste")
                        ("C-x" nil "Cut"))
                    nil))))
        (i 0))
    (if (not lay)
        (message "Layout %s not found" layout)
      (setq extra-dir
            (expand-file-name "ergoemacs-extras" user-emacs-directory))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq extra-dir (expand-file-name xtra extra-dir))
      (if (not (file-exists-p extra-dir))
          (make-directory extra-dir t))
      (setq lay (symbol-value lay))
      (setq file (expand-file-name
                  (concat "ergoemacs-layout-" layout
                          (if is-prefix "-prefix" "")
                          (if ergoemacs-theme--object
                              (concat "-" (ergoemacs-theme-component-map-list-md5 ergoemacs-theme--object))
                            "")".svg")
                  extra-dir))
      (with-temp-file file
        ;;(set-buffer-file-coding-system 'utf-8)
        (insert-file-contents
         (expand-file-name fn dir))
        (when is-prefix
          (goto-char (point-min))
          (while (search-forward ">A" nil t)
            (replace-match ">4A"))
          (goto-char (point-min))
          (while (re-search-forward ">M\\([0-9]+\\)<" nil t)
            (if (<= 60 (string-to-number (match-string 1)))
                (replace-match (format ">0A%s<" (- (string-to-number (match-string 1)) 60)) t t)
              (replace-match (format ">1A%s<" (match-string 1)) t t)))
          (goto-char (point-min))
          (while (re-search-forward ">C\\([0-9]+\\)<" nil t)
            (if (<= 60 (string-to-number (match-string 1)))
                (replace-match (format ">2A%s<" (- (string-to-number (match-string 1)) 60)) t t)
              (replace-match (format ">3A%s<" (match-string 1)) t t)))
          (goto-char (point-min))
          (while (re-search-forward ">MS" nil t)
            (replace-match ">0AA" t t))
          (goto-char (point-min))
          (while (re-search-forward ">MM" nil t)
            (replace-match ">1AA" t t))
          (goto-char (point-min))
          (while (re-search-forward ">CS" nil t)
            (replace-match ">2AA"))
          (goto-char (point-min))
          (while (re-search-forward ">CC" nil t)
            (replace-match ">3AA")))
        (when (string-equal system-type "windows-nt")
          ;; Use Arial Unicode MS when on windows
          (goto-char (point-min))
          (while (re-search-forward "\\(?:Helvetica\\|Sans\\)\\([\";]\\)" nil t)
            (replace-match "Arial Unicode MS\\1")))
        (while (< i (length lay))
          (goto-char (point-min))
          (when (search-forward (format ">T%s<" i) nil t)
            (replace-match
             (format ">%s<"
                     (ergoemacs-gen-svg-quote (nth i lay))) t t))
          (dolist (x ergoemacs-svn-prefixes)
            (let ((key-pre (nth 0 x))
                  (rep-pre (nth 1 x))
                  (rep-sym-pre (nth 2 x))
                  (final-txt (nth 3 x))
                  (sym-pre (nth 4 x))
                  (var-is-fixed-p (nth 5 x))
                  curr-var)
              ;; Variable Layout
              (setq curr-var (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from)))))
              
              (if var-is-fixed-p
                  (setq curr-var (nth i lay))
                (setq curr-var (nth i (symbol-value (intern (concat "ergoemacs-layout-" ergoemacs-translation-from))))))
              
              (unless (string= curr-var "")
                (setq txt (assoc (format "%s%s" key-pre curr-var)
                                 (symbol-value (ergoemacs-get-variable-layout))))
                
                (if (not txt)
                    (setq txt "")
                  (if (>= (length txt) 3)
                      (setq txt (nth 2 txt))
                    (setq txt "")))
                (when (string= "" txt)
                  (setq txt (all-completions (format "%s%s " key-pre curr-var)
                                             (symbol-value (ergoemacs-get-variable-layout))))
                  (if (= 0 (length txt))
                      (setq txt "")
                    (setq prefix-fixed (nth 3 (assoc (nth 0 txt) (symbol-value (ergoemacs-get-variable-layout)))))
                    (setq txt "⌨")
                    ;; Add to prefix list
                    (setq prefix-lst
                          (append prefix-lst
                                  (list
                                   (list
                                    (format "%s%s " key-pre curr-var)
                                    (format "%sA" prefix-num)
                                    (format "%sAA" prefix-num)
                                    (format "%s%s" final-txt (nth i lay))
                                    (format "%sAA-" prefix-num)
                                    prefix-fixed))))
                    (setq prefix-num (+ prefix-num 1))))
                (goto-char (point-min))
                (unless (string= "" txt)
                  (when (search-forward (format ">%s%s<" rep-pre i) nil t)
                    (replace-match  (format ">%s<" txt) t t))))
              
              ;; Fixed layout
              (goto-char (point-min))
              (setq txt (assoc (format "%s%s" key-pre (nth i lay)) fix))
              (if (not txt)
                  (setq txt "")
                (if (>= (length txt) 3)
                    (setq txt (nth 2 txt))
                  (setq txt "")))
              (when (string= txt "")
                (setq txt (all-completions (format "%s%s " key-pre (nth i lay)) fix))
                (if (= 0 (length txt))
                    (setq txt "")
                  (setq txt "⌨")))
              (unless (string= "" txt)
                (when (search-forward (format ">%s%s<" rep-pre i) nil t)
                  (replace-match  (format ">%s<" txt) t t)))
              ;; Space and other symbols
              (dolist (sym '("SPC"))
                (setq txt (assoc (format "%s%s" sym-pre sym) (symbol-value (ergoemacs-get-variable-layout))))
                (if (not txt)
                    (setq txt "")
                  (if (>= (length txt) 3)
                      (setq txt (nth 2 txt))
                    (setq txt "")))
                
                (when (string= "" txt)
                  (setq txt
                        (all-completions
                         (format "%s%s " sym-pre sym)
                         (symbol-value (ergoemacs-get-variable-layout))))
                  (if (= 0 (length txt))
                      (setq txt "")
                    (setq txt "⌨")))
                (goto-char (point-min))
                (unless (string= "" txt)
                  (when (search-forward (format ">%s-%s<" rep-sym-pre sym) nil t)
                    (replace-match  (format ">%s<" (ergoemacs-gen-svg-quote txt)) t t))))

              (dolist (sym '("F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12"))
                (setq txt (assoc (format "<%s>" (downcase sym)) (symbol-value (ergoemacs-get-variable-layout))))
                (if (not txt)
                    (progn
                      (setq txt (assoc (format "<%s>" (downcase sym)) (symbol-value (ergoemacs-get-fixed-layout))))
                      (if (not txt)
                          (setq txt "")
                        (if (>= (length txt) 3)
                            (setq txt (nth 2 txt))
                          (setq txt ""))))
                  (if (>= (length txt) 3)
                      (setq txt (nth 2 txt))
                    (setq txt "")))
                (when (string= "" txt)
                  (setq txt
                        (all-completions
                         (format "<%s> " (downcase sym))
                         (symbol-value (ergoemacs-get-variable-layout))))
                  (if (= 0 (length txt))
                      (progn
                        (setq txt
                              (all-completions
                               (format "<%s> " (downcase sym))
                               (symbol-value (ergoemacs-get-fixed-layout))))
                        (if (= 0 (length txt))
                            (setq txt "")
                          (setq txt "⌨")))
                    (setq txt "⌨")))
                (goto-char (point-min))
                (when (search-forward (format ">N%s<" (upcase sym)) nil t)
                  (replace-match  (format ">%s<" txt) t t))

                (setq txt (assoc (format "<C-%s>" (downcase sym)) (symbol-value (ergoemacs-get-variable-layout))))
                (if (not txt)
                    (progn
                      (setq txt (assoc (format "<C-%s>" (downcase sym)) (symbol-value (ergoemacs-get-fixed-layout))))
                      (if (not txt)
                          (setq txt "")
                        (if (>= (length txt) 3)
                            (setq txt (nth 2 txt))
                          (setq txt ""))))
                  (if (>= (length txt) 3)
                      (setq txt (nth 2 txt))
                    (setq txt "")))
                (when (string= "" txt)
                  (setq txt
                        (all-completions
                         (format "<C-%s> " (downcase sym))
                         (symbol-value (ergoemacs-get-variable-layout))))
                  (if (= 0 (length txt))
                      (progn
                        (setq txt
                              (all-completions
                               (format "<C-%s> " (downcase sym))
                               (symbol-value (ergoemacs-get-fixed-layout))))
                        (if (= 0 (length txt))
                            (setq txt "")
                          (setq txt "⌨")))
                    (setq txt "⌨")))
                (goto-char (point-min))
                (when (search-forward (format ">CC%s<" (upcase sym)) nil t)
                  (replace-match  (format ">%s<" txt) t t))
                
                (setq txt (assoc (format "<M-%s>" (downcase sym)) (symbol-value (ergoemacs-get-variable-layout))))
                (if (not txt)
                    (progn
                      (setq txt (assoc (format "<M-%s>" (downcase sym)) (symbol-value (ergoemacs-get-fixed-layout))))
                      (if (not txt)
                          (setq txt "")
                        (if (>= (length txt) 3)
                            (setq txt (nth 2 txt))
                          (setq txt ""))))
                  (if (>= (length txt) 3)
                      (setq txt (nth 2 txt))
                    (setq txt "")))
                (when (string= "" txt)
                  (setq txt
                        (all-completions
                         (format "<M-%s> " (downcase sym))
                         (symbol-value (ergoemacs-get-variable-layout))))
                  (if (= 0 (length txt))
                      (progn
                        (setq txt
                              (all-completions
                               (format "<M-%s> " (downcase sym))
                               (symbol-value (ergoemacs-get-fixed-layout))))
                        (if (= 0 (length txt))
                            (setq txt "")
                          (setq txt "⌨")))
                    (setq txt "⌨")))
                (goto-char (point-min))
                (when (search-forward (format ">MM%s<" (upcase sym)) nil t)
                  (replace-match  (format ">%s<" txt) t t))
                
                )
              ;; Legend/Key
              (goto-char (point-min))
              (when (search-forward (format ">%s<" rep-sym-pre) nil t)
                (replace-match (format ">%s<" (ergoemacs-gen-svg-quote final-txt)) t t))))
          (setq i (+ i 1)))
        (goto-char (point-min))
        (while (re-search-forward ">\\([0-4]?[CMAN][0-9]+\\|[0-4]?[CMAN]\\{2\\}[^\n<]*?\\|nil\\)<" nil t)
          (replace-match "><"))
        (goto-char (point-min))
        (when (search-forward ">title<" nil t)
          (if is-prefix
              (replace-match ">Continuation of Emacs Command Sequences:<")
            (replace-match (format
                            ">Layout: %s; Theme %s<"
                            layout
                            (or ergoemacs-theme "standard"))))))
      (when ergoemacs-inkscape
        (message "Converting to png")
        (shell-command (format "%s -z -f \"%s\" -e \"%s\"" ergoemacs-inkscape
                               file (concat (file-name-sans-extension file) ".png"))))
      (message "Layout generated to %s" file)
      (when prefix-lst
        (let ((ergoemacs-svn-prefixes prefix-lst))
          ;; Remove prefix generation for now.  It seems a bit buggy.
          ;; (ergoemacs-gen-svg layout file-name extra t)
          ))
      (when (and is-prefix ergoemacs-convert ergoemacs-inkscape)
        (message "Concatenating layouts.")
        ;; Concatenate two files
        (shell-command
         (format "%s -append \"%s\" \"%s\" \"%s\"" ergoemacs-convert
                 (replace-regexp-in-string "-prefix.svg" ".png" file)
                 (replace-regexp-in-string ".svg" ".png" file)
                 (replace-regexp-in-string ".svg" "-tmp.png" file)))
        (copy-file
         (replace-regexp-in-string ".svg" "-tmp.png" file)
         (replace-regexp-in-string "-prefix.svg" ".png" file) t)
        (delete-file (replace-regexp-in-string ".svg" "-tmp.png" file))))))


(defun ergoemacs-curr-svg ()
  "Generates the current ergoemacs layout, unless it already exists."
  (interactive)
  (let ((var (or ergoemacs-theme "standard"))
        (layout ergoemacs-keyboard-layout)
        (extra "ergo-layouts")
        (dir "")
        (file ""))
    (when var
      (setq extra (concat var "/ergo-layouts")))
    (setq dir (expand-file-name extra
                                (expand-file-name "ergoemacs-extras" user-emacs-directory)))
    (setq file (expand-file-name (concat "ergoemacs-layout-" layout ".svg") dir))
    (unless (file-exists-p file)
      (message "Generating SVG file...")
      (unless (featurep 'ergoemacs-extras)
        (require 'ergoemacs-extras))
      (ergoemacs-gen-svg layout "kbd-ergo.svg" extra)
      (message "Generated!"))
    (when (called-interactively-p 'interactive)
      (browse-url (concat "file://" file)))
    file))

;;;###autoload
(defun ergoemacs-svgs (&optional layouts)
  "Generate SVGs for all the defined layouts and themes."
  (interactive)
  (let* ((lay (or layouts (ergoemacs-get-layouts)))
         (saved-theme (or ergoemacs-theme "standard")))
    (dolist (x lay)
      (message "Generate SVG for %s" x)
      (condition-case err
          (progn
            (ergoemacs-gen-svg x)
            (ergoemacs-set-default 'ergoemacs-theme nil)
            (ergoemacs-gen-svg x "kbd-ergo.svg" "ergo-layouts"))
        (error (message "Error generating base SVG for %s; %s" x err)))
      (dolist (y (sort (ergoemacs-get-themes) 'string<))
        (condition-case err
            (progn
              (ergoemacs-set-default 'ergoemacs-theme y)
              (ergoemacs-gen-svg x "kbd-ergo.svg" (concat y)))
          (error (message "Error generating theme %s; %s" y err))))
      (message "Setting theme back to %s" saved-theme)
      (ergoemacs-set-default 'ergoemacs-theme saved-theme))))

;; (require 'json nil t)

;; (defun ergoemacs-layouts-json ()
;;   "Fixed layouts json string."
;;   (concat "kbd_layout = "
;;           (json-encode
;;            (mapcar
;;             (lambda(layout)
;;               `(,layout ,@(symbol-value (intern (concat "ergoemacs-layout-" layout)))))
;;             (ergoemacs-get-layouts))) ";"))

;; (defun ergoemacs-fixed-themes-json ()
;;   "Fixed themes json string."
;;   (require 'cl)
;;   (concat "fixed_layouts = "
;;           (json-encode
;;            (mapcar
;;             (lambda(theme)
;;               `(,theme .
;;                        ,(mapcar
;;                          (lambda(x)
;;                            `(,(with-temp-buffer
;;                                 (insert (ergoemacs-kbd (nth 0 x) t (nth 3 x)))
;;                                 (goto-char (point-min))
;;                                 (while (re-search-forward "S-\\(.\\)\\>" nil t)
;;                                   (replace-match (upcase (match-string 1))))
;;                                 (buffer-string))  . ,(nth 2 x)))
;;                          (remove-if (lambda(x) (not (nth 2 x)))
;;                                     (symbol-value
;;                                      (if (string= "" theme)
;;                                          (intern "ergoemacs-fixed-layout")
;;                                        (intern (concat "ergoemacs-fixed-layout-" theme))))))))
;;             `("" ,@(ergoemacs-get-themes))))
;;           ";"))

;; (defun ergoemacs-variable-themes-json ()
;;   "Variable themes json string."
;;   (require 'cl)
;;   (concat "var_layouts = "
;;           (json-encode
;;            (mapcar
;;             (lambda(theme)
;;               `(,theme .
;;                        ,(mapcar
;;                          (lambda(x)
;;                            `(,(with-temp-buffer
;;                                 (insert (ergoemacs-kbd (nth 0 x) t (nth 3 x)))
;;                                 (goto-char (point-min))
;;                                 (while (re-search-forward "S-\\(.\\)\\>" nil t)
;;                                   (replace-match (upcase (match-string 1))))
;;                                 (buffer-string))  . ,(nth 2 x)))
;;                          (remove-if (lambda(x) (not (nth 2 x)))
;;                                     (symbol-value
;;                                      (if (string= "" theme)
;;                                          (intern "ergoemacs-variable-layout")
;;                                        (intern (concat "ergoemacs-variable-layout-" theme))))))))
;;             `("" ,@(ergoemacs-get-themes))))
;;           ";"))


(provide 'ergoemacs-extras)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-extras.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
