;;; ergoemacs-translate.el --- Keyboard translation functions
;; 
;; Filename: ergoemacs-translate.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Sat Sep 28 20:08:09 2013 (-0500)
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

(defvar ergoemacs-needs-translation nil
  "Tells if ergoemacs keybindings need a translation")

(defvar ergoemacs-translation-from nil
  "Translation from keyboard layout")

(defvar ergoemacs-translation-to nil
  "Translation to keyboard layout")

(defvar ergoemacs-translation-assoc nil
  "Translation alist")

(defvar ergoemacs-translation-regexp nil
  "Translation regular expression")

(defvar ergoemacs-unshifted-regexp nil
  "Unshifted regular expression.")

(defvar ergoemacs-shifted-regexp nil
  "Shifted regular expression.")

(defvar ergoemacs-shifted-assoc nil
  "Translation alist.")


(defvar ergoemacs-translate-hash (make-hash-table :test 'equal)) 
(defun ergoemacs-translate-shifted (kbd)
  "Translates anything with S- and no C- in it to an upper-case character."
  (if (not kbd) nil
    (let ((ret kbd))
      (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\|C-\\)" ret)
        (when (string-match "^\\(.*\\)S-\\(.*\\)\\(.\\)$" ret)
          (setq ret (concat (match-string 1 ret)
                            (match-string 2 ret)
                            (upcase (match-string 3 ret))))))
      (symbol-value 'ret))))

(defvar ergoemacs-translations
  `((:name :ctl-to-alt
           :text ,(format "<Ctl%sAlt> " (ergoemacs-unicode-char "↔" " to "))
           :alt "C-"
           :ctl "M-")
    (:name :ctl-to-alt-shift
           :text ,(format "<Ctl%sAlt> " (ergoemacs-unicode-char "↔" " to "))
           :alt "C-"
           :ctl "M-"
           :shift "M-"
           :alt-shift "C-M-"
           :ctl-shift "C-M-")
    (:name :unchorded
           :text "<Ctl+>"
           :unchorded "C-"
           :alt ""
           :ctl "M-")
    (:name :unchorded-shift
           :text "<Ctl+>"
           :unchorded "C-"
           :alt ""
           :ctl "M-"
           :shift "M-"
           :alt-shift "C-M-"
           :ctl-shift "C-S-"))
  "Ergoemacs-mode translations")

(defvar ergoemacs-translation-names nil)
  
(defun ergoemacs-translation-install (trans-plist orig-key ret-plist)
  "Installs the translation.
TRANS-PLIST is the plist defining the translation.
ORIG-KEY is the original kbd-code
RET-PLIST is  the plist that the translation will be installed into."
  (let* ((name (plist-get trans-plist ':name))
         (key (intern (concat (symbol-name name) "-key")))
         (pretty (intern (concat (symbol-name name) "-pretty")))
         (ret ret-plist)
         case-fold-search
         new-key)
    (cond
     ;; Trifecta
     ((string-match "C-M-S-" orig-key)
      (if (plist-get trans-plist ':alt-ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Double keys
     ((string-match "C-S-" orig-key)
      (if (plist-get trans-plist ':ctl-shift)
          (setq new-key (concat (plist-get trans-plist ':ctl-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "C-M-" orig-key)
      (if (plist-get trans-plist ':alt-ctl)
          (setq new-key (concat (plist-get trans-plist ':alt-ctl)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-S-" orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key (concat (plist-get trans-plist ':alt-shift)
                                (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ;; Emacs saves these as M-A instead of M-S-a 
     ((string-match (format "M%s" ergoemacs-shifted-regexp) orig-key)
      (if (plist-get trans-plist ':alt-shift)
          (setq new-key
                (concat (plist-get trans-plist ':alt-shift)
                        (replace-match
                         (concat (plist-get trans-plist ':alt-shift)
                                 (cdr (assoc (match-string 2 orig-key) ergoemacs-shifted-assoc))) t t)))
        (setq new-key orig-key)))
     ;; Single Key combinations
     ((string-match "C-" orig-key)
      (if (plist-get trans-plist ':ctl)
          (setq new-key
                (concat (plist-get trans-plist ':ctl)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match "M-" orig-key)
      (if (plist-get trans-plist ':alt)
          (setq new-key
                (concat (plist-get trans-plist ':alt)
                        (replace-match "" nil nil orig-key)))
        (setq new-key orig-key)))
     ((string-match ergoemacs-shifted-regexp orig-key)
      (if (plist-get trans-plist ':shift)
          (setq new-key
                (concat (plist-get trans-plist ':shift)
                        (cdr (assoc (match-string 2 orig-key) ergoemacs-shifted-assoc))))
        (setq new-key orig-key)))
     ;; Unchorded
     ((plist-get trans-plist ':unchorded)
      (setq new-key
            (concat (plist-get trans-plist ':unchorded) orig-key)))
     (t
      (setq new-key orig-key)))
    (setq new-key (ergoemacs-translate-shifted new-key))
    (setq ret (plist-put ret name new-key))
    (setq ret (plist-put ret key (read-kbd-macro new-key t)))
    (setq ret (plist-put ret pretty (ergoemacs-pretty-key new-key)))
    (symbol-value 'ret)))

(defun ergoemacs-translate (key)
  "Translates KEY and returns a plist of the translations.

:normal
   No translation

:normal-shift
   No translation

:shift-translated
    S-a    -> a
    M-S-a  -> M-a
    C-S-a  -> C-a
    Anything without shift is nil.

All other translations are defined in `ergoemacs-translations'.

There are also :XXX-key and :XXX-pretty for actual key-strokes
and `ergoemacs-pretty-key' descriptions.

"
  (let* ((ret (gethash key ergoemacs-translate-hash))
         (orig-key key)
         tmp
         case-fold-search
         (key key)
         only-key
         shift-translated
         (ergoemacs-use-ergoemacs-key-descriptions t)
         ctl-to-alt
         unchorded
         unshifted-key)
    (or ret
        (progn
          (unless (stringp key)
            (setq key (key-description key))
            (setq orig-key key))
          (cond
           ((string-match "\\(^<.+>$\\|SPC\\|DEL\\|ESC\\|RET\\|TAB\\)" key)
            (setq only-key (replace-regexp-in-string "[CMS]-" "" key t))
            (if (string-match "S-" key)
                (setq shifted-key (replace-match "" t nil key))
              (setq shifted-key (concat "S-" key))))
           (t
            (setq only-key (replace-regexp-in-string "^.*\\(.\\)$" "\\1" key t)
                  shifted-key (assoc only-key ergoemacs-shifted-assoc))
            (when shifted-key
              (setq shifted-key (cdr shifted-key)))))
          (when (and (string-match "\\([A-Z]\\)$" key)
                     (not (string-match "\\<\\(SPC\\|DEL\\|ESC\\|RET\\|TAB\\)\\>" key)))
            (setq key
                  (replace-match
                   (concat "S-" (downcase (match-string 1 key))) t t key)))
          (when shifted-key
            (setq unshifted-key only-key)
            (unless (string-match "\\(^<.+>$\\|\\<SPC\\>\\|\\<DEL\\>\\|\\<ESC\\>\\|\\<RET\\>\\|\\<TAB\\>\\)" shifted-key)
              (when (string-match "[A-Z]" shifted-key)
                (setq shifted-key (concat "S-" (downcase shifted-key))))
              (when (string-match "[A-Z]" unshifted-key)
                (setq unshifted-key (concat "S-" (downcase unshifted-key))))))
          (when (string-match "S-" key)
            (setq shift-translated (replace-regexp-in-string "S-" "" key t)))
          (setq ret (plist-put ret ':normal (ergoemacs-translate-shifted key)))
          (setq ret (plist-put ret ':normal-key (read-kbd-macro (ergoemacs-translate-shifted key) t)))
          (setq ret (plist-put ret ':normal-pretty (ergoemacs-pretty-key key)))
          (setq ret (plist-put ret ':normal-shift (ergoemacs-translate-shifted key)))
          (setq ret (plist-put ret ':normal-shift-key (read-kbd-macro (ergoemacs-translate-shifted key) t)))
          (setq ret (plist-put ret ':normal-shift-pretty (ergoemacs-pretty-key key)))
          
          (if shift-translated
              (progn
                (setq ret (plist-put ret ':shift-translated (ergoemacs-translate-shifted shift-translated)))
                (setq ret (plist-put ret ':shift-translated-key (read-kbd-macro (ergoemacs-translate-shifted shift-translated) t)))
                (setq ret (plist-put ret ':shift-translated-pretty (ergoemacs-pretty-key shift-translated))))
            (setq ret (plist-put ret ':shift-translated nil))
            (setq ret (plist-put ret ':shift-translated-key nil))
            (setq ret (plist-put ret ':shift-translated-pretty nil)))
          
          (setq ret (plist-put ret ':shifted (ergoemacs-translate-shifted shifted-key)))
          (setq ret (plist-put ret ':shifted-key (read-kbd-macro (ergoemacs-translate-shifted shifted-key) t)))
          (setq ret (plist-put ret ':shifted-pretty (ergoemacs-pretty-key shifted-key)))
          
          (setq ret (plist-put ret ':unshifted (ergoemacs-translate-shifted unshifted-key)))
          (setq ret (plist-put ret ':unshifted-key (read-kbd-macro (ergoemacs-translate-shifted unshifted-key) t)))
          (setq ret (plist-put ret ':unshifted-pretty (ergoemacs-pretty-key unshifted-key)))
          
          (setq ret (plist-put ret ':ctl (ergoemacs-translate-shifted
                                          (concat "C-" unshifted-key))))
          (setq ret (plist-put ret ':ctl-key (read-kbd-macro (plist-get ret ':ctl) t)))
          (setq ret (plist-put ret ':ctl-pretty (ergoemacs-pretty-key (plist-get ret ':ctl))))

          (setq ret (plist-put ret ':raw (ergoemacs-translate-shifted
                                      (replace-regexp-in-string
                                       "\\<[CSMS]-" "" key))))
          (setq ret (plist-put ret ':raw-key  (read-kbd-macro (plist-get ret ':raw) t)))
          (setq ret (plist-put ret ':raw-pretty (ergoemacs-pretty-key
                                             (plist-get ret ':raw))))
          (if (assoc (plist-get ret ':raw) ergoemacs-shifted-assoc)
              (progn
                (setq ret (plist-put ret ':raw-shift
                               (ergoemacs-translate-shifted
                                (replace-regexp-in-string
                                 "\\<[CSM]-" ""
                                 (cdr (assoc (plist-get ret ':raw) ergoemacs-shifted-assoc))))))
                (setq ret (plist-put ret ':raw-shift-key
                                     (read-kbd-macro (plist-get ret ':raw-shift) t)))
                (setq ret (plist-put ret ':raw-shift-pretty
                                     (ergoemacs-pretty-key
                                      (plist-get ret ':raw-shift)))))
            (setq ret (plist-put ret ':raw-shift nil))
            (setq ret (plist-put ret ':raw-shift-key nil))
            (setq ret (plist-put ret ':raw-shift-pretty nil)))
          
          (setq ret (plist-put ret ':alt (ergoemacs-translate-shifted
                                          (concat "M-" unshifted-key))))
          (setq ret (plist-put ret ':alt-key (read-kbd-macro (plist-get ret ':alt) t)))
          (setq ret (plist-put ret ':alt-pretty (ergoemacs-pretty-key (plist-get ret ':alt))))
          
          (setq ret (plist-put ret ':alt-ctl (ergoemacs-translate-shifted
                                              (concat "M-C-" unshifted-key))))
          (setq ret (plist-put ret ':alt-ctl-key (read-kbd-macro (plist-get ret ':alt-ctl) t)))
          (setq ret (plist-put ret ':alt-ctl-pretty (ergoemacs-pretty-key (plist-get ret ':alt-ctl))))
          
          (setq ret (plist-put ret ':ctl-shift (ergoemacs-translate-shifted
                                                (concat "C-" shifted-key))))
          (setq ret (plist-put ret ':ctl-shift-key (read-kbd-macro (plist-get ret ':ctl-shift) t)))
          (setq ret (plist-put ret ':ctl-shift-pretty (ergoemacs-pretty-key (plist-get ret ':ctl-shift))))
          
          (setq ret (plist-put ret ':alt-shift (ergoemacs-translate-shifted
                                                (concat "M-" shifted-key))))
          (setq ret (plist-put ret ':alt-shift-key (read-kbd-macro (plist-get ret ':alt-shift) t)))
          (setq ret (plist-put ret ':alt-shift-pretty (ergoemacs-pretty-key (plist-get ret ':alt-shift))))
          
          (setq ret (plist-put ret ':alt-ctl-shift (ergoemacs-translate-shifted
                                                    (concat "M-C-" shifted-key))))
          (setq ret (plist-put ret ':alt-ctl-shift-key (read-kbd-macro (plist-get ret ':alt-ctl-shift) t)))
          (setq ret (plist-put ret ':alt-ctl-shift-pretty (ergoemacs-pretty-key (plist-get ret ':alt-ctl-shift))))
          (mapc
           (lambda(plist)
             (setq ret (ergoemacs-translation-install plist orig-key ret)))
           ergoemacs-translations)
          (puthash orig-key (symbol-value 'ret) ergoemacs-translate-hash)
          (symbol-value 'ret)))))

(defun ergoemacs-setup-translation (layout &optional base-layout)
  "Setup translation from BASE-LAYOUT to LAYOUT."
  (let ((orig-base (or base-layout "us"))
        lay shifted-list unshifted-list base
        len i)
    (setq lay (symbol-value (intern (concat "ergoemacs-layout-" layout))))
    (setq base (symbol-value (intern (concat "ergoemacs-layout-" orig-base))))
    
    (setq len (length base))
    (setq i 0)
    (while (< i 60)
      (unless (or (string= "" (nth i lay))
                  (string= "" (nth (+ i 60) lay)))
        (add-to-list 'ergoemacs-shifted-assoc
                     `(,(nth i lay) . ,(nth (+ i 60) lay)))
        (add-to-list 'ergoemacs-shifted-assoc
                     `(,(nth (+ i 60) lay) . ,(nth i lay)))
        (add-to-list 'unshifted-list (nth i lay))
        (add-to-list 'shifted-list (nth (+ i 60) lay)))
      (setq i (+ i 1)))
    (setq ergoemacs-shifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt shifted-list nil)))
    (setq ergoemacs-unshifted-regexp 
          (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                  (regexp-opt unshifted-list nil)))
    (unless (and (string= layout ergoemacs-translation-to)
                 (string= orig-base ergoemacs-translation-from))
      (if (equal layout orig-base)
          (progn
            (setq ergoemacs-translation-from orig-base)
            (setq ergoemacs-translation-to layout)
            (setq ergoemacs-needs-translation nil)
            (setq ergoemacs-translation-assoc nil)
            (setq ergoemacs-translation-regexp nil))
        (setq ergoemacs-translation-from orig-base)
        (setq ergoemacs-translation-to layout)
        (setq ergoemacs-needs-translation t)
        (setq ergoemacs-translation-assoc nil)
        (setq len (length base))
        (setq i 0)
        (while (< i len)
          (unless (or (string= "" (nth i base))
                      (string= "" (nth i lay)))
            (add-to-list 'ergoemacs-translation-assoc
                         `(,(nth i base) . ,(nth i lay))))
          (setq i (+ i 1)))
        (setq ergoemacs-translation-regexp
              (format "\\(-\\| \\|^\\)\\(%s\\)\\($\\| \\)"
                      (regexp-opt (mapcar (lambda(x) (nth 0 x))
                                          ergoemacs-translation-assoc) nil)))))
    ;; Pre-cache the translations...?  Takes too long to load :(
    (when nil
      (mapc
       (lambda(char)
         (unless (string= "" char)
           (ergoemacs-translate char)
           (ergoemacs-translate (concat "C-" char))
           (ergoemacs-translate (concat "M-" char))
           (ergoemacs-translate (concat "M-C-" char))))
       (append lay '("<f1>"  "<S-f1>"
                     "<f2>"  "<S-f2>"
                     "<f3>"  "<S-f3>"
                     "<f4>"  "<S-f4>"
                     "<f5>"  "<S-f5>"
                     "<f6>"  "<S-f6>"
                     "<f7>"  "<S-f7>"
                     "<f8>"  "<S-f8>"
                     "<f9>"  "<S-f9>"
                     "<f10>" "<S-f10>"
                     "<f11>" "<S-f11>"
                     "<f12>" "<S-f12>"
                     "SPC" "RET" "ESC" "DEL" "TAB"
                     "<home>" "<S-home>"
                     "<next>" "<S-next>"
                     "<prior>" "<S-prior>"
                     "<end>" "<S-end>"
                     "<insert>" "<S-insert>"
                     "<deletechar>" "<S-deletechar>"))))))

(defvar ergoemacs-kbd-hash (make-hash-table :test 'equal))
;; This is called so frequently make a hash-table of the results.

(defun ergoemacs-kbd (key &optional just-translate only-first)
  "Translates kbd code KEY for layout `ergoemacs-translation-from' to kbd code for `ergoemacs-translation-to'.
If JUST-TRANSLATE is non-nil, just return the KBD code, not the actual emacs key sequence.
"
  (save-match-data
    (if (not key)
        nil
      (let ((new-key (gethash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to)
                              ergoemacs-kbd-hash)))
        (if new-key
            (symbol-value 'new-key)
          (setq new-key key)
          (cond
           ((eq system-type 'windows-nt)
            (setq new-key (replace-regexp-in-string "<menu>" "<apps>" new-key)))
           (t
            (setq new-key (replace-regexp-in-string "<apps>" "<menu>" new-key))))
          ;; Translate Alt+ Ctl+ or Ctrl+ to M- and C-
          (setq new-key (replace-regexp-in-string "[Aa][Ll][Tt][+]" "M-" new-key))
          (setq new-key (replace-regexp-in-string "[Cc][Tt][Rr]?[Ll][+]" "C-" new-key))
          (when ergoemacs-needs-translation
            (setq new-key
                  (with-temp-buffer
                    (insert new-key)
                    (goto-char (point-min))
                    (when (re-search-forward ergoemacs-translation-regexp nil t)
                      (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                      (skip-chars-backward " "))
                    (when (not only-first)
                      (while (re-search-forward ergoemacs-translation-regexp nil t)
                        (replace-match (concat (match-string 1) (cdr (assoc (match-string 2) ergoemacs-translation-assoc)) (match-string 3)) t t)
                        (skip-chars-backward " ")))
                    (buffer-string))))
          (let ((case-fold-search t))
            (setq new-key (replace-regexp-in-string
                           ">>" ">"
                           (replace-regexp-in-string
                            "<<" "<"
                            (replace-regexp-in-string
                             "\\<\\(ENTER\\|enter\\)\\>" "<return>" new-key t t)))))
          (if (not just-translate)
              (condition-case err
                  (read-kbd-macro new-key t)
                (error
                 (read-kbd-macro (encode-coding-string new-key locale-coding-system) t)))
            (puthash `(,key ,just-translate ,only-first ,ergoemacs-translation-from ,ergoemacs-translation-to) new-key
                     ergoemacs-kbd-hash)
            new-key))))))

(defcustom ergoemacs-swap-alt-and-control nil
  "Swaps Alt and Ctrl keys"
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defcustom ergoemacs-change-fixed-layout-to-variable-layout nil
  "Change the fixed layout to variable layout keys.
For example, on dvorak, change C-j to C-c (copy/command)."
  :type 'boolean
  :set 'ergoemacs-set-default
  :group 'ergoemacs-mode)

(defun ergoemacs-get-kbd-translation (pre-kbd-code &optional dont-swap)
  "This allows a translation from the listed kbd-code and the true kbd code."
  (let ((ret (replace-regexp-in-string
              "[Cc]\\(?:on\\)?tro?l[+-]" "C-"
              (replace-regexp-in-string
               "[Aa]lt[+-]" "M-" pre-kbd-code))))
    (when (and ergoemacs-swap-alt-and-control (not dont-swap))
      (with-temp-buffer
        (insert ret)
        (goto-char (point-min))
        (while (re-search-forward "[MC]-" nil t)
          (cond
           ((string= "M-" (match-string 0)) ; M-A to C-S-a or M-a to C-a
            (replace-match "C-")
            (when (looking-at "[[:upper:]]\\( \\|$\\)")
              (replace-match (format "S-%s" (downcase (match-string 0))) t)))
           (t ; C-S-a to M-A or C-a to M-a
            (replace-match "M-")
            (cond
             ((looking-at "S-\\(.\\)\\( \\|$\\)")
              (replace-match
               (format "%s%s" (upcase (match-string 1)) (match-string 2)) t))
             ((looking-at "\\(.\\)\\( \\|$\\)")
              (replace-match
               (format "%s%s" (downcase (match-string 1)) (match-string 2)) t))))))
        (setq ret (buffer-string))))
    (symbol-value 'ret)))

(defun ergoemacs-key-fn-lookup (function &optional use-apps)
  "Looks up the key binding for FUNCTION based on `ergoemacs-get-variable-layout'."
  (let ((ret nil))
    (mapc
     (lambda(x)
       (when (and (equal (nth 1 x) function)
                  (if use-apps
                      (string-match "<\\(apps\\|menu\\)>" (nth 0 x))
                    (not (string-match "<\\(apps\\|menu\\)>" (nth 0 x)))))
         (setq ret (ergoemacs-kbd (nth 0 x) nil (nth 3 x)))))
     (symbol-value (ergoemacs-get-variable-layout)))
    (unless ret
      (mapc
       (lambda(x)
         (when (and (equal (nth 1 x) function)
                    (if use-apps
                        (string-match "<\\(apps\\|menu\\)>" (nth 0 x))
                      (not (string-match "<\\(apps\\|menu\\)>" (nth 0 x)))))
           (setq ret (read-kbd-macro
                      (ergoemacs-get-kbd-translation (nth 0 x))))))
       (symbol-value (ergoemacs-get-fixed-layout))))
    (unless ret ;; Attempt to do a function translation.
      (let ((new-fn (ergoemacs-translate-current-function function)))
        (unless (eq new-fn function)
          (setq ret (ergoemacs-key-fn-lookup new-fn use-apps)))))
    (symbol-value 'ret)))


(provide 'ergoemacs-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-translate.el ends here
