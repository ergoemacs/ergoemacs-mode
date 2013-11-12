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
                                          ergoemacs-translation-assoc) nil)))))))
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
