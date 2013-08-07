;;; ergoemacs-layouts.el --- keyboard layouts for ErgoEmacs -*- lexical-binding:t -*-

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

(defvar ergoemacs-layout-sw
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "å" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ö" "ä" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Å" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ö" "Ä" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Swedish layout.")

(defvar ergoemacs-layout-da
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "á" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "æ" "ø" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Á" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Æ" "Ø" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Danish layout.")

(defvar ergoemacs-layout-pt-nativo
  '("" "+" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "º" "<" ""
    "" ""  "'" "," "." "h" "x" "w" "l" "t" "c" "p" "~" "-" ""
    "" ""  "i" "e" "a" "o" "u" "m" "d" "s" "r" "n" "'" "|" ""
    "" "«"  "y" "ç" "j" "b" "k" "q" "v" "g" "f" "z" "" "" ""
    ;; Shifted
    "" "*" "!" "\"" "#" "$" "%" "&" "/" "(" ")" "=" "ª" ">" ""
    "" ""  "?" ";" ":" "H" "X" "W" "L" "T" "C" "P" "^" "_" ""
    "" ""  "I" "E" "A" "O" "U" "M" "D" "S" "R" "N" "`" "\\" ""
    "" "»"  "Y" "Ç" "J" "B" "K" "Q" "V" "G" "F" "Z" "" "" "")
  "PT Nativo layout URL `http://xahlee.info/kbd/pt-nativo_keyboard_layout.html'.")

(defvar ergoemacs-layout-us
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "US English QWERTY layout.")

(defvar ergoemacs-layout-dv
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "[" "]" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" ""  ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" ""  ""  ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "{" "}"  ""
    "" ""  "\"" "," "." "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Dvorak layout.  URL `http://en.wikipedia.org/wiki/Dvorak_Simplified_Keyboard'.")

(defvaralias 'ergoemacs-layout-us_dvorak 'ergoemacs-layout-dv)

(defvar ergoemacs-layout-programmer-dv
  '("" "$" "&" "[" "{" "}" "(" "=" "*" ")" "+" "]" "!" "#" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "" ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "" "%" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "`"  ""
    "" ""  "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Programmer Dvorak layout.")

(defvar ergoemacs-layout-gb-dv
  '("" "`" "[" "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "]"  ""
    "" ""  "/" "," "." "p" "y" "f" "g" "c" "r" "l" "'" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "#" ""
    "" "\\"  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "¬" "{" "&" "%" "£" "!" "(" ")" "\"" "$" "^" "*" "}" ""
    "" ""  "?" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "@" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "~" ""
    "" "|"  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "UK Dvorak layout.")

(defvar ergoemacs-layout-colemak
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "f" "p" "g" "j" "l" "u" "y" ";" "[" "]" "\\"
    "" ""  "a" "r" "s" "t" "d" "h" "n" "e" "i" "o" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "F" "P" "G" "J" "L" "U" "Y" ":" "{" "}" "|"
    "" ""  "A" "R" "S" "T" "D" "H" "N" "E" "I" "O" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "US Colemak layout URL `http://colemak.com/'.")

(defvar ergoemacs-layout-asset
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "j" "f" "g" "y" "p" "u" "l" ";" "[" "]" "\\"
    "" ""  "a" "s" "e" "t" "d" "h" "n" "i" "o" "r" "'" "" ""
    "" ""  "z" "x" "c" "v" "b" "k" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "J" "F" "G" "Y" "P" "U" "L" ":" "{" "}" "|"
    "" ""  "A" "S" "E" "T" "D" "H" "N" "I" "O" "R" "\"" "" ""
    "" ""  "Z" "X" "C" "V" "B" "K" "M" "<" ">" "?" "" "" "")
  "US Asset layout.  URL `http://millikeys.sourceforge.net/asset/'.")

(defvar ergoemacs-layout-workman
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "d" "r" "w" "b" "j" "f" "u" "p" ";" "[" "]" "\\"
    "" ""  "a" "s" "h" "t" "g" "y" "n" "e" "o" "i" "'" "" ""
    "" ""  "z" "x" "m" "c" "v" "k" "l" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "D" "R" "W" "B" "J" "F" "U" "P" ":" "{" "}" "|"
    "" ""  "A" "S" "H" "T" "G" "Y" "N" "E" "O" "I" "\"" "" ""
    "" ""  "Z" "X" "M" "C" "V" "K" "L" "<" ">" "?" "" "" "")
  "US Workman layout.  URL `http://www.workmanlayout.com/blog/'.")

(defvar ergoemacs-layout-gb
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "#" ""
    "" "\\"  "z" "x" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "¬" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "@" "~" ""
    "" "|"  "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "UK layout.  URL `http://en.wikipedia.org/wiki/Keyboard_layout'.")

(defvar ergoemacs-layout-it
  '("" "\\" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "è" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ò" "à" "ù" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "|" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "^" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "é" "+" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "ç" "°" "§" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Italian layout.  URL `http://en.wikipedia.org/wiki/Keyboard_layout'.")

(defvar ergoemacs-layout-es
  '("" "°" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "`" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ñ" "'" "ç" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "ª" "!" "\"" "£" "$" "%" "&" "/" "(" ")" "=" "?" "¿" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "^" "*" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ñ" "\"" "Ç" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Spanish layout.  URL `http://en.wikipedia.org/wiki/Keyboard_layout'.")

(defvaralias 'ergoemacs-layout-sp 'ergoemacs-layout-es)

(defvar ergoemacs-layout-fr
  '("" "²" "&" "é" "\"" "'" "(" "-" "è" "_" "ç" "à" ")" "=" ""
    "" ""  "a" "z" "e" "r" "t" "y" "u" "i" "o" "p" "^" "$" ""
    "" ""  "q" "s" "d" "f" "g" "h" "j" "k" "l" "m" "ù" "*" ""
    "" "<"  "w" "x" "c" "v" "b" "n" "," ";" ":" "!" "" "" ""
    ;; Shifted
    "" "³" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "°" "+" ""
    "" ""  "A" "Z" "E" "R" "T" "Y" "U" "I" "O" "P" "" "£" ""
    "" ""  "Q" "S" "D" "F" "G" "H" "J" "K" "L" "M" "%" "μ" ""
    "" ">"  "W" "X" "C" "V" "B" "N" "?" "." "/" "§" "" "" "")
  "French AZERTY layout.  URL `http://en.wikipedia.org/wiki/Keyboard_layout'.")

;; From Thomas Rikl
(defvar ergoemacs-layout-de
  '("" "" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "ß" "" ""
    "" ""  "q" "w" "e" "r" "t" "z" "u" "i" "o" "p" "ü" "+" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ö" "ä" "#" ""
    "" ""  "y" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "°" "!" "\"" "§" "$" "%" "&" "/" "(" ")" "=" "?" "" ""
    "" ""  "Q" "W" "E" "R" "T" "Z" "U" "I" "O" "P" "Ü" "*" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ö" "Ä" "'" ""
    "" ""  "Y" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "German QWERTZ layout.")

(defvaralias 'ergoemacs-layout-ge 'ergoemacs-layout-de)

;; From Baptiste Fouques
;; changed to bepo because it breaks how I run things (unfortunately)...
(defvar ergoemacs-layout-bepo
  '("" "$" "\"" "«" "»" "(" ")" "@" "+" "-" "/" "*" "=" "%" ""
    "" ""  "b" "é" "p" "o" "è" "^" "v" "d" "l" "j" "z" "w" ""
    "" ""  "a" "u" "i" "e" "," "c" "t" "s" "r" "n" "m" "ç" ""
    "" "ê" "à" "y" "x" "." "k" "'" "q" "g" "h" "f" "" "" ""
    ;; Shifted
    "" "#" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "°" "`" ""
    "" ""  "B" "É" "P" "O" "È" "!" "V" "D" "L" "J" "Z" "W" ""
    "" ""  "A" "U" "I" "E" ";" "C" "T" "S" "R" "N" "M" "Ç" ""
    "" "Ê" "À" "Y" "X" ":" "K" "?" "Q" "G" "H" "F" "" "" "")
  "French BÉPO layout.  URL `http://bepo.fr/'.")

(defvaralias 'ergoemacs-layout-bépo 'ergoemacs-layout-bepo)

(defvar ergoemacs-layout-fa
  '("" "‍" "۱" "۲" "۳" "۴" "۵" "۶" "۷" "۸" "۹" "۰" "-" "=" ""
    "" ""  "ض" "ص" "ث" "ق" "ف" "غ" "ع" "ه" "خ" "ح" "ج" "چ" "\\"
    "" ""  "ش" "س" "ی" "ب" "ل" "ا" "ت" "ن" "م" "ک" "گ" "" ""
    "" ""  "ظ" "ط" "ز" "ر" "ذ" "د" "پ" "و" "." "/" "" "" ""
    ;; Shifted
    "" "÷" "!" "٬" "٫" "﷼" "٪" "×" "،" "*" ")" "(" "ـ" "+" ""
    "" ""  "ْ" "ٌ" "ٍ" "ً" "ُ" "ِ" "َ" "ّ" "]" "[" "}" "{" "|"
    "" ""  "ؤ" "ئ" "ي" "إ" "أ" "آ" "ة" "»" "«" ":" "؛" "" ""
    "" ""  "ك" "ٓ" "ژ" "ٰ" "‌" "ٔ" "ء" ">" "<" "؟" "" "" "")
  "FA Persian standard layout.")




;;; Layout Functions
(defun ergoemacs-get-layouts-type ()
  "Gets the customization types for `ergoemacs-keyboard-layout'."
  `(choice ,@(mapcar
              (lambda(elt)
                `(const :tag ,elt :value ,elt))
              (sort (ergoemacs-get-layouts t) 'string<))))

(defun ergoemacs-set-layout (layout)
  "Set the ergoemacs layout to LAYOUT."
  (ergoemacs-set-default 'ergoemacs-keyboard-layout layout))

(defun ergoemacs-get-layouts-menu ()
  "Gets the easymenu entry for ergoemacs-layouts."
  `("Keyboard Layouts"
    ,@(mapcar
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
           `[,(concat lay " - " doc)
             (lambda() (interactive)
               (ergoemacs-set-layout ,lay)) :style radio :selected (string= ergoemacs-keyboard-layout ,lay)]))
       (ergoemacs-get-layouts))))

(defun ergoemacs-get-layouts-doc ()
  "Gets the list of all known layouts and the documentation associated with the layouts."
  (let ((lays (sort (ergoemacs-get-layouts t) 'string<)))
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
         (concat "\"" lay "\" (" doc ")" (if is-alias ", alias" ""))))
     lays "\n")))


(defun ergoemacs-reset-layouts ()
  "Reset Layout information."
  (interactive)
  (setq ergoemacs-get-layouts-no-aliases nil)
  (setq ergoemacs-get-layouts-aliases nil))

(defvar ergoemacs-get-layouts-no-aliases nil)
(defvar ergoemacs-get-layouts-aliases nil)

(defun ergoemacs-get-layouts (&optional aliases ob)
  "Get the list of all known layouts."
  (if (and ergoemacs-get-layouts-no-aliases
           (not aliases))
      (symbol-value 'ergoemacs-get-layouts-no-aliases)
    (if (and ergoemacs-get-layouts-aliases
             aliases)
        (symbol-value 'ergoemacs-get-layouts-aliases)
      (let (ret)
        (mapatoms
         (lambda(s)
           (let ((sn (symbol-name s)))
             (and (string-match "^ergoemacs-layout-" sn)
                  (if (or aliases
                          (and (not aliases)
                               (documentation-property
                                (intern sn) 'variable-documentation)))
                      (setq ret (cons (replace-regexp-in-string "ergoemacs-layout-" "" sn) ret))))))
         ob)
        (if aliases
            (setq ergoemacs-get-layouts-aliases)
          (setq ergoemacs-get-layouts-no-aliases))
        ret))))

(provide 'ergoemacs-layouts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-layouts.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
