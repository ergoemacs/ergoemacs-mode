;;; ergoemacs-layouts.el --- keyboard layouts for ErgoEmacs -* lexical-binding: t -*-

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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
(eval-when-compile
  (require 'ergoemacs-macros))

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


;; From tnielens
(defvar ergoemacs-layout-be
  '("" "²" "&" "é" "\"" "'" "(" "§" "è" "!" "ç" "à" ")" "-" ""
    "" ""  "a" "z" "e" "r" "t" "y" "u" "i" "o" "p" "^" "$" ""
    "" ""  "q" "s" "d" "f" "g" "h" "j" "k" "l" "m" "ù" "µ" ""
    "" "<"  "w" "x" "c" "v" "b" "n" "," ";" ":" "=" "" "" ""
    ;; Shifted
    "" "³" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "°" "_" ""
    "" ""  "A" "Z" "E" "R" "T" "Y" "U" "I" "O" "P" "¨" "*" ""
    "" ""  "Q" "S" "D" "F" "G" "H" "J" "K" "L" "M" "%" "£" ""
    "" ">"  "W" "X" "C" "V" "B" "N" "?" "." "/" "+" "" "" "")
  "Belgian AZERTY.")

(defvaralias 'ergoemacs-layout-bépo 'ergoemacs-layout-bepo)

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

(defvar ergoemacs-layout-da
  '("" "½" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "’" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "å" "\"" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "æ" "ø" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "@" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Å" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Æ" "Ø" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Danish layout.")

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

(defvar ergoemacs-layout-de-truly-andw
  '("<" "" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "" ""
    "#" "ß" "b" "u" "." "," "ü" "p" "c" "l" "m" "f" "x" "+" ""
    "" "" "h" "i" "e" "a" "o" "d" "t" "r" "n" "s" "" "" ""
    "" "" "k" "y" "ö" "ä" "q" "j" "g" "w" "v" "z" "" "" ""
    ;; Shifted
    ">" "°" "!" "\"" "§" "$" "%" "&" "/" "(" ")" "=" "_" "" ""
    "'" "?" "B" "U" ":" ";" "Ü" "P" "C" "L" "M" "F" "X" "*" ""
    "" "" "H" "I" "E" "A" "O" "D" "T" "R" "N" "S" "" "" ""
    "" "" "K" "Y" "Ö" "Ä" "Q" "J" "G" "W" "V" "Z" "" "" "")
  "German BU-TECK Layout.  URL `http://www.adnw.de'.")

(defvar ergoemacs-layout-dv
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "[" "]" ""
    "" ""  "'" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "=" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" ""  ""
    "" ""  ";" "q" "j" "k" "x" "b" "m" "w" "v" "z" ""  ""  ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "{" "}"  ""
    "" ""  "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "+" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""  ":" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Dvorak layout.  URL `http://en.wikipedia.org/wiki/Dvorak_Simplified_Keyboard'.")

(defvar ergoemacs-layout-eo
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "ŝ" "ĝ" "e" "r" "t" "ŭ" "u" "i" "o" "p" "ĵ" "ĥ" "\\"
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "" ""
    "" ""  "z" "ĉ" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+"  ""
    "" ""  "Ŝ" "Ĝ" "E" "R" "T" "Ŭ" "U" "I" "O" "P" "Ĵ" "Ĥ" "|"
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "" "" ""
    "" ""  "Z" "Ĉ" "C" "V" "B" "N" "M" "<" ">" "?" "" "" "")
  "Esperanto layout.")

(defvar ergoemacs-layout-eo-displaced
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "ŝ" "ĵ" "e" "r" "t" "ĝ" "u" "i" "o" "p" "[" "]" "\\"
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ŭ" "ĥ" "" ""
    "" ""  "z" "ĉ" "c" "v" "b" "n" "m" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "\"" "#" "$" "%" "'" "&" "*" "(" ")" "_" "+"  ""
    "" ""  "Ŝ" "Ĵ" "E" "R" "T" "Ĝ" "U" "I" "O" "P" "{" "}" "|"
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ŭ" "Ĥ" "" ""
    "" ""  "Z" "Ĉ" "C" "V" "B" "N" "M" ";" ":" "?" "" "" "")
  "Esperanto (displaced semicolon and quote, obsolete) layout.")

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

;; Thanks to Diego Efe
(defvar ergoemacs-layout-es-dv-1
  '("" "º" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "'" "¡" ""
    "" ""  "." "," "ñ" "p" "y" "f" "g" "c" "h" "l" "`" "+" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "r" "t" "n" "s" "'" "ç"  ""
    "" "<" "-" "q" "j" "k" "x" "b" "m" "w" "v" "z" ""  ""  ""
    ;; Shifted
    "" "ª" "!" "\"" "·" "$" "%" "&" "/" "(" ")" "=" "?" "¿"  ""
    "" ""  ":" ";" "Ñ" "P" "Y" "F" "G" "C" "H" "L" "^" "*" "|"
    "" ""  "A" "O" "E" "U" "I" "D" "R" "T" "N" "S" "¨" "Ç" ""
    "" ">" "_" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "Spanish Dvorak layout.  URL `http://djelibeibi.unex.es/dvorak'.")

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

(defvaralias 'ergoemacs-layout-ge 'ergoemacs-layout-de)

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

(defvar ergoemacs-layout-neo
  '("" "^" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "`" ""
    "" ""  "x" "v" "l" "c" "w" "k" "h" "g" "f" "q" "ß" "'" "\\"
    "" ""  "u" "i" "a" "e" "o" "s" "n" "r" "t" "d" "y" "" ""
    "" ""  "ü" "ö" "ä" "p" "z" "b" "m" "," "." "j" "" "" ""
    ;; Shifted
    "" "°" "ℓ" "§" "3" "»" "«" "$" "€" "„" "“" "”" "­" "¸" ""
    "" ""  "X" "V" "L" "C" "W" "K" "H" "G" "F" "Q" "ß" "~" "\\"
    "" ""  "U" "I" "A" "E" "O" "S" "N" "R" "T" "D" "Y" "" ""
    "" ""  "Ü" "Ö" "Ä" "P" "Z" "B" "M" "-" "·" "J" "" "" "")
  "Neo Layout.")

(defvar ergoemacs-layout-no
  '("" "|" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "+" "\\" ""
    "" ""  "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "å" "¨" ""
    "" ""  "a" "s" "d" "f" "g" "h" "j" "k" "l" "ø" "æ" "'" ""
    "" "<"  "z" "x" "c" "v" "b" "n" "m" "," "." "-" "" "" ""
    ;; Shifted
    "" "§" "!" "\"" "#" "¤" "%" "&" "/" "(" ")" "=" "?" "`" ""
    "" ""  "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "Å" "^" ""
    "" ""  "A" "S" "D" "F" "G" "H" "J" "K" "L" "Ø" "Æ" "*" ""
    "" ">"  "Z" "X" "C" "V" "B" "N" "M" ";" ":" "_" "" "" "")
  "Norwegian layout.")

(defvar ergoemacs-layout-programmer-dv
  '("" "$" "&" "[" "{" "}" "(" "=" "*" ")" "+" "]" "!" "#" ""
    "" ""  ";" "," "." "p" "y" "f" "g" "c" "r" "l" "/" "@" "\\"
    "" ""  "a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-" "" ""
    "" ""  "'" "q" "j" "k" "x" "b" "m" "w" "v" "z" "" "" ""
    ;; Shifted
    "" "~" "%"   "7" "5" "3" "1" "9" "0" "2" "4" "6" "8" "`"  ""
    "" ""   ":"  "<" ">" "P" "Y" "F" "G" "C" "R" "L" "?" "^" "|"
    "" ""   "A"  "O" "E" "U" "I" "D" "H" "T" "N" "S" "_" "" ""
    "" ""   "\"" "Q" "J" "K" "X" "B" "M" "W" "V" "Z" "" "" "")
  "US Programmer Dvorak layout.")

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

(defvaralias 'ergoemacs-layout-sp 'ergoemacs-layout-es)

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

(defvaralias 'ergoemacs-layout-us_dvorak 'ergoemacs-layout-dv)

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

(defvar ergoemacs-layout-workman
  '("" "`" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" ""
    "" ""  "й" "d" "r" "w" "b" "j" "f" "u" "p" ";" "[" "]" "\\"
    "" ""  "a" "s" "h" "t" "g" "y" "n" "e" "o" "i" "'" "" ""
    "" ""  "z" "x" "m" "c" "v" "k" "l" "," "." "/" "" "" ""
    ;; Shifted
    "" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+" ""
    "" ""  "Q" "D" "R" "W" "B" "J" "F" "U" "P" ":" "{" "}" "|"
    "" ""  "A" "S" "H" "T" "G" "Y" "N" "E" "O" "I" "\"" "" ""
    "" ""  "Z" "X" "M" "C" "V" "K" "L" "<" ">" "?" "" "" "")
  "US Workman layout.  URL `http://www.workmanlayout.com/blog/'.")

(defvar ergoemacs-layout-ru
  '("" "" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "\\"
    "" ""  "й" "ц" "у" "к" "е" "н" "г" "ш" "щ" "з" "х" "ъ" "" 
    "" ""  "ф" "ы" "в" "а" "п" "р" "о" "л" "д" "ж" "э"  "" ""
    "" ""  "я" "ч" "с" "м" "и" "т" "ь" "б" "ю" "." "" "" ""
    ;; Shifted
    "" "Ë" "!" "\"" "№" ";" "%" ":" "?" "*" "(" ")" "_" "+" "/"
    "" ""  "Й" "Ц" "У" "К" "Е" "Н" "Г" "Ш" "Щ" "З" "Х" "Ъ" "" 
    "" ""  "Ф" "Ы" "В" "А" "П" "Р" "О" "Л" "Д" "Ж" "Э" "" ""
    "" "" "Я" "Ч" "С" "М" "И" "Т" "Ь" "Б" "Ю" "," "" "" "")
  "Russian/Cryllic jcuken layout.")

(defvaralias 'ergoemacs-layout-jcuken 'ergoemacs-layout-ru)

(require 'help-mode)
(defvar quail-keyboard-layout-alist)
(defvar ergoemacs-keyboard-layout)
(defvar ergoemacs-dir)
(defvar ergoemacs-inkscape)
(declare-function ergoemacs-save "ergoemacs-lib")
(declare-function ergoemacs-translate-layout "ergoemacs-translate")
(declare-function ergoemacs-translate--svg-layout "ergoemacs-translate")
(declare-function ergoemacs-translate--png-layout "ergoemacs-translate")
(declare-function ergoemacs-component--prompt "ergoemacs-component")
(declare-function quail-insert-kbd-layout "quail")

(defun ergoemacs-layouts--current (&optional layout)
  "Gets the LAYOUT symbol.
If LAYOUT is unspecified, use `ergoemacs-keyboard-layout'."
  (intern (format "ergoemacs-layout-%s" (or layout ergoemacs-keyboard-layout))))

;;; Layout Functions
(defun ergoemacs-layouts--customization-type ()
  "Gets the customization types for `ergoemacs-keyboard-layout'."
  `(choice
    ,@(mapcar
              (lambda(elt)
                `(const :tag ,elt :value ,elt))
              (sort (ergoemacs-layouts--list t) 'string<))))

(defun ergoemacs-layouts--menu ()
  "Gets the keymap entry for ergoemacs-layouts."
  `(ergoemacs-keyboard-layout
    menu-item "Keyboard Layouts"
    (keymap
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
            `(,variable
              menu-item ,(concat lay " - " doc)
              (lambda() (interactive)
                (ergoemacs-set-layout ,lay))
              :button (:radio . (string= ergoemacs-keyboard-layout ,lay)))))
        (sort (ergoemacs-layouts--list) 'string<)))))

(defun ergoemacs-layouts--custom-documentation (&optional lays ini)
  "Get a documentation list of all known layouts.

LAYS is the layouts being processed.

If INI is non-nil, create information about the autohotkey ini
file."
  (let ((lays (or lays (sort (ergoemacs-layouts--list t) 'string<))))
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
         (if ini
             (concat lay "=" doc)
           (concat "\"" lay "\" (" doc ")" (if is-alias ", alias" "")))))
     lays "\n")))

(defvar ergoemacs-layouts--no-aliases nil)
(defvar ergoemacs-layouts--aliases nil)

(defun ergoemacs-layouts--reset ()
  "Reset Layout information."
  (interactive)
  (setq ergoemacs-layouts--no-aliases nil)
  (setq ergoemacs-layouts--aliases nil))

(defun ergoemacs-layouts--list (&optional aliases ob)
  "Get the list of all known layouts.

When ALIASES is non-nil, list aliases and actuval variables.

OB is the object array."
  (if (and ergoemacs-layouts--no-aliases
           (not aliases))
      ergoemacs-layouts--no-aliases
    (if (and ergoemacs-layouts--aliases
             aliases)
        ergoemacs-layouts--aliases
      (let (ret)
        (mapatoms
         (lambda(s)
           (let ((sn (symbol-name s)))
             (and (string-match "^ergoemacs-layout-" sn)
                  (if (and (not (functionp s))
                           (or aliases
                               (and (not aliases)
                                    (documentation-property
                                     (intern sn) 'variable-documentation))))
                      (setq ret (cons (replace-regexp-in-string "ergoemacs-layout-" "" sn) ret))))))
         ob)
        (if aliases
            (setq ergoemacs-layouts--aliases nil)
          (setq ergoemacs-layouts--no-aliases nil))
        ret))))

(defun ergoemacs-layout (layout)
  "Set the ergoemacs layout to LAYOUT."
  (ergoemacs-save 'ergoemacs-keyboard-layout layout))

(defvar ergoemacs-layout--quail-alist nil)
(defun ergoemacs-layout--quail-alist (&optional restore)
  "Modify the QUAIL `quail-keyboard-layout' variable.

This is based on the number of layouts defined by
`ergoemacs-mode'.

When RESTORE is non-nil, restore the `quail-keyboard-layout'"
  (cond
   ((and ergoemacs-layout--quail-alist (eq restore :refresh))
    (setq quail-keyboard-layout-alist
          (append ergoemacs-layout--quail-alist
                  (mapcar
                   (lambda(lay)
                     (cons lay (ergoemacs-translate-layout lay :quail)))
                   (ergoemacs-layouts--list)))))
   ((and ergoemacs-layout--quail-alist restore)
    (setq quail-keyboard-layout-alist ergoemacs-layout--quail-alist
          ergoemacs-layout--quail-alist nil))
   ;; Do nothing
   (ergoemacs-layout--quail-alist)
   (t
    ;; Add ergoemacs-mode layouts to `quail-keyboard-layout-alist'.
    (setq ergoemacs-layout--quail-alist quail-keyboard-layout-alist
          quail-keyboard-layout-alist
          (append quail-keyboard-layout-alist
                  (mapcar
                   (lambda(lay)
                     (cons lay (ergoemacs-translate-layout lay :quail)))
                   (ergoemacs-layouts--list)))))))

(defcustom ergoemacs-layouts-use-us-for-input-methods nil
  "Use the \"us\" layout for input methods.
Otherwise, `ergoemacs-mode' will try to adjust based on your layout."
  :type 'boolean
  :group 'ergoemacs-mode)

(defun ergoemacs-layout--update-quail ()
  "Tell quail of your currently used `ergoemacs-mode' layout."
  (when (featurep 'quail)
    (when ergoemacs-layouts-use-us-for-input-methods
      (quail-set-keyboard-layout (replace-regexp-in-string "ergoemacs-layout-" "" (symbol-name (ergoemacs :layout)))))))

(add-hook 'ergoemacs-init-hook #'ergoemacs-layout--update-quail)

(if (not (featurep 'quail))
    (eval-after-load 'quail
      '(progn
	 (ergoemacs-layout--quail-alist)
	 (ergoemacs-layout--update-quail)))
  (add-hook 'ergoemacs-init-hook #'ergoemacs-layout--quail-alist)
  (add-hook 'ergoemacs-init-hook #'ergoemacs-layout--update-quail))


(defalias 'ergoemacs-layout 'ergoemacs-set-layout)

(define-button-type 'ergoemacs-layout-help
  :supertype 'help-xref
  'help-function #'ergoemacs-layout-describe
  'help-echo (purecopy "mouse-2, RET: find this ergoemacs layout's definition"))

(defun ergoemacs-layout--regexp (&optional base)
  "Get a regular expression of recognized `ergoemacs-mode' layouts.

when BASE is non-nil, the regular expression shows the regular
expression matching the base layout."
  (let ((reg (regexp-opt (ergoemacs-layouts--list t) t))
        (f1 "[\"`']\\(%s\\)[\"`']")
        (f2 "Base Layout: \\(%s\\)"))
    (format (cond
             (base f2)
             (t f1)) (regexp-opt (ergoemacs-layouts--list) t))))

(defun ergoemacs-layout-describe (&optional layout)
  "Display the full documentation of an `ergoemacs-mode' LAYOUT.

LAYOUT can be either a symbol or string."
  (interactive (ergoemacs-component--prompt :layout))
  (let* ((layout (or (and layout
                          (or (and (stringp layout) layout)
                              (and (symbolp layout) (symbol-name layout))))
                     "us"))
         (s (intern (concat "ergoemacs-layout-" layout)))
         (sv (and (boundp s) (symbol-value s)))
         (el-file (find-lisp-object-file-name s 'defvar))
         (alias (condition-case nil
                    (indirect-variable s)
                  (error s)))
         (doc (or (documentation-property
                   s 'variable-documentation)
                  (documentation-property
                   s 'variable-documentation)))
         pt
         png svg)
    (unless (featurep 'quail)
      (require 'quail))
    (if (not sv)
        (message "You did not specify a valid ergoemacs layout %s" layout)
      (help-setup-xref (list #'ergoemacs-layout-describe (or layout "us"))
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (princ "`")
        (princ s)
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ "' is an `ergoemacs-mode' layout")
        (with-current-buffer standard-output
          (save-excursion
            (re-search-backward "`\\(ergoemacs-mode\\)'" nil t)
            (help-xref-button 1 'help-function-def 'ergoemacs-mode
                              (concat ergoemacs-dir "ergoemacs-mode.el"))))
        (when (file-readable-p el-file)
          (princ " defined in `")
          (princ (file-name-nondirectory el-file))
          (princ "'.")
          (with-current-buffer standard-output
            (save-excursion
              (re-search-backward "`\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-variable-def
                                s el-file)
              (re-search-backward "^`\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-variable-def
                                s el-file))))
        (princ "\n\n")
        (princ "Documentation:\n")
        (with-current-buffer standard-output
          (insert (or doc "Not documented as a layout.")))
        (princ "\n\n")
        (princ "Layout:\n")
        (cond
         ((and (setq svg (ergoemacs-translate--svg-layout (format "%s" layout)))
               (setq png (ergoemacs-translate--png-layout (format "%s" layout)))
               (image-type-available-p 'png))
          (with-current-buffer standard-output
            (insert-image (create-image png))
            (insert "\n")))
         ((and svg (image-type-available-p 'svg))
          (with-current-buffer standard-output
            (insert-image (create-image svg))
            (princ "\n"))))
        (with-current-buffer standard-output
          (if png
              (insert "[svg] [png]")
            (insert "[svg]"))
          (beginning-of-line)
          (if (looking-at "\\(\\[svg\\]\\) \\(\\[png\\]\\)")
              (progn
                (help-xref-button 1 'help-url svg)
                (help-xref-button 2 'help-url png))
            (if (looking-at "\\(\\[svg\\]\\)")
                (help-xref-button 1 'help-url svg)))
          (goto-char (point-max)))
        (princ "\n\n")
        (princ "ASCII Layout (Quail):\n")
        (with-current-buffer standard-output
          (quail-insert-kbd-layout (ergoemacs-translate-layout sv :quail)))
        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string))))))

(defalias 'describe-ergoemacs-layout 'ergoemacs-layout-describe)

(provide 'ergoemacs-layouts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-layouts.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
