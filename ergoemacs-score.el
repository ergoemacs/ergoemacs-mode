;;; ergoemacs-score.el --- Ergoemacs ergonomic score -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-score.el
;; Author: Matthew L. Fidler
;; Maintainer: 
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
(defvar ergoemacs-track-hand
  '(0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
      0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
  "Based on ergoemcs-layouts, which hand is typing?
0 represents left, 1 represents right.")

(defvar ergoemacs-track-row
  '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
      4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
      4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)
  "Based on ergoemacs-layouts, what row is being used?
1 = 1st row/number row
2 = 2nd row
3 = 3rd row/home row
4 = 4th row")

(defvar ergoemacs-track-finger
  ' (0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7
       0 0 0 1 2 3 3 4 4 5 6 7 7 7 7)
    "Track the finger based on the ergoemacs-layout.
0 = left pinky,
1 = left ring
2 = left middle
3 = left pointer
4 = right pointer
5 = right middle
6 = right ring
7 = right pinky
")

;; These are taken from http://www.colemak.com/wiki/index.php?title=Compare
(defvar ergoemacs-key-width 18.0
  "Assumption of key width (in px)")

(defvar ergoemacs-key-height 22.0
  "Assumption of key height (in px)")

(defvar ergoemacs-tab-key-width 28.0
  "Assumption of key width (in px)")

(defvar ergoemacs-lock-key-width 34.0
  "Assumption of lock key width (in px)")

(defvar ergoemacs-shift-key-width 26.0
  "Assumption of shift key width (in px)")

(defvar ergoemacs-return-key-width 36.0
  "Assumption of return key width (in px)")

(defvar ergoemacs-tab-key-width 28.0
  "Assumption of tab key width (in px)")

(defvar ergoemacs-key-width-m 0.010
  "Default key width (in m)")

(defvar ergoemacs-keyboard-coordinates-x nil
  "Keyboard x-coordinates (in m)")

(defvar ergoemacs-keyboard-coordinates-y nil
  "Keyboard y-coordinates (in m)")

(defun ergoemacs-calculate-keyboard-coordinates ()
  "Calculates `ergoemacs-keyboard-coordinates-x' and
`ergoemacs-keyboard-coordintes-y'"
  (setq ergoemacs-keyboard-coordinates-x
        (let ((i 0)
              (last 0)
              curr)
          (mapcar
           (lambda(_x)
             (setq i (+ i 1))
             (setq curr (+ last (/ ergoemacs-tab-key-width 2)))
             (cond
              ((or (= 17 i) (= 58 i))
               (setq last ergoemacs-tab-key-width))
              ((or (= 34 i) (= 75 i))
               (setq last ergoemacs-lock-key-width))
              ((or (= 41 i) (= 92 i))
               (setq last ergoemacs-shift-key-width))
              (t
               (setq last (+ last ergoemacs-key-width))))
             (* (/ ergoemacs-key-width-m ergoemacs-key-width) curr))
           ergoemacs-track-finger)))
  
  (setq ergoemacs-keyboard-coordinates-y
        (let ((i 0)
              (last 0)
              curr)
          (mapcar
           (lambda(_x)
             (setq i (+ i 1))
             (setq curr (+ last (/ ergoemacs-tab-key-width 2)))
             (cond
              ((= 58 i)
               (setq last 0))
              ((or (= 17 i) (= 34 i) (= 75 i)(= 41 i) (= 92 i))
               (setq last (+ last ergoemacs-tab-key-width))))
             (* (/ ergoemacs-key-width-m ergoemacs-key-width) curr))
           ergoemacs-track-finger))))

(ergoemacs-calculate-keyboard-coordinates)

(defun ergoemacs-key-properties (key layout &optional curr-i)
  "Key the KEY properties based on ergoemacs LAYOUT"
  (let ((i 0)
        (lay (intern-soft (format "ergoemacs-layout-%s" layout)))
        wi xh yh xc yc
        dx dy
        ret)
    (when lay
      (if curr-i
          (setq wi curr-i)
        (dolist (x (ergoemacs-sv lay))
          (when (string= key x)
            (setq wi i))
          (setq i (+ i 1))))
      (setq i wi)
      (setq xh (nth (if (<= (nth i ergoemacs-track-finger) 3)
                        (+ 32 (nth i ergoemacs-track-finger))
                      (+ 38 (- (nth i ergoemacs-track-finger) 4)))
                    ergoemacs-keyboard-coordinates-x))
      (setq yh (nth (if (<= (nth i ergoemacs-track-finger) 3)
                        (+ 32 (nth i ergoemacs-track-finger))
                      (+ 38 (- (nth i ergoemacs-track-finger) 4)))
                    ergoemacs-keyboard-coordinates-y))
      (setq xc (nth i ergoemacs-keyboard-coordinates-x))
      (setq yc (nth i ergoemacs-keyboard-coordinates-y))
      (setq dx (- xc xh))
      (setq dy (- yc yh))
      
      (setq ret
            `(:x ,xc
                 :y ,yc
                 :x-home ,xh
                 :y-home ,yh
                 :d-home ,(sqrt (+ (* dx dx) (* dy dy)))
                 
                 :hand ,(if (= 0 (nth i ergoemacs-track-hand))
                            'left
                          'right)

                 :finger ,(cond
                           ((or (= 0 (nth i ergoemacs-track-finger))
                                (= 7 (nth i ergoemacs-track-finger)))
                            'pinky)
                           ((or (= 1 (nth i ergoemacs-track-finger))
                                (= 6 (nth i ergoemacs-track-finger)))
                            'ring)
                           ((or (= 2 (nth i ergoemacs-track-finger))
                                (= 5 (nth i ergoemacs-track-finger)))
                            'middle)
                           (t
                            'pointer))

                 :finger-n ,(nth i ergoemacs-track-finger)

                 :row-n ,(nth i ergoemacs-track-row)

                 :row ,(cond
                        ((= 1 (nth i ergoemacs-track-row))
                         'number)
                        ((= 2 (nth i ergoemacs-track-row))
                         'top)
                        ((= 3 (nth i  ergoemacs-track-row))
                         'home)
                        ((= 4 (nth i ergoemacs-track-row))
                         'bottom))))
      ret)))

(defvar ergoemacs-key-hash nil
  "Key hash")

(defvar ergoemacs-distance-hash nil
  "Distance hash.")

(setq ergoemacs-distance-hash (make-hash-table :test 'equal))

(setq ergoemacs-key-hash (make-hash-table :test 'equal))

(declare-function ergoemacs-get-layouts "ergoemacs-layouts.el")
(dolist (layout (ergoemacs-get-layouts t))
  (let ((lay (intern-soft (format "ergoemacs-layout-%s" layout))))
    (when lay
      (dolist (key (ergoemacs-sv lay))
        (unless (string= key "")
          (puthash (cons layout key)
                   (ergoemacs-key-properties key layout)
                   ergoemacs-key-hash))))))

(defun ergoemacs-key-distance (key1 key2 &optional last-plist layout)
  "Gets the key distance based on the layout.
KEY1 is the first key pressed.
KEY2 is the second key pressed.
LAYOUT is the ergoemacs-layout used.
LAST-PLIST is the last property list returned by this function or nil if nothing was returned previously."
  (if layout
      (let ((ret (ergoemacs-gethash (cons (cons key1 key2) (cons last-plist layout)) ergoemacs-key-hash)))
        (if ret
            ret
          (let ((kp1 (ergoemacs-gethash (cons layout key1) ergoemacs-key-hash))
                (kp2 (ergoemacs-gethash (cons layout key2) ergoemacs-key-hash))
                kpl kpl1
                (kp12 (ergoemacs-gethash (cons layout (cons key1 key2)) ergoemacs-key-hash))
                dx dy)
            
            (when (and (not kp12) kp1 kp2
                       (eq (plist-get kp1 :finger-n) (plist-get kp2 :finger-n)))
              (setq dx (- (plist-get kp1 :x) (plist-get kp2 :x)))
              (setq dy (- (plist-get kp1 :y) (plist-get kp2 :y)))
              (setq kp12 (sqrt (+ (* dx dx) (* dy dy))))
              (puthash (cons layout (cons key1 key2)) kp12 ergoemacs-key-hash))
            
            (cond
             ((and (not last-plist) (not kp1) (not kp2))
              (setq ret `(:d 0 :dh 0
                             :finger-n -10
                             :key ,key2)))
             ((and last-plist (not kp1) (not kp2))
              (setq ret `(:d ,(plist-get last-plist :dh) :dh 0
                             :finger-n -10
                             :key ,key2)))
             ((and (not last-plist) kp1 (not kp2))

              ;; kp2 is not defined.  Assume space or no-length character.
              (setq ret `(:d ,(* 2 (plist-get kp1 :d-home)) :dh 0
                             :finger-n -10
                             :key ,key2)))

             ((and (not last-plist) (not kp1) kp2)
              ;; kp1 is not defined.  Assume space or no-length character.
              (setq ret `(:d ,(plist-get kp2 :d-home) :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))

             ((and last-plist (not kp1) kp2)
              ;; kp1 is not defined.  Assume space or no-length character.
              (setq ret `(:d ,(+ (plist-get last-plist :dh)
                                 (plist-get kp2 :d-home))
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))

             ((and last-plist kp1 (not kp2)
                   (eq (plist-get last-plist :finger-n) (plist-get kp1 :finger-n)))
              
              ;; Last keypress was on the same finger as kp1.  kp2 is a reset.
              (setq kpl (ergoemacs-gethash (cons layout (plist-get last-plist :key)) ergoemacs-key-hash))
              (setq kpl1 (ergoemacs-gethash (cons layout (cons (plist-get last-plist :key) key1))
                                  ergoemacs-key-hash))

              (when (not kpl1)
                (setq dx (- (plist-get kpl :x) (plist-get kp1 :x)))
                (setq dy (- (plist-get kpl :y) (plist-get kp1 :y)))
                (setq kpl1 (sqrt (+ (* dx dx) (* dy dy))))
                (puthash (cons layout
                               (cons (plist-get last-plist :key)
                                     key1)) kp12 ergoemacs-key-hash))
              (setq ret `(:d ,(+ kpl1 (plist-get kp1 :d-home)) :dh 0
                             :finger-n -10
                             :key ,key2)))
             ((and last-plist kp1 (not kp2))
              ;; last keypress was not on the same finger as kp1. kp2 is a
              ;; reset
              (setq ret `(:d ,(+ (plist-get last-plist :dh)
                                 (* 2 (plist-get kp1 :d-home)))
                             :dh 0
                             :finger-n -10
                             :key ,key2)))
             ((and (not last-plist)
                   (eq (plist-get kp1 :finger-n) (plist-get kp2 :finger-n)))

              ;; Distance when key1 and key2 are on the same finger is:
              ;; D(Home,Key1)+D(Key1,Key2)
              ;; Residual is D(Key2, Home)
              
              (setq ret `(:d ,(+ (plist-get kp1 :d-home) kp12) :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))
             ((not last-plist)

              ;; Distance when key1 and key2 are on a different finger is:
              ;; 2*D(Home,Key1)+D(Home,Key2)
              ;; Residual is D(Key2,Home)
              
              (setq ret `(:d ,(+ (* 2 (plist-get kp1 :d-home))
                                 (plist-get kp2 :d-home))
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))
             
             ((and (eq (plist-get last-plist :finger-n) (plist-get kp1 :finger-n))
                   (eq (plist-get last-plist :finger-n) (plist-get kp2 :finger-n)))
              
              ;; The last finger called is the same as the current finger
              ;; Key1 and Key 2 are on the same finger
              ;; Distance is D(Last-Key,Key1)+D(Key1,Key2)
              ;; Residual Distance is D(Key2,Home)
              
              (setq kpl (ergoemacs-gethash (cons layout (plist-get last-plist :key)) ergoemacs-key-hash))
              (setq kpl1 (ergoemacs-gethash (cons layout (cons (plist-get last-plist :key) key1))
                                  ergoemacs-key-hash))

              (when (not kpl1)
                (setq dx (- (plist-get kpl :x) (plist-get kp1 :x)))
                (setq dy (- (plist-get kpl :y) (plist-get kp1 :y)))
                (setq kpl1 (sqrt (+ (* dx dx) (* dy dy))))
                (puthash (cons layout
                               (cons (plist-get last-plist :key)
                                     key1)) kp12 ergoemacs-key-hash))
              
              (setq ret `(:d ,(+ kpl1 kp12)
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))
             ((and (eq (plist-get last-plist :finger-n) (plist-get kp1 :finger-n)))
              ;; The last finger is the same as kp1.  the kp1 finger is
              ;; different from kp2.
              ;;
              ;; Distance is D(Last,kp1)+D(kp1,home)+D(kp2,home)
              ;; Residual is D(kp2,home)
              (setq kpl (ergoemacs-gethash (cons layout (plist-get last-plist :key)) ergoemacs-key-hash))
              (setq kpl1 (ergoemacs-gethash (cons layout (cons (plist-get last-plist :key) key1))
                                  ergoemacs-key-hash))

              (when (not kpl1)
                (setq dx (- (plist-get kpl :x) (plist-get kp1 :x)))
                (setq dy (- (plist-get kpl :y) (plist-get kp1 :y)))
                (setq kpl1 (sqrt (+ (* dx dx) (* dy dy))))
                (puthash (cons layout
                               (cons (plist-get last-plist :key)
                                     key1)) kp12 ergoemacs-key-hash))

              (setq ret `(:d ,(+ kpl1 (plist-get kp1 :d-home) (plist-get kp2 :d-home))
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))
             ((and
               (not (eq (plist-get last-plist :finger-n) (plist-get kp1 :finger-n)))
               (eq (plist-get kp1 :finger-n) (plist-get kp2 :finger-n)))
              ;; The last finger called not the same as kp1
              ;; key1 and key2 are on the same finger.
              ;; Distance is D(Last-Key,home)+D(Key1,Key2)
              ;; Residual Distance is D(Key2,Home)
              (setq ret `(:d ,(+ (plist-get last-plist :dh) kp12)
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))
             (t
              ;; The three fingers are on different hands or the last finger
              ;; pressed and kp2 are on the same hand.  For this layout the
              ;; distance is given by:
              ;; d(Last,Home)+2*D(home,kp1)+D(home,kp2)
              ;; Residual distance is D(kp2,home)
              (setq ret `(:d ,(+ (plist-get last-plist :dh)
                                 (* 2 (plist-get kp1 :d-home))
                                 (plist-get kp2 :d-home))
                             :dh ,(plist-get kp2 :d-home)
                             :finger-n ,(plist-get kp2 :finger-n)
                             :key ,key2)))))
          (puthash (cons (cons key1 key2) (cons last-plist layout)) ret ergoemacs-key-hash)
          ret))
    (let (ret)
      (setq ret
            (mapcar
             (lambda(lay)
               (let (last-p
                     (dist (ergoemacs-gethash lay ergoemacs-distance-hash 0))
                     ret)
                 (when last-plist
                   (setq last-p (assoc lay last-plist))
                   (when last-p
                     (setq last-p (cadr last-p))))
                 (setq ret (ergoemacs-key-distance key1 key2 last-p lay))
                 (puthash lay (+ dist (plist-get ret :d)) ergoemacs-distance-hash)
                 `(,lay ,ret)))
             (ergoemacs-get-layouts)))
      ret)))

(defvar ergoemacs-last-distance-plist nil
  "Last distance plist")

(defvar ergoemacs-last-key-press nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-score.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
