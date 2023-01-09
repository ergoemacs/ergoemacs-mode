;;; ergoemacs-cua.el --- Keyboard keybinding translation -*- lexical-binding: t -*-

;; Copyright © 2013-2021  Free Software Foundation, Inc.

;; Filename: ergoemacs-cua.el
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Adapted from https://github.com/emacs-mirror/emacs/blob/3ae275eedc1c8d2a61a4a549c39c88bb08fd8ff2/lisp/emulation/cua-base.el#L623


(defvar ergoemacs--prefix-override-timer nil
  "Override timer for cua-style keys.")

(defvar ergoemacs--prefix-override-length nil
  "The last saved key prefix override length.")

(defvar ergoemacs--prefix-override-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control x)] 'ergoemacs--prefix-override-handler)
    (define-key map [(control c)] 'ergoemacs--prefix-override-handler)
    map)
  "Prefix override keymap.")

(defvar ergoemacs--ena-prefix-repeat-keymap nil
  "Variable that states that `ergoemacs-mode' is in the repeat phase, immediately after using the prefix key.")

(defvar ergoemacs--prefix-repeat-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [(control x) (control x)] 'ergoemacs--prefix-repeat-handler)
    (define-key map [(control c) (control c)] 'ergoemacs--prefix-repeat-handler)
    (dolist (key '(up down left right home end next prior))
      (define-key map (vector '(control x) key) 'ergoemacs--prefix-cut-handler)
      (define-key map (vector '(control c) key) 'ergoemacs--prefix-copy-handler)))
  "Prefix repeat keymap.")


(defcustom ergoemacs-prefix-override-inhibit-delay 0.2
  "If non-nil, time in seconds to delay before overriding prefix key.
If there is additional input within this time, the prefix key is
used as a normal prefix key.  So typing a key sequence quickly will
inhibit overriding the prefix key.
As a special case, if the prefix key is repeated within this time, the
first prefix key is discarded, so typing a prefix key twice in quick
succession will also inhibit overriding the prefix key.
If the value is nil, use a shifted prefix key to inhibit the override."
  :type '(choice (number :tag "Inhibit delay")
		         (const :tag "No delay" nil))
  :group 'ergoemacs)

(defcustom ergoemacs-enable-cua-keys t
  "Enable C-x and C-v for cut and copy.
If the value is t, these mappings are always enabled.  If the value is
`shift', these keys are only enabled if the last region was marked with
a shifted movement key.  If the value is nil, these keys are never
enabled."
  :type '(choice (const :tag "Disabled" nil)
		         (const :tag "Shift region only" shift)
		         (other :tag "Enabled" t))
  :group 'cua)

(defvar ergoemacs--ena-region-keymap nil
  "Variable that tells the `ergoemacs-mode' if the region is selected.

This is also used to select the region keymaps.")

(defvar ergoemacs--ena-prefix-override-keymap nil
  "Variable that tels the `ergoemacs-mode' of the overide step is active.

This override is enabled for active regions before the copy and paste are enabled.")

(defvar ergoemacs-inhibit-cua-keys nil
  "Buffer-local variable that may disable the CUA keymappings.")
(make-variable-buffer-local 'ergoemacs-inhibit-cua-keys)

(defvar ergeoemacs-mode-term-raw-mode)
(defvar ergoemacs-mode)
(defvar ergoemacs--temporary-disable)
(defvar ergoemacs-mode-regular)
(defvar ergoemacs-mode-send-emacs-keys)
(defvar ergoemacs-send-keys-term)
(defvar term-raw-map)
(defun ergoemacs--select-keymaps ()
  "Setup conditions for selecting the proper keymaps in `ergoemacs--keymap-alist'."
  (if (and (eq major-mode 'term-mode)
           (eq (current-local-map) term-raw-map))
      (setq ergoemacs-mode-regular nil
            ergoemacs-mode-send-emacs-keys nil
            ergeoemacs-mode-term-raw-mode t)
    (when ergeoemacs-mode-term-raw-mode
      (setq ergeoemacs-mode-term-raw-mode nil
            ergoemacs-mode-regular t
            ergoemacs-mode-send-emacs-keys ergoemacs-send-keys-term
            ergoemacs--temporary-disable nil))
    (when ergoemacs--temporary-disable
      ;; The temporary disable commands set `ergoemacs--temporary-disable' to t
      ;; The first time when the keys are put on the `unread-command-events', `ergoemacs-mode' is disabled
      ;; The second command is executed, and `ergoemacs-mode' is turned back on and `ergoemacs--temporary-disable' is to nil
      (if ergoemacs-mode-regular
          (progn
            (setq ergoemacs--ena-region-keymap nil
                  ergoemacs--ena-prefix-override-keymap nil
                  ergoemacs--ena-prefix-repeat-keymap nil
                  ergoemacs-mode-regular nil
                  ergoemacs-mode-send-emacs-keys nil))
        (setq ergoemacs--temporary-disable nil
              ergoemacs-mode-regular t
              ;; This assumes that `ergoemacs--tempoary-disable' is only called on the remap keys layer
              ergoemacs-mode-send-emacs-keys t)))
    (when ergoemacs-mode
      ;; The prefix override (when mark-active) operates in three substates:
      ;; [1] Before using a prefix key
      ;; [2] Immediately after using a prefix key
      ;; [3] A fraction of a second later
      (setq ergoemacs--ena-region-keymap ; Determines if the ergion is active
            (and (not ergeoemacs-mode-term-raw-mode) (region-active-p) (not deactivate-mark))
            ;; Enable Override -- This is the first state where the keys are intercepted; cua state [1]
            ergoemacs--ena-prefix-override-keymap
            (and ergoemacs--ena-region-keymap
                 (not ergeoemacs-mode-term-raw-mode)
                 ergoemacs-enable-cua-keys
	             (not ergoemacs-inhibit-cua-keys)
	             (or (eq ergoemacs-enable-cua-keys t)
		             (region-active-p))
	             (not executing-kbd-macro)
	             (not ergoemacs--prefix-override-timer))
            ;; Enable The repeat layer.  This is the layer that the keys are intercepted; cua state [2]
            ergoemacs--ena-prefix-repeat-keymap
            (and ergoemacs--ena-region-keymap
                 (not ergeoemacs-mode-term-raw-mode)
	             (or (timerp ergoemacs--prefix-override-timer)
		             (eq ergoemacs--prefix-override-timer 'shift)))))))

(defun ergoemacs--prefix-override-timeout ()
  "This is whap happens on the `ergoemacs-mode' timeout for C-c and C-v are supplied."
  (setq ergoemacs--prefix-override-timer t)
  (when (= (length (this-command-keys)) ergoemacs--prefix-override-length)
    (setq unread-command-events (cons 'ergoemacs-timeout unread-command-events))
    (if prefix-arg
        nil
      ;; FIXME: Why?
      (setq overriding-terminal-local-map nil))
    (ergoemacs--select-keymaps)))

(defun ergoemacs-prefix-command-preserve-state ()
  "Compatibility layer for `prefix-command-preserve-state'."
  (if (fboundp 'prefix-command-preserve-state)
      (prefix-command-preserve-state)
    (setq prefix-arg current-prefix-arg)
    (reset-this-command-lengths)))

(defun ergoemacs--prefix-override-replay (repeat)
  "This replays the events from the intial key press.

REPEAT is the flag that tells it if is repeated environmennt."
  (let* ((keys (this-command-keys))
	     (i (length keys))
	     (key (aref keys (1- i))))
    (setq ergoemacs--prefix-override-length (- i repeat))
    (setq ergoemacs--prefix-override-timer
	      (or
	       ;; In state [2], change to state [3]
	       (> repeat 0)
	       ;; In state [1], change directly to state [3]
	       (input-pending-p)
	       ;; In state [1], [T] disabled, so change to state [3]
	       (not (numberp ergoemacs-prefix-override-inhibit-delay))
	       (<= ergoemacs-prefix-override-inhibit-delay 0)
	       ;; In state [1], start [T] and change to state [2]
	       (run-with-timer ergoemacs-prefix-override-inhibit-delay nil
			               #'ergoemacs--prefix-override-timeout)))
    ;; Don't record this command
    (setq this-command last-command)
    ;; Restore the prefix arg
    ;; This should make it so that exchange-point-and-mark gets the prefix when
    ;; you do C-u C-x C-x C-x work (where the C-u is properly passed to the C-x
    ;; C-x binding after the first C-x C-x was rewritten to just C-x).
    (ergoemacs-prefix-command-preserve-state)
    
    ;; Push the key back on the event queue
    (if (version< emacs-version "26.2")
        (setq unread-command-events (cons key unread-command-events))
      (setq unread-command-events (cons (cons 'no-record key)
                                        unread-command-events)))))


(defun ergoemacs--prefix-override-handler ()
  "Start timer waiting for prefix key to be followed by another key.
Repeating prefix key when region is active works as a single prefix key."
  (interactive)
  (ergoemacs--prefix-override-replay 0))

(defun cua--prefix-repeat-handler ()
  "Repeating prefix key when region is active works as a single prefix key."
  (interactive)
  (ergoemacs--prefix-override-replay 1))

(defun ergoemacs--prefix-copy-handler (arg)
  "Copy region, then replay last key.

This uses `ergoemacs-copy-line-or-region' (unlike `cua-mode').

Pass prefix ARG to the respective copy functions."
  (interactive "P")
  (ergoemacs-copy-line-or-region arg)
  ;; Send next key
  (let ((keys (this-single-command-keys)))
    (setq unread-command-events
	      (cons (aref keys (1- (length keys))) unread-command-events))))

(defun cua--prefix-cut-handler (arg)
  "Cut region, then replay last key.

This uses `ergoemacs-cut-line-or-region' (unlike `cua-mode').

Pass prefix ARG to the respective copy functions."
  (interactive "P")
  (ergoemacs-cut-line-or-region arg)
  (let ((keys (this-single-command-keys)))
    (setq unread-command-events
	      (cons (aref keys (1- (length keys))) unread-command-events))))

(defvar ergoemacs-mode)
;;; Pre-command hook

(defun ergoemacs--cua-pre-command-handler-1 ()
  "Cancel prefix key timeout if user enters another key."
  (when ergoemacs--prefix-override-timer
    (if (timerp ergoemacs--prefix-override-timer)
	    (cancel-timer ergoemacs--prefix-override-timer))
    (setq ergoemacs--prefix-override-timer nil)))

(defun ergoemacs--cua-pre-command-handler ()
  "Cancel prefix key timeout if user enters another key. (has error protection)"
  (when ergoemacs-mode
    (condition-case nil
	    (ergoemacs--cua-pre-command-handler-1)
      (error nil))))


(defun ergoemacs--cua-post-command-handler-1 ()
  "Post command hook for ergoemacs-mode based cua-keys."
  ;; Select the keymaps for the next command
  (ergoemacs--select-keymaps))

(defun ergoemacs--cua-post-command-handler ()
  "Post command hook for `ergoemacs-mode' based cua keys."
  (when ergoemacs-mode
    (condition-case nil
      (ergoemacs--cua-post-command-handler-1)
    (error nil))))

(add-hook 'post-command-hook #'ergoemacs--cua-post-command-handler)
(add-hook 'pre-command-hook  #'ergoemacs--cua-pre-command-handler)

(defun ergoemacs--shift-control-prefix (prefix)
   "Handle S-C-x and S-C-c by emulating the fast double prefix function.
PREFIX is the key prefix that is being sent for these keys."
  ;; Don't record this command
  (setq this-command last-command)
  ;; Restore the prefix arg
  ;; This should make it so that exchange-point-and-mark gets the prefix when
  ;; you do C-u S-C-x C-x work (where the C-u is properly passed to the C-x
  ;; C-x binding after the first S-C-x was rewritten to just C-x).
  (ergoemacs-prefix-command-preserve-state)
  ;; Activate the cua--prefix-repeat-keymap
  (setq ergoemacs--prefix-override-timer 'shift)
  ;; Push duplicate keys back on the event queue
  (setq unread-command-events
        (cons prefix (cons prefix unread-command-events))))

(defun ergoemacs--shift-control-c-prefix ()
  "Shift control c prefix."
  (interactive)
  (ergoemacs--shift-control-prefix ?\C-c))

(defun ergoemacs--shift-control-x-prefix ()
  "Shift control x prefix."
  (interactive)
  (ergoemacs--shift-control-prefix ?\C-x))

(provide 'ergoemacs-cua)
;;; ergoemacs-cua.el ends here
