;;; ergoemacs-command-loop.el --- Keyboard translation functions -*- lexical-binding: t -*-

;; Copyright © 2013-2015  Free Software Foundation, Inc.

;; Filename: ergoemacs-command-loop.el
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

(defun ergoemacs-command-loop--redefine-quit-key (&optional key)
  "Redefines the quit-key in emacs.

Typically the emacs quit key is Ctrl+g, but it can be redefined
with this function."
  (let ((cur-input (current-input-mode))
        (new-key (listify-key-sequence (or key [7]))))
    (when (> 1 (length new-key))
      (error "Will not set a key sequence to the emacs key sequence"))
    (setq cur-input (reverse (append new-key (cdr (reverse cur-input)))))
    (apply 'set-input-mode cur-input)))

(defun ergoemacs-command-loop--universal-argument (&optional type)
  "Ergoemacs universal argument.
This is called through `ergoemacs-command-loop'"
  (interactive)
  (setq current-prefix-arg '(4))
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-universal-argument 'ergoemacs-command-loop--universal-argument)


(defun ergoemacs-command-loop--digit-argument (&optional type)
  "Ergoemacs digit argument.
 This is called through `ergoemacs-command-loop'"
  (interactive)
  (let* ((char (if (integerp last-command-event)
                   last-command-event
                 (get last-command-event 'ascii-character)))
         (digit (- (logand char ?\177) ?0)))
    (setq current-prefix-arg digit))
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-digit-argument 'ergoemacs-command-loop--digit-argument)

(defun ergoemacs-command-loop--negative-argument (&optional type)
  "Ergoemacs negative argument.
This is called through `ergoemacs-command-loop'"
  (setq current-prefix-arg '-)
  (ergoemacs-command-loop nil type nil t))

(defalias 'ergoemacs-negative-argument 'ergoemacs-command-loop--negative-argument)

;; Command Loop

;; (1) Read Key sequence
(defun ergoemacs-command-loop ()
  )

;; (2) Key sequence translated to command
(defun ergoemacs-command-loop--key-lookup ()
  )

(defvar ergoemacs-command-loop--execute-modify-command-list
  '(last-repeatable-command
    this-command
    this-original-command
    mc--this-command)
  "Commands that will be set to what `ergoemacs-command-loop' executes.")

(defvar ergoemacs-command-loop--shift-translated nil
  "This command is forced to be shift translated.")

(defvar ergoemacs-command-loop--mark-active nil
  "Determines if mark was active before ergoemacs command loop.")

(defvar ergoemacs-command-loop--pre-command-hook nil
  "Pre-command hook for `ergoemacs-mode' command loop.")

(defun ergoemacs-command-loop--pre-command-hook ()
  "Command loop `pre-command-hook.'"
  (setq ergoemacs-command-loop--mark-active mark-active)
  ;; Defer pre-command hook for functions that induce the
  ;; `ergoemacs-mode' command loop.
  (unless (ergoemacs :command-loop-p this-command)
    (run-hooks 'ergoemacs-command-loop--pre-command-hook)))

(add-hook 'ergoemacs-pre-command-hook #'ergoemacs-command-loop--pre-command-hook)

(defvar ergoemacs-command-loop--deferred-functions
  '(delete-selection-pre-hook ac-handle-pre-command cua--pre-command-handler mc/make-a-note-of-the-command-being-run)
  "Hooks that are moved to `ergoemacs-pre-command-hook'.
These hooks are deferred to make sure `this-command' is set appropriately.")

(defun ergoemacs-command-loop--depopulate-pre-command-hook ()
  "Depopulate `ergoemacs-command-loop--pre-command-hook' with `pre-command-hook' values."
  (ergoemacs-command-loop--populate-pre-command-hook t))

(add-hook 'ergoemacs-mode-shutdown-hook #'ergoemacs-command-loop--depopulate-pre-command-hook)

(defun ergoemacs-command-loop--populate-pre-command-hook (&optional depopulate)
  "Populate `ergoemacs-command-loop--pre-command-hook' with `pre-command-hook' values."
  (let ((from-hook (or (and depopulate 'ergoemacs-command-loop--pre-command-hook)
                       'pre-command-hook))
        do-append ergoemacs-mode)
    (dolist (item (default-value from-hook))
      (if (eq item t)
          (setq do-append t)
        (unless (or depopulate (not (memq item ergoemacs-command-loop--deferred-functions)))
          (add-hook 'ergoemacs-command-loop--pre-command-hook item do-append nil)
          (remove-hook 'pre-command-hook item nil))
        (when depopulate
          (add-hook 'pre-command-hook item do-append nil)
          (remove-hook 'ergoemacs-command-loop--pre-command-hook item do-append))))
    (save-excursion
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (unless (equal (default-value from-hook)
                         (ergoemacs-sv from-hook))
            (setq do-append nil)
            (dolist (item (ergoemacs-sv from-hook))
              (if (eq item t)
                  (setq do-append t)
                (unless (or depopulate (not (memq item ergoemacs-command-loop--deferred-functions)))
                  (add-hook 'ergoemacs-command-loop--pre-command-hook item do-append t)
                  (remove-hook 'pre-command-hook item t))
                (when depopulate
                  (add-hook 'pre-command-hook item do-append t)
                  (remove-hook 'ergoemacs-command-loop--pre-command-hook item t))))))))))

(add-hook 'ergoemacs-mode-startup-hook #'ergoemacs-command-loop--populate-pre-command-hook)

(defun ergoemacs-command-loop--execute-handle-shift-selection ()
  "Allow `ergoemacs-mode' command loop to handle shift selection.
This allows shift-selection of non-letter keys.
For instance in QWERTY M-> is shift translated to M-."
  (let ((this-command-keys-shift-translated
         (or this-command-keys-shift-translated
             (if ergoemacs-command-loop--shift-translated t nil))))

    (when (and ergoemacs-command-loop--shift-translated
               (ergoemacs :movement-p function))
      (cond
       ((and shift-select-mode
             ergoemacs-command-loop--mark-active
             (not mark-active))
        ;; Mark was active, then it was deactivated, now activate
        ;; again.
        (setq transient-mark-mode
              (cons 'only
                    (unless (eq transient-mark-mode 'lambda)
                      transient-mark-mode))
              mark-active t))
       (t ;; Mark was not active, activate mark.
        (handle-shift-selection))))))

(defun ergoemacs-command-loop--execute-rm-keyfreq (command)
  "Remove COMMAND from `keyfreq-mode' counts."
  (when (featurep 'keyfreq)
    (when keyfreq-mode
      (let (count)
        (setq count (gethash (cons major-mode command) keyfreq-table))
        (cond
         ((not count))
         ((= count 1)
          (remhash (cons major-mode command) keyfreq-table))
         (count
          (puthash (cons major-mode command) (- count 1)
                   keyfreq-table)))
        ;; Add local-fn to counter.
        (setq count (gethash (cons major-mode function) keyfreq-table))
        (puthash (cons major-mode function) (if count (+ count 1) 1)
                 keyfreq-table)))))

;; (3) execute command
(defun ergoemacs-command-loop--execute (command &optional keys)
  ;; The emacs command loop should preserve the undo command by
  ;; running `undo-boundary'.

  (cond
   ((or (stringp command) (vectorp command))
    ;; If the command is a keyboard macro (string/vector) then execute
    ;; it though `execute-kbd-macro'
    )
   (t
    ;; This should be a regular command.
    
    ;; Remove counting of `this-command' in `keyfreq-mode'
    (ergoemacs-command-loop--execute-rm-keyfreq this-command)
    
    ;; This command execute should modify the following variables:
    ;; - `last-repeatable-command'
    ;; - `this-command'
    ;; - `this-original-command'
    
    ;; In addition, other minor modes may store the command, so these
    ;; should be modified as well.
    
    ;; These are stored in `ergoemacs-command-loop--execute-modify-command-list'
    
    (dolist (var ergoemacs-command-loop--execute-modify-command-list)
      (set var command))
    
    ;; Handle Shift Selection
    (ergoemacs-command-loop--execute-handle-shift-selection)
    
    ))
  
  

  
  ;; - `last-nonmenu-event' -- does not count events from mouse/menus
  ;; - `last-command-event' -- Last input event
  ;; - `last-event-frame' -- (should be correct from emacs command loop)

  
  ;; In addition the following functions should be modified
  ;; - `this-command-keys'
  ;; - `this-command-keys-vector'
  
  ;; This allow `ergoemacs-mode' to fake the command keys that are
  ;; being supplied
  

  ;; After executing, the emacs loop should copy `this-command' into
  ;; `last-command'.
  ;; It should also change `last-prefix-arg'
    )

(defun ergoemacs-command-loop--run-pre-command-hook ()
  ;; `this-command' contains the command that is about to be run.
  ;; `last-command' describes the previous command
  )

(defun ergoemacs-command-loop--run-post-command-hook ()
  ;; `this-command' contains the command that was just run.
  ;; `last-command' contains the command before that.
  ;; Also run when emacs first enters the command loop.
  ;; `this-command' and `last-command' are both `nil'
  )

(provide 'ergoemacs-command-loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ergoemacs-command-loop.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
