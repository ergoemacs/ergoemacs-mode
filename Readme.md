[![MELPA Stable](http://stable.melpa.org/packages/ergoemacs-mode-badge.svg)](http://stable.melpa.org/#/ergoemacs-mode)
[![MELPA](http://melpa.org/packages/ergoemacs-mode-badge.svg)](http://melpa.org/#/ergoemacs-mode)
[![Build Status](https://secure.travis-ci.org/ergoemacs/ergoemacs-mode.png)](http://travis-ci.org/ergoemacs/ergoemacs-mode)

#  Ergoemacs Keybindings 

 Xah Lee, David Capello, and Matthew Fidler

## Library Information



ErgoEmacs keybindings improve GNU Emacs for people who did not grew
up with Emacs. User interface is based on common modern software
interface familiar to most people today, such as using 【Ctrl+C】 key
for Copy,【Ctrl+Z】 for undo, 【Ctrl+O】 for Open file, and also
bundles many Emacs Lisp functions that are not in GNU Emacs by default.


    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "us")
    (require 'ergoemacs-mode)
    (ergoemacs-mode 1)

Ergoemacs-mode makes use of `make-composed-keymap` and therefore is
only comptabile with emacs 24.1+


More information is found at http://ergoemacs.github.io
