[![MELPA Stable](http://stable.melpa.org/packages/ergoemacs-mode-badge.svg)](http://stable.melpa.org/#/ergoemacs-mode)
[![MELPA](http://melpa.org/packages/ergoemacs-mode-badge.svg)](http://melpa.org/#/ergoemacs-mode)
[![Build Status](https://secure.travis-ci.org/ergoemacs/ergoemacs-mode.png)](http://travis-ci.org/ergoemacs/ergoemacs-mode)

#  Ergoemacs Keybindings 

 Xah Lee, David Capello, Kin Storm, Walter Landry and Matthew Fidler

## Library Information

ErgoEmacs keybindings improve GNU Emacs for people who did not grew
up with Emacs. User interface is based on common modern software
interface familiar to most people today, such as using 【Ctrl+C】 key
for Copy,【Ctrl+Z】 for undo, 【Ctrl+O】 for Open file, and also
bundles many Emacs Lisp functions that are not in GNU Emacs by default.

```lisp
    (setq ergoemacs-theme nil)
    (setq ergoemacs-keyboard-layout "us")
    (require 'ergoemacs-mode)
    (ergoemacs-mode 1)
```

## Changing your own bindings

If you want to change your bindings, you can define keys in the `ergoemacs-user-keymap`.

For example, if you want to change `C-a` you can use the following Emacs customization command:

```lisp
(define-key ergoemacs-user-keymap (kbd "C-a") 'my-replacement-function)
```

If you wanted this to respect the keyboard layout for some reason, you can use:

```lisp
(ergoemacs-define-key ergoemacs-user-keymap (kbd "M-i") 'my-other-function)
```

This defines the Alt+i key on QWERTY and Alt+u on colemak.

You can also define keys that are partially layout dependent and partially fixed. 

For example if you wanted the ergoemacs-mode key `<menu> n t` to start
term-mode instead of `org-capture`, you can define the key as follows:

```lisp
(ergoemacs-define-key ergoemacs-user-keymap (kbd "<menu> n") 'org-capture (kbd "t"))
```

 In QWERTY, this key would be `<menu> n t`, in Colemak, this key would be `<menu> k t`
 
 ## How `ergoemacs-mode' works the "magic"
 
 `ergoemacs-mode` binds all of its keys in the
 `emulation-mode-map-alist`.  As a reference, Emacs looks up keys from
 the active keymap following the lisp-like pseudo-code below:
 
 ```lisp
 (or (if overriding-terminal-local-map
         (find-in overriding-terminal-local-map)
       (if overriding-local-map
           (find-in overriding-local-map)))
     (or (find-in (get-char-property (point) 'keymap))
         (find-in-any emulation-mode-map-alists)
         (find-in-any minor-mode-overriding-map-alist)
         (find-in-any minor-mode-map-alist)
         (if (get-text-property (point) 'local-map)
             (find-in (get-char-property (point) 'local-map))
           (find-in (current-local-map))))
     (find-in (current-global-map)))
 ```
This means that `ergoemacs-mode` overrides: 
- keybindings from minor modes
- keybindings from major modes (which are contained in the `current-local-map`)

While this seems useful, many major and minor modes make meaningful
changes in Emacs keybindings.  For example `gnus` binds the default
key of `kill-line`, that is `C-k` to `gnus-summary-kill-same-subject`
and sometimes other functions depending on what part of `gnus` you are
in.  The corresponding key in `ergoemacs-mode` in the "us" layout is `M-g`.  
If you make changes to the gnus keymap to bind `M-g` to `gnus-summary-kill-same-subject`, 
Emacs still overrides this key with whatever `ergoemacs-mode` has defined in the `gnus` keymap.  

To overcome this there `ergoemacs-mode` does the following:

- Intercepts the `ergoemacs-mode` key for `kill-line`
- Temporarily disables all `ergoemacs-mode` keys and adds the original
  key `C-k` to the unread events (and makes sure it isn't recorded)
- Emacs then carries out the correct command by processing the unread keys
- `ergoemacs-mode` re-enables the keybindings

It will also keep the shift selection active by sending shifted keys if necessary. 

If you think this is too much magic, you can turn this off with the variable `ergoemacs-mode-send-emacs-keys`
