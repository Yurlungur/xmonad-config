Coniguring Emacs
===

There are two pieces to my emacs configuration:
- The `.emacs` file
- Client-server mode

## The emacs file

My emacs file is available in the `dotfiles` directory:
```bash
cp dotfiles/home.emacs ~/.emacs
```

*I make no guarantees that everything in it will work for you.* There
are a few pieces of it you might find interesting, however.

### Macros

The following macros are useful when trying to load a module or
function that may not exist:
```lisp
(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))
(defmacro load-file-maybe (file)
  "*Try to load FILE, but don't signal an error if the file doesn't exist."
  (when (file-exists-p file)
    (load-file file)))
```

### Window resizing and zoom

These functions let you resize the window, indent a buffor, or zoom:
```lisp
;; Functions to make windows resize in a nice way
;; ----------------------------------------------------------------------
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))
;; ----------------------------------------------------------------------
```

I've added them to my keybindings via
```lisp
;; C-+ to increase font size, C-- to decrease
(global-set-key (kbd "C-+") '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]  '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--") '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))
;; dynamic window resize
(global-set-key [S-C-down] 'win-resize-minimize-vert)
(global-set-key [S-C-up] 'win-resize-enlarge-vert)
(global-set-key [S-C-left] 'win-resize-minimize-horiz)
(global-set-key [S-C-right] 'win-resize-enlarge-horiz)
(global-set-key [S-C-up] 'win-resize-enlarge-horiz)
(global-set-key [S-C-down] 'win-resize-minimize-horiz)
(global-set-key [S-C-left] 'win-resize-enlarge-vert)
(global-set-key [S-C-right] 'win-resize-minimize-vert)
```

The remainder is rather standard. Loading different modules and modes, etc.

## Client-server mode

Emacs can run in client-server mode. To start the emacs daemon, call
```bash
emacs -u <your username> --fg-daemon
```
This will start the server. Then, to invoke a client, call
```bash
emacsclient
```
instead of `emacs`. I have the following bash alias in my `~/.bash_aliases`:
```bash
alias tmacs='emacsclient -nw'
alias gmacs='emacsclient -c'
```
whichs is very convenient. 
I describe how to start the server automatically in 
[07-configuring-systemd](07-configuring-systemd.md).
