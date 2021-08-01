(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server (quote ask))
 '(TeX-view-program-list
   (quote
    (("Okular" "okular --unique %o#src:%n%b")
     ("Evince" "evince %o"))))
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Evince")
     (output-pdf "Okular")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default)))
 '(ecb-options-version "2.40")
 '(package-selected-packages (quote (markdown-mode dracula-theme slime haskell-mode)))
 '(preview-gs-options
   (quote
    ("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-scale-function 1.5)
 '(python-mode-hook nil t)
 '(python-python-command "python3")
 '(python-remove-cwd-from-path nil)
 '(python-shell-interpreter "python3")
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; MACROS
;;-----------------------------------------------------------------------
;; Try to require FEATURE, but don't signal an error if 'require' fails.
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
;;-----------------------------------------------------------------------


;; Global Functions
;;-----------------------------------------------------------------------
(defun shell-command-maybe (exe &optional paramstr)
  "Run executable EXE with PARAMSTR, or warn if EXE is not available; eg. "
  " (shell-command-maybe \"ls\" \"-l -a\")"
  (if (executable-find exe)
      (shell-command (concat exe " " paramstr))
    (message (concat "'" exe "' not found found; please install"))))

(defun djcb-zoom (n)
  "With positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
		      (+ (face-attribute 'default :height)
			 (* (if (> n 0) 1 -1) 10))))

(defun djcb-reset-zoom nil
  "Reset the faze size."
  (interactive)
  (set-face-attribute 'default nil :height 100))

(defun jmm-iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;-----------------------------------------------------------------------


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


;;;; KEYBINDINGS
;;-----------------------------------------------------------------------
;; reset indentation behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
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
;;-----------------------------------------------------------------------

;; OTHER STUFF
;;----------------------------------------------------------------------

;; elpa and melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Allows one to switch between windows with SHIFT+{LEFT,RIGHT,UP,DOWN}
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Sets font size to size 10 font for programming, size 13 for Text
(set-face-attribute 'default nil :height 100) ;; Normal

;; This makes the mark highlighted when you mark stuff
(setq-default transient-mark-mode t)

;; Press M-q to make everything automatically conform to an
;; 80-character limit. It takes care of comments too!
(setq fill-column 80)

;; Disable menu bar
(tool-bar-mode -1)

;; Sends backup files to ~/.emacs_backups or /tmp
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; prevent the generation of lock files
(setq create-lockfiles nil)

;; highlight the current line;
; (global-hl-line-mode t) ; turn it on for all modes by default

;; Gets rid of the startup message
(setq inhibit-startup-message t)

;; Turn off the annoying system bell
(setq visible-bell t)

;; Adds to the auto-mode-list.
;;----------------------------------------------------------------------
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.htm$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ci$" . c++-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.m" . maplev-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.m" . mathematica-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.nb" . mathematica-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mw" . maplev-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mpl" . maplev-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hs" . haskell-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rpar$" . python-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.emacs$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.xinitrc$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.xprofile$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.profile$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bash_aliases" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
;;----------------------------------------------------------------------


;; Adds various files/folders to path
(setq load-path (append (list "/usr/share/emacs/site-lisp/elib")
			load-path))
(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/haskell-haskell-mode-6d7a3c4")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Custom .el files to load at startup
;;---------------------------------------------------------------------
(require-maybe 'php-mode) ;;(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(require-maybe 'linum)
(require-maybe 'maplev)
(require-maybe 'cython-mode)

;; Setup shell-current-directory
(require-maybe 'shell-current-directory)

;; Setup compile
(when (require-maybe 'compile)
  (setq compilation-disable-input nil)
  (setq compilation-scroll-output t)
  (setq mode-compile-always-save-buffer-p t)
  (global-set-key [f9] 'compile))

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; Three types of indentation mode
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Sets line-numbers to be default when a file is found
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; Ads auctex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
; ensures useful error messages
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;; Ad slime the emacs IDE for LISP
;; using quicklisp
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "sbcl")
; (slime-setup '(slime-fancy))

;; Latex mode customization
(add-hook 'LaTeX-mode-hook (lambda ()
			     (flyspell-mode 1)
			     (flyspell-buffer)
;			     (global-ede-mode 0)
			     '(LaTeX-math-mode)
;			     '(turn-on-reftex)
;			     (setq reftex-plug-into-AUCTeX t)
			     '(LaTeX-command "latex-syntex=1"))) ;; use syntex

; ESS
(require-maybe 'ess-site)
; (load (expand-file-name "~/ESS/lisp/ess-site"))
; Julia ESS
(setq inferior-julia-program-name "/usr/bin/julia-release-basic")
(put 'upcase-region 'disabled nil)

; Sage
(if (file-exists-p "/usr/lib/sagemath/local/share/emacs")
    (add-to-list 'load-path "/usr/lib/sagemath/local/share/emacs"))
(when (require-maybe 'sage "sage")
  (setq sage-command "/usr/lib/sagemath/sage")
  (require 'sage-view "sage-view")
  (add-hook 'sage-startup-after-primpt-hook 'sage-view))


; Mathematica
(load-file-maybe "~/elisp/mathematica.el")

; Racket
(when (require-maybe 'geiser)
  (require-maybe 'quack))
