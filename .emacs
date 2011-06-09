;=============== beginning .emacs =================

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/")

(require 'tabbar)
(tabbar-mode 1)

(require 'bar-cursor)
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(bar-cursor-mode t)
(set-cursor-color "white")

; C-F5 toggle line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)
(add-hook 'python-mode-hook
  (lambda() (linum-mode 1)))

;; move between windows with alt-arrows (both X and console modes)
(if window-system
    (windmove-default-keybindings 'meta)
  (progn
    (global-set-key [(alt left)]  'windmove-left)
    (global-set-key [(alt up)]    'windmove-up)
    (global-set-key [(alt right)] 'windmove-right)
    (global-set-key [(alt down)]  'windmove-down)))

;; load these only if using window-system emacs
(when (display-graphic-p)
  ;; disables scrollbar
  (scroll-bar-mode -1)
  (display-battery-mode 1))

;; Disable menu bar except for GUI emacs
(if (not window-system)
  (add-hook 'term-setup-hook #'(lambda () (menu-bar-mode -1))))

(delete-selection-mode 1)

;(require 'paren)
;(show-paren-mode 1) ; until I make it less ubtrusive
;(setq show-paren-delay 0)
;(set-face-background 'show-paren-match-face (face-background 'default))
;(set-face-foreground 'show-paren-match-face "#ccc")
;(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;(global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
;(global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)

(setq inhibit-startup-message t)
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/site-lisp/color-theme/color-theme-railscasts.el")
(color-theme-railscasts)

; Set font
(set-default-font "-apple-Consolas-normal-normal-normal-*-16-200-72-72-m-0-iso10646-1")

;; Make command key on mac be Meta
;(setq ns-command-modifier 'meta)
;(setq mac-command-key-is-meta t)

;; Under OS X these lines will sync the kill ring with the clipboard
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

;(require 'yasnippet)

(defun
  kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;(require 'highlight-current-line)
;(highlight-current-line-on t)
;;; To customize the background color of the line
;(set-face-background 'highlight-current-line-face "#333")

(setq
  inhibit-startup-message t
  require-final-newline t
  ring-bell-function 'ignore
  mac-pass-command-to-system nil
  mac-command-key-is-meta t
  mac-command-modifier 'meta
)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(column-number-mode 1)
(line-number-mode t)

(require 'mouse)
(xterm-mouse-mode 1)
(mwheel-install)

;;full screen
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil
'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;-------- Command F12 on Mac
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key [M-f12] 'toggle-mode-line)
;--------
