(push "/opt/local/bin" exec-path)

;add rope and ropemacs
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-goodies-el/")

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))  ;; fixes Shift-Up text selection

(require 'mwheel)
(mouse-wheel-mode 1)

(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(menu-bar-mode -1) ;; hide top menu bar

(setq redisplay-dont-pause t  ;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  ;scroll-preserve-screen-position 1
)

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(setq backup-directory-alist `(("." . "/tmp")))
(setq search-highlight t) ; Highlight search object
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/site-lisp/color-theme/color-theme-railscasts.el")
(color-theme-railscasts)

; Set font
(set-default-font "-apple-Consolas-normal-normal-normal-*-16-200-72-72-m-0-iso10646-1")

(if auto-save-default
    (auto-save-mode -1))

;; under OS X these lines will sync the kill ring with the clipboard
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)
(global-set-key (kbd "M-n") 'toggle-fullscreen)

(column-number-mode t)

(require 'bar-cursor)
(bar-cursor-mode 1)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(require 'highlight-current-line)
(highlight-current-line-on t)
;; To customize the background color of the line
(set-face-background 'highlight-current-line-face "#333333")

(setq
  inhibit-startup-message t
  require-final-newline t
  ring-bell-function 'ignore
  mac-pass-command-to-system nil
  mac-option-key-is-meta nil
  mac-command-key-is-meta t
  mac-command-modifier 'meta
  mac-option-modifier nil
)

(setq-default truncate-lines t)  ;; disable line wrap
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(column-number-mode t)

(require 'mouse)
(xterm-mouse-mode 1)
(mwheel-install)

(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-set-key "\C-l" 'goto-line) ; [Ctrl]-[L]

;; --------------
(defvar LIMIT 1)
(defvar time 0)
(defvar mylist nil)

(defun time-now ()
   (car (cdr (current-time))))

(defun bubble-buffer ()
   (interactive)
   (if (or (> (- (time-now) time) LIMIT) (null mylist))
       (progn (setq mylist (copy-alist (buffer-list)))
               (delq (get-buffer " *Minibuf-0*") mylist)
                (delq (get-buffer " *Minibuf-1*") mylist)))
   (bury-buffer (car mylist))
   (setq mylist (cdr mylist))
   (setq newtop (car mylist))
   (switch-to-buffer (car mylist))
   (setq rest (cdr (copy-alist mylist)))
   (while rest
     (bury-buffer (car rest))
     (setq rest (cdr rest)))
   (setq time (time-now)))

(global-set-key [f4] 'bubble-buffer)
;; ----------------

;; Full screen
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil
'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(M-return)] 'toggle-fullscreen)

;;-------- Toggle modeline,  Command-F12 on Mac
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

(global-set-key [M-f12] 'toggle-mode-line)
;;--------

;; C-F5 toggle line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "<f5>") 'linum-mode)
(setq linum-format "%3d ")  ;; put a space after line number
(add-hook 'python-mode-hook
  (lambda() (linum-mode 1)))


;; load these only if using window-system emacs
(when (display-graphic-p)
  ;; disables scrollbar
  (scroll-bar-mode -1)
  (display-battery-mode 1))

;(setq mac-option-key-is-meta nil)
;(setq mac-command-key-is-meta t)
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier nil)

;(global-set-key [?\A-a] 'mark-whole-buffer)
;(global-set-key [?\A-s] 'save-buffer)
;(global-set-key [?\A-S] 'write-file)
;(global-set-key [?\A-p] 'ps-print-buffer)
;(global-set-key [?\A-o] 'find-file)
;(global-set-key [?\A-q] 'save-buffers-kill-emacs)
;(global-set-key [?\A-w] 'kill-buffer-and-window)
;(global-set-key [?\A-z] 'undo)
;(global-set-key [?\A-f] 'isearch-forward)
;(global-set-key [?\A-g] 'query-replace)
;(global-set-key [?\A-l] 'goto-line)
;(global-set-key [?\A-m] 'iconify-frame)
;(global-set-key [?\A-n] 'new-frame)

;(require 'redo)
(require 'mac-key-mode)
(mac-key-mode 1)
