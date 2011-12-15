(push "/opt/local/bin" exec-path)

;add rope and ropemacs
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-goodies-el/")

(require 'twik-bindings)

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))  ;; fixes Shift-Up text selection

;; fringes to show the beginning and end of a buffer and empty lines
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(require 'flymake)
(setq python-check-command "pyflakes")

(mouse-avoidance-mode 'animate)

(require 'mwheel)
(mouse-wheel-mode 1)

(setq-default cursor-type 'bar)
(blink-cursor-mode 1)
(delete-selection-mode 1)
(menu-bar-mode -1) ;; hide top menu bar

(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(setq redisplay-dont-pause t  ;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
)

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))) ;; one line at a time

(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(auto-compression-mode t) ; Transparently open compressed files

(setq search-highlight t) ; Highlight search object
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/site-lisp/color-theme/color-theme-railscasts.el")
(color-theme-railscasts)

; Set font
(set-default-font "-apple-Consolas-normal-normal-normal-*-17-200-72-72-m-0-iso10646-1")

(when (display-graphic-p)
    (setq-default line-spacing 0.15)) ; add some height between lines

(if auto-save-default
    (auto-save-mode -1))

(require 'bar-cursor)
(bar-cursor-mode 1)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(setq
  inhibit-startup-message t
  require-final-newline t
  ring-bell-function 'ignore
)


;; Use F10 to invoke ibuffer
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(global-set-key [(f10)] 'ibuffer)


(when (eq system-type 'darwin)
    (setq ns-alternate-modifier 'meta)
    (setq mac-command-modifier 'none)
    (require 'mac-key-mode)
    (mac-key-mode 1)
)

(setq x-select-enable-clipboard t)
(setq-default truncate-lines t)  ;; disable line wrap
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(global-auto-revert-mode 1)
(column-number-mode t)
(show-paren-mode t)


(require 'mouse)
(xterm-mouse-mode  t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; ----------------

;; Full screen
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil
'fullscreen)
                                           nil
                                           'fullboth)))

;;-------- bottom modeline
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))
;;--------

;; line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)

(setq linum-format "%3d ")  ;; put a space after line number
(add-hook 'python-mode-hook
  (lambda() (linum-mode 1)))

;; load these only if using window-system emacs
(when (display-graphic-p)
  ;; disables scrollbar
  (scroll-bar-mode -1)
  (display-battery-mode 1))

;; Reload .emacs [Alt]-[r]
(global-set-key "\M-r"
  '(lambda ()
     (interactive)
     (load-file "~/.emacs.d/init.el")))

(require 'redo+)

(setq vc-follow-symlinks 1) ;; don't ask to follow a configuration simlink

(require 'tabbar)
(tabbar-mode)

(setq tabbar-buffer-groups-function  ;; all tabs is just one group
  (lambda ()
    (list "All")))

;; enable iswitchb mode 'C-x b' and allow using arrow keys
(iswitchb-mode 1)
(defun iswitchb-local-keys ()
    (mapc (lambda (K)
            (let* ((key (car K)) (fun (cdr K)))
              (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
          '(("<right>" . iswitchb-next-match)
            ("<left>"  . iswitchb-prev-match)
            ("<up>"    . ignore             )
            ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(global-set-key [f2] 'visit-ansi-term)
(require 'term)
(defun visit-ansi-term ()
  "Rename, restart if killed, or create and switch to an ansi-term buffer"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(require 'ido)
(setq
  ido-enable-flex-matching t
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ;; ignore these guys
    '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
      "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t ; wait for RET, even with unique completion
)
(ido-mode t)
(ido-everywhere t)

(require 'uniquify) ;; overrides Emacs’ default mechanism for making buffer names unique
(setq uniquify-buffer-name-style 'forward)

;; cd ~/.emacs.d/vendor
;; curl http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2 | tar jx
;; cd auto-complete-1.3.1
;; make byte-compile
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete-1.3.1/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete-1.3.1/dict")
(ac-config-default)

(setq python-check-command "pyflakes")

;; Zen coding expand C-j
(add-to-list 'load-path "~/.emacs.d/vendor/zencoding/")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes


(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
;; optional key binding


;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
	(insert current-line)
	(decf n)))))


;; Paste at point NOT at cursor
(setq mouse-yank-at-point 't)

;; Make control+pageup/down scroll the other buffer
(global-set-key [C-next] 'scroll-other-window)
(global-set-key [C-prior] 'scroll-other-window-down)


;; Resize buffers with the hotkeys
(global-set-key (kbd "<f5>") 'shrink-window-horizontally)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f7>") 'shrink-window)
(global-set-key (kbd "<f8>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *true-mac-cut-buffer* "")
(setq *true-mac-cut-buffer2* t)

(setq interprogram-cut-function
            '(lambda (str push)
                        (setq *true-mac-cut-buffer* str)
                                 (setq *true-mac-cut-buffer2* push)))

(setq interprogram-paste-function
            '(lambda () nil))

(defun true-mac-cut-function () (interactive)
    (if mark-active
              (progn
                        (true-mac-copy-function)
                                (kill-region (point) (mark)))
          (beep)))

(defun true-mac-copy-function () (interactive)
    (if mark-active
              (mac-cut-function
                      *true-mac-cut-buffer*
                             *true-mac-cut-buffer2*)
          (beep)))

(defun true-mac-paste-function () (interactive)
    (if mark-active
              (kill-region (point) (mark)))
      (insert (mac-paste-function)))

;; ---
(add-hook 'html-mode-hook
  (lambda ()
    ;; Default indentation is usually 2 spaces, changing to 4.
    (set (make-local-variable 'sgml-basic-offset) 4)))
