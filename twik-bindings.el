;; under OS X these lines will sync the kill ring with the clipboard
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Fullscreen mode
(global-set-key (kbd "M-n") 'toggle-fullscreen)
(global-set-key [M-f11] 'toggle-fullscreen)
(global-set-key [A-f11] 'toggle-fullscreen)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key "\C-l" 'goto-line) ; [Ctrl]-[L]

(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key [f4] 'bubble-buffer)

(global-set-key [M-f12] 'toggle-mode-line)  ;; on/off Modeline Command-F12 on Mac

(global-set-key (kbd "C-c n") 'linum-mode)  ;; on/off line numbers

(global-set-key (kbd "s-<up>") 'tabbar-backward-group)
(global-set-key (kbd "s-<down>") 'tabbar-forward-group)
(global-set-key (kbd "s-<left>") 'tabbar-backward)
(global-set-key (kbd "s-<right>") 'tabbar-forward)

(global-set-key "\M-," 'tabbar-backward)
(global-set-key "\M-." 'tabbar-forward)

(global-set-key "\C-c\C-k" 'copy-line)

(global-set-key (kbd "C-S-d") 'duplicate-current-line)

(global-set-key [?\A-x] 'true-mac-cut-function)
(global-set-key [?\A-c] 'true-mac-copy-function)
(global-set-key [?\A-v] 'true-mac-paste-function)

;; Other commands to make you feel at home
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [?\A-a] 'mark-whole-buffer)
(global-set-key [?\A-s] 'save-buffer)
(global-set-key [?\A-S] 'write-file)
(global-set-key [?\A-p] 'ps-print-buffer)
(global-set-key [?\A-o] 'find-file)
(global-set-key [?\A-q] 'save-buffers-kill-emacs)
(global-set-key [?\A-w] 'kill-buffer-and-window)
(global-set-key [?\A-z] 'undo)
(global-set-key [?\A-f] 'isearch-forward)
(global-set-key [?\A-g] 'query-replace)
(global-set-key [?\A-l] 'goto-line)
(global-set-key [?\A-m] 'iconify-frame)
(global-set-key [?\A-n] 'new-frame)
(global-set-key [?\A-d] 'duplicate-current-line)

(setq x-select-enable-clipboard t)

(provide 'twik-bindings)
