(server-start)
(blink-cursor-mode 0)
(column-number-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)

(setq-default vertical-scroll-bar nil)
(setq inhibit-startup-message t)
(setq auto-mode-alist (cons '("cvs.*" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("mutt-.*" . text-mode) auto-mode-alist))
(setq auto-save-default nil)
(setq blink-matching-paren nil)
(setq diff-switches "-u")
(setq fill-column 75)
(setq indent-tabs-mode nil)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq mouse-yank-at-point nil)
(setq next-line-add-newlines nil)
(setq visible-bell t)
(setq x-select-enable-clipboard t)  ; Unbreak my software, please.

(add-to-list 'auto-mode-alist '("\\.md\\(wn\\)?" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.phi" . php-mode))

(add-hook 'markdown-mode-hook 'visual-line-mode)

(global-set-key "\C-xl" 'open-last-buffer)
(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-x/" 'comment-or-uncomment-region)

(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(load-library "naquadah-theme")

(when (window-system)
      (setq-default initial-frame-alist
		    '((vertical-scroll-bars . nil)
		      (width . 81)
		      (height . 50))) ; )
       (load-library "nyan-mode")
       (setq nyan-bar-length 10)
       (setq nyan-wavy-trail t)
       (nyan-mode)
       (nyan-start-animation))

; Macros
(fset 'open-last-buffer
   [?\C-x ?b ?\C-m])
(fset 'load-init-file
   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e ?\C-m ?\C-a ?\C-k ?~ ?/ ?. ?e ?m ?a ?c ?s ?\C-m])

(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(require 'python)
(define-key python-mode-map (kbd "RET") 'newline-and-indent)

(require 'font-lock)
(show-paren-mode t)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

(require 'nxml-mode)
(setq-default nxml-outline-child-indent 4)
(setq-default nxml-child-indent 4)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
