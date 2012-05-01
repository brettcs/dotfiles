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
(setq fill-column 75)
(setq indent-tabs-mode nil)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq mouse-yank-at-point nil)
(setq next-line-add-newlines nil)
(setq visible-bell t)
(setq x-select-enable-clipboard t)  ; Unbreak my software, please.

(setq auto-mode-alist (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook 'visual-line-mode)

(global-set-key "\C-xl" 'open-last-buffer)
(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-x\C-j" 'goto-line)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-x/" 'comment-or-uncomment-region)

(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(load-library "naquadah-theme")

(cond (window-system (setq-default initial-frame-alist
				   '((vertical-scroll-bars . nil)
				     (width . 81)
				     (height . 50)))))

; Macros
(fset 'open-last-buffer
   [?\C-x ?b ?\C-m])
(fset 'load-init-file
   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e ?\C-m ?\C-a ?\C-k ?~ ?/ ?. ?e ?m ?a ?c ?s ?\C-m])

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
