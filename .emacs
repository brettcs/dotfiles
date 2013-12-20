(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-library 'lisp-indent-function 1)

(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-init-file () (interactive)
  (load-file "~/.emacs"))

(defun switch-to-buffer-last () (interactive)
  (switch-to-buffer nil))

(setq-default indent-tabs-mode nil)
(setq-default vertical-scroll-bar nil)
(setq inhibit-startup-message t)
(setq auto-save-default nil)
(setq blink-matching-paren nil)
(setq diff-switches "-u")
(setq fill-column 75)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq mouse-yank-at-point nil)
(setq next-line-add-newlines nil)
(setq visible-bell t)
(setq x-select-enable-clipboard t)  ; Unbreak my software, please.

(setq auto-mode-alist
      (nconc '(("/db\\." . zone-mode)
               ("/cvs\\.*" . text-mode)
               ("/mutt-" . text-mode)
               ("\\.phi\\'" . php-mode)) auto-mode-alist))

(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-xl" 'switch-to-buffer-last)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-x/" 'comment-or-uncomment-region)
(global-set-key "\C-z" 'undo)

(server-start)
(blink-cursor-mode 0)
(column-number-mode 1)
(menu-bar-mode 0)

(when (window-system)
      (setq-default initial-frame-alist
                    '((vertical-scroll-bars . nil)
                      (width . 81)
                      (height . 50)))
      (tool-bar-mode 0)
      ; git://git.naquadah.org/naquadah-theme.git
      (with-library 'naquadah-theme)
      ; git://github.com/TeMPOraL/nyan-mode.git
      (with-library 'nyan-mode
        (setq nyan-bar-length 10)
        (setq nyan-wavy-trail t)
        (nyan-mode)
        (nyan-stop-animation)))

(with-library 'font-lock
  (show-paren-mode t)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))

(with-library 'magit
  (global-set-key "\C-xg" 'magit-status)
  (setq magit-repo-dirs (file-expand-wildcards "~/*repos"))
  (setq magit-status-buffer-switch-function 'switch-to-buffer))

(with-library 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\(wn\\)?\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(with-library 'nxml-mode
  (setq-default nxml-outline-child-indent 4)
  (setq-default nxml-child-indent 4))

(with-library 'perl-mode
  (defalias 'cperl-mode 'perl-mode))

(with-library 'python
  (define-key python-mode-map (kbd "RET") 'newline-and-indent))

(put 'narrow-to-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'upcase-region 'disabled nil)

(with-library 'local)
