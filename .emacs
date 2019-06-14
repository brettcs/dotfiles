(package-initialize)  ;; Added by Package.el.

(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-library 'lisp-indent-function 1)

(when (file-directory-p "~/.emacs.d/elisp")
  (let ((default-directory "~/.emacs.d/elisp/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)
    )
  )

(defun fill-text-mode () (interactive)
  (text-mode) (auto-fill-mode))

(defun load-init-file () (interactive)
  (load-file "~/.emacs"))

(defun switch-to-buffer-last () (interactive)
  (switch-to-buffer nil))

(defun visual-text-mode () (interactive)
  (text-mode) (visual-line-mode))

(setq-default fill-column 76)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default vertical-scroll-bar nil)
(setq inhibit-startup-message t)
(setq initial-frame-alist '((vertical-scroll-bars . nil)))
(setq auto-save-default nil)
(setq blink-matching-paren nil)
(setq diff-switches "-u")
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq mouse-yank-at-point nil)
(setq next-line-add-newlines nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq visible-bell t)
(setq x-select-enable-clipboard t)

(setq auto-mode-alist
      (nconc '(("/db\\." . zone-mode)
               ("/cvs\\.*" . fill-text-mode)
               ("/mutt-" . visual-text-mode)
               ("\\.automount$" . conf-unix-mode)
               ("\\.device$" . conf-unix-mode)
               ("\\.mount$" . conf-unix-mode)
               ("\\.path$" . conf-unix-mode)
               ("\\.scope$" . conf-unix-mode)
               ("\\.service$" . conf-unix-mode)
               ("\\.slice$" . conf-unix-mode)
               ("\\.socket$" . conf-unix-mode)
               ("\\.swap$" . conf-unix-mode)
               ("\\.target$" . conf-unix-mode)
               ("\\.timer$" . conf-unix-mode)
               ) auto-mode-alist))

(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-xl" 'switch-to-buffer-last)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-x/" 'comment-or-uncomment-region)
(global-set-key "\C-z" 'undo)

(blink-cursor-mode 0)
(column-number-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)

(when (string= (getenv "XDG_SESSION_TYPE") "x11")
  (with-library 'naquadah-theme))

(with-library 'font-lock
  (show-paren-mode t)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))

; https://github.com/alpaker/Fill-Column-Indicator.git
(with-library 'fill-column-indicator
  (setq fci-rule-column 80)
  (setq fci-rule-color "DarkOrange4")
  (add-hook 'emacs-lisp-mode-hook 'fci-mode)
  (add-hook 'js-mode-hook 'fci-mode)
  (add-hook 'perl-mode-hook 'fci-mode)
  (add-hook 'php-mode-hook 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode)
  (add-hook 'ruby-mode-hook 'fci-mode)
  (add-hook 'sh-mode-hook 'fci-mode))

(with-library 'magit
  (global-set-key "\C-xg" 'magit-status)
  (add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)
  (setq magit-repo-dirs (file-expand-wildcards "~/*repos"))
  (setq magit-repository-directories magit-repo-dirs))

(with-library 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\(wn\\)?\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(with-library 'nxml-mode
  (setq-default nxml-outline-child-indent 4)
  (setq-default nxml-child-indent 4))

(with-library 'perl-mode
  (defalias 'cperl-mode 'perl-mode))

(with-library 'rst
  (add-hook 'rst-mode-hook 'visual-line-mode))

; https://github.com/joostkremers/visual-fill-column
(with-library 'visual-fill-column
  (global-visual-fill-column-mode)
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (setq-default visual-fill-column-width 100))

(with-library 'ws-trim
  (setq-default ws-trim-level 1)
  (global-ws-trim-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'upcase-region 'disabled nil)

(with-library 'local)
