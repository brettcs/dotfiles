(setq user-emacs-directory "~/.config/emacs/")

(package-initialize)  ;; Added by Package.el.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
(put 'with-library 'lisp-indent-function 1)

(let ((default-directory (concat user-emacs-directory "elisp/")))
  (when (file-directory-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)
))

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
(setq auto-save-default t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t)))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/"))))
(setq blink-matching-paren nil)
(setq diff-switches "-u")
(setq enable-local-eval nil)
(setq enable-local-variables :safe)
(setq inhibit-startup-message t)
(setq kill-whole-line t)
(setq make-backup-files t)
(setq mouse-yank-at-point nil)
(setq next-line-add-newlines nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq visible-bell t)
(setq x-select-enable-clipboard t)

(setq auto-mode-alist
      (nconc '(("/db\\." . zone-mode)
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

(with-library 'elpy
  (elpy-enable)
  (with-library 'flycheck
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
))

(with-library 'ffap
  (ffap-bindings)
  (setq ffap-lax-url nil)
  (defadvice ffap-string-at-point (before ffap-perl-module-fix activate)
    "allow ffap to locate perl modules"
    (when (and (ad-get-arg 0) (eq major-mode 'perl-mode))
      (ad-set-arg 0 major-mode)))
  (defun my-ffap-perldoc (file)
    (let ((real-file (shell-command-to-string (concat "perldoc -ml " file))))
      (unless (string-match "No module found for " real-file)
        (substring real-file 0 -1)
        )))
  (add-to-list 'ffap-alist  '(perl-mode . my-ffap-perldoc))
)

(with-library 'font-lock
  (show-paren-mode t)
  (setq font-lock-maximum-decoration t)
  (global-font-lock-mode t))

; https://github.com/alpaker/Fill-Column-Indicator.git
(with-library 'fill-column-indicator
  (setq fci-rule-column 80)
  (setq fci-rule-color "DarkOrange4")
  (add-hook 'prog-mode-hook 'fci-mode)
)

(with-library 'helm-pydoc
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc))

(with-library 'magit
  (global-set-key "\C-xg" 'magit-status)
  (add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)
  (setq magit-repo-dirs (file-expand-wildcards "~/Repositories"))
  (setq magit-repository-directories
        (mapcar #'(lambda (p) (cons p 1)) magit-repo-dirs))
)

(with-library 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\(wn\\)?\\'" . markdown-mode))
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(with-library 'nxml-mode
  (setq-default nxml-outline-child-indent 4)
  (setq-default nxml-child-indent 4))

(with-library 'outline
  (add-hook 'outline-minor-mode-hook
            (lambda () (local-set-key "\C-c\C-o" outline-mode-prefix-map)))
  (add-hook 'prog-mode-hook 'outline-minor-mode)
)

(with-library 'perl-mode
  (defalias 'cperl-mode 'perl-mode)
  (define-key perl-mode-map (kbd "C-c C-d") 'cperl-perldoc)
)

(with-library 'projectile
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/Repositories"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)

(with-library 'python
  (setq python-shell-interpreter "python3")
)

(with-library 'rst
  (add-hook 'rst-mode-hook 'visual-line-mode))

; https://github.com/joostkremers/visual-fill-column
(with-library 'visual-fill-column
  (global-visual-fill-column-mode)
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (setq-default visual-fill-column-width 100))

(with-library 'ws-butler
  (add-hook 'prog-mode-hook #'ws-butler-mode)
)

(put 'narrow-to-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'upcase-region 'disabled nil)

(with-library 'local)
