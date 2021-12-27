(let ((confdirs (or (getenv "XDG_CONFIG_DIRS") (getenv "XDG_CONFIG_HOME") "~/.config"))
      confdir)
  (dolist (confdir (nreverse (split-string confdirs ":")))
    (let ((my-emacs-dir (concat (file-name-as-directory confdir) (file-name-as-directory "emacs"))))
      (when (file-exists-p (concat my-emacs-dir "init.el"))
        (setq user-emacs-directory my-emacs-dir)
))))

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
  (load-file (concat user-emacs-directory "init.el")))

(defun switch-to-buffer-last () (interactive)
  (switch-to-buffer nil))

(defun visual-text-mode () (interactive)
  (text-mode) (visual-line-mode))

(setq-default fill-column 76)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq blink-matching-paren nil)
(setq diff-switches "-u")
(setq enable-local-eval nil)
(setq enable-local-variables :safe)
(setq inhibit-startup-message t)
(setq kill-whole-line t)
(setq mouse-yank-at-point nil)
(setq my-project-directories '("~/Repositories/"))
(setq next-line-add-newlines nil)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq visible-bell t)
(setq x-select-enable-clipboard t)

(let ((cachehome (file-name-as-directory (or (getenv "XDG_CACHE_HOME") "~/.cache"))))
  (let ((cachedir (concat cachehome (file-name-as-directory "emacs/backups/"))))
    (setq auto-save-file-name-transforms `((".*" ,cachedir t)))
    (setq backup-directory-alist `(("." . ,cachedir))))
  (let ((can-cache (file-exists-p cachehome)))
    (setq auto-save-default can-cache)
    (setq make-backup-files can-cache))
)

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
               ("\\.txt$" . text-mode)
               ) auto-mode-alist))
(add-hook 'text-mode-hook 'visual-line-mode)

(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-xl" 'switch-to-buffer-last)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-x/" 'comment-or-uncomment-region)
(global-set-key "\C-z" 'undo)

(blink-cursor-mode 0)
(column-number-mode 1)
(menu-bar-mode 0)

(with-library 'local)

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
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (setq magit-repo-dirs my-project-directories)
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
  (add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
  (add-to-list 'interpreter-mode-alist '("@PERL@" . perl-mode))
  (defalias 'cperl-mode 'perl-mode)
  (define-key perl-mode-map (kbd "C-c C-d") 'cperl-perldoc)
)

(with-library 'projectile
  (setq projectile-project-search-path my-project-directories)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
)

(with-library 'python
  (setq python-shell-interpreter "python3")
)

; https://github.com/joostkremers/visual-fill-column
(with-library 'visual-fill-column
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  (setq-default visual-fill-column-width 100))

(with-library 'ws-butler
  (add-hook 'prog-mode-hook #'ws-butler-mode)
)

(put 'narrow-to-region 'disabled nil)
(put 'overwrite-mode 'disabled t)
(put 'upcase-region 'disabled nil)
