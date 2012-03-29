(cond 
 (window-system 
  (setq-default initial-frame-alist '(
                                      (vertical-scroll-bars . nil)
                                      (background-color . "Black")
                                      (foreground-color . "white")
                                      (cursor-color . "green")
                                      (width . 81)
                                      (height . 45)
                                     )
  )
 )
)

(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(server-start)
(setq kill-whole-line t)
(setq visible-bell t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default mouse-yank-at-point nil)
(setq-default fill-column 75)
(setq-default indent-tabs-mode nil)
(setq-default next-line-add-newlines nil)
(setq auto-mode-alist (cons '("mutt-.*" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("cvs.*" . text-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("*.pro" . prolog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("*.pre" . emacs-lisp-mode) auto-mode-alist))
(setq blink-matching-paren nil)
(setq x-select-enable-clipboard t)  ; Unbreak my software, please.
; (global-set-key "\C-c\C-c" 'kill-buffer-and-window)
; (global-set-key "\C--" 'undo)
(global-set-key "\C-xl" 'open-last-buffer)
(global-set-key "\M-Q" 'fill-quoted-email)
(global-set-key "\C-c\C-s" 'ispell-region)
(global-set-key "\C-c\C-m" 'ispell-email)
(global-set-key "\C-x\C-j" 'goto-line)
; (global-set-key "\C-v" 'yank)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-n" 'save-buffer)  ; Helpful with Dvorak.
(global-set-key "\C-xc" 'comment-or-uncomment-region)
(global-set-key "\C-x\C-c" 'comment-or-uncomment-region)

(setq-default server-temp-file-regexp "mutt-")

(require 'cc-mode)
(setq-default tab-width 4)
(setq c-basic-offset 4)
(define-key c-mode-base-map "\C-m" 'newline-and-indent)
; (c-set-style bsd)

(require 'python)
(define-key python-mode-map (kbd "RET") 'newline-and-indent)

(require 'font-lock)
(show-paren-mode t)
(setq font-lock-maximum-decoration t)
; Can't get this to work... hmmm.
; (set-frame-font "Courier")
; (set-default-font "9x15")

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-font-lock-mode t)

(put 'upcase-region 'disabled nil)

; (read-abbrev-file "~/.abbrev")

; (setq-default abbrev-mode t)

; All right -- faces!
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(blink-cursor nil)
 '(mouse-wheel-mode t nil (mwheel))
 '(tab-width 5))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(custom-state-face ((((class color) (background light)) (:foreground "green"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Magenta"))))
 '(font-lock-comment-face ((t (:foreground "Khaki"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "Aquamarine"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "MediumSlateBlue"))))
 '(font-lock-string-face ((t (:foreground "Gold"))))
 '(font-lock-type-face ((t (:foreground "SteelBlue"))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "SlateBlue"))))
 '(highlight ((t (:foreground "black" :background "darkseagreen2"))))
 '(show-paren-match-face ((((class color)) (:foreground "Yellow"))))
 '(show-paren-mismatch-face ((((class color)) (:foreground "Red")))))

; Macros
(fset 'open-last-buffer
   [?\C-x ?b ?\C-m])
(fset 'load-init-file
   [?\M-x ?l ?o ?a ?d ?- ?f ?i ?l ?e ?\C-m ?\C-a ?\C-k ?~ ?/ ?. ?e ?m ?a ?c ?s ?\C-m])
(fset 'load-abbrev-file
   [?\M-x ?r ?e ?a ?d ?- ?a ?b ?b ?r ?e ?v ?- ?f ?i ?l ?e ?\C-m ?\C-a ?\C-k ?~ ?/ ?. ?a ?b ?b ?r ?e ?v ?\C-m])
(fset 'fill-quoted-email
   [?\C-o ?\C-n ?\C-n ?\C-o ?\C-p ?\M-q ?\C-n ?\C-d ?> ?\M-q escape ?} ?\C-k escape ?{ ?\C-k])
(fset 'ispell-email
   [?\M-< ?\C-u ?2 ?\C-[ ?} ?\C-n ?\C-s ?B ?r ?e ?t ?t ?  ?S ?m ?i ?t ?h ?\C-a ?\C-p ?\C-p])
(fset 'unhtmlify-email
   [?\M-< ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?s ?t ?r ?i ?n ?g ?\C-m ?& ?n ?b ?s ?p ?\; ?\C-m ?  ?\C-m ?\M-< ?\C-[ ?} ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p ?\C-m ?< ?/ ?? ?[ ?A ?- ?Z ?a ?- ?z ?] ?[ ?^ ?> ?] ?* ?> ?\C-m ?\C-m ?\M-< ?\C-[ ?}])

(defun conditional-eval (question yes-result no-result)
  (if (y-or-n-p (concat question " "))
	 `(l ,yes-result)
    `(l ,no-result))
  )

(defalias 'quit 'save-buffers-kill-emacs)

;; I hate cperl-mode.
(defalias 'cperl-mode 'perl-mode)

(put 'narrow-to-region 'disabled nil)

(setq-default nxml-outline-child-indent 4)
(setq-default nxml-child-indent 4)
