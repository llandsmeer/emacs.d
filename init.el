(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(evil evil-surround evil-numbers evil-matchit evil-exchange
                        avy rainbow-delimiters evil-nerd-commenter company web-mode
                        python undo-tree rust-mode racer magit evil-magit
                        mustang-theme monokai-theme flatui-theme solarized-theme))

(dolist (package packages)
  (when (not (package-installed-p package))
    (package-install package)))

(add-to-list 'custom-theme-load-path "~/.emacs/themes")

(require 'solarized)
(load-theme 'mustang t)

;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(which-function-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'python)
(add-hook 'python-mode 'run-python)

(require 'undo-tree)
(setq undo-tree-auto-save-history 1)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(require 'racer)
(add-hook 'rust-mode-hook #'racer-mode)

(require 'company)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 1)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'prog-mode-hook #'eldoc-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(require 'evil)
(evil-mode 1)
(define-key evil-motion-state-map ":" 'execute-extended-command)
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map " " 'save-buffer)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-exchange)
(evil-exchange-install)

(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-l") 'avy-goto-line)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'evil-nerd-commenter)
(define-key evil-motion-state-map (kbd "C-c c") 'evilnc-comment-operator)

(require 'evil-magit)
(global-set-key (kbd "C-SPC") 'magit-commit)
