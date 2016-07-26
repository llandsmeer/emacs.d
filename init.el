(require 'cl)
(require 'package)

(require 'server)
(if (not (server-running-p))
    (server-start))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (ignore-errors (package-refresh-contents)))

(defvar packages '(evil evil-surround evil-numbers evil-matchit evil-exchange
                        avy rainbow-delimiters evil-nerd-commenter company web-mode
                        python undo-tree rust-mode racer magit evil-magit
                        auctex haskell-mode elscreen multi-term tuareg
                        mustang-theme monokai-theme flatui-theme solarized-theme
                        pydoc
                        xelb exwm use-package))

(dolist (package packages)
  (when (not (package-installed-p package))
    (ignore-errors (package-install package))))

(add-to-list 'custom-theme-load-path "~/.emacs/themes")

(require 'solarized)
(load-theme 'monokai t)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(if (member "Terminus" (font-family-list)) (ignore-errors
    (set-face-font 'default "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")))

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
(setq electric-pair-preserve-balance nil)
(which-function-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq use-dialog-box nil)
(setq whitespace-style '(face tabs trailing tab tab-mark))

(require 'python)
(setq python-shell-interpreter "ipython3")
(setq python-shell-interpreter-args "-i")
(add-hook 'python-mode-hook (lambda ()
        (run-python "ipython3 -i")))
(add-hook 'python-mode-hook (lambda ()
        (define-key python-mode-map (kbd "C-c C-/") 'pydoc-at-point)))

(require 'undo-tree)
(setq undo-tree-auto-save-history 1)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(require 'racer)
(add-hook 'rust-mode-hook #'racer-mode)

(require 'company)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'prog-mode-hook #'eldoc-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(require 'evil)
(evil-mode 1)
;; (evil-set-initial-state 'term-mode 'emacs)
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
(global-set-key (kbd "M-SPC") 'magit-push-current)

;; auctex
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))

;; haskell-mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)

;; elscreen
(elscreen-start)
(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create)
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill)
(define-key evil-normal-state-map (kbd "gT") 'elscreen-previous)
(define-key evil-normal-state-map (kbd "gt") 'elscreen-next)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-display-screen-number nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display nil)
(let ((update-visibility (lambda () (setq elscreen-display-tab
                            (if (elscreen-one-screen-p) nil t)))))
      (add-hook 'elscreen-create-hook update-visibility)
      (add-hook 'elscreen-kill-hook update-visibility)
      (funcall update-visibility))

(require 'bind-key)

(when nil
  (add-to-list 'load-path "~/.emacs.d/elpa/cl-generic-0.3")
  (add-to-list 'load-path "~/.emacs.d/elpa/xelb-0.8")
  (add-to-list 'load-path "~/.emacs.d/elpa/exwm-0.5")
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (add-hook 'exwm-update-class-hook (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-init-hook #'exwm-config--fix/ido-buffer-window-other-frame)
  (exwm-enable)
  (exwm-randr-enable)
  (exwm-systemtray-enable)
  (start-process "setxkbmap" nil "setxkbmap"
                 "-option" "ctrl:nocaps"
                 "-option" "compose:ralt"
                 "-option" "altwin:swap_lalt_lwin")
  (start-process "dropbox" nil "dropbox" "start")
  (start-process "volumeicon" nil "volumeicon")
  (start-process "nm-applet" nil "nm-applet")
  (start-process "octopi-notifier" nil "octopi-notifier")
  (start-process "i3-focus-overlay" nil "i3-focus-overlay")
  (start-process "blueberry-tray" nil "blueberry-tray")
  (start-process "xcape" nil "xcape")
  (exwm-input-set-key (kbd "s-r") (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  (exwm-input-set-key (kbd "s-h") 'evil-window-left)
  (exwm-input-set-key (kbd "s-l") 'evil-window-right)
  (exwm-input-set-key (kbd "s-k") 'evil-window-up)
  (exwm-input-set-key (kbd "s-j") 'evil-window-down)
  (exwm-input-set-key (kbd "C-s-h") 'evil-window-move-far-left)
  (exwm-input-set-key (kbd "C-s-l") 'evil-window-move-far-right)
  (exwm-input-set-key (kbd "C-s-k") 'evil-window-move-very-top)
  (exwm-input-set-key (kbd "C-s-j") 'evil-window-move-very-bottom)
  (exwm-input-set-key (kbd "s-o") (lambda (cmd) (interactive (start-process-shell-command "luakit" nil "luakit"))))
  (exwm-input-set-key (kbd "s-p") (lambda (cmd) (interactive (start-process-shell-command "thunar" nil "thunar"))))
  (exwm-input-set-key (kbd "s-v") 'evil-window-vsplit)
  (exwm-input-set-key (kbd "s-f") (lambda (cmd) (interactive (exwm-layout-set-fullscreen (not exwm--fullscreen)))))
  (exwm-input-set-key (kbd "s-SPC") (lambda (cmd) (interactive (start-process-shell-command "lxterminal" nil "lxterminal"))))
  (exwm-input-set-key (kbd "s-c") 'evil-window-delete)
  (exwm-input-set-key (kbd "s-s") 'evil-window-split))
