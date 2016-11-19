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
                        pydoc markdown-mode graphviz-dot-mode
                        xelb exwm use-package))

(dolist (package packages)
  (when (not (package-installed-p package))
    (ignore-errors (package-install package))))

(add-to-list 'custom-theme-load-path "~/.emacs/themes")

(require 'solarized)
(load-theme 'solarized-dark t)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(if (member "Terminus" (font-family-list)) (ignore-errors
    ;; (set-face-font 'default "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
))

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
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup" t)))
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq use-dialog-box nil)
(setq whitespace-style '(face tabs trailing tab tab-mark))

(require 'python)
(setq-default python-shell-interpreter "python3")
(setq-default python-shell-interpreter-args "-i")
(add-hook 'python-mode-hook (lambda ()
        (run-python "python3 -i")))
(add-hook 'python-mode-hook (lambda ()
        (define-key python-mode-map (kbd "C-c C-/") 'pydoc-at-point)))

(require 'undo-tree)
(setq undo-tree-auto-save-history 1)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(require 'racer)
(setq racer-rust-src-path "~/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
(setq racer-cmd "/usr/local/bin/racer")
(add-hook 'rust-mode-hook #'racer-mode)

(require 'company)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
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
(define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

; (define-key markdown-mode-map (kbd "C-c C-c C-o")
  ; (lambda ()
    ; (progn
    ; (start-process "pandoc" nil "pacdoc" "-t" "latex" "-s" "-o" "/tmp/pandoc.latex")
    ; (start-process "pdflatex" nil "pdflatex" "-halt-on-error" "/tmp/pandoc.latex")
    ; (call-process "evince" nil 0 nil "/tmp/pandoc.pdf")
    ; )))

(when nil
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
      (funcall update-visibility)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "c567c85efdb584afa78a1e45a6ca475f5b55f642dfcd6277050043a568d1ac6f" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (nasm-mode toml-mode elixir-mode web-mode use-package tuareg solarized-theme rainbow-identifiers rainbow-delimiters racer pydoc mustang-theme multi-term monokai-theme molokai-theme moe-theme markdown-mode haskell-mode graphviz-dot-mode flatui-theme exwm evil-surround evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-exchange elscreen distinguished-theme company avy autopair auctex)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
