(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Iosevka Term 11" nil t)
(setf dired-kill-when-opening-new-dired-buffer t)
(put 'dired-find-alternate-file 'disabled nil)

(global-font-lock-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t
  version-control t
  delete-old-versions t
  kept-new-versions 20
  kept-old-versions 5)

(setq inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/custom.el")

(setq package-selected-packages '(use-package))
(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package almost-mono-themes
  :config
  ;;(load-theme 'almost-mono-black t)
  ;;(load-theme 'almost-mono-gray t)
  ;;(load-theme 'almost-mono-cream t)
  (load-theme 'almost-mono-white t))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-define-key 'normal dired-mode-map "b" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "t" 'dired-create-empty-file)
  (evil-define-key 'normal dired-mode-map "d" 'dired-do-delete)
  (evil-define-key 'normal dired-mode-map (kbd "<RET>") 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "R" 'dired-do-rename)
  (evil-define-key 'normal dired-mode-map "r" 'dired-do-redisplay)
  (evil-define-key 'normal dired-mode-map "q" 'kill-current-buffer)
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init t))

(use-package key-chord
  :init
  (setq key-chord-two-keys-default 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  :config
  (key-chord-mode 1))

(use-package eros
  :config
  (eros-mode 1))

(use-package magit)
