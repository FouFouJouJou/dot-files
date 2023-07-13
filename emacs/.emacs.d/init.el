(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Iosevka Term" nil t)
(load-theme 'whiteboard)

(setq inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/custom.el")

(setq package-selected-packages '(use-package))
(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package which-key
  :config
  (which-key-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init t))

(use-package evil-escape
  :custom
  (evil-escape-key-sequence "jk"
                evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))


(use-package company
  :init
  (add-hook 'global-mode-hook #'company-mode)
  :config
  (global-company-mode 1))

(use-package eros);
(use-package lsp-mode)
