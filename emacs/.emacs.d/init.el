(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(load-theme 'whiteboard)
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
(package-install-selected-packages)
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

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

(use-package eros
  :config
  (eros-mode 1))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package key-chord
  :init
  (setq key-chord-two-keys-default 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  :config
  (key-chord-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 0))


(use-package fuzzy)
(use-package auto-complete
  :after fuzzy
  :config
  (ac-flyspell-workaround)
  (setq ac-use-fuzzy 1)
  :init
  (ac-config-default)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (global-auto-complete-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package lsp-mode)
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-delay 0.1)
  (setq lsp-ui-doc-show-with-cursor nil))
