(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "Iosevka Term 11" nil t)
;; (set-frame-font "Iosvmata 11" nil t)
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
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "t" 'dired-create-empty-file)
  (evil-define-key 'normal dired-mode-map "d" 'dired-do-delete)
  (evil-define-key 'normal dired-mode-map (kbd "<RET>") 'dired-view-file)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "R" 'dired-do-rename)
  (evil-define-key 'normal dired-mode-map "r" 'dired-do-redisplay)
  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
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

;;(use-package emacs
;;  :custom
;;  (tab-always-indent 'complete)
;;  (text-mode-ispell-word-completion nil)
;;  (read-extended-command-predicate #'command-completion-default-include-p))

;;(use-package marginalia
;;  :config
;;  (marginalia-mode 0))
;;
;;
;;(use-package fuzzy)
;;(use-package auto-complete
;;  :after fuzzy
;;  :config
;;  (ac-flyspell-workaround)
;;  (setq ac-use-fuzzy 1)
;;  :init
;;  (ac-config-default)
;;  (add-to-list 'ac-sources 'ac-source-yasnippet)
;;  (global-auto-complete-mode t))
;;
;;(use-package orderless
;;  :custom
;;  (completion-styles '(orderless basic))
;;  (completion-category-defaults nil)
;;  (completion-category-overrides '((file (styles partial-completion)))))
