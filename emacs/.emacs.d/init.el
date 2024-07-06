(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-frame-font "SF Mono 10" nil t)
(load-theme 'whiteboard)
(setenv "JAVA_HOME"  "/home/fuji/.jdks/corretto-21.0.2")
(global-font-lock-mode 0) ;; disable colors
(setq lsp-java-java-path "/home/fuji/.jdks/corretto-21.0.2/bin/java")
(setq lsp-clients-angular-language-server-command
  '("node"
    "/usr/local/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/local/lib/node_modules"
    "--tsProbeLocations"
    "/usr/local/lib/node_modules"
    "--stdio"))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
)

(setq inhibit-startup-screen t)
(setq custom-file "~/.emacs.d/custom.el")

(setq package-selected-packages '(use-package))
(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			))
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

(use-package key-chord
  :init
  (setq key-chord-two-keys-default 0.1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  :config
  (key-chord-mode 1))

(use-package almost-mono-themes
  :config
   (load-theme 'almost-mono-black t)
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;;(load-theme 'almost-mono-white t)
  )
