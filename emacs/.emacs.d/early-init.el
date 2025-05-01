(setq package-enable-at-startup t)
(setq package-install-upgrade-built-in t)

(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")
		       ("gnu-devel" . "https://elpa.gnu.org/devel/")))
(package-refresh-contents t)
(setq package-selected-packages '(almost-mono-themes
				  evil
				  evil-collection
				  key-chord
				  org
				  magit
				  seq
				  ediff
				  tide
				  typescript-mode
				  company
				  eldoc
				  verb
				  smex
				  ido-completing-read+
				  ido-yes-or-no
				  eglot))
(setq package-load-list '((goto-chg t)
			  (evil t)
			  (verb t)
			  (almost-mono-themes t)
			  (key-chord t)
			  (annalist t)
			  (evil-collection t)
			  (memoize t)
			  (ido-completing-read+ t)
			  (ido t)
			  (ido-yes-or-no t)
			  (smex t)
			  (compat t)
			  (llama t)
			  (with-editor t)
			  (transient t)
			  (seq t)
			  (magit t)
			  (magit-section t)
			  ))
(package-install-selected-packages t)
