(profiler-start 'cpu)
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
				  ido-yes-or-no))
(setq package-load-list '(all))
(package-install-selected-packages t)
