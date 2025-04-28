(defvar is-macos (string-equal "darwin" system-type))

(if is-macos
    (setq ns-alternate-modifier 'meta
  	mac-right-alternate-modifier 'none
  	mac-right-option-modifier nil
  	mac-control-modifier 'control
  	mac-command-modifier 'meta))

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(when (not is-macos)
  (set-frame-font "Iosevka Term 12" nil t))
(global-font-lock-mode 1)
(blink-cursor-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
;; (setq whitespace-style '(face tabs spaces trailing lines space-before-tab indentation empty space-after-tab space-mark tab-mark missing-newline-at-eof))

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-x") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'nil)
(global-set-key (kbd "C-x C-p") 'nil)
(global-set-key (kbd "C-x h") 'nil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq display-buffer-alist '(("\\*Buffer List\\*"
  			    (display-buffer-reuse-mode-window)
  			    (window-height . fit-window-to-buffer)
  			    (dedicated . t)
  			    (body-function . select-window))
  			   ("\\*Occur\\*"
  			    (display-buffer-reuse-mode-window)
  			    (window-height . fit-window-to-buffer)
  			    (body-function . select-window)
  			    (window-parameters . ((mode-line-format . none))))
  			   ("\\*Async Shell Command\\*"
  			    (display-buffer-reuse-mode-window)
  			    (body-function . select-window))
  			   ("compilation"
  			    (display-buffer-reuse-mode-window)
  			    (body-function . delete-other-windows))
  			   ("magit"
  			    (display-buffer-reuse-mode-window)
  			    (body-function . delete-other-windows))
  			   ("\\*Help\\*"
  			    (display-buffer-reuse-window)
  			    (body-function . select-window))
  			   ("\\*tide-documentation\\*"
  			    (display-buffer-reuse-window)
  			    (window-height . fit-window-to-buffer)
  			    (window-parameters . ((mode-line-format . none)))
  			    (body-function . select-window))
			   ("\\*eldoc\\*"
			    (display-buffer-no-window))))

(setq bookmark-save-flag 1)

(add-hook 'compilation-mode 'evil-collection-init)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq custom-file "~/.emacs.d/custom.el")

(setf dired-kill-when-opening-new-dired-buffer t)
(put 'dired-find-alternate-file 'disabled nil)

(setq eval-ssh--socket-files '("~/.ssh/mailer" "~/.ssh/github"))

(defun eval-ssh-darwin () 
  (let* ((format-string "ssh-add %s")
  	   (cmd (format format-string (string-join eval-ssh--socket-files " "))))
    (shell-command-to-string cmd)))

(defun eval-ssh-gnu ()
  (let* ((format-string "eval $(ssh-agent -s) && ssh-add %s && echo $SSH_AUTH_SOCK")
       (cmd (format format-string (string-join eval-ssh--socket-files " "))))
    (progn
      (shell-command-to-string "rm -r /tmp/ssh-*")
      (let ((result (shell-command-to-string cmd)))
      (when (string-match "/tmp/ssh-.*" result)
    	(setenv "SSH_AUTH_SOCK" (match-string 0 result)))))))
(if is-macos (eval-ssh-darwin) (eval-ssh-gnu))

(setq package-archives '(("elpa" . "https://tromey.com/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-selected-packages '(use-package
				  almost-mono-themes
				  evil
				  evil-collection
				  key-chord
				  org
				  magit
				  ediff
				  tide
				  typescript-mode
				  company
				  eldoc
				  verb
				  smex
				  ido-completing-read+
				  ido-yes-or-no))

(package-install-selected-packages t)

(require 'almost-mono-themes)
(load-theme 'almost-mono-white t)

(setq evil-want-keybinding nil)
(require 'evil)
(require 'key-chord)
(setq evil-insert-state-cursor '(box)
      evil-normal-state-cursor '(box)
      evil-want-integration t
      evil-want-minibuffer t)

(evil-define-key 'normal dired-mode-map "h" 'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "q" 'kill-current-buffer)
(evil-define-key 'normal dired-mode-map "(" 'dired-hide-details-mode)
(evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
(evil-define-key 'normal dired-mode-map "gg" 'revert-buffer)
(evil-define-key 'normal dired-mode-map "f" 'dired-create-empty-file)
(evil-define-key 'visual dired-mode-map "u" 'dired-unmark)
(evil-define-key 'visual dired-mode-map "s" 'dired-do-relsymlink)

(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)

(evil-define-key 'normal typescript-mode-map (kbd "C-x C-k") 'eldoc-print-current-symbol-info)
(evil-define-key 'normal typescript-mode-map (kbd "K") 'tide-documentation-at-point)
(evil-define-key 'normal tide-mode-map "gd" 'evil-jump-to-tag)

(setq key-chord-two-keys-default 0.1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(evil-set-initial-state 'shell-mode 'normal)
(evil-set-initial-state 'verb-response-body-mode 'motion)
(evil-set-initial-state 'verb-response-headers-mode 'motion)

(evil-mode)
(key-chord-mode 1)

(require 'evil-collection)
(setq evil-collection-setup-minibuffer t)
(add-hook 'dired-mode-hook 'evil-collection-init)
(add-hook 'compilation-mode-hook 'evil-collection-init)
(evil-collection-init t)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-max-window-height 1)
(setq ido-decorations '(
			"["
			"]"
			" | "
			" | ..."
			"["
			"]"
			" [No match]"
			" [Matched]"
			" [Not readable]"
			" [Too big]"
			" [Confirm]"))
(ido-mode 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'org)
(setq org-confirm-babel-evaluate nil)
(setq org-hide-leading-stars t)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.3))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.2))))
  '(org-level-6 ((t (:inherit outline-5 :height 1.1))))
  '(org-level-7 ((t (:inherit outline-5 :height 1.0)))))

(require 'typescript-mode)
(setq typescript-indent-level 4)
(setq typescript-auto-indent-flag t)
(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))

(global-eldoc-mode -1)
(setq eldoc-display-functions (list 'eldoc-display-in-echo-area))


(require 'magit)
(add-hook 'magit-mode-hook 'evil-collection-init)

(setq eval-ssh--socket-files '("~/.ssh/mailer" "~/.ssh/github"))

(defun eval-ssh-darwin () 
  (let* ((format-string "ssh-add %s")
	     (cmd (format format-string (string-join eval-ssh--socket-files " "))))
    (shell-command-to-string cmd)))

(defun eval-ssh-gnu ()
  (let* ((format-string "eval $(ssh-agent -s) && ssh-add %s && echo $SSH_AUTH_SOCK")
  	 (cmd (format format-string (string-join eval-ssh--socket-files " "))))
    (progn
      (shell-command-to-string "rm -r /tmp/ssh-*")
      (let ((result (shell-command-to-string cmd)))
  	(when (string-match "/tmp/ssh-.*" result)
  	  (setenv "SSH_AUTH_SOCK" (match-string 0 result)))))))
(if is-macos (eval-ssh-darwin) (eval-ssh-gnu))
