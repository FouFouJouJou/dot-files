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
(blink-cursor-mode 1)
(line-number-mode)
(column-number-mode)
(visual-line-mode)
(when (not is-macos)
  (set-frame-font "Iosvmata 12" nil t))
(global-font-lock-mode 1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq whitespace-line-column 250)
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab indentation empty space-after-tab space-mark tab-mark missing-newline-at-eof))
(global-whitespace-mode)

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
			      (display-buffer-no-window))
			     ("\\*HTTP Headers\\*"
			      (display-buffer-reuse-mode-window)
			      (dedicated . t)
			      (window-height . fit-window-to-buffer)
			      (body-function . select-window))
			     ("\\*ggtags-global\\*"
			      (display-buffer-no-window))))

(setq bookmark-save-flag 1)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq custom-file "~/.emacs.d/custom.el")

(setf dired-kill-when-opening-new-dired-buffer t)

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

(defun ssh-init ()
  (interactive)
  (if is-macos (eval-ssh-darwin) (eval-ssh-gnu)))

(load-theme 'almost-mono-white t)

(setq evil-want-keybinding nil)

(setq evil-insert-state-cursor '(box)
      evil-normal-state-cursor '(box)
      evil-want-minibuffer t)

(require 'evil)
(evil-define-key 'normal dired-mode-map "h" #'dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" #'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "q" #'kill-current-buffer)
(evil-define-key 'normal dired-mode-map "(" #'dired-hide-details-mode)
(evil-define-key 'normal dired-mode-map "u" #'dired-unmark)
(evil-define-key 'normal dired-mode-map "gg" #'revert-buffer)
(evil-define-key 'normal dired-mode-map "f" #'dired-create-empty-file)
(evil-define-key 'visual dired-mode-map "u" #'dired-unmark)
(evil-define-key 'normal typescript-mode-map (kbd "C-x C-k") #'eldoc-print-current-symbol-info)
(evil-define-key 'normal typescript-mode-map (kbd "K") #'tide-documentation-at-point)
(evil-define-key nil evil-insert-state-map (kbd "C-r") #'comint-history-isearch-backward-regexp)

(setq key-chord-two-keys-default 0.1)

(key-chord-define evil-insert-state-map "jk" #'evil-normal-state)
(evil-set-initial-state 'shell-mode 'normal)

(evil-mode)
(key-chord-mode 1)

(require 'evil-collection)
(setq evil-collection-setup-minibuffer t)
(evil-collection-init)

(setq ido-enable-flex-matching t)
(put 'dired-find-alternate-file 'disabled nil)
(setq ido-create-new-buffer 'always)
(setq-default confirm-nonexistent-file-or-buffer nil)
(setq ido-file-extensions-order '(".c" ".h" ".ts" ".js" ".org" ".el" ".json"))
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

(ido-ubiquitous-mode 1)

(ido-yes-or-no-mode 1)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(setq org-confirm-babel-evaluate nil)
(setq org-hide-leading-stars t)
(evil-define-key 'normal org-mode-map (kbd "M-k") #'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-l") #'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-j") #'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-h") #'org-metaleft)

(org-babel-do-load-languages
'org-babel-load-languages
'((shell . t)))

(setq typescript-indent-level 2)
(setq typescript-auto-indent-flag t)
(add-to-list 'auto-mode-alist '("\\.ts" . typescript-mode))
(add-hook 'typescript-mode-hook #'tide-setup)
(add-hook 'tide-mode-hook (lambda () (evil-define-key 'normal tide-mode-map "gd" #'evil-jump-to-tag)) 100)


(global-eldoc-mode -1)
(setq eldoc-display-functions (list #'eldoc-display-in-echo-area))

(require 'verb)
(setq verb-enabled-log 'nil
      verb-auto-kill-response-buffers t)
(evil-set-initial-state 'verb-response-body-mode 'motion)
(evil-set-initial-state 'verb-response-headers-mode 'motion)
(define-key verb-mode-map (kbd "C-c C-c") #'verb-send-request-on-point)
(define-key verb-mode-map (kbd "C-c C-<return>") #'verb-send-request-on-point-no-window)
(define-key verb-mode-map (kbd "C-c C-k") #'verb-kill-all-response-buffers)
(define-key verb-response-body-mode-map (kbd "C-c C-k") #'verb-kill-all-response-buffers)
(define-key verb-response-body-mode-map (kbd "C-c C-h") #'verb-toggle-show-headers)
(define-key verb-response-headers-mode-map (kbd "C-c C-k") #'verb-kill-all-response-buffers)

(add-hook 'c-mode-hook 'ggtags-mode)

(profiler-stop)
