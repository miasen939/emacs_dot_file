;;; my-DevOps.el --- coding operations/SRE -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(use-package projectile
  :defer t
  ;; 常用设置（可以根据需要增删）
  :custom
  (projectile-project-search-path '("~/projects/" "~/work/" "~/playground" "~/Documents/"))
  (projectile-files-command "ripgrep --files --hidden --follow --glob '!.git'")
  :config
  (projectile-mode +1)
  )


(use-package vterm
  :bind
  ("C-c t" . vterm)
  ;; "C-u C-c t" vterm-new-window
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)))
                                        ;解决vterm闪烁
  

  :init
  
  (setq vterm-timer-delay 0.05)  ; Faster vterm
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (defun my-vterm--setup ()

    ;; Inhibit early horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Suppress prompts for terminating active processes when closing vterm
    (setq-local confirm-kill-processes nil))
  (add-hook 'vterm-mode-hook #'my-vterm--setup)
  )

;; (use-package eat)
(use-package ghostel
  :bind (
         ;; ("C-c t" . ghostel)
         :map ghostel-semi-char-mode-map
         
         ("C-s"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))

  ;; (setq ghostel--pixel-anchor-supported-p nil)
  ;; (with-eval-after-load 'ghostel
  ;; (defun my/ghostel-disable-pixel-anchor (&rest _)
  ;;   nil)
  ;; 
  ;; (advice-add 'ghostel--pixel-anchor
  ;;             :override
  ;;             #'my/ghostel-disable-pixel-anchor))
  )


(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-status)
    ;; :custom
    ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    )

;; todo: tramp/postman


(provide 'my-DevOps)
;;; my-DevOps.el ends here
