;;; my-DevOps.el --- coding operations/SRE -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(use-package projectile
  :defer 0.2
  ;; 常用设置（可以根据需要增删）
  :custom
  (projectile-project-search-path '("~/projects/" "~/work/" "~/playground" "~/Documents/"))
  (projectile-files-command "ripgrep --files --hidden --follow --glob '!.git'") 
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c C-p") 'projectile-command-map)

  (projectile-mode +1)
  )

;; eat
;; (use-package eat
;;   )

;; vterm
(use-package vterm
  :bind
  ("C-c t" . vterm)
  :config
  ;; (add-hook 'vterm-mode-hook
  ;;           (lambda ()
  ;;             (setq-local global-hl-line-mode nil)))
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

(use-package magit
    :bind ("C-x g" . magit-status)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  (provide 'my-DevOps)

;;; my-DevOps.el ends here
