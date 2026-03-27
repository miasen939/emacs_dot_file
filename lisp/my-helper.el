;;; my-helper.el --- 发现式使用emacs -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(use-package casual-suite
  :bind
  (("M-j" . casual-avy-tmenu)
   ("C-o" . casual-editkit-main-tmenu)
   ("M-m" . casual-suite-tmenu)


   
   :map calc-mode-map
   ("M-m" . casual-calc-tmenu)

   :map dired-mode-map
   ("M-m" . casual-dired-tmenu)

   :map org-mode-map
   ("M-m" . casual-org-tmenu)

   :map org-table-fedit-map
   ("M-m" . casual-org-table-fedit-tmenu)

   :map org-agenda-mode-map
   ("M-m" . casual-agenda-tmenu)

   :map ibuffer-mode-map
   ("M-m" . casual-ibuffer-tmenu)

   :map bookmark-bmenu-mode-map
   ("M-m" . casual-bookmarks-tmenu)

   :map calendar-mode-map
   ("M-m" . casual-calendar-tmenu)

   :map compilation-mode-map
   ("M-m" . casual-compile-tmenu)

   :map help-mode-map
   ("M-m" . casual-help-tmenu)

   :map Info-mode-map
   ("M-m" . casual-info-tmenu)

   :map image-mode-map
   ("M-m" . casual-image-tmenu)))

 ;; casual-image
 ;; casual-isearch-tmenu
 ;; casual-bibtex-tmenu
 ;; casual-info
 ;; casual-symbol-overlay
 ;; re-builder
 ;; casual-calendar
 ;; casual bookmarks
 ;; casual compile
 ;; M-x man and casual man

(use-package embark
  :bind (("C-,"   . embark-act)
         ("C-M-," . embark-dwim)        ; 智能猜测最可能的操作
         ("C-h B" . embark-bindings)    ; 列出所有可用绑定
         :map minibuffer-local-map
         ("C-."   . embark-act)         ; minibuffer 里用 C-.
         ("C-c C-e" . embark-export)
         :map org-mode-map
         ("C-," . embark-act))          ; 导出候选列表
  :custom
  (embark-quit-after-action nil)        ; 执行 action 后不退出，方便连续操作
  (prefix-help-command #'embark-prefix-help-command)
  :init
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         keymap nil nil 'no-paging))
      #'which-key--hide-popup-ignore-command)) ; ← 修复：移到外层括号之后
  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (setq embark-action-indicator #'embark-which-key-indicator
        embark-become-indicator #'embark-which-key-indicator)
  :config
  (defun embark--truncate-target (target)
    (if (and (stringp target) (> (length target) 30))
        (concat (substring target 0 27) "...")
      target)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :defer 0.2
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  (which-key-sort-order 'which-key-key-order-alpha)
  :init
  (which-key-mode))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))


;; todo: c-h/hydra/transient/embark
(provide 'my-helper)

;;; my-helper.el ends here
