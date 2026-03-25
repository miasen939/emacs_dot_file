;;; my-windows-manage.el --- widows management -*- no-byte-compile: t; lexical-binding: t; -*-

  ;;; Commentary:

;; 
;;

    ;;; Code:
(use-package emacs
  :bind (("M-o" . other-window)
         ("s-o" . other-frame))
  :hook (after-make-frame-functions . (lambda (frame)
                                        (select-frame frame)
                                        (toggle-frame-maximized))))

(use-package tab-bar
  :ensure nil
  :demand t
  :bind (
         ("C-c s" . tab-bar-switch-to-tab)
         ("C-c V" . tab-bar-close-tab)
         )
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*dashboard*")
  :init
  (tab-bar-mode 1))

(use-package burly
  :after tab-bar
  :bind
  (("C-c v" . my/create-workspace)
   ("C-c r" . my/save-workspace)
   )
  :config
  (defun my/create-workspace ()
    "创建新的工作区标签."
    (interactive)
    (tab-bar-new-tab)
    (let ((name (read-string "Workspace name: ")))
      (tab-bar-rename-tab name)))

  (defun my/save-workspace ()
    "保存当前标签的窗口布局."
    (interactive)
    (burly-bookmark-windows
     (format "tab-%s" (alist-get 'name (tab-bar--current-tab))))))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ibuffer
  :ensure nil                  ; ibuffer 是 Emacs 内置，不需要从 MELPA 安装
  :bind
  ;; ("C-x C-b" . ibuffer)
                                        ; 覆盖默认 C-x C-b 的 list-buffers，用 ibuffer 代替

  :custom
  (ibuffer-default-sorting-mode 'major-mode)        ; 按 major-mode 排序
  (ibuffer-show-empty-filter-groups nil)            ; 隐藏空分组

  :hook
  (ibuffer-mode . ibuffer-auto-mode)                ; 打开 ibuffer 时自动开启分组并实时更新
  )

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
    (setq popper-reference-buffers
      '(;; 基础
        "\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Warnings\\*"
        "\\*Backtrace\\*"
        calendar-mode
        flymake-diagnostics-buffer-mode
        flycheck-error-list-mode
        pdf-outline-buffer-mode
        help-mode
        compilation-mode

        ;; 终端
        "^\\*eshell.*\\*$" eshell-mode
        "^\\*shell.*\\*$"  shell-mode
        "^\\*term.*\\*$"   term-mode
        "^\\*vterm.*\\*$"  vterm-mode

        ;; LSP
        "\\*eldoc.*\\*"    eldoc-mode
        "\\*xref\\*"
        "\\*Flymake diagnostics.*\\*"

        ;; Org
        ;;"\\*Org Agenda\\*"
        "\\*Capture\\*"
        ;;"\\*org-roam\\*"

        ;; 搜索
        "\\*Occur\\*"      occur-mode
        "\\*grep\\*"       grep-mode
        "\\*ripgrep-search\\*"

        ;; Magit
        "\\*magit-diff.*\\*"
        "\\*magit-process.*\\*"

        ;; Helpful
        helpful-mode

        ;; 其他
        "\\*Dictionary\\*"
        "^\\*sdcv:.*\\*$"  sdcv-mode
        "\\.gpg$"
        "\\*Python\\*"     inferior-python-mode))
  
  (popper-mode +1)
  (popper-echo-mode +1))
(provide 'my-windows-manage)

      ;;; my-windows-manage.el ends here
