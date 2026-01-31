;;; post-init.el --- Modern Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Emacs configuration for Windows GUI
;; 优化的 Windows GUI Emacs 配置文件

;;; Code:

;; ;;==============================================================================
;; ;;; 启动性能优化 (Performance Optimization)
;; ;;==============================================================================
;; 
;; ;; 启动时临时增加 GC 阈值，加快启动速度
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6)
;; 
;; ;; 启动完成后恢复正常 GC 设置
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold (* 16 1024 1024)
;;                   gc-cons-percentage 0.1)))
;; 
;; ;; 减少启动时的文件处理器数量
;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq file-name-handler-alist default-file-name-handler-alist)))
;; 
;; ;;==============================================================================
;; ;;; Package 管理 (Package Management)
;; ;;==============================================================================
;; 
;; ;; 确保 use-package 已安装
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; 
;; (require 'use-package)
(setq use-package-always-ensure t)  ;; 默认自动安装包
(setq use-package-always-defer t)   ;; 默认延迟加载

;;==============================================================================
;;; 基础设置 (Basic Settings)
;;==============================================================================

;; 文件编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Windows 特定设置
(when (eq system-type 'windows-nt)
  ;; 使用系统回收站
  (setq delete-by-moving-to-trash t)
  ;; 优化 Windows 性能
  (setq w32-pipe-read-delay 0)
  (setq w32-get-true-file-attributes nil))

;; ;; 用户界面优化
;; (setq inhibit-startup-screen t)           ;; 禁用启动画面
;; (setq initial-scratch-message nil)        ;; 清空 scratch buffer 消息
;; (tool-bar-mode -1)                        ;; 关闭工具栏
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))                   ;; 关闭滚动条
;; (setq use-dialog-box nil)                 ;; 禁用对话框

;; 编辑体验
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)       ;; 使用空格而非 Tab
(setq require-final-newline t)            ;; 文件末尾自动添加换行
(setq truncate-lines nil)                 ;; 自动换行
(global-hl-line-mode 1)                   ;; 高亮当前行
(show-paren-mode 1)                       ;; 显示匹配括号
(electric-pair-mode 1)                    ;; 自动配对括号
(delete-selection-mode 1)                 ;; 选中文本后输入会替换
(global-auto-revert-mode 1)               ;; 自动刷新文件

;; 备份和自动保存
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)
(setq make-backup-files t)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; 历史记录
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq history-length 500)

;;==============================================================================
;;; 字体和主题 (Fonts & Theme)
;;==============================================================================

(defun my/setup-fonts ()
  "设置字体配置."
  (when (display-graphic-p)
    ;; 英文字体
    (set-face-attribute 'default nil
                        :height 160          ;; 14pt (Windows 上建议 140-160)
                        :weight 'normal
                        :family "Fira Code")
    
    ;; 中文字体 - 使用更通用的字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family "Microsoft YaHei"
                                   :height 160)))))


;; 延迟加载字体设置
(add-hook 'after-init-hook #'my/setup-fonts)

;; 主题配置
(use-package ef-themes
  :demand t
  :bind (("<f5>" . modus-themes-rotate)
         ("C-<f5>" . modus-themes-select)
         ("M-<f5>" . modus-themes-load-random))
  :init
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)
  :config
  (modus-themes-load-theme 'ef-owl))

;;==============================================================================
;;; 文档查看 (Document Viewers)
;;==============================================================================

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-view-roll-minor-mode)
  :config
  (pdf-tools-install :no-query))

;;==============================================================================
;;; Org Mode
;;==============================================================================

(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c <up>" . org-priority-up)
         ("C-c <down>" . org-priority-down)
         ("C-c C-g C-r" . org-shiftmetaright))
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode))
  :custom
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t))

;;==============================================================================
;;; Markdown
;;==============================================================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-preview)
              ("C-c C-c l" . markdown-live-preview-mode)
              ("C-c C-s c" . markdown-insert-code)
              ("C-c C-s C" . markdown-insert-gfm-code-block)
              ("C-c C-s b" . markdown-insert-bold)
              ("C-c C-s i" . markdown-insert-italic)
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-i" . markdown-insert-image)
              ("C-c C-n" . markdown-next-heading)
              ("C-c C-p" . markdown-previous-heading))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-gfm-use-electric-backquote t)
  (markdown-asymmetric-header t))

(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc))
  :custom
  (markdown-toc-header-toc-start "<!-- TOC -->")
  (markdown-toc-header-toc-end "<!-- /TOC -->"))

(use-package valign
  :after markdown-mode
  :hook (markdown-mode . valign-mode)
  :custom
  (valign-fancy-bar t))

;;==============================================================================
;;; 补全框架 (Completion Framework)
;;==============================================================================

;; Vertico - 垂直补全界面
(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("<escape>" . minibuffer-keyboard-quit))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 15)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Orderless - 灵活的匹配样式
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - 补全注释
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult - 增强的搜索和导航
(use-package consult
  :bind (;; C-c 前缀
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x 前缀
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; 其他
         ("M-y" . consult-yank-pop)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ;; M-s 前缀
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s l" . consult-line)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key "M-.")
  (setq consult-narrow-key "<"))
;;==============================================================================
;;; Projectile - 项目管理
;;==============================================================================
(use-package projectile
  
  ;; 常用设置（可以根据需要增删）
  :custom
;  (projectile-project-search-path '("~/projects/" "~/work/" "~/code/" "~/src/"))  ;; 你的项目根目录们
  
  ;; 可选：让 mode-line 显示更简洁（显示项目名）
  (projectile-mode-line-function
   (lambda () (format " P[%s]" (projectile-project-name))))
  
  ;; 可选：开启缓存（速度更快，尤其是大项目）
  (projectile-enable-caching t)
  
  ;; 可选：使用 ripgrep / rg 作为默认搜索工具（强烈推荐）
  ;; 前提是你系统已安装 rg 命令
  (projectile-generic-command "rg --files --hidden --follow --glob '!.git'")

  :bind-keymap
  ;; 推荐的全局前缀键（最常用的是 C-c p）
;  ("C-c p" . projectile-command-map)
  
  ;; macOS 用户很常用 s-p（command + p）
  ;; ("s-p" . projectile-command-map)
  
  :config
  ;; 启动 projectile 全局模式
  (projectile-mode +1))
;;==============================================================================
;;; Embark - 上下文操作
;;==============================================================================

(use-package embark
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-." . embark-act)
         ("C-c C-e" . embark-export))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  :init
  ;; Which-key 集成
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
         keymap nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)))
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

;;==============================================================================
;;; Which-key - 按键提示
;;==============================================================================

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  (which-key-sort-order 'which-key-key-order-alpha)
  :init
  (which-key-mode))

;;==============================================================================
;;; 代码补全 (Code Completion)
;;==============================================================================

(use-package corfu
  :demand t
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("RET" . corfu-insert)
              ([return] . corfu-insert)
              ("C-g" . corfu-quit)
              ("M-SPC" . corfu-insert-separator)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-q" . corfu-quick-complete)
              ("C-q" . corfu-quick-insert))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-scroll-margin 5)
  (corfu-count 12)
  (corfu-max-width 60)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-max-height 20)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
  ;; 终端支持
  (unless (display-graphic-p)
    (require 'corfu-terminal)
    (corfu-terminal-mode +1))
  ;; 保存历史
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind (("C-c p p" . completion-at-point)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)))

;;==============================================================================
;;; 工作区管理 (Workspace Management)
;;==============================================================================

(use-package tab-bar
  :ensure nil
  :demand t
  :bind (("C-c v" . my/create-workspace)
         ("C-c s" . tab-bar-switch-to-tab)
         ("C-c V" . tab-bar-close-tab)
         ("C-c r" . my/save-workspace))
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*scratch*")
  :init
  (tab-bar-mode 1))

(use-package burly
  :after tab-bar
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

;;==============================================================================
;;; Dashboard - 启动界面
;;==============================================================================

(use-package dashboard
  :demand t
  :custom
  (dashboard-banner-logo-title "事情总是越想越困难，越做越简单，越拖越想放弃。")
  (dashboard-startup-banner
   (let ((images '("E:/Ingredient/ICON/miyamori300.png"
                   "E:/Ingredient/ICON/shirobako.png"
                   "E:/Ingredient/ICON/newgamenene.png")))
     (seq-random-elt (seq-filter #'file-exists-p images))))
  (dashboard-items '((recents . 10)
                     (agenda . 5)
                     (bookmarks . 5)
                     (projects . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

;;==============================================================================
;;; 导航和编辑增强 (Navigation & Editing)
;;==============================================================================

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g k" . avy-kill-region)
         ("M-g K" . avy-kill-ring-save-region)
         ("C-c C-j" . avy-resume))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-all-windows t)
  (avy-background t)
  (avy-single-candidate-jump t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package amx
  :demand t
  :config
  (amx-mode 1))

;;==============================================================================
;;; 编程支持 (Programming Support)
;;==============================================================================

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.3))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
  :hook
  ;; 最常用写法：在所有编程模式下自动启用（强烈推荐）
  (prog-mode . rainbow-delimiters-mode)
  
  ;; 可选：如果你还想在某些非 prog-mode 的地方也启用，比如 REPL、org-src 等
  ;; (emacs-lisp-mode . rainbow-delimiters-mode)
  ;; (clojure-mode  . rainbow-delimiters-mode)
  ;; (inferior-ess-mode . rainbow-delimiters-mode)   ;; R 的 REPL
  )

;;==============================================================================
;;; 文件历史和自动恢复 (File History & Auto-revert)
;;==============================================================================

(use-package autorevert
  :ensure nil
  :demand t
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode 1))

(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-saved-items 100)
  (recentf-exclude
   '("\\.tar$" "\\.tbz2$" "\\.tgz$" "\\.bz2$" "\\.gz$"
     "\\.zip$" "\\.7z$" "\\.rar$"
     "COMMIT_EDITMSG\\'"
     "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
     "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (recentf-mode 1)
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :demand t
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     corfu-history
     vertico-repeat-history))
  :config
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :demand t
  :custom
  (save-place-limit 400)
  :config
  (save-place-mode 1))

;;==============================================================================
;;; 启动优化 (Startup Optimization)
;;==============================================================================

;; 启动全屏
(add-hook 'emacs-startup-hook #'toggle-frame-maximized)

;; 显示启动时间
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs 启动完成，用时 %.2f 秒，加载了 %d 个包"
;;                      (float-time (time-subtract after-init-time before-init-time))
;;                      (length package-activated-list))))

;;==============================================================================

(provide 'post-init)
;;; post-init.el ends here
