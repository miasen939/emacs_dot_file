;;; post-init.el --- Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-

    (setq use-package-always-ensure t)  ;; 默认自动安装包
    (setq use-package-always-defer t)   ;; 默认延迟加载


   
    ;; 文件编码
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)

    ;; 编辑体验
    (setq-default tab-width 4)
    (setq-default indent-tabs-mode nil)       ;; 使用空格而非 Tab
    (setq require-final-newline t)            ;; 文件末尾自动添加换行
    (setq truncate-lines nil)                 ;; 自动换行
  ;;  (global-hl-line-mode 1)                   ;; 高亮当前行
    (show-paren-mode 1)                       ;; 显示匹配括号
    (electric-pair-mode 1)                    ;; 自动配对括号
    (delete-selection-mode 1)                 ;; 选中文本后输入会替换
    (global-auto-revert-mode 1)               ;; 自动刷新文件
  ;;  (setq display-line-numbers-type 'relative)  ;显示相对行号
;;    (global-display-line-numbers-mode 1)

    ;; 对选中文本使用括号引号自动放入
    ;; 备份和自动保存
    ;; (setq auto-save-default nil)
    ;; (setq auto-save-interval 300)
    ;; (setq auto-save-timeout 30)
    ;; (setq make-backup-files t)
    ;; (setq backup-directory-alist
    ;;       `(("." . ,(expand-file-name "backups" user-emacs-directory))))
    ;; (setq auto-save-file-name-transforms
    ;;       `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
    (auto-save-visited-mode 1)
    (setq auto-save-timeout 20)
    ;; 历史记录
    (savehist-mode 1)
    (save-place-mode 1)
    (recentf-mode 1)
    (setq recentf-max-saved-items 100)
    (setq history-length 500)
    ;; 启动全屏
    (add-hook 'emacs-startup-hook #'toggle-frame-maximized)

    ;;==============================================================================

(global-subword-mode 1)

(use-package emacs
  :bind ("M-o" . other-window)
  :hook (after-make-frame-functions . (lambda (frame)
                                        (select-frame frame)
                                        (toggle-frame-maximized))))

;; steal from system crafter
(setq large-file-warning-threshold nil)
(column-number-mode 1)

(custom-set-faces
 '(gnus-group-news-low ((t (:foreground "cyan"))))
 '(gnus-group-news-low-empty ((t (:foreground "cyan" :weight normal)))))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )



(defun my/setup-fonts ()
  "设置字体配置."
  (when (display-graphic-p)
    ;; 英文字体
    (set-face-attribute 'default nil
                        :height 130          ;; 14pt (Windows 上建议 140-160)
                        :weight 'normal
                        :family "DejaVu Sans mono")
    
    ;; 中文字体 - 使用更通用的字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family "Sarasa Fixed SC"
                                   :height 130)))))

;;(add-to-list 'default-frame-alist '(undecorated . t))


;; 延迟加载字体设置
(add-hook 'after-init-hook #'my/setup-fonts)

(use-package doom-themes
  :demand t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-vibrant t)
                                        ;  (load-theme 'doom-nord t)
                                        ;  (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; 其他主题配置
;; (use-package ef-themes
;;   :demand t
;;   :bind (("<f5>" . modus-themes-rotate)
;;          ("C-<f5>" . modus-themes-select)
;;          ("M-<f5>" . modus-themes-load-random))
;;   :init
;;   (setq modus-themes-mixed-fonts t
;;         modus-themes-italic-constructs t)
;;   :config
;;   (setq ef-owl-palette-overrides
;;         '((bg-region "#1a3f4a")))
;;   (moduns-themes-load-theme 'ef-owl))

(use-package dashboard
  :demand t
  :custom
  (dashboard-banner-logo-title "事情总是越想越困难，越做越简单，越拖越想放弃。\n\t\t\tStay Stong my friend.")
  ;; (dashboard-startup-banner
  ;;  (let ((images '("~/Pictures/icon/miyamori300.png"
  ;;                  "~/Pictures/icon/shirobako.png"
  ;;                  "~/Pictures/icon/newgamenene.png")))
  ;;    (seq-random-elt (seq-filter #'file-exists-p images))))

  (dashboard-items '(
                     (agenda . 10)
                     (recents . 8)
                     (bookmarks . 5)
                     (projects . 5)))
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)

  :config
  (dashboard-setup-startup-hook)
  )

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

(use-package good-scroll
  :ensure t
  :if window-system     ; 在图形化界面时才使用这个插件
  :init (good-scroll-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)

  :config
  (pdf-tools-install :no-query)
  )
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

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
  (org-hide-emphasis-markers t)
  (org-return-follows-link t)
  :config
  (setq org-startup-with-inline-images t)

                                        ;org-export
  (require 'ox-md)
  ;; img
  ;;org-todo
  ;; TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)"
                    "BLOCKED(b@)" "|" "DONE(d!)" "CANCELED(c@!)")))
  (setq org-log-done 'note) 
  ;; TODO colors
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("PLANNING" . (:foreground "DeepPink" :weight bold))
          ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("CANCELED" . (:foreground "LimeGreen" :weight bold))
          ))
  ;;org-agenda
  (setq org-agenda-files '
        ("~/Documents/org-note/agenda/TODOs.org")))
;;org-capture

(use-package org-download
  :ensure t
  :defer t
  :bind (:map org-mode-map
              ("C-c C-M-y" . org-download-clipboard)   ; 推荐快捷键：粘贴剪贴板图片
              ("C-c M-y"   . org-download-yank))       ; 另一种常用快捷键
  :config
  ;; 核心配置写在这里
  (setq org-download-method 'directory
        org-download-image-dir "./static/img")
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-link-format-function
        (lambda (link)
          (format "[[file:%s]]\n" (file-relative-name link))))

  ;; 是否自动给文件名加时间戳前缀（避免重名）
  (setq org-download-timestamp "_%Y%m%d_%H%M%S")

  ;; Mac/Linux 剪贴板截图特别推荐再加这一行
  (when (eq system-type 'darwin)   ; macOS
    (setq org-download-screenshot-method "screencapture -i %s"))
  (setq org-image-actual-width '(600))
  )

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/roam-note/"))
  (org-roam-capture-templates
   '(    ("d" "default" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         ("s" "Study" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :Study:

      ")
          :unnarrowed t)
         ("e" "Emacs" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :Emacs:

      ")
          :unnarrowed t)
         ("c" "COD note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :textbook:Computer Organization and Design MIPS Edition:Computer Architecture:Study:\n\n#+book:Computer Organization and Design MIPS Edition
  ")
          :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n i" . org-roam-node-insert-immediate))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  
  
  (setq org-id-locations-file "~/.emacs.d/var/.org-id-locations")
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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

;; (use-package casual
;;   :after org
;;   :bind (:map org-mode-map
;;               ("M-m" . casual-org-tmenu)
;;               :map org-table-fedit-map
;;               ("M-m" . casual-org-table-fedit-tmenu))
;;   :config
;;   )
;;
(use-package casual-suite
  :bind (;; 全局入口：在任何支持的 Buffer 中一键唤起
         ("M-m" . casual-suite-tmenu)
         
         ;; 如果你习惯在特定模式下使用更直观的快捷键
         :map calc-mode-map ("M-m" . casual-calc-tmenu)
         :map isearch-mode-map ("M-m" . casual-isearch-tmenu)
         :map dired-mode-map ("M-m" . casual-dired-tmenu)
         :map org-mode-map ("M-m" . casual-org-tmenu)
         :map org-table-fedit-map ("M-m" . casual-org-table-fedit-tmenu)
         :map org-agenda-mode-map ("M-m" . casual-agenda-tmenu)
         :map ibuffer-mode-map ("M-m" . casual-ibuffer-tmenu)
         :map bookmark-bmenu-mode-map ("M-m" . casual-bookmarks-tmenu)
         :map calendar-mode-map ("M-m" . casual-calendar-tmenu)
         :map compilation-mode-map ("M-m" . casual-compile-tmenu)
         :map eww-mode-map ("M-m" . casual-eww-tmenu)
         :map help-mode-map ("M-m" . casual-help-tmenu)
         :map Info-mode-map ("M-m" . casual-info-tmenu)
         ;;:map re-builder-mode-map ("M-m" . casual-re-builder-tmenu)
       ;;  :map shell-mode-map ("M-m" . casual-eshell-tmenu)
         :map image-mode-map ("M-m" . casual-image-tmenu))

  :config
  ;; 默认情况下，suite 会自动检测并启用它支持的所有模块
  ;; 你可以在这里进行全局微调
                                        ;      (setq casual-use-avy-for-navigation t)
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

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
         ("M-s d" . consult-fd)
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
  (setq consult-async-min-input 2)
  (setq consult-narrow-key "<"))

;;==============================================================================
;;; Projectile - 项目管理
;;==============================================================================
(use-package projectile
  :demand t
  ;; 常用设置（可以根据需要增删）
  :custom
  (projectile-project-search-path '("~/projects/" "~/work/" "~/playground" "~/Documents/"))
  (projectile-files-command "ripgrep --files --hidden --follow --glob '!.git'") 
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c C-p") 'projectile-command-map)

  (projectile-mode +1)
  )

;;==============================================================================
;;; Embark - 上下文操作
;;==============================================================================

(use-package embark
  :bind (("C-," . embark-act)
         ;;("C-;" . embark-dwim)
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

(use-package nerd-icons-corfu
  :after corfu
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; You can alternatively specify a function to perform the mapping,
          ;; use this when knowing the exact completion candidate is important.
          ;; Don't pass `:face' if the function already returns string with the
          ;; face property, though.
          (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  )

(use-package corfu-terminal
:ensure t)

;; (use-package corfu
;;   ;; Optional customizations
;;   ;; :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
;; 
;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))
;; 
;;   :init
;; 
;;   ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
;;   ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
;;   ;; variable `global-corfu-modes' to exclude certain modes.
;;   (global-corfu-mode)
;; 
;;   ;; Enable optional extension modes:
;;   ;; (corfu-history-mode)
;;   ;; (corfu-popupinfo-mode)
;;   )

;; A few more useful configurations...
;; (use-package emacs
;;   :custom
;;   ;; TAB cycle if there are only few candidates
;;   ;; (completion-cycle-threshold 3)
;; 
;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (tab-always-indent 'complete)
;; 
;;   ;; Emacs 30 and newer: Disable Ispell completion function.
;;   ;; Try `cape-dict' as an alternative.
;;   (text-mode-ispell-word-completion nil)
;; 
;;   ;; Hide commands in M-x which do not apply to the current mode.  Corfu
;;   ;; commands are hidden, since they are not used via M-x. This setting is
;;   ;; useful beyond Corfu.
;;   (read-extended-command-predicate #'command-completion-default-include-p))
;; Optionally:

;;         ;; If you add an entry for t, the library uses that as fallback.
;;         ;; The default fallback (when it's not specified) is the ? symbol.

;; The Custom interface is also supported for tuning the variable above.

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


(use-package pangu-spacing
  :demand 0.2
  :config
  (global-pangu-spacing-mode +1))

;; yasnippet ??TODO
(use-package yasnippet)
(use-package yasnippet-snippets)

;;==============================================================================
;;; 工作区管理 (Workspace Management)
;;==============================================================================



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

;;==============================================================================
                 ;;; 导航和编辑增强 (Navigation & Editing)
;;==============================================================================

(use-package avy
  :bind (
         ("C-." . avy-goto-char-timer)
                                        ;     ("C-。". avy-goto-char-timer)
         ("C-;" . avy-goto-line)
                                        ;           ("C-u C-;" . avy-goto-word-0)
         ("M-g w" . avy-goto-word-0)
         ("M-g k" . avy-kill-region)
         ("M-g K" . avy-kill-ring-save-region)
         ;;             avy-goto-char-in-line
         ;;               avy-zap
                                        ;("C-c C-j" . avy-resume)
         )
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-all-windows t)
  (avy-background t)
  (avy-single-candidate-jump t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
  )

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package amx
  :demand 0.2
  :config
  (amx-mode 1))

;; expand region
;; (use-package expand-region
;;   :bind ("C-=" . er/expand-region))
;; 
;;
(use-package expreg
  :bind ("C-=" . expreg-expand))

;; (use-package mwim
;;   :config
;;   (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
;;   (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

                                        ;
                                        ;==============================================================================

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
  ;;; Terminal
;;==============================================================================
;; eat
;; (use-package eat
;;   )
;; vterm
(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil))) ;解决vterm闪烁
  (defun my-vterm--setup ()

    ;; Inhibit early horizontal scrolling
    (setq-local hscroll-margin 0)

    ;; Suppress prompts for terminating active processes when closing vterm
    (setq-local confirm-kill-processes nil))

  :init
  
  (setq vterm-timer-delay 0.05)  ; Faster vterm
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  
  (add-hook 'vterm-mode-hook #'my-vterm--setup)

  )
;;==============================================================================

;;==============================================================================
;; ibuffer
(use-package ibuffer
  :ensure nil                  ; ibuffer 是 Emacs 内置，不需要从 MELPA 安装
  :bind
  ("C-x C-b" . ibuffer)        ; 覆盖默认 C-x C-b 的 list-buffers，用 ibuffer 代替

  :custom
  (ibuffer-default-sorting-mode 'major-mode)        ; 按 major-mode 排序
  (ibuffer-show-empty-filter-groups nil)            ; 隐藏空分组

  :hook
  (ibuffer-mode . ibuffer-auto-mode)                ; 打开 ibuffer 时自动开启分组并实时更新
  )

;;解耦
;; (add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory)) 
;; (require 'init-ime)

;;==============================================================================
;;==============================================================================
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
;;==============================================================================
;;
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
          treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
          treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-hide-dot-jj-directory           t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
;; 
;; (treemacs-start-on-boot)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  ("C-x C-d" . dirvish))
;; (use-package dired
;;   :ensure nil
;;   :config
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   ;; this command is useful when you want to close the window of `dirvish-side'
;;   ;; automatically when opening a file
;;   (put 'dired-find-alternate-file 'disabled nil))
;; 
;; (use-package dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
;;    '(("h" "~/"                          "Home")
;;      ("d" "~/Downloads/"                "Downloads")
;;      ("m" "/mnt/"                       "Drives")
;;      ("s" "/ssh:my-remote-server")      "SSH server"
;;      ("e" "/sudo:root@localhost:/etc")  "Modify program settings"
;;      ("t" "~/.local/share/Trash/files/" "TrashCan")))
;;   :config
;;   ;; (dirvish-peek-mode)             ; Preview files in minibuffer
;;   ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes           ; The order *MATTERS* for some attributes
;;         '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
;;         dirvish-side-attributes
;;         '(vc-state nerd-icons collapse file-size))
;;   ;; open large directory (over 20000 files) asynchronously with `fd' command
;;   (setq dirvish-large-directory-threshold 20000)
;;   :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish)
;;    :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
;;    (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
;;    ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
;;    ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
;;    ("f"   . dirvish-file-info-menu)    ; [f]ile info
;;    ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
;;    ("s"   . dirvish-quicksort)         ; [s]ort flie list
;;    ("r"   . dirvish-history-jump)      ; [r]ecent visited
;;    ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
;;    ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
;;    ("*"   . dirvish-mark-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dirvish-history-last)
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-e" . dirvish-emerge-menu)))

;; (use-package emms
;;   :demand 0.2
;;   :config
;;   (require 'emms-setup)
;;   (emms-all)                      ; 开启几乎所有稳定功能
;; 
;;   ;; 播放器优先顺序（mpv 最好，vlc 次之）
;;   (setq emms-player-list '(
;;                            emms-player-mpv
;;                            ))
;; 
;;   ;; 元数据读取（native 是纯 elisp 的，速度还可以）
;;   (setq emms-info-functions '(emms-info-exiftool
;;                               emms-info-ogginfo
;;                               emms-info-opusinfo   ; 如果你有 opus 文件
;;                               ))
;; 
;;   ;; 默认音乐目录（很重要！）
;;   (setq emms-source-file-default-directory "~/Music/music/")
;; 
;;   ;; 显示封面（需要 ImageMagick 或 Image backend）
;;   (setq emms-browser-covers 'emms-browser-cache-thumbnail-async
;;         emms-show-format "Playing: %s")
;;   (setq emms-browser-thumbnail-program "ffmpegthumbnailer")
;; 
;;   ;; 可选：modeline 显示当前歌曲（很实用）
;;   (emms-mode-line 1)
;;   (emms-playing-time 1)
;;   (setq emms-browser-thumbnail-small-size 80
;;         emms-browser-thumbnail-medium-size 150))

(use-package emms
  :demand 0.2
  :config
  

  (emms-all)
  (setq emms-player-list '(emms-player-mpv)
        emms-info-functions '(emms-info-native))

  )

;; mu4e 基本加载（复制粘贴，根据你的系统改路径）
;; (use-package mu4e
;;   :ensure nil                ; 因为不是 package.el 装的
;;   :load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.10.8"
;; 
;;   :config
;;   ;; 指定 mu 二进制路径（通常不需要，但保险起见）
;;   (setq mu4e-mu-binary (executable-find "mu"))
;; 
;;   ;; 最小配置示例（后面再细调）
;;   (setq mu4e-maildir "~/Mail"                  ; 你的邮件存储目录
;;         mu4e-get-mail-command "mbsync -a"      ; 用 isync/mbsync 拉邮件（或 offlineimap）
;;         mu4e-update-interval 300               ; 每 5 分钟检查一次
;;         mu4e-headers-auto-update t
;;         message-send-mail-function 'smtpmail-send-it
;;         smtpmail-default-smtp-server "smtp.gmail.com"  ; 根据你的邮箱改
;;         ;; ... 更多配置见下面
;;         ))

(use-package rime
  :demand 1.0
  :custom
  (default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-cursor "█")
  (setq rime-cursor-face '((t (:foreground "#00ff00"))))
  (setq rime-show-preedit t)
  (setq rime-user-data-dir "~/.config/fcitx/Rime/")
  (setq rime-posframe-style ' simple)
  (setq rime-show-preedit ' inline)
  ;; 默认值
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (defun +rime--posframe-display-content-a (args)
    "给 `rime--posframe-display-content' 传入的字符串加一个全角空格，以解决 `posframe' 偶尔吃字的问题。"
    (cl-destructuring-bind (content) args
      (let ((newresult (if (string-blank-p content)
                           content
                         (concat content "　"))))
        (list newresult))))

  (if (fboundp 'rime--posframe-display-content)
      (advice-add 'rime--posframe-display-content
                  :filter-args
                  #'+rime--posframe-display-content-a)
    (error "Function `rime--posframe-display-content' is not available."))
   )

(use-package quick-sdcv
  :ensure t
  :bind (("C-c D" . quick-sdcv-search-at-point)
         ("C-c d" . quick-sdcv-search-input))
  
  :custom
  (quick-sdcv-dictionary-data-dir "~/Ingrediant/dictionary/")
  (quick-sdcv-unique-buffers t)
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))

;; 推荐组合：auth-source + pass 的桥接 + 好用的界面
(use-package auth-source
  :ensure t
  :config
  (add-to-list 'auth-sources 'password-store))   ; 关键这行

(use-package auth-source-pass
  :ensure t
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package pass
  :ensure t
  :bind ("C-c p p" . pass))          ; 打开 pass 浏览器模式

(use-package password-store-menu
  :ensure t
  :bind ("C-c P" . password-store-menu))

(provide 'post-init)
;;; post-init.el ends here
