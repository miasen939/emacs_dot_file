;;; post-init.el --- Emacs Configuration -*- no-byte-compile: t; lexical-binding: t; -*-
 ;; (use-package benchmark-init
 ;; :ensure t
 ;; :config
 ;; (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

(setq recent-keys-length 1000)          ;C-h l 保存的历史快捷键数量

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'my-dashboard)
;;(load "~/.emacs.d/lisp/my-dash.el")

(require ' my-org-mode)

(require ' my-ebook-reader)

(use-package emacs
  :bind (("M-o" . other-window)
         ("s-o" . other-frame))
  :hook (after-make-frame-functions . (lambda (frame)
                                        (select-frame frame)
                                        (toggle-frame-maximized))))

;; steal from system crafter
(setq large-file-warning-threshold nil)
(column-number-mode 1)

(global-subword-mode 1)

;; (use-package hungry-delete
;; :config
;; (global-hungry-delete-mode))



(defun my/backward-kill-word ()
  "中英混排友好的向后删词，其余行为与默认 C-backspace 完全一致。"
  (interactive)
  (let ((ch (char-before)))
    (if (and ch (string-match-p "[\\u3000-\\u9fff\\uff00-\\uffef]" (string ch)))
        ;; 只有光标前是中日文时，删连续中日字符
        (delete-region (point)
                       (save-excursion
                         (while (and (not (bobp))
                                     (string-match-p "[\\u3000-\\u9fff\\uff00-\\uffef]"
                                                     (string (char-before))))
                           (backward-char))
                         (point)))
      ;; 其他一切交给原生            
      (backward-kill-word 1))))

(global-set-key (kbd "C-<backspace>") #'my/backward-kill-word)

(use-package beacon
  :defer 0.5
  :config
  (beacon-mode 1)
  ;; 可选配置
  (setq beacon-color "#4a5060")      
  (setq beacon-size 20)              
  (setq beacon-blink-duration 0.3))

;; 翻页保留5行重叠，不显得太跳
(setq next-screen-context-lines 5)

;; 滚动时距边缘保留3行
(setq scroll-margin 3)

;; 避免翻页时重新居中
(setq scroll-conservatively 101)

;; 翻页时光标跟着走，不乱跳
(setq scroll-preserve-screen-position t)
;; other-window 保持一致
(setq other-window-scroll-default nil)
(setq scroll-other-window-lines 3)
;; 像素级平滑滚动
(pixel-scroll-precision-mode 1)

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
                        :height 130          
                        :weight 'normal
                        :family "DejaVu Sans mono")
    
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family "Sarasa Fixed SC"
                                   )))))

(use-package valign
:hook (org-mode . valign-mode))         ;解决像素级对齐，让org sheet可以用中日文也对齐了

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
  :init (doom-modeline-mode 1)    )



;; 备用的其他主题配置
;; (use-package ef-themes
;;  :demand t
;;  :bind (("<f5>" . modus-themes-rotate)
;;         ("C-<f5>" . modus-themes-select)
;;         ("M-<f5>" . modus-themes-load-random))
;;  :init
;;  (setq modus-themes-mixed-fonts t
;;        modus-themes-italic-constructs t)
;;  :config
;;  (setq ef-owl-palette-overrides
;;        '((bg-region "#1a3f4a")))
;;  
;;  (ef-themes-load-theme 'ef-owl))

;; (use-package mood-line
;; :hook (after-init . mood-line-mode))

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

(use-package multiple-cursors
  :ensure t
  :bind
  (;; 最常用操作：选中词 → 连续按这个键添加相似光标
   ("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-prev-like-this)
   ;; 选中一个词后按这个 → 全缓冲区相同词都加光标
   ("C-c C->" . mc/mark-all-like-this)

   ;; 选中多行后 → 每行都出现光标（最实用的批量编辑）
   ("C-S-c C-S-c" . mc/edit-lines)

   ;; 其他常用（可选）
   ("M-<down-mouse-1>" . mc/add-cursor-on-click)   ; 鼠标点哪哪出现光标（很爽）
   ("C-!"          . mc/mark-all-dwim)            ; 智能全选（有区域就选区域，没区域就选全部）
   )

  :config
  ;; 建议：让这些常用命令在 mc 模式下也能正常工作
  (add-to-list 'mc/cmds-to-run-for-all 'forward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'delete-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-delete-char)
  )



(use-package undo-fu
  :init
  (global-unset-key (kbd "C-z"))
  :bind
  ("C-/"   . undo-fu-only-undo)
  ("C-S-/" . undo-fu-only-redo)
  ("C-z"   . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo)
  )

(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-strong-limit (* 16 1024 1024))
  (setq undo-fu-session-compression 'gz)
  (undo-fu-session-global-mode))

;; 关闭 小于号自动补全大于号
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?<) t (electric-pair-default-inhibit c))))

(use-package yasnippet
  :bind(("C-c p TAB" . yas-expand)
         ("C-c p i" . yas-insert-snippet)
         ("C-c p n" . yas-new-snippet)
         ("C-c p v" . yas-visit-snippet-file))
  
  :hook
  ;; 在常用模式下启用
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode)
   (org-mode  . yas-minor-mode))
  :config
  ;; 自定义 snippets 目录（与内置目录共存）
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"           ; 个人自定义 snippets
          yasnippet-snippets-dir))         ; 官方 snippet 集合（见下方）

  ;; 启动时加载所有 snippets
  (yas-reload-all)

  ;; 展开时不发出提示音
  (setq yas-verbosity 1)

  ;; 允许在任意位置嵌套展开
  (setq yas-triggers-in-field t))


;; 2. 安装官方 snippet 集合（强烈推荐）
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)



;; 4. 与 consult / vertico 集成（可选，用于搜索 snippet）
(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind (("C-c y" . consult-yasnipp )
         ))

(use-package avy
    :bind (
  ;;         ("C-." . avy-goto-char-timer)
                                          ;     ("C-。". avy-goto-char-timer)
           ("C-;" . avy-goto-line)
                                          ;           ("C-u C-;" . avy-goto-word-0)
           ("M-g w" . avy-goto-word-0)
           ("M-g W" . avy-goto-char)
           ("M-g 2" . avy-goto-char-2)
           ("M-g c" . avy-goto-char-timer)
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
    ;;(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
    )
(use-package ace-pinyin
  :ensure t
  :after avy
  :config
  (setq ace-pinyin-use-avy t)           ;; 明确用 avy 后端
  (ace-pinyin-global-mode +1))          ;; 全局开启

  (use-package ace-window
    :bind ("C-x o" . ace-window)
    :custom
    (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

  (use-package amx
    :defer 0.2
    :config
    (amx-mode 1))


  (use-package expreg
    :bind( ("C-=" . expreg-expand)
           ("C--" . expreg-contract)))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

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

(use-package yasnippet-capf
  :ensure t
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
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
         ("C-c p l" . cape-line))
  :config
  )

;; (use-package embark
;;   :bind (("C-," . embark-act)
;;          ;;("C-;" . embark-dwim)
;;          ("C-h B" . embark-bindings)
;;          :map minibuffer-local-map
;;          ("C-." . embark-act)
;;          ("C-c C-e" . embark-export))
;;   :custom
;;   (embark-quit-after-action nil)
;;   (prefix-help-command #'embark-prefix-help-command)
;;   :init
;;   ;; Which-key 集成
;;   (defun embark-which-key-indicator ()
;;     (lambda (&optional keymap targets prefix)
;;       (if (null keymap)
;;           (which-key--hide-popup-ignore-command)
;;         (which-key--show-keymap
;;          (if (eq (plist-get (car targets) :type) 'embark-become)
;;              "Become"
;;            (format "Act on %s '%s'%s"
;;                    (plist-get (car targets) :type)
;;                    (embark--truncate-target (plist-get (car targets) :target))
;;                    (if (cdr targets) "…" "")))
;;          keymap nil nil 'no-paging)
;;         #'which-key--hide-popup-ignore-command)))
;;   (setq embark-indicators
;;         '(embark-which-key-indicator
;;           embark-highlight-indicator
;;           embark-isearch-highlight-indicator))
;;   (setq embark-action-indicator #'embark-which-key-indicator
;;         embark-become-indicator #'embark-which-key-indicator)
;;   :config
;;   (defun embark--truncate-target (target)
;;     (if (and (stringp target) (> (length target) 30))
;;         (concat (substring target 0 27) "...")
;;       target)))

(use-package embark
  :bind (("C-,"   . embark-act)
         ("C-M-," . embark-dwim)        ; 智能猜测最可能的操作
         ("C-h B" . embark-bindings)    ; 列出所有可用绑定
         :map minibuffer-local-map
         ("C-."   . embark-act)         ; minibuffer 里用 C-.
         ("C-c C-e" . embark-export)
         :map org-mode-map
         ("C-," . embark-act))   ; 导出候选列表
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
      #'which-key--hide-popup-ignore-command))  ; ← 修复：移到外层括号之后
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
  :demand t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  (which-key-sort-order 'which-key-key-order-alpha)
  :init
  (which-key-mode))

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

;; (use-package casual-suite
;; 
;;     :bind (;; 全局入口：在任何支持的 Buffer 中一键唤起
;;            ("M-j" . casual-avy-tmenu)
;;            ("C-o" . casual-editkit-main-tmenu)
;;            ("M-m" ' casual-suite-tmenu)
;;            ;; 如果你习惯在特定模式下使用更直观的快捷键
;;            :map calc-mode-map ("M-m" . casual-calc-tmenu)
;; ;;           :map isearch-mode-map ("M-m" . )
;;            :map dired-mode-map ("M-m" . casual-dired-tmenu)
;;            :map org-mode-map ("M-m" . casual-org-tmenu)
;;            :map org-table-fedit-map ("M-m" . casual-org-table-fedit-tmenu)
;;            :map org-agenda-mode-map ("M-m" . casual-agenda-tmenu)
;;            :map ibuffer-mode-map ("M-m" . casual-ibuffer-tmenu)
;;            :map bookmark-bmenu-mode-map ("M-m" . casual-bookmarks-tmenu)
;;            :map calendar-mode-map ("M-m" . casual-calendar-tmenu)
;;            :map compilation-mode-map ("M-m" . casual-compile-tmenu)
;;            ;;           :map eww-mode-map ("M-m" . casual-eww-tmenu)
;;            :map help-mode-map ("M-m" . casual-help-tmenu)
;;            :map Info-mode-map ("M-m" . casual-info-tmenu)
;;            ;;:map re-builder-mode-map ("M-m" . casual-re-builder-tmenu)
;;            ;;  :map shell-mode-map ("M-m" . casual-eshell-tmenu)
;;            :map image-mode-map ("M-m" . casual-image-tmenu)
;;            )
;; 
;;     :config
;;     )

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



(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-cursor "█")
  (setq rime-cursor-face '((t (:foreground "#00ff00"))))
  (setq rime-show-preedit t)
  (setq rime-user-data-dir "~/.config/fcitx/Rime/")
  ;; 默认值
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  
  (defun my/update-cursor-by-ime ()
    "根据当前输入法状态实时更新光标颜色."
    (set-cursor-color (if current-input-method "#a7c080" "#51afef")))

  (add-hook 'post-command-hook #'my/update-cursor-by-ime)
  )

;; 中文英文之间插入空格
(use-package pangu-spacing
  :defer 0.3
  :config
  (global-pangu-spacing-mode +1))

(global-set-key (kbd "C-x C-\\") 'skk-mode)


(use-package ddskk
  :ensure t
  :init
  (setq skk-jisyo "~/.skk-jisyo")
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")

  (setq skk-extra-jisyo-file-list
        '("/usr/share/skk/SKK-JISYO.jinmei"
          "/usr/share/skk/SKK-JISYO.geo"))
  :config
  (setq skk-use-auto-fill t)
  (setq skk-save-jisyo-instantly t))

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

;; (use-package pass
;;   :ensure t
;;   :bind ("C-c p p" . pass))

(use-package password-store-menu
  :ensure t
  :bind ("C-c P" . password-store-menu))

;; (use-package listen)  
;; (use-package emms
;;   :defer t
;;   :config
;;   
;; 
;;   (emms-all)
;;   (setq emms-player-list '(emms-player-mpv)
;;         emms-info-functions '(emms-info-native))
;; 
;;   )

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

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
;;           treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
;;           treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-hide-dot-jj-directory           t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-files-by-mouse-dragging    t
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-project-follow-into-home        nil
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)
;; 
;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)
;; 
;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))
;; 
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))
;; 
;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))


;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)
;; 
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)
;; 
;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)
;; 
;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))
;; 
;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))
;; 
;; (treemacs-start-on-boot)

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
  (popper-echo-mode +1))                ; For echo area hints

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



(use-package bufferfile
  :ensure t
  :commands (bufferfile-copy
             bufferfile-rename
             bufferfile-delete)
  :custom
  ;; If non-nil, display messages during file renaming operations
  (bufferfile-verbose nil)

  ;; If non-nil, enable using version control (VC) when available
  (bufferfile-use-vc nil)

  ;; Specifies the action taken after deleting a file and killing its buffer.
  (bufferfile-delete-switch-to 'parent-directory))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  ("C-x C-d" . dirvish))

(provide 'post-init)
;;; post-init.el ends here
