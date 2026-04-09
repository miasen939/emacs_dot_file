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
(setq display-line-numbers-type 'relative)  ;显示相对行号
(global-display-line-numbers-mode 1)

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

(require ' my-emacs-ricing)



(require ' my-prog-mode)
(require ' my-org-mode)

(require ' my-autocomplete)

(require ' my-ebook-reader)

(require ' my-IME)

(require ' my-helper)

(require ' my-DevOps)

;; (require ' my-coding-config)
;; 
(require ' my-windows-manage)
;; 
(require ' my-file-management)

;;(require ' my-dwim)

(require ' my-living-in-emacs)

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

;; ============================================================
;; 前置：确保已安装系统级 LSP server
;;   C/C++:  sudo apt install clangd   (或 brew install llvm)
;;   Rust:   rustup component add rust-analyzer
;;   Python: pip install python-lsp-server  (pylsp)
;;           或 pip install pyright + npm i -g pyright
;; Elisp 不需要额外安装
;; ============================================================

;; ── eglot 基础设置 ──────────────────────────────────────────
;; (use-package eglot
;;   :ensure t
;; 
;;   :custom
;;   ;; 自动关闭没有 buffer 关联的 LSP server
;;   (eglot-autoshutdown t)
;;   ;; 不在 echo area 显示文档（改用 eldoc-box 或 hover）
;;   (eglot-echo-area-use-multiline-p nil)
;;   ;; 提升补全响应速度
;;   (eglot-events-buffer-size 0)
;; 
;;   :config
;;   ;; ── C/C++ ──────────────────────────────────────────────────
;;   ;; clangd 额外参数：开启后台索引、补全时带参数占位符
;;   (add-to-list 'eglot-server-programs
;;                '((c-mode c-ts-mode c++-mode c++-ts-mode)
;;                  . ("clangd"
;;                     "--background-index"
;;                     "--clang-tidy"
;;                     "--completion-style=detailed"
;;                     "--header-insertion=never")))
;; 
;;   ;; ── Rust ───────────────────────────────────────────────────
;;   ;; rust-analyzer 通常 eglot 自动识别，这里显式写出以便加参数
;;   (add-to-list 'eglot-server-programs
;;                '((rust-mode rust-ts-mode)
;;                  . ("rust-analyzer"
;;                     :initializationOptions
;;                     (:checkOnSave (:command "clippy")
;;                                   :cargo (:allFeatures t)))))
;; 
;;   ;; ── Python ─────────────────────────────────────────────────
;;   ;; 优先用 pylsp；如果你用 pyright 把下面注释互换即可
;;   (add-to-list 'eglot-server-programs
;;                '((python-mode python-ts-mode)
;;                  . ("pylsp")))
;;   ;; 用 pyright 则改成：
;;   ;; '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
;; 
;;   ;; ── Elisp ──────────────────────────────────────────────────
;;   ;; Elisp 不走 eglot，用内置方案即可（见下方单独配置）
;; 
;;   :hook
;;   ;; tree-sitter 版本的 mode（推荐）
;;   (c-ts-mode     . eglot-ensure)
;;   (c++-ts-mode   . eglot-ensure)
;;   (rust-ts-mode  . eglot-ensure)
;;   (python-ts-mode . eglot-ensure)
;;   ;; 非 tree-sitter 版本兜底（没开 treesit-auto 时生效）
;;   (c-mode        . eglot-ensure)
;;   (c++-mode      . eglot-ensure)
;;   (rust-mode     . eglot-ensure)
;;   (python-mode   . eglot-ensure))
;; 
;; 
;; ;; ── Elisp：不用 eglot，用内置工具 ──────────────────────────
;; (use-package elisp-mode
;;   :ensure nil  ;; 内置，不需要安装
;;   :hook
;;   (emacs-lisp-mode . eldoc-mode)     ;; 函数签名提示
;;   (emacs-lisp-mode . flymake-mode))
;; 实时语法检查

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (:map rustic-mode-map
              ;;("C-c C-c" . rustic-compile)
              )   
  :hook (rustic-mode . eglot-ensure)
  :config
  ;; 使用 eglot 而非 lsp-mode（更輕量）
  (setq rustic-lsp-server 'eglot)

  ;; 可選：保存時自動 format（需要 rustfmt 已安裝）
  (setq rustic-format-on-save t)

  ;; 可選：讓 tree-sitter 負責語法高亮（但 rustic 自己的 font-lock 也很好）
  ;; 如果你想要 tree-sitter hl，可以加這兩行：
  ;; (add-hook 'rustic-mode-hook #'treesit-font-lock-recompute-features)
  ;; 但大多數人覺得 rustic 自帶的高亮已經夠用了

  (setq lsp-rust-server 'lsp-mode)
  (setq lsp-rust-server 'rust-analyzer)
  )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-rust-analyzer-display-inlay-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  :hook (rustic-mode . lsp))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-display-errors-delay 0.1))

;; 实验功能
;;   (defvar my-prefix-overlays nil)
;; 
;;   (defun my-prefix-preview-cleanup ()
;;     (mapc #'delete-overlay my-prefix-overlays)
;;     (setq my-prefix-overlays nil))
;; 
;;   (defun my-prefix-overlay-at (pos char-str face-bg)
;;     (when (and (>= pos (point-min)) (< pos (point-max)))
;;       (let ((ov (make-overlay pos (1+ pos))))
;;         (overlay-put ov 'display
;;                      (propertize char-str 'face
;;                                  `(:background ,face-bg :foreground "white" :weight bold)))
;;         (push ov my-prefix-overlays))))
;; 
;;   (defun my-prefix-preview-maybe ()
;;     (let ((arg (prefix-numeric-value prefix-arg)))
;;       (if (and prefix-arg (numberp arg) (not (= arg 1)))
;;           (progn
;;             (my-prefix-preview-cleanup)
;;             (my-prefix-overlay-at
;;              (save-excursion (ignore-errors (forward-char arg)) (point))
;;              "▶" "#e67e00")
;;             (my-prefix-overlay-at
;;              (save-excursion (ignore-errors (backward-char arg)) (point))
;;              "◀" "#0072e6"))
;;         (my-prefix-preview-cleanup))))
;; 
;;   (add-hook 'post-command-hook #'my-prefix-preview-maybe)
;; 
;; ;; avy char 0
;;   (defvar my-alpha-jump-overlays nil)
;; 
;;   (defun my-alpha-jump-cleanup ()
;;     (mapc #'delete-overlay my-alpha-jump-overlays)
;;     (setq my-alpha-jump-overlays nil))
;; 
;;   (defun my-alpha-jump ()
;;     (interactive)
;;     (my-alpha-jump-cleanup)
;;     (let* ((cur (point))
;;            (bol (save-excursion (beginning-of-line) (point)))
;;            (eol (save-excursion (end-of-line) (point)))
;;            (char-map '()))
;;       (save-excursion
;;         (goto-char bol)
;;         (while (<= (point) eol)
;;           (let* ((pos (point))
;;                  (dist (- pos cur)))
;;             (when (and (not (= dist 0))
;;                        (>= dist -26)
;;                        (<= dist 26))
;;               (let* ((letter (if (> dist 0)
;;                                  (string (+ ?a (1- dist)))
;;                                (string (+ ?A (1- (abs dist))))))
;;                      (ov (make-overlay pos (1+ pos))))
;;                 ;; avy 的样式：用 avy-lead-face，完全覆盖原字符
;;                 (overlay-put ov 'display
;;                              (propertize letter 'face
;;                                          (if (> dist 0)
;;                                              'avy-lead-face
;;                                            'avy-lead-face-0)))
;;                 (push ov my-alpha-jump-overlays)
;;                 (push (cons (string-to-char letter) pos) char-map))))
;;           (forward-char 1)))
;;       ;; avy 风格：dimming 背景其余部分
;;       (let ((avy-background t))
;;         (when (bound-and-true-p avy-background)
;;           (let ((ov (make-overlay bol eol)))
;;             (overlay-put ov 'face 'avy-background-face)
;;             (push ov my-alpha-jump-overlays))))
;;       (let ((key (read-key)))
;;         (my-alpha-jump-cleanup)
;;         (let ((target (cdr (assoc key char-map))))
;;           (if target
;;               (goto-char target)
;;             (message "No match.")
;;             (push key unread-command-events))))))
;; 
;;   (global-set-key (kbd "C-;") #'my-alpha-jump)

(provide 'post-init)
;;; post-init.el ends here
