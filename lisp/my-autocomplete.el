;;; my-autocomplete.el --- auto compeletion -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(setq enable-recursive-minibuffers t)

;; 关闭 小于号自动补全大于号
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?<) t (electric-pair-default-inhibit c))))

(use-package yasnippet
  :bind(
        ;; ("C-c p TAB" . yas-expand)
        ;; ("C-c p i" . yas-insert-snippet)
        ;; ("C-c p n" . yas-new-snippet)
        ;; ("C-c p v" . yas-visit-snippet-file)
        )
  
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

  ;; 展开时不发出提示音
  (setq yas-verbosity 1)

  ;; 允许在任意位置嵌套展开
(setq enable-recursive-minibuffers t)  (setq yas-triggers-in-field t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  )

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind (("C-c y" . consult-yasnippet)
         ))

(use-package corfu
  :hook (after-init . global-corfu-mode)
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
  :bind (
         ;; ("C-c p p" . completion-at-point)
         ;; ("C-c p d" . cape-dabbrev)
         ;; ("C-c p f" . cape-file)
         ;; ("C-c p k" . cape-keyword)
         ;; ("C-c p s" . cape-symbol)
         ;; ("C-c p a" . cape-abbrev)
         ;; ("C-c p l" . cape-line)
         )
  )

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              )
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 15)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  (setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

  (setq read-extended-command-predicate #'command-completion-default-include-p)
  )

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))

  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-pcm-leading-wildcard t)
  )

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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


;; 优化 dired 协作
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)   ; 進入目錄/選定檔案
         ("DEL" . vertico-directory-delete-char)   ; 在路徑末端時整段刪掉一個目錄層級,而非單一字元
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)   ; 自動清掉被"蓋掉"的舊路徑殘骸
  )

(use-package consult-dir
  :ensure t
  :bind (("C-x C-r" . consult-dir)
         :map vertico-map
         ("C-x C-r" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))



;; (use-package embark
;;   :bind (("C-,"   . embark-act)
;;          ("C-M-," . embark-dwim)        ; 智能猜测最可能的操作
;;          ("C-h B" . embark-bindings)    ; 列出所有可用绑定
;;          :map minibuffer-local-map
;;          ("C-."   . embark-act)         ; minibuffer 里用 C-.
;;          ("C-c C-e" . embark-export)
;;          :map org-mode-map
;;          ("C-," . embark-act))          ; 导出候选列表
;;   :custom
;;   (embark-quit-after-action nil)        ; 执行 action 后不退出，方便连续操作
;;   (prefix-help-command #'embark-prefix-help-command)
;;   :init
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
;;          keymap nil nil 'no-paging))
;;       #'which-key--hide-popup-ignore-command)) ; ← 修复：移到外层括号之后
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
;;
(setq embark-help-key "?")
;; (setq embark-indicators
;;       '(embark-minimal-indicator   ; 只在 echo area 顯示目前目標的類型,不彈鍵位表
;;         embark-highlight-indicator ; 目標本身會被高亮,方便確認選對東西
;;         embark-isearch-highlight-indicator))


;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t
;;   :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package general
  :demand t
  :config
  (define-prefix-command 'my-semicolon-map)
  (keymap-global-set ";" 'my-semicolon-map)
  (keymap-global-set "；" 'my-semicolon-map)
  
  (general-define-key
   :keymaps 'my-semicolon-map
   "SPC" (lambda () (interactive) (insert ";"))

   "a" #'embark-act
   "d" #'embark-dwim
   "A" #'embark-act-all
   "s" #'embark-select
   "c" #'embark-collect
   "l" #'embark-live
   "e" #'embark-export
   "b" #'embark-bindings
   "B" #'embark-become

   ";" #'meow-reverse
   "r" #'consult-bookmark
   )
  
  (setq prefix-help-command #'embark-prefix-help-command) ;; 這行不放進 leader,直接設全域變數:任何 prefix key(如 C-x)按完後按 C-h,

  )
;; 會跳出 completing-read 讓你直接在裡面搜尋並執行,而不是死板的 help buffer



(provide 'my-autocomplete)

;;; my-autocomplete.el ends here
