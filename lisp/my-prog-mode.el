;;; my-prog-mode.el --- programming config -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 编程相关的配置，日后需要继续拆分
;;

;;; Code:
;; 类似包：iedit
(use-package multiple-cursors
  :ensure t
  :bind
  (;; 最常用操作：选中词 → 连续按这个键添加相似光标
   ("C->"     . mc/mark-next-like-this)
;;   ("C-<"     . mc/mark-prev-like-this)
   ;; 选中一个词后按这个 → 全缓冲区相同词都加光标
   ("C-c C->" . mc/mark-all-like-this)

   ;; 选中多行后 → 每行都出现光标（最实用的批量编辑）
   ("C-S-c C-S-c" . mc/edit-lines)

   ;; 其他常用（可选）
   ("M-<down-mouse-1>" . mc/add-cursor-on-click) ; 鼠标点哪哪出现光标（很爽）
   ;;("C-!" . mc/mark-all-dwim) ; 智能全选（有区域就选区域，没区域就选全部）
   )
  :config
  ;; 建议：让这些常用命令在 mc 模式下也能正常工作
  (add-to-list 'mc/cmds-to-run-for-all 'forward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'delete-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-delete-char)
  )


;; 类似包：undo-tree
  ;; (use-package undo-fu
  ;;   :init
  ;;   (global-unset-key (kbd "C-z"))
  ;;   :bind
  ;;   ("C-/"   . undo-fu-only-undo)
  ;;   ("C-S-/" . undo-fu-only-redo)
  ;;   ("C-z"   . undo-fu-only-undo)
  ;;   ("C-S-z" . undo-fu-only-redo)
  ;;   )
  ;; 
  ;; (use-package undo-fu-session
  ;;   :ensure t
  ;;   :config
  ;;   (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  ;;   (setq undo-strong-limit (* 16 1024 1024))
  ;;   (setq undo-fu-session-compression 'gz)
  ;;   (undo-fu-session-global-mode))


(use-package vundo
  :ensure t
  ;; 只在需要时加载，不影响启动速度
  :commands (vundo)
  ;; 快捷键绑定
  :bind
  ("C-x u" . vundo)   ; 覆盖默认的 undo，改为打开可视化树
  :config
  ;; 使用 Unicode 字符画树（更好看）
  ;; 需要字体支持，推荐 FiraCode Nerd Font 或 Noto Sans
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;; (setq vundo-glyph-alist vundo-ascii-symbols)
  (set-face-attribute 'vundo-default nil :family "FiraCode Nerd Font Mono")
  ;; (set-face-attribute 'vundo-default nil :family "Noto Sans Mono")
  (setq vundo-window-max-height 5)
  (setq vundo-compact-display t)  ; t = 紧凑，nil = 完整
  (setq vundo-roll-back-on-quit nil)
  )
;; 使用方法
;; fbnp ae w lr m d RET q/C-g


;;; origami 折り紙
;; (use-package origami
;;   :ensure t
;;   :hook (prog-mode . origami-mode)
;;   :bind
;;   (:map origami-mode-map
;;         ("C-c f t" . origami-toggle-node)        ;; 折叠/展开当前节点
;;         ("C-c f a" . origami-toggle-all-nodes)   ;; 折叠/展开全部
;;         ("C-c f o" . origami-open-node)          ;; 展开当前
;;         ("C-c f c" . origami-close-node)         ;; 折叠当前
;;         ("C-c f n" . origami-next-fold)          ;; 跳到下一个折叠点
;;         ("C-c f p" . origami-previous-fold)      ;; 跳到上一个折叠点
;;         ("C-c f r" . origami-reset)))            ;; 重置所有折叠状态

  ;; origami + tree-sitter 集成



;;; avy

(use-package avy
  :bind (
         ("C-;" . avy-goto-line)
         ;; 
         ;; ("C-'" . avy-goto-line)
                                        ;           ("C-u C-;" . avy-goto-word-0)
         ;; ("M-g w" . avy-goto-word-0)
         ;; ("M-g W" . avy-goto-char)
         ;; ("M-g 2" . avy-goto-char-2)
         ;; ("M-g c" . avy-goto-char-timer)
         ;; ("M-g k" . avy-kill-region)
         ;; ("M-g K" . avy-kill-ring-save-region)
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

(use-package flash
  :bind ("C-." . flash-jump)
  )
;;; avy 如果能支持中文（我的微软双拼）和日文就好了

(use-package expreg
  :bind( ("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package mwim
    :ensure t
    :bind
    ;; ("C-a" . mwim-beginning-of-code-or-line)
    ("C-e" . mwim-end-of-code-or-line))

(use-package rainbow-delimiters
    :hook
    ;; 最常用写法：在所有编程模式下自动启用（强烈推荐）
    (prog-mode . rainbow-delimiters-mode)

    ;; 可选：如果你还想在某些非 prog-mode 的地方也启用，比如 REPL、org-src 等
    ;; (emacs-lisp-mode . rainbow-delimiters-mode)
    ;; (clojure-mode  . rainbow-delimiters-mode)
    ;; (inferior-ess-mode . rainbow-delimiters-mode)   ;; R 的 REPL
    )

(use-package symbol-overlay
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook (prog-mode . symbol-overlay-mode))

  (use-package colorful-mode
    ;; :diminish
    ;; :ensure t ; Optional
    :demand t
    :custom
    (colorful-use-prefix t)
    (colorful-only-strings 'only-prog)
    (css-fontify-colors nil)
    :config
    (global-colorful-mode t)
    (add-to-list 'global-colorful-modes 'helpful-mode)
    )


;; TODO 结构化编辑
;; (use-package paredit
;;     :ensure t
;;     :hook
;;     (emacs-lisp-mode . paredit-mode)
;;     (lisp-mode       . paredit-mode)
;;     (scheme-mode     . paredit-mode))
;; 
;;   ;; 其他语言用 smartparens
;;   (use-package smartparens
;;     :ensure t
;;     :hook
;;     (rust-ts-mode   . smartparens-mode)
;;     (python-ts-mode . smartparens-mode)
;;     (c-ts-mode      . smartparens-mode)
;;     :config
;;     (require 'smartparens-config))

;;; TODO The ai support gptel



(use-package crux
  :demand t
  :config
  ;;(global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)
  ;;(global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key (kbd "C-k") #'crux-smart-kill-line)
  (global-set-key (kbd "C-a") #'crux-move-beginning-of-line)
  ;; (global-set-key (kbd "C-c o") #'crux-open-with)
  ;; (crux-reopen-as-root-mode)
  )
;; 实用函数
;; reopen as root/sudo-edit
;; delete/rename file and buffer
;; duplicate line/region
;; open new line/new line above
;; recentf-find-directory
;; kill other buffer
;; crux-cleanup-buffer-or-region
;; transpose windows
;; insert date/time
;; join line
;; 
;; 我目前觉得，克隆一行，快速选中一行，是很好用的



(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'prot/keyboard-quit-dwim)

;; (use-package evil
;;   :demand t
;;   :init
;;   ;; 在加载 Evil 之前设置（非常重要）
;;   (setq evil-disable-insert-state-bindings t)   ; ← 让 Insert 模式基本使用 Emacs 原生键
;;   (setq evil-want-keybinding nil)               ; 配合 evil-collection 使用
;; 
;;   ;; 其他常用设置
;;   (setq evil-undo-system 'undo-redo)            ; 使用 Emacs 新的 undo 系统（推荐）
;;   (setq evil-want-C-u-scroll t)                 ; C-u 滚动而不是 universal-argument
;;   (setq evil-want-C-i-jump nil)                 ; 如果你想让 C-i 做别的
;;   (setq evil-toggle-key "C-z")                  ; 默认 C-z 切换 Emacs 状态
;; 
;;   :config
;;   (evil-mode 1)
;; 
;;   ;; 让某些模式默认进入 Emacs 状态（可选）
;;   (evil-set-initial-state 'help-mode 'emacs)
;;   (evil-set-initial-state 'helpful-mode 'emacs)
;;   (evil-set-initial-state 'info-mode 'emacs)
;;   ;;(evil-set-initial-state 'dashboard-mode 'emacs)
;;   (evil-set-initial-state 'dired-mode 'emacs)
;; 
;;   (define-key evil-insert-state-map (kbd "C-o") #'evil-execute-in-normal-state)
;; 
;;   ;; 全局 Leader 键（类似 Spacemacs / Doom 的 SPC）
;;   (define-key evil-normal-state-map (kbd "SPC") nil)  ; 清空避免冲突
;;   (define-key evil-visual-state-map (kbd "SPC") nil)
;; 
;;   ;; 示例：常用 Leader 绑定
;;   (defun my/leader-key-setup ()
;;     (define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
;;     (define-key evil-normal-state-map (kbd "SPC f f") 'find-file)
;;     (define-key evil-normal-state-map (kbd "SPC b b") 'switch-to-buffer)
;;     (define-key evil-normal-state-map (kbd "SPC w s") 'split-window-below)
;;     (define-key evil-normal-state-map (kbd "SPC w v") 'split-window-right)
;;     (define-key evil-normal-state-map (kbd "SPC q q") 'save-buffers-kill-terminal))
;; 
;;   )
;; 
;; (use-package evil-escape
;;   :ensure t
;;   :init
;;   (evil-escape-mode 1)
;;   :after evil
;;   :config
;;   (setq-default evil-escape-key-sequnce "df")
;;   (setq-default evil-escape-delay 0.2))
;; ;; 还有一些evil的配套设施，我以后可以体验一下
;; (use-package goto-chg)


;;; the meow
;; (use-package meow
;;   :demand t
;;   :config
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;; 
;;     ;; 旧版 Meow 使用这个函数名
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;; 
;;     (meow-leader-define-key
;;      ;; 在旧版 Meow 中，通过 SPC j / SPC k 调用原来的按键
;;      '("j" . "H-j")
;;      '("k" . "H-k")
;; 
;;      ;; SPC 0-9 作为数字参数
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;; 
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;; 
;;   (meow-setup)
;;   (meow-global-mode 1))

;;; god-mode
;; ;;; the RSI and god mode
(use-package god-mode
  :defer 0.1
  :init
  (setq god-mode-enable-function-key-translation nil)
  ;; (god-mode-all 1)
  :config
  ;;  (god-mode-all)
  (which-key-enable-god-mode-support)
  (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1)))
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)

  ;; (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-mode-all 1)))
  ;; (define-key god-local-mode-map (kbd "i") #'god-mode-all)
  ;;    (setq god-exempt-major-modes nil)
  ;;  (setq god-exempt-predicates nil)

  
  (setq god-mode-alist
        '((nil . "C-")
          ("m" . "M-")
          ("g" . "C-M-")
          ))

  (define-key god-local-mode-map (kbd "z") #'repeat)
  ;; (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  ;; (define-key god-local-mode-map (kbd "]") #'forward-paragraph)

  (defvar my--last-input-method nil
    "记录进入 god-mode 之前最后使用的 input-method。")

  (defun my-god-save-and-disable-ime ()
    "进入 god-mode 时：保存当前输入法状态，然后强制关闭 Rime。"
    (setq my--last-input-method current-input-method) ; 保存当前状态
    (when (and current-input-method
               (fboundp 'deactivate-input-method))
      (deactivate-input-method)))

  (defun my-god-restore-ime ()
    "退出 god-mode 时：恢复之前保存的输入法（主要是 Rime）。"
    (when (and my--last-input-method
               (not current-input-method)) ; 只有当前没开启输入法时才恢复
      (set-input-method my--last-input-method))) ;;
  (add-hook 'god-mode-enabled-hook  #'my-god-save-and-disable-ime)
 ;; (add-hook 'god-mode-disabled-hook #'my-god-restore-ime)

  (custom-set-faces
   '(god-mode-lighter ((t (:inherit error)))))

  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'hollow 'box)))

  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  (add-hook 'read-only-mode-hook
            (lambda () (when buffer-read-only (god-local-mode 1))))

  (defun my-god-disable-on-input-method-activate ()
    "当输入法（Rime）被激活时，自动退出 god-mode。"
    (when god-local-mode
      (god-local-mode -1)))
  (add-hook 'input-method-activate-hook   #'my-god-disable-on-input-method-activate)

  (add-hook 'skk-mode-hook
            (lambda ()
              (if skk-mode
                  (god-local-mode -1)
                (god-local-mode 1))))
    )


;; u 这个按键还没用上，想一个好方法给他安排上

;; 一些特殊的buffer，比如dired、help、ibuffer、magit里，godmode的行为可能会有点奇怪
;; 我认为要不magit和大部分类似的特殊buffer就暂时先不用god mode了
;; G q Q 等按键还没用上
;; multiple-cursors 有点小问题
;; C-m 和C-j 都是newline？
;; M-r 等等很多键我觉得都有点功能多余了 可以用来自定义

(use-package goto-chg
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

(use-package rect  ; built-in
  :ensure nil
  :bind
  (:map rectangle-mark-mode-map  ; C-x r
        ("t" . string-rectangle)
        ("o" . open-rectangle)
        ("c" . clear-rectangle)
        ("n" . rectangle-number-lines)
        ("x" . rectangle-exchange-point-and-mark)
        ("*" . calc-grab-rectangle)
        (":" . calc-grab-sum-down)
        ("_" . calc-grab-sum-across)
        (" " . delete-whitespace-rectangle)))

;; Emacs 29+ 推荐写法
(repeat-mode 1)
(setq repeat-timeout 5)
(setq repeat-exit-key "<escape>")

(keymap-set global-map "C-z" #'repeat)


(provide 'my-prog-mode)

;;; my-prog-mode.el ends here
