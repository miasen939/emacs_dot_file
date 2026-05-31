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

  ;; 这一部分似乎没有机能
  (add-hook 'prog-mode-hook #'vundo-popup-mode)
  (add-hook 'text-mode-hook #'vundo-popup-mode)
  (add-hook 'org-mode-hook  #'vundo-popup-mode)
  )
;; 使用方法
;; fbnp ae w lr m d RET q/C-g


;;; origami 折り紙
(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode)
  :bind
  (:map origami-mode-map
        ("C-c f t" . origami-toggle-node)        ;; 折叠/展开当前节点
        ("C-c f a" . origami-toggle-all-nodes)   ;; 折叠/展开全部
        ("C-c f o" . origami-open-node)          ;; 展开当前
        ("C-c f c" . origami-close-node)         ;; 折叠当前
        ("C-c f n" . origami-next-fold)          ;; 跳到下一个折叠点
        ("C-c f p" . origami-previous-fold)      ;; 跳到上一个折叠点
        ("C-c f r" . origami-reset)))            ;; 重置所有折叠状态

  ;; origami + tree-sitter 集成


;;; god-mode
;;; the RSI and god mode
(use-package god-mode
  :defer 0.1
  :init
  (setq god-mode-enable-function-key-translation nil)

  :config
  ;;  (god-mode-all)
  (which-key-enable-god-mode-support)
  (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1)))
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  ;;    (setq god-exempt-major-modes nil)
  ;;  (setq god-exempt-predicates nil)

    
  (setq god-mode-alist
        '((nil . "C-")
          ("g" . "M-")
          ("q" . "C-M-")
          ))
  ;;    (define-key god-local-mode-map (kbd "V") #'scroll-down-command)

  (define-key god-local-mode-map (kbd "z") #'repeat)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)

  ;; === 针对 emacs-rime 的智能输入法保存与恢复 ===
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
  (add-hook 'god-mode-disabled-hook #'my-god-restore-ime)


  ;; (add-hook 'magit-mode-hook #'god-local-mode-pause)
  ;; (add-hook 'magit-mode-hook (lambda () (god-local-mode -1)))

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
  )
;; 估计因为 C-s 和 vertico没做连协，所以这些minibuffer页面用不了god mode
;; 一些特殊的buffer，比如dired、help、ibuffer、magit里，godmode的行为可能会有点奇怪
;; rime和skk和god mode之间有奇怪的交互
;; 我认为要不magit和大部分类似的特殊buffer就暂时先不用god mode了
;; G q Q 等按键还没用上
;; c-h k 有点小问题
;; multiple-cursors 有点小问题

;;; avy

(use-package avy
  :bind (
         ("C-;" . avy-goto-char-timer)
         ("C-." . avy-goto-char-in-line)
         ("C-'" . avy-goto-line)
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
  
;;; avy 还有很大的调整空间，如果能支持中文（我的微软双拼）和日文就好了

(use-package expreg
  :bind( ("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package mwim
    :ensure t
    :bind
    ("C-a" . mwim-beginning-of-code-or-line)
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


;; (use-package meow
;;   :ensure t
;;   :demand t
;;   :init
;;     (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;; 
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("h" . meow-left)
;;      '("l" . meow-right)
;;      '("<escape>" . ignore))
;; 
;;     (meow-leader-define-key
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
;; 
;; 
;;    (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore))
;;     )
;; 
;;     
;;   :config
;;   (meow-setup)
;;   ;;(meow-global-mode 1)
;;   )

;; (use-package key-chord
;;   :demand t
;;   :config
;;   (key-chord-mode 1)
;;   (key-chord-define-global ",." "<>\C-b")
;;   (key-chord-define-global "dd" 'kill-whole-line)
;;   (key-chord-define-global "ww" 'kmacro-start-macro-or-insert-counter)
;;   (key-chord-define-global "ee" 'kmacro-end-or-call-macro)
;;   (key-chord-define-global "jk" 'avy-goto-char-2)
;;   )
;; 最适合绑定标点符号组合

;; (keymap-set global-map
;;             "C-c a"
;;             #'org-agenda)

;; (defun my/smart-kill-region ()
;;   "如果有选区则 `'kill-region'，否则删除前一个字符."
;;   (interactive)
;;   (if (use-region-p)
;;       (kill-region (region-beginning) (region-end))
;;     (delete-char -1)))
;; 
;; (global-set-key (kbd "C-w") #'my/smart-kill-region)

(use-package crux
  :demand t
  :config
  ;;(global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key (kbd "C-k") #'crux-smart-kill-line)
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
;; move beginning
;; crux-cleanup-buffer-or-region
;; transpose windows
;; insert date/time
;; join line
;; 

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

(provide 'my-prog-mode)

;;; my-prog-mode.el ends here
