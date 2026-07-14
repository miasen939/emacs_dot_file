;;; my-prog-mode.el --- programming config -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 编程相关的配置，日后需要继续拆分
;; nano test more

;;; Code:
;; 类似包：iedit
(use-package multiple-cursors
  :ensure t
  :bind
  (;; 最常用操作：选中词 → 连续按这个键添加相似光标
;;    ("C->"     . mc/mark-next-like-this)
;; ;;   ("C-<"     . mc/mark-prev-like-this)
;;    ;; 选中一个词后按这个 → 全缓冲区相同词都光标
;;    ("C-c C->" . mc/mark-all-like-this)
;; 
;;    ;; 选中多行后 → 每行都出现光标（最实用的批量编辑）
;;    ("C-S-c C-S-c" . mc/edit-lines)

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



;;; the meow

(defun my/disable-emacs-input-method ()
  "Disable Emacs built-in input method if active."
  (when current-input-method
    (toggle-input-method)))

(defun my/meow-disable-input-method-on-normal (state)
  "Disable input method when Meow enters normal`state' STATE."
  
  (when (eq state 'normal)
    (my/disable-emacs-input-method)))
(add-hook 'meow-switch-state-hook
          #'my/meow-disable-input-method-on-normal)
(defun my/meow-insert-and-input-method-on ()
  "Enter Meow insert state and enable Emacs input method."
  (interactive)
  (meow-insert)
  (unless current-input-method
    (toggle-input-method)))
(use-package meow
  :demand t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    ;; 旧版 Meow 使用这个函数名
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))


    (meow-leader-define-key
     ;; 在 Motion 中，通过 SPC j / SPC k 调用原来的按键
     '("j" . "H-j")
     '("k" . "H-k")

     ;; SPC 0-9 作为数字参数
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     )

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     ;; '("Q" . next-buffer)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)

     ;; 自定义按键
     ;; '("/" . isearch-forward)             ;这个可以用来当hydra
     '("\\" . my/meow-insert-and-input-method-on)
     '("<" . beginning-of-buffer)
     '(">" . end-of-buffer)
     '("Q" . next-buffer)
     '("Z" . undo-redo)
     '(":" . execute-extended-command)
     ;; '("C" . undo-redo)
     ;; '("V" . undo-redo)
     ;; '("M" . meow-start-kmacro-or-insert-counter)
     ;; '("F" . meow-start-kmacro-or-insert-counter meow-start-kmacro-or-insert-counter)
     '("N" . meow-end-of-thing)
     '("P" . meow-beginning-of-thing)
     ;; '("S" . org-emphasize)
     '("S" . my/meow-surround)
     '("/" . my/meow-surround)
     
     ))
  (meow-setup)
  (meow-global-mode 1)
  (setq meow-use-clipboard t)
  )
(use-package key-chord
  :demand t
  :config
  (key-chord-mode 1)
  (key-chord-define meow-insert-state-keymap "kj" [escape])
  )
(use-package evil-surround
  :demand t
)
(defvar my/surround-aliases
  '((?b . ?*)                    ; bold
    (?i . ?/)                    ; italic
    (?u . ?_)                    ; underline
    (?s . ?+)                    ; strike-through
    (?c . ?~)                    ; code
    (?v . ?=)                    ; verbatim
    (?m . ?$))                   ; math
  "Aliases used by `my/meow-surround'.")
(defun my/meow-surround (input-char)
  "Surround the active region or symbol at point.

INPUT-CHAR is translated according to `my/surround-aliases',
then handled using evil-surround's delimiter rules."
  (interactive
   (list
    (read-char
     "Surround: [b]old [i]talic [u]nderline [s]trike [c]ode [v]erbatim [m]ath: ")))

  (let* ((char (or (alist-get input-char my/surround-aliases)
                   input-char))
         (bounds
          (cond
           ((use-region-p)
            (cons (region-beginning) (region-end)))
           ((bounds-of-thing-at-point 'symbol))
           (t nil))))

    (if bounds
        (evil-surround-region
         (car bounds)
         (cdr bounds)
         'exclusive
         char)

      ;; 没有选区和 symbol：插入一对空标记。
      (let* ((pair (evil-surround-pair char))
             (left (car pair))
             (right (cdr pair)))
        (insert left right)
        (backward-char (length right))))

    (deactivate-mark)))
;; (defun my/meow-surround (char)
;;   "Surround Meow selection or symbol at point using evil-surround.
;; 
;; If neither exists, insert an empty delimiter pair and put point
;; between them."
;;   (interactive
;;    (list (read-char "Surround with: ")))
;; 
;;   (let ((bounds
;;          (cond
;;           ;; 优先使用 Meow/Emacs 当前选区
;;           ((use-region-p)
;;            (cons (region-beginning) (region-end)))
;; 
;;           ;; 没有选区时，使用光标处 symbol
;;           ((bounds-of-thing-at-point 'symbol))
;; 
;;           ;; 两者都没有
;;           (t nil))))
;; 
;;     (if bounds
;;         ;; 复用 evil-surround 的实际包裹功能
;;         (evil-surround-region
;;          (car bounds)
;;          (cdr bounds)
;;          'exclusive
;;          char)
;; 
;;       ;; 没有可包裹内容时，插入空的一对
;;       (let* ((pair (evil-surround-pair char))
;;              (left (car pair))
;;              (right (cdr pair)))
;;         (insert left right)
;;         (backward-char (length right))))
;; 
;;     ;; 清除 Meow 选区
;;     (when (use-region-p)
;;       (deactivate-mark))))

(use-package wrap-region
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("{-" "-}" "#")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode ruby-mode))))
)
;; 这 meow还得再改
;; 我可能需要数字 + hljk来移动
;; 另一方面是，我可能需要 yy，虽然xy也能执行但是yy按起来更顺手，同理dd d3d
;; motion mode 想法很好，dired和magit都有jk相关的快捷键，怎么把这两个快捷键救回来？
;; meow只给了一个最小可用的配置，很多地方都可以自定义这些我也会慢慢调整
;; keypad很好用，但是还是有点小问题
;; 用 general el 来创建leader key
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

;;(keymap-set global-map "C-z" #'repeat)



(provide 'my-prog-mode)

;;; my-prog-mode.el ends here
