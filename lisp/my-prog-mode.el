;;; My-prog-mode.el --- programming config -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 编程相关的配置，日后需要继续拆分
;; nano test more

;;; Code:
;; (keymap-global-set "C-s" 'isearch-forward)

(setq display-line-numbers-type 'relative)

(dolist (hook '(prog-mode-hook dired-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

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
         ;; ("C-;" . avy-goto-line)
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
  ;; (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  ;;(define-key isearch-mode-map (kbd "C-'") 'avy-isearch)
)

(use-package flash
  :bind ("C-." . flash-jump)
  :custom
  (flash-labels "asdfjkl;ghqwertyuiopzxcvbnm")
  (flash-label-uppercase t)     ; double available labels (a-z + A-Z)
  (flash-multi-window t)
  (flash-autojump t)            ; auto-jump when single match
  (flash-backdrop nil)          ; no dimming
  (flash-rainbow t)             ; colorful labels
  (flash-rainbow-shade 2)       ; 1-9: pastel to dark
  (flash-highlight-matches t)
  (flash-label-position 'overlay)
  (flash-char-jump-labels t)    ; labels on f/t/F/T matches
  (flash-nohlsearch t)          ; clear highlight after jump
  (flash-search-history t)
  (flash-evil-setup t)
  )

(use-package expreg
  :bind( ("C-=" . expreg-expand)
         ("C--" . expreg-ract)
         ("C-o" . expreg-expand)
         ("C-S-O" . expreg-contract)
         )

  :config
  
)





;; (use-package mwim
;;     :ensure t
;;     :bind
;;     ;; ("C-a" . mwim-beginning-of-code-or-line)
;;     ("C-e" . mwim-end-of-code-or-line))

(use-package rainbow-delimiters
    :hook
    ;; 最常用写法：在所有编程模式下自动启用（强烈推荐）
    (prog-mode . rainbow-delimiters-mode)

    ;; 可选：如果你还想在某些非 prog-mode 的地方也启用，比如 REPL、org-src 等
    ;; (emacs-lisp-mode . rainbow-delimiters-mode)
    ;; (clojure-mode  . rainbow-delimiters-mode)
    ;; (inferior-ess-mode . rainbow-delimiters-mode)   ;; R 的 REPL
    )

(;; use-package symbol-overlay
 ;;  :bind (("M-i" . symbol-overlay-put)
 ;;         ("M-n" . symbol-overlay-jump-next)
 ;;         ("M-p" . symbol-overlay-jump-prev)
 ;;         ("M-N" . symbol-overlay-switch-forward)
 ;;         ("M-P" . symbol-overlay-switch-backward)
 ;;         ("M-C" . symbol-overlay-remove-all))
 ;;   :hook (prog-mode . symbol-overlay-mode)
  )

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
  (global-set-key (kbd "C-x C-<backspace>") #'crux-kill-line-backwards)
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


(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap))

;;; ===the meow modeling scheme===




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

;; (keymap-set global-map "C-z" #'repeat)



;; useful evil package
;; (use-package evil-nerd-commenter
;;   :demand t
;;   :config
;;   (evilnc-default-hotkeys))

(use-package evil-matchit)

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))


(use-package kdl-mode)


;; dapmode
;; realgud




(setq repeat-keep-prefix t)
(use-package repeat
  :init
  (repeat-mode 1)
  :config
  (setq  repeat-exit-key "<escape>")
  
  ;; (defvar-keymap my-line-repeat-map
  ;;   :repeat t
  ;;   "n" #'next-line
  ;;   "p" #'previous-line)
  (defvar-keymap my-word-repeat-map
    :repeat t
    "f" #'forward-word
    "b" #'backward-word)
  ;; (defvar-keymap my-char-repeat-map
  ;;   :repeat t
  ;;   "f" #'forward-char
  ;;   "b" #'backward-char)

  (defvar-keymap my-page-repeat-map
    :repeat t
    "v" #'scroll-up-command     ; 向下翻頁 (即 C-v)
    "u" #'scroll-down-command)

  (defvar expreg-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "o" #'expreg-expand)
      (define-key map "u" #'expreg-contract)
      map))

  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map)
  ;; 
  (defvar-keymap my-paren-repeat-map
  :repeat t
  "f" #'forward-sexp             ; 跳到下個括號/運算式 (C-M-f)
  "b" #'backward-sexp            ; 跳到上個括號/運算式 (C-M-b)
  "u" #'backward-up-list         ; 跳出當前括號外層 (C-M-u)
  "d" #'down-list
  "SPC" #'mark-sexp)
)

  
  

(provide 'my-prog-mode)

;;; my-prog-mode.el ends here
