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
   ("C-<"     . mc/mark-prev-like-this)
   ;; 选中一个词后按这个 → 全缓冲区相同词都加光标
   ("C-c C->" . mc/mark-all-like-this)

   ;; 选中多行后 → 每行都出现光标（最实用的批量编辑）
   ("C-S-c C-S-c" . mc/edit-lines)

   ;; 其他常用（可选）
   ("M-<down-mouse-1>" . mc/add-cursor-on-click) ; 鼠标点哪哪出现光标（很爽）
   ("C-!"          . mc/mark-all-dwim) ; 智能全选（有区域就选区域，没区域就选全部）
   )

  :config
  ;; 建议：让这些常用命令在 mc 模式下也能正常工作
  (add-to-list 'mc/cmds-to-run-for-all 'forward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-char)
  (add-to-list 'mc/cmds-to-run-for-all 'delete-char)
  (add-to-list 'mc/cmds-to-run-for-all 'backward-delete-char)
  )


;; 类似包：undo-tree
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
;;; the RSI mode and god mode
(use-package god-mode
  :defer 0.1
  :init
  (setq god-mode-enable-function-key-translation nil)

  :config
;;  (god-mode-all)
  (global-set-key (kbd "<escape>") #'god-local-mode)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  ;;    (setq god-exempt-major-modes nil)
  ;;  (setq god-exempt-predicates nil)

    
  (setq god-mode-alist
        '((nil . "C-")
          ("g" . "M-")
          ("z" . "C-M-")
          ))
  ;;    (define-key god-local-mode-map (kbd "V") #'scroll-down-command)

  (define-key god-local-mode-map (kbd ".") #'repeat)
    
  (define-key god-local-mode-map (kbd "'") #'avy-goto-char-timer)
  (define-key god-local-mode-map (kbd ";") #'my/goto-line-or-end)
    




  (define-key god-local-mode-map (kbd "S-<backspace>") #'kill-whole-line)

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
         ("C-'" . avy-goto-char-timer)
         ("C-." . avy-goto-char-in-line)
         ("C-;" . my/goto-line-or-end)
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

  (defun my/goto-line-or-end (arg)
    "Without prefix arg: use avy-goto-line.
  With prefix arg (C-u): go to end of buffer."
    (interactive "P")
    (if arg
        (avy-goto-end-of-line)
      (avy-goto-line)))

  (global-set-key (kbd "C-;") #'my/goto-line-or-end)

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

(provide 'my-prog-mode)

;;; my-prog-mode.el ends here
