;;; my-evil.el --- Description -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:


;;;; 1. Evil 本体
;; evil-want-keybinding 必须在 evil 加载“之前”设为 nil，
;; 否则 evil 会先自带一套 keybinding，evil-collection 就接不上了。
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil          ; 交给 evil-collection
        evil-want-C-u-scroll t            ; C-u 向上翻半页（Vim 习惯）
        evil-want-C-i-jump nil            ; 避免 TAB 在 org 里被抢
        evil-undo-system 'undo-redo       ; Emacs 28+ 内建
        evil-respect-visual-line-mode t
        evil-split-window-below t
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  ;; 这些 buffer 里保持 Emacs state，不进 Evil
  (dolist (m '(eshell-mode vterm-mode))
    (evil-set-initial-state m 'emacs)))
 
;;;; 2. evil-collection：给 magit / dired / help / vertico 等补 Evil 键位
;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-collection-init))
 
;;;; 3. general.el：SPC 作为 leader
;; 关键点：
;;  - :keymaps 'override 让 SPC 前缀压过各 mode 自己的 map
;;    （dired、magit、help 等 special-mode 里 SPC 本来有绑定）
;;  - :states 不含 insert，所以插入模式下 SPC 仍然是空格
;;  - :global-prefix "M-SPC" 让 insert / emacs state 下也能用 leader
(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer my/leader
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"))
 
;; Elpaca 是异步安装的：必须等 general 装好，下面才能用 my/leader
(elpaca-wait)
 
 
;;;; 辅助函数
(defun my/open-init ()
  "打开 init 文件（LazyVim: <leader>fc）。"
  (interactive)
  (find-file user-init-file))
 
(defun my/kill-other-buffers ()
  "关掉除当前 buffer 外的所有普通 buffer（LazyVim: <leader>bo）。"
  (interactive)
  (dolist (b (buffer-list))
    (unless (or (eq b (current-buffer))
                (string-prefix-p " " (buffer-name b))) ; 跳过内部 buffer
      (kill-buffer b))))
 
(defun my/search-word ()
  "对光标下的符号做 ripgrep（LazyVim: <leader>sw）。"
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol t)))
 
(defun my/grep-cwd ()
  "在当前目录 ripgrep（LazyVim: <leader>sG）。"
  (interactive)
  (consult-ripgrep default-directory))
 
(defun my/toggle-relative-numbers ()
  "切换相对行号（LazyVim: <leader>uL）。"
  (interactive)
  (setq-local display-line-numbers
              (if (eq display-line-numbers 'relative) t 'relative)))
 
(defvar my/--saved-wconf nil)
(defun my/toggle-maximize-window ()
  "最大化 / 还原当前窗口（LazyVim: <leader>wm）。"
  (interactive)
  (if (and my/--saved-wconf (one-window-p))
      (progn (set-window-configuration my/--saved-wconf)
             (setq my/--saved-wconf nil))
    (setq my/--saved-wconf (current-window-configuration))
    (delete-other-windows)))
 
;;;; Leader 键位
(my/leader
  ;; ── 顶层 ──────────────────────────────
  "SPC" '(project-find-file        :wk "find file (root)")
  ","   '(consult-buffer           :wk "switch buffer")
  "/"   '(consult-ripgrep          :wk "grep (root)")
  ":"   '(consult-complex-command  :wk "command history")
  "e"   '(project-dired            :wk "explorer (root)")
  "E"   '(dired-jump               :wk "explorer (cwd)")
  "-"   '(evil-window-split        :wk "split below")
  "|"   '(evil-window-vsplit       :wk "split right")
  "n"   '(view-echo-area-messages  :wk "messages")
  "l"   '(elpaca-manager           :wk "Elpaca")        ; 对应 <leader>l = Lazy
  "L"   '(elpaca-log               :wk "Elpaca log")
  ";" '(execute-extended-command :wk "M-x")

  ;; ── b: buffer ─────────────────────────
  "b"   '(:ignore t                :wk "buffer")
  "bb"  '(mode-line-other-buffer   :wk "other buffer")
  "bd"  '(kill-current-buffer      :wk "delete")
  "bD"  '(kill-buffer-and-window   :wk "delete + window")
  "bo"  '(my/kill-other-buffers    :wk "delete others")
 
  ;; ── c: code（eglot）───────────────────
  "c"   '(:ignore t                :wk "code")
  "ca"  '(eglot-code-actions       :wk "code action")
  "cr"  '(eglot-rename             :wk "rename")
  "cf"  '(eglot-format             :wk "format")
  "cs"  '(consult-imenu            :wk "symbols")
 
  ;; ── f: file / find ────────────────────
  "f"   '(:ignore t                :wk "file/find")
  "ff"  '(project-find-file        :wk "find (root)")
  "fF"  '(find-file                :wk "find (cwd)")
  "fr"  '(consult-recent-file      :wk "recent")
  "fb"  '(consult-buffer           :wk "buffers")
  "fc"  '(my/open-init             :wk "config file")
  "fe"  '(project-dired            :wk "explorer (root)")
  "fE"  '(dired-jump               :wk "explorer (cwd)")
  "ft"  '(ghostel                   :wk "terminal")
 
  ;; ── g: git（magit 替代 lazygit）───────
  "g"   '(:ignore t                :wk "git")
  "gg"  '(magit-status             :wk "status")
  "gs"  '(magit-status             :wk "status")
  "gb"  '(magit-blame-addition     :wk "blame")
  "gl"  '(magit-log-current        :wk "log")
  "gL"  '(magit-log-buffer-file    :wk "log (file)")
  "gd"  '(magit-diff-buffer-file   :wk "diff (file)")
 
  ;; ── s: search（consult 替代 telescope）─
  "s"   '(:ignore t                :wk "search")
  "sg"  '(consult-ripgrep          :wk "grep (root)")
  "sG"  '(my/grep-cwd              :wk "grep (cwd)")
  "sw"  '(my/search-word           :wk "word (root)")
  "sb"  '(consult-line             :wk "buffer lines")
  "sh"  '(describe-symbol          :wk "help")
  "sk"  '(embark-bindings          :wk "keymaps")
  "sm"  '(consult-mark             :wk "marks")
  "sj"  '(evil-show-jumps          :wk "jumplist")
  "sr"  '(query-replace-regexp     :wk "replace")
  "ss"  '(consult-imenu            :wk "symbols")
  "sS"  '(consult-imenu-multi      :wk "symbols (project)")
  "sd"  '(consult-flymake          :wk "diagnostics")
  "sc"  '(consult-complex-command  :wk "command history")
  "sC"  '(execute-extended-command :wk "commands")
  "s\"" '(consult-register         :wk "registers")
  "sq"  '(consult-compile-error    :wk "quickfix")
  "sy"  '(consult-yank-from-kill-ring :wk "kill ring")
  "sM"  '(consult-man              :wk "man pages")
 
  ;; ── u: ui / toggle ────────────────────
  "u"   '(:ignore t                :wk "ui/toggle")
  "us"  '(flyspell-mode            :wk "spelling")
  "uw"  '(visual-line-mode         :wk "wrap")
  "ul"  '(display-line-numbers-mode :wk "line numbers")
  "uL"  '(my/toggle-relative-numbers :wk "relative numbers")
  "ud"  '(flymake-mode             :wk "diagnostics")
  "uh"  '(eglot-inlay-hints-mode   :wk "inlay hints")
  "ur"  '(evil-ex-nohighlight      :wk "clear highlight")
 
  ;; ── w: windows ────────────────────────
  "w"   '(:ignore t                :wk "windows")
  "ww"  '(other-window             :wk "other")
  "wd"  '(delete-window            :wk "delete")
  "w-"  '(evil-window-split        :wk "split below")
  "w|"  '(evil-window-vsplit       :wk "split right")
  "wm"  '(my/toggle-maximize-window :wk "maximize")
  "wh"  '(evil-window-left         :wk "left")
  "wj"  '(evil-window-down         :wk "down")
  "wk"  '(evil-window-up           :wk "up")
  "wl"  '(evil-window-right        :wk "right")
 
  ;; ── x: diagnostics（flymake 替代 trouble）
  "x"   '(:ignore t                :wk "diagnostics")
  "xx"  '(flymake-show-project-diagnostics :wk "project")
  "xX"  '(flymake-show-buffer-diagnostics  :wk "buffer")
 
  ;; ── <tab>: tabs（tab-bar）────────────
  "TAB"     '(:ignore t            :wk "tab")
  "TAB TAB" '(tab-new              :wk "new tab")
  "TAB d"   '(tab-close            :wk "close tab")
  "TAB j"   '(tab-next             :wk "next tab")
  "TAB k"   '(tab-previous         :wk "prev tab")
  "TAB o"   '(tab-close-other      :wk "close others")
 
  ;; ── q: quit ───────────────────────────
  "q"   '(:ignore t                :wk "quit")
  "qq"  '(save-buffers-kill-terminal :wk "quit Emacs")
 
  ;; ── h: help（LazyVim 没有，Emacs 特有）─
  "h"   '(:ignore t                :wk "help")
  "hf"  '(describe-function        :wk "function")
  "hv"  '(describe-variable        :wk "variable")
  "hk"  '(describe-key             :wk "key")
  "hm"  '(describe-mode            :wk "mode")
  "hi"  '(info                     :wk "info")

  ;; -- j: journal
  "j"   '(:ignore t                :wk "journal")
  "jj"  '(org-journal-new-entry                     :wk "new j entry")
  "jo"  '(org-journal-open-current-journal-file                     :wk "open journal")
  )

 
;;;; 非 leader 的 LazyVim 习惯键
;; 注意：C-h 在 normal/motion state 下会被覆盖成“向左切窗口”，
;;       Emacs 的帮助前缀请用 F1 或 SPC h。
(evil-define-key '(normal motion) 'global
  (kbd "C-h") #'evil-window-left
  (kbd "C-j") #'evil-window-down
  (kbd "C-k") #'evil-window-up
  (kbd "C-l") #'evil-window-right
  (kbd "H")   #'previous-buffer          ; LazyVim: <S-h>
  (kbd "L")   #'next-buffer              ; LazyVim: <S-l>
  (kbd "]d")  #'flymake-goto-next-error  ; LazyVim: ]d
  (kbd "[d")  #'flymake-goto-prev-error) ; LazyVim: [d
;; ]b / [b 不用写：evil-collection 的 unimpaired 已经绑好了
 
;; gc / gcc 注释（LazyVim 内建同名操作）
(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode 1))
 


;; (use-package evil-surround
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-define-key 'normal 'global
;;     (kbd "gsa") #'evil-surround-region   ; gsa iw "   给 inner word 加双引号
;;     (kbd "gsd") #'evil-surround-delete   ; gsd "      删除外层双引号
;;     (kbd "gsr") #'evil-surround-change)  ; gsr " '    把双引号换成单引号
;;   (evil-define-key 'visual 'global
;;     (kbd "gsa") #'evil-surround-region))
                                        ; 选中后 gsa "
 

(provide 'my-evil)

;;; my-evil.el ends here
