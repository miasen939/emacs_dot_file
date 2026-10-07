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
        ;; evil-want-C-i-jump nil            ; 避免 TAB 在 org 里被抢
        ;; 有可能把C-i在org mode改为jumplist吗
        evil-undo-system 'undo-redo       ; Emacs 28+ 内建
        evil-respect-visual-line-mode t
        evil-split-window-below t
        evil-vsplit-window-right t)

  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; 这些 buffer 里保持 Emacs state，不进 Evil
  (dolist (m '(eshell-mode vterm-mode ghostel-mode))
    (evil-set-initial-state m 'emacs))
  )
;; #todo org RET 正常打开链接
 
;;;; 2. evil-collection：给 magit / dired / help / vertico 等补 Evil 键位
(use-package evil-collection
  :ensure t
  :demand t
  ;; :after evil
  :config
  (evil-collection-init))
 
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
    :global-prefix "M-SPC")

  (with-eval-after-load 'dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

  )
 
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

(general-def
  :states '(normal motion)
  "g SPC" '(find-file :wk "find file")
  "g a" '(crux-move-beginning-of-line :wk "go begining of line")
  "g e" '(move-end-of-line :wk "go End of line")
  "g l" '(evil-goto-line :wk "go Last")

  "s" 'flash-jump)

;;;; Leader keymap 键位
(my/leader
  ;; ── 顶层 ──────────────────────────────
  ;; "SPC" '(project-find-file        :wk "find file (root)")
  ","   '(consult-buffer           :wk "switch buffer")
  "/"   '(consult-ripgrep          :wk "grep (root)")
  ":"   '(consult-complex-command  :wk "command history")
  "e"   '(project-dired            :wk "explorer (root)")
  "E"   '(dired-jump               :wk "explorer (cwd)")
  "-"   '(evil-window-split        :wk "split below")
  "|"   '(evil-window-vsplit       :wk "split right")
  "."   '(other-window                    :wk "other window")
  "u"   '(universal-argument                    :wk "universal-argument")

  "SPC" '(execute-extended-command :wk "M-x")
  ;; ";" '(execute-extended-command :wk "M-x")

  "x" '(org-capture :wk "org-capture")
  "RET" '(bookmark-jump :wk "bookmark")
  "a" '(org-agenda :wk "agenda")

  ;; ── b: buffer ─────────────────────────
  "b"   '(:ignore t                :wk "buffer")
  "bb"  '(mode-line-other-buffer   :wk "other buffer")
  "bd"  '(kill-current-buffer      :wk "delete")
  "bD"  '(kill-buffer-and-window   :wk "delete + window")
  "bo"  '(my/kill-other-buffers    :wk "delete others")

  "bs"  '(save-buffer    :wk "save-buffer")
  "bi"  '(ibuffer    :wk "ibuffer")
  "be"  '(eval-buffer    :wk "eval-buffer")
  ;; #todo narrow
 
  ;; ── c: code（eglot）───────────────────
  "c"   '(:ignore t                :wk "code")
  "ca"  '(eglot-code-actions       :wk "code action")
  "cr"  '(eglot-rename             :wk "rename")
  "cf"  '(eglot-format             :wk "format")
  "cs"  '(consult-imenu            :wk "symbols")
  "cc"  '(compile            :wk "compile")
  "cC"  '(recompile            :wk "recompile")
 
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

  "fs"  '(save-buffer                  :wk "C-x C-s")
 
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
 
 
  ;; ── w: windows ────────────────────────
  "w"   '(:ignore t                :wk "windows")
  "ww"  '(other-window             :wk "other")
  "wd"  '(delete-window            :wk "delete")
  "w-"  '(evil-window-split        :wk "split below")
  "ws"  '(evil-window-split        :wk "split below")
  "w|"  '(evil-window-vsplit       :wk "split right")
  "wv"  '(evil-window-vsplit       :wk "split right")
  "wm"  '(my/toggle-maximize-window :wk "maximize")

  "wb"  '(evil-window-left         :wk "left")
  "wn"  '(evil-window-down         :wk "down")
  "wp"  '(evil-window-up           :wk "up")
  "wf"  '(evil-window-right        :wk "right")
 
  "wj"  '(evil-window-split        :wk "split below")
  "wl"  '(evil-window-vsplit       :wk "split right")
  ;; 参考 doom 的窗口键位
  ;; #todo 使用 C-x 4
  
 
  ;; ── <tab>: tabs（tab-bar）────────────
  "t"     '(:ignore t            :wk "tab")
  "t t" '(tab-new              :wk "new tab")
  "t d"   '(tab-close            :wk "close tab")
  "t j"   '(tab-next             :wk "next tab")
  "t k"   '(tab-previous         :wk "prev tab")
  "t o"   '(tab-close-other      :wk "close others")
 
  ;; ── q: quit ───────────────────────────
  "q"   '(:ignore t                :wk "quit")
  "qq"  '(save-buffers-kill-terminal :wk "quit Emacs")
  "qr"  '(restart-emacs :wk "restart Emacs")
  ;; TODO copy/delete/rename/move file,copy file route.
 
  ;; ── h: help（LazyVim 没有，Emacs 特有）─
  "h"   '(:ignore t                :wk "help")
  "hf"  '(describe-function        :wk "function")
  "hv"  '(describe-variable        :wk "variable")
  "hk"  '(describe-key             :wk "key")
  "hm"  '(describe-mode            :wk "mode")
  "hc"  '(describe-key-briefly            :wk "mode")
  "hi"  '(info                     :wk "info")


  ;; -- n: Note: org mode journal and roam
  "n"   '(:ignore t                :wk "journal&roam")
  "nj"  '(org-journal-new-entry                     :wk "new j entry")
  "no"  '(org-journal-open-current-journal-file                     :wk "open journal")

  "nf"  '(org-roam-node-find     :wk "roam find node")
  "ni"  '(org-roam-node-insert   :wk "roam insert node")
  "nb"  '(org-roam-buffer-toggle :wk "roam backlinks")
  "nt"  '(org-set-tags-command :wk "org tag")
  "nT"  '(org-roam-tag-add :wk "roam tag")
  "na"  '(org-roam-alias-add :wk "roam alias")

  "ns"  '(org-roam-db-sync :wk "roam sync")

  ;; -- r: register&bookmark
  "r"  '(:ignore t            :wk "bookmark")
  "rr" '(consult-bookmark     :wk "jump")
  "ra" '(bookmark-set         :wk "add")
  "rd" '(bookmark-delete      :wk "delete")
  "rn" '(bookmark-rename      :wk "rename")
  "rl" '(bookmark-bmenu-list  :wk "list")


  ;; ---- i: insert
  "i"   '(:ignore t :which-key "insert")
  "is"  '(consult-yasnippet          :which-key "yasnippet")
  "iy"  '(consult-yank-replace          :which-key "yank-replace")

  ;; #TODO org mode text object
  ;; #TODO 把org emphasis 改造成类似 word 的感觉
  )

 
;;;; 非 leader 的 LazyVim 习惯键
;; 注意：C-h 在 normal/motion state 下会被覆盖成“向左切窗口”，
;;       Emacs 的帮助前缀请用 F1 或 SPC h。
(evil-define-key '(normal motion) 'global
  ;; (kbd "C-h") #'evil-window-left
  ;; (kbd "C-j") #'evil-window-down
  ;; (kbd "C-k") #'evil-window-up
  ;; (kbd "C-l") #'evil-window-right
  (kbd "H")   #'previous-buffer          ; LazyVim: <S-h>
  (kbd "L")   #'next-buffer              ; LazyVim: <S-l>
  (kbd "]d")  #'flymake-goto-next-error  ; LazyVim: ]d
  (kbd "[d")  #'flymake-goto-prev-error) ; LazyVim: [d
;; ]b / [b 不用写：evil-collection 的 unimpaired 已经绑好了
 
 


(use-package evil-surround
  :ensure t
  :demand t
  :config
  (evil-define-key 'normal 'global
    (kbd "gsa") #'evil-surround-region   ; gsa iw "   给 inner word 加双引号
    (kbd "gsd") #'evil-surround-delete   ; gsd "      删除外层双引号
    (kbd "gsr") #'evil-surround-change)  ; gsr " '    把双引号换成单引号
  (evil-define-key 'visual 'global
    (kbd "gsa") #'evil-surround-region))
                                        ; 选中后 gsa "
 

(use-package evil-nerd-commenter
  :ensure t
  :demand t
  ;; :after evil            ; 必须在 evil 之后加载，否则 evilnc-comment-operator 可能没定义
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "gc") #'evilnc-comment-operator)          ; gc{motion} / visual 下 gc
  ;; (global-set-key (kbd "M-;") #'evilnc-comment-or-uncomment-lines)
  )


;; #TODO evil 的 repeat map

;; #TODO SPC m major mode keymap

;; #TODO vim marks highlight
;; #TODO ghostel evil keybinding




(general-create-definer my/local-leader
  :states '(normal visual motion insert emacs)
  :prefix "SPC m"               ; normal / visual / motion 状态
  :non-normal-prefix "M-SPC m") ; insert / emacs 状态
 
;;;; ---------------------------------------------------------------------------
;;;; 辅助命令：替代 Doom 的 +org/* 函数
;;;; ---------------------------------------------------------------------------
 
(defun my/org-remove-link ()
  "删除光标处的链接，保留描述文本；没有描述时保留原始链接。"
  (interactive)
  (let ((ctx (org-element-context)))
    (unless (eq (org-element-type ctx) 'link)
      (user-error "光标处没有链接"))
    (let* ((beg  (org-element-property :begin ctx))
           (end  (- (org-element-property :end ctx)
                    (org-element-property :post-blank ctx)))
           (cbeg (org-element-property :contents-begin ctx))
           (cend (org-element-property :contents-end ctx))
           ;; 在删除之前取出文本，否则位置会失效
           (text (if cbeg
                     (buffer-substring-no-properties cbeg cend)
                   (org-element-property :raw-link ctx))))
      (delete-region beg end)
      (insert text))))
 
(defun my/org-babel-remove-all-results ()
  "删除当前 buffer 中所有 src block 的结果。"
  (interactive)
  (org-babel-remove-result-one-or-many t))
 
(defun my/org-table-recalculate-all ()
  "重新计算当前表格的所有行。"
  (interactive)
  (org-table-recalculate t))
 
(defun my/org-refile-to-current-file ()
  "只在当前文件的标题中选择 refile 目标。"
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 9))))
    (call-interactively #'org-refile)))
 
(defun my/org-refile-to-file (file)
  "选择一个 org 文件，在它的标题中选择 refile 目标。"
  (interactive (list (read-file-name "Refile 到文件: " org-directory nil t)))
  (let ((org-refile-targets `((,file :maxlevel . 9))))
    (call-interactively #'org-refile)))
 
(defun my/org-refile-to-clock ()
  "Refile 到正在计时的任务下。"
  (interactive)
  (org-refile 2))   ; 数字前缀参数 2 = 目标为当前 clock
 
(defun my/org-goto-last-refile ()
  "跳到上次 refile 的位置。"
  (interactive)
  (org-refile-goto-last-stored))
 
;;;; ---------------------------------------------------------------------------
;;;; 键位
;;;; ---------------------------------------------------------------------------
 
(with-eval-after-load 'org
  (my/local-leader
    :keymaps 'org-mode-map
 
    ;; ---- 单键 ----
    "#" '(org-update-statistics-cookies :which-key "update cookies")
    "'" '(org-edit-special              :which-key "edit special")
    "*" '(org-ctrl-c-star               :which-key "C-c *")
    "-" '(org-ctrl-c-minus              :which-key "C-c -")
    "," '(org-switchb                   :which-key "switch org buffer")
    "." '(consult-org-heading           :which-key "goto heading")
    "/" '(consult-org-agenda            :which-key "goto agenda heading")
    "@" '(org-cite-insert               :which-key "insert citation")
    "A" '(org-archive-subtree-default   :which-key "archive subtree")
    "e" '(org-export-dispatch           :which-key "export")
    "f" '(org-footnote-action           :which-key "footnote")
    "h" '(org-toggle-heading            :which-key "toggle heading")
    "i" '(org-toggle-item               :which-key "toggle item")
    "I" '(org-id-get-create             :which-key "create ID")
    "k" '(org-babel-remove-result       :which-key "remove result")
    "K" '(my/org-babel-remove-all-results :which-key "remove all results")
    "n" '(org-store-link                :which-key "store link")
    "o" '(org-set-property              :which-key "set property")
    "q" '(org-set-tags-command          :which-key "set tags")
    "t" '(org-todo                      :which-key "todo")
    "T" '(org-todo-list                 :which-key "todo list")
    "x" '(org-toggle-checkbox           :which-key "toggle checkbox")
 
    ;; ---- d: date ----
    "d"  '(:ignore t :which-key "date")
    "dd" '(org-deadline                 :which-key "deadline")
    "ds" '(org-schedule                 :which-key "schedule")
    "dt" '(org-time-stamp               :which-key "timestamp")
    "dT" '(org-time-stamp-inactive      :which-key "inactive timestamp")
 
    ;; ---- s: subtree ----
    "s"  '(:ignore t :which-key "subtree")
    "sh" '(org-promote-subtree          :which-key "promote")
    "sl" '(org-demote-subtree           :which-key "demote")
    "sj" '(org-move-subtree-down        :which-key "move down")
    "sk" '(org-move-subtree-up          :which-key "move up")
    "sd" '(org-cut-subtree              :which-key "cut")
    "sc" '(org-clone-subtree-with-time-shift :which-key "clone")
    "sn" '(org-narrow-to-subtree        :which-key "narrow")
    "sN" '(widen                        :which-key "widen")
    "sb" '(org-tree-to-indirect-buffer  :which-key "indirect buffer")
    "sr" '(org-refile                   :which-key "refile")
    "ss" '(org-sparse-tree              :which-key "sparse tree")
    "sS" '(org-sort                     :which-key "sort")
    "sa" '(org-toggle-archive-tag       :which-key "toggle ARCHIVE tag")
    "sA" '(org-archive-subtree          :which-key "archive")
 
    ;; ---- r: refile ----
    "r"  '(:ignore t :which-key "refile")
    "rr" '(org-refile                   :which-key "refile")
    "rR" '(org-refile-reverse           :which-key "refile (reverse)")
    "r." '(my/org-refile-to-current-file :which-key "to current file")
    "rf" '(my/org-refile-to-file        :which-key "to file")
    "rc" '(my/org-refile-to-clock       :which-key "to running clock")
 
    ;; ---- l: link ----
    "l"  '(:ignore t :which-key "link")
    "ll" '(org-insert-link              :which-key "insert link")
    "lL" '(org-insert-all-links         :which-key "insert all stored")
    "ls" '(org-store-link               :which-key "store link")
    "lS" '(org-insert-last-stored-link  :which-key "insert last stored")
    "li" '(org-id-store-link            :which-key "store ID link")
    "ld" '(my/org-remove-link           :which-key "remove link")
    "lt" '(org-toggle-link-display      :which-key "toggle display")
    "lc" '(org-cliplink                 :which-key "cliplink") ; 需要 org-cliplink 包
 
    ;; ---- c: clock ----
    "c"  '(:ignore t :which-key "clock")
    "ci" '(org-clock-in                 :which-key "clock in")
    "cI" '(org-clock-in-last            :which-key "clock in last")
    "co" '(org-clock-out                :which-key "clock out")
    "cc" '(org-clock-cancel             :which-key "cancel")
    "cg" '(org-clock-goto               :which-key "goto clock")
    "ce" '(org-set-effort               :which-key "set effort")
    "cr" '(org-resolve-clocks           :which-key "resolve")
    "cR" '(org-clock-report             :which-key "report")
 
    ;; ---- b: table ----
    "b"   '(:ignore t :which-key "table")
    "bc"  '(org-table-create-or-convert-from-region :which-key "create")
    "ba"  '(org-table-align             :which-key "align")
    "b-"  '(org-table-insert-hline      :which-key "hline")
    "br"  '(org-table-recalculate       :which-key "recalc line")
    "bR"  '(my/org-table-recalculate-all :which-key "recalc table")
    "bs"  '(org-table-sort-lines        :which-key "sort")
    "bf"  '(org-table-edit-formulas     :which-key "edit formulas")
    "bd"  '(:ignore t :which-key "delete")
    "bdc" '(org-table-delete-column     :which-key "column")
    "bdr" '(org-table-kill-row          :which-key "row")
    "bi"  '(:ignore t :which-key "insert")
    "bic" '(org-table-insert-column     :which-key "column")
    "bir" '(org-table-insert-row        :which-key "row")
    "bih" '(org-table-insert-hline      :which-key "hline")
 
    ;; ---- a: attachment ----
    "a"  '(:ignore t :which-key "attach")
    "aa" '(org-attach                   :which-key "attach menu")
    "an" '(org-attach-new               :which-key "new")
    "ao" '(org-attach-open              :which-key "open")
    "aO" '(org-attach-open-in-emacs     :which-key "open in emacs")
    "af" '(org-attach-reveal-in-emacs   :which-key "attach dir")
    "au" '(org-attach-url               :which-key "from URL")
    "ad" '(org-attach-delete-one        :which-key "delete")
    "aD" '(org-attach-delete-all        :which-key "delete all")
 
    ;; ---- p: priority ----
    "p"  '(:ignore t :which-key "priority")
    "pp" '(org-priority                 :which-key "set")
    "pu" '(org-priority-up              :which-key "up")
    "pd" '(org-priority-down            :which-key "down")
 
    ;; ---- g: goto ----
    "g"  '(:ignore t :which-key "goto")
    "gg" '(consult-org-heading          :which-key "heading")
    "gc" '(org-clock-goto               :which-key "running clock")
    "gi" '(org-id-goto                  :which-key "by ID")
    "gr" '(my/org-goto-last-refile      :which-key "last refile")
 
    ;; ---- P: publish ----
    "P"  '(:ignore t :which-key "publish")
    "Pp" '(org-publish-current-project  :which-key "project")
    "Pf" '(org-publish-current-file     :which-key "file")
    "Pa" '(org-publish-all              :which-key "all")
 
    ;; ---- m: org-roam ----
    "m"   '(:ignore t :which-key "roam")
    "mf"  '(org-roam-node-find          :which-key "find node")
    "mi"  '(org-roam-node-insert        :which-key "insert node")
    "mm"  '(org-roam-buffer-toggle      :which-key "backlinks buffer")
    "mt"  '(org-roam-tag-add            :which-key "add tag")
    "mT"  '(org-roam-tag-remove         :which-key "remove tag")
    "ma"  '(org-roam-alias-add          :which-key "add alias")
    "mA"  '(org-roam-alias-remove       :which-key "remove alias")
    "mr"  '(org-roam-refile             :which-key "refile to node")

    ))
 



(provide 'my-evil)

;;; my-evil.el ends here
