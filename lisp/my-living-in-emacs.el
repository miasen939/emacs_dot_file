;;; my-living-in-emacs.el --- misc emacs pacakages -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; living in Emacs!
;;

;;; Code:
;; 推荐组合：auth-source + pass 的桥接 + 好用的界面

;;; 依赖：sudo pacman -S pass gnupg git
;;; 一定不要忘了 安全备份私钥
;;; 一定不要忘了 pass phrase
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


;;; 词典，依赖sdcv
;;; 词典：quick-sdcv 或 dictionary + sdcv（更现代推荐 define-word 或 powerthesaurus）
(use-package password-store-menu
  :ensure t
  :bind ("C-c P" . password-store-menu))

(use-package quick-sdcv
  :ensure t
  :bind (("C-c D" . quick-sdcv-search-at-point)
         ("C-c d" . quick-sdcv-search-input))
  
  :custom
  (quick-sdcv-dictionary-data-dir "~/Ingrediant/dictionary/")
  (quick-sdcv-unique-buffers t)
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼"))



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

;;; todo: mu4e/notmuch calc calendar emms/listen
(defconst my/gmail-address "miasen939@gmail.com"
  "我的 Gmail 地址.")

(defconst my/gmail-full-name "GONG YUXI"
  "邮件中显示的姓名.")

(use-package mu4e
  ;; mu4e 随系统的 mu 包安装，不能让 package.el/MELPA 再装一份。
  :ensure nil
  :commands (mu4e mu4e-compose-new)

  :init
  ;; 快速启动 mu4e
  (global-set-key (kbd "C-c m") #'mu4e)

  :config
  ;; 基本身份
  (setq user-mail-address my/gmail-address
        user-full-name my/gmail-full-name
        mail-user-agent 'mu4e)

  ;; 获取邮件
  (setq mu4e-get-mail-command "mbsync gmail"

        ;; 每 5 分钟执行一次 mbsync 并重新索引。
        ;; 只有启动过 mu4e 后，mu4e 的更新计时器才会工作。
        mu4e-update-interval (* 5 60)

        ;; mbsync 与 mu4e 配合时建议启用，
        ;; 移动邮件时修改 Maildir 文件名。
        mu4e-change-filenames-when-moving t

        ;; Gmail 的 SMTP 会自动把已发送邮件加入 Sent。
        ;; 因此 mu4e 不要再保存一次，避免重复。
        mu4e-sent-messages-behavior 'delete

        ;; Gmail 标签通常会产生重复副本。
        mu4e-search-skip-duplicates t

        ;; 阅读设置
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        message-kill-buffer-on-exit t)


  ;; 只有同步了 All Mail 才启用这一行。
  ;; 在 Gmail 中，把邮件移入 All Mail 基本相当于归档。
  ;; (setq mu4e-refile-folder "/gmail/[Gmail]/All Mail")
  
  ;; ;; 下面的文件夹名称必须与你的 mbsync 目录完全一致。
  ;; (setq mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
  ;;       mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail"
  ;;       mu4e-trash-folder  "/gmail/[Gmail]/Trash")
  ;; 
  ;; ;; 主界面的邮箱快捷方式
  ;; (setq mu4e-maildir-shortcuts
  ;;       '((:maildir "/gmail/INBOX"
  ;;          :key ?i
  ;;          :favorite t)
  ;; 
  ;;         (:maildir "/gmail/[Gmail]/Sent Mail"
  ;;          :key ?s)
  ;; 
  ;;         (:maildir "/gmail/[Gmail]/Drafts"
  ;;          :key ?d)
  ;; 
  ;;         (:maildir "/gmail/[Gmail]/Trash"
  ;;          :key ?t)))

  ;; 下面的文件夹名称必须与你的 mbsync 目录完全一致。
(setq mu4e-drafts-folder "/gmail/[Gmail]/草稿"
      mu4e-sent-folder   "/gmail/[Gmail]/已发邮件"
      mu4e-trash-folder  "/gmail/[Gmail]/已删除邮件")

;; 只有同步了“所有邮件”后才启用。
;; 在 Gmail 中，把邮件移入“所有邮件”基本相当于归档。
;; (setq mu4e-refile-folder "/gmail/[Gmail]/所有邮件")

;; 主界面的邮箱快捷方式
(setq mu4e-maildir-shortcuts
      '((:maildir "/gmail/INBOX"
         :key ?i
         :favorite t)

        (:maildir "/gmail/[Gmail]/已发邮件"
         :key ?s)

        (:maildir "/gmail/[Gmail]/草稿"
         :key ?d)

        (:maildir "/gmail/[Gmail]/已加星标"
         :key ?f)

        (:maildir "/gmail/[Gmail]/已删除邮件"
         :key ?t)

        (:maildir "/gmail/[Gmail]/垃圾邮件"
         :key ?j)))

  ;; 阅读邮件时自动换行
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

  ;; ------------------------------
  ;; Gmail SMTP 发送设置
  ;; ------------------------------
  (require 'smtpmail)

  (setq auth-sources '("~/.authinfo.gpg"
                       "~/.authinfo")

        message-send-mail-function #'smtpmail-send-it

        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user my/gmail-address)
  (setq mu4e-sent-messages-behavior 'delete)


  (setq mu4e-maildir-shortcuts
      '((:maildir "/gmail/INBOX"
         :name "Inbox"
         :key ?i
         :favorite t)

        (:maildir "/gmail/[Gmail]/已发邮件"
         :name "Sent"
         :key ?s)

        (:maildir "/gmail/[Gmail]/草稿"
         :name "Drafts"
         :key ?d)

        (:maildir "/gmail/[Gmail]/已加星标"
         :name "Starred"
         :key ?f)

        (:maildir "/gmail/[Gmail]/已删除邮件"
         :name "Trash"
         :key ?t)

        (:maildir "/gmail/[Gmail]/垃圾邮件"
         :name "Spam"
         :key ?j)))
  )



;; listen


(use-package listen
  :ensure t

  :commands
  (listen
   listen-menu
   listen-play
   listen-pause
   listen-next
   listen-seek
   listen-volume
   listen-quit
   listen-status
   listen-library
   listen-library-from-playlist-file
   listen-queue
   listen-queue-list
   listen-queue-new
   listen-queue-play
   listen-queue-add-files
   listen-queue-add-from-playlist-file
   listen-queue-shuffle
   listen-queue-deduplicate
   listen-queue-goto-current)

  :bind
  (
   ("C-c e" . listen)
   ;; 主菜单
   ;; ("C-c l l" . listen)
   ;; 
   ;; ;; 播放控制
   ;; ("C-c l SPC" . listen-pause)
   ;; ("C-c l n"   . listen-next)
   ;; ("C-c l s"   . listen-seek)
   ;; ("C-c l v"   . listen-volume)
   ;; ("C-c l q"   . listen-quit)
   ;; 
   ;; ;; 文件、音乐库和队列
   ;; ("C-c l f"   . listen-play)
   ;; ("C-c l a"   . listen-queue-add-files)
   ;; ("C-c l b"   . listen-library)
   ;; ("C-c l L"   . listen-queue-list)
   ;; ("C-c l Q"   . listen-queue)
   ;; ("C-c l i"   . listen-status)
   )

  :init
  ;; ============================================================
  ;; 音乐目录
  ;; ============================================================

  (setq listen-directory
        (expand-file-name "~/Music/"))

  ;; ============================================================
  ;; 播放器
  ;; ============================================================

  ;; 明确使用 MPV。
  ;; 如果不设置，Listen 会自动检测，优先选择 MPV。
  (setq listen-backend
        #'make-listen-player-mpv)

  ;; MPV 启动时的默认音量
  (setq listen-mpv-volume 50)

  ;; ============================================================
  ;; 队列重复方式
  ;; ============================================================

  ;; nil      播完后停止
  ;; 'queue   循环播放整个队列
  ;; 'shuffle 播完后打乱队列并继续播放
  (setq listen-queue-repeat-mode nil)

  ;; ============================================================
  ;; Mode line
  ;; ============================================================

  ;; 标题过长时截断
  (setq listen-lighter-title-max-length 36)

  ;; 可用格式：
  ;; %s 播放状态
  ;; %a 艺术家
  ;; %A 专辑
  ;; %t 标题
  ;; %e 已播放时间
  ;; %r 剩余时间
  ;; %E 附加信息
  (setq listen-lighter-format
        " %s %a — %t  %e %r%E ")

  ;; 在 Mode line 中额外显示整个队列的剩余时长
  (setq listen-lighter-extra-functions
        '(listen-queue-format-remaining))

  ;; ============================================================
  ;; 元数据读取
  ;; ============================================================

  ;; ffprobe 并发进程数量。
  ;; Listen 默认使用约一半 CPU 核心，这里限制在最多 4 个，
  ;; 防止首次扫描大量音乐时启动过多进程。
  (setq listen-queue-max-probe-processes
        (min 4 (max 1 (/ (num-processors) 2))))

  :config
  ;; 确保 MPV 后端和队列功能已经加载
  (require 'listen-mpv)
  (require 'listen-queue)
  (require 'listen-library)

  ;; ============================================================
  ;; Dired 集成
  ;; ============================================================

  ;; 在 Dired 中标记音乐文件，然后按 C-c l a，
  ;; 将标记的文件加入 Listen 队列。
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map
                "C-c l a"
                #'listen-queue-add-files)))


;; emms
(defun my/emms-track-description (track)
  "生成比较简洁的 TRACK 显示名称。"
  (let ((artist (emms-track-get track 'info-artist))
        (title  (emms-track-get track 'info-title))
        (album  (emms-track-get track 'info-album))
        (name   (emms-track-name track)))
    (cond
     ;; 艺术家、标题和专辑都有
     ((and artist title album)
      (format "%s — %s  [%s]"
              artist title album))

     ;; 只有艺术家和标题
     ((and artist title)
      (format "%s — %s"
              artist title))

     ;; 只有标题
     (title
      (format "%s" title))

     ;; 没有标签时显示文件名，不显示完整路径
     ((eq (emms-track-type track) 'file)
      (file-name-base name))

     ;; 网络流等其他类型
     (t
      (emms-track-simple-description track)))))


;; (use-package emms
;;   :ensure t
;;   :commands (emms-browser
;;              emms-smart-browse
;;              emms-playlist-mode-go
;;              emms-play-file
;;              emms-play-url
;;              emms-play-directory-tree
;;              emms-add-directory-tree
;;              emms-add-file
;;              emms-pause
;;              emms-stop
;;              emms-next
;;              emms-previous
;;              emms-random
;;              emms-shuffle
;;              emms-show)
;;   :bind
;;   (;; 界面
;;    ("C-c e b"   . emms-smart-browse)
;;    ("C-c e l"   . emms-playlist-mode-go)
;; 
;;    ;; 添加与播放
;;    ("C-c e a"   . emms-add-directory-tree)
;;    ("C-c e d"   . emms-play-directory-tree)
;;    ("C-c e f"   . emms-play-file)
;;    ("C-c e u"   . emms-play-url)
;; 
;;    ;; 播放控制
;;    ("C-c e SPC" . emms-pause)
;;    ("C-c e s"   . emms-stop)
;;    ("C-c e n"   . emms-next)
;;    ("C-c e p"   . emms-previous)
;;    ("C-c e r"   . emms-random)
;;    ("C-c e S"   . emms-shuffle)
;;    ("C-c e i"   . emms-show))
;; 
;;   :init
;;   ;; 音乐目录
;;   (setq emms-source-file-default-directory
;;         (expand-file-name "~/Music/"))
;; 
;;   ;; 播放列表设置
;;   (setq emms-playlist-buffer-name "*EMMS Music*"
;;         emms-playlist-default-major-mode 'emms-playlist-mode
;;         emms-playlist-mode-center-when-go t
;; 
;;         ;; 保存播放列表时默认使用 M3U
;;         emms-source-playlist-default-format 'm3u
;; 
;;         ;; 播放完整个列表后停止
;;         emms-repeat-playlist nil
;; 
;;         ;; M-x emms-show 的显示格式
;;         emms-show-format "%s")
;; 
;;   :config
;;   ;; 加载官方完整稳定配置
;;   (require 'emms-setup)
;;   (emms-all)
;; 
;;   ;; ------------------------------------------------------------
;;   ;; 播放器：mpv
;;   ;; ------------------------------------------------------------
;; 
;;   (require 'emms-player-mpv)
;; 
;;   (setq emms-player-list '(emms-player-mpv)
;;         emms-player-mpv-command-name "mpv"
;; 
;;         ;; --no-config：
;;         ;; 不读取 ~/.config/mpv/mpv.conf，避免用户配置影响 EMMS。
;;         ;;
;;         ;; 如果你希望 EMMS 也使用自己的 mpv.conf，
;;         ;; 删除下面的 --no-config 即可。
;;         emms-player-mpv-parameters
;;         '("--quiet"
;;           "--really-quiet"
;;           "--no-config"
;;           "--no-audio-display"
;;           "--force-window=no"
;;           "--vo=null"))
;; 
;;   ;; ------------------------------------------------------------
;;   ;; 音乐标签与缓存
;;   ;; ------------------------------------------------------------
;; 
;;   (require 'emms-info-native)
;; 
;;   (setq emms-info-functions
;;         '(emms-info-native emms-info-cueinfo)
;; 
;;         ;; 音乐文件变化后自动更新信息
;;         emms-info-auto-update t
;; 
;;         ;; 异步读取标签，避免大量音乐时卡住 Emacs
;;         emms-info-asynchronously t)
;; 
;;   ;; 自定义歌名显示方式
;;   (setq emms-track-description-function
;;         #'my/emms-track-description)
;; 
;;   ;; ------------------------------------------------------------
;;   ;; 浏览器与专辑封面
;;   ;; ------------------------------------------------------------
;; 
;;   (require 'emms-browser)
;; 
;;   ;; 自动生成并缓存封面缩略图
;;   (setq emms-browser-covers
;;         #'emms-browser-cache-thumbnail-async)
;; 
;;   (setq emms-browser-thumbnail-small-size 64
;;         emms-browser-thumbnail-medium-size 128)
;; 
;;   ;; ------------------------------------------------------------
;;   ;; 自动保存、恢复播放列表
;;   ;; ------------------------------------------------------------
;; 
;;   (require 'emms-history)
;; 
;;   (setq emms-history-file
;;         (expand-file-name "var/emms/history"
;;                           user-emacs-directory)
;; 
;;         ;; 启动 Emacs 后恢复歌单，但不要自动播放
;;         emms-history-start-playing nil)
;; 
;;   ;; 创建保存 history 的目录
;;   (make-directory
;;    (file-name-directory emms-history-file)
;;    t)
;; 
;;   ;; 如果存在历史记录就恢复
;;   (when (file-readable-p emms-history-file)
;;     (condition-case err
;;         (emms-history-load)
;;       (error
;;        (message "加载 EMMS 播放列表历史失败：%s"
;;                 (error-message-string err)))))
;; 
;;   (require 'emms-mode-line)
;;   (emms-mode-line 1)
;; 
;;   (require 'emms-playing-time)
;;   (emms-playing-time 1))



(use-package pomm
  :config
  (pomm-mode-line-mode 1)
  (setq alert-default-style 'libnotify)
  ;;(setq pomm-audio-files)
  (setq pomm-audio-enabled t))

(use-package beancount
  :ensure t
  :mode ("\\.beancount\\'" . beancount-mode)
  :mode ("\\.bean\\'" . beancount-mode))
;; 依赖：sudo pacman -S beancount fava beancount-language-server


;; (use-package notmuch)
;; (use-package mu4e)

(setq browse-url-browser-function #'browse-url-firefox
        browse-url-firefox-program "firefox")

(use-package elfeed
  :ensure t
  :commands elfeed

  ;; 打开 Elfeed
  ;; :bind (("C-c e" . elfeed))

  :custom
  ;; 默认只显示最近一个月的未读内容
  (elfeed-search-filter "@1-month-ago +unread")

  ;; 标题显示宽度
  (elfeed-search-title-max-width 100)

  ;; 打开文章后，列表中的光标留在原位置
  (elfeed-search-remain-on-entry t)

  :hook
  ;; 阅读长文章时自动换行
  (elfeed-show-mode . visual-line-mode)

  :config
  
  
  (setq elfeed-feeds
        '(;; 每个网址后面可以添加标签
          ("https://sachachua.com/blog/category/emacs-news/feed/"
           emacs news weekly)

          ("https://nullprogram.com/feed/"
           programming emacs)

          ;; ("https://animeanime.jp/rss/index.rdf"
         ;;   anime japanese)
         ;;  
         ;; ("https://natalie.mu/comic/feed/news"
         ;;  anime manga japanese)
         ;; 
         ;; ;; 动画：英文
         ("https://www.animenewsnetwork.com/all/rss.xml?ann-edition=us"
           anime english)
          
         ;; ("https://myanimelist.net/rss/news.xml"
         ;;  anime english)
         ;; 
         ;; ;; 游戏：日文
         ;; ("https://www.4gamer.net/rss/news_topics.xml"
         ;;  game japanese)
         ;; ("https://automaton-media.com/feed/"
         ;;  game japanese)
         ;; ("https://www.gamespark.jp/rss/index.rdf"
         ;;  game japanese)
         ;; 
         ;; ;; 游戏：英文，日本游戏为主
         ;; ("https://www.gematsu.com/feed"
         ;;  game english japanese-games)
          
          )))

;; (setq elfeed-feeds
;;       '(;; Emacs
;;         ("https://sachachua.com/blog/category/emacs-news/feed"
;;          emacs weekly)
;;         ("https://planet.emacslife.com/atom.xml"
;;          emacs community)
;;         ("https://protesilaos.com/codelog.xml"
;;          emacs linux)
;; 
;;         ;; Linux / systems
;;         ("https://archlinux.org/feeds/news/"
;;          linux arch important)
;;         ("https://jvns.ca/atom.xml"
;;          systems programming)
;; 
;;         ;; 技术社区
;;         ("https://lobste.rs/t/programming.rss"
;;          programming community)
;; 
;;         ;; Research
;;         ("https://rss.arxiv.org/rss/cs.OS+cs.NI"
;;          research os network)))


(provide 'my-living-in-emacs)

;;; my-living-in-emacs.el ends here
