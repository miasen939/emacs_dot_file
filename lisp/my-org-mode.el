;;; my-org-mode.el --- org-mode config -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

  ;; 测试
  ;; 成功了！

   ;;; Code:
  (use-package org
    :ensure t
    :pin gnu ;; 更新此内置包
    :mode ("\\.org\\'" . org-mode)
    :bind (("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           :map org-mode-map
           ("C-c <up>" . org-priority-up)
           ("C-c <down>" . org-priority-down)
           ("C-c C-g C-r" . org-shiftmetaright))
    :hook ((org-mode . org-indent-mode)
           (org-mode . visual-line-mode))
    :custom
    (org-log-done 'time)
    (org-return-follows-link t)
    (org-hide-emphasis-markers t)
    (org-return-follows-link t)


    :config
    ;; (setq org-startup-with-inline-images t)

    ;; phone refile

    (setq org-refile-targets
          '((nil :maxlevel . 1)
            (org-agenda-files :maxlevel . 1)))

    (setq org-refile-use-outline-path 'file)        ; 显示文件名前缀
    (setq org-outline-path-complete-in-steps nil)   ; 关键！一次性显示所有候选


    (setq image-use-external-converter t)
    (setq org-image-actual-width nil   ;; 使用图片原始宽度，不强制缩放
          image-transform-smoothing nil
          max-image-size nil)

    ;;(add-to-list 'org-modules 'org-habit)
    (setq org-modules '(org-habit
                        org-id      ;; 如果用 org-roam 或跨文件链接
                        org-attach  ;; 如果用附件
                        ))
    (org-load-modules-maybe t)  ;; org-habit 此时才真正加载完毕

    (setq org-habit-graph-column 50
          org-habit-preceding-days 30
          org-habit-following-days 3
          org-habit-show-habits-only-for-today t
          org-habit-show-all-today t)


                                          ;org-export
    (with-eval-after-load 'ox
      (require 'ox-md))



    ;; img

    ;;org-todo
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "PLANNING(p)" "IN-PROGRESS(i@/!)"
                      "BLOCKED(b@)" "WAITING(w@)" "|" "DONE(d!)" "CANCELED(c@!)")))
    (setq org-log-done 'note) 
    ;; TODO colors
    (setq org-todo-keyword-faces
          '(
            ("TODO" . (:foreground "GoldenRod" :weight bold))
            ("NEXT" . (:foreground "DeepSkyBlue1" :weight bold)) ;蓝色
            ("SOMEDAY" . (:foreground "RosyBrown" :weight bold))
            ("PLANNING" . (:foreground "DeepPink" :weight bold))
            ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
            ("BLOCKED" . (:foreground "Red" :weight bold))
            ("WAITING" . (:foreground "khaki4" :weight bold)) ;
            ("DONE" . (:foreground "LimeGreen" :weight bold))
            ("CANCELED" . (:foreground "DimGrey" :weight bold))
            ))
    ;;org-agenda
    (setq org-agenda-files 
          '("~/Documents/org-agenda/TODOs.org"
            "~/Documents/org-agenda/habits.org")
          ) ;;一定要注意，改这个路径的同时要改org capture的路径
    :hook
    (org-mode . org-link-preview-refresh)

    )

  (use-package org-capture
    :ensure nil
    :bind ("C-c c" . org-capture)
    :config
    (setq org-capture-templates
          '(("t" "Todo" entry
             (file+headline "~/Documents/org-agenda/TODOs.org" "inbox:inbox:")
             "* TODO %?\n  %U\n")
            ("s" "Someday" entry
             (file+headline "~/Documents/org-agenda/TODOs.org" "inbox:inbox:")
             "* SOMEDAY %?\n  %U\n")
            ("n" "Next" entry
             (file+headline "~/Documents/org-agenda/TODOs.org" "inbox:inbox:")
             "* NEXT %?\n  %U\n"))))
  (use-package org-appear
    :hook (org-mode . org-appear-mode)
    :config
    (setq org-appear-autoemphasis t   ;; *bold* / /italic/
          org-appear-autolinks t      ;; 链接
          org-appear-autosubmarkers t))

  (use-package org-auto-tangle
    :hook (org-mode . org-auto-tangle-mode))

(use-package org-transclusion
  :after org
  :bind (("S-<f12>" . org-transclusion-add)
         ("C-.  m" . org-transclusion-transient-menu)
         ("C-.  t" . org-transclusion-mode)
         ))

(use-package org-download
  :ensure t
  :after org
  :bind (:map org-mode-map
              ("C-c C-M-y" . org-download-clipboard)   ; 推荐快捷键：粘贴剪贴板图片
              ("C-c M-y"   . org-download-yank))       ; 另一种常用快捷键
  :config
  ;; 核心配置写在这里
  (setq org-download-method 'directory
        org-download-image-dir "./static/img")
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-link-format-function
        (lambda (link)
          (format "[[file:%s]]\n" (file-relative-name link))))

  ;; 是否自动给文件名加时间戳前缀（避免重名）
  (setq org-download-timestamp "_%Y%m%d_%H%M%S")

  ;; Mac/Linux 剪贴板截图特别推荐再加这一行
  ;; (when (eq system-type 'darwin)   ; macOS
  ;;   (setq org-download-screenshot-method "screencapture -i %s"))
  (setq org-image-actual-width '(600))
  )

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/roam-note/"))
  (org-roam-capture-templates
   '(    ("d" "default" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         
         ("M" "MOC note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :MOC:
        ")
          :unnarrowed t)
         
         ("D" "directory note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :directory:
    ")
          :unnarrowed t)
         
         ("f" "fleeting note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :fleeting:
    ")
          :unnarrowed t)
         ("P" "person note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :person:
    ")
          :unnarrowed t)
         
         ("p" "project notes" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :project:\n\n
    ")
          :unnarrowed t)
         ("t" "terminology" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :terminology:
    ")
          :unnarrowed t)
         ("r" "Reference" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :reference:\n\n* source
    ")
          :unnarrowed t)
         ("o" "anime/manga/game/visual novel note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags: :otaku:\n\n* source
    ")
          :unnarrowed t)
         ("T" "Thoughts note" plain "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n\n#+date: %U\n\n#+filetags:thoughts:
    ")
          :unnarrowed t)
         )
   )

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n p" . org-roam-buffer-display-dedicated)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n A" . org-roam-alias-remove)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n N" . org-roam-dailies-goto-next-note)
         ("C-c n B" . org-roam-dailies-goto-previous-note)
         ("C-c n T" . org-roam-dailies-goto-today))
  
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  
  
  (setq org-id-locations-file "~/.emacs.d/var/.org-id-locations")
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

  (use-package org-roam-ui
  :vc (:url "https://github.com/org-roam/org-roam-ui.git"
            :branch "main")   ; 可选：指定分支，默认是 main/master
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  ;; (org-roam-ui-mode +1)  ; 手动启用或加 hook
  )

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-preview)
              ("C-c C-c l" . markdown-live-preview-mode)
              ("C-c C-s c" . markdown-insert-code)
              ("C-c C-s C" . markdown-insert-gfm-code-block)
              ("C-c C-s b" . markdown-insert-bold)
              ("C-c C-s i" . markdown-insert-italic)
              ("C-c C-l" . markdown-insert-link)
              ("C-c C-i" . markdown-insert-image)
              ("C-c C-n" . markdown-next-heading)
              ("C-c C-p" . markdown-previous-heading))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-gfm-use-electric-backquote t)
  (markdown-asymmetric-header t))

(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-or-refresh-toc))
  :custom
  (markdown-toc-header-toc-start "<!-- TOC -->")
  (markdown-toc-header-toc-end "<!-- /TOC -->"))

(use-package valign
  :after markdown-mode
  :hook (markdown-mode . valign-mode)
  :custom
  (valign-fancy-bar t))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  (provide 'my-org-mode)

;;; my-org-mode.el ends here
