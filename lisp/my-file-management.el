;;; my-file-management.el --- dired and more -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq dired-mouse-drag-files t)
  (keymap-set dired-mode-map "M-m" #'casual-dired-tmenu)
  )

(defun my/dired-open-folder-externally ()
  "用外部文件管理器打开当前 dired 目录（或光标处文件所在目录）。"
  (interactive)
  (let ((dir (if (derived-mode-p 'dired-mode)
                 (dired-current-directory)
               (file-name-directory (or (buffer-file-name) default-directory)))))
    (start-process "xdg-open" nil "xdg-open" (expand-file-name dir))))

;; w dired copy current dir 

(use-package async
  :ensure t
  :after dired
  :config
  (dired-async-mode 1))

 ;; dired-rsync

(keymap-global-set "C-x C-r" 'crux-recentf-find-directory)

(use-package dired-open
  :ensure nil
  :config
  ;; Doesn't work as expected!
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;; -- OR! --
  (setq dired-open-extensions '(
                                ;; ("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                ("mkv" . "mpv")
                                ("webm" . "mpv")
                                )
        ))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))



(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind
  ("C-x C-d" . dirvish))
;; 要求大量依赖


;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-buffer-name-function            #'treemacs-default-buffer-name
;;           Treemacs-buffer-name-prefix              " *Treemacs-Buffer-"
;;           treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                2000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-hide-dot-jj-directory           t
;;           treemacs-indentation                     2
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-files-by-mouse-dragging    t
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-project-follow-into-home        nil
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil)
;; 
;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)
;; 
;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (when treemacs-python-executable
;;       (treemacs-git-commit-diff-mode t))
;; 
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))
;; 
;;     (treemacs-hide-gitignored-files-mode nil))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t d"   . treemacs-select-directory)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))


;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)
;; 
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)
;; 
;; (use-package treemacs-magit
;;   :after (treemacs magit)
;;   :ensure t)
;; 
;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))
;; 
;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))
;; 
;; (treemacs-start-on-boot)

;; (use-package quick-fasd
;;   :bind (
;;          ("C-x C-d" . quick-fasd-find-path)
;;          :map minibuffer-local-completion-map 
;;          ("C-x C-d" . quick-fasd-find-path) 
;;          )
;;   :init
;;   (quick-fasd-mode 1))


;; (use-package dired-preview)
;; 这个不太行

;; dired++ diredx



(use-package dired-hacks-utils
  :elpaca (dired-hacks-utils :host github :repo "Fuco1/dired-hacks")
  :hook (dired-mode . dired-hacks-utils-mode))

(use-package dired-subtree
  :elpaca (dired-subtree :host github :repo "Fuco1/dired-hacks")
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil) ; 不然背景色很丑
  :bind (:map dired-mode-map
         ("TAB" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle)))

(use-package dired-rainbow
  :elpaca (dired-rainbow :host github :repo "Fuco1/dired-hacks")
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("html" "htm" "xhtml" "css"))
  (dired-rainbow-define document "#9561e2" ("pdf" "doc" "docx" "epub" "org"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "avi" "mkv" "flac" "wav"))
  (dired-rainbow-define image "#f66d9b" ("png" "jpg" "jpeg" "gif" "svg" "webp"))
  (dired-rainbow-define compressed "#51d88a" ("zip" "gz" "tar" "7z" "rar" "xz"))
  (dired-rainbow-define compiled "#4dc0b5" ("el" "elc" "py" "rs" "c" "cpp" "js" "ts")))

(use-package dired-narrow
  :elpaca (dired-narrow :host github :repo "Fuco1/dired-hacks")
  :after dired
  :bind (:map dired-mode-map
         ("/" . dired-narrow-fuzzy)))

(use-package dired-ranger
  :elpaca (dired-ranger :host github :repo "Fuco1/dired-hacks")
  :after dired
  :bind (:map dired-mode-map
         ("W" . dired-ranger-copy)
         ("X" . dired-ranger-move)
         ("Y" . dired-ranger-paste)))

(provide 'my-file-management)

;;; my-file-management.el ends here
