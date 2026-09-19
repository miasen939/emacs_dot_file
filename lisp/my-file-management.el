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
