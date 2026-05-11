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


(use-package pomm
  :defer 0.1
  :config
  (pomm-mode-line-mode 1)
  (setq alert-default-style 'libnotify)
  ;;(setq pomm-audio-files)
  (setq pomm-audio-enabled t)

  ;;(pomm-start);defer 0.1后马上启动pomm番茄钟

  ;;kde notification
  ;;

  ;; org-clock
  ;;
;;; my-pomm-integration.el
  ;; 策略：番茄钟为默认状态，主动启动 Third Time 时暂停番茄钟，退出后自动恢复

  (defun my/pomm-third-time-status-changed ()
    "Third Time 状态变化时，同步控制番茄钟。"
    (let ((tt-status (alist-get 'status pomm-third-time--state)))
      (cond
       ;; Third Time 启动 → 停止番茄钟
       ((eq tt-status 'running)
        (when (eq (alist-get 'status pomm--state) 'running)
          (pomm-stop)))
       ;; Third Time 停止 → 启动番茄钟
       ((eq tt-status 'stopped)
        (unless (eq (alist-get 'status pomm--state) 'running)
          (pomm-start))))))

  (add-hook 'pomm-third-time-on-status-changed-hook
            #'my/pomm-third-time-status-changed)

  ;; Emacs 启动时自动开启番茄钟（延迟1秒等待 pomm 加载完毕）
  
  
  )

(use-package beancount
  :ensure t
  :mode ("\\.beancount\\'" . beancount-mode)
  :mode ("\\.bean\\'" . beancount-mode))
;; 依赖：sudo pacman -S beancount fava beancount-language-server

(provide 'my-living-in-emacs)

;;; my-living-in-emacs.el ends here
