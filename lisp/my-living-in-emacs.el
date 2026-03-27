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

(provide 'my-living-in-emacs)

;;; my-living-in-emacs.el ends here
