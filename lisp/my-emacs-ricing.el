;;; my-emacs-ricing.el --- rice the toyota -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(use-package doom-themes
  :demand t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-one") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-vibrant t)
                                        ;  (load-theme 'doom-nord t)
                                        ;  (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.

  (doom-themes-org-config)
  )




(use-package doom-modeline
  ;;:init (doom-modeline-mode 1)
  :hook (after-init . doom-modeline-mode))



;; 备用的其他主题配置
;; (use-package ef-themes
;;  :demand t
;;  :bind (("<f5>" . modus-themes-rotate)
;;         ("C-<f5>" . modus-themes-select)
;;         ("M-<f5>" . modus-themes-load-random))
;;  :init
;;  (setq modus-themes-mixed-fonts t
;;        modus-themes-italic-constructs t)
;;  :config
;;  (setq ef-owl-palette-overrides
;;        '((bg-region "#1a3f4a")))
;;  
;;  (ef-themes-load-theme 'ef-owl))

;; (use-package mood-line
;; :hook (after-init . mood-line-mode))



;; 字体安装： sudo pacman -S ttf-sarasa-gothic

(custom-set-faces
 '(gnus-group-news-low ((t (:foreground "cyan"))))
 '(gnus-group-news-low-empty ((t (:foreground "cyan" :weight normal)))))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )



(defun my/setup-fonts ()
  "设置字体配置."

  (when (display-graphic-p)
    ;; 英文字体
    (set-face-attribute 'default nil
                        :height 130
                        :weight 'normal
                        :family "DejaVu Sans mono")
    
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family "Sarasa Fixed SC"
                                   )))))

(use-package valign
  :hook (org-mode . valign-mode))         ;解决像素级对齐，让org sheet可以用中日文也对齐了

;;(add-to-list 'default-frame-alist '(undecorated . t))


;; 延迟加载字体设置
(add-hook 'after-init-hook #'my/setup-fonts)

(use-package beacon
  :defer 0.5
  :config
  (beacon-mode 1)
  ;; 可选配置
  (setq beacon-color "#4a5060")
  (setq beacon-size 20)
  (setq beacon-blink-duration 0.3))

;; (use-package dashboard
;;   :demand t
;;   :custom
;;   (dashboard-banner-logo-title "事情总是越想越困难，越做越简单，越拖越想放弃。\n\t\t\tStay Stong my friend.")
;; 
;;   
;; 
;;   (dashboard-startup-banner
;;    (let* ((image-dir (expand-file-name "~/Pictures/icon/"))
;;           (images (directory-files image-dir t "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)$" t)))
;;      (if images
;;          (seq-random-elt images)
;;        (message "No images found in %s" image-dir)
;;        nil)))
;; 
;;   (dashboard-center-content t)
;;   (dashboard-vertically-center-content t)
;; 
;;   :config
;;   (setq dashboard-items nil)
;;   
;;   ;;(setq initial-buffer-choice 'dashboard-open)
;;   ;;(add-hook 'server-after-make-frame-hook 'dashboard-open)
;; 
;;   ;; show agenda
;;   (setq dashboard-week-agenda t)
;;   (setq dashboard-items '(
;;                           (agenda    . 20)
;;                           ))
;;   (dashboard-setup-startup-hook)
;;   )

(provide 'my-emacs-ricing)

;;; my-emacs-ricing.el ends here
