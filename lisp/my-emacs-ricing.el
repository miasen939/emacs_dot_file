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
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )



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





;;(add-to-list 'default-frame-alist '(undecorated . t))



;; (use-package beacon
;;   :defer 3
;;   :config
;;   (beacon-mode 1)
;;   ;; 可选配置
;;   ;; (setq beacon-color "#4a5060")
;;   ;; (setq beacon-size 20)
;;   ;; (setq beacon-blink-duration 0.3)
;;   )

(use-package dashboard
  :demand t
  :custom
  ;; (dashboard-banner-logo-title "事情总是越想越困难，越做越简单，越拖越想放弃。\n\t\t\tStay Stong my friend.\n\t")
  (dashboard-banner-logo-title "You Only Live Once.")
  
  (dashboard-startup-banner
   (let* ((image-dir (expand-file-name "~/Pictures/icon/"))
          (images (directory-files image-dir t "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)$" t)))
     (if images
         (seq-random-elt images)
       (message "No images found in %s" image-dir)
       nil)))

  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (setq dashboard-items nil)
  
  (setq initial-buffer-choice 'dashboard-open)
  (add-hook 'server-after-make-frame-hook 'dashboard-open)
  
  ;; show agenda
  (setq dashboard-week-agenda t)
  (setq dashboard-items '(
                          (agenda    . 20)
                          ))
  (dashboard-setup-startup-hook)
  )

;; 只让背景透明（文字和光标等保持不透明）



(set-fontset-font t 'unicode
                   (font-spec :family "Symbols Nerd Font Mono")
                   nil 'append)

;; (set-fontset-font t 'unicode
;;                    (font-spec :family "MesloLGS Nerd Font Mono" :size 28)
;;                    nil 'append)
(use-package keycast)





(set-frame-parameter nil 'alpha-background 92)              ; make current frame transparent
(add-to-list 'default-frame-alist '(alpha-background . 92)) ; make new frames transparent
(defun my/toggle-window-transparency ()
  "Toggle current frame's background transparency."
  (interactive)
  (let* ((desired-alpha 92)
         (current-alpha (frame-parameter nil 'alpha-background)))
    (if (equal current-alpha desired-alpha)
        (progn
          (set-frame-parameter nil 'alpha-background nil)
          (setq default-frame-alist (assq-delete-all 'alpha-background default-frame-alist)))
      (progn
        (set-frame-parameter nil 'alpha-background desired-alpha)
        (add-to-list 'default-frame-alist '(alpha-background . 92))))))


;; (use-package ultra-scroll
;;   :ensure (:host github
;;            :repo "jdtsmith/ultra-scroll")
;;   :init
;;   (setq scroll-conservatively 3)
;;   :config
;;   (ultra-scroll-mode 1))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(use-package dimmer
  :ensure t
  :demand
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t)
  )

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists 'skip) ; prevent extra bars in nested lists + skip intermediate bars
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed; check the wiki
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))
  ;; Note: wrap likely not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode kdl-mode emacs-lisp-mode) . indent-bars-mode))


(defun my/time-until-bedtime ()
  "显示距离今晚 23:00 还有多久。"
  (interactive)
  (let* ((now (decode-time))
         (target (encode-time
                  (list 0 0 23
                        (decoded-time-day now)
                        (decoded-time-month now)
                        (decoded-time-year now)
                        nil -1 (decoded-time-zone now))))
         (secs (floor (float-time (time-subtract target (current-time))))))
    ;; 已过 23:00 则顺延到明天
    (when (< secs 0)
      (setq secs (+ secs 86400)))
    (message "距离 23:00 睡觉还有 %d 小时 %d 分钟"
             (/ secs 3600)
             (/ (% secs 3600) 60))))

(provide 'my-emacs-ricing)

;;; my-emacs-ricing.el ends here
