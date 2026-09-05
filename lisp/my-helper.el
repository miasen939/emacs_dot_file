;;; my-helper.el --- 发现式使用emacs -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
;; (use-package casual-suite
;;   :demand t
;;   :bind
;;   (("M-j" . casual-avy-tmenu)
;;    ("C-o" . casual-editkit-main-tmenu)
;;    ;; ("M-m" . casual-suite-tmenu)
;; 
;; 
;;    
;;    :map calc-mode-map
;;    ("M-m" . casual-calc-tmenu)
;; 
;;    :map dired-mode-map
;;    ("M-m" . casual-dired-tmenu)
;; 
;;    :map org-mode-map
;;    ("M-m" . casual-org-tmenu)
;; 
;;    :map org-table-fedit-map
;;    ("M-m" . casual-org-table-fedit-tmenu)
;; 
;;    :map org-agenda-mode-map
;;    ("M-m" . casual-agenda-tmenu)
;; 
;;    :map ibuffer-mode-map
;;    ("M-m" . casual-ibuffer-tmenu)
;; 
;;    :map bookmark-bmenu-mode-map
;;    ("M-m" . casual-bookmarks-tmenu)
;; 
;;    :map calendar-mode-map
;;    ("M-m" . casual-calendar-tmenu)
;; 
;;    :map compilation-mode-map
;;    ("M-m" . casual-compile-tmenu)
;; 
;;    :map help-mode-map
;;    ("M-m" . casual-help-tmenu)
;; 
;;    :map Info-mode-map
;;    ("M-m" . casual-info-tmenu)
;; 
;;    :map image-mode-map
;;    ("M-m" . casual-image-tmenu)))

;; (keymap-set org-agenda-mode-map "M-m" #'casual-agenda-tmenu)
;; (keymap-set org-mode-map "M-m" #'casual-org-tmenu)
;; TODO 不知道为什么这一段 casual 的代码一直报错

 ;; casual-image
 ;; casual-isearch-tmenu
 ;; casual-bibtex-tmenu
 ;; casual-info
 ;; casual-symbol-overlay
 ;; re-builder
 ;; casual-calendar
 ;; casual bookmarks
 ;; casual compile
 ;; M-x man and casual man


(use-package which-key
  :defer 0.2
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.25)
  (which-key-sort-order 'which-key-key-order-alpha)
  :init
  (which-key-mode))

(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))


;; todo: c-h/hydra/transient/embark
(use-package hydra)
(use-package default-text-scale)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("0" (text-scale-set 0) "reset")
;;   ("f" nil "finished" :exit t))
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" default-text-scale-increase "in")
  ("k" default-text-scale-decrease "out")
  ("0" default-text-scale-reset "reset")
  ("f" nil "finished" :exit t))

(provide 'my-helper)

;;; my-helper.el ends here
