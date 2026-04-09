;;; my-IME.el --- emacs input method -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; Emacs的输入法
;;

;;; Code:

;; 需要配置雾凇拼音
;; 依赖 rime-Fcitx5

(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-show-candidate 'posframe)
  (setq rime-cursor "█")
  (setq rime-cursor-face '((t (:foreground "#00ff00"))))
  (setq rime-show-preedit t)
  (setq rime-user-data-dir "~/.config/fcitx/Rime/")
  ;; 默认值
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  
  (defun my/update-cursor-by-ime ()
    "根据当前输入法状态实时更新光标颜色."
    (set-cursor-color (if current-input-method "#a7c080" "#51afef")))

  (add-hook 'post-command-hook #'my/update-cursor-by-ime)
  )


;; 中文英文之间插入空格
(use-package pangu-spacing
  :defer 0.3
  :config
  (global-pangu-spacing-mode +1))

(global-set-key (kbd "C-x C-\\") 'skk-mode)


;; skk输入法，使用方法可查询wiki
(use-package ddskk
  :ensure t
  :init
  (setq skk-jisyo "~/.skk-jisyo")
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")

  (setq skk-extra-jisyo-file-list
        '("/usr/share/skk/SKK-JISYO.jinmei"
          "/usr/share/skk/SKK-JISYO.geo"))
  :config
  (setq skk-use-auto-fill t)
  (setq skk-save-jisyo-instantly t))

(provide 'my-IME)

;;; my-IME.el ends here
