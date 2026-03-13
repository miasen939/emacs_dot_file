;;; my-dashboard.el --- my dashboard ricing -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; 
;;

;;; Code:
(use-package dashboard
    :demand t
    :custom
    (dashboard-banner-logo-title "事情总是越想越困难，越做越简单，越拖越想放弃。\n\t\t\tStay Stong my friend.")

    

    (dashboard-startup-banner
     (let* ((image-dir (expand-file-name "~/Pictures/icon/"))
            (images (directory-files image-dir t "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)$" t)))
       (if images
           (seq-random-elt images)
         (message "No images found in %s" image-dir)
         nil)))

    ;; (dashboard-items '(
    ;;                    (agenda . 10)
    ;;                    (recents . 8)
    ;;                    (bookmarks . 5)
    ;;                    (projects . 5)))
    (dashboard-center-content t)
    (dashboard-vertically-center-content t)

    :config
    (dashboard-setup-startup-hook)
    )
(provide 'my-dashboard)

;;; my-dashboard.el ends here


