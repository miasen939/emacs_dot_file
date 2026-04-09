;;; my-ebook-reader.el --- emacs book readers -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; pdf-tools 强大的且性能良好的阅读器，需要编译
;;

;;; Code:
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)

  :config
  (pdf-tools-install :no-query)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  ;; 编译后自动更新 PDF
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  )

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))



(provide 'my-ebook-reader)

;;; my-ebook-reader.el ends here
