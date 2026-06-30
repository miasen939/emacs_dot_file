;;; my-ebook-reader.el --- emacs book readers -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init (pdf-tools-install :no-query)
  :config
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
