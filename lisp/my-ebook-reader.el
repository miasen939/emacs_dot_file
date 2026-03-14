;;; my-ebook-reader.el --- emacs book readers -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; pdf-tools 强大的且性能良好的阅读器，需要编译
;;

;;; Code:
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)

  :config
  (pdf-tools-install :no-query))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))



(provide 'my-ebook-reader)

;;; my-ebook-reader.el ends here
