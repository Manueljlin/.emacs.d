;;; emmet-mode  -*- lexical-binding: t -*-
;;; Minor mode to add emmet support. Might replace with emmet-ls


(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         ;; (html-mode . emmet-mode)
         ;; (css-mode . emmet-mode)
         (web-mode . emmet-mode)))
