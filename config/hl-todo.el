;;; hl-todo.el                                       -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; straight up stolen from doom emacs lol


(use-package hl-todo
  :ensure t

  :hook ((prog-mode . hl-todo-mode)
         (conf-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))

  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("HACK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold)
     ;; ("INFO" font-lock-keyword-face bold)
     ("BUG" error bold))))
