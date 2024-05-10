;;; Git gutter                                       -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; https://ianyepan.github.io/posts/emacs-git-gutter/


(use-package git-gutter
  :ensure t
  :hook
  ((prog-mode     . git-gutter-mode)
   (markdown-mode . git-gutter-mode)
   (conf-mode     . git-gutter-mode))

  :custom
  (git-gutter:update-interval 0.02)
  (git-gutter:hide-gutter     nil))


(use-package git-gutter-fringe
  :if (display-graphic-p)
  :ensure t
  
  :after git-gutter
  
  :config
  (define-fringe-bitmap 'git-gutter-fr:added    [192] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [192] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [128 192 224 240] nil nil 'bottom))
