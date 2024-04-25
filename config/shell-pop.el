;;; shell-pop.el                                     -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Package to open a small `shell' buffer below





(use-package shell-pop
  :defer t
  :ensure t
  
  :custom
  (shell-pop-window-size 20)
  (shell-pop-window-position "bottom"))
