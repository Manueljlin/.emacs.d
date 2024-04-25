;;; window-mgmt.el                                   -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;;; Ace-window
(use-package ace-window
  :disabled t                           ; using windmove-*, windmive-swap-states-*
  :ensure t
  :after doom-themes
  :custom-face
  (aw-leading-char-face                 ; face for number
   ((t (:foreground ,(doom-color 'teal)
        :weight bold))))
  
  (aw-background-face                   ; face for window content
   ((t (:foreground ,(doom-color 'base1))))))

  
