;;; Line highlight                                   -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(use-package hl-line
  :ensure nil

  :custom
  (hl-line-sticky-flag . nil)           ; Only show on active buffers
  (global-hl-line-sticky-flag . nil)    ; Only show on active buffers
  
  :hook
  (prog-mode          . hl-line-mode)
  (text-mode          . hl-line-mode)
  (markdown-mode      . hl-line-mode)
  (conf-mode          . hl-line-mode))
