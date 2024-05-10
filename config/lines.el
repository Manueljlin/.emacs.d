;;; Line numbers and highlights                      -*- lexical-binding: t -*-
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(use-package display-line-numbers
  :ensure nil

  :hook
  (prog-mode          . display-line-numbers-mode)
  ;; (text-mode          . display-line-numbers-mode)
  (markdown-mode      . display-line-numbers-mode)
  (conf-mode          . display-line-numbers-mode)

  :config
  ;;; use vi-like relative line numbers
  ;; (setq display-line-numbers-type 'relative)

  ;;; precalculate width of line numbers (slow)
  ;; (setq display-line-numbers-width-start   t)
  
  ;;; use explicit width (fast)
  (setq-default display-line-numbers-width 4))



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
