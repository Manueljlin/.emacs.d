;;; Indentation settings                             -*- lexical-binding: t -*-
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;;;
;;; Indentation config priority: `editorconfig' > `dtrt-indent' > defaults
;;;
;;; Most of this file is ripped from from Doom Emacs' :module editorconfig
;;; github.com/doomemacs/doomemacs/blob/master/modules/tools/editorconfig/config.el
;;;



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Indentation defaults
;;; Will work as the fallback of dtrt-indent afaict

(setq-default
 indent-tabs-mode nil    ; Use soft tabs (spaces) instead of hard tabs
 tab-width 2)            ; Make tab character 2ch wide

;; Remember that modes often have their own indentation config



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Editorconfig

(use-package editorconfig
  :ensure t
  :hook (elpaca-after-init . editorconfig-mode))

;;; Automatically set indentation settings to the current one in file as
;;; fallback to missing editorconfig
(use-package dtrt-indent
  :ensure t)

(add-hook
 'editorconfig-after-apply-functions
 (lambda (props)
   "Adjust indentation if `editorconfig' hasn't changed it"
   (unless (and (gethash 'indent_style props)
                (gethash 'indent_size props))
     (message "No EditorConfig properties found, falling back to dtrt-indent")
     (dtrt-indent-mode 1))))



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Auto pair (){}[]""

(use-package electric
  :ensure nil
  :init
  (electric-indent-mode -1)
  (electric-pair-mode -1)
  :hook
  (((prog-mode
     markdown-mode
     conf-mode)
    . (lambda ()
        (electric-indent-local-mode 1)
        (electric-pair-local-mode 1)))
   (emacs-lisp-mode
    . (lambda ()
        (electric-indent-local-mode 1)
        ;; we're using parinfer
        (electric-pair-local-mode -1)))))



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Others

;;; Enable line wrapping, make wrapped lines adapt to the indentation level
(use-package adaptive-wrap
  :ensure t
  :hook
  (prog-mode . adaptive-wrap-prefix-mode)
  :config
  (setq adaptive-wrap-extra-indent 4))
