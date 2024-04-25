;;; Theming                                          -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'doom-ayu-dark t)
  ;; (load-theme 'doom-ayu-mirage t)
  
  (doom-themes-org-config)
  
  ;; (set-face-attribute 'mode-line nil
  ;;                     :box '(:line-width 10 :color "gray20"))

  ;; (setq-default
  ;;  mode-line-format
  ;;  (cons
  ;;   (propertize "\u200b" 'display
  ;;               '((raise -0.5)
  ;;                 (height 1.6)))
  ;;   mode-line-format))
  
  :custom-face
  (fill-column-indicator ((t (:foreground ,(doom-color 'base0)))))

  :ensure (:type git
           :host github
           :repo "doomemacs/themes"
           :branch "master"))



;;; Solaire
;;; Use darker backgrounds on non editable buffers

(use-package solaire-mode
  :after doom-themes
  
  :config
  ;; (advice-add
  ;;  'solaire-mode-fix-minibuffer :around
  ;;  (lambda (og-fn &rest args) (apply og-fn nil)))
  
  (solaire-global-mode +1)
  
  :ensure (:type git
           :host github
           :repo "hlissner/emacs-solaire-mode"
           :branch "master"))
