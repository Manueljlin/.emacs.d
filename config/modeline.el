;;; Modeline                                         -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Vanilla modeline

(setq mn/modeline-format "%l:%C")
(setq mode-line-percent-position nil ; nuke `Top'|`Bot'|`50%'
      mode-line-position-column-line-format '(mn/modeline-format)) ; Make column number 1 index based
(column-number-mode t)



;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(use-package doom-modeline
  :disabled t
  :hook (elpaca-after-init . doom-modeline-mode)

  :custom
  (doom-modeline-height          28)    ; Set custom height
  (doom-modeline-icon            nil)   ; Use all-the-icons
  (doom-modeline-major-mode-icon t)     ; Display icon for major mode
  (doom-modeline-modal           nil)   ; Show/hide Meow mode
  (doom-modeline-indent-info     t)     ; Show indentation info
  (doom-modeline-minor-modes     nil)   ; Hide minor modes
  (doom-modeline-bar-width       2)     ; Width of vertical line on the left
  
  :config
  ;; (set-face-background 'doom-modeline-bar (face-background 'mode-line))          ; Remove mode-line bar on active windows
  (set-face-background 'doom-modeline-bar-inactive (face-background 'mode-line)) ; Remove mode-line bar on inactive windows
  (column-number-mode))               ; Show column number on modeline


(use-package mini-echo
  :hook (elpaca-after-init . mini-echo-mode)
  
  :custom-face
  (mini-echo-minibuffer-window ((t (:background ,(doom-color 'bg-alt)))))
  (mini-echo-window-divider ((t (:foreground ,(doom-color 'base0)))))
  (fill-column-indicator ((t (:foreground ,(doom-color 'base0))))) ; this should be moved somewhere else

  :config
  ;; workaround for line wrap in -nw
  (when (not (display-graphic-p))
    (setq mini-echo-right-padding 1))
  (setq mini-echo-position-format mn/modeline-format)

  ;; (setq mini-echo-default-segments
  ;;       '(:long ("major-mode" "buffer-name" "mn-buffer-position" "eglot" "vcs")
  ;;         :short ("major-mode" "buffer-name-short" "mn-buffer-position" "vcs")))
  
  ;; (setq mini-echo-rules
  ;;       '((treemacs-mode :long  (("major-mode" . 0))
  ;;                        :short (("major-mode" . 0)))))

  ;; https://github.com/liuyinz/mini-echo.el#usage
  
  :ensure (:type git
           :host github
           :repo "liuyinz/mini-echo.el"
           :branch "master"
           :files ("mini-echo.el",
                   "mini-echo-segments.el")))
