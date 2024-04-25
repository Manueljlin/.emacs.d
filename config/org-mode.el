;;; Org mode                                         -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






(use-package org
  :ensure nil

  :preface
  (defun mn/org-mode-setup ()
    (org-indent-mode)                     ; Auto indent nested headers
    ;; (variable-pitch-mode t)               ; Use Sans Serif
    (auto-fill-mode 0)                    ; Disables automatic line breaking
    (visual-line-mode t)                  ; Enable word wrap
    (set-face-attribute 'org-code             nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block            nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-date             nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword  nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-checkbox         nil :inherit 'fixed-pitch) ; [ ] [x]
    (set-face-attribute 'org-block-begin-line     :inherit 'fixed-pitch)
    (set-face-attribute 'org-block-end-line       :inherit 'fixed-pitch))
  
  :hook (org-mode . mn/org-mode-setup)
  
  :config
  (setq
   ;; Hide the ellipsis in folded headers
   ;; org-hide-emphasis-markers t
   
   ;; Show this indicator in folded headers
   org-ellipsis "▾"))
   
  


;; Align org tables despite using (variable-pitch-mode t)
;; (use-package valign
;;   :hook (org-mode . valign-mode))

(use-package org-appear
  :ensure (:type git
            :host github
            :repo "awth13/org-appear")
  :hook org-mode
  :config (setq org-appear-autolinks      t
                org-appear-autosubmarkers t
                org-appear-autoentities   t
                org-appear-inside-latex   t))
 
