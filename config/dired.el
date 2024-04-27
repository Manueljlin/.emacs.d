;;; Dirvish                                          -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(use-package dirvish
  :ensure t
  :defer t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("g" "~/Git" "Git repos"
      "h" "~"     "Home")))
  
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-subtree-always-show-state t)
  (setq dirvish-attributes
        '(;; file-time
          ;; file-size                     ; show file size
          ;; collapse                      ; show nested directories
          subtree-state                 ; show > v icons in tree style dir toggle
          vc-state                      ; show uncommited git changes in fringe
          git-msg))                      ; show last commit name
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("M-<left>"  . dirvish-history-go-backward)
   ("M-<right>" . dirvish-history-go-forward)
   ("TAB"       . dirvish-subtree-toggle)))


(use-package dired-x
  :ensure nil
  :defer t
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Additional syntax highlighting for dired
(use-package diredfl
  :ensure t
  :defer t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))
