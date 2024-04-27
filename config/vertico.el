;;; vertico.el                                       -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(add-hook
 'minibuffer-setup-hook
 (lambda ()
   (set-window-fringes (selected-window) 0 0)
   (solaire-mode -1)
   ;; (set-display-table-slot standard-display-table 0 ?\ )
   (setq-local
       default-text-properties '(line-spacing 0.35 line-height 1.25))))

;;; Mini-frame
;;; Move mini buffer up
(use-package mini-frame
  :disabled t
  ;; :ensure t
  :after doom-themes
  :hook (window-setup . (lambda () (mini-frame-mode)))
  
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
     `((top . -1)
       (width . 0.8)
       (left . 0.5)
       (left-fringe . 0)
       (right-fringe . 0)
       (child-frame-border-width . 0)
       (background-color . "#252635")
       (background-color . ,(doom-lighten (doom-color 'bg) 0.035))))))


;;; Vertico
;;; Better completion UI for the minibuffer (i.e for M-x)
(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-scroll-margin 2)
  :config
  (advice-add
   'vertico--format-candidate :around
   (lambda (og-fn &rest args)
     (let* ((candidate (apply og-fn args))
            (padded-candidate (concat "  " candidate)))
       (set-text-properties 0 2 (text-properties-at 0 candidate) padded-candidate)
       padded-candidate))))

;;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :hook (elpaca-after-init)
  :config
  (savehist-mode))


;;; A few more useful configurations...
;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
               (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator
                (car args)))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)


;;; Use orderless (fuzzy finding) completion
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;;; Marginalia
;;; Show annotation descriptions in minibuffer UI
;;; Equivalent of Ivy Rich
;; TODO: enable again
(use-package marginalia
  :ensure t
  :after corfu
  :custom
  (marginalia-max-relative-age 0)
  ;; (marginalia-align 'right)
  
  :init
  (marginalia-mode))


;;; Consult
;;; Provide enhanced UI versions of common Emacs commands
;;; Equivalent of Counsel
(use-package consult
  :ensure t
  :after corfu
  :bind (("C-c M-x"           . consult-mode-command)
         ("C-c h"             . consult-history)
         ("C-c k"             . consult-kmacro)
         ("C-c m"             . consult-man)
         ("C-c i"             . consult-info)
         ([remap Info-search] . consult-info)
         
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
         
         ;; Custom M-# bindings for fast register access
         ("M-#"   . consult-register-load)
         ("M-'"   . consult-register-store) ; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ; orig. yank-pop
         
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake) ; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line) ; orig. goto-line
         ("M-g M-g" . consult-goto-line) ; orig. goto-line
         ("M-g o"   . consult-outline) ; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line) ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ; needed by consult-line to detect isearch
         
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ; orig. next-matching-history-element
         ("M-r" . consult-history)) ; orig. previous-matching-history-element
    
    :hook (completion-list-mode . consult-preview-at-point-mode)

    :init (advice-add #'register-preview :override #'consult-register-window))


;;; TODO: Look if Embark might be interesting to have too
