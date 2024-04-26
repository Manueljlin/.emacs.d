;;; saner-emacs.el                                   -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Misc. settings to improve Emacs default UX
;;; Mostly stolen from "ideasman32/emacs-for-vimmers". Thanks <3



;; TODO: fix indentation
(add-hook
 'window-setup-hook
 (lambda ()
   
   ;;; Windows specific settings
   (when (eq system-type 'windows-nt)
     (setq default-directory (format "C:/Users/%s" user-login-name)))
   
   ;;; macOS specific settings
   (when (eq system-type 'darwin)
     (setq mac-command-modifier       'control    ; Make the command key work as control
           mac-right-option-modifier  'none       ; Make Emacs not use AltGR as Meta and for US Intl instead.
           native-comp-driver-options '("-Wl,-w") ; Hide warning with Emacs<29 native-comp compiled with Xcode
           dired-use-ls-dired         nil))       ; Stop dired warning (tries to use the missing --dired flag)
   
   ;;; UI
   (setq frame-resize-pixelwise             t       ; Per pixel OS window resize
         window-resize-pixelwise            t
         use-dialog-box                     nil     ; Always use Emacs dialogs
      
         ring-bell-function                 'ignore ; shutup.el

         column-number-indicator-zero-based nil     ; Make column number 1 index based

         message-truncate-lines             t       ; Don't expand echo area with
                                                    ; long ass messages
      
         text-scale-mode-step               1.1)    ; Make zoom steps smaller
  
   ;; Be able to answer prompts with y and n instead
   (defalias 'yes-or-no-p 'y-or-n-p)

   ;; Set C--, C-= to CUA-style text scale/zoom shortcuts
   (global-set-key [?\C-\=] 'text-scale-increase)
   (global-set-key [?\C-\-] 'text-scale-decrease)

   ;; Scroll wheel text scale/zoom shortcuts
   ;; (global-set-key [C-wheel-up] 'text-scale-increase)
   ;; (global-set-key [C-wheel-down] 'text-scale-decrease)
   
   ;; Make the escape key be equivalent to C-g, I think
   ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
   
   ;; Indicate empty lines, kind of like Vim's tilde (~)
   ;; (setq-default indicate-empty-lines 1)
   (add-hook
    'prog-mode-hook
    #'(lambda ()
        (setq indicate-empty-lines 1)))

   ;; Define a new bitmap to that of a tilde
   (progn
     (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
     (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))
   (set-fringe-bitmap-face 'tilde 'font-lock-comment-face)



  ;;; Scrolling
  (setq ;scroll-margin                   2             ; +2 lines ahead on scroll, kinda buggy with touchpad + pixel-scroll
        scroll-conservatively           scroll-margin ; don't recenter (???)

        ;; Screen specific (PgUp/PgDown or C-v/M-v)
        scroll-preserve-screen-position t             ; keep pos when scrolling by screen
        scroll-error-top-bottom         t             ; move cursor to top/bottom even if already viewing top/bottom

        ;; Mouse specific
        mouse-wheel-scroll-amount '(1 ((shift) . 6))) ; Shift + scroll -> 6 lines


  (unless (version<= emacs-version "28.2")
    (pixel-scroll-precision-mode))

  ;;; Undo
  (setq undo-limit        (* 1024 1024 64)  ; increase undo limits to 64mb
        undo-strong-limit (* 1024 1024 96)) ; increase strong limits to 96mb

;;; Encoding

   ;; Use UTF-8 as much as possible
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)

  ;;; Show matching paren instantly
  (setq-default show-paren-delay 0)))
