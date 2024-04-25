;;; init.el                                          -*- lexical-binding: t -*-
;;;
;;;
;;;   _    ___ _             _
;;;   _ ___ __ ___  __    _ ___
;;;   __   _     ___    __  ___
;;;       _           ___     _
;;;      _  _ __             _
;;;      ___   __            _
;;;            __           _
;;;             _      _   _
;;;            _      _    _
;;;               _  _    _
;;;           __  ___
;;;          _   _ _     _
;;;         _   _
;;;       _    _
;;;      _    _
;;;     _
;;;   __
;;;  
;;;
;;;
;;; Potato Emacs
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; This configuration depends on Emacs 29+.
;;;
;;; Tips:
;;;  * C-x C-e  ----  Evaluate expression
;;;  * C-h f    ----  Describe function
;;;  * C-h v    ----  Describe variable


(require 'json)
(require 'treesit)


(message
 "early-init.el loaded in %s seconds, starting init.el..."
 (format "%.2f seconds"
         (float-time
          (time-subtract
           (current-time)

           before-init-time))))


;;; Imports
(defmacro with-timer (name &rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f" ,name (float-time (time-since time)))))

(mapc
 (lambda (config-file-name)
   (with-timer
    config-file-name
    (load (concat (locate-user-emacs-file "config/") config-file-name ".el"))))
 '(;; essentials
   "package-management"                 ; elpaca, use-package
   "optimization"                       ; gcmh
   ;; "exec-path"                          ; make macOS and Emacs daemon see $PATH
   ;; TODO: ^^ see how to conditionally load it only (when (memq window-system '(mac ns x))
   ;; to get small speed up

   ;; modal editing
   "meow"

   ;; file editing
   "indentation"                        ; editorconfig & dtrt-indent
   "line-numbers"                       ; hooks&config for display-line-numbers-mode
   "line-highlight"                     ; hooks&config for hl-line-mode

   ;; ux improvements
   "saner-emacs"                        ; misc config for built in modes
   "vertico"                            ; vertico marginalia orderless consult
   "rainbow-delimiters"                 ; color matching braces () {} []
   "helpful"                            ; add improvements to help commands
   "window-mgmt"                        ; ace-window TODO: consider replacing with windmove
   
   ;; theming
   "faces"                              ; set main faces (fonts)
   "themes"                             ; doom-themes + solaire-mode
   "prog-modes"

   ;; ui modifications
   "dired"
   "modeline"
   "git-gutter"))
 
   ;; "magit"
   ;; "eat"
   



;;; Show init time and additional info
(defun display-startup-echo-area-message ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format "%.2f seconds"
           (float-time
            (time-subtract
             (current-time)
             before-init-time)))
   gcs-done))



(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq file-name-handler-alist default-file-name-handler-alist)
   (makunbound 'default-file-name-handler-alist)))


(provide 'init)
;;; init.el ends here
