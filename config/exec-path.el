;;; exec-path-from-shell  -*- lexical-binding: t -*-



;; Use path in macOS or potentially linux daemon like systemd
;; I use this for installing lsp stuff automagically
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t

  :preface
  (defun mn/deferred-exec-path-from-shell ()
    (exec-path-from-shell-initialize)
    (dolist (envvar '("PATH"))
      (add-to-list 'exec-path-from-shell-variables envvar))
    (message "~~~~  exec-path-from-shell :: got env variables!  ~~~~"))
  
  ;; :hook lsp
  :hook (lsp-mode . mn/deferred-exec-path-from-shell)
  
  ;; :config
  ;; (dolist (envvar '("PATH"))
  ;;   (add-to-list 'exec-path-from-shell-variables envvar))
  ;; (exec-path-from-shell-initialize)
  )
