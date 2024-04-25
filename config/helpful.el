;;; Helpful  -*- lexical-binding: t -*-
;;; Replaces some of Emacs' built in help commands to be more... helpful.



(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ;; ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))
