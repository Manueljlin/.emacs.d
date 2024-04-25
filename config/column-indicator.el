;;; Column indicator                                 -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(defun mn/setup-column-indicator (width)
  "Set column width indicator in current buffer by entering width number"
  (interactive "nType column width to use: ")
  (mn/setup-column-indicator-by-arg width)
  (message "Manually set %d as column width" width))

(defun mn/setup-column-indicator-by-arg (width)
  "Set column width indicator in current buffer by calling function directly"
  ;; Emacs column count by default is 0 index. Despite having changed that
  ;; earlier to be 1 index based like VSCode, IDEA et al, we still have
  ;; to substract 1 from the column width here.
  (setq-local display-fill-column-indicator-column (- width 1))
  (display-fill-column-indicator-mode))
