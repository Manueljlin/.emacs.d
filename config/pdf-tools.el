;;; pdf-tools.el                                     -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  
  ;; :after qpdf
  
  ;; :preface
  ;; (defun mn/pdf-tools-workaround-opaque ()
  ;;   "Replace PDF if text selection is opaque due to OCR with Tesseract"
  ;;   (interactive)
  ;;   (unless (equal (file-name-extension (buffer-file-name)) "pdf")
  ;;     (error "Buffer should visit a pdf file."))
  ;;   (unless (equal major-mode 'pdf-view-mode)
  ;;     (pdf-view-mode))
  ;;   ;; save file in QDF-mode
  ;;   (qpdf-run (list
  ;;              (concat "--infile="
  ;;                      (buffer-file-name))
  ;;              "--qdf --object-streams=disable"
  ;;              "--replace-input"))
  ;;   ;; do replacements
  ;;   (text-mode)
  ;;   (read-only-mode -1)
  ;;   (while (re-search-forward "3 Tr" nil t)
  ;;     (replace-match "7 Tr" nil nil))
  ;;   (save-buffer)
  ;;   (pdf-view-mode))
  
  :config
  (pdf-loader-install))
