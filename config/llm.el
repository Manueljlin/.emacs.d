;;; llm.el                                           -*- lexical-binding: t -*-
;;;
;;; Configuration for Ellama, an Ollama client for Emacs.
;;; At least it respects my privacy, even if it'll end up replacing me.
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(use-package ellama
  :ensure t

  :preface
  (require 'llm-ollama)
  (require 'llm-openai)

  (setq llm-warn-on-nonfree nil)

  (defvar
    mn/llm::phi3
    (make-llm-ollama :chat-model "phi3:instruct"
                     :embedding-model "phi3:instruct"))
  (defvar
    mn/llm::wizardlm2
    (make-llm-ollama :chat-model "wizardlm2:7b-q4_1"
                     :embedding-model "wizardlm2:7b-q4_1"))

  (let ((key (with-current-buffer (generate-new-buffer "groq-api-key-tmp")
               (insert-file-contents (concat user-emacs-directory "/groq-api-key"))
               (buffer-string))))
    (defvar
     mn/llm::groq-llama3-70b
     (make-llm-openai-compatible :key key
                                 :chat-model "llama3-70b-8192"
                                 :embedding-model "llama3-70b-8192"
                                 :url "https://api.groq.com/openai/v1/")))
  (defun mn/pull-llms ()
    "Pull LLMs with Ollama CLI"
    (interactive)
    (mapc
     (lambda (llm)
       (let ((model-name (llm-ollama-chat-model (symbol-value llm))))
         (message (concat "Pulling " model-name))
         (async-shell-command (concat "ollama pull " model-name))))
     '(mn/llm::phi3 mn/llm::wizardlm2)))

  :config
  (setopt ellama-language "English")

  (setopt ellama-provider mn/llm::groq-llama3-70b)
  ;; (setopt ellama-provider mn/llm::phi3)
  (setopt ellama-naming-provider mn/llm::phi3)

  ;; (setopt ellama-provider
  ;;         (make-llm-ollama :chat-model "phi3:instruct"
  ;;                          :embedding-model "phi3:instruct"))
  ;; (setopt ellama-naming-provider
  ;;         (make-llm-ollama :chat-model "phi3:instruct"
  ;;                          :embedding-model "phi3:instruct"))

  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm))
