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

  (defun mn/create-ollama-docker ()
    (interactive)
    (async-shell-command
     (concat "docker volume create ollama-data && "
             "docker volume create ollama-config"))
    (async-shell-command
     (concat "docker run -d "
             "--device /dev/kfd --device /dev/dri "
             "-v ollama-data:/root/.ollama/models "
             "-v ollama-config:/root/.ollama/config "
             "-p 11434:11434 "
             "--name ollama "
             "-e HSA_OVERRIDE_GFX_VERSION=10.3.0 "
             "ollama/ollama:rocm")))

  (defun mn/start-ollama-docker ()
    (interactive)
    (async-shell-command "docker start -a ollama"))

  (defun mn/stop-ollama-docker ()
    (interactive)
    (async-shell-command "docker stop ollama"))

  (defun mn/nuke-ollama-docker ()
    (interactive)
    (async-shell-command "docker rm ollama"))

  (defun mn/pull-ollama-llms ()
    "Pull LLMs with Ollama CLI"
    (interactive)
    (mapc
     (lambda (llm)
       (let ((model-name (llm-ollama-chat-model (symbol-value llm))))
         (message (concat "Pulling " model-name))
         (async-shell-command (concat "docker exec -it ollama ollama pull " model-name))))
     '(mn/llm::phi3 mn/llm::wizardlm2)))

  :config
  (setopt ellama-language "English")

  (setopt ellama-provider mn/llm::wizardlm2) ; default LLM
  (setopt ellama-naming-provider mn/llm::wizardlm2)
  (setopt ellama-providers '(("Phi-3 3.8B Instruct"   . mn/llm::phi3)
                             ("WizardLM2 7B Instruct" . mn/llm::wizardlm2)
                             ("Groq Llama 3 70B"      . mn/llm::groq-llama3-70b))))
