;;; Corfu                                            -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




(use-package corfu
  :after orderless
  
  :hook ((prog-mode   . corfu-mode)
         (elisp-mode  . corfu-mode)
         (shell-mode  . corfu-mode)
         (eshell-mode . corfu-mode)
         (corfu-mode  . corfu-popupinfo-mode))
  
  :custom
  (tab-always-indent 'complete)         ; Map TAB to indentation+completion
  (corfu-auto t)                        ; Enable auto completion
  (corfu-separator ?\s)                 ; Orderless field separator
  (corfu-auto-delay 0.2)                ; Auto completion list delay
  (corfu-auto-prefix 3)                 ; Start autocompletions after 3 chars
  (corfu-quit-no-match 'separator)      ; Quit if there's no match
  (corfu-popupinfo-delay .4)            ; Aulto completion inline docs delay
  (corfu-scroll-margin 2)               ; Use 2 lines scroll margin

  :ensure (:type git
           :host github
           :repo "minad/corfu"
           :branch "main"
           :files ("corfu.el"
                   "extensions/corfu-popupinfo.el")))


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Terminal specific config

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :after corfu

  :config
  (corfu-terminal-mode +1)
  
  :ensure (:type git
           :host codeberg
           :repo "akib/emacs-corfu-terminal"
           :branch "master"))


;; todo: https://github.com/jdtsmith/kind-icon
;;       or/and https://github.com/rainstormstudio/nerd-icons.el


;; (use-package nerd-icons
;;   :custom (nerd-icons-font-family "PragmataPro Mono Liga"))



;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default)
;;   (kind-icon-use-icons t)
;;   ;; https://user-images.githubusercontent.com/34292770/223941409-91850da5-4ef1-4c90-a834-d526836d873d.png
;;   (kind-icon-mapping
;;    '((array          " " :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
;;      (boolean        " " :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
;;      (color          " " :icon "symbol-color"       :face success                          :collection "vscode")
;;      (command        " " :icon "chevron-right"      :face default                          :collection "vscode")
;;      (constant       " " :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
;;      (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
;;      (constructor    " " :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;      (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
;;      (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
;;      (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
;;      (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
;;      (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
;;      (file           " " :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
;;      (folder         " " :icon "folder"             :face font-lock-doc-face               :collection "vscode")
;;      (function       " " :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;      (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
;;      (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
;;      (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
;;      (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
;;      (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
;;      (module         " " :icon "file-code-outline"  :face font-lock-preprocessor-face)
;;      (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
;;      (operator       " " :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
;;      (param          " " :icon "gear"               :face default                          :collection "vscode")
;;      (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
;;      (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
;;      (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
;;      (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
;;      (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
;;      (text           " " :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
;;      (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
;;      (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
;;      (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
;;      (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
;;      (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
;;      (t              " " :icon "question"           :face font-lock-warning-face           :collection "vscode")))
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

