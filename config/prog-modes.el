;;; Programming modes and completion                 -*- lexical-binding: t -*-
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Treesit-auto

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; LSP Config

(use-package eglot
  :defer 2
  :ensure nil)
  ;; :custom (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))


;;; Eglot go brr
;;; requires `emacs-lsp-booster' in $PATH
(use-package eglot-booster
  :after  eglot
  :config (eglot-booster-mode)
  :ensure (:type git
           :host github
           :repo "jdtsmith/eglot-booster"))



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Web

;;; TS
(use-package typescript-ts-mode
  :ensure nil
  :hook eglot-ensure
  :mode ("\\.ts\\'" "\\.mts\\'" "\\.cts\\'"))


;;; TSX
(use-package tsx-ts-mode
  :ensure nil
  :hook eglot-ensure
  :mode "\\.tsx\\'")


;;; JS
(use-package js-ts-mode
  :ensure nil
  :hook eglot-ensure
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.cjs\\'"))

  
;;; TSX
(use-package jsx-ts-mode
  :ensure nil
  :hook eglot-ensure
  :mode "\\.jsx\\'")


;;; TODO: Vue


;;; TODO: Svelte



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Markdown
;;; (not really programming but eh)
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; C, C++ etc

;;;
;;; C
(use-package c-ts-mode
  :ensure nil
  :mode ("\\.c\\'" "\\.h\\'")
  :hook (c-ts-mode . (lambda () (mn/setup-column-indicator-by-arg 80)))
  :custom
  ;; https://emacs.stackexchange.com/a/78291
  ;; https://www.reddit.com/r/emacs/comments/12ofm8k/comment/kx8m0p5
  ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter#n120
  (c-ts-mode-indent-style
   #'(lambda ()
       ;; based on c-ts-mode--indent-styles
       ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter#n255
       `(((match nil "parameter_list" nil 1 1)       standalone-parent c-ts-mode-indent-offset)
         ((match ")" "parameter_list" nil nil nil)   standalone-parent 0)
         ((match nil "parameter_list" nil 2 nil)     (nth-sibling 1)   0)
         ((and no-node (parent-is "parameter_list")) (nth-sibling 1)   0)
         
         ,@(alist-get 'linux (c-ts-mode--indent-styles 'c))))))


                              
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Rust

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'"))



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; JVM

;;;
;;; Java
(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'")



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Sadness

;;;
;;; TODO PHP
;;; (phpactor)

;;; Laravel
(defun mn/install-laravel-ide-helper ()
  "Install laravel-ide-helper in the current Laravel 8+ project"
  (interactive)
  (async-shell-command "composer require --dev barryvdh/laravel-ide-helper"))



;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Lisps

;;;
;;; Emacs Lisp
(use-package elisp-mode
  :ensure nil
  :after doom-themes
  :hook
  ;;; Why tf does this add 100ms to init time?! It isn't even ran!!
  (emacs-lisp-mode
   . (lambda ()
       (mn/setup-column-indicator-by-arg 80)
       (when (and (fboundp 'doom-color) (fboundp 'doom-darken))
         (mapc
          (lambda (face-color-pair)
            (let ((face-from-pair (car face-color-pair))
                  (color-from-pair (doom-darken (cdr face-color-pair) 0.3)))
              (face-remap-add-relative face-from-pair :foreground color-from-pair)))
          `((rainbow-delimiters-depth-1-face . ,(doom-color 'fg))
            (rainbow-delimiters-depth-2-face . ,(doom-color 'blue))
            (rainbow-delimiters-depth-3-face . ,(doom-color 'orange))
            (rainbow-delimiters-depth-4-face . ,(doom-color 'green))
            (rainbow-delimiters-depth-5-face . ,(doom-color 'cyan))
            (rainbow-delimiters-depth-6-face . ,(doom-color 'yellow))
            (rainbow-delimiters-depth-7-face . ,(doom-color 'teal))
            (rainbow-delimiters-depth-8-face . ,(doom-color 'violet))
            (rainbow-delimiters-depth-9-face . ,(doom-color 'magenta))))))))

;;;
;;; Common Lisp
(use-package slime
  :ensure t
  :hook common-lisp-mode
  :config
  (setq inferior-lisp-program "sbcl"))

;;;
;;; Shared
(use-package parinfer-rust-mode
  :ensure t
  :hook emacs-lisp-mode
  ; TODO: hook up common-lisp-mode, lisp-mode
  :config
  (setq parinfer-rust-auto-download t)
  (setq parinfer-rust-disable-troublesome-modes t)
  (setq parinfer-rust-dim-parens nil))





;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; PSX Devel

;; CMake? Haha, that's funny. Emacs.
;; This is based on the scripts by `prochazkaml' in PSXDEV.net. Thank you!
(defun mn/psx-psyq/build ()
  "Build the current PlayStation project using Psy-Q in a 32-bit Wine bottle"
  (interactive)
  (message "PSX-dev/Psy-Q: Running %s in %s bottle..."
           (propertize "make32" 'face 'font-lock-constant-face)
           (propertize "Psy-Q"  'face 'bold))
  ;; set PSX dev. envvars. from `make32.bat' and compile
  (let (project-root (project-root))
    (async-shell-command
     (format
      "flatpak run --command=bottles-cli com.usebottles.bottles -b psy-q -i \"%\""
      (concat "wine cmd /c " project-root "/tools/make32.bat")))
    
    ;; move `main.exe' to `root/MAIN.EXE'
    (rename-file (concat project-root "main.exe")
                 (concat project-root "/root/main.exe"))
    ;; make iso (mkpsxiso is a replacement of the original BUILDCD)
    (async-shell-command
     (format "%/mkpsxiso -y %"
             project-root
             (concat project-root "/build.xml")))))

(defun mn/psx-psyq/run ()
  "Run the current build in PCSX-Redux"
  (interactive)
  ;; (async-shell-command "mednafen *.cue")
  (message "PSX-dev/Psy-Q: Running in PCSX-Redux")
  (let*
      ((project-root (project-root))
       (build-info   (libxml-parse-xml-region (f-read-text (concat project-root "/build.xml"))))
       (cue-sheet    (xml-get-prop (car build-info)))
       (run-cmd      (format "%/pcsx-redux -iso=%"
                             project-root
                             (shell-quote-argument
                              (concat project-root (if cue-sheet
                                                       cue-sheet
                                                     "/output.cue"))))))
     (async-shell-command run-cmd)))

(defun mn/psx-psyq/build-then-run ()
  "Build current PlayStation project, then run"
  (interactive)
  (mn/psx-psyq/build)
  (mn/psx-psyq/run))

