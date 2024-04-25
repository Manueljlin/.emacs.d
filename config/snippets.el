;;; Snippets                                         -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; YASnippet
;;; Template system for Emacs

(use-package yasnippet
  :ensure t
  :defer t                              ; deferred until company requests it as
                                        ; a backend
  :config (yas-global-mode))




;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Snippets

;; Doom Snippets
;; Snippets for lots of different modes. Not on MELPA
(use-package doom-snippets
  :after yasnippet

  :ensure (:type git
           :host github
           :repo "doomemacs/snippets"
           :branch "master"))


;; alternative to doom-snippets. disabled because there is a lot of overlap
;; between the two, but might be useful in the future
;; (use-package yasnippet-snippets
;;   :after yasnippet)
