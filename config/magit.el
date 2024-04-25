;;; Magit                                            -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; A pretty powerful Git client
;;; https://magit.vc/manual/magit.html#Getting-Started






;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Start magit status buffer

;; C-x g  ----  Open status buffer, to display current repo info




;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Status buffer shortcuts:

;; g      ---- Update git status
;; s      ---- Stage changes (git add .)

;; c      ---- Commit commands and arguments
;; -> c c ---- New commit. Type commit name in new buffer and pres C-c C-c to commit

;; P      ---- Push commands (that's an uppercase P indeed)
;; -> P p ---- Push to remote


(use-package magit
  :ensure nil
  :defer t)
