;;; Emacs optimizations                              -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Non-early-init.el optimizations go here
;;; Mostly stolen from "doomemacs/develop/core/core.el". Thanks <3


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Misc

;; Don't do a second case insensitive pass over `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Don't ping things that look like domain names (?! aight)
(setq ffap-machine-p-known 'reject)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Rendering

;; Disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

;; Update the UI less often
(setq idle-update-delay 1.0)            ; default is 0.5

;; PGTK specific timeout to add latency to frame operations
;; Should theoretically make `lsp-ui', `company-box' etc feel snappier
;; Untested, comes from Doom Emacs
(setq pgtk-wait-for-event-timeout 0.001)

;; Reduce rendering work by not rendering cursors or regions
;; in unfocused windows
;; (setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Better performing scrolling at the expense of some inaccurate syntax
;; highlighting, which fixes itself anyways.
(setq fast-but-imprecise-scrolling t)

;; Disable font compacting for better Windows performance
;; (this config is designed to be cross platform!)
(setq inhibit-compacting-font-caches t)

;; Don't do fontification while receiving input
(setq redisplay-skip-fontification-on-input t)


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; Garbage Collector Magic Hack
;;; Defer the garbage collection to idle times

(use-package gcmh
  :ensure t
  :hook (emacs-startup)

  
  :custom
  (gcmh-idle-delay             'auto)   ; default is 15s
  (gchm-auto-idle-delay-factor 20)
  (gcmh-high-cons-threshold (* 16 1024 1024))

  :config
  (gcmh-mode 1))
