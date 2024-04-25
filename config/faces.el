;;; Fonts                                            -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;;; A size 100 is equals to 10pt
;;; Available weights:
;;;  * ultra-bold
;;;  * extra-bold
;;;  * bold
;;;  * semi-bold
;;;  * normal
;;;  * semi-light
;;;  * light
;;;  * extra-light
;;;  * ultra-light

;;; Documentation:
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html

(defvar mn/default-font-family "PragmataPro Mono Liga")
(defvar mn/default-font-size 95)
(defvar mn/default-font-size-mac 142)
(defvar mn/default-font-weight 'normal)

(defvar mn/variable-pitch-font-family "Noto Sans")
(defvar mn/variable-pitch-font-size 115)
(defvar mn/variable-pitch-font-weight 'normal)

(defvar mn/variable-pitch-font-family-mac "Inter")
(defvar mn/variable-pitch-font-size-mac 150)

;; macOS specific tweaks, since the UI seems to calculate font sizes
;; differently
(when (eq system-type 'darwin)
  (setq mn/default-font-size          mn/default-font-size-mac
        mn/variable-pitch-font-family mn/variable-pitch-font-family-mac
        mn/variable-pitch-font-size   mn/variable-pitch-font-size-mac))

(add-hook 'window-setup-hook #'mn/deferred-set-face)

(defun mn/deferred-set-face ()
  "Runs during `emacs-startup-hook.'"
  (set-face-attribute 'default nil
                      :family mn/default-font-family
                      :height mn/default-font-size
                      :weight mn/default-font-weight)

  (set-face-attribute 'fixed-pitch nil
                      ;; Can't inherit all properties from 'default for some reason
                      :family mn/default-font-family
                      :height mn/default-font-size
                      :weight mn/default-font-weight)

  (set-face-attribute 'variable-pitch nil
                      :family mn/variable-pitch-font-family
                      :height mn/variable-pitch-font-size
                      :weight mn/variable-pitch-font-weight))
