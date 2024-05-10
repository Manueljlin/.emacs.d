;;; early-init.el                                     -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;; Set window color to black while loading to avoid FOUC
(push '(background-color . "#1a1b26") initial-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set modeline colors while loading to avoid FOUC
(set-face-attribute 'mode-line-active nil
                    :box nil
                    :background "#171822")
(set-face-attribute 'mode-line-inactive nil
                    :box nil
                    :background "#1a1b26")

(setq
 ;;
 ;; Ignore random CLI arguments
 command-line-x-option-alist nil

 ;;
 ;; GC config
 gc-cons-threshold       most-positive-fixnum ; Stop GC at startup. MUST be reset later to avoid freezes. gcmh will manage this
 read-process-output-max (* 16 1024 1024)     ; LSP-mode performance tweak (16mb)

 ;;
 ;; Lockfiles, autosave and backups
 create-lockfiles nil                                                   ; Stop Emacs from creating .#foo.txt if editing foo.txt
 backup-directory-alist         `((".*" . ,temporary-file-directory))   ; Save backups to /tmp/
 auto-save-file-name-transforms `((".*"   ,temporary-file-directory t)) ; Save autosaves to /tmp/

 delete-old-versions t                  ; Automatically delete old backup files

 ;;
 ;; Avoid auto resize of window while loading
 ;; (*Significant* speedup, about 100ms)
 frame-inhibit-implied-resize t

 ;; Optimizations to jit lock
 ;; From https://tychoish.com/post/towards-faster-emacs-start-times/
 jit-lock-stealth-time nil
 jit-lock-defer-time   nil
 jit-lock-defer-time   0.05
 jit-lock-stealth-load 200

 ;;
 ;; Hide modeline and headerline while loading
 mode-line-format   nil
 header-line-format nil

 ;;
 ;; Don't load the startup screen
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 inhibit-default-init t

 ;;
 ;; Make the initial mode for *scratch* fundamental-mode instead of
 ;; lisp-interaction-mode, so it starts faster
 initial-major-mode 'fundamental-mode

 ;;
 ;; Change default scratch message
 initial-scratch-message "
   _    ___ _             _
   _ ___ __ ___  __    _ ___
   __   _     ___    __  ___
       _           ___     _
      _  _ __             _
      ___   __            _
            __           _
             _      _   _
            _      _    _
               _  _    _
           __  ___
          _   _ _     _
         _   _
       _    _
      _    _
     _
   __

"

 ;;; Don't load `package.el' as we'll be using `elpaca'
 package-enable-at-startup  nil
 package--init-file-ensured nil
 package-quickstart         nil)

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;;; Make titlebar color on macOS match the theme
(if (not (featurep 'ns))
    (push '(ns-appearance . nil) default-frame-alist)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))



;;; If Emacs build has native comp enabled...
(when (featurep 'native-compile)
  ;; shut up with the "*Warning* i'm compiling and the variable is unused/
  ;; line is too long" whatevers
  ;; Annoying errors got me acting unwise
  (setq native-comp-async-report-warnings-errors nil))


;;; Emacs' custom-file overrides my custom-set-faces, so I have to nucc the
;;; custom file in order for my config to work properly on macOS too.
(setq custom-file (make-temp-file "emacs-custom-tmp"))
