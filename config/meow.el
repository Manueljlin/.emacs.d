;;; Meow                                             -*- lexical-binding: t -*-
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(use-package meow
  :ensure t
  :preface
  ;; Reverse (t)ill and (f)ind, like Vim
  ;; https://github.com/meow-edit/meow/issues/382#issuecomment-1353971582
  (defmacro mn/meow-call-negative (form)
    `(let ((current-prefix-arg -1))
       (call-interactively ,form)))

  (defun mn/meow-negative-find ()
    "Find the previous N char read from minibuffer."
    (interactive)
    (mn/meow-call-negative 'meow-find))

  (defun mn/meow-negative-till ()
    "Backward till the previous N char read from minibuffer."
    (interactive)
    (mn/meow-call-negative 'meow-till))

  (defconst mn/cheatsheet-physical-layout-iris
    "
┏━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┓      ┏━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┓
┃   <AE01> │   <AE02> │   <AE03> │   <AE04> │   <AE05> ┃      ┃   <AE06> │   <AE07> │   <AE08> │   <AE09> │   <AE10> ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃      ┃┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┗━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┛      ┗━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┛
┏━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┓      ┏━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━┓
┃   <AD01> │   <AD02> │   <AD03> │   <AD04> │   <AD05> ┃      ┃   <AD06> │   <AD07> │   <AD08> │   <AD09> │   <AD10> ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃      ┃┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠──────────┼──────────┼──────────┼──────────┼──────────┃      ┃──────────┼──────────┼──────────┼──────────┼──────────┨
┃   <AC01> │   <AC02> │   <AC03> │   <AC04> │   <AC05> ┃      ┃   <AC06> │   <AC07> │   <AC08> │   <AC09> │   <AC10> ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃      ┃┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠──────────┼──────────┼──────────┼──────────┼──────────┃      ┃──────────┼──────────┼──────────┼──────────┼──────────┨
┃   <AB01> │   <AB02> │   <AB03> │   <AB04> │   <AB05> ┃      ┃   <AB06> │   <AB07> │   <AB08> │   <AB09> │   <AB10> ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┠┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃      ┃┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┼┄┄┄┄┄┄┄┄┄┄┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┃          │          │          │          │          ┃      ┃          │          │          │          │          ┃
┗━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┛      ┗━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┷━━━━━━━━━━┛
")

  (defconst mn/cheatsheet-layout-canary-ortho
    '((<AE01> "1" "!") (<AE02> "2" "@") (<AE03> "3" "#") (<AE04> "4" "$") (<AE05> "5" "%") (<AE06> "6" "^") (<AE07> "7" "&") (<AE08> "8" "*") (<AE09> "9" "(")  (<AE10> "0" ")")
      (<AD01> "w" "W") (<AD02> "l" "L") (<AD03> "y" "Y") (<AD04> "p" "P") (<AD05> "b" "B") (<AD06> "z" "Z") (<AD07> "f" "F") (<AD08> "o" "O") (<AD09> "u" "U") (<AD10> "'" "\"")
      (<AC01> "c" "C") (<AC02> "r" "R") (<AC03> "s" "S") (<AC04> "t" "T") (<AC05> "g" "G") (<AC06> "m" "M") (<AC07> "n" "N") (<AC08> "e" "E") (<AC09> "i" "I") (<AC10> "a" "A")
      (<AB01> "q" "Q") (<AB02> "j" "J") (<AB03> "v" "V") (<AB04> "d" "D") (<AB05> "k" "K") (<AB06> "x" "X") (<AB07> "h" "H") (<AB08> "/" "?") (<AB09> "," "<") (<AB10> "." ">")))

  (defun mn/meow-setup ()
    (meow-global-mode 1)
    (setq meow-cheatsheet-layout             mn/cheatsheet-layout-canary-ortho
          meow-cheatsheet-physical-layout    mn/cheatsheet-physical-layout-iris

          ;; By default, if after pressing the leader key you press an unmapped
          ;; character, it will insert it into the buffer for some reason. Don't.
          meow-keypad-self-insert-undefined  nil)
    
    (meow-motion-overwrite-define-key
     '("o" . meow-prev)
     '("n" . meow-left)
     '("e" . meow-next)
     '("i" . meow-right)
     
     '("r o" . windmove-up)
     '("r n" . windmove-left)
     '("r e" . windmove-down)
     '("r i" . windmove-right)
     '("R o" . windmove-swap-states-up)
     '("R n" . windmove-swap-states-left)
     '("R e" . windmove-swap-states-down)
     '("R i" . windmove-swap-states-right)

     '("/" . consult-line)
     ;; '("<escape>" . 'keyboard-escape-quit)
     '("<escape>" . ignore))



    
    ;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;;; Leader key

    ;; partially based on https://cheatography.com/stop-start/cheat-sheets/doom-emacs/
    (meow-leader-define-key
     ;;
     ;; Execute the original commands in MOTION state that have been overriden with SPC-letter
     '("o" . "H-o")
     '("n" . "H-n")
     '("e" . "H-e")
     '("i" . "H-i")
     '("r" . "H-r")

     
     ;;
     ;; Left side

     '("p" . project-find-dir)
     '("b" . consult-buffer)
     
     '("t" . dirvish-side)
     '("T" . dirvish-layout-toggle)

     '("d" . delete-window)
     '("k" . kill-this-buffer)
     '("K" . kill-buffer-and-window)

     ;; ~~ Right side ~~
     '("f" . split-window-vertically)
     '("n" . split-window-horizontally)

     '("z" . balance-windows)           ; Make all windows the same size
     
     ;; ---
     
     '("0" . meow-digit-argument)
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument))
    
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)

     ;; ~~~~ Canary Ortho ~~~~
     
     ;; ~~ Top row ~~
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("y" . meow-save)                   ; Save (Copy) to kill ring 
     '("Y" . meow-clipboard-save)         ; Copy to system clipboard
     ;; '("Y" . meow-sync-grab)
     '("p" . meow-yank)                   ; Yank (Paste) from kill ring
     '("P" . clipboard-yank)              ; Paste from system clipboard
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     
     ;; ---
     
     '("z" . meow-pop-selection)
     '("Z" . recenter-top-bottom)
     '("f" . meow-find)
     '("F" . mn/meow-negative-find)
     '("o" . meow-prev)
     '("O" . meow-prev-expand)
     '("u" . meow-undo)
     ;; '("U" . meow-undo-in-selection)
     '("U" . undo-redo)
     '("'" . repeat)
     '("\\". meow-visit)

     ;; ~~ Home row ~~
     '("c" . meow-change)
     '("r o" . windmove-up)
     '("r n" . windmove-left)
     '("r e" . windmove-down)
     '("r i" . windmove-right)
     '("R o" . windmove-swap-states-up)
     '("R n" . windmove-swap-states-left)
     '("R e" . windmove-swap-states-down)
     '("R i" . windmove-swap-states-right)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("T" . mn/meow-negative-till)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     
     ;; ---
     
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-left)
     '("N" . meow-left-expand)
     '("e" . meow-next)
     '("E" . meow-next-expand)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("a" . meow-append)
     '("A" . meow-open-below)

     ;; ~~ Bottom row ~~
     '("q" . meow-quit)
     '("j" . meow-replace)
     '("v" . meow-search)
     '("d" . meow-kill)
     '("k" . meow-block)
     '("K" . meow-to-block)
     
     ;; ---
     
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("h" . meow-reverse)
     '("/" . consult-line)
     '("?" . meow-comment)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)

     ;; ~~ Other stuff ~~
     '("-" . negative-argument)
     '("!" . meow-join)
     
     '("{" . backward-paragraph)
     '("}" . forward-paragraph)
     '("(" . meow-beginning-of-thing)
     '(")" . meow-end-of-thing)
     ;; '("[" . backward-page)
     ;; '("]" . forward-page)
     '("[" . beginning-of-buffer)
     '("]" . end-of-buffer)

     ;; doesn't really work with dtrt-indent (will always indent 4 spaces), but
     ;; honestly it's not necessary.
     ;; '("<" . indent-rigidly-left-to-tab-stop)
     ;; '(">" . indent-rigidly-right-to-tab-stop)
     
     '("<" . shrink-window-horizontally)
     '(">" . enlarge-window-horizontally)
     ;; '("<prior>" . shrink-window)         ; Page Up
     ;; '("<next>"  . enlarge-window)        ; Page Down
     '("="  . shrink-window)
     '("\"" . enlarge-window)

     '("'" . repeat)
     '("<escape>" . ignore)
     '("<return>" . ignore)
     '("<backspace>" . ignore)))
  
  :hook (emacs-startup . mn/meow-setup))
  ;; :hook (elpaca-after-init . mn/meow-setup))
  ;; :config (mn/meow-setup))
