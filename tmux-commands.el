(require 'general)
(require 'hydra)

(general-create-definer my-s-def
  :prefix "s")
(my-s-def
  :states '(normal motion visual)
  :keymaps 'override
  ","    'tmux-ls
  "."    'hydra-tmux-combo/body
  "'"    'tmux-display-pane-numbers
  ";"    'hydra-tmux-command-history/body
  "-"    'tmux-last-dir
  "_"    'tmux-split-window-vertical
  "b"    'tmux-capture-pane
  "c"    'tmux-ctrl-c
  "d"    'tmux-ctrl-d
  "e"    'tmux-clear-pane
  "f"    'tmux-minibuffer-run-shell-cmd
  "g"    'tmux-cd-default-directory
  "h"    'tmux-home-dir
  "i"    'tmux-insert-state
  "j"    'tmux-ivy-run-shell
  "m"    'hydra-tmux-copy-mode/body
  "n"    'tmux-n
  "o"    'tmux-swap-pane
  "p"    'tmux-pwd
  "q"    'tmux-q
  "r"    'tmux-rename-window
  "s"    'tmux-send-region
  "t"    'hydra-tmux-window-config/body
  "w"    'tmux-last-window
  "y"    'tmux-y
  "z"    'tmux-ctrl-z
  "D"    'tmux-kill-pane
  "K"    'tmux-kill-window
  "N"    'tmux-new-window
  "SPC"  'tmux-space
  "RET"  'tmux-ctrl-m
  "0"    'tmux-select-window-0
  "1"    'tmux-select-window-1
  "2"    'tmux-select-window-2
  "3"    'tmux-select-window-3
  "4"    'tmux-select-window-4
  "5"    'tmux-select-window-5
  "6"    'tmux-select-window-6
  "7"    'tmux-select-window-7
  "8"    'tmux-select-window-8
  "9"    'tmux-select-window-9
  )

(defhydra hydra-tmux-combo ()
  ("c"   tmux-ctrl-c)
  ("d"   tmux-ctrl-d)
  ("q"   tmux-q)
  ("j"   tmux-command-history-next)
  ("k"   tmux-command-history-prev)
  "b"   tmux-capture-pane
  "e"   tmux-clear-pane
  "g"   tmux-cd-default-directory
  "h"   tmux-home-dir
  "o"   tmux-swap-pane
  "n"   tmux-n
  "p"   tmux-pwd
  "u"   tmux-up-dir
  "w"   tmux-last-window
  "y"   tmux-y
  "z"   tmux-ctrl-z
  " "   tmux-
  " "   tmux-
  " "   tmux-
  " "   tmux-
  " "   tmux-
  "D"   tmux-kill-pane
  "N"   tmux-new-window
  "RET" tmux-ctrl-m
  "SPC" tmux-space
  "-"   tmux-last-dir
  "."   tmux-next-window
  ","   tmux-ls
  "_"   tmux-split-window-vertical
  "0"   tmux-select-window-0
  "1"   tmux-select-window-1
  "2"   tmux-select-window-2
  "3"   tmux-select-window-3
  "4"   tmux-select-window-4
  "5"   tmux-select-window-5
  "6"   tmux-select-window-6
  "7"   tmux-select-window-7
  "8"   tmux-select-window-8
  "9"   tmux-select-window-9
  )

(defhydra hydra-tmux-command-history (:body-pre (tmux-begin-cmd-history) :hint nil :foreign-keys warn)
  "
  ;; _DEL_: page up _SPC_: page down _j_: down _k_: up _RET_: select
"
  ("DEL" tmux-page-up)
  ("SPC" tmux-page-down)
  ("RET" tmux-run-a-history-cmd :exit t)
  ("j" tmux-copy-mode-down)
  ("k" tmux-copy-mode-up)
  ("<escape>" tmux-quit-copy-mode :exit t)
  )

(defhydra hydra-tmux-copy-mode (:body-pre (tmux-begin-copy-mode) :hint nil :foreign-keys warn)
  "
  ;; _DEL_: page up _SPC_: page down _j_: down _k_: up _<escape>_: exit
"
  ("DEL" tmux-halfpage-up)
  ("SPC" tmux-halfpage-down)
  ("j" tmux-copy-mode-down)
  ("k" tmux-copy-mode-up)
  ("<escape>" tmux-quit-copy-mode :exit t)
  )

(defhydra hydra-tmux-window-config (:body-pre (setq tmux-selected-pane "0"))
  ("h" tmux-resize-pane-left)
  ("l" tmux-resize-pane-right)
  ("j" tmux-resize-pane-down)
  ("k" tmux-resize-pane-up)
  ("s" tmux-select-pane)
  ("-"  tmux-split-window-vertical)
  ("|" tmux-split-window-horizontal)
  ("<escape>" tmux-select-pane-0 :exit t)
  )
