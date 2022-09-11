#| Modes

Insert mode    :~: default *top-map*
Rearrange mode :~: 

Okay looks like stump already has the concept of modes.

The *root-map* can be accessed via prefix or by pushing it into *top-map* with `command-mode` command.
Mode is exited with C-g which I suppose is bound to `TODO` command in the *top-map* during `command-mode`.

This file should be a collection of commands that I bind

|#
                     ;; args 
(defcommand dump-top-map () () ;; interactive args
  (with-open-file (s (append-to-path *data-dir* "top-map.sexp") :direction :output)))


(defvar *input-mode* 'insert-mode)
(defvar *rearrange-map* )



#| QUESTLOG

Commands I want:
----------------
Open emacs with stumpwm config file $FILE
Open emacs with sly repl into stumpwm
Chaining commands sequentially
Query layout left/right/top/down
Switch modes (rebind *top-map*)
Intercept unmodified keys by rebinding in *top-map*
Split frame, close split
Move window between frames and create / close splits as necessary
Move windows in frame to other group
Summon windows from frame in other group as new split (so the stack abstraction)
Name windows / name frames / name groups
Serialize and load / unload for various state
API for datalisp (maybe via swank thread)










|#
