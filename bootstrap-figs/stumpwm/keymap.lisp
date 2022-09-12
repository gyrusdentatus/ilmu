;; MASSIVE TODO: FIXME.


#| Keymap

*top-map*
 ^    |     use prefix or mode switch
 |    v     to move *top-map* pointer down
*root-map*
   ...
*other-maps*

The idea is to maintain DSLs for interacting.

STRATEGY
========

The top map has access to launching and rearranging via caps-lock (TODO keyboard.lisp).
There are various modes that can be switched to or accessed via prefix:
- rearrange  : commands defined in lib.lisp (TODO rename commands.lisp)
- resize     : example from stump source code with different bindings
- meta       : datalisp interop, useful for naming, exporting, importing.
- commands (default, not useful)


|#

(define-key *root-map* (kbd "u") "exec chromium")
(define-key *root-map* (kbd "z") "exec kitty")
(define-key *top-map* (kbd "M-z") "exec kitty")
(define-key *top-map* (kbd "M-x") "exec")
(define-key *top-map* (kbd "M-c") "colon")
(define-key *top-map* (kbd "M-h") "fprev")
(define-key *top-map* (kbd "M-l") "fnext")
(define-key *top-map* (kbd "M-j") "next-in-frame")
(define-key *top-map* (kbd "M-k") "prev-in-frame")

#| Modes

Insert mode    :~: default *top-map*
Command mode   :~: default *root-map*
Rearrange mode :~: mode for moving windows between (frames/stacks) and (creating/closing) (splits/frames/stacks).

Okay looks like stump already has the concept of modes.

The *root-map* can be accessed via prefix or by pushing it into *top-map* with `command-mode` command.
Mode is exited with C-g which I suppose is bound to `TODO` command in the *top-map* during `command-mode`.

This file should be a collection of commands that I bind

|#

(define-interactive-keymap (rearrange tile-group) (:on-enter #'enter-rearrange-mode
						   :on-exit #'exit-rearrange-mode
						   :abort-if #'abort-rearrange-mode-p)
  ((kbd "j") "move window up somehow")
  ...)

;; TODO: resize tile-group
;;       use different binds than in stump source.



;; structure of command-mode switch in stump

(defun rearrange-mode-start-message ()
  (message "Press q to exit rearrange-mode."))
(defun rearrange-mode-end-message ()
  (message "Exited rearrange mode"))

(defvar *rearrange-mode-start-hook* '(rearrange-mode-start-message)
  "A hook callled whenever rearrange mode is started")
(defvar *rearrange-mode-end-hook* '(rearrange-mode-end-message)
  "A hook called whenever rearrange mode is ended")

(defcommand rearrange-mode () ()
  "Rearrange mode allows you to invoke StumpWM commands, without prefix, to rearrange windows
   by opening or closing splits as necessary for the frames to contain the moving window in
   relation to all other windows. To exit command mode, type @key{q}."
  (run-hook *rearrange-mode-start-hook*)
  (push-top-map *rearrange-map*))


