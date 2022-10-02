#| Keymaps

The top map with caps lock (hyper) allows launching and traversing windows.
A mode switch gives access to rearranging and resizing frames / windows.
Menus are used to access niche or bulk behaviours as well as catalogue windows.
Groups are largely ignored (by user) in favour of menus, they are "window RAM".

There are various modes that can be switched to or accessed via prefix:
- rearrange  : commands for moving windows defined in commands.lisp also has resize cmds from stump src.
- meta       : datalisp interop, useful for naming, exporting, importing, launching and documenting.
- commands   : default *root-map* not useful since all useful commands will be accessible via other binds.

FIXME: NOTES REGARDING CURRENT MACHINE

H- doesn't work! FIXME!!
M- is Alt_L or Alt_R
S- is shift! remember that!
s- is windows key and caps via guix config.

TODO: List all actions I need

|#

;; ROOT MAP
(define-key *root-map* (kbd "u") "exec chromium")
(define-key *root-map* (kbd "z") "exec kitty")

;; TOP MAP - META = MODIFY
(define-key *top-map* (kbd "M-e") "eval-selection") ;; fixme
(define-key *top-map* (kbd "M-n") "gnew")
(define-key *top-map* (kbd "M-w") "pull-from-windowlist")
(define-key *top-map* (kbd "M-Up") "gnext")
(define-key *top-map* (kbd "M-Down") "gprev")
(define-key *top-map* (kbd "M-h") "move left")
(define-key *top-map* (kbd "M-l") "move right")
(define-key *top-map* (kbd "M-j") "move down")
(define-key *top-map* (kbd "M-k") "move up")

;; TOP MAP - SUPER = TRAVERSE
(define-key *top-map* (kbd "s-z") "exec kitty")
(define-key *top-map* (kbd "s-x") "exec")
(define-key *top-map* (kbd "s-c") "colon")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-Up") "gnext")
(define-key *top-map* (kbd "s-Down") "gprev")
(define-key *top-map* (kbd "s-e") "echo-frame-windows")
(define-key *top-map* (kbd "s-b") "prev-in-frame")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-S-h") "move left")
(define-key *top-map* (kbd "s-S-l") "move right")
(define-key *top-map* (kbd "s-S-j") "move down")
(define-key *top-map* (kbd "s-S-k") "move up")

#| REARRANGE MODE

Ideally I could use a,s,d,f,q,w,e,r as modifiers for hjkl while in the mode.

a: 
s: move with split even if not at screen edge
d: delete the split and move entire frame with focus (same as if only one window was in frame when moving)
f: float window and move it as if being dragged around by the mouse, single tap f would cycle focus of floating
q: query focus for context and open results in frame direction from focus. (or query for relation to frame dir)
w: window moves with focus (same as shift would do I suppose)
e: entire frame moves with focus, swaps frames rather than deleting splits like d does.
r: resize focused window

(define-interactive-keymap (rearrange tile-group) (:on-enter #'enter-rearrange-mode
						   :on-exit #'exit-rearrange-mode
						   :abort-if #'abort-rearrange-mode-p)
  ((kbd "k") "move window up somehow")
  ...)
|#

;; TODO: META MODE
;;       once tala works.

#| deprecated in favour of define-interactive-keymap macro

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

|#
