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
(define-key *top-map* (kbd "M-n") "gnew")
(define-key *top-map* (kbd "M-w") "pull-from-windowlist")
(define-key *top-map* (kbd "M-Up") "gnext")
(define-key *top-map* (kbd "M-Down") "gprev")
(define-key *top-map* (kbd "M-h") "move-window left")
(define-key *top-map* (kbd "M-l") "move-window right")
(define-key *top-map* (kbd "M-j") "move-window down")
(define-key *top-map* (kbd "M-k") "move-window up")

;; TOP MAP - SUPER = TRAVERSE
(define-key *top-map* (kbd "s-z") "exec kitty")
(define-key *top-map* (kbd "s-x") "exec")
(define-key *top-map* (kbd "s-c") "colon")
(define-key *top-map* (kbd "s-e") "echo-frame-windows")
(define-key *top-map* (kbd "s-b") "prev-in-frame")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")

#|
;; REARRANGE MODE
(define-interactive-keymap (rearrange tile-group) (:on-enter #'enter-rearrange-mode
						   :on-exit #'exit-rearrange-mode
						   :abort-if #'abort-rearrange-mode-p)
  ((kbd "j") "move window up somehow")
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
