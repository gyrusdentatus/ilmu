#| QUESTLOG

Commands I want:
----------------
Name window so that s-e (query stack) becomes useful (currently shows bash,bash,bash..)
Open emacs with stumpwm config file $FILE
Open emacs with sly repl into stumpwm
Chaining commands sequentially             :~: (run-commands cmd1 cmd2 ...)
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


;; queries : check whether layout offers motion or if split is required

;; check-window
;; move-focus direction
;; check-window -> (if same (progn split (move-window direct))
;;                          (progn (fprev) ;; moves selection to previous
;;                                 (move-window direction)))


;; splits : make necessary split to perform motion given query
;; NOTE use exchange-windows in direction of split == moving window with split.
;; NOTE not needed since the internals don't have to send window when splitting

;; only :~: removes all splits.
;; remove-split
;; hsplit  (-equally -uniformly)  | These are rewritten to split in hjkl-like
;; vsplit                         | directions. see internals.lisp for details.

;; motions : package queries and splits into useful actions

#|
;; all of these require sb-thread. may fail with earlier bug
;; see https://config.phundrak.com/stumpwm.html
(defcommand term (&optional program) ()
  "Invoke a terminal, possibly with a @arg{program}."
  (sb-thread:make-thread
   (lambda ()
     (run-shell-command (if program
			    (format nil "kitty ~A" program)
			    "kitty")))))

(defcommand sly-start-server (port) ((:string "Port number: "))
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:create-server :port (parse-integer port)
							 :dont-close t))))

(defcommand sly-stop-server (port) ((:string "Port number: "))
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:stop-server (parse-integer port)))))
|#
(defcommand window-send-clipboard () ()
  (window-send-string (get-x-selection)))

;; FIXME: make interactive by echoing selection and doing y-or-n-p.
(defcommand eval-selection () () 
  (eval-line (get-x-selection)))

#| NAVIGATION

The idea is that you have stacks of windows inside frames.
You can move focus between frames and then flip through the windows of the frame.
You can also take the window on top with you as you move the focus, if you do then
new frames will be made as necessary by splitting in the desired direction.

Finally, if you move a window from a frame so that the frame becomes empty then
the split will be closed and the frame collapsed.

The idea here is that you can easily rearrange the desktop if you have few
windows in total or a main stack that you split off-of to work.

Groups are only used to send and receive stacks of windows (frames) that are
not required in the current context (group).

The group > frame > window organization can be subverted in future by menus
and tags. Then you can use a hook when switching groups to move windows into
frames on newly active group which are "shared memory" and should also be in
that group. Ideally very few commands are needed for navigation and these
concepts can be avoided by user who just opens, closes and rearranges windows.
|#

(defcommand (dirsplit tile-group) (dir) ((:direction "Direction: "))
"Split the current frame into 2 frames in the desired direction."
  (split-frame-in-dir (current-group) dir 1/2))

#| Case analysis

Last window -> If move in valid direction: close split, otherwise: noop.
Not last window -> If move in invalid direction: open split and move-window, otherwise: move-window.

|#

(defcommand (move tile-group) (dir) ((:direction "Direction: "))
  "Split the frame if necessary to move in direction. Closes split if it leaves empty frame."
  (if (detect-last-window (current-group))
      (unless (detect-monitor-edge (current-group) dir)
	(let ((last-frame (tile-group-current-frame (current-group))))
	  (move-window dir)
	  (remove-split (current-group) last-frame)))
      (progn
	(when (detect-monitor-edge (current-group) dir)
	  (dirsplit dir))
	(move-window dir))))

;; such a soup of accessors, find some simple example to work from... really need repl!
;;( (frame-windows (current-group) (tile-group-current-frame (current-group)))

;; scraps

                     ;; args 
(defcommand dump-top-map () () ;; interactive args
  (with-open-file (s (append-to-path *data-dir* "top-map.sexp") :direction :output)))


