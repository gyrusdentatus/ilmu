#| QUESTLOG

Commands I want:
----------------
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
;; split -> exchange-windows in direction of split == moving window with split.

;; only :: removes all splits.
;; remove-split
;; hsplit  (-equally -uniformly)
;; vsplit

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

;; Menu can use stump cli command runner with eval-line
(defcommand eval-selection () ()
  (eval-line (get-x-selection)))


(defcommand go-left () ()
  (hsplit)
  (move-window :left))
(defcommand go-right () ()
  (hsplit)
  (move-window :right))
(defcommand go-up () ()
  (vsplit)
  (move-window :up))
(defcommand go-down () ()
  (vsplit)
  (move-window :down))

;; such a soup of accessors, find some simple example to work from... really need repl!
;;( (frame-windows (current-group) (tile-group-current-frame (current-group)))

;; scraps

                     ;; args 
(defcommand dump-top-map () () ;; interactive args
  (with-open-file (s (append-to-path *data-dir* "top-map.sexp") :direction :output)))
