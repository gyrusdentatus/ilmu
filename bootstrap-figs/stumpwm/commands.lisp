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

(defcommand go-left () ()
  )
(defcommand go-right () ()
  )
(defcommand go-up () ()
  )
(defcommand go-down () ()
  )



;; scraps

                     ;; args 
(defcommand dump-top-map () () ;; interactive args
  (with-open-file (s (append-to-path *data-dir* "top-map.sexp") :direction :output)))
