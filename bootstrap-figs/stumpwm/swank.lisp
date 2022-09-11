;; credit https://github.com/kitnil/dotfiles/blob/c247e7a3cab9b7159fad1f37fab6c0e36d0ba21b/dot_stumpwm.d/swank.lisp

(require :swank)

(swank-loader:init)

(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (let ((swank::*loopback-interface* "127.0.0.1"))
       (swank:create-server :port (parse-integer port)
			 ;; :style swank:*communication-style*
                            :dont-close t)))
   :name "swank"))
