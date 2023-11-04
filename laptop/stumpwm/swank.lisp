;; credit https://github.com/kitnil/dotfiles/blob/c247e7a3cab9b7159fad1f37fab6c0e36d0ba21b/dot_stumpwm.d/swank.lisp

(require :slynk)

(defcommand slynk (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (let ((slynk:*loopback-interface* "127.0.0.1"))
       (slynk:create-server :port (parse-integer port)
                            :dont-close t)))
   :name "slynk"))
