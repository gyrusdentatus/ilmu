#| INTERNALS

These things are basically bug reports to the stump core.

A real solution would rewrite the internals rather than hack around it like this.

|#



;; splitting functions

(defun split-frame-dir (group dir ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((p  (tile-group-current-frame group))
	 (w (if (or (eq dir :left)
		    (eq dir :right))
		(ratio-or-pixel (frame-width p) ratio)
		(frame-width p)))
         (h (if (or (eq dir :up)
		    (eq dir :down))
		(ratio-or-pixel (frame-height p) ratio)
		(frame-height p)))
	 (x (if (eq dir :right)
		(+ (frame-x p) w)
		(frame-x p)))
	 (y (if (eq dir :down)
		(+ (frame-y p) h)
		(frame-y p)))
	 (wh (if (or (eq dir :left)
		     (eq dir :right))
		 (cons (- (frame-width p) w) h)
		 (cons w (- (frame-height p) h))))
	 (f (make-frame :number (find-free-frame-number group)
			:x x
			:y y
			:width (car wh)
			:height (cdr wh)
			:window nil)))
    ;; adjust the parameters of parent frame
    (setf (frame-width p) w
	  (frame-height p) h)
    (when (eq dir :up)
      (setf (frame-y p) (+ (frame-y p) h)))
    (when (eq dir :left)
      (setf (frame-x p) (+ (frame-x p) w)))
    ;; bureaucracy
    (run-hook-with-args *new-frame-hook* f)
    (if (or (eq dir :up) (eq dir :left))
	(run-hook-with-args *split-frame-hook* p f p)
	(run-hook-with-args *split-frame-hook* p p f))
    (values p f)))

(defun split-frame (group dir &optional (ratio 1/2))
  "Split the current frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type dir (member :row :column :up :down :right :left))
  (let* ((frame (tile-group-current-frame group))
         (head (frame-head group frame)))
    ;; backwards compat
    (when (eq dir :row)     (setf dir :right))
    (when (eq dir :column)  (setf dir :down))
    ;; don't create frames smaller than the minimum size
    (when (or (and (member dir '(:up :down))
		   (>= (frame-height frame) (* *min-frame-height* 2)))
	      (and (member dir '(:left :right))
		   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (split-frame-dir group dir ratio)
	;; swap f1 and f2 when we insert new window above or before old window
	(when (or (eq dir :left) (eq dir :up)) (rotatef f1 f2))
	(setf (tile-group-frame-head group head)
		 (if (atom (tile-group-frame-head group head))
		     (list f1 f2)
		     (funcall-on-node (tile-group-frame-head group head)
				      (lambda (tree)                                                         
					(substitute (list f1 f2) frame tree))                                
				      (lambda (tree)
					(unless (atom tree)
					  (find frame tree))))))
	;; undo swap, the windows stay in the resized parent window
	(when (or (eq dir :left) (eq dir :up)) (rotatef f1 f2))
	(migrate-frame-windows group frame f1)
	;; NOTE: this default is noisy for my intended purposes
	;; (choose-new-frame-window f2 group) ;; moves a window to new frame
	(when (eq frame (tile-group-current-frame group))
	  (setf (tile-group-current-frame group) f1))
	(setf (tile-group-last-frame group) f2)
	(sync-frame-windows group f1)
	(sync-frame-windows group f2)
	;; we also need to show the window we moved to the new frame (if we did)
	(when (frame-window f2)
	  (unhide-window (frame-window f2)))
	(frame-number f2)))))

    
(defun split-frame-in-dir (group dir &optional (ratio 1/2))
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir ratio)
        (progn
          (when (frame-window f)
            (update-decoration (frame-window f)))
          (show-frame-indicator group))
        (message "Cannot split smaller than minimum size."))))  


;; query functions

(defun detect-monitor-edge (group direction)
  "Checks if there is a frame in the given direction"
  (let ((frame (tile-group-current-frame group)))
    (move-focus direction)
    (if (eq (frame-number frame)
	    (frame-number (tile-group-current-frame group)))
	t
	(progn
	  (fselect frame)
	  nil))))

(defun detect-last-window (group &optional (frame nil))
  "Checks if the current frame has only one window"
  (unless frame (setf frame (tile-group-current-frame (current-group))))
  (= 1 (length (frame-windows (current-group) frame))))

(defun detect-empty-frame (group &optional (frame nil))
  "Checks if the current frame has only one window"
  (unless frame (setf frame (tile-group-current-frame (current-group))))
  (= 0 (length (frame-windows (current-group) frame))))

(defcommand test-detection () ()
  (when (detect-monitor-edge (current-group) :up)
    (echo "something")))
