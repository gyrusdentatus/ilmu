#| INTERNALS

These things are basically bug reports to the stump core.

A real solution would rewrite the internals rather than hack around it like this.

|#


(defun split-frame-dir (group p dir ratio)
  "Return 2 new frames. The first one stealing P's number and window"
  (let* ((w (if (or (eq dir :left)
		    (eq dir :right))
		(ratio-or-pixel (frame-width p) ratio)
		(frame-width p))
         (h (if (or (eq dir :up)
		    (eq dir :down))
		(ratio-or-pixel (frame-height p) ratio)
		(frame-height p))
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
    (if (or (eq dir :up) (eq dir :left))
	(run-hook-with-args *split-frame-hook* p f p)
	(run-hook-with-args *split-frame-hook* p p f))
    (run-hook-with-args *new-frame-hook* f)
    (values p f)))

(defun split-frame (group how &optional (ratio 1/2))
  "Split the current frame into 2 frames. Return new frame number, if
it succeeded. NIL otherwise. RATIO is a fraction of the screen to
allocate to the new split window. If ratio is an integer then the
number of pixels will be used. This can be handy to setup the
desktop when starting."
  (check-type how (member :row :column :up :down :right :left))
  (let* ((frame (tile-group-current-frame group))
         (head (frame-head group frame))
	 ;; once we've created split we need to move window provenance
	 (migrate-to-children-frames
	   (lambda (f1 f2)
	     (migrate-frame-windows group frame f1)
	     (choose-new-frame-window f2 group) ;; moves a window to new frame
	     (when (eq frame (tile-group-current-frame group))
	       (setf (tile-group-current-frame group) f1))
	     (setf (tile-group-last-frame group) f2)
	     (sync-frame-windows group f1)
	     (sync-frame-windows group f2)
	     ;; we also need to show the window we moved to the new frame
	     (when (frame-window f2)
	       (unhide-window (frame-window f2)))
	     (frame-number f2))))
    ;; backwards compat
    (when (eq how :row) (setf how :right))
    (when (eq how :column)  (setf how :down))
    ;; don't create frames smaller than the minimum size
    (when (or (and (member how '(:up :down))
		   (>= (frame-height frame) (* *min-frame-height* 2)))
	      (and (member how '(:left :right))
		   (>= (frame-width frame) (* *min-frame-width* 2))))
      (multiple-value-bind (f1 f2) (split-frame-dir group frame how ratio)
	(setf (tile-group-frame-head group head)
		 (if (atom (tile-group-frame-head group head))
		     (list f1 f2)
		     (funcall-on-node (tile-group-frame-head group head)
				      (lambda (tree)                                                         
					(substitute (list f1 f2) frame tree))                                
				      (lambda (tree)
					(unless (atom tree)
					  (find frame tree))))))
	(if (member how '(:right :down))
	    (funcall migrate-to-children-frames f1 f2)
	    (funcall migrate-to-children-frames f2 f1))))))


(defun detect-monitor-edge (group direction)
  (let ((frame (tile-group-current-frame group)))
