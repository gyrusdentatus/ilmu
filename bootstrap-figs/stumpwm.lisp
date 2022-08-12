(require :ttf-fonts)
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))
(setf *data-dir* (concat (getenv "HOME") "/.cache/stumpwm"))

(in-package :stumpwm)
(define-key *root-map* (kbd "u") "exec chromium")
(define-key *root-map* (kbd "z") "exec kitty")
(define-key *top-map* (kbd "M-z") "exec kitty")
(define-key *top-map* (kbd "M-x") "exec")
(define-key *top-map* (kbd "M-c") "colon")
(define-key *top-map* (kbd "M-h") "fprev")
(define-key *top-map* (kbd "M-l") "fnext")
(define-key *top-map* (kbd "M-j") "next-in-frame")
(define-key *top-map* (kbd "M-k") "prev-in-frame")

;; meta-shift-hjkl to move window across splits (and create them or close them as needed).
;; the moving across splits thing should be a mode, hold shift to move focus and otherwise you move window w/hjkl.
;; some keybinds to move stacks of windows to (and from) different groups, also useful; merging and splitting groups.

#| replify stump
(in-package :stumpwm)

(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
|#

#| bind keys to *root-map* for C-t prefixed commands and *top-map* for unprefixed commands.
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "u") "exec kitty")
|#
