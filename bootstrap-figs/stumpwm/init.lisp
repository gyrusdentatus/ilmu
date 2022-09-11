(in-package :stumpwm)

(require :ttf-fonts)
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))

(defun append-to-path (path str)
  (pathname (concatenate 'string (namestring path) str)))

(setf *data-dir* (append-to-path (uiop:xdg-cache-home) "stumpwm/"))
(setf *cfg-dir* (append-to-path (uiop:xdg-config-home) "stumpwm/"))

(defun load-config-file (file)
  (load (append-to-path *cfg-dir* file)))

(load-config-file "keyboard.lisp") ;; caps -> hyper, sort out modifiers.
(load-config-file "lib.lisp")      ;; define various commands that can be bound.
(load-config-file "swank.lisp")    ;; make it possible to open repl to running stump instance.
(load-config-file "keymap.lisp")     ;; bind commands to keys in keymaps.

#| bind keys to *root-map* for C-t prefixed commands and *top-map* for unprefixed commands.
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "u") "exec kitty")
|#
