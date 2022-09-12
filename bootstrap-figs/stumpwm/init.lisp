(in-package :stumpwm)

(require :ttf-fonts)
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))

#| REGARDING CACHE DIRECTORY

~/.cache is less permanent data, should be recoverable if it is destroyed.
~/.local/share is XDG_DATA_HOME by default, this data is not as transient.  |#

;; utils

(defun show-cache-pathname (filename)
  (uiop:xdg-data-home #p"stumpwm/" filename))

(defun show-config-pathname (filename)
  (uiop:xdg-config-home #p"stumpwm/" filename))

(defun load-config-file (file)
  (load (show-config-pathname file)))

(load-config-file "keyboard.lisp") ;; caps -> hyper, sort out modifiers.
(load-config-file "commands.lisp") ;; define various commands that can be bound.
(load-config-file "swank.lisp")    ;; make it possible to open repl to running stump instance.
(load-config-file "keymap.lisp")   ;; bind commands to keys in keymaps.

#| bind keys to *root-map* for C-t prefixed commands and *top-map* for unprefixed commands.
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "u") "exec kitty")
|#


;; deprecated
(defun append-to-path (path str)
  (pathname (concatenate 'string (namestring path) str)))
