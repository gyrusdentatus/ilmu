#| Keymap

*top-map*
 ^    |     use prefix or mode switch
 |    v     to move *top-map* pointer down
*root-map*
   ...
*other-maps*

The idea is to maintain DSLs for interacting.

|#

(define-key *root-map* (kbd "u") "exec chromium")
(define-key *root-map* (kbd "z") "exec kitty")
(define-key *top-map* (kbd "M-z") "exec kitty")
(define-key *top-map* (kbd "M-x") "exec")
(define-key *top-map* (kbd "M-c") "colon")
(define-key *top-map* (kbd "M-h") "fprev")
(define-key *top-map* (kbd "M-l") "fnext")
(define-key *top-map* (kbd "M-j") "next-in-frame")
(define-key *top-map* (kbd "M-k") "prev-in-frame")
