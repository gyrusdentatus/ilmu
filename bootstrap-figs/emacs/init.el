;; yay
(require 'evil) ;; emacs-evil emacs-evil-collection
(evil-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(setq backup-directory-alist `(("." . "~/.cache/emacs")))

(defun open-init-file ()
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(evil-set-leader '(normal visual) (kbd ","))

(evil-define-key 'normal 'global (kbd "<leader>c") 'open-init-file)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
(evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "<leader>b") 'list-buffers)
(evil-define-key 'normal 'global (kbd "<leader>d") 'evil-delete-buffer)
(evil-define-key 'normal 'global (kbd "<leader>g") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>f") 'make-frame)
(evil-define-key 'normal 'global (kbd "<leader>o") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>ws") 'evil-window-split)
(evil-define-key 'normal 'global (kbd "<leader>wv") 'evil-window-vsplit)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'evil-window-delete)
(evil-define-key 'visual 'global (kbd "<leader>e") 'eval-region)

;; emacs-geiser
;; Make into hook          -- memnomic is "tools scheme"
(evil-define-key 'normal 'global (kbd "<leader>ts") 'run-geiser)
(evil-define-key 'visual scheme-mode-map (kbd "<leader>e") 'geiser-eval-region)
(evil-define-key 'normal scheme-mode-map (kbd "<leader>e") 'geiser-eval-last-sexp)
(evil-define-key 'normal scheme-mode-map (kbd "<leader>h") 'geiser-doc-symbol-at-point)

;; emacs-sly
;; "tools lisp"
(evil-define-key 'normal 'global (kbd "<leader>tl") 'sly)
(evil-define-key 'visual sly-mode-map (kbd "<leader>e") 'sly-eval-region)
(evil-define-key 'normal sly-mode-map (kbd "<leader>e") 'sly-eval-last-expression)
(evil-define-key 'normal sly-mode-map (kbd "<leader>h") 'sly-documentation)
;; Fix annoying thing in sly repl.
(evil-define-key 'insert sly-mode-map (kbd ",") 'self-insert-command)

;; emacs-youtube-dl


;; emacs-evil-*something about parenthesis*

