;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Timer-based Elisp code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-last-command-time nil
  "...")

(defun my-maybe-show-todo-file ()
  (if 
  (delete-other-windows)
  (find-file "~/todo"))

(run-at-time t 60 (function show-todo-file))
