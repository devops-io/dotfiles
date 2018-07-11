(defvar nginx-mode-hook nil)
 
(defvar nginx-mode-map
  (let ((nginx-mode-map (make-keymap)))
    (define-key nginx-mode-map "\C-j" 'newline-and-indent)
    nginx-mode-map))
 
(defconst nginx-font-lock-keywords-global
  (list
   (regexp-opt
    '("daemon"
      "env"
      "include"
      "master_process"
      "pid"
      "ssl_engine"
      "user"
      "timer_resolution"
      "worker_rlimit_core"
      "worker_rlimit_nofile"
      "worker_priority"
      "worker_processes"
      "working_directory")
    t) 1 'font-lock-function-name-face))
 
(defvar nginx-font-lock-keywords nginx-font-lock-keywords-global)
 
(defun nginx-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map nginx-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(nginx-font-lock-keywords))
  (setq major-mode 'nginx-mode)
  (setq mode-name "nginx")
  (run-hooks 'nginx-mode-hook))
 
(provide 'nginx-mode)