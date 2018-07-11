(defun rspec-compile-by-line ()
  "a compilation file"
  (interactive)
  (save-some-buffers 1)
;;  (compile (format "cd %s;source config/.envrc; bundle exec rspec %s:%d"
  (compile (format "cd %s;bundle exec rspec %s:%d"
                   (get-closest-gemfile-root)
                   (buffer-file-name)
                   (line-number-at-pos)
                   ) t))
(global-set-key (kbd "C-c C-e") 'rspec-compile-by-line)


(defun rspec-compile-file ()
  (interactive)
  (save-some-buffers 1)
  (compile (format "rspec %s"
                   (file-relative-name (buffer-file-name))
                   ) t))
(global-set-key (kbd "C-c C-s") 'rspec-compile-file)

;; (defun javascript-compile-file ()
;;   (interactive)
;;   (save-some-buffers 1)
;;   (compile (format "firefox %s"
;;                    (file-relative-name (buffer-file-name))
;;                    ) t))
;; (global-set-key (kbd "C-c C-f") 'javascript-compile-file)

(defun ant-compile-file ()
  (interactive)
  (save-some-buffers 1)
  (compile (format "ant debug"
                   (file-relative-name (buffer-file-name))
                   ) t))
(global-set-key (kbd "C-c C-a") 'rspec-compile-file)

(defun ruby-compile-file ()
  (interactive)
  (save-some-buffers 1)
  (compile (format "ruby %s"
                   (file-relative-name (buffer-file-name))
                   ) t))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'ruby-compile-file)))

(defun cpp-compile-file ()
  (interactive)
  (save-some-buffers 1) ;;stop asking to save things all the time
  (compile (format "/usr/bin/g++ %s && ./a.out"
		   (file-relative-name (buffer-file-name))
		   ) t))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'cpp-compile-file)
))

(defun get-closest-gemfile-root (&optional (file "Gemfile"))
  (let ((root (expand-file-name "/")))
    (loop
     for d = default-directory then (expand-file-name ".." d)
     if (file-exists-p (expand-file-name file d))
     return d
     if (equal d root)
     return nil)))

(provide 'custom-compile)
