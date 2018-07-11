(add-to-list 'auto-mode-alist '("\\.sls\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . conf-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . conf-mode))

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


(setq create-lockfiles nil)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(show-paren-mode 1)

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")


;;/usr/local/acl90express
;; (setq fi:common-lisp-host "localhost")
;; (setq fi:common-lisp-image-name "/usr/local/acl90express/alisp")
;; (setq fi:common-lisp-image-file "/usr/local/acl90express/alisp.dxl")
;; (setq fi:common-lisp-directory "/usr/local/acl90express")
;; (load "/usr/local/acl90express/eli/fi-site-init")

(global-set-key (kbd "C-c SPC") 'ace-jump-char-mode)
;;When you have an active region that spans multiple lines, the following will
;;add a cursor to each line:

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;;When you want to add multiple cursors not based on continuous lines, but based on
;;keywords in the buffer, use:

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)




;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 20)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

 ;; visuals
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

(global-subword-mode)


;;map function keys
(define-key global-map [f1] 'copy-to-register)
(define-key global-map [f2] 'insert-register)
(define-key global-map [f3] 'point-to-register)
(define-key global-map [f4] 'jump-to-register)
(define-key global-map [f5] 'start-kbd-macro)
(define-key global-map [f6] 'end-kbd-macro)

(setq make-backup-files nil) ;; do not make backup files
(setq auto-save-default nil) ; stop creating #autosave# files
;; backup in one place. flat, no tree structure
;; (setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))


;; (defun zap-up-to-char (arg char)
;;   "Kill up to, but not including ARGth occurrence of CHAR.
;; Case is ignored if `case-fold-search' is non-nil in the current buffer.
;; Goes backward if ARG is negative; error if CHAR not found.
;; Ignores CHAR at point."
;;   (interactive "p\ncZap up to char: ")
;;   (let ((direction (if (>= arg 0) 1 -1)))
;;     (kill-region (point)
;; 		 (progn
;; 		   (forward-char direction)
;; 		   (unwind-protect
;; 		       (search-forward (char-to-string char) nil nil arg)
;; 		     (backward-char direction))
;; 		   (point))))) 




;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;;                   (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; (require 'ido)
;; (require 'recentf)
;; (recentf-mode 1)

(set-background-color "black")
(set-foreground-color "gray")
(set-cursor-color "red")
(setq inhibit-splash-screen t)
;;(set-default-font "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1")
(tool-bar-mode -1)
;;;setting mark region
(setq-default transient-mark-mode nil)

(defun insert-date()
  (interactive)
  (insert (format-time-string "%T"))
)

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)


(add-to-list 'load-path "~/.emacs.d/custom/")
(require 'custom-compile)
(require 'custom-orgmode)
(require 'custom-ui)
(require 'hide-lines)

;; (add-to-list 'load-path "~/.emacs.d")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(org-adapt-indentation nil)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(custom-changed ((t (:background "white" :foreground "white"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :foreground "red")) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "red")))))

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet) 
(yas/initialize)
;; (setq yas-snippet-dirs '("~/.emacs.d/yasnippet/snippets/" "~/.emacs.d/yasnippet/snippets/chef-snips"))
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet/snippets"                 ;; personal snippets
        ;; ""           ;; foo-mode and bar-mode snippet collection
        ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))



(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C j)] 'aj-toggle-fold)


; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)


(global-set-key (kbd "C-c k") 'mode-compile-kill)

(global-set-key (kbd "C-c y") 'clipboard-yank)

(fset 'copy-to-clipboard
   [?\M-x ?c ?l ?i ?p ?b ?o ?a ?r ?d ?- ?k ?i ?l ?l ?- ?r ?e ?g ?i ?o ?n ?\C-m ?\C-y])

(global-set-key (kbd "M-c") 'copy-to-clipboard)

;; (require 'yaml-mode)
(add-to-list 'load-path "~/.emacs.d/yaml-mode/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(icomplete-mode 99)

;;(global-set-key (kbd "C-c k") 'mode-compile-kill)
(global-set-key (kbd "C-c i") 'string-insert-rectangle)
(put 'downcase-region 'disabled nil)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
 
 (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
   (flet ((process-list ())) ad-do-it))

;; (require 'buffer-move)
(put 'upcase-region 'disabled nil)

;;figure out why we cant load this later..
;; (add-to-list 'load-path "~/.emacs.d/helm-descbinds/")
;; (require 'helm-descbinds)
;; (helm-descbinds-mode)


(add-to-list 'load-path "~/.emacs.d/multiple-cursors/")
(require 'multiple-cursors)
