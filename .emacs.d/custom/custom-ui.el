;; custom user interface elements
;; **list of functions
;; (defadvice show-paren-function
;; (defun djcb-opacity-modify (&optional dec)
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;


;; (defadvice show-paren-function
;;       (after show-matching-paren-offscreen activate)
;;       "If the matching paren is offscreen, show the matching line in the
;;         echo area. Has no effect if the character before point is not of
;;         the syntax class ')'."
;;       (interactive)
;;       (if (not (minibuffer-prompt))
;;           (let ((matching-text nil))
;;             ;; Only call `blink-matching-open' if the character before point
;;             ;; is a close parentheses type character. Otherwise, there's not
;;             ;; really any point, and `blink-matching-open' would just echo
;;             ;; "Mismatched parentheses", which gets really annoying.
;;             (if (char-equal (char-syntax (char-before (point))) ?\))
;;                 (setq matching-text (blink-matching-open)))
;;             (if (not (null matching-text))
;;                 (message matching-text)))))
;; (show-paren-mode)

;;; transparency
(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal


(global-set-key (kbd "M-Z") 'zap-up-to-char)
(global-set-key (kbd "C-x t") 'insert-date)
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;;(djcb-opacity-modify t)
(provide 'custom-ui)
