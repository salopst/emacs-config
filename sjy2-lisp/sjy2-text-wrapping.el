;; Core wrapping function
(defun sjy2-wrap-with (left &optional right)
  "Wrap region or symbol with LEFT and RIGHT."
  (let* ((right (or right left))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol))))
    (if bounds
        (save-excursion
          (goto-char (cdr bounds))
          (insert right)
          (goto-char (car bounds))
          (insert left))
      (user-error "No region or symbol"))))

;; Wrapper commands
(defun sjy2-wrap-parens () (interactive) (sjy2-wrap-with "(" ")"))
(defun sjy2-wrap-brackets () (interactive) (sjy2-wrap-with "[" "]"))
(defun sjy2-wrap-braces () (interactive) (sjy2-wrap-with "{" "}"))
(defun sjy2-wrap-quotes () (interactive) (sjy2-wrap-with "\""))
(defun sjy2-wrap-slashes () (interactive) (sjy2-wrap-with "/"))
(defun sjy2-wrap-asterisks () (interactive) (sjy2-wrap-with "*"))
(defun sjy2-wrap-tildes () (interactive) (sjy2-wrap-with "~"))

;; Keybindings
(sjy2/bind "(" #'sjy2-wrap-parens)
(sjy2/bind "[" #'sjy2-wrap-brackets)
(sjy2/bind "{" #'sjy2-wrap-braces)
(sjy2/bind "\"" #'sjy2-wrap-quotes)
(sjy2/bind "/" #'sjy2-wrap-slashes)
(sjy2/bind "*" #'sjy2-wrap-asterisks)
(sjy2/bind "~" #'sjy2-wrap-tildes)