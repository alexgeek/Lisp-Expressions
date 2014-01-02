;; Helper functions
(defun term (expr)
    (car expr))

(defun coefficient (expr)
    (caar expr))

(defun var (expr)
    (caar (cdar expr)))
    
(defun index (expr)
    (car (cdar (cdar expr))))
    
(defun p+term (expr t1)
    (cond
        ;; if we reach end of expression just return term to be appended to expression
        ((null (term expr)) t1)
        ;; if equal variable and power, add coefficients
        ( (eq (var expr) (car (cadr t1))) (list (list (+ (coefficient expr) (car t1)) (cadr t1))))
        ;; recursive case on next term in expr
        (t (p+term (cdr expr) t1))))


(print "Expression: ")
(princ '((2 (x 1))) )
(print "Term: ")
(princ (term '((2 (x 1)))))
(print "Coefficient: ")
(princ (coefficient '((2 (x 1)))))
(print "Variable: ")
(princ (var '((2 (x 1)))))
(print "Index: ")
(princ (index '((2 (x 1)))))
(print "P+t: ")
(princ (p+term '((10 (x 1))) '(-5 (x 1)) ))
