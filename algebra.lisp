; http://www.csc.villanova.edu/~dmatusze/resources/lisp/lisp-example.html

(defun simplify (expr)
    expr)

(defun poly+ (p1 p2)
    (cond
        (null (car p1)) p2
        t (cons (car p1) (poly+ (cdr p1) p2))))

    
(print (poly+ '(1 ("x" 2)) '(4 ("x" 2)))

