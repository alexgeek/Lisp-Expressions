;;;;;;;;;;;;;;;;;;;
;; abp24         ;;
;; CM20214 CW 1  ;;
;; Uses clisp    ;;
;;;;;;;;;;;;;;;;;;;

;; representation: '((coefficient (variable exponent) (variable exponent) ...) (coefficient (variable exponent) ... ))

;; return term that is the sum of two like terms (does not check they are compatible)
(defun t+t (t1 t2)
    (list (+ (car t1) (car t2)) (cadr t1)))
    
;; terms have like variables e.g. '(5 (x 1)) and '(2 (x 1))
(defun t-like (t1 t2)
    (and
        (eq (caadr t1) (caadr t2))
        (if (and (not (null (cadr t1))) (not (null (cadr t2))))
            (= (cadr (cadr t1)) (cadr (cadr t2))))
    )
)

;; add term to a poly e.g. (p+t '((5 (x 1))) '(2 (x 1)))
(defun p+t (p1 t1)
    (cond
        ;; base case if reach end of poly, just appends the term on the end of the poly
        ((null p1) (list t1))
        ;; if variables are alike, return a list with the terms summed and the rest of list
        ((t-like (car p1) t1)
            (cons (t+t (car p1) t1) (cdr p1))
        )
        ;; recurse on p1
        (t (cons (car p1) (p+t (cdr p1) t1)))
))

(defun poly+ (p1 p2)
    (cond
        ((null p2) p1)
        (t (poly+ (p+t p1 (car p2)) (cdr p2)))
    )
)

(defun t*t (t1 t2)
    (cond 
        ;; if either t1 or t2 are 0, return a zero
        ((or (= (car t1) 0) (= (car t2) 0)) '(0 (x 0)))
        ;; if t1 is just a number multiply by t2
        ((= (cadr (cadr t1)) 0) 
            (cons (* (car t1) (car t2)) (cdr t2)))
        ;; other way round of above
        ((= (cadr (cadr t2)) 0)
            (cons (* (car t1) (car t2)) (cdr t1)))
        ((equal (cdr t1) (cdr t2))
            (cons (* (car t1) (car t2)) (cdr t1)))
        (t
            (cons (* (car t1) (car t2)) (append (cdr t1) (cdr t2)))
        )
    )
)

;; multiply poly by term
(defun p*t (p1 t1)
    (cond
        ((null p1) '())
        (t (cons (t*t (car p1) t1) (p*t (cdr p1) t1)))
    )
)

;; multiply poly by poly
(defun poly* (p1 p2)
    (cond
        ((null p2) p1)
        (t (poly* (p*t p1 (car p2)) (cdr p2)))
    )
)

;; subtract p2 from p1 builds on previous work
(defun poly- (p1 p2)
    (poly+ p1 (p*t p2 '(-1 (x 0)))))

;; Tests (from http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html)

;; utility functions for quick tests
(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))
(defmacro check (&body forms)
    (write-line "Begin Test")
    `(progn
        ,@(loop for f in forms collect `(report-result ,f ',f))))

;; summing terms  
(check 
    (equal (t+t '(1 (x 1)) '(1 (x 1))) '(2 (x 1)))
    (equal (t+t '(2 (x 1)) '(-1 (x 1))) '(1 (x 1)))
)

;; summing poly and a term
(check
    (equal (p+t '((5 (x 2))) '(2 (x 2))) '((7 (x 2))))
    (equal (p+t '((5 (x 2)) (7 (y 2))) '(5 (x 2))) '((10 (x 2)) (7 (y 2))))
    (equal (p+t '((8974353 (x 1))) '(-909232 (x 1))) '((8065121 (x 1))))
)

;; summing poly and poly
(check
    (equal (poly+ '((20 (x 2))) '((1 (x 2)))) '((21 (x 2))))
    (equal (poly+ '((4 (x 2)) (7 (y 5))) '((-3 (y 5)))) '((4 (x 2)) (4 (y 5))))
)

;; multiplying terms
(check
    (equal (t*t '(5 (x 2)) '(2 (x 2))) '(10 (x 2)))
    (equal (t*t '(2 (x 1)) '(4 (y 1))) '(8 (x 1) (y 1)))
    (equal (t*t '(3 (x 1) (y 1)) '(4 (x 1) (y 1))) '(12 (x 1) (y 1)))
    (equal (t*t '(2 (x 0)) '(5 (x 2))) '(10 (x 2)))
    (equal (t*t '(3 (x 3)) '(-4 (x 0))) '(-12 (x 3)))
)

;; multiply poly by term
(check
    (equal (p*t '((2 (x 1))) '(3 (x 1))) '((6 (x 1))))
    (equal (p*t '((3 (x 2)) (7 (y 5))) '(4 (z 1))) '((12 (x 2) (z 1)) (28 (y 5) (z 1))))
)

;; multiplying poly by poly
(check
    (equal (poly* '((5 (x 2))) '((10 (x 2)))) '((50 (x 2))))
    (equal (poly* '((2 (x 1)) (10 (y 1))) '((3 (z 1)))) '((6 (x 1) (z 1)) (30 (y 1) (z 1))))
)

;; subtraction
(check
    (equal (poly- '((5 (x 2))) '((3 (x 2)))) '((2 (x 2))))
    (equal (poly- '((10 (z 5))) '((3 (y 2)) (20 (z 5)))) '((-10 (z 5)) (-3 (y 2))))
)
