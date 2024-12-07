(define (f1orf2 cond v f1 f2) (if cond (f1 v) (f2 v)))
(f1orf2 #t 1 (lambda (x) (+ x 1)) (lambda (x) (+ x 2)))
(f1orf2 #f 1 (lambda (x) (+ x 1)) (lambda (x) (+ x 2)))
