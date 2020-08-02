(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (each) (append (list first) each)) rests))

(define (unzip list1 list2)
  (cond
  ((eq? list1 nil) nil)
  (else (cons (list (car list1) (car list2)) (unzip (cdr list1) (cdr list2))))))

(define (index-list length)
  (cond
  ((= length 0) nil)
  ((= length 1) (list 0))
  (else (append (index-list (- length 1)) (list (- length 1))))))

(define (zip list)
  (if (eq? (car list) nil) nil
    (cons (map (lambda (each) (car each)) list)
      (zip (map (lambda (each) (cdr each)) list)))))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
    (unzip (index-list (length s)) s))
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
    (cond
    ((eq? denoms nil) nil)
    ((= total 0) nil)
    ((< total (car denoms)) (list-change total (cdr denoms)))
    ((= total (car denoms)) (cons (list (car denoms)) (list-change total (cdr denoms))))
    (else (append
      (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
      (list-change total (cdr denoms))))))
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr)
        ((quoted? expr) expr)

        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
            (cons form (cons params (let-to-lambda body)))))

        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons (cons (quote lambda)
           (cons (car (zip (let-to-lambda values))) (let-to-lambda body)))
           (cadr (zip (let-to-lambda values))))))

        (else (map let-to-lambda expr))))
