#lang mzscheme

(define-syntax assert
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
                   'test-exp
                   observed-ans
                   correct-ans))))))

(define (in-s? n)
  (cond
    ((< n 0) #f)
    ((= n 0) #t)
    (else (in-s? (- n 3)))))
(assert (in-s? 4) #f)
(assert (in-s? 6) #t)

(define (listofint? l)
  (cond
    ((eq? '() l) #t)
    (else
     (and (integer? (car l))
          (listofint? (cdr l))))))
(assert (listofint? '(1 2 3 4 5 6 7)) #t)

(define (s-exp? s)
  (cond
    ((symbol? s) #t)
    (else (s-list? s))))
(define (s-list? s)
  (cond
    ((eq? '() s) #t)
    (else (and
           (s-exp? (car s))
           (s-exp? (cdr s))))))
(assert (s-list? '(a b c)) #t)
(assert (s-list? '(an (((s-list)) (with () lots) ((of) nesting)))) #t)

(define (bintree? t)
  (cond
    ((integer? t) #t)
    (else
     (and
      (symbol? (car t))
      (bintree? (car (cdr t)))
      (bintree? (car (cdr (cdr t))))))))
(assert (bintree? 1) #t)
(assert (bintree? 2) #t)
(assert (bintree? '(foo 1 2)) #t)
(assert (bintree? '(bar 1 (foo 1 2))) #t)
(assert (bintree? '(baz
                    (bar 1 (foo 1 2))
                    (biz 4 5))) #t)

(define (list-length l)
  (cond
    ((eq? '() l) 0)
    (else (+ 1 (list-length (cdr l))))))

(define (lc-exp? s)
  (define (identifier? s)
    (and
     (symbol? s)
     (not (eq? 'lambda s))))
  (cond
    ((identifier? s) #t)
    ((and
      (list? s)
      (let ((lth (list-length s)))
        (cond
          ((and
             (= 2 lth)
             (lc-exp? (car s))
             (lc-exp? (car (cdr s)))) #t)
           ((and
             (= 3 lth)
             (eq? 'lambda (car s))
             (identifier? (caadr s))
             (lc-exp? (caddr s))) #t)
           (else #f)))))
    (else #f)))

(assert (lc-exp? '(lambda (x) (+ x 5))) #t)