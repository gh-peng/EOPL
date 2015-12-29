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

(define (zero)
  '())
(define (iszero? x)
  (eqv? x '()))

(define (list->int loi base)
  (define (f loi r b)
    (cond
      ((null? loi) r)
      (else (f (cdr loi) (+ r (* (car loi) b)) (* base b)))))
  (f loi 0 1))
(define (int->list n base)
    (cond
      ((zero? n) '())
      (else (cons (remainder n base) (int->list (quotient n base) base)))))
(define (int->list2 n base)
    (cond
      ((zero? n) '())
      (else (append (int->list2 (quotient n base) base) (list (remainder n base))))))

(assert (list->int '(1 2) 16) 33)
(assert (list->int '(2 0 1) 16) 258)
(assert (int->list 33 16) '(1 2))
(assert (int->list 258 16) '(2 0 1))
(define (successor x)
  (int->list (+ 1 (list->int x 16))  16))
(define (predecessor x)
  (int->list (+ -1 (list->int x  16))  16))

(assert (iszero? (zero)) #t)
(define (plus a b)
  (cond
    ((iszero? b) a)
    (else (plus (successor a) (predecessor b)))))

(define two (successor (successor (zero))))
(define three (successor two))
;(display (plus two three))
;(display (plus (int->list 12 16) (int->list 13 16)))

(define (int->2 n)
  (int->list2 n 2))
(define (int->8 n)
  (int->list2 n 8))
(define (int->16 n)
  (define (f0 o)
    (cond
      ((= 15 o) 'F)
      ((= 14 o) 'E)
      ((= 13 o) 'D)
      ((= 12 o) 'C)
      ((= 11 o) 'B)
      ((= 10 o) 'A)
      (else o)))
  (define (f1 l)
    (cond
      ((null? l) '())
      (else (cons (f0 (car l)) (f1 (cdr l))))))
  (f1 (int->list2 n 16)))
(assert (int->16 188) '(B C))
(assert (int->16 48377) '(B C F 9))
(assert (int->16 81985529216486895) '(1 2 3 4 5 6 7 8 9 A B C D E F))