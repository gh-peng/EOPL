#lang mzscheme

;(load "../Libs/basic.ss")

(define-syntax assert
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
                   'test-exp
                   observed-ans
                   correct-ans))))))
(define (list-length l)
  (define (f l c)
    (cond
      ((null? l) c)
      (else (f (cdr l) (+ 1 c)))))
  (f l 0))
(define (nth-element l n)
  (cond
    ((zero? n) (car l))
    (else (nth-element (cdr l) (- n 1)))))
(assert (nth-element '(1 2 3) 0) 1)
(assert (nth-element '(1 2 3) 1) 2)
(assert (nth-element '(1 2 3) 2) 3)
(define (remove-first c l)
  (cond
    ((null? l) l)
    ((eq? (car l) c) (cdr l))
    (else (cons (car l) (remove-first c (cdr l))))))
(assert (remove-first 'a '(a b c)) '(b c))
(assert (remove-first 'b '(e f g)) '(e f g))
(assert (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
(assert (remove-first 'a '()) '())

(define (in-s? n)
  (cond
    ((< n 0) #f)
    ((= n 0) #t)
    (else (in-s? (- n 3)))))
(assert (in-s? 4) #f)
(assert (in-s? 6) #t)

(define (listofpred? pred l)
  (cond
    ((null? l) #t)
    (else
     (and (pred (car l))
          (listofpred? pred (cdr l))))))

(define (listofint? l)
  (listofpred? integer? l))
(assert (listofint? '(1 2 3 4 5 6 7)) #t)

(define (listofsymbol? l)
  (listofpred? symbol? l))
(assert (listofsymbol? '(a b c d e f g h i j k)) #t)
(assert (listofsymbol? '(a b c d e 2 3 4 5 6 7)) #f)


(define (s-exp? s)
  (cond
    ((symbol? s) #t)
    (else (s-list? s))))
(define (s-list? s)
  (cond
    ((null? s) #t)
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
             (lc-exp? (cadr s))) #t)
           ((and
             (= 3 lth)
             (eq? 'lambda (car s))
             (identifier? (caadr s))4
             (lc-exp? (caddr s))) #t)
           (else #f)))))
    (else #f)))
;(assert (lc-exp? '(lambda (x) (+ 5))) #t)

(define (subst n o s-list)
  (cond
    ((symbol? s-list) (if (eq? o s-list) n s-list))
    ((null? s-list) '())
    (else
     (cons
      (subst n o (car s-list))
      (subst n o (cdr s-list))))))
(s-list? '((b c) (b () d)))
(assert (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
(define (number-elements-from lst n)
  (define (f lst n r)
    (cond
      ((null? lst) r)
      (else
       (f (cdr lst) (+ n 1) (append r (list (list n (car lst))))))))
  (f lst n '()))
(assert (number-elements-from '(v0 v1 v2) 0) '((0 v0) (1 v1) (2 v2)))
(define (list-sum loi)
  (define (f loi r)
    (cond
      ((null? loi) r)
      (else
       (f (cdr loi) (+ r (car loi))))))
  (f loi 0))
(assert (list-sum '(1 2 3 4 5 6 7 8 9 10)) 55)


