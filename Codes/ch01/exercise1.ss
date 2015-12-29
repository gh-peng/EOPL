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

(define (minus1 n)
  (- n 1))

(define (duple n x)
  (cond
    ((zero? n) '())
    (else (cons x (duple (minus1 n) x)))))
(define (tail-duple n x)
  (define (f n r)
    (cond
      ((zero? n) r)
      (else (f (minus1 n) (cons x r)))))
  (f n '()))
(assert (duple 2 3) '(3 3))
(assert (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(assert (duple 0 '(blah)) '())
(assert (tail-duple 2 3) '(3 3))
(assert (tail-duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(assert (tail-duple 0 '(blah)) '())
(define (invert lst)
  (cond
    ((null? lst) '())
    (else (cons (list (cadar lst) (caar lst)) (invert (cdr lst))))))
(define (tail-invert lst)
  (define (f lst r)
    (cond
      ((null? lst) r)
      (else (f (cdr lst) (append r (list (list (cadar lst) (caar lst))))))))
  (f lst '()))
(assert (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))
(assert (tail-invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))
(define (down lst)
  (cond
    ((null? lst) '())
    (else
     (cons
      (list (car lst))
      (down (cdr lst))))))
(define (tail-down lst)
  (define (f lst r)
    (cond
      ((null? lst) r)
      (else (f (cdr lst) (append r (list (list (car lst))))))))
  (f lst '()))
(assert (down '(1 2 3)) '((1) (2) (3)))
(assert (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(assert (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))
(assert (tail-down '(1 2 3)) '((1) (2) (3)))
(assert (tail-down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(assert (tail-down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))

(define (swapper s1 s2 slist)
  (cond
    ((symbol? slist)
     (cond
        ((eq? s1 slist) s2)
        ((eq? s2 slist) s1)
        (else slist)))
    ((null? slist) '())
    (else
     (cons (swapper s1 s2 (car slist))
           (swapper s1 s2 (cdr slist))))))
(assert (swapper 'a 'd '(a b c d)) '(d b c a))
(assert (swapper 'a 'd '(a d () c d)) '(d a () c a))
(assert (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))

(define (product sos1 sos2)
  (define (f s sos2)
    (cond
      ((null? sos2) '())
      (else (cons (list s (car sos2)) (f s (cdr sos2))))))
  (cond
    ((null? sos1) '())
    (else (append (f (car sos1) sos2) (product (cdr sos1) sos2)))))
(assert (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

(define (filter-in pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst)) (cons (car lst) (filter-in pred (cdr lst))))
    (else (filter-in pred (cdr lst)))))
(define (tail-filter-in pred lst)
  (define (f lst r)
    (cond
      ((null? lst) r)
      (else
       (f (cdr lst) (if (pred (car lst))
                        (append r (list (car lst)))
                        r)))))
  (f lst '()))
(assert (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(assert (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
(assert (tail-filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(assert (tail-filter-in symbol? '(a (b c) 17 foo)) '(a foo))

(define (list-index pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) 0)
    (else (let ((r (list-index pred (cdr lst))))
            (if (integer? r)
                (+ 1 r)
                #f)))))
(define (tail-list-index pred lst)
  (define (f lst r)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) r)
      (else (f (cdr lst) (+ 1 r)))))
  (f lst 0))
(assert (list-index number? '(a 2 (1 3) b 7)) 1)
(assert (list-index symbol? '(a (b c) 17 foo)) 0)
(assert (list-index symbol? '(1 2 (a b) 3)) #f)
(assert (tail-list-index number? '(a 2 (1 3) b 7)) 1)
(assert (tail-list-index symbol? '(a (b c) 17 foo)) 0)
(assert (tail-list-index symbol? '(1 2 (a b) 3)) #f)
(define (every? pred lst)
  (cond
    ((null? lst) #t)
    ((pred (car lst)) (every? pred (cdr lst)))
    (else #f)))
(assert (every? number? '(a b c 3 e)) #f)
(assert (every? number? '(1 2 3 5 4)) #t)

(define (exists? pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (exists? pred (cdr lst)))))
(assert (exists? number? '(a b c 3 e)) #t)
(assert (exists? number? '(a b c d e)) #f)

(define (up lst)
  (define (f0 l lst)
    (cond
      ((null? l) lst)
      (else (cons (car l) (f0 (cdr l) lst)))))
  (define (f x lst)
    (cond
      ((list? x) (f0 x lst))
      (else (cons x lst))))
  (cond
    ((null? lst) '())
    (else (f (car lst) (up (cdr lst))))))
(assert (up '((1 2) (3 4))) '(1 2 3 4))
(assert (up '((x (y)) z)) '(x (y) z))
(assert (up '((x (y)) z (x))) '(x (y) z x))

(define (flatten slist)
  (define (f0 l lst)
    (cond
      ((null? l) lst)
      (else (cons (car l) (f0 (cdr l) lst)))))
  (define (f x lst)
    (cond
      ((list? x) (f0 x lst))
      (else (cons x lst))))
  (cond
    ((not (list? slist)) slist)
    ((null? slist) '())
    (else (f (flatten (car slist)) (flatten (cdr slist))))))

(assert (flatten '(a b c)) '(a b c))
(assert (flatten '((a) () (b ()) () (c))) '(a b c))
(assert (flatten '((a b) c (((d)) e))) '(a b c d e))
(assert (flatten '(a b (() (c)))) '(a b c))

(define (merge loi1 loi2)
  (cond
    ((null? loi1) loi2)
    ((null? loi2) loi1)
    ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
    (else (cons (car loi2) (merge loi1 (cdr loi2))))))

(define (tail-merge loi1 loi2)
  (define (f loi1 loi2 r)
    (cond
      ((null? loi1) (append r loi2))
      ((null? loi2) (append r loi1))
      ((< (car loi1) (car loi2)) (f (cdr loi1) loi2 (append r (list (car loi1)))))
      (else (f loi1 (cdr loi2) (append r (list (car loi2)))))))
  (f loi1 loi2 '()))

(assert (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(assert (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))
(assert (tail-merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(assert (tail-merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))

(define (sort loi)
  (define (f pred loi)
    (cond
      ((null? loi) '())
      ((pred (car loi)) (cons (car loi) (f pred (cdr loi))))
      (else (f pred (cdr loi)))))
  (cond
    ((null? loi) '())
    (else
     (append
      (sort (f (lambda (x) (< x (car loi))) (cdr loi)))
      (list (car loi))
      (sort (f (lambda (x) (>= x (car loi))) (cdr loi)))))))
(assert (sort '(8 2 5 2 3)) '(2 2 3 5 8))

(define (sort/predicate pred loi)
  (define (f pred loi)
    (cond
      ((null? loi) '())
      ((pred (car loi)) (cons (car loi) (f pred (cdr loi))))
      (else (f pred (cdr loi)))))
  (cond
    ((null? loi) '())
    (else
     (append
      (sort/predicate pred (f (lambda (x) (pred x (car loi))) (cdr loi)))
      (list (car loi))
      (sort/predicate pred (f (lambda (x) (not (pred x (car loi)))) (cdr loi)))))))
(assert (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(assert (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))

(define (bintree? t)
  (cond
    ((list? t) (and
                (symbol? (car t))
                (bintree? (car (cdr t)))
                (bintree? (car (cdr (cdr t))))))
    ((integer? t) #t)
    (else #f)))

(define (fun-tree t fun)
  (cond
    ((list? t)
     (list (car t)
           (fun-tree (cadr t) fun)
           (fun-tree (caddr t) fun)))
    (else (fun t))))
(define (double-tree t)
  (fun-tree t (lambda (x) (if (list? x) x (* 2 x)))))


(assert
 (double-tree '(root
                (left0 1 2)
                (right0 2 3)))
 '(root
   (left0 2 4)
   (right0 4 6)))
(define (interior-node l r s)
  (list l r s))
(define (leaf n)
  n)
(define (mark-leaves-with-red-depth t)
  (define (f t d)
    (cond
      ((list? t)
       (let ((nd (if (eq? 'red (car t)) (+ 1 d) d)))
         (list (car t)
                (f (cadr t) nd)
                (f (caddr t) nd))))
       (else d)))
  (f t 0))
(assert
 (mark-leaves-with-red-depth
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))
 '(red
   (bar 1 1)
   (red 2 (quux 2 2))))

(define (path n bst)
  (define (f bst)
    (cond
      ((null? bst) #f)
      ((= n (car bst)) '())
      ((< n (car bst)) (cons 'left (f (cadr bst))))
      (else (cons 'right (f (caddr bst))))))
  (f bst))

(assert
 (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())
                                         ())
                                     (31 () ()))))
 '(right left left))
