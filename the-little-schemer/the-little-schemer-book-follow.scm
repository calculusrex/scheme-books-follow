
;; PREREQUISITES

(define (atom? x) (and (not (pair? x))
		       (not (null? x))))

(define (sub1 x) (1- x)) (define (add1 x) (1+ x))

(define (zero? x)
  (= x 0))

;; -----------------------------------------------

(define (range a b)
  (if (>= a b)
      '()
      (cons a (range (1+ a) b))))

(define (rng n)
  (range 0 n))

;; -----------------------------------------------

(define (lat?-- xs)
  (cond ((atom? xs) #f)
	((null? xs) #t)
	((pair? (car xs)) #f)
	(else (lat? (cdr xs)))))




(define lat?
  (lambda (l)
    (cond ((null? l) #t)
	  ((atom? (car l)) (lat? (cdr l)))
	  (else #f)   )))




(define (member-- a lat)
  (cond ((null? lat) #f)
	((eq? a (car lat)) lat)
	(else (member a (cdr lat)))))

(define (rember a lat)
  (cond ((null? lat) '())
	((eq? a (car lat)) (cdr lat))
	(else (cons (car lat)
		    (rember a (cdr lat))))))

(define (replace a b xs)
  (cond ((null? xs) '())
	((eq? a (car xs)) (cons b (cdr xs)))
	(else (cons (car xs)
		    (replace a b (cdr xs))))))

(define (firsts xss)
  (cond ((null? xss) '())
	(else (cons (car (car xss))
		    (firsts (cdr xss))))))

(define (firsts-- xss)
  (cond ((null? xss) '())
	((atom? (car xss)) (firsts (cdr xss)))
	(else (cons (car (car xss))
		    (firsts (cdr xss))))))

(define (eqan? a b)
  (if (and (number? a) (number? b))
      (= a b)
      (eq? a b)))


;; (define (insertR a b l)
;;   (

(define (rember-tree a l)
  (cond ((null? l) '())
	((atom? (car l))
	 (if (eqan? (car l) a)
	     (rember-tree a (cdr l))
	     (cons (car l)
		   (rember-tree a
				(cdr l)))))
	(else
	 (cons (rember-tree a (car l))
	       (rember-tree a (cdr l))))))


(define sau
  (lambda (a b)
    (cond (a #t)
	  (else b))))

;; (define sau
;;   (lambda (a b)
;;     (cond (a #t)
;; 	  (b #t)
;; 	  (else #f))))


;; (define member?
;;   (lambda (a lat)
;;     (cond ((null? lat) #f)
;; 	  (else (or (eq? a (car lat))
;; 		    (member? a (cdr lat)))))))


;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) '())
;;      (else (cond
;; 	    ((eq? (car lat) a) (cdr lat))
;; 	    (else (cons (car lat)
;; 			(rember a (cdr lat)))))))))




;; (define a 'x)
;; (define l '((x y) y z x (((x y z) x y a z) x)))

(define (rmember* a l)
  (cond ((null? l) '())
	((pair? (car l)) (cons (rmember* a (car l))
			       (rmember* a (cdr l))))
	((eq? a (car l)) (rmember* a (cdr l)))
	(else (cons (car l)
		    (rmember* a (cdr l))))))


(define (insertR* new old l)
  (cond ((null? l) '())
	((pair? (car l)) (cons (insertR* new old (car l))
			       (insertR* new old (cdr l))))
	((eq? old (car l)) (cons old
				 (cons new
				       (insertR* new old (cdr l)))))
	(else (cons (car l)
		    (insertR* new old (cdr l))))))

;; (define new 'pecker)
;; (define old 'chuck)
;; (define l '(
;; 	    (how much (wood))
;; 	    could
;; 	    ((a (wood) chuck))
;; 	    (((chuck)))
;; 	    (if (a) ((wood chuck)))
;; 	    could chuck wood))


(define (insertL* new old l)
  (cond ((null? l) '())
	((pair? (car l)) (cons (insertL* new old (car l))
			       (insertL* new old (cdr l))))
	((eq? old (car l)) (cons new
				 (cons old
				       (insertL* new old (cdr l)))))
	(else (cons (car l)
		    (insertL* new old (cdr l))))))

;; (define a 'banana)
;; (define l '((banana)
;; 	    (split ((((banana ice)))
;; 		    (cream (banana))
;; 		    sherbet))
;; 	    (banana)
;; 	    (bread)
;; 	    (banana brandy)))

(define (occur* a l)
  (cond ((null? l) 0)
	((pair? (car l)) (+ (occur* a (car l))
			    (occur* a (cdr l))))
	((eq? a (car l)) (1+ (occur* a (cdr l))))
	(else (occur* a (cdr l)))))


(define (occur** a l)
  (cond ((null? l) 0)
	((atom? (car l))
	 (if (eq? a (car l))
	     (1+ (occur** a (cdr l)))
	     (occur** a (cdr l))))
	(else (+ (occur** a (car l))
		 (occur** a (cdr l))))))


(define (subst* new old l)
  (cond ((null? l) '())
	((atom? (car l))
	 (if (eq? old (car l))
	     (cons new (subst* new old (cdr l)))
	     (cons (car l)
		   (subst* new old (cdr l)))))
	(else (cons (subst* new old (car l))
		    (subst* new old (cdr l))))))


;; (define a 'chips)
;; (define l '((potato) (chips ((with) fish) (chips))))

(define (member* a l)
  (cond ((null? l) #f)
	((pair? (car l))
	 (or (member* a (car l))
	     (member* a (cdr l))))
	((eq? a (car l)) #t)
	(else (member* a (cdr l)))))

(define (member** a l)
  (cond ((null? l) #f)
	((pair? (car l))
	 (or (member** a (car l))
	     (member** a (cdr l))))
	((eq? a (car l)) #t)
	(else (or #f
		  (member** a (cdr l))))))

(define (member*** a l)
  (cond ((null? l) #f)
	((atom? (car l))
	 (if (eq? a (car l))
	     #t
	     (member*** a (cdr l))))
	(else (or (member*** a (car l))
		  (member*** a (cdr l))))))

(define (leftmost-- l)
  (cond ((null? l) '())
	((atom? (car l)) (car l))
	(else (let ((f (lambda (a b)
			 (cond ((and (null? a) (null? b)) '())
			       ((null? a) b)
			       ((null? b) a)
			       (else a)))))
		(f (leftmost-- (car l))
		   (leftmost-- (cdr l)))))))
	      
(define (leftmost l)
  (if (atom? (car l))
      (car l)
      (leftmost (car l))))


(define (tup+ xs ys)
  (cond ((null? xs) ys)
	((null? ys) xs)
	(else (cons (+ (car xs)
		       (car ys))
		    (tup+ (cdr xs)
			  (cdr ys))))))

;; (define (> x y)
;;   (cond ((= x 0) #f)
;; 	((= y 0) #t)
;; 	(else (> (sub1 x) (sub1 y)))))

;; (define (< x y)
;;   (cond ((= y 0) #f)
;; 	((= x 0) #t)
;; 	(else (< (sub1 x) (sub1 y)))))

;; (define (== x y)
;;   (cond ((and (zero? x) (zero? y)) #t)
;; 	((or (zero? x) (zero? y)) #f)
;; 	(else (== (sub1 x) (sub1 y)))))

(define (== x y)
  (cond ((zero? x) (zero? y))
	((zero? y) #f)
	(else (== (sub1 x) (sub1 y)))))

(define (^ x y)
  (if (= y 0)
      1
      (* x (^ x (sub1 y)))))

;; (div 7 2) -> (1+ (div 5 2)) -> (1+ (1+ (div 3 2))) -> (1+ (1+ (1+ (div 1 2)))) -> (1+ (1+ (1+ 0)))

(define (div x y)
  (if (> y x)
      0
      (1+ (div (- x y) y))))

(define (lungime xs)
  (if (null? xs)
      0
      (1+ (length (cdr xs)))))
      

(define (pick n lat)
  (if (zero? (sub1 n))
      (car lat)
      (pick (sub1 n) (cdr lat))))

;; (define (rempick n lat)
;;   (if (zero? (sub1 n))
;;       (cdr lat)
;;       (cons (car lat)
;; 	    (rempick (sub1 n)
;; 		     (cdr lat)))))


(define (no-nums xs)
  (cond ((null? xs) '())
	((number? (car xs)) (no-nums (cdr xs)))
	(else (cons (car xs)
		    (no-nums (cdr xs))))))


(define (all-nums xs)
  (cond ((null? xs) '())
	((number? (car xs)) (cons (car xs)
				  (all-nums (cdr xs))))
	(else (all-nums (cdr xs)))))

(define (one? n)
  (zero? (sub1 n)))


(define (rempick n lat)
  (if (one? n)
      (cdr lat)
      (cons (car lat)
	    (rempick (sub1 n)
		     (cdr lat)))))

;; (define (eqlist? xs ys)
;;   (cond ((and (null? xs) (null? ys)) #t)
;; 	((or (null? xs) (null? ys)) #f)
;; 	((eqan? (car xs) (car ys))
;; 	 (eqlist? (cdr xs) (cdr ys)))
;; 	(else #f)))


(define (eqlist? xs ys)
  (cond ((and (null? xs) (null? ys)) #t)
	((or (null? xs) (null? ys)) #f)
	((and (pair? (car xs)) (pair? (car ys)))
	 (and (eqlist? (car xs) (car ys))
	      (eqlist? (cdr xs) (cdr ys)))) 
	((or (pair? (car xs)) (pair? (car ys))) #f)
	((eqan? (car xs) (car ys))
	 (eqlist? (cdr xs) (cdr ys)))
	(else #f)))

;; (define (eqlist? xs ys)
;;   (cond ((and (null? xs) (null? ys)) #t)
;; 	((or (null? xs) (null? ys)) #f)
;; 	(else (and (equal? (car xs) (car ys))
;; 		   (eqlist? (cdr xs) (cdr ys))))))
	

;; eu: page 92, yoyo: page 57

(define (equal? a b)
  (cond ((and (pair? a) (pair? b)) (eqlist? a b))
	((or (pair? a) (pair? b)) #f)
	(else (eqan? a b))))

(define (plus a b)
  (cond ((zero? b) a)
	(else (add1 (plus a (sub1 b))))))

(define (plus a b)
  (cond ((zero? b) a)
	(else (plus (add1 a) (sub1 b)))))


(define test-expressions--numbered?
  '((3 + (x ^ 2))
    (2 + (2 * 4))
    (x + (2 - 3))
    ((1 + 1) / (3 - 10))
    ((1 + x) / (3 ^ 10))))

(define numbered?--accepted-symbols
  '(+ * ^ /))

(define (numbered? z)
  (cond ((null? z) #t)
	((number? (car z)) (numbered? (cdr z)))
	((symbol? (car z))
	 (if (member (car z)
		     numbered?--accepted-symbols)
	     (numbered? (cdr z))
	     #f))
	(else (or (numbered? (car z))
		  (numbered? (cdr z))))))

(define (test-numbered? expressions)
  (let ((single-test
	 (lambda (expr)
	   (display expr)
	   (display " -> ")
	   (display (numbered? expr))
	   (newline))))
    (let loop ((exprs expressions))
      (if (null? (cdr exprs))
	  (single-test (car exprs))
	  (begin
	    (single-test (car exprs))
	    (loop (cdr exprs)))))))
			 
