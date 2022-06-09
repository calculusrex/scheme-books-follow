



(define (square x) (* x x))

;; ------------------------------------------------------

(define (compose f g)
  (lambda args
    (f (apply g args))))

(define* ((iterate n) f)
  (if (= n 0)
      identity
      (compose f
	       ((iterate (- n 1)) f))))

(define (identity x) x)

;; (define (iterate n f)
;;   (if (= n 0)
;;       identity
;;       (compose f
;; 	       (iterate (- n 1) f))))

;; ;; an experiment of mine to check when largenums fail
;; (define (foo n x)
;;   (let ((f (compose
;; 	    ((iterate n) sqrt)
;; 	    ((iterate n) square))))
;;     (f x)))

(define (parallel-combine-- h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(define (parallel-combine h f g)
  (lambda args
    (h (apply f args) (apply g args))))


