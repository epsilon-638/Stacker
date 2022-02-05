#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define module-datum `(module funstacker-mod "funstacker.rkt"
                          (handle-args ,@src-datums)))
  (datum->syntax #f module-datum))
(provide read-syntax)


(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (display (first HANDLE-ARGS-EXPR))))
(provide (rename-out [funstacker-module-begin #%module-begin]))

(define (is-op arg)
	(or (or (equal? - arg) 
					(equal? + arg))
			(or (equal? / arg) 
					(equal? * arg))))

(define (agg-stack accumulator arg)
	(cond 
		[(number? arg) (cons arg accumulator)]
		[(is-op arg)
		 (define op-result
			 (arg (first accumulator) (second accumulator)))
		 (cons op-result (drop accumulator 2))]))

(define (handle-args . args)
	(for/fold ([stack-acc empty])
						([arg (in-list args)]
						 #:unless (void? arg))
						(agg-stack stack-acc arg)))

(provide handle-args + * - /)
