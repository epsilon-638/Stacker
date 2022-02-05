#lang br/quicklang

(define (read-syntax path port)
	(define src-lines (port->lines port))
	(define src-datums (format-datums '(stack-em ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
																,@src-datums))
	(datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
		 (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(require racket/base)
(define stack empty)

(define (pop-stack!)
	(define arg (first stack))
	(set! stack (rest stack))
	arg)

(define (push-stack! arg)
	(set! stack (cons arg stack)))

(define (is-op arg)
	(or (or (equal? - arg) 
					(equal? + arg))
			(or (equal? / arg) 
					(equal? * arg))))

(define (stack-em [arg #f])
	(cond
		[(number? arg) (push-stack! arg)]
		[(is-op arg) 
		 (define op-result (arg (pop-stack!) (pop-stack!)))
		 (push-stack! op-result)]))

(provide stack-em + * - /)
