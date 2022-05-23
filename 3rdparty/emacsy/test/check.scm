; <PLAINTEXT>
; Copyright (c) 2005-2006 Sebastian Egner.
; 
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; ``Software''), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
; 
; -----------------------------------------------------------------------
; 
; Lightweight testing (reference implementation)
; ==============================================
;
; Sebastian.Egner@philips.com
; in R5RS + SRFI 23 (error) + SRFI 42 (comprehensions)
;
; history of this file:
;   SE, 25-Oct-2004: first version based on code used in SRFIs 42 and 67
;   SE, 19-Jan-2006: (arg ...) made optional in check-ec
;
; Naming convention "check:<identifier>" is used only internally.

; -- portability --

; PLT:      (require (lib "23.ss" "srfi") (lib "42.ss" "srfi"))
; Scheme48: ,open srfi-23 srfi-42 

; -- utilities --

(define-module (check)
;;  #:use-module (ice-9 syncase)
  #:export (check-set-mode!
            check:mode
            check-reset!
            check-passed?
            check:failed
            check-report
            check-exit
            )
  #:export-syntax (check check-throw check-true check-false use-private-modules)
  ) 

(define check:write write)

; You can also use a pretty printer if you have one.
; However, the output might not improve for most cases
; because the pretty printers usually output a trailing
; newline.

; PLT:      (require (lib "pretty.ss")) (define check:write pretty-print)
; Scheme48: ,open pp (define check:write p)

; -- mode --

(define check:mode #f)

(define (check-set-mode! mode)
  (set! check:mode
        (case mode
          ((off)           0)
          ((summary)       1)
          ((report-failed) 10)
          ((report)        100)
          (else (error "unrecognized mode" mode)))))

(check-set-mode! 'report)

; -- state --

(define check:correct  #f)
(define check:failed   #f)
(define check:error    #f)

(define (check-reset!)
  (set! check:correct 0)
  (set! check:failed   '()))

(define (check:add-correct!)
  (set! check:correct (+ check:correct 1)))

(define (check:add-failed! expression actual-result expected-result)
  (set! check:failed
        (cons (list expression actual-result expected-result)
              check:failed)))

(check-reset!)

; -- reporting --

(define (check:report-expression expression)
  (newline)
  (check:write expression)
  (display " => "))

(define (check:report-actual-result actual-result)
  (check:write actual-result)
  (display " ; "))

(define (check:report-correct cases)
  (display "correct")
  (if (not (= cases 1))
      (begin (display " (")
             (display cases)
             (display " cases checked)")))
  (newline))

(define (check:report-failed expected-result)
  (display "*** failed ***")
  (newline)
  (display " ; expected result: ")
  (check:write expected-result)
  (newline))

(define (check-report)
  (if (>= check:mode 1)
      (begin
        (newline)
        (display "; *** checks *** : ")
        (display check:correct)
        (display " correct, ")
        (display (length check:failed))
        (display " failed.")
        (if (or (null? check:failed) (<= check:mode 1))
            (newline)
            (let* ((w (car (reverse check:failed)))
                   (expression (car w))
                   (actual-result (cadr w))
                   (expected-result (caddr w)))                  
              (display " First failed example:")
              (newline)
              (check:report-expression expression)
              (check:report-actual-result actual-result)
              (check:report-failed expected-result))))))

(define (check-passed? expected-total-count)
  (and (= (length check:failed) 0)
       (= check:correct expected-total-count)))
       
; -- simple checks --

(define (check:proc expression thunk equal expected-result)
  (case check:mode
    ((0) #f)
    ((1)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
           (check:add-correct!)
           (check:add-failed! expression actual-result expected-result))))
    ((10)
     (let ((actual-result (thunk)))
       (if (equal actual-result expected-result)
           (check:add-correct!)
           (begin
             (check:report-expression expression)
             (check:report-actual-result actual-result)
             (check:report-failed expected-result)
             (check:add-failed! expression actual-result expected-result)))))
    ((100)
     (check:report-expression expression)
     (let ((actual-result (thunk)))
       (check:report-actual-result actual-result)
       (if (equal actual-result expected-result)
           (begin (check:report-correct 1)
                  (check:add-correct!))
           (begin (check:report-failed expected-result)
                  (check:add-failed! expression 
				     actual-result 
				     expected-result)))))
    (else (error "unrecognized check:mode" check:mode)))
  (if #f #f))

(define-syntax check
  (syntax-rules (=>)
    ((check expr => expected)
     (check expr (=> equal?) expected))
    ((check expr (=> equal) expected)
     (if (>= check:mode 1)
         (check:proc 'expr (lambda () expr) equal expected)))))

(define-syntax check-throw
  (syntax-rules (=>)
    ((check-throw expr => expected-key)
     (check:proc 'expr (lambda ()
                         (let ((thrown-key 'no-throw))
                          (catch #t
                            (lambda () expr)
                            (lambda (key . args)
                              (set! thrown-key key)))
                          thrown-key)) equal? expected-key))
    ((check-throw expr)
     (check-throw expr => 'no-throw))))

(define-syntax check-true
  (syntax-rules ()
    ((check-true expr)
     (check (if expr #t #f) => #t))))

(define-syntax check-false
  (syntax-rules ()
    ((check-false expr)
     (check expr => #f))))


; -- parametric checks --

(define (check:proc-ec w)
  (let ((correct? (car w))
        (expression (cadr w))
        (actual-result (caddr w))
        (expected-result (cadddr w))
	(cases (car (cddddr w))))
    (if correct?
        (begin (if (>= check:mode 100)
                   (begin (check:report-expression expression)
                          (check:report-actual-result actual-result)
                          (check:report-correct cases)))
               (check:add-correct!))
        (begin (if (>= check:mode 10)
                   (begin (check:report-expression expression)
                          (check:report-actual-result actual-result)
                          (check:report-failed expected-result)))
               (check:add-failed! expression 
				  actual-result 
				  expected-result)))))

(define-syntax check-ec:make
  (syntax-rules (=>)
    ((check-ec:make qualifiers expr (=> equal) expected (arg ...))
     (if (>= check:mode 1)
         (check:proc-ec
	  (let ((cases 0))
	    (let ((w (first-ec 
		      #f
		      qualifiers
		      (:let equal-pred equal)
		      (:let expected-result expected)
		      (:let actual-result
                            (let ((arg arg) ...) ; (*)
                              expr))
		      (begin (set! cases (+ cases 1)))
		      (if (not (equal-pred actual-result expected-result)))
		      (list (list 'let (list (list 'arg arg) ...) 'expr)
			    actual-result
			    expected-result
			    cases))))
	      (if w
		  (cons #f w)
		  (list #t 
			'(check-ec qualifiers 
				   expr (=> equal) 
				   expected (arg ...))
			(if #f #f)
		        (if #f #f)
			cases)))))))))

; (*) is a compile-time check that (arg ...) is a list
; of pairwise disjoint bound variables at this point.

(define-syntax check-ec
  (syntax-rules (nested =>)
    ((check-ec expr => expected)
     (check-ec:make (nested) expr (=> equal?) expected ()))
    ((check-ec expr (=> equal) expected)
     (check-ec:make (nested) expr (=> equal) expected ()))
    ((check-ec expr => expected (arg ...))
     (check-ec:make (nested) expr (=> equal?) expected (arg ...)))
    ((check-ec expr (=> equal) expected (arg ...))
     (check-ec:make (nested) expr (=> equal) expected (arg ...)))

    ((check-ec qualifiers expr => expected)
     (check-ec:make qualifiers expr (=> equal?) expected ()))
    ((check-ec qualifiers expr (=> equal) expected)
     (check-ec:make qualifiers expr (=> equal) expected ()))
    ((check-ec qualifiers expr => expected (arg ...))
     (check-ec:make qualifiers expr (=> equal?) expected (arg ...)))
    ((check-ec qualifiers expr (=> equal) expected (arg ...))
     (check-ec:make qualifiers expr (=> equal) expected (arg ...)))

    ((check-ec (nested q1 ...) q etc ...)
     (check-ec (nested q1 ... q) etc ...))
    ((check-ec q1 q2             etc ...)
     (check-ec (nested q1 q2)    etc ...))))

;; XXX I added this just so I could integrate it with unit tests.
(define (check-exit)
  (exit (if (and #;(= (length test-errors) 0) 
                 (= 0 (length check:failed))) 0 1)))

;; Include everything a module uses including its non-exported
;; interface. This is intended to be used with unit testing ONLY!
(define-syntax use-private-modules
  (syntax-rules ()
    ((use-private-modules . modules)
     (eval-when (compile load eval)
                ;; Some trickery so we can test private procedures.
                (for-each (lambda (module) 
                            (module-use! (current-module) (resolve-module module)))
                          'modules)))))
