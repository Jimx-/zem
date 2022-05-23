;;; Layout for tests.                                                       
;;;                                                                         
;;; <file:block-test.scm>=                                                  
;;; @subsection Legal Stuff                                                
;;;                                                                         
;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;                                                          
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;;
;;; This file is part of Emacsy.
;;;                                                            
;;; Emacsy is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.
(use-modules (emacsy block)
             (oop goops))

(eval-when (compile load eval)
           ;; Some trickery so we can test private procedures.
           (module-use! (current-module) (resolve-module '(emacsy block))))

;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-                     
;;; @section Block Module                                                  
;;;                                                                         
;;; \epigraph{Wearied I fell asleep: But now lead on; In me is no delay; with thee to go, Is to stay here}{Paradise Lost \\John Milton}
;;;                                                                         
;;; The [[block]] module handles blocking in Emacsy.  When I prototyped     
;;; Emacsy, I considered this the riskiest part of the project.  If I       
;;; couldn't get this to work, it wouldn't be worth trying to develop the   
;;; idea further.  To understand what I mean, one can try running the       
;;; following in Emacs \verb|M-: (read-key)|.  This will evaluate           
;;; [[read-key]] and effectively block until there is another key press.    
;;;                                                                         
;;; Implementing ``blocking'' on a small set of bare functions can be done  
;;; without too much trickery.  However, what if you have computations      
;;; that follow after these functions?  For instance if you evaluate        
;;; \verb|M-: (message "Got %s" (read-key))|, [[read-key]] must block       
;;; until a key is pressed, then resume the computation that will call      
;;; [[message]].  An Operating System must perform a similar operation      
;;; whenever a system call is made, usually implemented using interrupts    
;;; or traps.  Without recourse to interrupts and bare stack manipulation,  
;;; what can we do to achieve a similar feature?                            
;;;                                                                         
;;; GNU Guile has a terrific feature called delimited continuations.  Here  
;;; is an example of a delimited continuation from the Guile Manual.  This  
;;; continuation [[cont]]                                                   
;;;                                                                         
;;; @verbatim                                                        
;;; (define cont                                                            
;;;   (call-with-prompt                                                     
;;;    ;; tag                                                               
;;;    'foo                                                                 
;;;    ;; thunk                                                             
;;;    (lambda ()                                                           
;;;      (+ 34 (abort-to-prompt 'foo)))                                     
;;;    ;; handler                                                           
;;;    (lambda (k) k)))                                                     
;;; @end  verbatim                                                          
;;;                                                                         
;;; \noindent could be rewritten as                                         
;;;                                                                         
;;; @verbatim                                                        
;;; (define cont                                                            
;;;   (lambda (x)                                                           
;;;     (+ 34 x)))                                                          
;;; @end  verbatim.                                                         
;;;                                                                         
;;; \noindent I had to read and re-read this example to let it sink in.     
;;; What does it buy us?  It allows us to abort a computation at any time   
;;; and resume it later.\footnote{Lua's coroutines also seem like a good    
;;;   candidate for pulling off a trick like this.  Python's generators,    
;;;   however, do not.}  So if we were to implement [[read-key]], we abort  
;;; the computation if there has been no key press.  Our main loop in       
;;; \verb|C| continues to run, redraw, wait for key presses.  When a key    
;;; press comes, we can resume that computation---that continuation.        
;;; That's the idea.  What's beautiful about this is that the user code     
;;; has access to the same rich input services as the system code without   
;;; any unnatural contortions.  These ``system calls'' look like regular    
;;; procedure calls much like the Unix call to [[open]] looks like a        
;;; regular function call.                                                  
;;;                                                                         
;;; One of the key features I figured one bought by embedding a             
;;; higher-level language like Scheme was garbage collection.  High-level   
;;; blocking while still being low-level non-blocking is a huge boon.       
;;; What we'll implement is a simple blocking system using Guile's          
;;; delimited continuations, also called prompts.                           
;;;                                                                         
;;; Let's start with the tests, so the usage is somewhat obvious.           
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(define done-blocking? #f)
(define (i-block)
  (block-yield)
  (set! done-blocking? #t))
;;; [[i-block]] will immediately yield.  If it is not called with           
;;; [[call-blockable]] then it will throw an error.                         
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(check-throw (i-block) => 'misc-error)
;;; Now we can call [[i-block]] and capture its continuation.               
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(check-true (call-blockable (lambda () (i-block))))
(check (length blocking-continuations) => 1)
;;; Now we should be able to resume [[i-block]] by running [[block-tick]].  
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(check done-blocking? => #f)
(check (block-tick) => #t)
(check done-blocking? => #t)
(check (length blocking-continuations) => 0)
;;; Let's exercise this [[block-until]] procedure.                          
;;;                                                                         
;;; <block:test>=                                                           
(define continue-blocking? #t)
(define (i-block-until)
  (block-until (lambda () (not continue-blocking?))))
(check (length blocking-continuations) => 0)
(call-blockable (lambda () (i-block-until)))
(check (length blocking-continuations) => 1)
;;; \noindent Now, even if we call [[block-tick]] it shouldn't be resumed.  
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(block-tick)
(check (length blocking-continuations) => 1)
;;; \noindent Let's change the condition for our blocking call.             
;;;                                                                         
;;;                                                                         
;;; <block:test>=                                                           
(set! continue-blocking? #f)
(check (length blocking-continuations) => 1)
(block-tick)
(check (length blocking-continuations) => 0)
;;; \noindent Let's exercise [[block-kill]].                                
;;;                                                                         
;;; <block:test>=                                                           
(set! continue-blocking? #t)
(let ((bc (call-blockable (lambda () (i-block-until)))))
  (check (length blocking-continuations) => 1)
  (block-tick)
  (check (length blocking-continuations) => 1)
  (check-throw (block-kill bc) => 'block-killed)
  ;; The killed block is not cleaned out immediately.
  (check (length blocking-continuations) => 1)
  (block-tick)
  (check (length blocking-continuations) => 0))

;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
