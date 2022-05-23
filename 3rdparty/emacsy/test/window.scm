;;; <file:window-test.scm>=                                                 
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
;;; <+ Test Preamble>=                                                      
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())

(use-modules (emacsy window))
(eval-when (compile load eval)
           (module-use! (current-module) (resolve-module '(emacsy window)))) 
;;; <window:Windows Tests>=                                                 
  (check (window? root-window) => #t)
;;; <window:Windows Tests>=                                                 
  (check (window-live? root-window) => #t)
;;; <window:Windows Tests>=                                                 
(check (edges->bcoords '(0 1 1 0)) => '(0 0 1 1))
;;; <window:Windows Tests>=                                                 
(check (bcoords->edges '(0 0 1 1)) => '(0 1 1 0))
;;; Let's project a point in the current window to the point in its         
;;; ultimate parent window.                                                 
;;;                                                                         
;;;                                                                         
;;; <window:Windows Tests>=                                                 
(define i-window (make <internal-window>))
(define window (make <window>))
(check (window? i-window) => #t)
(check (window? window) => #t)
;;; Let's test window splitting.                                            
;;;                                                                         
;;;                                                                         
;;; <window:Windows Tests>=                                                 
(check (procedure? split-window) => #t)
(define s-window (split-window window))
(check (is-a? s-window <internal-window>) => #t)
;;; Let's test window splitting with a different size value.                
;;;                                                                         
;;; <window:Windows Tests>=                                                 
(define small-window (make <window>))
(define parent-window (split-window small-window 0.2))
(define big-window (cdr (window-children parent-window)))
(check (orientation parent-window) => 'vertical)
;;; Let's test window splitting with a different orientation.               
;;;                                                                         
;;;                                                                         
;;; <window:Windows Tests>=                                                 
(define left-window (make <window>))
(define parent-window-2 (split-window left-window 0.2 'right))
(define right-window (cdr (window-children parent-window-2)))
(check (orientation parent-window-2) => 'horizontal)
;;; <window:Windows Tests>=                                                 
(let* ((w (make <window>))
       (sw (split-window w))
       (c (cadr (window-children sw)))
       (sc (split-window c))
       (nc (cadr (window-children sc)))
  )

  (check (window-list w) => (list w))
  (check (window-tree sw) => (list w (list c nc)))
  (check (window-list sw) => (list w c nc))
  ;(check (window-list sw) => (list w c #f))
  ) 


;;; <+ Test Postscript>=                                                    
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
