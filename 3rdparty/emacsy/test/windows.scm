;;; <file:windows-test.scm>=
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

(use-modules (emacsy windows))
(use-private-modules (emacsy windows))

;; (eval-when (compile load eval)
;;            (module-use! (current-module) (resolve-module '(emacsy windows))))
;;; <windows:Windows Tests>=
(check (window? root-window) => #t)
;;; <windows:Windows Tests>=
  (check (window-live? root-window) => #t)
;;; <windows:Windows Tests>=
(check (edges->bcoords '(0 1 1 0)) => '(0 0 1 1))
;;; <windows:Windows Tests>=
(check (bcoords->edges '(0 0 1 1)) => '(0 1 1 0))
;;; <windows:Windows Tests>=
(define i-window (make <internal-window>))
(define window (make <window>))
(check (window? i-window) => #t)
(check (window? window) => #t)
;;; Let's test window splitting.
;;;
;;;
;;; <windows:Windows Tests>=
(check (procedure? split-window) => #t)
(define s-window (split-window window))
(check (is-a? s-window <internal-window>) => #t)
(check (window-pixel-bcoords s-window) => '(0 0 1 1))
(check (window-pixel-bcoords window) => '(0. .5 1. .5))
;;; Let's test window splitting with a different size value.
;;;
;;; <windows:Windows Tests>=
(define small-window (make <window>))
(define parent-window (split-window small-window 0.2))
(define big-window (cdr (window-children parent-window)))
(check (orientation parent-window) => 'vertical)
(check (window-pixel-bcoords small-window) => '(0. .2 1. .2))
(check (window-pixel-bcoords big-window) => '(0. 0. 1. .8))
;;; Let's test window splitting with a different orientation.
;;;
;;;
;;; <windows:Windows Tests>=
(define left-window (make <window>))
(define parent-window-2 (split-window left-window 0.2 'right))
(define right-window (cdr (window-children parent-window-2)))
(check (orientation parent-window-2) => 'horizontal)
(check (window-pixel-bcoords left-window) => '(0. 0. .2 1.))
(check (window-pixel-bcoords right-window) => '(.2 .0 .8 1.))
;;; Let's test the pixel-window at the top of the hierarchy.
;;;
;;;
;;; <windows:Windows Tests>=
(define pixel-window (make <pixel-window> #:pixel-size '(500 400)))

;(update-window pixel-window)
;(define sub-window   (window-child pixel-window))
(define sub-window   (make <window>))
(check (window? pixel-window) => #t)
(check (window? sub-window) => #t)
(set! (window-parent sub-window) pixel-window)
(set! (window-child pixel-window) sub-window)
(check (window-child pixel-window) => sub-window)
(check (window-project sub-window #(1. 1. 1.)) => #(500. 400. 1.))
(check (window-project sub-window #(0. 0. 1.)) => #(0. 0. 1.))
(format #t "Splitting the window\n")
(define sub-window-2 (split-window sub-window))
(check (window-project sub-window #(1. 1. 1.)) => #(500. 400. 1.))
(check (window-project sub-window #(0. 0. 1.)) => #(0. 200. 1.))

(check (window-unproject sub-window #(0. 200. 1.)) => #(0. 0. 1.))
;;; <windows:Windows Tests>=
(let* ((w (make <window>))
       (sw (split-window w))
       (c (cdr (window-children sw)))
       (sc (split-window c))
  )

  (check (window-list w) => (list w))
  (check (window-tree sw) => (list w c))
  (check (window-list sw) => (list w c))
  (check (window-list sw) => (list w c #f))
  )
;;; <windows:Projection Tests>=
(check (window-project window #(0 0 1)) => #(0. .5 1.))
(check (window-project window #(1. 1. 1.)) => #(1. 1. 1.))
(check (window-unproject window #(0 .5 1.)) => #(0. 0. 1.))
(check (window-unproject window #(1. 1. 1.)) => #(1. 1. 1.))

;;; <+ Test Postscript>=
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
