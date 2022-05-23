;;; <file:mru-stack-test.scm>=
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
(use-modules (emacsy mru-stack)
             (check))

(use-private-modules (emacsy mru-stack))
;;; <mru-stack:test>=
(define s (make <mru-stack>))
(mru-add! s 'a)
(mru-add! s 'b)
(mru-add! s 'c)
(check (mru-list s) => '(c b a))
(check (mru-recall! s 'a) => '(a c b))
(check (mru-ref s) => 'a)
(mru-next! s)
(check (mru-ref s) => 'c)
(mru-next! s)
(check (mru-ref s) => 'b)
(mru-next! s)
(check (mru-ref s) => 'a)
(mru-prev! s)
(check (mru-ref s) => 'b)
(check (mru-list s) => '(a c b))
(mru-remove! s 'c)
(check (mru-list s) => '(a b))
(check (mru-ref s) => 'b)
(mru-remove! s 'a)
(mru-remove! s 'b)
(check (mru-list s) => '())
(check (mru-ref s) => #f)
(mru-next! s)
(check (mru-ref s) => #f)
(mru-add! s 'a)
(mru-add! s 'b)
(mru-add! s 'c)
(check (mru-list s) => '(c b a))
(mru-remove! s 'c)
(check (mru-list s) => '(b a))
(check (mru-ref s) => 'b)
(let ((ms (make <mru-stack>)))
  (for-each (lambda (x) (mru-add! ms x)) '(a b c))
  (mru-next! ms)
  (mru-remove! ms 'a)
  (mru-remove! ms 'b)
  (mru-remove! ms 'c)
  (check (mru-list ms) => '()))
(check-exit)
