;;; Guimax --- Guile UI with Emacsy
;;; Copyright (C) 2009, 2010, 2011, 2018 Jose Antonio Ortega Ruiz
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Taken from Geiser: guile/geiser/{modules,xref}.scm

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Modified BSD License. You should
;;; have received a copy of the license along with this program. If
;;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;;; Commentary:

;; @node Introspection
;; @section Introspection

;;; Code:

(define-module (emacsy introspection)
  #:use-module (system vm program)
  #:use-module (system vm debug)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)

  #:use-module (emacsy agenda)
  #:use-module (emacsy block)
  #:use-module (emacsy buffer)
  #:use-module (emacsy command)
  #:use-module (emacsy core)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy event)
  #:use-module (emacsy help)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy keymap)
  #:use-module (emacsy klecl)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy mode)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy text)
  #:use-module (emacsy util)

  #:export (all-modules
            all-variables
            find-module
            find-object
            module-file-name
            module-location
            module-name?
            object-name
            object-type
            program-location
            program-module
            submodules
            symbol->object
            symbol-location
            symbol-module
            symbol-type
            xassq))

(define (make-location file-name line)
  (list (cons "file-name" (and (string? file-name) file-name))
        (cons "file" (and (string? file-name) file-name)) ; compat
        (cons "line" (and (number? line) (+ 1 line)))))

(define (symbol->object sym)
  (and (symbol? sym)
       (module-defined? (current-module) sym)
       (module-ref (current-module) sym)))

;; Return hash table mapping file-name to list of modules defined in that
;; file. H/t andy wingo.
(define (fill-file->module-mapping! ret)
  (define (record-module m)
    (let ((f (module-filename m)))
      (hash-set! ret f (cons m (hash-ref ret f '())))))
  (define (visit-module m)
    (record-module m)
    (hash-for-each (lambda (k v) (visit-module v))
                   (module-submodules m)))
  (visit-module (resolve-module '() #f))
  ret)

(define file->modules (fill-file->module-mapping! (make-hash-table)))

(define (program-file p)
  (let ((src (program-source p 0)))
    (and (pair? src) (cadr src))))

(define (program-module p)
  (let* ((f (program-file p))
         (mods (or (hash-ref file->modules f)
                   (hash-ref (fill-file->module-mapping! file->modules) f))))
    (and (pair? mods) (not (null? mods)) (car mods))))

(define (module-name? module-name)
  (and (list? module-name)
       (not (null? module-name))
       (every symbol? module-name)))

(define (symbol-module sym . all)
  (and sym
       (catch 'module-name
         (lambda ()
           (apropos-fold (lambda (module name var init)
                           (if (eq? name sym)
                               (throw 'module-name (module-name module))
                               init))
                         #f
                         (regexp-quote (symbol->string sym))
                         (if (or (null? all) (not (car all)))
                             (apropos-fold-accessible (current-module))
                             apropos-fold-all)))
         (lambda (key . args)
           (and (eq? key 'module-name) (car args))))))

(define (module-location name)
  (make-location (module-file-name name) #f))

(define (find-module mod-name)
  (and (module-name? mod-name)
       (resolve-module mod-name #f #:ensure #f)))

(define (module-file-name module-name)
  (and (module-name? module-name)
       (or ((@@ (ice-9 session) module-filename) module-name)
           (module-filename (resolve-module module-name #f)))))

(define (submodules mod)
  (hash-map->list (lambda (k v) v) (module-submodules mod)))

(define (root-modules)
  (submodules (resolve-module '() #f)))

(define (all-modules)
  (define (maybe-name m)
    (and (module-kind m) (format #f "~A" (module-name m))))
  (let* ((guile (resolve-module '(guile)))
         (roots (remove (lambda (m) (eq? m guile)) (root-modules)))
         (children (append-map all-child-modules roots)))
    (cons "(guile)" (filter-map maybe-name children))))

(define* (all-child-modules mod #:optional (seen '()))
  (let ((cs (filter (lambda (m) (not (member m seen))) (submodules mod))))
    (fold (lambda (m all) (append (all-child-modules m all) all))
          (list mod)
          cs)))

(define (symbol-location sym)
  (cond ((symbol-module sym) => module-location)
        (else (let ((obj (symbol->object sym)))
                (or (and (program? obj) (program-location obj))
                    '())))))

(define (program-location p)
  (cond ((not (program? p)) #f)
        ((program-source p 0) =>
         (lambda (s) (make-location (program-file-name p) (source:line s))))
        ((program-file-name p) => (lambda (s) (make-location s #f)))
        (else #f)))

(define (program-file-name p)
  (let* ((mod (program-module p))
         (name (and (module? mod) (module-name mod))))
    (and name (module-file-name name))))

(define (symbol-type o)
  ((compose object-type symbol->object) o))

(define (object-type o) ; pretty-object-type? "Guile procedure" "instance of .." ?
  (cond ((and (procedure? o) (assq-ref (procedure-properties o) 'command-name)) 'command)
        ((macro? o) 'macro)
        ((procedure? o) 'procedure)
        ((is-a? o <class>) (class-name o))
        ((instance? o) (class-name (class-of o)))
        ((variable? o) 'variable)
        (else o)))

(define (xassq o alist)
  (find (compose (cut eq? o <>) cdr) alist))

(define (safe-variable-ref o)
  (catch #t (lambda _ (variable-ref o)) (const #f)))

(define* (find-object o #:optional (alist (all-variables (current-module))))
  (find (compose (cut eq? o <>) safe-variable-ref cdr) alist))

(define (module-variables module)
  (module-map cons module))

(define* (all-variables #:optional (module (current-module)))
  (append-map module-variables ((apropos-fold-accessible module) cons '()))
  ;;(append-map module-variables (apropos-fold-all cons '()))
  )

(define* (object-name o #:optional (module (current-module)))
  (cond ((procedure? o) (procedure-name o))
        (else (let ((entry (find-object o (all-variables module))))
                (and=> entry car)))))
