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

;;; Commentary:

;; @node Keymap
;; @section Keymap

;; The keymap stores the mapping between key strokes---or events---and
;; commands.  Emacs uses lists for its representation of keymaps. Emacsy
;; instead uses a class that stores entries in a hash table.  Another
;; difference for Emacsy is that it does not convert @verb{|S-C-a|} to a
;; different representation like @verb{|[33554433]|}; it leaves it as a
;; string that is expected to be turned into a canonical representation
;; ``C-A''.
;;
;; Here is an example of the keymap representation in Emacs.
;;
;; @verbatim
;; > (let ((k (make-sparse-keymap)))
;;     (define-key k "a"         'self-insert-command)
;;     (define-key k "<mouse-1>" 'mouse-drag-region)
;;     (define-key k "C-x C-f"   'find-file-at-point)
;;     k)
;;
;; (keymap
;;  (24 keymap
;;      (6 . find-file-at-point))
;;  (mouse-1 . mouse-drag-region)
;;  (97 . self-insert-command))
;; @end verbatim
;;
;; When I initially implemented Emacsy, I replicated Emacs' keymap
;; representation, but I realized it wasn't necessary.  And it seems
;; preferrable to make the representation more transparent to casual
;; inspection.  Also, Emacsy isn't directly responsible for the
;; conversion of keyboard events into [[key-event]]s---that's a lower
;; level detail that the embedding application must handle.  Here is the
;; same keymap as above but in Emacsy.
;;
;; @verbatim
;; > (let ((k (make-keymap)))
;;     (define-key k "a"       'self-insert-command)
;;     (define-key k "mouse-1" 'mouse-drag-region)
;;     (define-key k "C-x C-f" 'find-file-at-point)
;;     k)
;;
;; #<keymap
;;   a self-insert-command
;;   C-x #<keymap
;;         C-f find-file-at-point>
;;   mouse-1 mouse-drag-region>
;; @end verbatim
;;
;; There are a few differences in how the keymap is produced, and the
;; representation looks slightly different too.  For one thing it's not a
;; list.
;;
;; @c \todo[inline]{Justify decisions that deviate from Emacs' design.}
;;
;; Our keymap class has a hashtable of entries and possibly a parent
;; keymap.

;;; Code:

(define-module (emacsy keymap)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy event)
  #:export (<keymap>
            lookup-key
            lookup-key?
            define-key
            keymap?
            make-keymap
            lookup-key-entry?))

;;.
(define-class <keymap> ()
  (entries #:getter entries #:init-thunk (lambda () (make-hash-table)))
  (parent #:accessor parent #:init-keyword #:parent #:init-value #f))

(define-public keymap-parent parent)

;;.
(define* (lookup-key keymap keys #:optional (follow-parent? #t))
  (define* (lookup-key* keymap keys #:optional (follow-parent? #t))
    (if (null? keys)
        keymap
        (let ((entry (hash-ref (entries keymap) (car keys))))
          (if entry
              (if (keymap? entry)
                  ;; Recurse into the next keymap.
                  (1+if-number (lookup-key* entry (cdr keys) follow-parent?))
                  ;; Entry exists.
                  (if (null? (cdr keys))
                      ;; Specifies the right number of keys; return
                      ;; entry.
                      entry
                      ;; Entry exists but there are more keys; return a
                      ;; number.
                      1))
              ;; No entry; try the parent.
              (if (and follow-parent? (parent keymap))
                  (lookup-key* (parent keymap) keys follow-parent?)
                  ;; No entry; no parent.
                  #f)))))
  (lookup-key* keymap (if (string? keys)
                          (kbd keys)
                          keys) follow-parent?))

;;; We propagate the error using a number using the following procedure.
(define (1+if-number x)
  (if (number? x)
      (1+ x)
      x))

;;.
(define* (lookup-key? keymap keyspec #:optional (keymap-ok? #f))
   (let* ((keys (if (string? keyspec)
                    (kbd keyspec)
                    keyspec))
          (result (lookup-key keymap keys)))
     (if keymap-ok?
         (and (not (boolean? result))
              (not (number? result)))
         (and (not (keymap? result))
              (not (boolean? result))
              (not (number? result))))))

(define (make-trampoline module name)
  "Creates a trampoline out of a symbol in a given module, e.g. (lambda () (name))"
  (let ((var (module-variable module name)))
    (unless var
      (scm-error 'no-such-variable "make-trampoline" "Can't make a trampoline for variable named '~a that does not exist in module ~a." (list name module) #f))
    (let ((proc (lambda () ((variable-ref var)))))
      (set-procedure-property! proc 'name
                               (string->symbol (format #f "~a-trampoline" name)))
      proc)))

;;.
(define (define-key keymap key-list-or-string symbol-or-procedure-or-keymap)
  (let* ((keys (if (string? key-list-or-string)
                   (kbd key-list-or-string)
                   key-list-or-string))
         (entry (lookup-key keymap (list (car keys)) #f))
         (procedure-or-keymap
          (if (symbol? symbol-or-procedure-or-keymap)
              (make-trampoline (current-module) symbol-or-procedure-or-keymap)
              symbol-or-procedure-or-keymap)))
    (cond
     ;; Error
     ((number? entry)
      (error "Terminal key binding already found for ~a keys." entry))
     ;; Keymap available for the first key; recurse!
     ((keymap? entry)
      (define-key entry (cdr keys) procedure-or-keymap))
     (else
      (if (= 1 (length keys))
          ;; This is our last key, just add it to our keymap.
          (begin
            (hash-set! (entries keymap) (car keys) procedure-or-keymap)
            keymap)
          ;; We've got a lot of keys left that need to be hung on some
          ;; keymap.
          (define-key keymap (rcdr keys)
            (define-key (make-keymap) (list (rcar keys)) procedure-or-keymap)))))))

;;; Let's define a keymap predicate, which is defined in Emacs as
;;; [[keymapp]] ('p' for predicate).  I am adopting Scheme's question mark
;;; for predicates which seems more natural.

;;.
(define (keymap? obj)
  (is-a? obj <keymap>))

;;.
(define* (make-keymap #:optional (parent #f))
  (make <keymap> #:parent parent))

;;.
(define-method (write (obj <keymap>) port)
  (write-keymap obj port))

;;.
(define* (write-keymap obj port #:optional (keymap-print-prefix 0))
  (display  "#<keymap " port)
  (hash-for-each (lambda (key value)
                   (do ((i 1 (1+ i)))
                       ((> i keymap-print-prefix))
                     (display " " port))
                   (display "\n" port)
                   (display key port)
                   (display " " port)
                   (if (keymap? value)
                       (write-keymap value port (+ 2 keymap-print-prefix))
                       (display value port)))
                 (entries obj))
  (if (parent obj)
      (write-keymap (parent obj) port (+ 2 keymap-print-prefix)))
  (display ">" port))

;;.
(define (lookup-key-entry? result)
  (and (not (boolean? result)) (not (number? result))))
