;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright (C) 2019 by Amar Singh<nly@disroot.org>
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

(define-module (emacsy buffer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (emacsy mode)
  #:export (next-buffer
            prev-buffer
            buffer-previous!
            buffer-next!
            other-buffer!
            with-buffer
            save-excursion
            <buffer>
            buffer-stack
            last-buffer
            aux-buffer
            buffer-name
            buffer-name
            set-buffer-name!
            buffer-modified?
            buffer-modified-tick
            current-local-map
            use-local-map
            buffer-list
            current-buffer
            add-buffer!
            remove-buffer!
            set-buffer!
            switch-to-buffer
            local-var
            local-var-bound?
            emacsy-mode-line))

;;; Commentary:

;; @node Buffer
;; @section Buffer

;; @quotation
;; And when you gaze long into an abyss the abyss also gazes into you.
;; @author Beyond Good and Evil, Friedrich Nietzsche
;; @end quotation

;; A buffer in Emacs represents text, including its mode, local
;; variables, etc.  A Emacsy buffer is not necessarily text.  It can be
;; extended to hold whatever the host application is interested in.
;; Emacs' concepts of buffer, window, and mode are directly analogous to
;; the model, view, and controller respectively---the MVC pattern.

;;; Code:

;; @defmac with-buffer @dots{}
;; A convenience macro to work with a given buffer.
;; @end defmac
;;.
(define-syntax with-buffer
  (syntax-rules ()
    ((with-buffer buffer e ...)
     (let ((old-buffer (current-buffer))
           (result *unspecified*))
       (in-out-guard
        (lambda () (set-buffer! buffer))
        (lambda () e ...)
        (lambda () (set-buffer! old-buffer)))))))

;; @defmac save-excursion @dots{}
;; A convenience macro to do some work
;; @end defmac
;;.
(define-syntax save-excursion
  (syntax-rules ()
    ((save-excursion body ...)
     (let ((old-buffer (current-buffer))
           (old-point ((@ (emacsy text) point))))
       (in-out-guard
        (lambda _ #t)
        (lambda _ body ...)
        (lambda _
          (set-buffer! old-buffer)
          ((@ (emacsy text) goto-char) old-point)))))))

;;.
(define-class <buffer> ()
  (name #:init-keyword #:name)
  (file-name #:accessor buffer-file-name #:init-form #f #:init-keyword #:buffer-file-name)
  (keymap #:accessor local-keymap #:init-keyword #:keymap #:init-form (make-keymap))
  (locals #:accessor local-variables #:init-form `((before-change-functions . ,(make-hook 2))
                                                   (after-change-functions . ,(make-hook 3))))
  (buffer-modified? #:accessor buffer-modified? #:init-value #f)
  (buffer-modified-tick #:accessor buffer-modified-tick #:init-value 0)
  (buffer-enter-hook #:accessor buffer-enter-hook #:init-form (make-hook 0))
  (buffer-exit-hook #:accessor buffer-exit-hook #:init-form (make-hook 0))
  (buffer-kill-hook #:accessor buffer-kill-hook #:init-form (make-hook 0))
  (buffer-modes #:accessor buffer-modes #:init-form '() #:init-keyword #:buffer-modes))
(export local-keymap local-variables buffer-file-name buffer-modified? buffer-enter-hook buffer-exit-hook buffer-kill-hook before-buffer-change-hook after-buffer-change-hook after-change-hook before-change-hook buffer-modified-tick buffer-modes)

;;.
(define-variable before-buffer-change-hook (make-hook 1) "This hook is called prior to the buffer being changed with one argument, the buffer.")

;;.
(define-variable after-buffer-change-hook (make-hook 1) "This hook is called after to the buffer has changed with one argument, the buffer.")

;;; The buffer module also keeps track of the live buffers and the current
;;; one.

;;.
(define buffer-stack (make <mru-stack>))

;;.
(define last-buffer #f)

;;.
(define aux-buffer #f)

;; Buffer's have a name, and there is always a current buffer or it's
;; false.  Note that methods do not work as easily with optional
;; arguments.  It seems best to define each method with a different
;; number of arguments as shown below.
(define-method (buffer-name)
  (buffer-name (current-buffer)))

;;.
(define-method (buffer-name (buffer <buffer>))
  (slot-ref buffer 'name))

;;.
(define-method (set-buffer-name! name)
  (set-buffer-name! name (current-buffer)))

;;.
(define-method (set-buffer-name! name (buffer <buffer>))
  (slot-set! buffer 'name name))

;;.
(define-method (buffer-modified?)
  (buffer-modified? (current-buffer)))

;;.
(define-method (buffer-modified-tick)
  (buffer-modified-tick (current-buffer)))

;;.
(define-method (write (obj <buffer>) port)
  (write (string-concatenate (list "#<buffer '" (buffer-name obj) "'>")) port))

;; @c @node
;; @subsection Emacs Compatibility

;;.
(define (current-local-map)
  (local-keymap (current-buffer)))

;;.
(define (use-local-map keymap)
  (set! (local-keymap (current-buffer)) keymap))

;;.
(define (buffer-list)
  (mru-list buffer-stack))

;;.
(define (current-buffer)
  ;; Perhaps instead of returning #f for no buffer there should be an
  ;; immutable void-buffer class.
  (or aux-buffer
      (mru-ref buffer-stack)))

;;.
(define (add-buffer! buffer)
  (mru-add! buffer-stack buffer))

;;.
(define (remove-buffer! buffer)
  (mru-remove! buffer-stack buffer))

(define* (buffer-previous! #:optional (incr 1))
  (mru-next! buffer-stack incr))

(define* (buffer-next! #:optional (incr 1))
  (buffer-previous! (- incr)))

;;.
(define-interactive (next-buffer #:optional (incr 1))
  (buffer-next! incr)
  (switch-to-buffer (mru-ref buffer-stack)))

;;.
(define-interactive (prev-buffer #:optional (incr 1))
  (next-buffer (- incr)))

;;.
(define (set-buffer! buffer)
  ;;(emacsy-log-debug "set-buffer! to ~a" buffer)
  (if (mru-set! buffer-stack buffer)
      (set! aux-buffer #f)
      (set! aux-buffer buffer)))

;; This is scary, we will override it when we have <text-buffer>.
;;.
(define-interactive (kill-buffer #:optional (buffer (current-buffer)))
  (remove-buffer! buffer))

;;.
(define* (other-buffer! #:optional (incr 1))
  (buffer-previous! incr)
  (when (mru-contains? buffer-stack (current-buffer))
    (mru-recall! buffer-stack (current-buffer)))
  (current-buffer))

(define-interactive (other-buffer #:optional (count 1))
  (buffer-previous! count)
  (switch-to-buffer (mru-ref buffer-stack)) #t)


;;; This is our primitive procedure for switching buffers.  It does not
;;; handle any user interaction.
(define* (primitive-switch-to-buffer buffer #:optional recall?)
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer))
  (run-hook (buffer-exit-hook (current-buffer)))
  (set! last-buffer (current-buffer))
  (if (and recall?
           (mru-contains? buffer-stack buffer))
      (begin
        (emacsy-log-debug "Recall buffer ~a" buffer)
        (when recall? (mru-recall! buffer-stack buffer))
        (set! aux-buffer #f))
      (begin
        (emacsy-log-debug "Set buffer to ~a" buffer)
        (set-buffer! buffer)))
  (emacsy-log-debug "Running enter hook for ~a" (current-buffer))
  (run-hook (buffer-enter-hook (current-buffer)))
  (current-buffer))

;;.
(define switch-to-buffer primitive-switch-to-buffer)

;;.
(define (local-var-ref symbol)
  (let ((result (assq symbol (local-variables (current-buffer)))))
    (if (pair? result)
     (cdr result)
     ;(variable-ref (make-undefined-variable))
     (throw 'no-such-local-variable symbol))))

;; If buffers were in their own modules I could dynamically add variables
;; to their namespace.  Interesting idea.

;;.
(define (local-var-set! symbol value)
  (slot-set! (current-buffer)
             'locals
             (assq-set! (local-variables (current-buffer)) symbol value)))

;;.
(define local-var
               (make-procedure-with-setter local-var-ref local-var-set!))

;;.
(define (local-var-bound? symbol)
  (assq symbol (local-variables (current-buffer))))

;;.
;; method
(define-method (emacsy-mode-line)
  (emacsy-mode-line (current-buffer)))

(define-method (emacsy-mode-line (buffer <buffer>))
  (format #f "-:~a- ~a    (~a~a~{~a~^ ~})"
          (if (buffer-modified? buffer) "**" "--")
          (buffer-name buffer)
          (if (local-var-bound? 'mode-name) (local-var 'mode-name) "")
          (if (null? (buffer-modes buffer)) "" " ")
          (map mode-name (buffer-modes buffer))))
