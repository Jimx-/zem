(define-module (zem core mode)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (define-derived-mode)
  #:declarative? #f)

(define-syntax define-derived-mode
  (lambda (x)
    (syntax-case x ()
      ((_ child parent name body ...)
       (syntax-case (datum->syntax x
                                   (list (string->symbol (string-append "enter-"
                                                                        (symbol->string
                                                                         (syntax->datum #'child))))
                                         (string->symbol (string-append (symbol->string
                                                                         (syntax->datum #'child))
                                                                        "-map"))
                                         (string->symbol (string-append (symbol->string
                                                                         (syntax->datum #'child))
                                                                        "-hook"))))
           ()
         ((enter-mode map hook)
          #`(begin
              (define-public map (make-keymap))

              (define-public child (make <mode> #:mode-name name #:mode-map map))

              (define-public hook (make-hook))

              (define-interactive (enter-mode)
                (set! (local-var 'major-mode) child)
                (set! (local-var 'mode-name) name)
                body ...
                (run-hook hook)))))))))

(define-public auto-mode-alist '())

(define (find-auto-mode name mode-lists)
  (if (null? mode-lists)
      #f
      (match-let
       (((rx . mode-proc) (car mode-lists)))
       (if (regexp-exec rx name)
           mode-proc
           (find-auto-mode name (cdr mode-lists))))))


(define-public (set-auto-mode)
  (let ((mode-proc (find-auto-mode (buffer-name) auto-mode-alist)))
    (mode-proc)))
