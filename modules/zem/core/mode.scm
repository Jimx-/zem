(define-module (zem core mode)
  #:use-module (emacsy emacsy)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (define-derived-mode
             define-minor-mode)
  #:declarative? #f)

(eval-when (expand load eval)
           (define (derived-mode-var-name child)
             (string->symbol (string-append
                              (symbol->string child)
                              "-var")))

           (define (derived-mode-map-name child)
             (string->symbol (string-append
                              (symbol->string child)
                              "-map")))

           (define (derived-mode-hook-name child)
             (string->symbol (string-append
                              (symbol->string child)
                              "-hook")))

           (define (minor-mode-var-name mode)
             (string->symbol (string-append
                              (symbol->string mode)
                              "-var")))

           (define (minor-mode-map-name mode)
             (string->symbol (string-append
                              (symbol->string mode)
                              "-map")))

           (define (minor-mode-hook-name mode)
             (string->symbol (string-append
                              (symbol->string mode)
                              "-hook")))

           (define (minor-mode-hook-on mode)
             (string->symbol (string-append
                              (symbol->string mode)
                              "-on-hook")))

           (define (minor-mode-hook-off mode)
             (string->symbol (string-append
                              (symbol->string mode)
                              "-off-hook"))))

(define-syntax define-derived-mode-map
  (lambda (x)
    (syntax-case x ()
      ((_ id)
       (unless (defined? (syntax->datum #'id))
         #'(define-public id (make-keymap)))))))

(define-syntax define-derived-mode
  (lambda (x)
    (syntax-case x ()
      ((_ child parent name body ...)
       (syntax-case (datum->syntax x
                                   (list (derived-mode-var-name (syntax->datum #'child))
                                         (derived-mode-map-name (syntax->datum #'child))
                                         (derived-mode-hook-name (syntax->datum #'child))))
           ()
         ((mode-var map hook)
          #`(begin
              (define-derived-mode-map map)

              (define-public mode-var (make <mode> #:mode-name name #:mode-map map))

              (define-public hook (make-hook))

              (define-interactive (child)
                body ...

                (when (not (eq? parent fundamental-mode))
                  (parent))

                (set! (local-var 'major-mode) mode-var)
                (set! (local-var 'mode-name) name)

                (unless (keymap-parent map)
                  (set! (keymap-parent map) (current-local-map)))
                (use-local-map map)

                (run-hook hook)))))))))

(define-syntax define-minor-mode
  (lambda (x)
    (syntax-case x ()
      ((_ mode init-value lighter body ...)
       (syntax-case (datum->syntax x
                                   (list (minor-mode-var-name (syntax->datum #'mode))
                                         (minor-mode-map-name (syntax->datum #'mode))
                                         (minor-mode-hook-name (syntax->datum #'mode))
                                         (minor-mode-hook-on (syntax->datum #'mode))
                                         (minor-mode-hook-off (syntax->datum #'mode))
                                         (symbol->string (syntax->datum #'mode))))
           ()
         ((mode-var map hook hook-on hook-off pretty-name)
          #`(begin
              (define-derived-mode-map map)

              (define-public mode-var (make <mode> #:mode-name lighter #:mode-map map))

              (define-public hook (make-hook))
              (define-public hook-on (make-hook))
              (define-public hook-off (make-hook))

              (define-interactive (mode #:optional arg)
                (when (not (local-var-bound? (quote mode)))
                  (set! (local-var (quote mode)) init-value))

                (set! (local-var (quote mode))
                      (cond
                       ((eq? arg 'toggle) (not (local-var (quote mode))))
                       ((and (number? arg) (< arg 1) #f))
                       (else #t)))

                body ...

                (run-hook hook)
                (if (local-var (quote mode))
                    (run-hook hook-on)
                    (run-hook hook-off))

                (set! (buffer-modes (current-buffer)) (cons mode-var (buffer-modes (current-buffer))))

                (agenda-schedule
                 (let ((buffer (current-buffer)))
                   (colambda ()
                             (with-buffer buffer
                               (message "~a ~aabled in current buffer"
                                        pretty-name
                                        (if (local-var (quote mode)) "en" "dis"))))))))))))))

(define-public auto-mode-alist '())

(define (find-auto-mode name mode-lists)
  (if (null? mode-lists)
      #f
      (match-let
       (((rx . mode) (car mode-lists)))
       (if (regexp-exec rx name)
           mode
           (find-auto-mode name (cdr mode-lists))))))


(define-public (set-auto-mode)
  (let ((mode (find-auto-mode (buffer-name) auto-mode-alist)))
    (mode)))
