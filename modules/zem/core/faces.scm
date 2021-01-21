(define-module (zem core faces)
  #:use-module (zem util plist)
  #:export (define-face
             set-face-attribute
             face-attribute))

(define face-attribute-alist '())

(define-syntax define-face
  (syntax-rules ()
    ((_ face spec)
     (set-face-spec (quote face) spec))))

(define (face-spec-match-display display)
  (eq? display #t))

(define (face-spec-choose spec defaults)
  (if (null? spec)
      defaults
      (let* ((display (caar spec))
             (attrs (cdar spec))
             (defaults (if (eq? display 'default)
                           defaults
                           attrs)))
        (if (face-spec-match-display display)
            attrs
            (face-spec-choose (cdr spec) defaults)))))

(define (set-face-attribute face . args)
  (let* ((old-attrs (or (assq-ref face-attribute-alist face)
                        '()))
         (new-attrs (apply plist-new old-attrs args)))
    (set! face-attribute-alist
          (assoc-set! face-attribute-alist face new-attrs))))

(define (set-face-spec face spec)
  (let ((attrs (face-spec-choose spec '())))
    (apply set-face-attribute face attrs)))

(define* (face-attribute face attribute #:optional (inherit? #f))
  (let ((attrs (assq-ref face-attribute-alist face)))
    (or (plist-get attrs attribute)
        (if inherit?
            (let ((inh-from (face-attribute face ':inherit)))
              (if (eq? inh-from 'unspecified)
                  'unspecified
                  (face-attribute inh-from attribute #t)))
            'unspecified))))

(define-face default '((#t)))

(define-face bold '((#t :weight bold)))

(define-face fixed-pitch '((#t)))
