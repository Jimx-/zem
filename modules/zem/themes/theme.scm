(define-module (zem themes theme))

(define (color-value->rgb color)
  (map (lambda (i) (logand #xff (ash color (- (* i 8))))) '(2 1 0)))

(define (rgb->color-value rgb)
  (apply logior (map (lambda (v i) (ash v (* i 8))) rgb '(2 1 0))))

(define-public (blend-color a b)
  (let ((rgba (color-value->rgb a))
        (rgbb (color-value->rgb b)))
    (rgb->color-value
     (map (lambda (a b) (inexact->exact (floor (/ (+ a b) 2)))) rgba rgbb))))

(define-public (darken-color a f)
  (rgb->color-value
   (map (lambda (v) (inexact->exact (floor (* v (- 1.0 f))))) (color-value->rgb a))))

(define-public (lighten-color a f)
  (rgb->color-value
   (map (lambda (v) (inexact->exact (floor (* v (+ 1.0 f))))) (color-value->rgb a))))
