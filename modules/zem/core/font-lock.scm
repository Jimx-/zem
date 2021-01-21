(define-module (zem core font-lock)
  #:use-module (zem core faces)
  #:use-module (zem core mode)
  #:export (font-lock-mode))

(define-public font-lock-comment-face 'font-lock-comment-face)
(define-public font-lock-string-face 'font-lock-string-face)
(define-public font-lock-builtin-face 'font-lock-builtin-face)
(define-public font-lock-keyword-face 'font-lock-keyword-face)
(define-public font-lock-function-name-face 'font-lock-function-name-face)
(define-public font-lock-variable-name-face 'font-lock-variable-name-face)
(define-public font-lock-type-face 'font-lock-type-face)
(define-public font-lock-constant-face 'font-lock-constant-face)
(define-public font-lock-preprocessor-face 'font-lock-preprocessor-face)

(define-face font-lock-comment-face '((t :inherit fixed-pitch)))

(define-face font-lock-string-face '((t :inherit fixed-pitch)))

(define-face font-lock-keyword-face '((t :inherit fixed-pitch)))

(define-face font-lock-function-name-face '((t :inherit fixed-pitch)))

(define-face font-lock-variable-name-face '((t :inherit fixed-pitch)))

(define-face font-lock-type-face '((t :inherit fixed-pitch)))

(define-face font-lock-constant-face '((t :inherit fixed-pitch)))

(define-face font-lock-preprocessor-face '((t :inherit fixed-pitch)))

(define-minor-mode font-lock-mode #f "Font-Lock")
