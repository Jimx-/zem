(define-module (zem themes doom-one)
  #:use-module (zem core faces)
  #:use-module (zem ui style))

(set! background-color #x282c34)

(set-face-attribute 'default ':foreground #xf8f8f2)

(define-face font-lock-keyword-face '((t :foreground #x51afef)))
(define-face font-lock-string-face '((t :foreground #x98be65)))
(define-face font-lock-type-face '((t :foreground #xecbe78)))
(define-face font-lock-constant-face '((t :foreground #xda8548)))
(define-face font-lock-function-name-face '((t :foreground #xa6e22e)))
(define-face font-lock-comment-face '((t :foreground #x404040)))

(define-face mode-line '((t :foreground #xf8f8f2 :background #x606060 :inherit fixed-pitch)))

(define-face line-number '((t :foreground #x404040 :background #x202020 :inherit fixed-pitch)))

(define-face cursor '((t :foreground #x808080)))

(define-face scroll-bar '((t :foreground #x404040)))

(define-face window-divider '((t :background #x0f0f0f)))

(define-face highlight `((t :background #x302e36)))

(define-face region '((t :background #x2257a0)))
