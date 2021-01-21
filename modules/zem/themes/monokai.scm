(define-module (zem themes monokai)
  #:use-module (zem core faces)
  #:use-module (zem ui style))

(set! background-color #x272822)

(set-face-attribute 'default ':foreground #xf8f8f2)

(define-face font-lock-keyword-face '((t :foreground #xf92672)))
(define-face font-lock-string-face '((t :foreground #xe6db74)))
(define-face font-lock-type-face '((t :foreground #x66d9ef)))
(define-face font-lock-constant-face '((t :foreground #xae81ff)))
(define-face font-lock-function-name-face '((t :foreground #xa6e22e)))
(define-face font-lock-comment-face '((t :foreground #x404040)))

(define-face mode-line '((t :foreground #xf8f8f2 :background #x606060 :inherit fixed-pitch)))

(define-face line-number '((t :foreground #x404040 :background #x202020 :inherit fixed-pitch)))

(define-face cursor '((t :foreground #x808080)))

(define-face scroll-bar '((t :foreground #x404040)))

(define-face window-divider '((t :background #x0f0f0f)))

(define-face highlight '((t :background #x333428)))

(define-face region '((t :background #x414236)))
