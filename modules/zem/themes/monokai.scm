(define-module (zem themes monokai)
  #:use-module (zem core faces))

(set-face-attribute 'default ':foreground #xf8f8f2)

(define-face font-lock-keyword-face '((t :foreground #xf92672)))
(define-face font-lock-string-face '((t :foreground #xe6db74)))
(define-face font-lock-type-face '((t :foreground #x66d9ef)))
(define-face font-lock-constant-face '((t :foreground #xae81ff)))
(define-face font-lock-function-name-face '((t :foreground #xa6e22e)))
(define-face font-lock-comment-face '((t :foreground #x404040)))
