(define-module (zem ui highlight)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight c)
  #:export (get-syntax-lexer
            highlight-code))

(define (get-syntax-lexer name)
  lex-c)

(define (highlight-code code lex)
  (highlight lex code))
