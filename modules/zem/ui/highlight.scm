(define-module (zem ui highlight)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight c)
  #:use-module (ice-9 match)
  #:export (get-syntax-lexer
            highlight-code))

(define (get-syntax-lexer name)
  lex-c)

(define token-type->scm
  (match-lambda
   (#("Keyword") 'keyword)
   (#("Keyword" "Type") 'type)
   (#("Name") 'variable-name)
   (#("Name" "Function") 'function-name)
   (#("Punctuation") 'text)
   (#("Operator") 'operator)
   (#("Literal" "String") 'string)
   (#("Literal" "Number" _) 'constant)
   (#("Comment" "Preproc") 'preprocessor)
   (#("Comment" "PreprocFile") 'preprocessor)
   (#("Text") 'text)
   (else 'text)))

(define (token->scm token)
  (cons (token-type->scm (vector-ref token 0)) (vector-ref token 1)))

(define-public (highlight-code code lex)
  (highlight lex code))
