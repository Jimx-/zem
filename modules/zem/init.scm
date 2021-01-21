(define-module (zem init)
  #:use-module (zem api font)
  #:use-module (zem core faces)
  #:use-module (zem core font-lock)
  #:use-module (zem themes monokai)
  #:use-module (emacsy emacsy))

;; (set! debug-on-error? #t)

(define default-font (load-font
                      "monospace.ttf"
                      13))

(set-face-attribute 'default ':font default-font)
(set-face-attribute 'fixed-pitch ':font default-font)
