(define-module (zem init)
  #:use-module (zem api font)
  #:use-module (zem core faces)
  #:use-module (zem core font-lock)
  #:use-module (zem themes monokai))

(define default-font (load-font
                      "monospace.ttf"
                      13))

(set-face-attribute 'fixed-pitch ':font default-font)
