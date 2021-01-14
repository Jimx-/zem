(define-module (zem core commands)
  #:use-module (emacsy emacsy)
  #:use-module (zem core buffer)
  #:use-module (zem core mode)
  #:use-module (zem progmodes cc-mode)
  #:use-module (statprof))

(define-interactive (profiler-start)
  (statprof-start))

(define-interactive (profiler-end)
  (statprof-stop))

(define-interactive (profiler-report)
  (statprof-display))
