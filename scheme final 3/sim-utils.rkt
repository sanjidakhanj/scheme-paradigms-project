(module sim-utils racket
  (provide sim-secs2hour
           sim-log!)

  (require "sim-event.rkt")
  (require "sim-lane.rkt")
  
  (define (sim-secs2hour seconds)
  (let* ((hours (quotient seconds 3600))
         (remaining-seconds (- seconds (* hours 3600)))
         (minutes (quotient remaining-seconds 60))
         (seconds (modulo remaining-seconds 60)))
    (list hours minutes seconds)))

  ; IO()
  (define (sim-log! time event lanes)  ;; can omit time?
    (let*
        ([aux (sim-secs2hour (event-time event))]
         [hours (first aux)]
         [minutes (second aux)]
         [secs (third aux)]
         [ev (event->string event)]
         [user (event-user event)]
         [lanes-str (apply
                     string-append 
                     (map
                      (lambda (x)(format "~n\t~a" (lane->string x)))
                      lanes))]
         )
      (printf "~a:~a:~a|User: ~a|~a~a~n"
              (~r hours #:min-width 2 #:pad-string "0")
              (~r minutes #:min-width 2 #:pad-string "0") 
              (~r secs #:min-width 2 #:pad-string "0")
              (~r user #:min-width 2 )
              ev
              lanes-str
              )
             
      ))
  

  )
      
         
