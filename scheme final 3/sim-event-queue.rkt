;; Done
(module sim-event-queue racket
  
(provide
   sim-add-event
   )
  (require "sim-event.rkt")

  ;; This is a sort of "priority queue"
  ;; in O(nlog(n))

  
  ;; Sort the queue based on the specified criteria
  (define (sim-add-event sim-event-queue ev)
   (sort (append sim-event-queue (list ev)) 
          (lambda (event1 event2)
            (let* ((time1 (event-time event1))
                   (time2 (event-time event2))
                   (customer1 (event-user event1))
                   (customer2 (event-user event2)))
              (cond
               ((< time1 time2) #t)
               ((> time1 time2) #f)
               (else (< customer1 customer2))))))))
  
  ; Return the sorted queue
