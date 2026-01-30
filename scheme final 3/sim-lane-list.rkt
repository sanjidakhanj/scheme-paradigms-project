;; done
(module sim-lane-list racket
  (provide lane-list?
           less-crowded
           )

  (require "sim-lane.rkt")

  
  ;; Check if a list consists fo lanes
  (define (lane-list? lst)
    ;; Checks if the given value is a list consisting of lanes
    (and (list? lst)
         (for/list ([lane lst])
           (and (pair? lane)
                (lane? lane)))))
   
    

  ;; Find the least crowded lane in a list of lanes
  ;; This is a sort of "priority queue"
  ;; in O(nlog(n))
  (define (less-crowded sim-lanes )
    ;; Finds and returns the least crowded lane in the given list of lanes
    (cond [(null? sim-lanes) '()] ;If the list is empty, return empty list
          [else
           (let ((sorted-lenes (sort sim-lanes
                                      (lambda (x y)
                                        (< (lane-length x) (lane-length y))))))
                 (car sorted-lenes))]))) ; Return the first (least crowded) lane