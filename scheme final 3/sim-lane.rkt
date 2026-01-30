;;done
(module sim-lane racket
  (provide lane
           lane?
           lane-id
           lane-user
           lane-busy?
           lane->string
           lane-length
           lane-pop
           lane-queue
           ; These routines fire events
           lane-append!
           lane-pop!
           lane-bussy-set!
           )

  ;; Import required module
  (require racket/match)
  (define sim-fire-event! null) ; hack to passed procedure


  
  ;; Degine lane data sturcture 
  (define (lane id sym)
    (set! sim-fire-event! sym) ;; dirty hack
    (cons id (cons null null)))

  ;; Get length of a lane queue
  (define (lane-length ln)
    ;; Contract: lane-length: lane -> integer
    ;; Returns the length of the queue in the given lane 
     
     (car(cdr(cdr ln)))
  )


  ;; Check if a value represents a lane
  (define (lane? val)
    (and (pair? val)
         (pair? (cdr val))
         (null? (cddr val)))
    )

  ;;Check if a lane is busy
  (define (lane-busy? lane)
    (not (null? (cadr lane))))

  ;; Set the user of a lane
  (define (lane-bussy-set ln user)
    (match ln
      [(cons id (cons _ q))
       (cons id (cons user q))]))


  ;; Get the ID of a lane
  (define (lane-id lane)
    (car lane))

  ;; Get the user of a lane
  (define (lane-user lane)
    (cadr lane))

  ;; Get the queue of a lane
  (define (lane-queue lane)
    (cddr lane))

  ;; Converts lane data structure to string 
  (define (lane->string ln)
    (let
        ([lane-qu-str (apply
                       string-append 
                       (map
                        (lambda (x)(format "~a:" x))
                        (lane-queue ln)))]
          )
      (format "L(~a)[~a]|<=~a"
              (lane-id ln)
              (~a
               (if (lane-busy? ln) (lane-user ln) "None" ) #:min-width 4)
              lane-qu-str)))


  ;; Append a user to a lane
  (define (lane-append ln user)
    (match ln
      [(cons id (cons u q))
       (cons id (cons u (append q (list user))))]))

  ;; Remove a user from a lane
  (define (lane-pop ln)
    (match ln
      [(cons id (cons u (cons q qs)))
       (values q
               (cons id (cons u qs)))]))


  ; IO() fires an event
  ; Sets the user of a lane and fire a event
  (define (lane-bussy-set! ln user)
    (let
        ([new-lane (lane-bussy-set ln user)])
      (begin       
        (if (and
             (> (lane-length new-lane) 0)  
             (not (lane-busy? new-lane))) ;; till is free
            (begin
              (sim-fire-event! 2(first (lane-queue new-lane)) (lane-id new-lane))) ;; CHECKOUT
            (void))
        )          
      new-lane))

  ;; IO () fires an event
  ;; Append a user to a lane an dfire an event 
  (define (lane-append! ln user)
    (let
        ([new-lane (lane-append ln user)])
      (begin
        (if (and
             (= (lane-length new-lane) 1)  ;; i'm the only 
             (not (lane-busy? new-lane))) ;; till is free
            (sim-fire-event! 2 user (lane-id new-lane)) ;; CHECKOUT
            (void))
        )          
      new-lane))
         

  ;; IO() fires an event
  ;; Remove a user from a lane and fire an event
  (define (lane-pop! ln )
    (let*-values
        ([(user new-lane) (lane-pop ln )]
         [ (new-la2) (lane-bussy-set new-lane user)]
         )
      (begin
        (sim-fire-event! 3 user (lane-id new-la2)) ;; CHECKOUT
        new-la2)))



  
    
  )
