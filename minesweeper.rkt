#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)


(define g_w 30)
(define g_h 30)
(define rows 0)
(define cols 0)
(define grids `()) (define grid_images `()) (define grid_posns `()) (define game? #t)  
(define fg_color "white")
(define mine_count 0)
(define n_list `())
(define grid_values `()) (define rev_list `()) (define shown_list `())
(define scn_w 900)
(define scn_h 480)


(define main-frame (new frame%
                   [label "Minesweeper"]
                   [min-width 500]
                   [min-height 500]
                   [stretchable-width #f]
                   [stretchable-height #f]
                   ))
(define rowpanel (new vertical-panel%
                      [parent main-frame]
                      [alignment '(center bottom )]
                      [vert-margin 100]	    	             
                      [horiz-margin 100]
                      [spacing 0]))
(define new-game (new button%
                     [label "level-easy"]
                     [parent rowpanel]
                     [min-width 200]
                     [min-height 50] (callback (lambda (button event)

                                               (set! game? #t)
                                                 (begin (set! rows 9) (set! cols 9) (set! mine_count 80)
                                                        (set! scn_w 270) (set! scn_h 270)
                                                   (define (unopen-counter grid_images)
  (cond [(null? grid_images) 0]
        [(equal? (car grid_images) grid_image ) (+ 1 (unopen-counter (cdr grid_images)))]
        [else (unopen-counter (cdr grid_images))]))


(define scn (rectangle scn_w scn_h "solid" "blue")  )
(define grid_image (overlay
                    (rectangle g_w g_h "outline" fg_color)
                    (rectangle g_w g_h 209 "green")
                    (rectangle g_w g_h "solid" "red")
                    ))
(define m_image
  (overlay
   (circle (/ g_w 8) "solid" "black")
   (let loop ((i 0))
     (if (< i 8) (place-image (circle (/ g_w 30) 255 "red") (+ (/ g_w 5) (* (/ g_w 6) (cos (* i (/ pi 4))))) (+ (/ g_h 5) (* (/ g_h 6) (sin (* i (/ pi 4))))) (loop (add1 i))) (rectangle (/ (* g_w 2) 5) (/ (* g_h 2) 5) 0 "black"))
     )
   (rectangle g_w g_h "solid" "orange"))
  )

(define (grid_image_value v)
  (overlay
   (text/font (if (number? v) (if (= v 0) "" (number->string v)) "") (round (/ g_w 2.5)) fg_color "Gill Sans" 'swiss 'normal 'bold #f)
   (rectangle g_w g_h "outline" fg_color)
   (rectangle g_w g_h "solid" (color 0 0 0 180))
   (rectangle g_w g_h 255 "white"))
  )

(define (index i j)
  (+ i (* j cols))
  )

(define (sum l)
  (foldl + 0 l)
  )

(define (init_grids)
  (set! grids (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                (if (< i cols) (let ((index (+ i (* j cols))))
                                                  (append (list (list (cond
                                                                        ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
                                                                        (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
                                                                                  (grid_image_value (list-ref grid_values index))
                                                                                  grid_image
                                                                                  ))
                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
                                                  ) (loop_j (add1 j)))
                                 ) empty)
                ))
  )
;  (for ([j rows])
;                (for ([i cols])
;                  (let* ((index (+ i (* j cols))))
;                    (set! grids (append grids (list (list (cond
;                                          ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
;                                          (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
;                                                    (grid_image_value (list-ref grid_values index))
;                                                    grid_image
;                                                    ))
;                                          ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2))))))))))
;                  

  
(define (init_grid_images)
  (set! grid_images (let loop ((i 0))
                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
                      ))
  (if (= mine_count (unopen-counter grid_images)) (set! game? 1) void) )

  
  
  ;(for [(i (length grids))]
;    (set! grid_images (append grid_images (list (list-ref (list-ref grids i) 0)))))
  
(define (init_grid_posns)
  (set! grid_posns (let loop ((i 0))
                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
                     ))
  )

                
(define (init_game i j )
  (begin
    (set!-values (rev_list shown_list game?) (values empty empty #t))
    (set! grid_values (make-list (* rows cols) 0))
    ;(define num1 (index i j))
;          (let loop ((i 0))
;            (if (< i (* cols rows)) (append (list 0) (loop (add1 i))) empty)
;            ))

    (let loop ((i 0))
      (let ((num (random (* cols rows))))
        (if (< i mine_count)
            (if (or (= num (index i j)) (equal? (list-ref grid_values num) "m"))
                                 (loop i)
                                 (begin (set! grid_values (list-set grid_values num "m"))
                                                                                         (loop (add1 i))))
            void)
        )
      )
    


    (set! n_list
          (let loop ((i 0))
            (if (< i (* cols rows)) (append (list (list 0 0 0 0 0 0 0 0)) (loop (add1 i))) empty)
            ))

    (let loop_j ((j 0))
      (if (< j rows) (let loop_i ((i 0))
                       (if (< i cols) (begin
                                        (if (not (equal? (list-ref grid_values (index i j)) "m"))
                                            (begin
                                              (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 0 (if (equal? (list-ref grid_values (index (- i 1) (- j 1))) "m") 1 0)))) void)
                                              (if                              (>= (- j 1) 0)           (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 1 (if (equal? (list-ref grid_values (index    i    (- j 1))) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 2 (if (equal? (list-ref grid_values (index (+ i 1) (- j 1))) "m") 1 0)))) void)
                                              (if      (<= (+ i 1) (- cols 1))                          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 3 (if (equal? (list-ref grid_values (index (+ i 1)    j   )) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 4 (if (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if                              (<= (+ j 1) (- rows 1))  (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 5 (if (equal? (list-ref grid_values (index    i    (+ j 1))) "m") 1 0)))) void)
                                              (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 6 (if (equal? (list-ref grid_values (index (- i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if      (>= (- i 1) 0)                                   (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 7 (if (equal? (list-ref grid_values (index (- i 1)    j   )) "m") 1 0)))) void)
                                              (set! grid_values (list-set grid_values (index i j) (sum (list-ref n_list (index i j)))))) void)
                                        (loop_i (add1 i))
                                        ) (loop_j (add1 j)))
                       ) void)
      )
    (init_grids)
    (init_grid_images)
    (init_grid_posns)
    
    ))
(define (find-index x y)
  (list (floor (/ x g_w)) (floor (/ y g_h)))
  )






(define (check-4-sides i j)
  (begin
    (if (not (member (list i j) rev_list)) (set! rev_list (append (list (list i j)) rev_list)) void)
    (if (>= (- i 1) 0) (if (and (equal? (list-ref grid_values (index (- i 1) j)) 0) (not (member (list (- i 1) j) rev_list)))
                           (check-4-sides (- i 1) j)
                           void) void)
    (if (>= (- j 1) 0) (if (and (equal? (list-ref grid_values (index i (- j 1))) 0) (not (member (list i (- j 1)) rev_list)))
                           (check-4-sides i (- j 1))
                           void) void)
    (if (<= (+ i 1) (- cols 1)) (if (and (equal? (list-ref grid_values (index (+ i 1) j)) 0) (not (member (list (+ i 1) j) rev_list)))
                                    (check-4-sides (+ i 1) j)
                                    void) void)
    (if (<= (+ j 1) (- rows 1)) (if (and (equal? (list-ref grid_values (index i (+ j 1))) 0) (not (member (list i (+ j 1)) rev_list)))
                                    (check-4-sides i ( + j 1))
                                    void) void)
    
    )
  )

(define (reveal i j)  
  (let ((v (list-ref grid_values (index i j))))    
    
      (if (eq? v "m")    (set! game? #f)
        
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))
 (define (co-reveal i j )
   (let ((v (list-ref grid_values (index i j))))      
      (if (eq? v "m")    void      
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))

(define (show_n)
  (let loop ((n 0))
    (if (< n (length rev_list))
        (let ((i (list-ref (list-ref rev_list n) 0))
              (j (list-ref (list-ref rev_list n) 1)))
          (begin
            (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (- i 1) (- j 1))) 0)) (not (member (list (- i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (- j 1))) shown_list)) void) void)
            (if                              (>= (- j 1) 0)           (if (and (not (equal? (list-ref grid_values (index    i    (- j 1))) 0)) (not (member (list    i    (- j 1)) shown_list))) (set! shown_list (append (list (list    i    (- j 1))) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (+ i 1) (- j 1))) 0)) (not (member (list (+ i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (- j 1))) shown_list)) void) void)
            (if      (<= (+ i 1) (- cols 1))                          (if (and (not (equal? (list-ref grid_values (index (+ i 1)    j   )) 0)) (not (member (list (+ i 1)    j   ) shown_list))) (set! shown_list (append (list (list (+ i 1)    j   )) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) 0)) (not (member (list (+ i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (+ j 1))) shown_list)) void) void)
            (if                              (<= (+ j 1) (- rows 1))  (if (and (not (equal? (list-ref grid_values (index    i    (+ j 1))) 0)) (not (member (list    i    (+ j 1)) shown_list))) (set! shown_list (append (list (list    i    (+ j 1))) shown_list)) void) void)
            (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (- i 1) (+ j 1))) 0)) (not (member (list (- i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (+ j 1))) shown_list)) void) void)
            (if      (>= (- i 1) 0)                                   (if (and (not (equal? (list-ref grid_values (index (- i 1)    j   )) 0)) (not (member (list (- i 1)    j   ) shown_list))) (set! shown_list (append (list (list (- i 1)    j   )) shown_list)) void) void)
            (loop (add1 n))
            ))
        (void))
    )
  
  )


(define (render ws)
  (place-images
   (append
    (list
     
     )
    grid_images)
   (append
    (list

     )
    grid_posns)
   scn)
  )

(define (key-handler ws ke)
  (if (and (key=? "r" ke)  game?) (init_game  ) void))
  

(define (release-handler ws ke)
  ws
  )
 (define c 1)
 
(define (mouse-handler ws x y me)
  (let ((i (list-ref (find-index x y) 0))
        (j (list-ref (find-index x y) 1)))
    (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) (= c 1))
        (begin
         (init_game i j )
          (reveal i j)
          (co-reveal (+ 1 i) j) (co-reveal (- i 1) j) (co-reveal i (+ j 1)) (co-reveal i (- j 1)) 
          (show_n)
          (init_grids)
          (init_grid_images)
          (set! c 0)
          
          ) void)
     (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) )
        (begin         
          (reveal i j)
          (show_n)
          (init_grids)
          (init_grid_images)
          (if (equal? 1 game?) (begin (set! game? #f) (init_grids)  (init_grid_images)  (play-sound "C:\\Users\\Abhijeet\\Downloads\\applause-01.wav" #f) (set! game? 1)) void)
          (if (not game?) (begin (init_grids) (init_grid_images) (play-sound "\\home\\devansh\\Desktop\explosion-01.wav" #f)) void)
          (set! c 0)
          
        ) void)
    )
  )

(define (step ws)
  ws
  )   
                                                 (big-bang 0
                                                 (on-tick step)
                                                 (on-mouse mouse-handler)
                                                 (on-draw render)
                                                 (on-release release-handler)
                                                 (on-key key-handler)))))))
(define scores (new button%
                     [label "level-intermidiate"]
                     [parent rowpanel]
                     [min-width 200]
                     [min-height 50] (callback (lambda (button event) (begin (set! rows 16) (set! cols 16) (set! mine_count 40))
                      (set! game? #t) (set! scn_w 480)(define (unopen-counter grid_images)
  (cond [(null? grid_images) 0]
        [(equal? (car grid_images) grid_image ) (+ 1 (unopen-counter (cdr grid_images)))]
        [else (unopen-counter (cdr grid_images))]))


(define scn (rectangle scn_w scn_h "solid" "blue")  )
(define grid_image (overlay
                    (rectangle g_w g_h "outline" fg_color)
                    (rectangle g_w g_h 209 "green")
                    (rectangle g_w g_h "solid" "red")
                    ))
(define m_image
  (overlay
   (circle (/ g_w 8) "solid" "black")
   (let loop ((i 0))
     (if (< i 8) (place-image (circle (/ g_w 30) 255 "red") (+ (/ g_w 5) (* (/ g_w 6) (cos (* i (/ pi 4))))) (+ (/ g_h 5) (* (/ g_h 6) (sin (* i (/ pi 4))))) (loop (add1 i))) (rectangle (/ (* g_w 2) 5) (/ (* g_h 2) 5) 0 "black"))
     )
   (rectangle g_w g_h "solid" "orange"))
  )

(define (grid_image_value v)
  (overlay
   (text/font (if (number? v) (if (= v 0) "" (number->string v)) "") (round (/ g_w 2.5)) fg_color "Gill Sans" 'swiss 'normal 'bold #f)
   (rectangle g_w g_h "outline" fg_color)
   (rectangle g_w g_h "solid" (color 0 0 0 180))
   (rectangle g_w g_h 255 "white"))
  )

(define (index i j)
  (+ i (* j cols))
  )

(define (sum l)
  (foldl + 0 l)
  )

(define (init_grids)
  (set! grids (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                (if (< i cols) (let ((index (+ i (* j cols))))
                                                  (append (list (list (cond
                                                                        ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
                                                                        (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
                                                                                  (grid_image_value (list-ref grid_values index))
                                                                                  grid_image
                                                                                  ))
                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
                                                  ) (loop_j (add1 j)))
                                 ) empty)
                ))
  )
;  (for ([j rows])
;                (for ([i cols])
;                  (let* ((index (+ i (* j cols))))
;                    (set! grids (append grids (list (list (cond
;                                          ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
;                                          (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
;                                                    (grid_image_value (list-ref grid_values index))
;                                                    grid_image
;                                                    ))
;                                          ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2))))))))))
;                  

  
(define (init_grid_images)
  (set! grid_images (let loop ((i 0))
                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
                      ))
  (if (= mine_count (unopen-counter grid_images)) (set! game? 1) void) )

  
  
  ;(for [(i (length grids))]
;    (set! grid_images (append grid_images (list (list-ref (list-ref grids i) 0)))))
  
(define (init_grid_posns)
  (set! grid_posns (let loop ((i 0))
                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
                     ))
  )

                
(define (init_game i j )
  (begin
    (set!-values (rev_list shown_list game?) (values empty empty #t))
    (set! grid_values (make-list (* rows cols) 0))
    ;(define num1 (index i j))
;          (let loop ((i 0))
;            (if (< i (* cols rows)) (append (list 0) (loop (add1 i))) empty)
;            ))

    (let loop ((i 0))
      (let ((num (random (* cols rows))))
        (if (< i mine_count)
            (if (or (= num (index i j)) (equal? (list-ref grid_values num) "m"))
                                 (loop i)
                                 (begin (set! grid_values (list-set grid_values num "m"))
                                                                                         (loop (add1 i))))
            void)
        )
      )
    


    (set! n_list
          (let loop ((i 0))
            (if (< i (* cols rows)) (append (list (list 0 0 0 0 0 0 0 0)) (loop (add1 i))) empty)
            ))

    (let loop_j ((j 0))
      (if (< j rows) (let loop_i ((i 0))
                       (if (< i cols) (begin
                                        (if (not (equal? (list-ref grid_values (index i j)) "m"))
                                            (begin
                                              (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 0 (if (equal? (list-ref grid_values (index (- i 1) (- j 1))) "m") 1 0)))) void)
                                              (if                              (>= (- j 1) 0)           (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 1 (if (equal? (list-ref grid_values (index    i    (- j 1))) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 2 (if (equal? (list-ref grid_values (index (+ i 1) (- j 1))) "m") 1 0)))) void)
                                              (if      (<= (+ i 1) (- cols 1))                          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 3 (if (equal? (list-ref grid_values (index (+ i 1)    j   )) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 4 (if (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if                              (<= (+ j 1) (- rows 1))  (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 5 (if (equal? (list-ref grid_values (index    i    (+ j 1))) "m") 1 0)))) void)
                                              (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 6 (if (equal? (list-ref grid_values (index (- i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if      (>= (- i 1) 0)                                   (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 7 (if (equal? (list-ref grid_values (index (- i 1)    j   )) "m") 1 0)))) void)
                                              (set! grid_values (list-set grid_values (index i j) (sum (list-ref n_list (index i j)))))) void)
                                        (loop_i (add1 i))
                                        ) (loop_j (add1 j)))
                       ) void)
      )
    (init_grids)
    (init_grid_images)
    (init_grid_posns)
    
    ))
(define (find-index x y)
  (list (floor (/ x g_w)) (floor (/ y g_h)))
  )






(define (check-4-sides i j)
  (begin
    (if (not (member (list i j) rev_list)) (set! rev_list (append (list (list i j)) rev_list)) void)
    (if (>= (- i 1) 0) (if (and (equal? (list-ref grid_values (index (- i 1) j)) 0) (not (member (list (- i 1) j) rev_list)))
                           (check-4-sides (- i 1) j)
                           void) void)
    (if (>= (- j 1) 0) (if (and (equal? (list-ref grid_values (index i (- j 1))) 0) (not (member (list i (- j 1)) rev_list)))
                           (check-4-sides i (- j 1))
                           void) void)
    (if (<= (+ i 1) (- cols 1)) (if (and (equal? (list-ref grid_values (index (+ i 1) j)) 0) (not (member (list (+ i 1) j) rev_list)))
                                    (check-4-sides (+ i 1) j)
                                    void) void)
    (if (<= (+ j 1) (- rows 1)) (if (and (equal? (list-ref grid_values (index i (+ j 1))) 0) (not (member (list i (+ j 1)) rev_list)))
                                    (check-4-sides i ( + j 1))
                                    void) void)
    
    )
  )

(define (reveal i j)  
  (let ((v (list-ref grid_values (index i j))))    
    
      (if (eq? v "m")    (set! game? #f)
        
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))
 (define (co-reveal i j )
   (let ((v (list-ref grid_values (index i j))))      
      (if (eq? v "m")    void      
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))

(define (show_n)
  (let loop ((n 0))
    (if (< n (length rev_list))
        (let ((i (list-ref (list-ref rev_list n) 0))
              (j (list-ref (list-ref rev_list n) 1)))
          (begin
            (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (- i 1) (- j 1))) 0)) (not (member (list (- i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (- j 1))) shown_list)) void) void)
            (if                              (>= (- j 1) 0)           (if (and (not (equal? (list-ref grid_values (index    i    (- j 1))) 0)) (not (member (list    i    (- j 1)) shown_list))) (set! shown_list (append (list (list    i    (- j 1))) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (+ i 1) (- j 1))) 0)) (not (member (list (+ i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (- j 1))) shown_list)) void) void)
            (if      (<= (+ i 1) (- cols 1))                          (if (and (not (equal? (list-ref grid_values (index (+ i 1)    j   )) 0)) (not (member (list (+ i 1)    j   ) shown_list))) (set! shown_list (append (list (list (+ i 1)    j   )) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) 0)) (not (member (list (+ i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (+ j 1))) shown_list)) void) void)
            (if                              (<= (+ j 1) (- rows 1))  (if (and (not (equal? (list-ref grid_values (index    i    (+ j 1))) 0)) (not (member (list    i    (+ j 1)) shown_list))) (set! shown_list (append (list (list    i    (+ j 1))) shown_list)) void) void)
            (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (- i 1) (+ j 1))) 0)) (not (member (list (- i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (+ j 1))) shown_list)) void) void)
            (if      (>= (- i 1) 0)                                   (if (and (not (equal? (list-ref grid_values (index (- i 1)    j   )) 0)) (not (member (list (- i 1)    j   ) shown_list))) (set! shown_list (append (list (list (- i 1)    j   )) shown_list)) void) void)
            (loop (add1 n))
            ))
        (void))
    )
  
  )


(define (render ws)
  (place-images
   (append
    (list
     
     )
    grid_images)
   (append
    (list

     )
    grid_posns)
   scn)
  )

(define (key-handler ws ke)
  (if (and (key=? "r" ke)  game?) (init_game  ) void))
  

(define (release-handler ws ke)
  ws
  )
 (define c 1)
 
(define (mouse-handler ws x y me)
  (let ((i (list-ref (find-index x y) 0))
        (j (list-ref (find-index x y) 1)))
    (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) (= c 1))
        (begin
         (init_game i j )
          (reveal i j)
          (co-reveal (+ 1 i) j) (co-reveal (- i 1) j) (co-reveal i (+ j 1)) (co-reveal i (- j 1)) 
          (show_n)
          (init_grids)
          (init_grid_images)
          (set! c 0)
          
          ) void)
     (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) )
        (begin         
          (reveal i j)
          (show_n)
          (init_grids)
          (init_grid_images)
          (if (equal? 1 game?) (begin (set! game? #f) (init_grids)  (init_grid_images)  (play-sound "C:\\Users\\Abhijeet\\Downloads\\applause-01.wav" #f) (set! game? 1)) void)
          (if (not game?) (begin (init_grids) (init_grid_images) (play-sound "C:\\Users\\Abhijeet\\Downloads\\explosion-01.wav" #f)) void)
          (set! c 0)
          
        ) void)
    )
  )

(define (step ws)
  ws
  )
                                                 (big-bang 0
                                                 (on-tick step)
                                                 (on-mouse mouse-handler)
                                                 (on-draw render)
                                                 (on-release release-handler)
                                                 (on-key key-handler))))))
(define level (new button%
                     [label "level-hard"]
                     [parent rowpanel]
                     [min-width 200]
                     [min-height 50] (callback (lambda (button event)

                                                 (set! game? #t)
                                                  (set! rows 16) (set! cols 30) (set! mine_count 99)(define (unopen-counter grid_images)
  (cond [(null? grid_images) 0]
        [(equal? (car grid_images) grid_image ) (+ 1 (unopen-counter (cdr grid_images)))]
        [else (unopen-counter (cdr grid_images))]))


(define scn (rectangle scn_w scn_h "solid" "blue")  )
(define grid_image (overlay
                    (rectangle g_w g_h "outline" fg_color)
                    (rectangle g_w g_h 209 "green")
                    (rectangle g_w g_h "solid" "red")
                    ))
(define m_image
  (overlay
   (circle (/ g_w 8) "solid" "black")
   (let loop ((i 0))
     (if (< i 8) (place-image (circle (/ g_w 30) 255 "red") (+ (/ g_w 5) (* (/ g_w 6) (cos (* i (/ pi 4))))) (+ (/ g_h 5) (* (/ g_h 6) (sin (* i (/ pi 4))))) (loop (add1 i))) (rectangle (/ (* g_w 2) 5) (/ (* g_h 2) 5) 0 "black"))
     )
   (rectangle g_w g_h "solid" "orange"))
  )

(define (grid_image_value v)
  (overlay
   (text/font (if (number? v) (if (= v 0) "" (number->string v)) "") (round (/ g_w 2.5)) fg_color "Gill Sans" 'swiss 'normal 'bold #f)
   (rectangle g_w g_h "outline" fg_color)
   (rectangle g_w g_h "solid" (color 0 0 0 180))
   (rectangle g_w g_h 255 "white"))
  )

(define (index i j)
  (+ i (* j cols))
  )

(define (sum l)
  (foldl + 0 l)
  )

(define (init_grids)
  (set! grids (let loop_j ((j 0))
                (if (< j rows) (let loop_i ((i 0))
                                (if (< i cols) (let ((index (+ i (* j cols))))
                                                  (append (list (list (cond
                                                                        ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
                                                                        (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
                                                                                  (grid_image_value (list-ref grid_values index))
                                                                                  grid_image
                                                                                  ))
                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
                                                  ) (loop_j (add1 j)))
                                 ) empty)
                ))
  )
;  (for ([j rows])
;                (for ([i cols])
;                  (let* ((index (+ i (* j cols))))
;                    (set! grids (append grids (list (list (cond
;                                          ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
;                                          (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
;                                                    (grid_image_value (list-ref grid_values index))
;                                                    grid_image
;                                                    ))
;                                          ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2))))))))))
;                  

  
(define (init_grid_images)
  (set! grid_images (let loop ((i 0))
                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
                      ))
  (if (= mine_count (unopen-counter grid_images)) (set! game? 1) void) )

  
  
  ;(for [(i (length grids))]
;    (set! grid_images (append grid_images (list (list-ref (list-ref grids i) 0)))))
  
(define (init_grid_posns)
  (set! grid_posns (let loop ((i 0))
                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
                     ))
  )

                
(define (init_game i j )
  (begin
    (set!-values (rev_list shown_list game?) (values empty empty #t))
    (set! grid_values (make-list (* rows cols) 0))
    ;(define num1 (index i j))
;          (let loop ((i 0))
;            (if (< i (* cols rows)) (append (list 0) (loop (add1 i))) empty)
;            ))

    (let loop ((i 0))
      (let ((num (random (* cols rows))))
        (if (< i mine_count)
            (if (or (= num (index i j)) (equal? (list-ref grid_values num) "m"))
                                 (loop i)
                                 (begin (set! grid_values (list-set grid_values num "m"))
                                                                                         (loop (add1 i))))
            void)
        )
      )
    


    (set! n_list
          (let loop ((i 0))
            (if (< i (* cols rows)) (append (list (list 0 0 0 0 0 0 0 0)) (loop (add1 i))) empty)
            ))

    (let loop_j ((j 0))
      (if (< j rows) (let loop_i ((i 0))
                       (if (< i cols) (begin
                                        (if (not (equal? (list-ref grid_values (index i j)) "m"))
                                            (begin
                                              (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 0 (if (equal? (list-ref grid_values (index (- i 1) (- j 1))) "m") 1 0)))) void)
                                              (if                              (>= (- j 1) 0)           (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 1 (if (equal? (list-ref grid_values (index    i    (- j 1))) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 2 (if (equal? (list-ref grid_values (index (+ i 1) (- j 1))) "m") 1 0)))) void)
                                              (if      (<= (+ i 1) (- cols 1))                          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 3 (if (equal? (list-ref grid_values (index (+ i 1)    j   )) "m") 1 0)))) void)
                                              (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 4 (if (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if                              (<= (+ j 1) (- rows 1))  (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 5 (if (equal? (list-ref grid_values (index    i    (+ j 1))) "m") 1 0)))) void)
                                              (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 6 (if (equal? (list-ref grid_values (index (- i 1) (+ j 1))) "m") 1 0)))) void)
                                              (if      (>= (- i 1) 0)                                   (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 7 (if (equal? (list-ref grid_values (index (- i 1)    j   )) "m") 1 0)))) void)
                                              (set! grid_values (list-set grid_values (index i j) (sum (list-ref n_list (index i j)))))) void)
                                        (loop_i (add1 i))
                                        ) (loop_j (add1 j)))
                       ) void)
      )
    (init_grids)
    (init_grid_images)
    (init_grid_posns)
    
    ))
(define (find-index x y)
  (list (floor (/ x g_w)) (floor (/ y g_h)))
  )






(define (check-4-sides i j)
  (begin
    (if (not (member (list i j) rev_list)) (set! rev_list (append (list (list i j)) rev_list)) void)
    (if (>= (- i 1) 0) (if (and (equal? (list-ref grid_values (index (- i 1) j)) 0) (not (member (list (- i 1) j) rev_list)))
                           (check-4-sides (- i 1) j)
                           void) void)
    (if (>= (- j 1) 0) (if (and (equal? (list-ref grid_values (index i (- j 1))) 0) (not (member (list i (- j 1)) rev_list)))
                           (check-4-sides i (- j 1))
                           void) void)
    (if (<= (+ i 1) (- cols 1)) (if (and (equal? (list-ref grid_values (index (+ i 1) j)) 0) (not (member (list (+ i 1) j) rev_list)))
                                    (check-4-sides (+ i 1) j)
                                    void) void)
    (if (<= (+ j 1) (- rows 1)) (if (and (equal? (list-ref grid_values (index i (+ j 1))) 0) (not (member (list i (+ j 1)) rev_list)))
                                    (check-4-sides i ( + j 1))
                                    void) void)
    
    )
  )

(define (reveal i j)  
  (let ((v (list-ref grid_values (index i j))))    
    
      (if (eq? v "m")    (set! game? #f)
        
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))
 (define (co-reveal i j )
   (let ((v (list-ref grid_values (index i j))))      
      (if (eq? v "m")    void      
        (if (not (equal? v 0))
            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
            (check-4-sides i j))
        )
    ))

(define (show_n)
  (let loop ((n 0))
    (if (< n (length rev_list))
        (let ((i (list-ref (list-ref rev_list n) 0))
              (j (list-ref (list-ref rev_list n) 1)))
          (begin
            (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (- i 1) (- j 1))) 0)) (not (member (list (- i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (- j 1))) shown_list)) void) void)
            (if                              (>= (- j 1) 0)           (if (and (not (equal? (list-ref grid_values (index    i    (- j 1))) 0)) (not (member (list    i    (- j 1)) shown_list))) (set! shown_list (append (list (list    i    (- j 1))) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (+ i 1) (- j 1))) 0)) (not (member (list (+ i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (- j 1))) shown_list)) void) void)
            (if      (<= (+ i 1) (- cols 1))                          (if (and (not (equal? (list-ref grid_values (index (+ i 1)    j   )) 0)) (not (member (list (+ i 1)    j   ) shown_list))) (set! shown_list (append (list (list (+ i 1)    j   )) shown_list)) void) void)
            (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) 0)) (not (member (list (+ i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (+ j 1))) shown_list)) void) void)
            (if                              (<= (+ j 1) (- rows 1))  (if (and (not (equal? (list-ref grid_values (index    i    (+ j 1))) 0)) (not (member (list    i    (+ j 1)) shown_list))) (set! shown_list (append (list (list    i    (+ j 1))) shown_list)) void) void)
            (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (- i 1) (+ j 1))) 0)) (not (member (list (- i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (+ j 1))) shown_list)) void) void)
            (if      (>= (- i 1) 0)                                   (if (and (not (equal? (list-ref grid_values (index (- i 1)    j   )) 0)) (not (member (list (- i 1)    j   ) shown_list))) (set! shown_list (append (list (list (- i 1)    j   )) shown_list)) void) void)
            (loop (add1 n))
            ))
        (void))
    )
  
  )


(define (render ws)
  (place-images
   (append
    (list
     
     )
    grid_images)
   (append
    (list

     )
    grid_posns)
   scn)
  )

(define (key-handler ws ke)
  (if (and (key=? "r" ke)  game?) (init_game  ) void))
  

(define (release-handler ws ke)
  ws
  )
 (define c 1)
 
(define (mouse-handler ws x y me)
  (let ((i (list-ref (find-index x y) 0))
        (j (list-ref (find-index x y) 1)))
    (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) (= c 1))
        (begin
         (init_game i j )
          (reveal i j)
          (co-reveal (+ 1 i) j) (co-reveal (- i 1) j) (co-reveal i (+ j 1)) (co-reveal i (- j 1)) 
          (show_n)
          (init_grids)
          (init_grid_images)
          (set! c 0)
          
          ) void)
     (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) )
        (begin         
          (reveal i j)
          (show_n)
          (init_grids)
          (init_grid_images)
          (if (equal? 1 game?) (begin (set! game? #f) (init_grids)  (init_grid_images)  (play-sound "C:\\Users\\Abhijeet\\Downloads\\applause-01.wav" #f) (set! game? 1)) void)
          (if (not game?) (begin (init_grids) (init_grid_images) (play-sound "C:\\Users\\Abhijeet\\Downloads\\explosion-01.wav" #f)) void)
          (set! c 0)
          
        ) void)
    )
  )

(define (step ws)
  ws
  )
                                                 
                      (big-bang 0
                                                 (on-tick step)
                                                 (on-mouse mouse-handler)
                                                 (on-draw render)
                                                 (on-release release-handler)
                                                 (on-key key-handler))))))

(send main-frame show #t)


;(define (unopen-counter grid_images)
;  (cond [(null? grid_images) 0]
;        [(equal? (car grid_images) grid_image ) (+ 1 (unopen-counter (cdr grid_images)))]
;        [else (unopen-counter (cdr grid_images))]))
;
;
;(define scn (rectangle scn_w scn_h "solid" "blue")  )
;(define grid_image (overlay
;                    (rectangle g_w g_h "outline" fg_color)
;                    (rectangle g_w g_h 209 "green")
;                    (rectangle g_w g_h "solid" "red")
;                    ))
;(define m_image
;  (overlay
;   (circle (/ g_w 8) "solid" "black")
;   (let loop ((i 0))
;     (if (< i 8) (place-image (circle (/ g_w 30) 255 "red") (+ (/ g_w 5) (* (/ g_w 6) (cos (* i (/ pi 4))))) (+ (/ g_h 5) (* (/ g_h 6) (sin (* i (/ pi 4))))) (loop (add1 i))) (rectangle (/ (* g_w 2) 5) (/ (* g_h 2) 5) 0 "black"))
;     )
;   (rectangle g_w g_h "solid" "orange"))
;  )
;
;(define (grid_image_value v)
;  (overlay
;   (text/font (if (number? v) (if (= v 0) "" (number->string v)) "") (round (/ g_w 2.5)) fg_color "Gill Sans" 'swiss 'normal 'bold #f)
;   (rectangle g_w g_h "outline" fg_color)
;   (rectangle g_w g_h "solid" (color 0 0 0 180))
;   (rectangle g_w g_h 255 "white"))
;  )
;
;(define (index i j)
;  (+ i (* j cols))
;  )
;
;(define (sum l)
;  (foldl + 0 l)
;  )
;
;(define (init_grids)
;  (set! grids (let loop_j ((j 0))
;                (if (< j rows) (let loop_i ((i 0))
;                                (if (< i cols) (let ((index (+ i (* j cols))))
;                                                  (append (list (list (cond
;                                                                        ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
;                                                                        (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
;                                                                                  (grid_image_value (list-ref grid_values index))
;                                                                                  grid_image
;                                                                                  ))
;                                                                        ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2)))) (loop_i (add1 i)))
;                                                  ) (loop_j (add1 j)))
;                                 ) empty)
;                ))
;  )
;;  (for ([j rows])
;;                (for ([i cols])
;;                  (let* ((index (+ i (* j cols))))
;;                    (set! grids (append grids (list (list (cond
;;                                          ((equal? (list-ref grid_values index) "m") (if game? grid_image m_image))
;;                                          (else (if (or (member (list i j) shown_list) (member (list i j) rev_list))
;;                                                    (grid_image_value (list-ref grid_values index))
;;                                                    grid_image
;;                                                    ))
;;                                          ) (+ (* i g_w) (/ g_w 2)) (+ (* j g_h) (/ g_h 2))))))))))
;;                  
;
;  
;(define (init_grid_images)
;  (set! grid_images (let loop ((i 0))
;                      (if (< i (length grids)) (append (list (list-ref (list-ref grids i) 0)) (loop (add1 i))) empty)
;                      ))
;  (if (= mine_count (unopen-counter grid_images)) (set! game? 1) void) )
;
;  
;  
;  ;(for [(i (length grids))]
;;    (set! grid_images (append grid_images (list (list-ref (list-ref grids i) 0)))))
;  
;(define (init_grid_posns)
;  (set! grid_posns (let loop ((i 0))
;                     (if (< i (length grids)) (append (list (make-posn (list-ref (list-ref grids i) 1) (list-ref (list-ref grids i) 2))) (loop (add1 i))) empty)
;                     ))
;  )
;
;                
;(define (init_game i j )
;  (begin
;    (set!-values (rev_list shown_list game?) (values empty empty #t))
;    (set! grid_values (make-list (* rows cols) 0))
;    ;(define num1 (index i j))
;;          (let loop ((i 0))
;;            (if (< i (* cols rows)) (append (list 0) (loop (add1 i))) empty)
;;            ))
;
;    (let loop ((i 0))
;      (let ((num (random (* cols rows))))
;        (if (< i mine_count)
;            (if (or (= num (index i j)) (equal? (list-ref grid_values num) "m"))
;                                 (loop i)
;                                 (begin (set! grid_values (list-set grid_values num "m"))
;                                                                                         (loop (add1 i))))
;            void)
;        )
;      )
;    
;
;
;    (set! n_list
;          (let loop ((i 0))
;            (if (< i (* cols rows)) (append (list (list 0 0 0 0 0 0 0 0)) (loop (add1 i))) empty)
;            ))
;
;    (let loop_j ((j 0))
;      (if (< j rows) (let loop_i ((i 0))
;                       (if (< i cols) (begin
;                                        (if (not (equal? (list-ref grid_values (index i j)) "m"))
;                                            (begin
;                                              (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 0 (if (equal? (list-ref grid_values (index (- i 1) (- j 1))) "m") 1 0)))) void)
;                                              (if                              (>= (- j 1) 0)           (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 1 (if (equal? (list-ref grid_values (index    i    (- j 1))) "m") 1 0)))) void)
;                                              (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 2 (if (equal? (list-ref grid_values (index (+ i 1) (- j 1))) "m") 1 0)))) void)
;                                              (if      (<= (+ i 1) (- cols 1))                          (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 3 (if (equal? (list-ref grid_values (index (+ i 1)    j   )) "m") 1 0)))) void)
;                                              (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 4 (if (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) "m") 1 0)))) void)
;                                              (if                              (<= (+ j 1) (- rows 1))  (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 5 (if (equal? (list-ref grid_values (index    i    (+ j 1))) "m") 1 0)))) void)
;                                              (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 6 (if (equal? (list-ref grid_values (index (- i 1) (+ j 1))) "m") 1 0)))) void)
;                                              (if      (>= (- i 1) 0)                                   (set! n_list (list-set n_list (index i j) (list-set (list-ref n_list (index i j)) 7 (if (equal? (list-ref grid_values (index (- i 1)    j   )) "m") 1 0)))) void)
;                                              (set! grid_values (list-set grid_values (index i j) (sum (list-ref n_list (index i j)))))) void)
;                                        (loop_i (add1 i))
;                                        ) (loop_j (add1 j)))
;                       ) void)
;      )
;    (init_grids)
;    (init_grid_images)
;    (init_grid_posns)
;    
;    ))
;(define (find-index x y)
;  (list (floor (/ x g_w)) (floor (/ y g_h)))
;  )
;
;
;
;
;
;
;(define (check-4-sides i j)
;  (begin
;    (if (not (member (list i j) rev_list)) (set! rev_list (append (list (list i j)) rev_list)) void)
;    (if (>= (- i 1) 0) (if (and (equal? (list-ref grid_values (index (- i 1) j)) 0) (not (member (list (- i 1) j) rev_list)))
;                           (check-4-sides (- i 1) j)
;                           void) void)
;    (if (>= (- j 1) 0) (if (and (equal? (list-ref grid_values (index i (- j 1))) 0) (not (member (list i (- j 1)) rev_list)))
;                           (check-4-sides i (- j 1))
;                           void) void)
;    (if (<= (+ i 1) (- cols 1)) (if (and (equal? (list-ref grid_values (index (+ i 1) j)) 0) (not (member (list (+ i 1) j) rev_list)))
;                                    (check-4-sides (+ i 1) j)
;                                    void) void)
;    (if (<= (+ j 1) (- rows 1)) (if (and (equal? (list-ref grid_values (index i (+ j 1))) 0) (not (member (list i (+ j 1)) rev_list)))
;                                    (check-4-sides i ( + j 1))
;                                    void) void)
;    
;    )
;  )
;
;(define (reveal i j)  
;  (let ((v (list-ref grid_values (index i j))))    
;    
;      (if (eq? v "m")    (set! game? #f)
;        
;        (if (not (equal? v 0))
;            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
;            (check-4-sides i j))
;        )
;    ))
; (define (co-reveal i j )
;   (let ((v (list-ref grid_values (index i j))))      
;      (if (eq? v "m")    void      
;        (if (not (equal? v 0))
;            (if (not (member (list i j) shown_list)) (set! shown_list (append (list (list i j)) shown_list)) void)
;            (check-4-sides i j))
;        )
;    ))
;
;(define (show_n)
;  (let loop ((n 0))
;    (if (< n (length rev_list))
;        (let ((i (list-ref (list-ref rev_list n) 0))
;              (j (list-ref (list-ref rev_list n) 1)))
;          (begin
;            (if (and (>= (- i 1) 0)          (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (- i 1) (- j 1))) 0)) (not (member (list (- i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (- j 1))) shown_list)) void) void)
;            (if                              (>= (- j 1) 0)           (if (and (not (equal? (list-ref grid_values (index    i    (- j 1))) 0)) (not (member (list    i    (- j 1)) shown_list))) (set! shown_list (append (list (list    i    (- j 1))) shown_list)) void) void)
;            (if (and (<= (+ i 1) (- cols 1)) (>= (- j 1) 0))          (if (and (not (equal? (list-ref grid_values (index (+ i 1) (- j 1))) 0)) (not (member (list (+ i 1) (- j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (- j 1))) shown_list)) void) void)
;            (if      (<= (+ i 1) (- cols 1))                          (if (and (not (equal? (list-ref grid_values (index (+ i 1)    j   )) 0)) (not (member (list (+ i 1)    j   ) shown_list))) (set! shown_list (append (list (list (+ i 1)    j   )) shown_list)) void) void)
;            (if (and (<= (+ i 1) (- cols 1)) (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (+ i 1) (+ j 1))) 0)) (not (member (list (+ i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (+ i 1) (+ j 1))) shown_list)) void) void)
;            (if                              (<= (+ j 1) (- rows 1))  (if (and (not (equal? (list-ref grid_values (index    i    (+ j 1))) 0)) (not (member (list    i    (+ j 1)) shown_list))) (set! shown_list (append (list (list    i    (+ j 1))) shown_list)) void) void)
;            (if (and (>= (- i 1) 0)          (<= (+ j 1) (- rows 1))) (if (and (not (equal? (list-ref grid_values (index (- i 1) (+ j 1))) 0)) (not (member (list (- i 1) (+ j 1)) shown_list))) (set! shown_list (append (list (list (- i 1) (+ j 1))) shown_list)) void) void)
;            (if      (>= (- i 1) 0)                                   (if (and (not (equal? (list-ref grid_values (index (- i 1)    j   )) 0)) (not (member (list (- i 1)    j   ) shown_list))) (set! shown_list (append (list (list (- i 1)    j   )) shown_list)) void) void)
;            (loop (add1 n))
;            ))
;        (void))
;    )
;  
;  )
;
;
;(define (render ws)
;  (place-images
;   (append
;    (list
;     
;     )
;    grid_images)
;   (append
;    (list
;
;     )
;    grid_posns)
;   scn)
;  )
;
;(define (key-handler ws ke)
;  (if (and (key=? "r" ke)  game?) (init_game  ) void))
;  
;
;(define (release-handler ws ke)
;  ws
;  )
; (define c 1)
; 
;(define (mouse-handler ws x y me)
;  (let ((i (list-ref (find-index x y) 0))
;        (j (list-ref (find-index x y) 1)))
;    (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) (= c 1))
;        (begin
;         (init_game i j )
;          (reveal i j)
;          (co-reveal (+ 1 i) j) (co-reveal (- i 1) j) (co-reveal i (+ j 1)) (co-reveal i (- j 1)) 
;          (show_n)
;          (init_grids)
;          (init_grid_images)
;          (set! c 0)
;          
;          ) void)
;     (if (and game? (string=? me "button-down") (< 0 x scn_w) (< 0 y scn_h) )
;        (begin         
;          (reveal i j)
;          (show_n)
;          (init_grids)
;          (init_grid_images)
;          (if (equal? 1 game?) (begin (set! game? #f) (init_grids)  (init_grid_images)  (play-sound "C:\\Users\\Abhijeet\\Downloads\\applause-01.wav" #f) (set! game? 1)) void)
;          (if (not game?) (begin (init_grids) (init_grid_images) (play-sound "C:\\Users\\Abhijeet\\Downloads\\explosion-01.wav" #f)) void)
;          (set! c 0)
;          
;        ) void)
;    )
;  )
;
;(define (step ws)
;  ws
;  )

;(big-bang 0
;          (on-tick step)
;          (on-mouse mouse-handler)
;          (on-draw render)
;          (on-release release-handler)
;          (on-key key-handler)
;          )