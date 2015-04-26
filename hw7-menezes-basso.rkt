;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname hw7-menezes-basso) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Olivia Menezes (menezes.o@husky.neu.edu)
;;Robert Basso (basso.r@husky.neu.edu)

;;HOMEWORK 7-- Frogger

(require 2htdp/image)
(require 2htdp/universe)

;; A Player is a (make-player Number Number Direction)
(define-struct player (x y dir))

(define p1 (make-player 300 260 'up))
(define p2 (make-player 660 300 'right))
(define p3 (make-player -10 400 'left))

;; A Vehicle is a (make-vehicle Number Number Direction)
(define-struct vehicle (x y dir))

(define v1 (make-vehicle 20 60 'right))
(define v2 (make-vehicle 160 60 'right))
(define v3 (make-vehicle 300 60 'right))
(define v4 (make-vehicle 440 60 'right))

(define v5 (make-vehicle 30 100 'left))
(define v6 (make-vehicle 170 100 'left))
(define v7 (make-vehicle 310 100 'left))
(define v8 (make-vehicle 450 100 'left))

(define v9 (make-vehicle 10 140 'right))
(define v10 (make-vehicle 150 140 'right))
(define v11 (make-vehicle 290 140 'right))
(define v12 (make-vehicle 430 140 'right))

(define v13 (make-vehicle 40 180 'left))
(define v14 (make-vehicle 180 180 'left))
(define v15 (make-vehicle 320 180 'left))
(define v16 (make-vehicle 460 180 'left))

(define v17 (make-vehicle 20 220 'right))
(define v18 (make-vehicle 160 220 'right))
(define v19 (make-vehicle 300 220 'right))
(define v20 (make-vehicle 440 220 'right))

(define v21 (make-vehicle 610 300 'right))
(define v22 (make-vehicle -10 400 'left))
(define v23 (make-vehicle 300 260 'right))



;; A Set of Vehicles (VSet) is one of:     
;; - empty     
;; - (cons Vehicle VSet)   

(define vset1 (list v1 v2 v3 v4))
(define vset2 (list v21 v22))
(define vset3 (list v21 v22 v23))
(define vsetall (list v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 
                      v11 v12 v13 v14 v15 v16 v17 v18 v19 v20))


;; A World is a (make-world Player VSet)
;;The VSet represents the set of vehicles moving across the screen
(define-struct world (player vehicles))

(define frog-image (circle 10 "solid" "green"))
(define vehic-image (rectangle 30 20 "solid" "brown"))

(define scene0   
  (place-image
   (rectangle 600 40 "solid" "gray")
   300 20
   (place-image
    (rectangle 600 40 "solid" "gray")
    300 260
    (empty-scene 600 280))))

;;place-car: Vehicle Scene -> Image
;;places a vehicle on a scene
(define (place-car v scn)
  (place-image vehic-image
               (vehicle-x v)
               (vehicle-y v)
               scn))

(check-expect (place-car v1 scene0) (place-image
                                     vehic-image
                                     20 60
                                     scene0))

;;place-all-cars: Vset Scene -> Image
;;places all the cars on the scene
(define (place-all-cars avset scn)
  (cond [(empty? avset) scn]
        [else (place-car (first avset)(place-all-cars (rest avset) scn))]))

(check-expect (place-all-cars vset1 scene0) (place-image vehic-image
                                                         20 60
                                                         (place-image
                                                          vehic-image
                                                          160 60
                                                          (place-image
                                                           vehic-image
                                                           300 60
                                                           (place-image
                                                            vehic-image
                                                            440 60
                                                            scene0)))))



(define world0 (make-world p1 vsetall))
(define world-tests (make-world p1 vset1))
(define world-tests1 (make-world p2 vset1))
(define world-tests2 (make-world p3 vset1))

;;draw-world: World -> World
;;draws the world on the scene
(define (draw-world w)
  (place-image frog-image
               (player-x (world-player w))
               (player-y (world-player w))
               (place-all-cars (world-vehicles w) scene0)))

(check-expect (draw-world world-tests) (place-image
                                        frog-image
                                        300 260
                                        (place-image 
                                         vehic-image
                                         20 60
                                         (place-image
                                          vehic-image
                                          160 60
                                          (place-image
                                           vehic-image
                                           300 60
                                           (place-image
                                            vehic-image
                                            440 60
                                            scene0))))))



;;----------------------------------------------------------------------------------------------------------------------------------------------------
;;MAKE IT MOVE!

;;f-off-screen?: World Key -> World
;;returns the player if they go off screen
(define (f-off-screen? w k)
  (cond [(key=? "left" k)
         (make-world (make-player 600 (player-y (world-player w)) "left") (world-vehicles w))]
        [(key=? "right" k) 
         (make-world (make-player 0 (player-y (world-player w)) "right") (world-vehicles w))]))

(check-expect (f-off-screen? world-tests1 "right") (make-world (make-player 0 300 "right") vset1))
(check-expect (f-off-screen? world-tests2 "left") (make-world (make-player 600 400 "left") vset1))
  
  
;;leap-frog: World Key -> World
;;moves the frog either right, left, up, or down
(define (leap-frog w k)
  (cond [(and (key=? "left" k)(> (player-x (world-player w)) 0)) 
         (make-world (make-player
                      (- (player-x (world-player w)) 5) (player-y (world-player w)) "left") (world-vehicles w))]
        [(and (key=? "right" k) (< (player-x (world-player w)) 600))
         (make-world (make-player
                      (+ (player-x (world-player w)) 5) (player-y (world-player w)) "right") (world-vehicles w))]
        [(key=? "up" k) 
         (make-world (make-player 
                      (player-x (world-player w)) (- (player-y (world-player w)) 5) "up") (world-vehicles w))]
        [(key=? "down" k) 
         (make-world (make-player 
                      (player-x (world-player w)) (+ (player-y (world-player w)) 5) "down") (world-vehicles w))]
        [else (f-off-screen? w k)]))

(check-expect (leap-frog world0 "left") (make-world (make-player 295 260 "left") vsetall))
(check-expect (leap-frog world0 "right") (make-world (make-player 305 260 "right") vsetall))
(check-expect (leap-frog world0 "up") (make-world (make-player 300 255 "up") vsetall))
(check-expect (leap-frog world0 "down") (make-world (make-player 300 265 "down") vsetall))
(check-expect (leap-frog world-tests1 "right") (make-world (make-player 0 300 "right") vset1))
(check-expect (leap-frog world-tests2 "left") (make-world (make-player 600 400 "left") vset1))

;;car-off-screen?: Vehicle -> Vehicle
;;returns a car if it goes off screen
(define (car-off-screen? v)
  (cond [(symbol=? 'right (vehicle-dir v)) (make-vehicle 0 (vehicle-y v) 'right)]
        [(symbol=? 'left (vehicle-dir v)) (make-vehicle 600 (vehicle-y v) 'left)]))

(check-expect (car-off-screen? v21) (make-vehicle 0 300 'right))
(check-expect (car-off-screen? v22) (make-vehicle 600 400 'left))
  
;;move-car: Vehicle -> Vehicle
;;changes the position of a car
(define (move-car v)
  (cond [(and (symbol=? 'right (vehicle-dir v))
              (< (vehicle-x v) 600))
         (make-vehicle (+ (vehicle-x v) 3) (vehicle-y v) 'right)]
        [(and (symbol=? 'left (vehicle-dir v))
              (> (vehicle-x v) 0))
         (make-vehicle (- (vehicle-x v) 3) (vehicle-y v) 'left)]
        [else (car-off-screen? v)]))

(check-expect (move-car v1) (make-vehicle 23 60 'right))
(check-expect (move-car v5) (make-vehicle 27 100 'left))
(check-expect (move-car v21) (make-vehicle 0 300 'right))
(check-expect (move-car v22) (make-vehicle 600 400 'left))


;;move-all-cars: Vset -> Vset
;;changes the position of all the cars
(define (move-all-cars avset)
  (cond [(empty? avset) empty]
        [else (cons (move-car (first avset))(move-all-cars (rest avset)))]))


(check-expect (move-all-cars vset1) (list (make-vehicle 23 60 'right)
                                          (make-vehicle 163 60 'right)
                                          (make-vehicle 303 60 'right)
                                          (make-vehicle 443 60 'right)))

;;MOVE: World -> World
;;moves all the cars on the scene
(define (MOVE w)
  (make-world (world-player w)
              (move-all-cars (world-vehicles w))))

(check-expect (MOVE world-tests) (make-world p1 (list (make-vehicle 23 60 'right)
                                                      (make-vehicle 163 60 'right)
                                                      (make-vehicle 303 60 'right)
                                                      (make-vehicle 443 60 'right))))

;; in-range?: Number, Number, Number -> Boolean
;; is n1 within range of n2?
(define (in-range? n1 n2 range)
  (and (< n1 (+ n2 range))
       (> n1 (- n2 range))))

(check-expect (in-range? 6 4 5) true)

;; hit?: Player Vehicle -> Boolean
;; was the player hit by the vehicle?
(define (hit? player vehicle)
  (and (in-range? (player-y player)
                  (vehicle-y vehicle)
                  (+ (/ (image-height frog-image) 2)
                     (/ (image-height vehic-image) 2)))
       (in-range? (player-x player)
                  (vehicle-x vehicle)
                  (+ (/ (image-width frog-image) 2)
                     (/ (image-width vehic-image) 2)))))

(check-expect (hit? p1 v23) true)


;; hit-one: Player Vset -> Boolean
;;determines whether a player has hit a vehicle
(define (hit-one p avset)
  (cond [(empty? avset) false]
        [(hit? p (first avset)) true]
        [else (hit-one p (rest avset))]))

(check-expect (hit-one p1 vsetall) false)
(check-expect (hit-one p1 vset3) true)

;;HIT: World -> World
;;tells whether the player has hit any vehicles
(define (HIT w)
  (hit-one (world-player w)
           (world-vehicles w)))

(check-expect (HIT world0) false)



(big-bang world0
          (to-draw draw-world)
          (on-tick MOVE)
          (on-key leap-frog)
          (stop-when HIT))
