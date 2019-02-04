;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders
;; an interpretation of the arcade game Space Invaders

;; Constants:
;;==================================================================================================

(define WIDTH  300)
(define HEIGHT 500)

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 1000) ; increasing INVADE-RATE will increase the amount of time that passes between invaders spawing

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-LANDED-Y (- HEIGHT (/ (image-height INVADER) 2)))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2)) ;tank y placement

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-LAUNCH-Y (- (- HEIGHT (image-height TANK)) (/ (image-height MISSILE) 2))) ;height of missile when first fired


;; Data Definitions:
;;==================================================================================================

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 250 250 -10))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 20 200))

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I4) (list M2 M4) T0)) ;-> next game state should only include I4, M4, and T0 with I4 and M4 positions updated

;; Functions
;;==================================================================================================
;; Game -> Game
;; start the world with (main (make-game empty empty T0))
;; 
(define (main g)
  (big-bang g                    ; Game
    (on-tick   tock)             ; Game -> Game
    (to-draw   render)           ; Game -> Image
    (stop-when invader-landed?)  ; Game -> Boolean
    (on-key    handle-key)))     ; Game KeyEvent -> Game

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; produce the next game state, should:
;; update the invaders positions
;; update the missile positions
;; remove missiles that have left the screen
;; detect any missile invader collisions and remove those objects
;; and spawn new invaders
(check-expect (tock G0) G0)
(check-expect (tock G4) (make-game (list (make-invader (- 250 INVADER-X-SPEED) (+ 250 INVADER-Y-SPEED) -10))
                                   (list (make-missile 20 (- 200 MISSILE-SPEED)))
                                   T0))

;(define (tock g) G0) ;stub

(define (tock g)
  
  (spawn-invaders (detect-hits (remove-missiles (update-missiles (update-invaders g))))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; update invader function and helpers
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; pass ListOfInvaders object to advance-invaders
(check-expect (update-invaders G0) G0)
(check-expect (update-invaders G3) (make-game (list (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                                                    (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2)))
                                              (list M1 M2)
                                              T1)) 

;(define (update-invaders g) G0) ;stub

(define (update-invaders g)
  (make-game (advance-invaders (game-invaders g)) (game-missiles g) (game-tank g)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders -> ListOfInvaders
;; check if the invaders are at the screen boundary, if true pass invader to change-invader-dir
;; pass each invader to update-invader-position to advance position
(check-expect (advance-invaders (game-invaders G0)) empty)
(check-expect (advance-invaders (game-invaders G3)) (list (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                                                          (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))))
(check-expect (advance-invaders (list (make-invader (- 300 (/ (image-width INVADER) 2)) 250 10))) (list (make-invader (- (- 300 (/ (image-width INVADER) 2)) INVADER-X-SPEED)
                                                                                                                      (+ 250 INVADER-Y-SPEED) -10)))

;(define (advance-invaders loi) empty) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else (if (invader-at-bound? (first loi))
                  (append (list (update-invader-position (change-invader-dir (first loi))))
                          (advance-invaders (rest loi)))
                  (append (list (update-invader-position (first loi)))
                          (advance-invaders (rest loi))))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Invader -> Boolean
;; produce true if the Invader is at the MTS boundary
(check-expect (invader-at-bound? (make-invader 25 25 10))    false)
(check-expect (invader-at-bound? (make-invader 0 25 10 ))    true)
(check-expect (invader-at-bound? (make-invader WIDTH 25 10)) true)

;(define (invader-at-bound? I1) false) ;stub

(define (invader-at-bound? i)
  (or (<= (invader-x i) (/ (image-width INVADER) 2))
      (>= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Invader -> Invader
;; change the direction of the invader at boundary
(check-expect (change-invader-dir I1) (make-invader (invader-x I1) (invader-y I1) (* -1 (invader-dx I1))))
(check-expect (change-invader-dir I2) (make-invader (invader-x I2) (invader-y I2) (* -1 (invader-dx I2))))

;(define (change-invader-dir I1) I1) ;stub

(define (change-invader-dir i)
  (make-invader (invader-x i) (invader-y i) (* -1 (invader-dx i))))

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Invader -> Invader
;; advance invader position by INVADER-X-SPEED and INVADER-Y-SPEED
(check-expect (update-invader-position (make-invader 10 10 10))  (make-invader (+ 10 INVADER-X-SPEED) (+ 10 INVADER-Y-SPEED)  10))
(check-expect (update-invader-position (make-invader 10 10 -10)) (make-invader (- 10 INVADER-X-SPEED) (+ 10 INVADER-Y-SPEED) -10))

;(define (update-invader-position I1) I1) ;stub

(define (update-invader-position i)
  (if (< (invader-dx i) 0)
      (make-invader (- (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))
      (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; update missile functions and helper
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; pass ListOfMissiles object to advance-missiles
(check-expect (update-missiles G0) G0)
(check-expect (update-missiles G3) (make-game (list I1 I2)
                                              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                                                    (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED)))
                                              T1))

;(define (update-missiles g) G0) ;stub

(define (update-missiles g)
  (make-game (game-invaders g) (advance-missiles (game-missiles g)) (game-tank g)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfMissiles -> ListOfMissiles
;; advance all current game state missiles by constant MISSILE-SPEED
(check-expect (advance-missiles (game-missiles G0)) empty)
(check-expect (advance-missiles (game-missiles G3)) (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))))

;(define (advance-missiles lom) empty) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (append
          (list (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED)))
          (advance-missiles (rest lom)))]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; remove missiles function and helper
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; pass ListOfMissiles object to check-missiles
(check-expect (remove-missiles G0) G0)
(check-expect (remove-missiles (make-game (list (make-invader 150 100 12) (make-invader 150 HEIGHT -10))
                                          (list (make-missile 150 300) (make-missile 150 -7.5))
                                          T1))
              (make-game (list (make-invader 150 100 12) (make-invader 150 HEIGHT -10))
                         (list (make-missile 150 300))
                         T1))

;(define (remove-missiles g) G0) ;stub

(define (remove-missiles g)
  (make-game (game-invaders g) (check-missiles (game-missiles g)) (game-tank g)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfMissiles -> ListOfMissiles
;; remove all missiles that have gone off the screen (missile height is >= -7.5)
(check-expect (check-missiles empty) empty)
(check-expect (check-missiles (list (make-missile 10 10)  (make-missile 5 15)   (make-missile 1 1)))    (list (make-missile 10 10) (make-missile 5 15) (make-missile 1 1)))
(check-expect (check-missiles (list (make-missile 5 -7.5) (make-missile 10 10)  (make-missile 1 1)))    (list (make-missile 10 10) (make-missile 1 1)))
(check-expect (check-missiles (list (make-missile 10 10)  (make-missile 5 -7.5) (make-missile 1 1)))    (list (make-missile 10 10) (make-missile 1 1)))
(check-expect (check-missiles (list (make-missile 10 10)  (make-missile 1 1)    (make-missile 5 -7.5))) (list (make-missile 10 10) (make-missile 1 1)))

;(define (check-missiles lom) empty) ;stub

(define (check-missiles lom)
  (cond [(empty? lom) empty]
        [(<= (missile-y (first lom)) -7.5)
         (check-missiles (rest lom))]
        [else (append (list (first lom))
                      (check-missiles (rest lom)))]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; detect hits function and helpers
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; pass ListOfInvaders and ListOfMissiles to check-each-invader and check-each-missile functions
(check-expect (detect-hits G0) G0)
(check-expect (detect-hits G3) (make-game (list (make-invader 150 500 -10)) (list (make-missile 150 300)) T1))

;(define (detect-hits g) g) ;stub

(define (detect-hits g)
  (make-game
   (check-each-invader (game-invaders g) (game-missiles g))
   (check-each-missile (game-missiles g) (game-invaders g))
   (game-tank g)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; pass each invader and a ListOfMissiles object to invader-hit? to check for collisions
;; if a collision is detected, remove the invader from loi (this function), and the missile from lom (remove-missile)
;; only the ListOfInvaders is returned to detect-hits
(check-expect (check-each-invader empty empty) empty)
(check-expect (check-each-invader (list I1 I2) (list M1 M2)) (list I2))

;(define (check-each-invader loi lom) loi) ;stub

(define (check-each-invader loi lom)
  (cond [(empty? loi) empty]
        [(invader-hit? (first loi) lom)
         (check-each-invader (rest loi) (remove-missile (first loi) lom))]
        [else (append (list (first loi))
                      (check-each-invader (rest loi) lom))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Invader ListOfMissiles -> Boolean
;; produce true if the invader has been hit (images overlap or just touch),  by any of the missiles in lom
(check-expect (invader-hit? I1 empty) false)
(check-expect (invader-hit? I2 (list M1 M2)) false)
(check-expect (invader-hit? I1 (list M1 M2)) true)

;(define (invader-hit? i lom) false) ;stub

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [(and
          (and (<= (- (missile-x (first lom)) (invader-x i)) 12.5)
               (<= (- (invader-x i)           (missile-x (first lom))) 12.5))
          (and (<= (- (missile-y (first lom)) (invader-y i)) 15.5)
               (<= (- (invader-y i)           (missile-y (first lom))) 15.5))) true]
        [else (invader-hit? i (rest lom))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Invader ListOfMissiles -> ListOfMissiles
;; remove the missile that has struck an invader, is only called if a missile has struck an invader
(check-expect (remove-missile I1 (list M1 M2)) (list M1))

;(define (remove-missile i lom) lom) ;stub

(define (remove-missile i lom)
  (cond [(and
          (and (<= (- (missile-x (first lom)) (invader-x i)) 12.5)
               (<= (- (invader-x i)           (missile-x (first lom))) 12.5))
          (and (<= (- (missile-y (first lom)) (invader-y i)) 15.5)
               (<= (- (invader-y i)           (missile-y (first lom))) 15.5)))
         (rest lom)]
        [else (append (list (first lom))
                      (remove-missile i (rest lom)))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; pass each missile and a ListOfInvaders to missile-hit? to check for collisions
;; if a collision is detected, remove the missile from lom (this function), and the invader from loi (remove-invader)
;; only the ListOfMissiles is returned to detect-hits
(check-expect (check-each-missile empty empty) empty)
(check-expect (check-each-missile (list M1 M2) (list I1 I2)) (list M1))

;(define (check-each-missile lom loi) lom) ;stub

(define (check-each-missile lom loi)
  (cond [(empty? lom) empty]
        [(missile-hit? (first lom) loi)
         (check-each-missile (rest lom) (remove-invader (first lom) loi))]
        [else (append (list (first lom))
                      (check-each-missile (rest lom) loi))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Missile ListOfInvaders -> Boolean
;; produce true if the missile has hit any of the invaders in the loi (images overlap or just touch)
(check-expect (missile-hit? M1 empty) false)
(check-expect (missile-hit? M1 (list I1 I2)) false)
(check-expect (missile-hit? M2 (list I1 I2)) true)

;(define (missile-hit? i lom) false) ;stub

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [(and
          (and (<= (- (missile-x m)            (invader-x (first loi))) 12.5)
               (<= (- (invader-x (first loi))  (missile-x m)) 12.5))
          (and (<= (- (missile-y m)            (invader-y (first loi))) 15.5)
               (<= (- (invader-y (first loi))  (missile-y m)) 15.5))) true]
        [else (missile-hit? m (rest loi))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Missile ListOfInvaders -> ListOfInvaders
;; remove the invader that was struck by a missile, only called if an invader has been hit by a missile
(check-expect (remove-invader M2 (list I2 I1)) (list I2))

;(define (remove-invader m loi) loi) ;stub

(define (remove-invader m loi)
  (cond [(and
          (and (<= (- (missile-x m)           (invader-x (first loi))) 12.5)
               (<= (- (invader-x (first loi)) (missile-x m)) 12.5))
          (and (<= (- (missile-y m)           (invader-y (first loi))) 15.5)
               (<= (- (invader-y (first loi)) (missile-y m)) 15.5)))
         (rest loi)]
        [else (append (list (first loi))
                      (remove-invader m (rest loi)))]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; spawn invaders function and helper -> produce new invaders at a random interval, invaders should spawn at a random x location and y location = (0.5*INVADER height)
;; invader direction should be a random natural less than 13
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Game
;; spawn new invaders if (random INVADE-RATE) is less than or equal to 10 ~ 28% chance of spawing every second ~ 1 invader every 3.5 seconds
;; increasing the INVADE-RATE will make between spawns longer, decreasing it will decrease the time between spawing
;; there is no (check-expect) because the function relies on the random function

;(define (spawn-invaders g) G0) ;stub

(define (spawn-invaders g)
  (if (<= (random INVADE-RATE) 10)
      (make-game (add-invader (game-invaders g)) (game-missiles g) (game-tank g))
      g))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders -> ListOfInvaders
;; add a new invader to the invaders list
;; there is no (check-expect) because the function relies on the random function

(define (add-invader loi)
  (append loi (list (make-invader (+ 5 (random (- WIDTH (image-width INVADER)))) (/ (image-height INVADER) 2) (random 13)))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; render function and helpers -> create the image to display every clock tick
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Image
;; use place-images to display all on screen images
;; place-images requires a list of images and a list of posns corresponding to each image
(check-expect (render G0) (place-images (list empty-image empty-image TANK) (list (make-posn 0 0) (make-posn 0 0) (make-posn (tank-x T0) TANK-Y)) MTS))
(check-expect (render G3) (place-images (list INVADER INVADER MISSILE MISSILE TANK)
                                        (list (make-posn (invader-x I1) (invader-y I1)) (make-posn (invader-x I2) (invader-y I2))
                                              (make-posn (missile-x M1) (missile-y M1)) (make-posn (missile-x M2) (missile-y M2))
                                              (make-posn (tank-x T1) TANK-Y)) MTS))

;(define (render g) (place-image TANK (tank-x (game-tank g)) (- HEIGHT (/ (image-height TANK) 2)) MTS)) ;stub

(define (render g)
  (place-images (build-image-list g) (build-posn-list g) MTS))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> ListOfImages
;; produce a list of all images in the current game state
;; empty lists and end of lists should produce an empty-image
;; pass ListOfInvaders to invader-images
;; pass ListOfMissiles to missile-images
(check-expect (build-image-list G0) (list empty-image empty-image TANK))
(check-expect (build-image-list G3) (list INVADER INVADER empty-image MISSILE MISSILE empty-image TANK))

;(define (build-image-list g) empty) ;stub

(define (build-image-list g)
  (append (invader-images (game-invaders g))
          (missile-images (game-missiles g))
          (list TANK)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders -> ListOfImages
;; produce a list of INVADER images, there should be one image for each (make-invader) in the ListOfInvaders
;; empty lists and end of lists should produce an empty-image
(check-expect (invader-images (game-invaders G0)) (list empty-image))
(check-expect (invader-images (game-invaders G3)) (list INVADER INVADER empty-image))

;(define (invader-images loi) empty) ;stub

(define (invader-images loi)
  (cond [(empty? loi) (list empty-image)]
        [else
         (append
          (list INVADER)
          (invader-images (rest loi)))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfMissiles -> ListOfImages
;; produce a list of MISSILE images, there should be one image for each (make-missile) in the ListOfMissiles
;; empty lists and end of lists should produce an empty-image
(check-expect (missile-images (game-missiles G0)) (list empty-image))
(check-expect (missile-images (game-missiles G3)) (list MISSILE MISSILE empty-image))

;(define (missile-images lom) empty) ;stub

(define (missile-images lom)
  (cond [(empty? lom) (list empty-image)]
        [else
         (append
          (list MISSILE)
          (missile-images (rest lom)))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> ListOfPosn
;; produce a list of all posns for each Invader, Missile, and Tank in the current game state
;; empty lists and end of lists should produce a (make-posn 0 0)
;; pass ListOfInvaders to invader-posns
;; pass ListOfMissiles to missile-posns
(check-expect (build-posn-list G0) (list (make-posn 0 0) (make-posn 0 0) (make-posn (tank-x T0) TANK-Y)))
(check-expect (build-posn-list G3) (list (make-posn (invader-x I1) (invader-y I1)) (make-posn (invader-x I2) (invader-y I2)) (make-posn 0 0)
                                         (make-posn (missile-x M1) (missile-y M1)) (make-posn (missile-x M2) (missile-y M2)) (make-posn 0 0)
                                         (make-posn (tank-x T1)     TANK-Y))) 

;(define (build-posn-list g) empty) ;stub

(define (build-posn-list g)
  (append (invader-posns (game-invaders g))
          (missile-posns (game-missiles g))
          (list (make-posn (tank-x (game-tank g)) TANK-Y))))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders -> ListOfPosns
;; produce a list of posns, there should be one posn for each Invader in ListOfInvaders
;; empty lists and end of lists should produce a (make-posn 0 0)
(check-expect (invader-posns (game-invaders G0)) (list (make-posn 0 0)))
(check-expect (invader-posns (game-invaders G3)) (list (make-posn (invader-x I1) (invader-y I1)) (make-posn (invader-x I2) (invader-y I2)) (make-posn 0 0)))

;(define (invader-images loi) empty) ;stub

(define (invader-posns loi)
  (cond [(empty? loi) (list (make-posn 0 0))]
        [else
         (append
          (list (make-posn (invader-x (first loi)) (invader-y (first loi))))
          (invader-posns (rest loi)))]))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfMissiles -> ListOfPosns
;; produce a list of posns, there should be one posn for each Missile in ListOfMissiles
;; empty lists and end of lists should produce a (make-posn 0 0)
(check-expect (missile-posns (game-missiles G0)) (list (make-posn 0 0)))
(check-expect (missile-posns (game-missiles G3)) (list (make-posn (missile-x M1) (missile-y M1)) (make-posn (missile-x M2) (missile-y M2)) (make-posn 0 0)))

;(define (missile-images lom) empty) ;stub

(define (missile-posns lom)
  (cond [(empty? lom) (list (make-posn 0 0))]
        [else
         (append
          (list (make-posn (missile-x (first lom)) (missile-y (first lom))))
          (missile-posns (rest lom)))]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; invader-landed? and helper -> stop game if invader has reached the bottom of the screen
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game -> Boolean
;; produce true if an Invader has reached the bottom of the MTS (landed)
;; pass ListOfInvades to check-invader-height to check each individual Invader
(check-expect (invader-landed? G0) false)
(check-expect (invader-landed? G2) false)
(check-expect (invader-landed? G3) true)

;(define (invader-landed? g) false) ;stub

(define (invader-landed? g) (check-invader-height (game-invaders g)))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ListOfInvaders -> Boolean
;; profuce true if an Invader height is greater than INVADER-LANDED-Y (bottom of INVADER just touching the bottom of the screen)
(check-expect (check-invader-height (game-invaders G0)) false)
(check-expect (check-invader-height (game-invaders G2)) false)
(check-expect (check-invader-height (game-invaders G3)) true)

; (define (check-invader-height loi) false);

(define (check-invader-height loi)
  (cond [(empty? loi) false]
        [(>= (invader-y (first loi)) INVADER-LANDED-Y) true]
        [else (check-invader-height (rest loi))]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; handle-key function and helpers -> update tank position based on arrow keys, fire missiles with space key
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Game KeyEvent -> Game
;; produce a new game updated based on key presses (space, arrow keys)
(check-expect (handle-key G0 "left")  (make-game empty empty (make-tank (- (tank-x T0) TANK-SPEED) -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank (+ (tank-x T0) TANK-SPEED)  1)))
(check-expect (handle-key G0 " ")     (make-game empty (list (make-missile 150 468.5)) T0             ))
(check-expect (handle-key G0 "a")     G0)

;(define (handle-key g ke) (make-game empty empty T0)) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "left")  (make-game (game-invaders g) (game-missiles g) (move-tank (game-tank g) -1))]                                                     ;tank moves left
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (move-tank (game-tank g)  1))]                                                     ;tank moves right
        [(key=? ke " ") (make-game (game-invaders g) (append (game-missiles g) (list (make-missile (tank-x (game-tank g)) MISSILE-LAUNCH-Y))) (game-tank g))] ;missile fired
        [else g]))                                                                                                                                            ;all other keys don't change game state

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Tank Integer -> Tank
;; produce new tank with updated position based on Integer (-1 -> move left, 1-> move right)
;; check to see if the tank is at the boundary and trying to move in that direction by passing the Tank to tank-at-bound?
;; if true return the unchanged Tank object (tank can't move beyond screen boundary)
;; if false return the updated Tank object with new position
(check-expect (move-tank (make-tank (/ (image-width TANK) 2) -1) -1) (make-tank (/ (image-width TANK) 2) -1))
(check-expect (move-tank T0 -1) (make-tank 148 -1))
(check-expect (move-tank T0  1) (make-tank 152  1))

;(define (move-tank g dir) (make-tank 0 -1)) ;stub

(define (move-tank t dir)
  (if (tank-at-bound? t dir)
      t
      (if (> dir 0)
          (make-tank (+ (tank-x t) TANK-SPEED)  1)
          (make-tank (- (tank-x t) TANK-SPEED) -1))))

;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Tank Integer -> Boolean
;; produce true if the tank is against either screen boundary and the direction of travel is towards
;; the boundary it is against
(check-expect (tank-at-bound? T0 1) false)
(check-expect (tank-at-bound? (make-tank (- WIDTH (/ (image-width TANK) 2)) TANK-Y)  1) true)
(check-expect (tank-at-bound? (make-tank (+ 0     (/ (image-width TANK) 2)) TANK-Y) -1) true)

;(define (tank-at-bound? T0 1) false) ;stub

(define (tank-at-bound? t dir)
  (cond [(and (= dir -1) (<= (tank-x t)          (/ (image-width TANK) 2))) true]
        [(and (= dir  1) (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))) true]
        [else false]))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@