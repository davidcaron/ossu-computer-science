;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname final-project-space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ============
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 3)
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define TOUCHING 7)

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-WIDTH (image-width TANK))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; ============
;; Data Definitions:

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


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ============
;; Functions

;; Tank -> Tank
;; move tank by its speed, limiting to WIDTH

(check-expect (move-tank T0) (make-tank (+ TANK-SPEED (/ WIDTH 2)) 1))
(check-expect (move-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (move-tank (make-tank 0 -1)) (make-tank 0 -1))


(define (move-tank t)
  (if (= (tank-dir t) 1)
      (if (>= (+ TANK-SPEED (tank-x t) (/ TANK-WIDTH 2)) WIDTH)
          (make-tank (tank-x t) (tank-dir t))
          (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))
      (if (<= (- (tank-x t) (+ TANK-SPEED (/ TANK-WIDTH 2))) 0)
          (make-tank (tank-x t) (tank-dir t))
          (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))))
  

;; Missile -> Missile
;; move missile by its speed

(check-expect (move-missile (make-missile 150 300)) (make-missile 150 (- 300 MISSILE-SPEED)))

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfMissiles -> ListOfMissiles
;; move a list of missiles

(check-expect (move-missiles (list (make-missile 150 300))) (list (move-missile (make-missile 150 300))))

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                    (move-missiles (rest lom)))]))
      

;; Game -> Game
;; start the game; call with (main (make-game empty empty (make-tank (/ WIDTH 2) 1)))
;;

#;
(define (main game)
  (big-bang game                 ; Game
    (on-tick   tock)     ; Game -> Game
    (to-draw   render)   ; Game -> Image
    (stop-when ...)      ; Game -> Boolean
    (on-mouse  ...)      ; Game Integer Integer MouseEvent -> Game
    (on-key    ...)))    ; Game KeyEvent -> Game

(define (main game)
  (big-bang game                 ; Game
    (on-tick   tock)     ; Game -> Game
    (to-draw   render)
    (on-key    handle-key)))


;; Game KeyEvent -> Game
;; handle keyboard events

(define (handle-key game ke)
  (cond [(key=? ke " ") (make-game (game-invaders game)
                                   (cons (missile-fire (game-tank game)) (game-missiles game))
                                   (game-tank game))]
        [(key=? ke "left") (make-game (game-invaders game)
                                      (game-missiles game)
                                      (make-tank (tank-x (game-tank game)) -1))]
        [(key=? ke "right") (make-game (game-invaders game)
                                       (game-missiles game)
                                       (make-tank (tank-x (game-tank game)) 1))]
        [else game]))


;; Tank -> Missile
;; Fire missile

(check-expect (missile-fire T0) (make-missile (/ WIDTH 2) TANK-Y))

(define (missile-fire tank)
  (make-missile (tank-x tank) TANK-Y))

;; ListOfInvaders -> ListOfInvaders
;; Generate invaders randomly

(define (make-invaders loi)
  (if (= 0 (random 30))
      (cons (make-invader (+ 15 (random (- WIDTH 30))) 0 (- (random 10) 5)) loi)
      loi))

;; ListOfInvaders -> ListOfInvaders
;; Move invaders

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (move-invaders (rest loi)))]))

;; Invader -> Invader
;; move an invader

(define (move-invader i)
  (if (or
       (>= (+ (invader-x i) (invader-dx i)) (- WIDTH 10))
       (<= (+ (invader-x i) (invader-dx i)) 10))
      (make-invader (invader-x i) (invader-y i) (* -1 (invader-dx i)))
      (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))))

;; ListOfMissiles ListOfInvaders -> ListOfInvaders
;; destroy invaders touched by missiles

(check-expect (kill-invaders (list M1 M2) (list I1)) empty)
(check-expect (kill-invaders (list M1) (list I1)) (list I1))


(define (kill-invaders lom loi)
  (cond [(empty? loi) empty]
        [(kill-invader lom (first loi)) (kill-invaders lom (rest loi))]
        [else (cons (first loi) (kill-invaders lom (rest loi)))]
        ))

;; ListOfMissiles Invader -> Boolean
;; Detect if invader is touched by any missile

(define (kill-invader lom i)
  (cond [(empty? lom) false]
        [(missile-touches? (first lom) i) true]
        [else (kill-invader (rest lom) i)]))

;; Missile, Invader -> Boolean
;; Detect if invader is touched by missile

(check-expect (missile-touches? M1 I1) false)
(check-expect (missile-touches? M2 I1) true)

(define (missile-touches? m i)
  (and (>= (- (missile-y m) 10) (- (invader-y i) TOUCHING))
       (<= (- (missile-y m) 10) (+ (invader-y i) TOUCHING))
       (>= (missile-x m) (- (invader-x i) TOUCHING))
       (<= (missile-x m) (+ (invader-x i) TOUCHING))))

;; Game -> Game
;; everyone moves
(define (tock game)
  (if (game-over? (game-invaders game))
      game
      (make-game (move-invaders (kill-invaders (game-missiles game) (make-invaders (game-invaders game))))
                 (move-missiles (game-missiles game))
                 (move-tank (game-tank game)))))

;; ListOfInvaders -> Boolean
;; Detects if game is over ( Are there any invaders at the bottom?)

(check-expect (game-over? (list I1)) false)
(check-expect (game-over? (list (make-invader (/ WIDTH 2) HEIGHT 12))) true)

(define (game-over? loi)
  (cond [(empty? loi) false]
        [(>= (invader-y (first loi)) (- HEIGHT 8)) true]
        [else (game-over? (rest loi))]))

;; ListOfMissiles, Image -> Image
;; place missiles on background

(define (place-missiles lom background)
  (cond [(empty? lom) background]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (place-missiles (rest lom) background))]))


;; ListOfInvaders, Image -> Image
;; place invaders on background

(define (place-invaders loi background)
  (cond [(empty? loi) background]
        [else (place-image INVADER
                           (invader-x (first loi))
                           (invader-y (first loi))
                           (place-invaders (rest loi) background))]))

;; Game, Image -> Image
;; Place tank on background

(define (place-tank game background)
  (place-image TANK (tank-x (game-tank game)) TANK-Y background))

;; Game -> Image
;; render everything
(define (render game)
  (place-tank game
              (place-missiles (game-missiles game)
                              (place-invaders (game-invaders game) BACKGROUND))))

(main (make-game empty empty (make-tank (/ WIDTH 2) 1)))

