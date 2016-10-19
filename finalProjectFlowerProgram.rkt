(require 2htdp/image)
(require 2htdp/universe)

;;Flower drawing program

;; Constants
;; ==========

(define WIDTH 400)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT "lightblue"))

(define CENTER (circle 15 "solid" "lightyellow"))
(define PETAL (ellipse 30 50 "solid" "purple"))
(define FLOWER
  (overlay (above CENTER(rectangle 1 10 0 "white"))
           (overlay/align "center" "top" (above (beside (rotate 216 PETAL) (rectangle 1 1 0 "white")(rotate 144 PETAL))
                                                (rotate 180 (beside (rotate 72 PETAL)(rectangle 10 0 0 "white")(rotate 288 PETAL))))
                          (above (rectangle 1 61 0 "white") PETAL))))

;; Data Defintions
;; ===============

(define-struct flower (x y size))
;; Flower is (make-flower Integer Integer Natural)
;; interp. a flower with x position, y position, and a side length in pixels
(define FLOWER0 (make-flower 0 0 0))
(define FLOWER1 (make-flower (/ WIDTH 2) (/ HEIGHT 2) 15))

#;
(define (fn-for-flower)
  (... (flower-x f)        ;Integer
       (flower-y f )        ;Integer
       (flower-size f )))   ;Natural

;; Template rules used:
;;  - compound: 3 fields

;; Functions
;; =========


;;Flower -> Flower
;; Start Animation. it starts with (main(make-flower (/ WIDTH 2) (/ HEIGHT 2) 15)))
;; produces n times 2

(define (main f)
  (big-bang f
            (on-tick            tock) ; Flower -> Flower
            (to-draw          render) ;Flower -> Image
            (on-mouse handle-mouse))) ;Flower Integer Interger MouseEvent - > Flower


;; Flower -> Flower
;; adds 1 to the size of the flower every clock tick
(check-expect (tock (make-flower 0 0 5)) (make-flower 0 0 6))
(check-expect (tock (make-flower 20 30 19)) (make-flower 20 30 20))
(check-expect (tock (make-flower 30 40 49)) (make-flower 30 40 50))


;; (define (tock f) f)
;;  <took template from Flower>
(define (tock f)
  (make-flower (flower-x f)        ;Integer
               (flower-y f)        ;Integer
               (add1 (flower-size f))))   ;Natural

;; Flower -> Image
;; renders the flower of the given size of the MTS at the correct x and y coordinates
(check-expect (render (make-flower 10 10 20)) (place-image(rotate 20 (scale (/ 20 100) FLOWER))
                                                          10 10 MTS))
(check-expect (render (make-flower 20 20 0)) (place-image empty-image
                                                          20 20 MTS))
(check-expect (render (make-flower 10 10 370)) (place-image(rotate 10 (scale (/ 370 100) FLOWER))
                                                          10 10 MTS))

;; (define (render f) MTS)
;;  <took template from Flower>
(define (render f)
  (place-image (if (zero? (flower-size f))
                          empty-image
                          (rotate (remainder (flower-size f) 360) (scale (/ (flower-size f) 100) FLOWER)))
                          (flower-x f) 
                          (flower-y f)       
                          MTS ))  


;; Flower Integer Interger MouseEvent -> Flower
;; replaces the flower with a new one of size 0 at the mouse click

(check-expect (handle-mouse  (make-flower 0 0 0 ) 5 5 "button-down") (make-flower 5 5 0 ))
(check-expect (handle-mouse  (make-flower 0 0 0 ) 5 5 "button-up") (make-flower 0 0 0 ))
(check-expect (handle-mouse  (make-flower 50 50 55 ) 10 10 "button-down") (make-flower 10 10 0 ))


;; (define (handle-mouse f x y me) f)
;;  <took template from Flower>
(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down")(make-flower x y 0)]
        [else f]))
