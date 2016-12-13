#lang racket
(require "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt" plot)
(provide (all-defined-out))
(plot-new-window? #t)
(define N 1000)
(define CYCLES 400)
(define SPEED 100)
(define DELTA .95)
(define ROUNDS 1)

(define (build-population l m h)
  (define p
    (append
     (build-list l (lambda (_) (lows)))
     (build-list m (lambda (_) (mediums)))
     (build-list h (lambda (_) (highs)))))
  (list->vector (shuffle p)))

; one shot rd
(define point-list
  (list
   (list 50 50 900)
   (list 50 100 850)
   (list 50 150 800)
   ;(list 50 200 750)
   (list 50 250 700)
   ;(list 50 300 650)
   ;(list 50 350 500)
   ;(list 50 400 550)
   ;(list 50 450 500)

   (list 800 50 150)
   (list 900 50 50)

   (list 300 300 400)
   (list 500 400 100)
   (list 700 200 100)

   ))

(define (evolve-rd population cycles speed rounds delta)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-population population rounds delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-lows-mediums p3)
               (evolve-rd p3 (- cycles 1) speed rounds delta))]))

(define (test1 test-point file-name)
  (collect-garbage)
  (define A (apply build-population test-point))
  (define rd-types
    (time (evolve-rd A CYCLES SPEED ROUNDS DELTA)))
  (out-data file-name (map list (flatten rd-types)))
  (define rd (lines rd-types))
  (plot rd #:x-min 0.0 #:x-max N #:y-min 0 #:y-max N #:title "rd"))

(define (test1s test-points)
  (for ([i (length test-points)])
    (test1 (list-ref test-points i)
           (string-append "rd" (number->string i)))))

(define file-list
  (list "rd0" "rd1" "rd2" "rd3" "rd4" "rd5" "rd6" "rd7" "rd8"))

(define (plot-dynamics file-list)
  (define data (load-dynamics file-list))
  (plot data
        #:x-max N #:y-max N
        #:x-label "lows" #:y-label "mediums" #:title "one shot rd"))

;; passed one shot rd

(define (main)
  (collect-garbage)
  (test1s point-list)
  (plot-dynamics file-list))
