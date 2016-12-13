#lang racket
(require "automata.rkt" "population.rkt" "scan.rkt" "inout.rkt" plot)
(provide (all-defined-out))
(plot-new-window? #t)
(define N 1000)
(define CYCLES 400)
(define SPEED 100)
(define DELTA .95)
(define ROUNDS 400)

(define (build-population l m h a)
  (define p
    (append
     (build-list l (lambda (_) (lows)))
     (build-list m (lambda (_) (mediums)))
     (build-list h (lambda (_) (highs)))
     (build-list a (lambda (_) (accommodator)))))
  (list->vector (shuffle p)))

; l m h a
(define point-list
  (list
   (list 100 50 50 800)
   (list 50 50 850 50)
   (list 50 150 700 100)
   (list 50 250 600 100)
   (list 50 350 500 100)
   (list 50 450 400 100)
   (list 50 550 300 100)
   (list 50 650 200 100)
   (list 50 750 100 100)

   ))

(define (evolve-rd population cycles speed rounds delta)
  (cond
   [(zero? cycles) '()]
   [else (define p2 (match-population population rounds delta))
         (define pp (population-payoffs p2))
         (define p3 (regenerate p2 speed))
         (cons (scan-mediums-highs p3)
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
        #:x-label "mediums" #:y-label "highs" #:title "(l) m h a"))

;; passed one shot rd

(define (main)
  (collect-garbage)
  (test1s point-list)
  (plot-dynamics file-list))
