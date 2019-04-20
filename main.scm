#lang racket

(require "solver.scm")
(require "taquin.scm")

; Si `iterator` est un iterateur paresseux et `n` est un naturel,
; (n-solutions iterator n) renvoie la liste des `n` premiers itérés de
; `iterator`
(define n-solutions
  (lambda (iterator n)
    (if (zero? n) '()
        (let ((pair (iterator)))
          (if (null? pair) '()
              (cons (car pair) (n-solutions (cdr pair) (- n 1))))))))

(define taquin-init-state (taquin-make-state '((2 3 6) (1 x 5) (7 8 4))))

(n-solutions (rp-solve taquin-init-state taquin-adj-states taquin-acc-state?) 1)
;(n-solutions (rp-solve-heuristic taquin-init-state taquin-adj-states taquin-acc-state? taquin-heuristic) 1)
