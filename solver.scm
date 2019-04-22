#lang racket

(struct node (state parent visited))

(provide rp-solve)
;(provide rp-solve-heuristic)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle,
; (rp-solve s adj acc-state?) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés par ordre de longueur. 
(define rp-solve
  (lambda (s adj acc-state?)
    'TODO))

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle et
; `heuristic` est une fonction d'heuristic pour ce puzzle,
; (rp-solve s adj acc-state? heuristic) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés dans l'ordre de l'heuristique
; (define rp-solve-heuristic
;   (lambda (s adj acc-state? heuristic)
;     'TODO))
