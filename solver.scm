#lang racket

;un node du graphe de recherce composé d'un état, de son node parent et de la direction prise pour l'atteinde, null pour l'état initial.
(struct node (state parent symbol))

(provide rp-solve)
;(provide rp-solve-heuristic)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle,
; (rp-solve s adj acc-state?) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés par ordre de longueur. 
(define rp-solve
  (lambda (s adj acc-state?)
    (rp-solve-acc (list (node s #f #f)) adj acc-state?)))

(define rp-solve-acc
  (lambda (queue adj acc-state?)
    (let ((current (car queue)))
      (cond
        [(null? queue) (lambda () '())]
        [(acc-state? (node-state current))
         (lambda () (cons (build-solution current)
                          (rp-solve-acc (cdr queue) adj acc-state?)))]
        [else (rp-solve-acc (append (cdr queue)
                                   (map (lambda (pair) (node (cdr pair) current (car pair)))
                                        (filter-map (lambda (pair) (and (not (is-cyclic-state current (cdr pair))) pair))
                                                    (adj (node-state current)))))
                           adj
                           acc-state?)]))))

(define is-cyclic-state
  (lambda (parent state)
    (cond [(not parent) #f]
          [(equal? (node-state parent) state) #t]
          [else (is-cyclic-state (node-parent parent) state)])))

(define build-solution
  (lambda (node)
    (if (node-parent node)
        (append (build-solution (node-parent node))
                (list (node-symbol node)))
        '())))
