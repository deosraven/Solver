#lang racket

(require data/heap)

;un node du graphe de recherce composé d'un état, de son node parent et de la direction prise pour l'atteinde, null pour l'état initial.
(struct node (state parent symbol))

(provide rp-solve)
(provide rp-solve-heuristic)

; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle,
; (rp-solve s adj acc-state?) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés par ordre de longueur. 
(define (rp-solve s adj acc-state?)
  (rp-solve-acc (list (node s #f #f)) adj acc-state?))

(define (rp-solve-acc queue adj acc-state?)
  (if (null? queue)
      (lambda () '())
      (let ((current (car queue)))
        (if (acc-state? (node-state current))
            (lambda () (cons (build-solution current)
                             (rp-solve-acc (cdr queue) adj acc-state?)))
            (rp-solve-acc (append (cdr queue)
                                  (map (lambda (pair) (node (cdr pair) current (car pair)))
                                       ; Filter out cyclic states
                                       (filter-map (lambda (pair) (and (not (is-cyclic-state current (cdr pair))) pair))
                                                   (adj (node-state current)))))
                          adj
                          acc-state?)))))

(define (is-cyclic-state parent state)
  (cond [(not parent) #f]
        [(equal? (node-state parent) state) #t]
        [else (is-cyclic-state (node-parent parent) state)]))

(define (build-solution node)
  (if (not (node-parent node)) '()
      (append (build-solution (node-parent node))
              (list (node-symbol node)))))


; Si `s` est l'état initial d'un puzzle régulier, `adj` est sa fonction
; d'adjacence, `acc-state?` est le predicat accepteur de ce puzzle et
; `heuristic` est une fonction d'heuristic pour ce puzzle,
; (rp-solve s adj acc-state? heuristic) est un itérateur paresseux de solutions
; du puzzle. Les mots du langage sont donnés dans l'ordre de l'heuristique
(define (rp-solve-heuristic s adj acc-state? heuristic)
  (let ((queue (make-priority)))
    (priority-insert! queue (node s #f #f) (heuristic s))
    (rp-solve-heuristic-acc queue adj acc-state? heuristic)))

(define (rp-solve-heuristic-acc queue adj acc-state? heuristic)
  (if (priority-empty? queue)
      (lambda () '())
      (let ((current (priority-pop! queue)))
        (if (acc-state? (node-state current))
            (lambda () (cons (build-solution current)
                             (rp-solve-heuristic-acc queue adj acc-state? heuristic)))
            (begin
              ; Add every new node with an acyclic state to the priority queue
              (map (lambda (pair)
                     (priority-insert! queue
                                       (node (cdr pair) current (car pair))
                                       (heuristic (cdr pair))))
                   (filter-map (lambda (pair) (and (not (is-cyclic-state current (cdr pair))) pair))
                               (adj (node-state current))))

              ; Recursive call with the completed priority queue
              (rp-solve-heuristic-acc queue
                                      adj
                                      acc-state?
                                      heuristic))))))


;; Priority queue based on heap
(define (make-priority)
  (make-heap (lambda (x y) (<= (second x) (second y)))))

(define (priority-insert! queue x priority) 
  (heap-add! queue (list x priority)))
 
(define (priority-pop! queue)
  (begin0 
    (first (heap-min queue))
    (heap-remove-min! queue)))

(define (priority-empty? queue)
  (= 0 (heap-count queue)))