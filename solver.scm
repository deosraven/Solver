#lang racket

; Pour la priority queue basée sur les heap utilisée dans `rp-solve-heuristic`
(require data/heap)

; Structure représentant un node du graphe de recherce composée
; d'un état,
; de son node parent 
; et du symbole permettant d'atteinde cet état par la fonction de transition (#f pour l'état initial).
(struct node (state parent symbol))

(provide rp-solve)
(provide rp-solve-heuristic)

; Si `s` est l'état initial d'un puzzle régulier
; et si `adj` est sa fonction d'adjacence
; et si `acc-state?` est le predicat accepteur de ce puzzle,
; alors (rp-solve s adj acc-state?) est un itérateur paresseux de solutions du puzzle.
; Les mots du langage sont donnés par ordre de longueur. 
(define (rp-solve s adj acc-state?)
  (rp-solve-acc (list (node s #f #f)) adj acc-state?))

; Si `queue` est une liste contenant des `node`
; et si `adj` est la fonction d'adjacence du puzzle
; et si `acc-state?` est le prédicat accepteur du puzzle,
; alors (rp-solve s adj acc-state?) est un itérateur paresseux de solutions du puzzle.
; Les mots du langage sont donnés par ordre de longueur. 
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

; Si `parent` est un node
; et si `st` est un état de puzzle
; alors (is-cyclic-state parent st) retourne #t si et seulement si cet état particulier s'est déjà produit dans un `node` parent.
(define (is-cyclic-state parent st)
  (cond [(not parent) #f]
        [(equal? (node-state parent) st) #t]
        [else (is-cyclic-state (node-parent parent) st)]))

; Si `node` est une instance de la structure `node`
; alors (build-solution node) retourne une liste de symbole représentant le mot permettant
; de passer de l'état initial à l'état actuel de `node` (dans notre utilisation: l'état accepteur).
(define (build-solution node)
  (if (not (node-parent node)) '()
      (append (build-solution (node-parent node))
              (list (node-symbol node)))))

; Si `s` est l'état initial d'un puzzle régulier
; et si `adj` est sa fonction d'adjacence
; et si `acc-state?` est le predicat accepteur de ce puzzle
; et si `heuristic` est une fonction d'heuristic pour ce puzzle,
; alors (rp-solve s adj acc-state? heuristic) est un itérateur paresseux de solutions du puzzle.
; Les mots du langage sont donnés dans l'ordre de l'heuristique
(define (rp-solve-heuristic s adj acc-state? heuristic)
  (let ((queue (make-priority)))
    (priority-insert! queue (node s #f #f) (heuristic s))
    (rp-solve-heuristic-acc queue adj acc-state? heuristic)))

; Si `queue` est une liste contenant des `node`
; et si `adj` est sa fonction d'adjacence
; et si `acc-state?` est le predicat accepteur de ce puzzle
; et si `heuristic` est une fonction d'heuristic pour ce puzzle,
; alors (rp-solve s adj acc-state? heuristic) est un itérateur paresseux de solutions du puzzle.
; Les mots du langage sont donnés dans l'ordre de l'heuristique
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

; Retourne une priority queue basée sur les heaps, dont la priorité est exprimée
; à l'aide d'un entier naturel. Les plus petits entiers sont prioritaires.
(define (make-priority)
  (make-heap (lambda (x y) (<= (second x) (second y)))))

; Si `queue` est une priority queue
; et si `data` est une donnée quelconque
; et si `priority` est un entier naturel,
; alors  (priority-insert! queue data priority) ajoute `data` à `queue` avec une priorité `priority`.
(define (priority-insert! queue data priority)
  (heap-add! queue (list data priority)))
 
; Si `queue` est une priority queue
; alors (priority-pop! queue) renvoie l'élément prioritaire de `queue` et le supprime de celle-ci.
(define (priority-pop! queue)
  (begin0 
    (first (heap-min queue))
    (heap-remove-min! queue)))

; Si `queue` est une priority queue
; alors (priority-empty? queue) renvoie #t si la `queue` est vide, #f sinon
(define (priority-empty? queue)
  (= 0 (heap-count queue)))
