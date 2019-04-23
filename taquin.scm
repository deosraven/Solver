#lang racket

(provide taquin-make-state)
(provide taquin-adj-states)
(provide taquin-acc-state?)
(provide taquin-heuristic)

; Etats du taquin comprenant un vecteur représentant le board, l'index de la case vide dans ce vecteur et la taille d'un côté du taquin (N pour taquin N*N)
(struct state (board blank size) #:transparent)

; La fonction taquin-make-state prend en entrée une liste de N
; listes. La i-ème sous-liste représente la i-ème ligne du taquin, en
; commençant à compter depuis le haut. Les nombres sont numérotés à partir de 1
; et le trou est symbolisé par le symbole 'x.
; La fonction renvoie la représentation de l'état correspondant.
(define (taquin-make-state lls)
  (let ((flat (flatten lls)))
    (state (list->vector flat)
           (taquin-find-blank flat 0)
           (sqrt (length flat)))))

; Si `list` est une liste de taille N*N représentant un jeu de taquin (et contenant donc les nombres de 1 à (N*N)-1 et un 'x représentant une case vide)
; et `position` vaut 0 alors (taquin-find-blank list position) renvoie l'index de la case vide dans list.
(define (taquin-find-blank list position)
  (if (or (null? list)
          (eq? (car list) 'x))
      position
      (taquin-find-blank (cdr list) (+ 1 position))))         
    
; Si `st` est la représentation d'un état, (taquin-adj-states st)
; renvoie une liste de paire pointées, dont le car est le symbole menant
; à l'état représenté par le cdr.
(define (taquin-adj-states st)
  (append (move-left st)
          (move-up st)
          (move-right st)
          (move-down st)))

; (taquin-acc-state? st) renvoie vrai si et seulement si `st` est la
; représentation de l'état accepteur.
(define (taquin-acc-state? state)
  (let ((board (state-board state)))
    (equal? board (list->vector (taquin-solution 1 (vector-length board))))))

; Si `size` est le nombre de case d'un jeu de taquin (= N*N) et `current` vaut 0 alors (taquin-solution current size) renvoie une liste de taille N*N comprenant dans l'ordre les nombres 1 à (N*N)-1
; puis x et correspondant au jeu de taquin résolu.
(define taquin-solution
  (lambda (current size)
    (if (= current size)
        '(x)
        (cons current (taquin-solution (+ 1 current) size)))))

; Si `st` est la représentation d'un état, (taquin-heuristic st)
; renvoie le nombre d'éléménts mal placés par rapport à la solution
(define taquin-heuristic
  (lambda (st)
    (count-different (vector->list (state-board st))
                     (taquin-solution 1 (vector-length (state-board st))))))

; Si `left` et `right` sont deux listes de tailles identiques alors (count different left right) renvoie le nombre de différences entre les deux listes.
(define count-different
  (lambda (left right)
    (if (or (null? left) (null? right))
        0
        (+ (if (equal? (car left) (car right)) 0 1) (count-different (cdr left) (cdr right))))))

; H2 - distance de manhattan
; (define taquin-heuristic
;   (lambda (state)
;     'TODO))

; Si `state` est la représentation d'un état, (taquin-heuristic state)
; renvoie le nombre d'éléménts mal placés par rapport à la solution
(define (taquin-heuristic-wrong-place state)
  (count-different (vector->list (state-board state))
                   (taquin-solution 1 (vector-length (state-board state)))))

(define (count-different left right)
  (if (or (null? left) (null? right))
      0
      (+ (if (equal? (car left) (car right)) 0 1) (count-different (cdr left) (cdr right)))))

(define (taquin-heuristic state)
  (taquin-heuristic-list (state-size state)
                         0
                         (vector->list (state-board state))))

(define (taquin-heuristic-list size index list)
  (if (or (null? list) (equal? (car list) 'x)) 0
      (+ (taquin-heuristic-list size (+ 1 index) (cdr list))
         ; right is the coordinates of the index where the current value should be
         ; current is the coordinates of the current index
         (let ((right (index-to-coordinates size (- (car list) 1)))
               (current (index-to-coordinates size index)))
           (+ (abs (- (car right)
                      (car current)))
              (abs (- (cdr right)
                      (cdr current))))))))

(define (index-to-coordinates size index)
  (cons (floor (/ index size)) (modulo index size)))  

(define (move st direction condition pos)
  (if condition
      (list (cons direction
                  (state (vector-copy-swap (state-board st)
                                           (state-blank st)
                                           (+ (state-blank st) pos))
                         (+ (state-blank st) pos)
                         (state-size st))))
      null))

; Si `st` est un état, `direction` est une des directions de déplacement possible, `condition` est une condition correspondant aux restriction de mouvement
; d'une case vide en fonction de `direction` et `pos` est le modificateur de position permettant en l'ajoutant à la position de la case vide de la déplacer dans `direction` 
; alors (move st direction condition pos) renvoie une paire pointée dont le car est la direction et le cdr est l'état obtenu en déplacant la case vide dans cette direction.
(define move
  (lambda (st direction condition pos)
    (if condition
        (list (cons direction
                    (state (vector-copy-swap (state-board st)
                                             (state-blank st)
                                             (+ (state-blank st) pos))
                           (+ (state-blank st) pos)
                           (state-size st))))
        null)))

; Si `st` est un état alors (move-left st) renvoie une paire pointée dont le car est 'l et le cdr est le nouvel état obtenu en déplacant la case vide vers la gauche.  La fonction renvoie null si c'est impossible.
(define move-left
  (lambda (st)
    (move st
          'l
          (not (equal? (modulo (state-blank st) (state-size st)) 0))
          -1)))

; Si `st` est un état alors (move-up st) renvoie une paire pointée dont le car est 'u et le cdr est le nouvel état obtenu en déplacant la case vide vers le haut.  La fonction renvoie null si c'est impossible.
(define move-up
  (lambda (st)
    (move st
          'u
          (>= (state-blank st) (state-size st))
          (- (state-size st)))))

; Si `st` est un état alors (move-right st) renvoie une paire pointée dont le car est 'r et le cdr est le nouvel état obtenu en déplacant la case vide vers la droite.  La fonction renvoie null si c'est impossible.
(define move-right
  (lambda (st)
    (move st
          'r
          (not (equal? (modulo (state-blank st)
                               (state-size st))
                       (- (state-size st) 1)))
          1)))

; Si `st` est un état alors (move-down st) renvoie une paire pointée dont le car est 'd et le cdr est le nouvel état obtenu en déplacant la case vide vers le bas.  La fonction renvoie null si c'est impossible.
(define move-down
  (lambda (st)
    (move st
          'd
          (< (state-blank st)
             (- (vector-length (state-board st))
                (state-size st)))
          (state-size st))))

; Si `v` est un vecteur, `pos1` est un un index dans ce vecteur et `pos2` est un index dans ce vecteur, alors (vector-copy-swap v pos1 pos2) renvoie 
(define vector-copy-swap
  (lambda (v pos1 pos2)
    (let ((copy (vector-copy v)))
      (vector-swap copy pos1 pos2)
      copy)))

(define vector-swap
  (lambda (v pos1 pos2)
    (let ((temp (vector-ref v pos1)))
      (vector-set*! v pos1 (vector-ref v pos2) pos2 temp))))
