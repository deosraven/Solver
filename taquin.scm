#lang racket

(provide taquin-make-state)
(provide taquin-adj-states)
(provide taquin-acc-state?)
(provide taquin-heuristic)

; État du taquin comprenant un vecteur représentant le board, l'index de la case vide dans ce vecteur et la taille d'un côté du taquin (N pour taquin N*N)
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

; Si `list` est une liste de symboles
; et si `position` est un entier naturel décrivant le nombre d'éléments déjà traités,
; alors (taquin-find-blank list position) renvoie l'index du premier symbole `x` placé après `position` dans `list`.
(define (taquin-find-blank list position)
  (if (or (null? list)
          (eq? (car list) 'x))
      position
      (taquin-find-blank (cdr list) (+ 1 position))))         
    
; Si `st` est un `state`,
; alors (taquin-adj-states st) renvoie une liste de paires pointées, dont
; le car est le symbole menant à l'état représenté par le cdr.
(define (taquin-adj-states st)
  (append (move-left st)
          (move-up st)
          (move-right st)
          (move-down st)))

; Si `st` est un `state`,
; alors (taquin-acc-state? st) renvoie vrai si et seulement si `st` est la
; représentation de l'état accepteur.
(define (taquin-acc-state? st)
  (= (taquin-count-wrong 1 (vector->list (state-board st))) 0))

; Si `st` est un `state`,
; alors (taquin-heuristic-wrong-place st)
; renvoie le nombre d'éléménts mal placés par rapport à l'état accepteur
; Cette fonction n'est ni utilisée ni exportée, mais implémente H1
(define (taquin-heuristic-wrong-place st)
  (taquin-count-wrong 1 (vector->list (state-board st))))

; Si `value` vaut 1
; et si `list` est une liste correspondant à un état du taquin
; alors (taquin-count-wrong value list) renvoie le nombre d'éléments mal placés par rapport à la solution
(define (taquin-count-wrong value list)
  (cond [(null? list) 0]
        [(equal? (car list) 'x) (if (= (length list) 1) 0 1)]
        [else (+ (if (equal? (car list) value) 0 1)
                 (taquin-count-wrong (+ value 1) (cdr list)))]))

; Si `st` est un `state` du taquin,
; alors (taquin-heuristic st) renvoie la somme des distances de Manhattan entre chaque case et sa position dans l'état accepteur.
; H2
(define (taquin-heuristic st)
  (taquin-heuristic-list (state-size st)
                         0
                         (vector->list (state-board st))))

; Si `size` est la taille d'un côté d'un board de taquin
; et si `index` est un entier naturel décrivant le nombre d'éléments déjà traités
; et si `list` est une liste d'éléments du tableau de taquin restants à traiter,
; alors (taquin-heuristic-list size index list) renvoie la somme des distances de Manhattan
; entre chaque case contenue dans `list` et sa position dans l'état accepteur.
(define (taquin-heuristic-list size index list)
  (if (null? list) 0
      (+ (taquin-heuristic-list size (+ 1 index) (cdr list))
         ; right is the coordinates of the index where the current value should be
         ; current is the coordinates of the current index
         (let ((right (index-to-coordinates size (- (if (equal? (car list) 'x)
                                                     (* size size)
                                                     (car list)) 1)))
               (current (index-to-coordinates size index)))
           (+ (abs (- (car right)
                      (car current)))
              (abs (- (cdr right)
                      (cdr current))))))))

; Si `size` est la taille d'un côté du jeu de taquin
; et si `index` est un entier naturel décrivant un index dans le vecteur d'état du jeu,
; alors (index-to-coordinates size index) renvoie une paire pointée dont le car est l'index
; de la ligne et le cdr est l'index de la colonne de la représentation matricielle du terrain de jeu
(define (index-to-coordinates size index)
  (cons (floor (/ index size)) (modulo index size)))  

; Si `st` est un `state`
; et si `direction` est un symbole représentant une direction de déplacement possible
; et si `condition` est un booléen représentant les restrictions de mouvement en fonction de la `direction` (par exemple,
; on ne peut pas se déplacer vers la gauche si nous nous situons sur le bord gauche du jeu)
; et si `pos` est un entier qui exprime la différence d'index entre l'élément à déplacer et sa nouvelle position
; alors (move st direction condition pos) renvoie une paire pointée dont le car est un symbole représentant la direction
; et le cdr est le `state` obtenu en déplaçant la case vide dans cette direction.
(define (move st direction condition pos)
  (if condition
      (list (cons direction
                  (state (vector-copy-swap (state-board st)
                                           (state-blank st)
                                           (+ (state-blank st) pos))
                         (+ (state-blank st) pos)
                         (state-size st))))
      null))

; Si `st` est un `state`
; alors (move-left st) renvoie une liste contenant une paire pointée dont le car est 'l et le cdr est le nouvel état obtenu en déplacant la case vide vers la gauche.
; La fonction renvoie null si le déplacement est invalide.
(define (move-left st)
  (move st
        'l
        (not (equal? (modulo (state-blank st) (state-size st)) 0))
        -1))

; Si `st` est un `state`
; alors (move-up st) renvoie une liste contenant une paire pointée dont le car est 'u et le cdr est le nouvel état obtenu en déplacant la case vide vers le haut.
; La fonction renvoie null si le déplacement est invalide.
(define (move-up st)
  (move st
        'u
        (>= (state-blank st) (state-size st))
        (- (state-size st))))

; Si `st` est un `state`
; alors (move-right st) renvoie une liste contenant une paire pointée dont le car est 'r et le cdr est le nouvel état obtenu en déplacant la case vide vers la droite.
; La fonction renvoie null si le déplacement est invalide.
(define (move-right st)
  (move st
        'r
        (not (equal? (modulo (state-blank st)
                             (state-size st))
                     (- (state-size st) 1)))
        1))

; Si `st` est un `state` 
; alors (move-down st) renvoie une liste contenant une paire pointée dont le car est 'd et le cdr est le nouvel état obtenu en déplacant la case vide vers le bas.
; La fonction renvoie null si le déplacement est invalide.
(define (move-down st)
  (move st
        'd
        (< (state-blank st)
           (- (vector-length (state-board st))
              (state-size st)))
        (state-size st)))

; Si `v` est un vecteur
; et si `pos1` et `pos2` sont deux entiers naturels représentant des index dans ce vecteur
; alors (vector-swap v pos1 pos2) échange les valeurs aux positions `pos1` et `pos2` dans `v`.
(define (vector-swap v pos1 pos2)
  (let ((temp (vector-ref v pos1)))
    (vector-set*! v pos1 (vector-ref v pos2) pos2 temp)))

; Si `v` est un vecteur
; et si `pos1` et `pos2` sont deux entiers naturels représentant des index dans ce vecteur
; alors (vector-copy-swap v pos1 pos2) renvoie une copie du vecteur `v` dont les éléments en positions `pos1` et `pos2` ont été échangés.
(define (vector-copy-swap v pos1 pos2)
  (let ((c (vector-copy v)))
    (vector-swap c pos1 pos2)
    c))
