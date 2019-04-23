#lang racket

(provide taquin-make-state)
(provide taquin-adj-states)
(provide taquin-acc-state?)
;(provide taquin-heuristic)

; Etats du taquin comprenant un vecteur représentant le board et l'index de la case vide dans ce vecteur 
(struct state (board blank))

; La fonction taquin-make-state prend en entrée une liste de N
; listes. La i-ème sous-liste représente la i-ème ligne du taquin, en
; commençant à compter depuis le haut. Les nombres sont numérotés à partir de 1
; et le trou est symbolisé par le symbole 'x.
; La fonction renvoie la représentation de l'état correspondant.
; Peut probablement être optimisé pour passer de O(2n) à O(n) en faisant flatten + find-blank en une opération
; Prog défensive ?
(define taquin-make-state
  (lambda (lls)
    (let ((flat (flatten lls)))
      (state (list->vector flat)
                    (taquin-find-blank flat 0)))))

(define taquin-find-blank
  (lambda (list position)
    (if (or (null? list)
            (eq? (car list) 'x))
        position
        (taquin-find-blank (cdr list) (+ 1 position)))))        

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x))
                           (flatten (cdr x))))
        (else (list x))))
    
; Si `state` est la représentation d'un état, (taquin-adj-states state)
; renvoie une liste de paire pointées, dont le car est le symbole menant
; à l'état représenté par le cdr.
(define taquin-adj-states
  (lambda (st)
    (append null
      (list (let ((left (move-left st))) 
        (if left 
          (cons 'l left)
          null))) 
      (list (let ((up (move-up st))) 
        (if up 
          (cons 'u up) 
          null))) 
      (list (let ((right (move-right st))) 
        (if right
          (cons 'r right) 
          null))) 
      (list (let ((down (move-down st))) 
        (if down 
          (cons 'd down) 
          null))))))

; (taquin-acc-state? state) renvoie vrai si et seulement si `state` est la
; représentation de l'état accepteur
(define taquin-acc-state?
  (lambda (state)
    (let ((board (state-board state)))
      (equal? board (list->vector (taquin-solution 1 (vector-length board)))))))

(define taquin-solution
  (lambda (current size)
    (if (= current size)
        '(x)
        (cons current (taquin-solution (+ 1 current) size)))))

; Si `state` est la représentation d'un état, (taquin-heuristic state)
; renvoie...
; (define taquin-heuristic
;   (lambda (state)
;     'TODO))

(define move-left
  (lambda (st)
    (if (equal? (modulo (state-blank st)
                        (sqrt (vector-length (state-board st))))
                0)
        #f
        (state (vector-copy-swap (state-board st)
                                 (- (state-blank st) 1)
                                 (state-blank st))
               (- (state-blank st) 1)))))

(define move-up
  (lambda (st)
    (if (< (state-blank st) (sqrt (vector-length (state-board st)))) #f
      (state (vector-copy-swap (state-board st) (- (state-blank st) (sqrt (vector-length (state-board st)))) (state-blank st)) (- (state-blank st) (sqrt (vector-length (state-board st))))))))

(define move-right
  (lambda (st)
    (if (equal? (modulo (state-blank st) (sqrt (vector-length (state-board st)))) (- (sqrt (vector-length (state-board st))) 1)) #f
      (state (vector-copy-swap (state-board st) (state-blank st) (+ (state-blank st) 1)) (+ (state-blank st) 1)))))

(define move-down
  (lambda (st)
    (if (>= (state-blank st) (- (vector-length (state-board st)) (sqrt (vector-length (state-board st))))) #f
      (state (vector-copy-swap (state-board st) (state-blank st) (+ (state-blank st) (sqrt (vector-length (state-board st))))) (+ (state-blank st) (sqrt (vector-length (state-board st))))))))

(define vector-copy-swap
  (lambda (v pos1 pos2)
    (let ((copy (vector-copy v)))
      (vector-swap copy pos1 pos2)
      copy)))

(define vector-swap
  (lambda (v pos1 pos2)
    (let ((temp (vector-ref v pos1)))
      (vector-set*! v pos1 (vector-ref v pos2) pos2 temp))))
