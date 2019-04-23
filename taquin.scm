#lang racket

(provide taquin-make-state)
(provide taquin-adj-states)
(provide taquin-acc-state?)
;(provide taquin-heuristic)

(struct state (board blank))

; La fonction taquin-make-state prend en entrée une liste de N
; listes. La i-ème sous-liste représente la i-ème ligne du taquin, en
; commençant à compter depuis le haut. Les nombres sont numérotés à partir de 1
; et le trou est symbolisé par le symbole 'x.
; La fonction renvoie la représentation de l'état correspondant.
; Peut probablement être optimisé pour passer de O(2n) à O(n) en faisant flatten + find-blank en une opération
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
  (lambda (state)
    'TODO))

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

; > (move-left (taquin-make-state '((1 2 3) (4 x 5) (6 7 8))))
; '#(1 2 3 x 4 5 6 7 8)

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

; (let* ((v (list->vector '(1 2 3 4 x 5 6 7 8))) (c (vector-copy v))) (vector-swap c 2 5) c)

(define vector-copy-swap
  (lambda (v pos1 pos2)
    (let ((copy (vector-copy v)))
      (vector-swap copy pos1 pos2)
      copy)))

(define vector-swap
  (lambda (v pos1 pos2)
    (let ((temp (vector-ref v pos1)))
      (vector-set*! v pos1 (vector-ref v pos2) pos2 temp))))

; > (let ((v (list->vector '(1 2 3 4 x 5 6 7 8)))) (cons (vector-copy-swap v 2 5) v))
; '(#(1 2 5 4 x 3 6 7 8) . #(1 2 3 4 x 5 6 7 8))

; Ce que j'ai actuellement :
; > (let ((x (taquin-make-state '((1 2 3) (4 x 5) (6 7 8))))) (state-board x))
; '#(1 2 3 4 x 5 6 7 8)
; > (let ((x (taquin-make-state '((1 2 3) (4 x 5) (6 7 8))))) (state-blank x))
; 4
