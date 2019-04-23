(define move-left
  (lambda (st)
    (if (equal? (modulo (state-blank st)
                        (sqrt (vector-length (state-board st))))
                0)
        null
        (list (cons 'l
                    (state (vector-copy-swap (state-board st)
                                             (- (state-blank st) 1)
                                             (state-blank st))
                           (- (state-blank st) 1)))))))

(define move-up
  (lambda (st)
    (if (< (state-blank st)
           (sqrt (vector-length (state-board st))))
        null
        (list (cons 'u
                    (state (vector-copy-swap (state-board st)
                                             (- (state-blank st)
                                                (sqrt (vector-length (state-board st))))
                                             (state-blank st))
                           (- (state-blank st)
                              (sqrt (vector-length (state-board st))))))))))

(define move-right
  (lambda (st)
    (if (equal? (modulo (state-blank st)
                        (sqrt (vector-length (state-board st))))
                (- (sqrt (vector-length (state-board st))) 1))
        null
        (list (cons 'r
                    (state (vector-copy-swap (state-board st)
                                             (state-blank st)
                                             (+ (state-blank st) 1))
                           (+ (state-blank st) 1)))))))

(define move-down
  (lambda (st)
    (if (>= (state-blank st)
            (- (vector-length (state-board st))
               (sqrt (vector-length (state-board st)))))
        null
        (list (cons 'd
                    (state (vector-copy-swap (state-board st)
                                             (state-blank st)
                                             (+ (state-blank st)
                                                (sqrt (vector-length (state-board st)))))
                           (+ (state-blank st)
                              (sqrt (vector-length (state-board st))))))))))