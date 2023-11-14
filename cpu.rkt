#lang racket
(require "memory.rkt")

(provide (struct-out cpu)
         (all-defined-out))
(struct cpu (a x y pc s c z i d b v n) #:mutable)
(define (create-cpu)
  (cpu 0 0 0 0 0 #f #f #f #f #f #f #f))

(define (get-operand-address a-cpu a-memory mode)
  (match mode
    ['Immediate (cpu-pc a-cpu)]
    ['ZeroPage (send a-memory read (cpu-pc a-cpu))]
    ['ZeroPage_X (+ (send a-memory read (cpu-pc a-cpu)) (cpu-x a-cpu))]
    ['ZeroPage_Y (+ (send a-memory read (cpu-pc a-cpu)) (cpu-y a-cpu))]
    ['Absolute (send a-memory read-u16 (cpu-pc a-cpu))]
    ['Absolute_X (+ (send a-memory read-u16 (cpu-pc a-cpu)) (cpu-x a-cpu))]
    ['Absolute_Y (+ (send a-memory read-u16 (cpu-pc a-cpu)) (cpu-y a-cpu))]
    ['Indirect_X
     (let* ([base (+ (send a-memory read (cpu-pc a-cpu)) (cpu-x a-cpu))]
            [lo (send a-memory read base)]
            [hi (send a-memory read (add1 base))])
       (bitwise-ior (arithmetic-shift hi 8) lo))]
    ['Indirect_Y
     (let* ([base (send a-memory read (cpu-pc a-cpu))]
            [lo (send a-memory read base)]
            [hi (send a-memory read (add1 base))]
            [deref_base (bitwise-ior (arithmetic-shift hi 8) lo)])
       (+ deref_base (cpu-y a-cpu)))]
    [else (error "The addressing mode is unsupported")]))

(define (update-zero-flag a-cpu val)
  (set-cpu-z! a-cpu (zero? val)))

(define (update-negative-flag a-cpu val)
  (set-cpu-n! a-cpu (not (zero? (bitwise-and #b10000000 val)))))

(define (update-zero-negative-flags a-cpu val)
  (begin
    (update-zero-flag a-cpu val)
    (update-negative-flag a-cpu val)))

(define (lda a-cpu a-memory mode)
  (let ([val (send a-memory read (get-operand-address a-cpu a-memory mode))])
    (begin
      (set-cpu-a! a-cpu val)
      (update-zero-negative-flags a-cpu (cpu-a a-cpu)))))

(define (tax a-cpu)
  (begin
    (set-cpu-x! (cpu-a a-cpu))
    (update-zero-negative-flags a-cpu (cpu-x a-cpu))))

(define (interpret a-cpu a-memory)
  (begin
    (set-cpu-pc! a-cpu (send a-memory read-u16 #xfffc))
    (let/ec break
            (let loop ()
              (let ([opcode (send a-memory read (cpu-pc a-cpu))])
                (set-cpu-pc! a-cpu (add1 (cpu-pc a-cpu)))
                (match opcode
                  [#xa9
                   (begin
                     (lda a-cpu a-memory 'Immediate)
                     (set-cpu-pc! a-cpu (add1 (cpu-pc a-cpu))))]
                  [#xa5
                   (begin
                     (lda a-cpu a-memory 'ZeroPage)
                     (set-cpu-pc! a-cpu (add1 (cpu-pc a-cpu))))]
                  [#xad
                   (begin
                     (lda a-cpu a-memory 'Absolute)
                     (set-cpu-pc! a-cpu (+ (cpu-pc a-cpu) 2)))]
                  [#xaa (tax a-cpu)]
                  [#x00 (break)]
                  [else (error "crash!")])
                (loop))))))

(module+ test
  (require rackunit)
  (test-case "LDA Immediate"
    (let ([a-cpu (create-cpu)])
      (interpret a-cpu
                 (let ([a-memory (new memory%)])
                   (send a-memory load #x8000 (bytes #xa9 #x15 #x00))
                   (send a-memory write-u16 #xfffc #x8000)
                   a-memory))
      (check-equal? #x15 (cpu-a a-cpu))
      (check-equal? #f (cpu-z a-cpu))))
  (test-case "LDA from memory"
    (let ([a-cpu (create-cpu)])
      (interpret a-cpu
                 (let ([a-memory (new memory%)])
                   (send a-memory write #x10 #x55)
                   (send a-memory load #x8000 (bytes #xa5 #x10 #x00))
                   (send a-memory write-u16 #xfffc #x8000)
                   a-memory))
      (check-equal? #x55 (cpu-a a-cpu)))))
