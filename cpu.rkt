#lang racket
(require "memory.rkt")

(provide (struct-out cpu)
         (all-defined-out))
(struct cpu (a x y pc s c z i d b v n) #:mutable)
(define (create-cpu)
  (cpu 0 0 0 0 0 #f #f #f #f #f #f #f))

(define (update-zero-flag a-cpu val)
  (set-cpu-z! a-cpu (zero? val)))

(define (update-negative-flag a-cpu val)
  (set-cpu-n! a-cpu (not (zero? (bitwise-and #b10000000 val)))))

(define (update-zero-negative-flags a-cpu val)
  (begin
    (update-zero-flag a-cpu val)
    (update-negative-flag a-cpu val)))

(define (lda a-cpu val)
  (begin
    (set-cpu-a! a-cpu val)
    (update-zero-negative-flags a-cpu (cpu-a a-cpu))))

(define (tax a-cpu)
  (begin
    (set-cpu-x! (cpu-a a-cpu))
    (update-zero-negative-flags a-cpu (cpu-x a-cpu))))

(define (interpret a-cpu a-memory)
  (begin
    (set-cpu-pc! a-cpu 0)
    (let/ec break
            (let loop ()
              (let ([opcode (send a-memory read (cpu-pc a-cpu))])
                (set-cpu-pc! a-cpu (+ (cpu-pc a-cpu) 1))
                (match opcode
                  [#xa9
                   (let ([param (send a-memory read (cpu-pc a-cpu))])
                     (set-cpu-pc! a-cpu (+ (cpu-pc a-cpu) 1))
                     (lda a-cpu param))]
                  [#xaa (tax a-cpu)]
                  [#x00 (break)]
                  [else (error "crash!")])
                (loop))))))

(module+ test
  (require rackunit)
  (test-case "LDA Immediate"
    (let ([a-cpu (create-cpu)])
      (interpret a-cpu (let ([a-memory (new memory%)])
                         (send a-memory load #x0 (bytes #xa9 #x15 #x00))
                         a-memory))
      (check-equal? #x15 (cpu-a a-cpu))
      (check-equal? #f (cpu-z a-cpu)))))
