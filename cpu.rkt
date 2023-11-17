#lang typed/racket
(require "memory.rkt")

(provide (struct-out registers)
         (all-defined-out))
(struct registers
        ([a : Integer] [x : Integer]
                       [y : Integer]
                       [pc : Integer]
                       [s : Integer]
                       [c : Boolean]
                       [z : Boolean]
                       [i : Boolean]
                       [d : Boolean]
                       [b : Boolean]
                       [v : Boolean]
                       [n : Boolean])
  #:mutable)

(: create-cpu (-> registers))
(define (create-cpu)
  (registers 0 0 0 0 0 #f #f #f #f #f #f #f))

(define-type Addressing (-> registers (Instance Memory%) Integer))

(: get-operand-accumulator Addressing)
(define (get-operand-accumulator r m)
  (registers-a r))

(: get-operand-immediate Addressing)
(define (get-operand-immediate r m)
  (send m read (add1 (registers-pc r))))

(: get-operand-immediate Addressing)
(define get-operand-relative get-operand-immediate)

(: get-operand-zeropage Addressing)
(define (get-operand-zeropage r m)
  (send m read (get-operand-immediate r m)))

(: get-operand-zeropage-x Addressing)
(define (get-operand-zeropage-x r m)
  (send m read (+ (get-operand-immediate r m) (registers-x r))))

(: get-operand-zeropage-y Addressing)
(define (get-operand-zeropage-y r m)
  (send m read (+ (get-operand-immediate r m) (registers-y r))))

(: read-u16-after-pc Addressing)
(define (read-u16-after-pc r m)
  (send m read-u16 (add1 (registers-pc r))))

(: get-operand-absolute Addressing)
(define (get-operand-absolute r m)
  (send m read (read-u16-after-pc r m)))

(: get-operand-absolute-x Addressing)
(define (get-operand-absolute-x r m)
  (send m read (+ (read-u16-after-pc r m) (registers-x r))))

(: get-operand-absolute-y Addressing)
(define (get-operand-absolute-y r m)
  (send m read (+ (read-u16-after-pc r m) (registers-y r))))

(: get-operand-indirect-x Addressing)
(define (get-operand-indirect-x r m)
  (send m
        read
        (let* ([base (+ (get-operand-immediate r m) (registers-x r))]
               [lo (send m read base)]
               [hi (send m read (add1 base))])
          (bitwise-ior (arithmetic-shift hi 8) lo))))

(: get-operand-indirect-y Addressing)
(define (get-operand-indirect-y r m)
  (send m
        read
        (let* ([base (get-operand-immediate r m)]
               [lo (send m read base)]
               [hi (send m read (add1 base))]
               [deref-base (bitwise-ior (arithmetic-shift hi 8) lo)])
          (+ deref-base (registers-y r)))))

(: get-operand-implied Addressing)
(define (get-operand-implied r m)
  0)

(struct instruction
        ([mnemonic : Symbol] [len : Integer] [cycles : Integer] [addressing : Addressing]))

(define instructions
  (hash #x69
        (instruction 'ADC 2 2 get-operand-immediate)
        #x65
        (instruction 'ADC 2 3 get-operand-zeropage)
        #x75
        (instruction 'ADC 2 4 get-operand-zeropage-x)
        #x6d
        (instruction 'ADC 3 4 get-operand-absolute)
        #x7d
        (instruction 'ADC 3 4 get-operand-absolute-x)
        #x79
        (instruction 'ADC 3 4 get-operand-absolute-y)
        #x61
        (instruction 'ADC 2 6 get-operand-indirect-x)
        #x71
        (instruction 'ADC 2 5 get-operand-indirect-y)
        #x29
        (instruction 'AND 2 2 get-operand-immediate)
        #x25
        (instruction 'AND 2 3 get-operand-zeropage)
        #x35
        (instruction 'AND 2 4 get-operand-zeropage-x)
        #x2d
        (instruction 'AND 3 4 get-operand-absolute)
        #x3d
        (instruction 'AND 3 4 get-operand-absolute-x)
        #x39
        (instruction 'AND 3 4 get-operand-absolute-y)
        #x21
        (instruction 'AND 2 6 get-operand-indirect-x)
        #x31
        (instruction 'AND 2 5 get-operand-indirect-y)
        #x0a
        (instruction 'ASL 1 2 get-operand-accumulator)
        #x06
        (instruction 'ASL 2 5 get-operand-zeropage)
        #x16
        (instruction 'ASL 2 6 get-operand-zeropage-x)
        #x0e
        (instruction 'ASL 3 6 get-operand-absolute)
        #x1e
        (instruction 'ASL 3 7 get-operand-absolute-x)
        #x90
        (instruction 'BCC 2 2 get-operand-relative)
        #xb0
        (instruction 'BCS 2 2 get-operand-relative)
        #xf0
        (instruction 'BEQ 2 2 get-operand-relative)
        #x24
        (instruction 'BIT 2 3 get-operand-zeropage)
        #x2c
        (instruction 'BIT 3 4 get-operand-absolute)
        #x00
        (instruction 'BRK 1 7 get-operand-implied)
        #xaa
        (instruction 'TAX 1 2 get-operand-implied)
        #xa9
        (instruction 'LDA 2 2 get-operand-immediate)
        #xa5
        (instruction 'LDA 2 3 get-operand-zeropage)
        #xb5
        (instruction 'LDA 2 4 get-operand-zeropage-x)
        #xad
        (instruction 'LDA 3 4 get-operand-absolute)))

(: update-zero-flag (-> registers Integer Void))
(define (update-zero-flag r val)
  (set-registers-z! r (zero? val)))

(: update-negative-flag (-> registers Integer Void))
(define (update-negative-flag r val)
  (set-registers-n! r (not (zero? (bitwise-and #b10000000 val)))))

(: update-zero-negative-flags (-> registers Integer Void))
(define (update-zero-negative-flags r val)
  (begin
    (update-zero-flag r val)
    (update-negative-flag r val)))

(: set-registers-a-update! (-> registers Integer Void))
(define (set-registers-a-update! r val)
  (begin
    (set-registers-a! r val)
    (update-zero-negative-flags r (registers-a r))))

(: adc (-> registers Integer Void))
(define (adc r val)
  (let* ([s (+ (registers-a r) val (if (registers-c r) 1 0))]
         [c (> s #xff)]
         [a (bitwise-and s #xff)]
         [v (not (zero? (bitwise-and (bitwise-xor val a) (bitwise-xor (registers-a r) a) #x80)))])
    (begin
      (set-registers-c! r c)
      (set-registers-v! r v)
      (set-registers-a-update! r a))))

(: instruction/and (-> registers Integer Void))
(define (instruction/and r val)
  (set-registers-a-update! r (bitwise-and (registers-a r) val)))

(: asl (-> registers Integer Void))
(define (asl r val)
  (let* ([v (arithmetic-shift val 1)] [c (> v #xff)] [a (bitwise-and v #xff)])
    (begin
      (set-registers-c! r c)
      (set-registers-a-update! r a))))

(: branch (-> registers Integer Boolean Void))
(define (branch r val condition)
  (if condition (set-registers-pc! r (- (+ (registers-pc r) val) 2)) (void)))

(: bcc (-> registers Integer Void))
(define (bcc r val)
  (branch r val (not (registers-c r))))

(: bcs (-> registers Integer Void))
(define (bcs r val)
  (branch r val (registers-c r)))

(: beq (-> registers Integer Void))
(define (beq r val)
  (branch r val (registers-z r)))

(: instruction/bit (-> registers Integer Void))
(define (instruction/bit r val)
  (let ([res (bitwise-and (registers-a r) val)])
    (begin
      (set-registers-z! r (zero? res))
      (set-registers-n! r (not (zero? (bitwise-and #b10000000 val))))
      (set-registers-v! r (not (zero? (bitwise-and #b01000000 val)))))))

(: lda (-> registers Integer Void))
(define (lda r val)
  (begin
    (set-registers-a! r val)
    (update-zero-negative-flags r (registers-a r))))

(: tax (-> registers Void))
(define (tax r)
  (begin
    (set-registers-x! r (registers-a r))
    (update-zero-negative-flags r (registers-x r))))

(: interpret (-> registers (Instance Memory%) Void))
(define (interpret r m)
  (begin
    (set-registers-pc! r (send m read-u16 #xfffc))
    (let/ec break
            :
            Void
            (let loop
              :
              Void
              ()
              (let ([opcode
                     :
                     Integer
                     (send m read (registers-pc r))])
                (if (hash-has-key? instructions opcode)
                    (let* ([a-instruction (hash-ref instructions opcode)]
                           [mnemonic (instruction-mnemonic a-instruction)]
                           [len (instruction-len a-instruction)]
                           [cycles (instruction-cycles a-instruction)]
                           [addressing (instruction-addressing a-instruction)]
                           [val (addressing r m)])
                      (begin
                        (match mnemonic
                          ['ADC (adc r val)]
                          ['AND (instruction/and r val)]
                          ['ASL (asl r val)]
                          ['BCC (bcc r val)]
                          ['BCS (bcs r val)]
                          ['BEQ (beq r val)]
                          ['BIT (instruction/bit r val)]
                          ['LDA (lda r val)]
                          ['TAX (tax r)]
                          ['BRK (break (void))])
                        (set-registers-pc! r (+ (registers-pc r) len))))
                    (error "no such insturction"))
                (loop))))))

(module+ test
  (require typed/rackunit)
  (test-case "ASL Accumulator"
    (let ([r (create-cpu)])
      (set-registers-a! r #xfe)
      (interpret r
                 (let ([m (new memory%)])
                   (send m load #x8000 (bytes #x0a #x00))
                   m))
      (check-equal? (registers-a r) #xfc)
      (check-equal? #t (registers-c r))
      (check-equal? #t (registers-n r))))
  (test-case "ADC Immediate"
    (let ([r (create-cpu)])
      (set-registers-a! r #x02)
      (interpret r
                 (let ([m (new memory%)])
                   (send m load #x8000 (bytes #x69 #x03 #x00))
                   m))
      (check-equal? (registers-a r) #x05)
      (check-equal? #f (registers-z r))
      (check-equal? #f (registers-n r))))
  (test-case "ADC Immediate negative"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([m (new memory%)])
                   (send m load #x8000 (bytes #x69 #xf3 #x00))
                   m))
      (check-equal? #xf3 (registers-a r))
      (check-equal? #f (registers-z r))
      (check-equal? #t (registers-n r))))
  (test-case "ADC ZeroPage"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([m (new memory%)])
                   (send m write #xf3 #x37)
                   (send m load #x8000 (bytes #x65 #xf3 #x00))
                   m))
      (check-equal? #x37 (registers-a r))
      (check-equal? #f (registers-z r))
      (check-equal? #f (registers-n r))))
  (test-case "BCS"
    (let ([r (create-cpu)])
      (set-registers-c! r #t)
      (interpret r
                 (let ([m (new memory%)])
                   (send m load #x8000 (bytes #xb0 #x4 #x00))
                   m))
      (check-equal? (registers-pc r) #x8004)))

  (test-case "LDA Immediate"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([m (new memory%)])
                   (send m load #x8000 (bytes #xa9 #x15 #x00))
                   m))
      (check-equal? #x15 (registers-a r))
      (check-equal? #f (registers-z r))))
  (test-case "LDA from memory"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([m (new memory%)])
                   (send m write #x10 #x55)
                   (send m load #x8000 (bytes #xa5 #x10 #x00))
                   m))
      (check-equal? #x55 (registers-a r)))))
