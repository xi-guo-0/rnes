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

(struct instruction
        ([mnemonic : Symbol] [len : Integer] [cycles : Integer] [addressing-mode : Symbol]))

(define instructions
  (hash #x00
        (instruction 'BRK 1 7 'NoneAddressing)
        #xaa
        (instruction 'TAX 1 2 'NoneAddressing)
        #xa9
        (instruction 'LDA 2 2 'Immediate)
        #xa5
        (instruction 'LDA 2 3 'ZeroPage)
        #xb5
        (instruction 'LDA 2 4 'ZeroPage_X)
        #xad
        (instruction 'LDA 3 4 'Absolute)))

(: get-operand-address (-> registers (Instance Memory%) Symbol Integer))
(define (get-operand-address r a-memory mode)
  (match mode
    ['Immediate (registers-pc r)]
    ['ZeroPage (send a-memory read (registers-pc r))]
    ['ZeroPage_X (+ (send a-memory read (registers-pc r)) (registers-x r))]
    ['ZeroPage_Y (+ (send a-memory read (registers-pc r)) (registers-y r))]
    ['Absolute (send a-memory read-u16 (registers-pc r))]
    ['Absolute_X (+ (send a-memory read-u16 (registers-pc r)) (registers-x r))]
    ['Absolute_Y (+ (send a-memory read-u16 (registers-pc r)) (registers-y r))]
    ['Indirect_X
     (let* ([base (+ (send a-memory read (registers-pc r)) (registers-x r))]
            [lo (send a-memory read base)]
            [hi (send a-memory read (add1 base))])
       (bitwise-ior (arithmetic-shift hi 8) lo))]
    ['Indirect_Y
     (let* ([base (send a-memory read (registers-pc r))]
            [lo (send a-memory read base)]
            [hi (send a-memory read (add1 base))]
            [deref_base (bitwise-ior (arithmetic-shift hi 8) lo)])
       (+ deref_base (registers-y r)))]
    [else (error "The addressing mode is unsupported")]))

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

(: lda (-> registers (Instance Memory%) Symbol Void))
(define (lda r a-memory mode)
  (let ([val (send a-memory read (get-operand-address r a-memory mode))])
    (begin
      (set-registers-a! r val)
      (update-zero-negative-flags r (registers-a r)))))

(: tax (-> registers Void))
(define (tax r)
  (begin
    (set-registers-x! r (registers-a r))
    (update-zero-negative-flags r (registers-x r))))

(: interpret (-> registers (Instance Memory%) Void))
(define (interpret r a-memory)
  (begin
    (set-registers-pc! r (send a-memory read-u16 #xfffc))
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
                     (send a-memory read (registers-pc r))])
                (if (hash-has-key? instructions opcode)
                    (let* ([a-instruction (hash-ref instructions opcode)]
                           [mnemonic (instruction-mnemonic a-instruction)]
                           [len (instruction-len a-instruction)]
                           [cycles (instruction-cycles a-instruction)]
                           [addressing-mode (instruction-addressing-mode a-instruction)])

                      (set-registers-pc! r (add1 (registers-pc r)))
                      (begin
                        (match mnemonic
                          ['LDA (lda r a-memory addressing-mode)]
                          ['TAX (tax r)]
                          ['BRK (break (void))])
                        (set-registers-pc! r (+ (registers-pc r) (sub1 len)))))
                    (error "no such insturction"))
                (loop))))))

(module+ test
  (require typed/rackunit)
  (test-case "LDA Immediate"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([a-memory (new memory%)])
                   (send a-memory load #x8000 (bytes #xa9 #x15 #x00))
                   (send a-memory write-u16 #xfffc #x8000)
                   a-memory))
      (check-equal? #x15 (registers-a r))
      (check-equal? #f (registers-z r))))
  (test-case "LDA from memory"
    (let ([r (create-cpu)])
      (interpret r
                 (let ([a-memory (new memory%)])
                   (send a-memory write #x10 #x55)
                   (send a-memory load #x8000 (bytes #xa5 #x10 #x00))
                   (send a-memory write-u16 #xfffc #x8000)
                   a-memory))
      (check-equal? #x55 (registers-a r)))))
