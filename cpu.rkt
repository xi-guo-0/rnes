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

(define stack-base #x0100)
(define stack-init-val #xfd)

(: create-cpu (-> registers))
(define (create-cpu)
  (registers 0 0 0 stack-init-val 0 #f #f #f #f #f #f #f))

(: read-u16-after-pc Addressing)
(define (read-u16-after-pc r m)
  (send m read-u16 (add1 (registers-pc r))))

(: stack-push (-> registers (Instance Memory%) Integer Void))
(define (stack-push r m val)
  (begin
    (send m write (+ stack-base (registers-s r)) val)
    (set-registers-s! r (sub1 (registers-s r)))))

(: stack-push-u16 (-> registers (Instance Memory%) Integer Void))
(define (stack-push-u16 r m val)
  (let ([hi (arithmetic-shift val -8)] [lo (bitwise-and val #xff)])
    (stack-push r m hi)
    (stack-push r m lo)))

(: stack-pop (-> registers (Instance Memory%) Integer))
(define (stack-pop r m)
  (begin
    (set-registers-s! r (add1 (registers-s r)))
    (send m read (+ stack-base (registers-s r)))))

(: stack-pop-u16 (-> registers (Instance Memory%) Integer))
(define (stack-pop-u16 r m)
  (let ([lo (stack-pop r m)] [hi (stack-pop r m)]) (bitwise-ior (arithmetic-shift hi 8) lo)))

(define-type Addressing (-> registers (Instance Memory%) Integer))

(: get-address-immediate Addressing)
(define (get-address-immediate r m)
  (add1 (registers-pc r)))

(: get-address-zeropage Addressing)
(define (get-address-zeropage r m)
  (get-operand-immediate r m))

(: get-address-zeropage-x Addressing)
(define (get-address-zeropage-x r m)
  (+ (get-operand-immediate r m) (registers-x r)))

(: get-address-zeropage-y Addressing)
(define (get-address-zeropage-y r m)
  (+ (get-operand-immediate r m) (registers-y r)))

(: get-address-absolute Addressing)
(define (get-address-absolute r m)
  (read-u16-after-pc r m))

(: get-address-absolute-x Addressing)
(define (get-address-absolute-x r m)
  (+ (read-u16-after-pc r m) (registers-x r)))

(: get-address-absolute-y Addressing)
(define (get-address-absolute-y r m)
  (+ (read-u16-after-pc r m) (registers-y r)))

(: get-operand-accumulator Addressing)
(define (get-operand-accumulator r m)
  (registers-a r))

(: get-operand-immediate Addressing)
(define (get-operand-immediate r m)
  (send m read (get-address-immediate r m)))

(: get-operand-relative Addressing)
(define get-operand-relative get-operand-immediate)

(: get-operand-zeropage Addressing)
(define (get-operand-zeropage r m)
  (send m read (get-address-zeropage r m)))

(: get-operand-zeropage-x Addressing)
(define (get-operand-zeropage-x r m)
  (send m read (get-address-zeropage-x r m)))

(: get-operand-zeropage-y Addressing)
(define (get-operand-zeropage-y r m)
  (send m read (get-address-zeropage-y r m)))

(: get-operand-absolute Addressing)
(define (get-operand-absolute r m)
  (send m read (get-address-absolute r m)))

(: get-operand-absolute-x Addressing)
(define (get-operand-absolute-x r m)
  (send m read (get-address-absolute-x r m)))

(: get-operand-absolute-y Addressing)
(define (get-operand-absolute-y r m)
  (send m read (get-address-absolute-y r m)))

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
        #x30
        (instruction 'BMI 2 2 get-operand-relative)
        #xd0
        (instruction 'BNE 2 2 get-operand-relative)
        #x10
        (instruction 'BPL 2 2 get-operand-relative)
        #x00
        (instruction 'BRK 1 7 get-operand-implied)
        #x50
        (instruction 'BVC 2 2 get-operand-relative)
        #x70
        (instruction 'BVS 2 2 get-operand-relative)
        #x18
        (instruction 'CLC 1 2 get-operand-implied)
        #xd8
        (instruction 'CLD 1 2 get-operand-implied)
        #x58
        (instruction 'CLI 1 2 get-operand-implied)
        #xb8
        (instruction 'CLV 1 2 get-operand-implied)
        #xc9
        (instruction 'CMP 2 2 get-operand-immediate)
        #xc5
        (instruction 'CMP 2 3 get-operand-zeropage)
        #xd5
        (instruction 'CMP 2 4 get-operand-zeropage-x)
        #xcd
        (instruction 'CMP 3 4 get-operand-absolute)
        #xdd
        (instruction 'CMP 3 4 get-operand-absolute-x)
        #xd9
        (instruction 'CMP 3 4 get-operand-absolute-y)
        #xc1
        (instruction 'CMP 2 6 get-operand-indirect-x)
        #xd1
        (instruction 'CMP 2 5 get-operand-indirect-y)
        #xe0
        (instruction 'CPX 2 2 get-operand-immediate)
        #xe4
        (instruction 'CPX 2 3 get-operand-zeropage)
        #xec
        (instruction 'CPX 3 4 get-operand-absolute)
        #xc0
        (instruction 'CPY 2 2 get-operand-immediate)
        #xc4
        (instruction 'CPY 2 3 get-operand-zeropage)
        #xcc
        (instruction 'CPY 3 4 get-operand-absolute)
        #xc6
        (instruction 'DEC 2 5 get-address-zeropage)
        #xd6
        (instruction 'DEC 2 6 get-address-zeropage-x)
        #xce
        (instruction 'DEC 3 6 get-address-absolute)
        #xde
        (instruction 'DEC 3 7 get-address-absolute-x)
        #xca
        (instruction 'DEX 1 2 get-operand-implied)
        #x88
        (instruction 'DEY 1 2 get-operand-implied)
        #x49
        (instruction 'EOR 2 2 get-operand-immediate)
        #x45
        (instruction 'EOR 2 3 get-operand-zeropage)
        #x55
        (instruction 'EOR 2 4 get-operand-zeropage-x)
        #x4d
        (instruction 'EOR 3 4 get-operand-absolute)
        #x5d
        (instruction 'EOR 3 4 get-operand-absolute-x)
        #x59
        (instruction 'EOR 3 4 get-operand-absolute-y)
        #x41
        (instruction 'EOR 2 6 get-operand-indirect-x)
        #x51
        (instruction 'EOR 2 5 get-operand-indirect-y)
        #xe6
        (instruction 'INC 2 5 get-operand-zeropage)
        #xf6
        (instruction 'INC 2 6 get-operand-zeropage-x)
        #xee
        (instruction 'INC 3 6 get-operand-absolute)
        #xfe
        (instruction 'INC 3 7 get-operand-absolute-x)
        #xe8
        (instruction 'INX 1 2 get-operand-implied)
        #xc8
        (instruction 'INY 1 2 get-operand-implied)
        #x4c
        (instruction 'JMP 3 3 get-operand-absolute)
        #x6c
        (instruction 'JMP 3 5 get-operand-absolute)
        #x20
        (instruction 'JSR 3 6 get-operand-absolute)
        #xaa
        (instruction 'TAX 1 2 get-operand-implied)
        #xa9
        (instruction 'LDA 2 2 get-operand-immediate)
        #xa5
        (instruction 'LDA 2 3 get-operand-zeropage)
        #xb5
        (instruction 'LDA 2 4 get-operand-zeropage-x)
        #xad
        (instruction 'LDA 3 4 get-operand-absolute)
        #xa2
        (instruction 'LDX 2 2 get-operand-immediate)
        #xa6
        (instruction 'LDX 2 3 get-operand-zeropage)
        #xb6
        (instruction 'LDX 2 4 get-operand-zeropage-y)
        #xae
        (instruction 'LDX 3 4 get-operand-absolute)
        #xbe
        (instruction 'LDX 3 4 get-operand-absolute-y)
        #xa0
        (instruction 'LDY 2 2 get-operand-immediate)
        #xa4
        (instruction 'LDY 2 3 get-operand-zeropage)
        #xb4
        (instruction 'LDY 2 4 get-operand-zeropage-x)
        #xac
        (instruction 'LDY 3 4 get-operand-absolute)
        #xbc
        (instruction 'LDY 3 4 get-operand-absolute-x)
        #x4a
        (instruction 'LSR-A 1 2 get-operand-accumulator)
        #x46
        (instruction 'LSR-M 2 5 get-address-zeropage)
        #x56
        (instruction 'LSR-M 2 6 get-address-zeropage-x)
        #x4e
        (instruction 'LSR-M 3 6 get-address-absolute)
        #x5e
        (instruction 'LSR-M 3 7 get-address-absolute-x)
        #xea
        (instruction 'NOP 1 2 get-operand-implied)
        #x09
        (instruction 'ORA 2 2 get-operand-immediate)
        #x05
        (instruction 'ORA 2 3 get-operand-zeropage)
        #x15
        (instruction 'ORA 2 4 get-operand-zeropage-x)
        #x0d
        (instruction 'ORA 3 4 get-operand-absolute)
        #x1d
        (instruction 'ORA 3 4 get-operand-absolute-x)
        #x19
        (instruction 'ORA 3 4 get-operand-absolute-y)
        #x01
        (instruction 'ORA 2 6 get-operand-indirect-x)
        #x11
        (instruction 'ORA 2 5 get-operand-indirect-y)
        #x48
        (instruction 'PHA 1 3 get-operand-implied)
        #x08
        (instruction 'PHP 1 3 get-operand-implied)
        #x68
        (instruction 'PLA 1 4 get-operand-implied)
        #x28
        (instruction 'PLP 1 4 get-operand-implied)
        #x2a
        (instruction 'ROL-A 1 2 get-operand-implied)
        #x26
        (instruction 'ROL-M 2 5 get-address-zeropage)
        #x36
        (instruction 'ROL-M 2 6 get-address-zeropage-x)
        #x2e
        (instruction 'ROL-M 3 6 get-address-absolute)
        #x3e
        (instruction 'ROL-M 3 7 get-address-absolute-x)
        #x6a
        (instruction 'ROR-A 1 2 get-operand-implied)
        #x66
        (instruction 'ROR-M 2 5 get-address-zeropage)
        #x76
        (instruction 'ROR-M 2 6 get-address-zeropage-x)
        #x6e
        (instruction 'ROR-M 3 6 get-address-absolute)
        #x7e
        (instruction 'ROR-M 3 7 get-address-absolute-x)))

(: update-zero-flag! (-> registers Integer Void))
(define (update-zero-flag! r val)
  (set-registers-z! r (zero? val)))

(: update-negative-flag! (-> registers Integer Void))
(define (update-negative-flag! r val)
  (set-registers-n! r (not (zero? (bitwise-and #b10000000 val)))))

(: update-zero-negative-flags! (-> registers Integer Void))
(define (update-zero-negative-flags! r val)
  (begin
    (update-zero-flag! r val)
    (update-negative-flag! r val)))

(: set-registers-a-update! (-> registers Integer Void))
(define (set-registers-a-update! r val)
  (begin
    (set-registers-a! r val)
    (update-zero-negative-flags! r (registers-a r))))

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

(: bmi (-> registers Integer Void))
(define (bmi r val)
  (branch r val (registers-n r)))

(: bne (-> registers Integer Void))
(define (bne r val)
  (branch r val (not (registers-z r))))

(: bpl (-> registers Integer Void))
(define (bpl r val)
  (branch r val (not (registers-n r))))

(: bvc (-> registers Integer Void))
(define (bvc r val)
  (branch r val (not (registers-v r))))

(: bvs (-> registers Integer Void))
(define (bvs r val)
  (branch r val (registers-v r)))

(: clc (-> registers Integer Void))
(define (clc r val)
  (set-registers-c! r #f))

(: cld (-> registers Integer Void))
(define (cld r val)
  (set-registers-d! r #f))

(: cli (-> registers Integer Void))
(define (cli r val)
  (set-registers-i! r #f))

(: clv (-> registers Integer Void))
(define (clv r val)
  (set-registers-v! r #f))

(: cmp (-> registers Integer Void))
(define (cmp r val)
  (let ([a (- (registers-a r) val)])
    (begin
      (set-registers-c! r (>= a 0))
      (set-registers-z! r (zero? a))
      (set-registers-n! r (not (zero? (bitwise-and #b10000000 a)))))))

(: cpx (-> registers Integer Void))
(define (cpx r val)
  (let ([a (- (registers-x r) val)])
    (begin
      (set-registers-c! r (>= a 0))
      (set-registers-z! r (zero? a))
      (set-registers-n! r (not (zero? (bitwise-and #b10000000 a)))))))

(: cpy (-> registers Integer Void))
(define (cpy r val)
  (let ([a (- (registers-y r) val)])
    (begin
      (set-registers-c! r (>= a 0))
      (set-registers-z! r (zero? a))
      (set-registers-n! r (not (zero? (bitwise-and #b10000000 a)))))))

(: dec (-> registers (Instance Memory%) Integer Void))
(define (dec r m addr)
  (let ([val (sub1 (send m read addr))])
    (begin
      (send m write addr val)
      (update-zero-negative-flags! r val))))

(: dex (-> registers Integer Void))
(define (dex r val)
  (begin
    (set-registers-x! r (sub1 (registers-x r)))
    (update-zero-negative-flags! r (registers-x r))))

(: dey (-> registers Integer Void))
(define (dey r val)
  (begin
    (set-registers-y! r (sub1 (registers-y r)))
    (update-zero-negative-flags! r (registers-y r))))

(: eor (-> registers Integer Void))
(define (eor r val)
  (set-registers-a-update! r (bitwise-xor (registers-a r) val)))

(: inc (-> registers (Instance Memory%) Integer Void))
(define (inc r m addr)
  (let ([val (add1 (send m read addr))])
    (begin
      (send m write addr val)
      (update-zero-negative-flags! r val))))

(: inx (-> registers Integer Void))
(define (inx r val)
  (begin
    (set-registers-x! r (add1 (registers-x r)))
    (update-zero-negative-flags! r (registers-x r))))

(: iny (-> registers Integer Void))
(define (iny r val)
  (begin
    (set-registers-y! r (add1 (registers-y r)))
    (update-zero-negative-flags! r (registers-y r))))

(: jmp (-> registers Integer Void))
(define (jmp r val)
  (set-registers-pc! r (- (+ (registers-pc r) val) 3)))

(: jsr (-> registers (Instance Memory%) Integer Void))
(define (jsr r m val)
  (begin
    (stack-push-u16 r m val)
    (set-registers-pc! r (- (send m read-u16 (add1 (registers-pc r))) 3))))

(: lda (-> registers Integer Void))
(define (lda r val)
  (begin
    (set-registers-a! r val)
    (update-zero-negative-flags! r (registers-a r))))

(: ldx (-> registers Integer Void))
(define (ldx r val)
  (begin
    (set-registers-x! r val)
    (update-zero-negative-flags! r (registers-x r))))

(: ldy (-> registers Integer Void))
(define (ldy r val)
  (begin
    (set-registers-y! r val)
    (update-zero-negative-flags! r (registers-y r))))

(: lsr-a (-> registers Void))
(define (lsr-a r)
  (begin
    (set-registers-c! r (odd? (registers-a r)))
    (set-registers-a-update! r (arithmetic-shift (registers-a r) -1))))

(: lsr-m (-> registers (Instance Memory%) Integer Void))
(define (lsr-m r m addr)
  (let* ([val (send m read addr)] [c (odd? val)] [v (arithmetic-shift val -1)])
    (begin
      (set-registers-c! r c)
      (send m write addr v)
      (update-zero-negative-flags! r v))))

(define (nop)
  (void))

(: ora (-> registers Integer Void))
(define (ora r val)
  (set-registers-a! r (bitwise-ior (registers-a r) val)))

(: pha (-> registers (Instance Memory%) Void))
(define (pha r m)
  (stack-push r m (registers-a r)))

(: php (-> registers (Instance Memory%) Void))
(define (php r m)
  (let ([p (bitwise-ior (arithmetic-shift (if (registers-c r) 1 0) 0)
                        (arithmetic-shift (if (registers-z r) 1 0) 1)
                        (arithmetic-shift (if (registers-i r) 1 0) 2)
                        (arithmetic-shift (if (registers-d r) 1 0) 3)
                        (arithmetic-shift (if (registers-b r) 1 0) 4)
                        (arithmetic-shift 1 5)
                        (arithmetic-shift (if (registers-v r) 1 0) 6)
                        (arithmetic-shift (if (registers-n r) 1 0) 7))])
    (stack-push r m p)))

(: pla (-> registers (Instance Memory%) Void))
(define (pla r m)
  (set-registers-a-update! r (stack-pop r m)))

(: plp (-> registers (Instance Memory%) Void))
(define (plp r m)
  (let ([p (stack-pop r m)])
    (begin
      (set-registers-c! r (bitwise-bit-set? p 0))
      (set-registers-z! r (bitwise-bit-set? p 1))
      (set-registers-i! r (bitwise-bit-set? p 2))
      (set-registers-d! r (bitwise-bit-set? p 3))
      (set-registers-b! r (bitwise-bit-set? p 4))
      (set-registers-v! r (bitwise-bit-set? p 6))
      (set-registers-n! r (bitwise-bit-set? p 7)))))

(: rol-a (-> registers Void))
(define (rol-a r)
  (let ([c (registers-c r)])
    (begin
      (set-registers-c! r (bitwise-bit-set? (registers-a r) 7))
      (set-registers-a-update! r
                               (bitwise-ior (bitwise-and (arithmetic-shift (registers-a r) 1) #xff)
                                            (if c 1 0))))))

(: rol-m (-> registers (Instance Memory%) Integer Void))
(define (rol-m r m addr)
  (let* ([c (registers-c r)]
         [a (send m read addr)]
         [val (bitwise-ior (bitwise-and (arithmetic-shift a 1) #xff) (if c 1 0))])
    (begin
      (set-registers-c! r (bitwise-bit-set? a 7))
      (send m write addr val)
      (update-zero-negative-flags! r val))))

(: ror-a (-> registers Void))
(define (ror-a r)
  (let* ([c (registers-c r)]
         [a (registers-a r)]
         [val (bitwise-ior (bitwise-and (arithmetic-shift a -1) #xff)
                           (arithmetic-shift (if c 1 0) 7))])
    (begin
      (set-registers-c! r (bitwise-bit-set? a 0))
      (set-registers-a-update! r val))))

(: ror-m (-> registers (Instance Memory%) Integer Void))
(define (ror-m r m addr)
  (let* ([c (registers-c r)]
         [a (send m read addr)]
         [val (bitwise-ior (bitwise-and (arithmetic-shift a -1) #xff)
                           (arithmetic-shift (if c 1 0) 7))])
    (begin
      (set-registers-c! r (bitwise-bit-set? a 0))
      (send m write addr val)
      (update-zero-negative-flags! r val))))

(: tax (-> registers Void))
(define (tax r)
  (begin
    (set-registers-x! r (registers-a r))
    (update-zero-negative-flags! r (registers-x r))))

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
                          ['BMI (bmi r val)]
                          ['BNE (bne r val)]
                          ['BPL (bpl r val)]
                          ['BRK (break (void))]
                          ['BVC (bvc r val)]
                          ['BVS (bvs r val)]
                          ['CLC (clc r val)]
                          ['CLD (cld r val)]
                          ['CLI (cli r val)]
                          ['CLV (clv r val)]
                          ['CMP (cmp r val)]
                          ['CPX (cpx r val)]
                          ['CPY (cpy r val)]
                          ['DEC (dec r m val)]
                          ['DEX (dex r val)]
                          ['DEY (dey r val)]
                          ['EOR (eor r val)]
                          ['INC (inc r m val)]
                          ['INX (inx r val)]
                          ['INY (iny r val)]
                          ['JMP (jmp r val)]
                          ['JSR (jsr r m val)]
                          ['LDA (lda r val)]
                          ['LDX (ldx r val)]
                          ['LDY (ldy r val)]
                          ['LSR-A (lsr-a r)]
                          ['LSR-M (lsr-m r m val)]
                          ['NOP (nop)]
                          ['ORA (ora r val)]
                          ['TAX (tax r)]
                          ['PHA (pha r m)]
                          ['PHP (php r m)]
                          ['PLA (pla r m)]
                          ['PLP (plp r m)]
                          ['ROL-A (rol-a r)]
                          ['ROL-M (rol-m r m val)]
                          ['ROR-A (ror-a r)]
                          ['ROR-M (ror-m r m val)])
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

  (test-case "CPY Immediate"
    (let ([r (create-cpu)] [m (new memory%)])
      (set-registers-y! r #x01)
      (send m load #x8000 (bytes #xc0 #x02 #x00))
      (interpret r m)
      (check-equal? (registers-c r) #f)
      (check-equal? (registers-z r) #f)
      (check-equal? (registers-n r) #t)))

  (test-case "DEC Absolute"
    (let ([r (create-cpu)] [m (new memory%)])
      (send m write #x1234 #x56)
      (send m load #x8000 (bytes #xce #x34 #x12 #x00))
      (interpret r m)
      (check-equal? (send m read #x1234) #x55)))

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
