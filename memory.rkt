#lang typed/racket

(provide Memory%
         memory%)

(define-type Memory%
             (Class (field (m Bytes))
                    (load (-> Integer Bytes Void))
                    (read (-> Integer Integer))
                    (read-u16 (-> Integer Integer))
                    (write (-> Integer Integer Void))
                    (write-u16 (-> Integer Integer Void))))

(: memory% Memory%)
(define memory%
  (class object%

    (field (m (make-bytes #xffff)))

    (define/public (read addr) (bytes-ref m addr))

    (define/public (read-u16 addr)
      (let ([lo (bytes-ref m addr)] [hi (bytes-ref m (add1 addr))])
        (bitwise-ior (arithmetic-shift hi 8) lo)))

    (define/public (write addr val) (bytes-set! m addr val))

    (define/public (write-u16 addr val)
      (let ([hi (arithmetic-shift val -8)] [lo (bitwise-and val #xff)])
        (begin
          (write addr lo)
          (write (add1 addr) hi))))

    (define/public (load dest-start src) (bytes-copy! m dest-start src))

    (super-new)))

(module+ test
  (require typed/rackunit)
  (test-case "memory% write"
    (let ([a-memory (new memory%)])
      (send a-memory write #x0 #x1)
      (check-equal? #x1 (send a-memory read #x0))))
  (test-case "memory% write-u16"
    (let ([a-memory (new memory%)])
      (send a-memory write-u16 #x0 #x1234)
      (check-equal? #x34 (send a-memory read #x0))
      (check-equal? #x12 (send a-memory read #x1))
      (check-equal? #x1234 (send a-memory read-u16 #x0)))))
