#lang racket
(require racket/trace)

(trace-define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))

; Having been re-written like that
; The procedure now describes a linear recursive procedure (a tree recursion)
; There's a chain of deferred operations that branches out the execution
; That makes it way slower than the original implementation