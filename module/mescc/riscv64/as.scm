;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2021 Gabriel Wicki <gabriel@erlikon.ch>
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; define riscv64 assembly

;;; Code:

(define-module (mescc riscv64 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (riscv64:instructions))

(define (e->x o)
  (string-drop o 1))

(define (e->l o)
  (string-append (string-drop-right (string-drop o 1) 1) "l"))


(define (riscv64:function-preamble . rest)
  '(("RD_SP RS1_SP !-24 ADDI") ;; allocate stack
    ("RS1_SP RS2_FP SD")       ;; save fp
    ("RS1_SP RS2_RA @8 SD")    ;; save ra
    ("RS1_SP RS2_TP @16 SD")   ;; save tp
    ("RD_S0 RS1_SP !24 ADDI")  ;; save original stack pointer in s0
    ))

(define (riscv64:function-locals . rest)
  ;; allocate stack for local variables?
  ;; what's `rest' used for?  is it being used at all?
  `(("#sub____$i32,%esp" (#:immediate ,(+ (* 4 1025) (* 20 4)))))) ; 4*1024 buf, 20 local vars

(define (riscv64:r->local info n)
  (or n (error "invalid value: riscv64:r->local: " n))
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))))))

(define (riscv64:value->r info v)
  ;; store value in register?
  (let ((r (get-r info)))
    `((,(string-append r " !" (number->string v) " ADDI") ;(#:immediate1 ,v)
       ))))

(define (riscv64:ret . rest)
  ;; return (from function call)
  '(("RETURN"))) ;; RS1_RA JALR

(define (riscv64:r-zero? info)
  ;; test whether register is equal to zero?
  ;; this doesn't really work in RISC-V ?
  ;; beqz ? maybe?
  (let ((r (get-r info)))
    `((,(string-append "test___%" r "," "%" r)))))

(define (riscv64:local->r info n)
  ;; load local variable to register ?
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____0x8(%ebp),%" r) (#:immediate1 ,n))
           `(,(string-append "mov____0x32(%ebp),%" r) (#:immediate ,n))))))

(define (riscv64:r0+r1 info)
  ;; add register0 to register1
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "add____%" r1 ",%" r0)))))

(define (riscv64:call-label info label n)
  ;; jump to label ?
  `((call32 (#:offset ,label))
    ("add____$i8,%esp" (#:immediate1 ,(* n 4)))))

(define (riscv64:r->arg info i)
  ;; store register on stack ?
  (let ((r (get-r info)))
    `((,(string-append "push___%" r)))))

(define (riscv64:label->arg info label i)
  ;; store
  `(("push___$i32" (#:address ,label))))

(define (riscv64:r-negate info)
  ;; ???
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:r0-r1 info)
  ;;; subtract r1 from r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sub____%" r1 ",%" r0)))))

(define (riscv64:zf->r info)
  ;; zf?
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "sete___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:xor-zf info)
  ;; ???
  '(("lahf")
    ("xor____$i8,%ah" (#:immediate1 #x40))
    ("sahf")))

(define (riscv64:r->local+n info id n)
  ;; ???
  (let ((n (+ (- 0 (* 4 id)) n))
        (r (get-r info)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))))))

(define (riscv64:r-mem-add info v)
  ;; ???
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,(%" r ")") (#:immediate1 ,v))
           `(,(string-append "add____$i32,(%" r ")") (#:immediate ,v))))))

(define (riscv64:r-byte-mem-add info v)
  ;; ???
  (let ((r (get-r info)))
    `((,(string-append "addb___$i8,(%" r ")") (#:immediate1 ,v)))))

(define (riscv64:r-word-mem-add info v)
  ;; ???
  (let ((r (get-r info)))
    `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v)))))

(define (riscv64:local-ptr->r info n)
  ;; what's a local pointer?
  (let ((r (get-r info)))
    (let ((n (- 0 (* 4 n))))
      `((,(string-append "mov____%ebp,%" r))
        ,(if (< (abs n) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,n))
             `(,(string-append "add____$i32,%" r)  (#:immediate ,n)))))))

(define (riscv64:label->r info label)
  ;; ???
  (let ((r (get-r info)))
    `((,(string-append "mov____$i32,%" r) (#:address ,label)))))

(define (riscv64:r0->r1 info)
  ;; why should this be necessary?
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r0 ",%" r1)))))

(define (riscv64:byte-mem->r info)
  ;; ???
  (let ((r (get-r info)))
    `((,(string-append "movzbl_(%" r "),%" r)))))

(define (riscv64:byte-r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:byte-signed-r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movsbl_%" l ",%" r)))))

(define (riscv64:word-r info)
  (let* ((r (get-r info))
         (x (e->x r)))
    `((,(string-append "movzwl_%" x ",%" r)))))

(define (riscv64:word-signed-r info)
  (let* ((r (get-r info))
         (x (e->x r)))
    `((,(string-append "movswl_%" x ",%" r)))))

(define (riscv64:jump info label)
  `(("jmp32 " (#:offset ,label))))

(define (riscv64:jump-z info label)
  `(("je32  " (#:offset ,label))))

(define (riscv64:jump-nz info label)
  `(("jne32 " (#:offset ,label))))

(define (riscv64:jump-byte-z info label)
  `(("test___%al,%al")
    ("je32  " (#:offset ,label))))

;; signed
(define (riscv64:jump-g info label)
  `(("jg32  " (#:offset ,label))))

(define (riscv64:jump-ge info label)
  `(("jge32 " (#:offset ,label))))

(define (riscv64:jump-l info label)
  `(("jl32  " (#:offset ,label))))

(define (riscv64:jump-le info label)
  `(("jle32 " (#:offset ,label))))

;; unsigned
(define (riscv64:jump-a info label)
  `(("ja32  " (#:offset ,label))))

(define (riscv64:jump-ae info label)
  `(("jae32 " (#:offset ,label))))

(define (riscv64:jump-b info label)
  `(("jb32  " (#:offset ,label))))

(define (riscv64:jump-be info label)
  `(("jbe32 " (#:offset ,label))))

(define (riscv64:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (e->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (riscv64:label-mem->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____0x32,%" r) (#:address ,label)))))

(define (riscv64:word-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "movzwl_(%" r "),%" r)))))

(define (riscv64:mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "mov____(%" r "),%" r)))))

(define (riscv64:local-add info n v)
  (let ((n (- 0 (* 4 n))))
    `(,(if (and (< (abs n) #x80)
                (< (abs v) #x80)) `("add____$i8,0x8(%ebp)" (#:immediate1 ,n) (#:immediate1 ,v))
                `("add____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v))))))

(define (riscv64:label-mem-add info label v)
  `(,(if (< (abs v) #x80) `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
         `("add____$i32,0x32" (#:address ,label) (#:immediate ,v)))))

(define (riscv64:nop info)
  '(("nop")))

(define (riscv64:swap-r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xchg___%" r0 ",%" r1)))))

;; signed
(define (riscv64:g?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setg___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:ge?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setge__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:l?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setl___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:le?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setle__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

;; unsigned
(define (riscv64:a?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "seta___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:ae?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setae__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:b?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setb___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:be?->r info)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "setbe__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:test-r info)
  (let ((r (get-r info)))
    `((,(string-append "test___%" r ",%" r)))))

(define (riscv64:r->label info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____%" r ",0x32") (#:address ,label)))))

(define (riscv64:r->byte-label info label)
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movb___%" l ",0x32") (#:address ,label)))))

(define (riscv64:r->word-label info label)
  (let* ((r (get-r info))
        (x (e->x r)))
    `((,(string-append "movw___%" x ",0x32") (#:address ,label)))))

(define (riscv64:call-r info n)
  (let ((r (get-r info)))
    `((,(string-append "call___*%" r))
      ("add____$i8,%esp" (#:immediate1  ,(* n 4))))))

(define (riscv64:r0*r1 info)
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          (,(string-append "mul____%" r1))
          ,@(if (equal? r0 "eax") '()
                `((,(string-append "mov____%eax,%" r0))
                  ("pop____%eax"))))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          (,(string-append "mul____%" r1))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%eax,%" r0))
          ("pop____%eax")))))

(define (riscv64:r0<<r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shl____%cl,%" r0)))))

(define (riscv64:r0>>r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shr____%cl,%" r0)))))

(define (riscv64:r0-and-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r1 ",%" r0)))))

(define (riscv64:r0/r1 info signed?)
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          ,@(if (equal? r0 "eax") '()
                `((,(string-append "mov____%eax,%" r0))
                  ("pop____%eax"))))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%ebx")) `(,(string-append "div___%ebx")))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%eax,%" r0))
          ("pop____%eax")))))

(define (riscv64:r0%r1 info signed?)
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if (not (member "edx" allocated))
        `(,@(if (equal? r0 "eax") '()
                `(("push___%eax")
                  (,(string-append "mov____%" r0 ",%eax"))))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%" r1)) `(,(string-append "div___%" r1)))
          (,(string-append "mov____%edx,%" r0)))
        `(("push___%eax")
          ("push___%ebx")
          ("push___%edx")
          (,(string-append "mov____%" r1 ",%ebx"))
          (,(string-append "mov____%" r0 ",%eax"))
          ,(if signed? '("cltd") '("xor____%edx,%edx"))
          ,(if signed? `(,(string-append "idiv___%ebx")) `(,(string-append "div___%ebx")))
          ("pop____%edx")
          ("pop____%ebx")
          (,(string-append "mov____%edx,%" r0))
          ("pop____%eax")))))

(define (riscv64:r+value info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r) (#:immediate ,v))))))

(define (riscv64:r0->r1-mem info)
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "mov____%" r0 ",(%" r1 ")")))))

;; exact duplicate?
;; (define (riscv64:byte-r0->r1-mem info)
;;   (let* ((r0 (get-r0 info))
;;          (r1 (get-r1 info))
;;          (l0 (e->l r0)))
;;     `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (riscv64:word-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (x0 (e->x r0)))
    `((,(string-append "mov____%" x0 ",(%" r1 ")")))))

(define (riscv64:r-cmp-value info v)
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "cmp____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "cmp____$i32,%" r) (#:immediate ,v))))))

(define (riscv64:push-register info r)
  `((,(string-append "push___%" r))))

(define (riscv64:pop-register info r)
  `((,(string-append "pop____%" r))))

(define (riscv64:return->r info)
  (let ((r (get-r info)))
    (if (equal? r "RS_A0") '()
        `((,(string-append "RD_A0 " r "ADDI"))))))

(define (riscv64:r0-or-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r1 ",%" r0)))))

(define (riscv64:shl-r info n)
  (let ((r (get-r info)))
    `((,(string-append "shl____$i8,%" r) (#:immediate1 ,n)))))

(define (riscv64:r+r info)
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r)))))

(define (riscv64:not-r info)
  (let ((r (get-r info)))
    `((,(string-append "not____%" r)))))

(define (riscv64:r0-xor-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r1 ",%" r0)))))

(define (riscv64:r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "mov____%" r2 ",(%" r1 ")")))))

(define (riscv64:byte-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (l2 (e->l r2)))
    `((,(string-append "mov____(%" r0 "),%" l2))
      (,(string-append "mov____%" l2 ",(%" r1 ")")))))

(define (riscv64:word-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (x2 (e->x r2)))
    `((,(string-append "mov____(%" r0 "),%" x2))
      (,(string-append "mov____%" x2 ",(%" r1 ")")))))

(define (riscv64:r0+value info v)
  (let ((r0 (get-r0 info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r0) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r0) (#:immediate ,v))))))

(define (riscv64:value->r0 info v)
  (let ((r0 (get-r0 info)))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (riscv64:byte-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info))
         (l (e->l r) ))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" l ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" l ",0x32(%ebp)") (#:immediate ,n))))))

(define (riscv64:word-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info))
         (x (e->x r)))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" x ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" x ",0x32(%ebp)") (#:immediate ,n))))))

(define (riscv64:r-and info v)
  (let ((r (get-r info)))
    `((,(string-append "and____$i32,%" r) (#:immediate ,v)))))

(define (riscv64:push-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "push___%" r0)))))

(define (riscv64:r1->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r1 ",%" r0)))))

(define (riscv64:pop-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "pop____%" r0)))))

(define (riscv64:swap-r-stack info)
  (let ((r (get-r info)))
    `((,(string-append "xchg___%" r ",(%esp)")))))

(define (riscv64:swap-r1-stack info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "xchg___%" r0 ",(%esp)")))))

(define (riscv64:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((;,(string-append  "mov____%" r2 ",%" r1)
             "RD0 RS2 ADDI")))
        `((,(string-append  "pop____%" r0))
          (,(string-append  "push___%" r0))))))

(define riscv64:instructions
  `(
    (a?->r . ,riscv64:a?->r)
    (ae?->r . ,riscv64:ae?->r)
    (b?->r . ,riscv64:b?->r)
    (be?->r . ,riscv64:be?->r)
    (byte-mem->r . ,riscv64:byte-mem->r)
    (byte-r . ,riscv64:byte-r)
    (byte-r->local+n . ,riscv64:byte-r->local+n)
    (byte-r0->r1-mem . ,riscv64:byte-r0->r1-mem)
    (byte-r0->r1-mem . ,riscv64:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,riscv64:byte-r0-mem->r1-mem)
    (byte-signed-r . ,riscv64:byte-signed-r)
    (call-label . ,riscv64:call-label)
    (call-r . ,riscv64:call-r)
    (function-locals . ,riscv64:function-locals)
    (function-preamble . ,riscv64:function-preamble)
    (g?->r . ,riscv64:g?->r)
    (ge?->r . ,riscv64:ge?->r)
    (jump . ,riscv64:jump)
    (jump-a . ,riscv64:jump-a)
    (jump-ae . ,riscv64:jump-ae)
    (jump-b . ,riscv64:jump-b)
    (jump-be . ,riscv64:jump-be)
    (jump-byte-z . ,riscv64:jump-byte-z)
    (jump-g . , riscv64:jump-g)
    (jump-ge . , riscv64:jump-ge)
    (jump-l . ,riscv64:jump-l)
    (jump-le . ,riscv64:jump-le)
    (jump-nz . ,riscv64:jump-nz)
    (jump-z . ,riscv64:jump-z)
    (l?->r . ,riscv64:l?->r)
    (label->arg . ,riscv64:label->arg)
    (label->r . ,riscv64:label->r)
    (label-mem->r . ,riscv64:label-mem->r)
    (label-mem-add . ,riscv64:label-mem-add)
    (le?->r . ,riscv64:le?->r)
    (local->r . ,riscv64:local->r)
    (local-add . ,riscv64:local-add)
    (local-ptr->r . ,riscv64:local-ptr->r)
    (long-r0->r1-mem . ,riscv64:r0->r1-mem)
    (long-r0-mem->r1-mem . ,riscv64:r0-mem->r1-mem)
    (mem->r . ,riscv64:mem->r)
    (nop . ,riscv64:nop)
    (not-r . ,riscv64:not-r)
    (pop-r0 . ,riscv64:pop-r0)
    (pop-register . ,riscv64:pop-register)
    (push-r0 . ,riscv64:push-r0)
    (push-register . ,riscv64:push-register)
    (r+r . ,riscv64:r+r)
    (r+value . ,riscv64:r+value)
    (r->arg . ,riscv64:r->arg)
    (r->byte-label . ,riscv64:r->byte-label)
    (r->label . ,riscv64:r->label)
    (r->local . ,riscv64:r->local)
    (r->local+n . ,riscv64:r->local+n)
    (r->word-label . ,riscv64:r->word-label)
    (r-and . ,riscv64:r-and)
    (r-byte-mem-add . ,riscv64:r-byte-mem-add)
    (r-cmp-value . ,riscv64:r-cmp-value)
    (r-mem-add . ,riscv64:r-mem-add)
    (r-negate . ,riscv64:r-negate)
    (r-word-mem-add . ,riscv64:r-word-mem-add)
    (r-zero? . ,riscv64:r-zero?)
    (r0%r1 . ,riscv64:r0%r1)
    (r0*r1 . ,riscv64:r0*r1)
    (r0+r1 . ,riscv64:r0+r1)
    (r0+value . ,riscv64:r0+value)
    (r0->r1 . ,riscv64:r0->r1)
    (r0->r1-mem . ,riscv64:r0->r1-mem)
    (r0-and-r1 . ,riscv64:r0-and-r1)
    (r0-mem->r1-mem . ,riscv64:r0-mem->r1-mem)
    (r0-or-r1 . ,riscv64:r0-or-r1)
    (r0-r1 . ,riscv64:r0-r1)
    (r0-xor-r1 . ,riscv64:r0-xor-r1)
    (r0/r1 . ,riscv64:r0/r1)
    (r0<<r1 . ,riscv64:r0<<r1)
    (r0>>r1 . ,riscv64:r0>>r1)
    (r1->r0 . ,riscv64:r1->r0)
    (r2->r0 . ,riscv64:r2->r0)
    (ret . ,riscv64:ret)
    (return->r . ,riscv64:return->r)
    (shl-r . ,riscv64:shl-r)
    (swap-r-stack . ,riscv64:swap-r-stack)
    (swap-r0-r1 . ,riscv64:swap-r0-r1)
    (swap-r1-stack . ,riscv64:swap-r1-stack)
    (test-r . ,riscv64:test-r)
    (value->r . ,riscv64:value->r)
    (value->r0 . ,riscv64:value->r0)
    (word-mem->r . ,riscv64:word-mem->r)
    (word-r . ,riscv64:word-r)
    (word-r->local+n . ,riscv64:word-r->local+n)
    (word-r0->r1-mem . ,riscv64:word-r0->r1-mem)
    (word-r0-mem->r1-mem . ,riscv64:word-r0-mem->r1-mem)
    (word-signed-r . ,riscv64:word-signed-r)
    (xor-zf . ,riscv64:xor-zf)
    (zf->r . ,riscv64:zf->r)
    ))
