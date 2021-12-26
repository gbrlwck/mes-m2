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

;; register dedication:
;; T6 for Zero-Flag


;; these procs are used for adjusting register names in x86
;; (define (e->x o)
;;   (string-drop o 1))
;; (define (e->l o)
;;   (string-append (string-drop-right (string-drop o 1) 1) "l"))


(define (riscv64:function-preamble . rest)
  '(("# riscv64:function-preamble")
    ("RD_SP RS1_SP !-24 ADDI") ;; allocate stack
    ("RS1_SP RS2_FP SD")       ;; save fp
    ("RS1_SP RS2_RA @8 SD")    ;; save ra
    ("RS1_SP RS2_TP @16 SD")   ;; save tp
    ("RD_S0 RS1_SP !24 ADDI")  ;; save original stack pointer in s0
    ))

(define (riscv64:function-locals . rest)
  ;; allocate stack for local variables
  ;;    writing on the stack decreases the pointer
  ;;    if we want to allocate stack space we need to increase the pointer first
  `(("# riscv64:function-locals")
    ("RD_SP" (#:immediate1 ,(+ (* 4 1025) (* 20 4))) "ADDI")) ; 4*1024 buf, 20 local vars
  )

(define (riscv64:r->local info n)
  (or n (error "invalid value: riscv64:r->local: " n))
  (let ((r (get-r info))
        (n (- 0 (* 4 n)))) ;; whuat? negate 4n
    `(("# riscv64:r->local")
      (,(string-append "RD_" r " " (#:immediate n) " SD")))
    ;; we align everything 64 bit (?)
    ;; `(,(if (< (abs n) #x80)
    ;;       `(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
    ;;       `(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))))
    ))

(define (riscv64:value->r info v)
  ;; store value in register
  (let ((r (get-r info)))
    `(("# riscv64:value->r")
      (,(string-append "RD_" r " ") (#:immediate1 ,v) " ADDI"))))

(define (riscv64:ret . rest)
  ;; return (from function call)
  '(("# riscv64:ret")
    ("RETURN"))) ;; RS1_RA JALR

(define (riscv64:r-zero? info)
  ;; test whether register is equal to zero -> store 1 or 0 in T6
  (let ((r (get-r info)))
    `(("# riscv64:r-zero?")
      (,(string-append "RD_T6 RS1_" r " SLTIU") ;;,(string-append "test___%" r "," "%" r)
       ))))

(define (riscv64:local->r info n)
  ;; load local variable into register
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(("# riscv64:local->r")
      (,(string-append "RD_" r " RS1_SP " " LD ") (#:immediate1 ,n))
      ;; (if (< (abs n) #x80) `(,(string-append "mov____0x8(%ebp),%" r) (#:immediate1 ,n))
      ;;      `(,(string-append "mov____0x32(%ebp),%" r) (#:immediate ,n)))
      )))

(define (riscv64:r0+r1 info)
  ;; add register0 to register1
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "RD_" r0 " RS0_" r0 " RS1_" r1 " ADD"))
      ;;,(string-append "add____%" r1 ",%" r0)
      )))

(define (riscv64:call-label info label n)
  ;; jump to label
  ;; not sure what n is for
  `(("# riscv64:call-label")
    ("RD_A7" (#:offset ,label) "ADDI")
    ("JAL")
    ;;("add____$i8,%esp" (#:immediate1 ,(* n 4))) ;; not sure what this does in x86
    ))

(define (riscv64:r->arg info i)
  ;; push register to stack
  (let ((r (get-r info)))
    `(("# riscv64:r->arg")
      (,(string-append "RD_SP RS1_" r " ADDI")
       ("RD_SP !-8 ADDI") ;,(string-append "push___%" r)
       ))))

(define (riscv64:label->arg info label i)
  ;; store ?? TODO
  `(("# riscv64:label->arg")
    ("push___$i32" (#:address ,label))))

(define (riscv64:r-negate info)
  ;; negate a number?
  (let* ((r (get-r info))
         (l (e->l r)))
    ;; sete: https://stackoverflow.com/questions/53011701/what-does-the-instruction-sete-do-in-assembly
    ;; movzbl: https://stackoverflow.com/questions/9317922/what-does-the-movzbl-instruction-do-in-ia-32-att-syntax#9318005
    `(
      ;; sets target value to 0 or one depending on ZERO Flag
      (,(string-append "RD_T6 RS1_" r " SLTIU"))
      ;;(,(string-append "sete___%" l))
      ;; loads r into l (?)
      ;;(,(string-append "movzbl_%" l ",%" r))
      )))

(define (riscv64:r0-r1 info)
  ;;; subtract r1 from r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "RD_" r1 " RS0_" r1 " RS1_" r0 " SUB" ;"sub____%" r1 ",%" r0
                       )))))

(define (riscv64:zf->r info)
  ;; move Zero-Flag to a register?
  ;; identical to r-negate? TODO
  (riscv64:r-negate info)
  ;; (let* ((r (get-r info))
  ;;        (l (e->l r)))
  ;;   `((,(string-append "sete___%" l))
  ;;     (,(string-append "movzbl_%" l ",%" r))))
  )

(define (riscv64:xor-zf info)
  ;; XOR Zero-Flag
  ;; with what exactly?
  ;; TODO
  '(;; LAHF — Load Status Flags into AH Register
    (,(string-append "RS1_" r " RD_T6 SLTIU"))
    ("RD_T6 RS1_T6 $i8 XORI") ;; XOR or XORI? ;; immediate o nao
    ;;("lahf")
    ;; XOR the zero flag with
    ;;("xor____$i8,%ah" (#:immediate1 #x40))
    ;; store AH into flags
    ;;("sahf")
    ))

(define (riscv64:r->local+n info id n)
  ;; lookup local variable / get from stack
  ;; save register as local variable ?
  (let ((n (+ (- 0 (* 4 id)) n))
        (r (get-r info)))
    `(,(string-append "RD_" r " RS1_SP "
                      `((,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n))
                      " ADDI"))
       ;; `(,(if (< (abs n) #x80)
       ;;      `(,(string-append "RD_" r " RS1_SP " "ADDI") (#:immediate1 n))
       ;;      `(,(string-append "RD_" r " RS1_SP " "ADDI") (#:immediate n))
       ;;      ;;`(,(string-append "mov____%" r ",0x8(%ebp)") (#:immediate1 ,n))
       ;;      ;;`(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n))
       ;;      ))
      ))

(define (riscv64:r-mem-add info v)
  ;; addition which can be stored/read from memory (but not both at the same time)
  (let ((r (get-r info)))
    `(";; riscv64:r-mem-add\n"
      ,(string-append "RD_" r " RS1_SP "
                      `((,(if (< (abs v) #x80)
                              #:immediate1
                              #:immediate)
                         ,v))
                      " ADDI")
                         ;;`(,(if (< (abs v) #x80)
                         ;;       ;;`(,(string-append "RD_" r " RS1_SP " (#:immediate1 ,v) "ADDI")
                         ;;       ;;`(,(string-append "RD_" r " RS1_SP " (#:immediate ,v) "ADDI")
                         ;;       `(,(string-append "add____$i8,(%" r ")") (#:immediate1 ,v))
                         ;;       `(,(string-append "add____$i32,(%" r ")") (#:immediate ,v))
                         ;;       ))
                        )))

(define (riscv64:r-byte-mem-add info v)
  ;; (let ((r (get-r info)))
  ;;   `((,(string-append "addb___$i8,(%" r ")") (#:immediate1 ,v))))
  (riscv64:r-mem-add info v))

(define (riscv64:r-word-mem-add info v)
  ;; (let ((r (get-r info)))
  ;;   `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v))))
  (riscv64:r-mem-add info v))

(define (riscv64:local-ptr->r info n)
  ;; what's a *local* pointer?
  (let ((r (get-r info)))
    (let ((n (- 0 (* 4 n))))
      `((,(string-append "mov____%ebp,%" r))
        ,(if (< (abs n) #x80)
             `(,(string-append "RD_" r " RS1_SP " (#:immediate1 n) "ADDI")
               `(,(string-append "RD_" r " RS1_SP " (#:immediate n) "ADDI")
             ;; `(,(string-append "add____$i8,%" r) (#:immediate1 ,n))
             ;; `(,(string-append "add____$i32,%" r)  (#:immediate ,n))
             )))))))

(define (riscv64:label->r info label)
  ;; load label (address) into register
  (let ((r (get-r info)))
    `((,(string-append "RD_" r " $i32 ADDI")
       ;;,(string-append "mov____$i32,%" r) (#:address ,label)
       ))))

(define (riscv64:r0->r1 info)
  ;; load value in register r0 into r1
  ;; why should this be necessary?
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(((string-append "RD_" r0 " RS0_" r0 "RS1_" r1 " ADDI")
       ;;,(string-append  "mov____%" r0 ",%" r1)
       ))))

(define (riscv64:byte-mem->r info)
  ;; ???
  (let ((r (get-r info)))
    ;; move byte to word with zero extension
    `(("NOP" ;;,(string-append "movzbl_(%" r "),%" r)
       ))))

(define (riscv64:byte-r info)
  ;; load byte and zero-extend to word
  ;; this is unnecessary in riscv, as we have word-size all the time
  (let* ((r (get-r info))
         (l (e->l r)))
    `(("RD_" r " RS1_" r " LBU")))) ;;,(string-append "movzbl_%" l ",%" r)

(define (riscv64:byte-signed-r info)
  (let* ((r (get-r info)))
    `(("RD_" r " RS1_" r " LB")))) ;;,(string-append "movsbl_%" l ",%" r)

(define (riscv64:word-r info)
  (let* ((r (get-r info)))
    `(("RD_" r " RS1_" r " LWU")))) ;;,(string-append "movzwl_%" x ",%" r)


(define (riscv64:word-signed-r info)
  (let* ((r (get-r info)))
    `(("RD_" r " RS1_" r " LW")))) ;;,(string-append "movswl_%" x ",%" r)


(define (riscv64:long-r info)
  ;; load (unsigned) long into register
  (let* ((r (get-r info)))
    `((,(string-append "RD_" r " RS1_" r " LDU")))))

(define (riscv64:long-signed-r info)
  ;; load signed long into register
  (let* ((r (get-r info)))
    `(("# riscv64:long-signed-r")
      (,(string-append "RD_" r " RS1_" r " LD")))))

(define (riscv64:jump info label)
  ;; unconditional jump
  `(,(string-append "RD_A0" (#:offset label) "ADDI")
    ("JALR") ;; or JAL? ;;("jmp32 " (#:offset ,label))
    ))

(define (riscv64:jump-z info label)
  ;; jump if zero
  `(((#:offset ,label) " BEQZ")
    ;;("je32  " (#:offset ,label))
    ))

(define (riscv64:jump-nz info label)
  `(((#:offset ,label) " BNEZ")
    ;;("jne32 " (#:offset ,label))
    ))

(define (riscv64:jump-byte-z info label)
  (riscv64:jump-z info label)
  ;; `(("test___%al,%al")
  ;;   ("je32  " (#:offset ,label)))
  )

;; signed
;; TODO WHOLE SECTION
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
  ;; ? TODO
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (l0 (e->l r0)))
    `((,(string-append "mov____%" l0 ",(%" r1 ")")))))

(define (riscv64:label-mem->r info label)
  ;; ? TODO
  (let ((r (get-r info)))
    `((,(string-append "mov____0x32,%" r) (#:address ,label)))))

(define (riscv64:word-mem->r info)
  ;; ?? TODO
  (let ((r (get-r info)))
    `((,(string-append "movzwl_(%" r "),%" r)))))

(define (riscv64:mem->r info)
  ;; ?? TODO
  (let ((r (get-r info)))
    `((,(string-append "mov____(%" r "),%" r)))))

(define (riscv64:local-add info n v)
  ;; add directly in memory ?
  (let ((n (- 0 (* 4 n))))
    `(,(if (and (< (abs n) #x80)
                (< (abs v) #x80))
           `("add____$i8,0x8(%ebp)" (#:immediate1 ,n) (#:immediate1 ,v))
           `("add____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v))))))

(define (riscv64:label-mem-add info label v)
  ;; 
  `(,(if (< (abs v) #x80)
         `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
         `("add____$i32,0x32" (#:address ,label) (#:immediate ,v)))))

(define (riscv64:nop info)
  '(("NOP")))

(define (riscv64:swap-r0-r1 info)
  ;; swaps content of memory locations?
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
  ;; janneke: it's true if r is zero
  ;; https://en.wikipedia.org/wiki/TEST_(x86_instruction)
  ;; bit-wise AND for the two operands
  (let ((r (get-r info)))
    `((
       ,(string-append "test___%" r ",%" r)))))

(define (riscv64:r->label info label)
  ;; fill r with label address?
  (let ((r (get-r info)))
    `((,(string-append "mov____%" r ",0x32") (#:address ,label)))))

(define (riscv64:r->byte-label info label)
  ;; fill r with label address byte?
  (let* ((r (get-r info))
         (l (e->l r)))
    `((,(string-append "movb___%" l ",0x32") (#:address ,label)))))

(define (riscv64:r->word-label info label)
  ;; see above
  (let* ((r (get-r info))
        (x (e->x r)))
    `((,(string-append "movw___%" x ",0x32") (#:address ,label)))))

(define (riscv64:call-r info n)
  ;; Saves procedure linking information on the stack and branches to the called procedure specified using the target operand. The target operand specifies the address of the first instruction in the called procedure. The operand can be an immediate value, a general-purpose register, or a memory location.
  (let ((r (get-r info)))
    `((,(string-append "call___*%" r))
      ("add____$i8,%esp" (#:immediate1  ,(* n 4))))))

(define (riscv64:r0*r1 info)
  ;; simple multiplication ?
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
  ;; left-shift? 
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shl____%cl,%" r0)))))

(define (riscv64:r0>>r1 info)
  ;; right-shift?
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mov____%" r1 ",%ecx"))
      (,(string-append "shr____%cl,%" r0)))))

(define (riscv64:r0-and-r1 info)
  ;; logical AND (or is it bitwise?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r1 ",%" r0)))))

(define (riscv64:r0/r1 info signed?)
  ;; simple division?
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
  ;; modulo ?
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
  ;; add immediate to register?
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r) (#:immediate ,v))))))

(define (riscv64:r0->r1-mem info)
  ;; store value in r0 at address r1 ?
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "mov____%" r0 ",(%" r1 ")")))))

(define (riscv64:word-r0->r1-mem info)
  ;; see above
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         (x0 (e->x r0)))
    `((,(string-append "mov____%" x0 ",(%" r1 ")")))))

(define (riscv64:r-cmp-value info v)
  ;; compare value in register?
  (let ((r (get-r info)))
    `(,(if (< (abs v) #x80) `(,(string-append "cmp____$i8,%" r) (#:immediate1 ,v))
           `(,(string-append "cmp____$i32,%" r) (#:immediate ,v))))))

(define (riscv64:push-register info r)
  ;; push to stack
  `((,(string-append "push___%" r))))

(define (riscv64:pop-register info r)
  ;; pop from stack
  `((,(string-append "pop____%" r))))

(define (riscv64:return->r info)
  ;; write return value to register
  (let ((r (get-r info)))
    (if (equal? r "RS_A0") '()
        `((,(string-append "RD_A0 " r "ADDI"))))))

(define (riscv64:r0-or-r1 info)
  ;; logical OR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r1 ",%" r0)))))

(define (riscv64:shl-r info n)
  ;; shift left ;; not the same as <<
  (let ((r (get-r info)))
    `((,(string-append "shl____$i8,%" r) (#:immediate1 ,n)))))

(define (riscv64:r+r info)
  ;; add r+r, which is the same as (* 2 r)
  ;; not the same as r0+r1?
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r)))))

(define (riscv64:not-r info)
  ;; logical NOT ? 
  (let ((r (get-r info)))
    `((,(string-append "not____%" r)))))

(define (riscv64:r0-xor-r1 info)
  ;; XOR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r1 ",%" r0)))))

(define (riscv64:r0-mem->r1-mem info)
  ;; store datum at r0 at r1
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "mov____%" r2 ",(%" r1 ")")))))

(define (riscv64:byte-r0-mem->r1-mem info)
  ;; see above
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (l2 (e->l r2)))
    `((,(string-append "mov____(%" r0 "),%" l2))
      (,(string-append "mov____%" l2 ",(%" r1 ")")))))

(define (riscv64:word-r0-mem->r1-mem info)
  ;; see above
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         (x2 (e->x r2)))
    `((,(string-append "mov____(%" r0 "),%" x2))
      (,(string-append "mov____%" x2 ",(%" r1 ")")))))

(define (riscv64:r0+value info v)
  ;; add immediate to r0 ?
  (let ((r0 (get-r0 info)))
    `(,(if (< (abs v) #x80) `(,(string-append "add____$i8,%" r0) (#:immediate1 ,v))
           `(,(string-append "add____$i32,%" r0) (#:immediate ,v))))))

(define (riscv64:value->r0 info v)
  ;; load value into r0?
  (let ((r0 (get-r0 info)))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (riscv64:byte-r->local+n info id n)
  ;; store byte at local+n (?)
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
    `((;;,(string-append "push___%" r0)
       "RD_SP !-8 ADDI" ;; grow stack
       ,(string-append "RD_SP RS1_" r0 " SD") ;; store value
       ))))

(define (riscv64:r1->r0 info)
  ;; move r1 to r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((;;,(string-append  "mov____%" r1 ",%" r0)
       ,(string-append r1 " " r0 " ADDI")))))

(define (riscv64:pop-r0 info)
  ;; pop stack into r0
  (let ((r0 (get-r0 info)))
    `((;;,(string-append "pop____%" r0)
       ,(string-append r0 " RS1_SP LD")
       "RD_SP !8 ADDI")))) ;; shrink stack

(define (riscv64:swap-r-stack info)
  ;; swaps register content with stack value
  (let ((r (get-r info)))
    `((,(string-append "xchg___%" r ",(%esp)")))))

(define (riscv64:swap-r1-stack info) ;; name vs register used
  (let ((r0 (get-r0 info)))
    `((,(string-append "xchg___%" r0 ",(%esp)")))))

(define (riscv64:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((;,(string-append  "mov____%" r2 ",%" r1)
             ,(string-append r2 " " r1 " ADDI"))))
        `((() ;;,(string-append  "pop____%" r0)
           )
          (;;,(string-append  "push___%" r0)
           )))))

(define riscv64:instructions
  `((a?->r . ,riscv64:a?->r)
    (ae?->r . ,riscv64:ae?->r)
    (b?->r . ,riscv64:b?->r)
    (be?->r . ,riscv64:be?->r)
    (byte-mem->r . ,riscv64:byte-mem->r)
    (byte-r . ,riscv64:byte-r)
    (byte-r->local+n . ,riscv64:byte-r->local+n)
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
    (long-r . ,riscv64:long-r)
    (long-signed-r . ,riscv64:long-signed-r)
    (xor-zf . ,riscv64:xor-zf)
    (zf->r . ,riscv64:zf->r)))
