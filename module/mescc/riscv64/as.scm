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

(define (rd r)
  (string-append "RD_" r))
(define (rs1 r)
  (string-append "RS1_" r))
(define (rs2 r)
  (string-append "RS2_" r))

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
    ("RD_FP RS1_SP !24 ADDI")  ;; save original stack pointer in s0
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
        (n (- 0 (* 4 n)))) ;; calculate location of local variable
    `(("# riscv64:r->local")
      (,(rd r) (#:immediate ,n) "SD"))))

(define (riscv64:value->r info v)
  ;; store value in register
  (let ((r (get-r info)))
    `(("# riscv64:value->r")
      (,(rd r) (#:immediate1 ,v) "ADDI"))))

(define (riscv64:ret . rest)
  ;; i386:ret is "leave" and "return" which is x86 slang for
  ;; restore base and stack pointers
  ;; return (from function call)
  ;; in x86 this simulates:
  ;; pop ecx
  ;; jmp ecx
  '(("# riscv64:ret")
    ("RD_SP RS1_FP ADDI")  ;; restore old stack pointer
    ("RD_FP RS1_SP LD")    ;; restore frame/base pointer
    ("RD_RA RS1_SP @8 LD") ;; restore return address
    ("RD_TP RS1_SP @16 LD");; restore thread pointer
    ;;("RETURN")
    )) ;; RS1_RA JALR

(define (riscv64:r-zero? info)
  ;; test whether register is equal to zero -> store 1 or 0 in T6
  (let ((r (get-r info)))
    `(("# riscv64:r-zero?")
      ("RD_T6" ,(rs1 r) "SLTIU"))))

(define (riscv64:local->r info n)
  ;; load local variable into register
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(("# riscv64:local->r")
      (,(rd r) "RS1_FP"
       (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n) "LD"))))

(define (riscv64:r0+r1 info)
  ;; add register0 to register1
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(rd r0) ,(rs1 r0) ,(rs2 r1) "ADD"))))

(define (riscv64:call-label info label n)
  ;; jump to label
  ;; not sure what n is for
  `(("# riscv64:call-label")
    ("RD_A7" (#:offset ,label) "ADDI")
    ("JAL")
    ;;("add____$i8,%esp" (#:immediate1 ,(* n 4))) ;; not sure what this does in x86
    ;; TODO
    ))

(define (riscv64:r->arg info i)
  ;; push register to stack
  (let ((r (get-r info)))
    `(("# riscv64:r->arg")
      ("RD_SP" ,(rs1 r) "ADDI")
      ("RD_SP !-8 ADDI"))))

(define (riscv64:label->arg info label i)
  ;; store ?? TODO
  `(("# riscv64:label->arg")
    ("RD_SP" (#:address ,label) "SD") ;; -> pointer size is 8 (== double)
    ("RD_SP !0x8 ADDI") ;; increment stack pointer ;; ("push___$i32" (#:address ,label))
    ))

(define (riscv64:r-negate info)
  ;; negate a number
  (let* ((r (get-r info))
         ;;(l (e->l r))
         )
    ;; sete: https://stackoverflow.com/questions/53011701/what-does-the-instruction-sete-do-in-assembly
    ;;  sets argument register to 1 of zero flag is set
    ;; movzbl: https://stackoverflow.com/questions/9317922/what-does-the-movzbl-instruction-do-in-ia-32-att-syntax#9318005
    `(
      ;; sets target value to 0 or one depending on ZERO Flag
      ;;(,(string-append "RD_T6 RS1_" r " SLTIU"))
      ;;(,(string-append "sete___%" l))
      ;; loads r into l (?)
      ;;(,(string-append "movzbl_%" l ",%" r))


      ;; let's try two's complement
      ("# riscv64:r-negate")
      (,(rd r) ,(rs2 r) "SUB")))) ;; x0 reference by omitting rs1

(define (riscv64:r0-r1 info)
  ;; subtract r1 from r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SUB")))) ;"sub____%" r1 ",%" r0

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
  `(;; LAHF — Load Status Flags into AH Register
    ("# riscv64:xor-zf")
    ("RD_T6 SLTIU")
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
    `(("# riscv64:r->local+n")
      (,(rd r) "RS1_SP"
       (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n)
       "ADDI"))))

(define (riscv64:r-mem-add info v)
  ;; addition which can be stored/read from memory (but not both at the same time)
  (let ((r (get-r info)))
    `(("# riscv64:r-mem-add")
      (,(rd r) "RS1_SP"
       (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v)
       "ADDI"))))

(define (riscv64:r-byte-mem-add info v)
  ;; (let ((r (get-r info)))
  ;;   `((,(string-append "addb___$i8,(%" r ")") (#:immediate1 ,v))))
  (riscv64:r-mem-add info v))

(define (riscv64:r-word-mem-add info v)
  ;; (let ((r (get-r info)))
  ;;   `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v))))
  (riscv64:r-mem-add info v))

(define (riscv64:r-long-mem-add info v)
  ;; (let ((r (get-r info)))
  ;;   `((,(string-append "addw___$i8,(%" r ")") (#:immediate2 ,v))))
  (riscv64:r-mem-add info v))

(define (riscv64:local-ptr->r info n)
  ;; what's a *local* pointer?
  (let ((r (get-r info)))
    (let ((n (- 0 (* 4 n))))
      `(("# riscv64:local-ptr->r")
        (,(rd r)
          "RS1_SP"
          (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n)
          "ADDI")))))

(define (riscv64:label->r info label)
  ;; load label (address) into register
  (let ((r (get-r info)))
    `(("# riscv64:label->r")
      (,(rd r) (#:address ,label) "ADDI"))))

(define (riscv64:r0->r1 info)
  ;; load value in register r0 into r1 -- simple mov
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0->r1")
      (,(rd r0) ,(rs1 r1) "ADDI"))))

(define (riscv64:byte-mem->r info)
  ;; ???
  (let ((r (get-r info)))
    ;; move byte to word with zero extension
    `(("# riscv64:byte-mem->r")
      ("NOP" ;;,(string-append "movzbl_(%" r "),%" r)
       ))))

(define (riscv64:byte-r info)
  ;; load byte and zero-extend to word
  ;; this is unnecessary in riscv, as we have word-size all the time
  (let* ((r (get-r info)))
    `(("# riscv64:byte-r")
      (,(rd r) ,(rs1 r) "LBU")))) ;;,(string-append "movzbl_%" l ",%" r)

(define (riscv64:byte-signed-r info)
  (let* ((r (get-r info)))
    `(("# riscv64:byte-signed-r")
      (,(rd r) ,(rs1 r) "LB")))) ;;,(string-append "movsbl_%" l ",%" r)

(define (riscv64:word-r info)
  (let* ((r (get-r info)))
    `(("# riscv64:word-r")
      (,(rd r) ,(rs1 r) "LWU")))) ;;,(string-append "movzwl_%" x ",%" r)

(define (riscv64:word-signed-r info)
  (let* ((r (get-r info)))
    `((,(rd r) ,(rs1 r) "LW")))) ;;,(string-append "movswl_%" x ",%" r)

(define (riscv64:long-r info)
  ;; load (unsigned) long into register
  (let* ((r (get-r info)))
    `((,(rd r) ,(rs1 r) "LDU"))))

(define (riscv64:long-signed-r info)
  ;; load signed long into register
  (let* ((r (get-r info)))
    `(("# riscv64:long-signed-r")
      (";; long signed is the default register kind" ,r)
      ;;(,(rd r " RS1_" r " LD"))
      )))

(define (riscv64:jump info label)
  ;; unconditional jump
  `(("# riscv64:jump")
    ("RD_A0" (#:offset ,label) "ADDI")
    ("RETURN"))) ;; == RS1_RA JALR

(define (riscv64:jump-z info label)
  ;; jump if zero
  `(("# riscv64:jump-z")
    ("RS1_T6" (#:offset ,label) "BEQZ"))) ;;("je32  " (#:offset ,label))

(define (riscv64:jump-nz info label)
  `(((#:offset ,label) "BNEZ"))) ;;("jne32 " (#:offset ,label))

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
  ;; load content at r1 into r0
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info))
         ;(l0 (e->l r0))
         )
    `((,(rd r0) ,(rs1 r1) "LB"))))

(define (riscv64:label-mem->r info label)
  ;; load value from label (in memory) to register
  (let ((r (get-r info)))
    `(("# riscv64:label-mem->r")
      (,(rd r) (#:address ,label) "ADDI")
      (,(rd r) ,(rs1 r) "LD"))))

(define (riscv64:word-mem->r info)
  ;; load word size value at address in register into same register
  (let ((r (get-r info)))
    `(("# riscv64:word-mem->r")
      (,(rd r) ,(rs1 r) "LH"))))

(define (riscv64:long-mem->r info)
  ;; move zero-extended word to long
  (let ((r (get-r info)))
    `(("# riscv64:long-mem->r")
      (";; no action; we expect register content to be of type long")))) ;;(,(string-append "movzwl_(%" r "),%" r))

(define (riscv64:mem->r info)
  ;; load value of address in register
  (let ((r (get-r info)))
    `(("# riscv64:mem->r")
      (,(rd r)
       ,(rs1 r)
       "LD"))))

(define (riscv64:local-add info n v)
  ;; add v to local variable at BP-n
  (let ((n (- 0 (* 4 n)))
        (r (get-r info))
        (r2 (get-r info))
        (c (and (< (abs n) #x80)
                (< (abs v) #x80))))
    `(("# riscv64:local-add")
      ;; store var addr in r
      (,(rd r) ,(rs1 "FP") (,(if c #:immediate1 #:immediate) ,n) "ADDI")
      (,(rd r2) (,(if c #:immediate1 #:immediate) ,v) "ADDI") ;; store value in r2
      (,(rd r) ,(rs1 r2) "SD")))) ;; save to memory

(define (riscv64:label-mem-add info label v)
  ;; add value v to memory stored at label
  `(("# riscv64:label-mem-add")
    ("RD_T0" (#:address ,label) "ADDI") ;; write label address to register
    ("RD_T1 RS1_T0 LD") ;; load value at address
    ("RD_T1 RS1_T1"
     (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v)
     "ADDI") ;; add offset to label address
    ("RS1_T0 RS2_T1 RS2_T1 SD") ;; store augmented value back to memory
    ;; ,(if (< (abs v) #x80)
    ;;      `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
    ;;      `("add____$i32,0x32" (#:address ,label) (#:immediate ,v)))
    ))

(define (riscv64:nop info)
  '(("# NOP")))

(define (riscv64:swap-r0-r1 info)
  ;; swaps content of registers
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (r (get-r info)))
    `(("# riscv64:swap-r0-r1")
      (,(rd r) ,(rs1 r1) "ADDI") ;; move r1 to r
      (,(rd r1) ,(rs1 r0) "ADDI") ;; move r0 to r1
      (,(rd r0) ,(rs1 r) "ADDI") ;; move r to r0
      ))) ;;(,(string-append "xchg___%" r0 ",%" r1))

;; signed
(define (riscv64:g?->r info)
  (let* ((r (get-r info)))
    `(("# riscv64:g?->r")
      (,(string-append "; this is the register: " r)))))

(define (riscv64:ge?->r info)
  ;; sete tests whether the zero flag is set
  (let* ((r (get-r info))
         ;(l (e->l r))
         )
    `(("# riscv64:ge?->r")
      (,(string-append "; this is the register: " r))

      ;;(,(string-append "setge__%" l))
      ;;(,(string-append "movzbl_%" l ",%" r))
      )))

(define (riscv64:l?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "setl___%" r))
      (,(string-append "movzbl_%" r ",%" r)))))

(define (riscv64:le?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "setle__%" r))
      (,(string-append "movzbl_%" r ",%" r)))))

;; unsigned
(define (riscv64:a?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "seta___%" r))
      (,(string-append "movzbl_%" r ",%" r)))))

(define (riscv64:ae?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "setae__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:b?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "setb___%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:be?->r info)
  (let* ((r (get-r info)))
    `((,(string-append "setbe__%" l))
      (,(string-append "movzbl_%" l ",%" r)))))

(define (riscv64:test-r info)
  ;; janneke: it's true if r is zero
  ;; https://en.wikipedia.org/wiki/TEST_(x86_instruction)
  ;; bit-wise AND for the two operands
  (let ((r (get-r info)))
    `(("# riscv64:test-r")
      (,(string-append "RD_T6 RS1_" r " SLTIU"))
      ;;(,(string-append "test___%" r ",%" r))
      )))

(define (riscv64:r->label info label)
  ;; store value in r to label?
  ;; this doesn't seem to make much sense... since we get r from the unused registers?
  ;; what am i missing!?
  (let ((r (get-r info)))
    `(("# riscv64:r->label")
      ("RD_T0" (#:address ,label) "ADDI")
      (,(rs2 r) "RS1_T0 SD"))))

(define (riscv64:r->byte-label info label)
  ;; fill r with label address byte?
  (let* ((r (get-r info))
         ;(l (e->l r))
         )
    `(("# riscv64:r->byte-label")
      ("RD_T0" (#:address ,label) "ADDI") ;; load label into register t0
      ("RD_T0" ,(rs1 r) "SB") ;; store content of r in address (t0)
      ;(,(string-append "movb___%" l ",0x32") (#:address ,label))
      )))

(define (riscv64:r->word-label info label)
  ;; see above
  (let* ((r (get-r info))
        ;(x (e->x r))
         )
    `(("# riscv64:r->word-label")
      ("RD_T0" (#:address ,label) "ADDI") ;; load label into register t0
      ("RD_T0" ,(rs1 r) "SH") ;; store content of r in address (t0)

      ;(,(string-append "movw___%" x ",0x32") (#:address ,label))
      )))

(define (riscv64:r->long-label info label)
  ;; see above
  (let* ((r (get-r info))
        ;(x (e->x r))
         )
    `(("# riscv64:r->long-label")
      ("RD_T0" (#:address ,label) "ADDI") ;; load label into register t0
      ("RD_T0" ,(rs1 r) "SD") ;; store content of r in address (t0)
      ;(,(string-append "movw___%" x ",0x32") (#:address ,label))
      )))

(define (riscv64:call-r info n)
  ;; any different from: riscv64:jump ?
  ;; ???

  ;; Saves procedure linking information on the stack and branches to the called procedure specified using the target operand. The target operand specifies the address of the first instruction in the called procedure. The operand can be an immediate value, a general-purpose register, or a memory location.
  (let ((r (get-r info)))
    `(("# riscv64:call-r")
      ("RD_A7" ,(rs1 r) "ADDI")
      ("JALR")
      ;;(,(string-append "call___*%" r))
      ;;("add____$i8,%esp" (#:immediate1  ,(* n 4)))
      ;; TODO
      )))

(define (riscv64:r0*r1 info)
  ;; simple multiplication ?
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0*r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "MUL"))))

(define (riscv64:r0<<r1 info)
  ;; left-shift!
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0<<r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SLL"))))

(define (riscv64:r0>>r1 info)
  ;; right-shift!
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0>>r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SRL"))))

(define (riscv64:r0-and-r1 info)
  ;; logical AND (or is it bitwise?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-and-r1")
      ("RD_T6" ,(rs1 r0) ,(rs2 r1) "AND")))) ;,(string-append "and____%" r1 ",%" r0)

(define (riscv64:r0/r1 info signed?)
  ;; simple division
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0/r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "DIV"))))

(define (riscv64:r0%r1 info signed?)
  ;; modulo
  ;; TODO
  (let ((allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0%r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "REM"))))

(define (riscv64:r+value info v)
  ;; add immediate to register
  (let ((r (get-r info)))
    `(("# riscv64:r+value")
      (,(rd r) ,(rs1 r) (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v) "ADDI"))))

(define (riscv64:r0->r1-mem info)
  ;; store value in r0 at address r1 ?
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:r0->r1-mem")
      (,(rd r1) ,(rs1 r0) "SD"))))

(define (riscv64:word-r0->r1-mem info)
  ;; see above
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:word-r0->r1-mem")
      (,(rd r1) ,(rs1 r0) "SH"))))

(define (riscv64:quad-r0->r1-mem info)
  ;; see above
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:quad-r0->r1-mem")
      (,(rd r1) ,(rs1 r0) "SQ"))))

(define (riscv64:r-cmp-value info v)
  ;; compare value in register?
  (let ((r (get-r info))
        (r2 (get-r info)))
    `(("# riscv64:r-cmp-value")
      (,(rd r2) (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v) "ADDI")
      (,(rd r2) ,(rs1 r) ,(rs2 r2) "SUB")
      ()
      ;; ,(if (< (abs v) #x80)
      ;;      `(,(string-append "cmp____$i8,%" r) (#:immediate1 ,v))
      ;;      `(,(string-append "cmp____$i32,%" r) (#:immediate ,v)))
      )))

(define (riscv64:push-register info r)
  ;; push to stack
  `(("# riscv64:push-register")
    ("RD_SP" ,(rs1 r) "SD")
    ("RD_SP !-0x8 ADDI")))

(define (riscv64:pop-register info r)
  ;; pop from stack
  `(("# riscv64:pop-register")
    ("RD_SP" ,(rs1 r) "LD")
    ("RD_SP !0x8 ADDI")
    ;(,(string-append "pop____%" r))
    ))

(define (riscv64:return->r info)
  ;; write return value to register
  (let ((r (get-r info)))
    ;;(if (equal? r "RS_A0") '())
    `(("# riscv64:return->r")
      ("RD_A0" ,(string-append  "RS1_" r) "ADDI"))))

(define (riscv64:r0-or-r1 info)
  ;; logical OR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-or-r1")
      ("RD_T6" ,(rs1 r0) ,(rs2 r1) "OR")))) ;,(string-append "or_____%" r1 ",%" r0)

(define (riscv64:shl-r info n)
  ;; shift left ;; not the same as << !
  (let ((r (get-r info)))
    `(("# riscv64:shl-r")
      (,(rd r) (#:immediate1 ,n) "SLLI")
      ;;(,(string-append "shl____$i8,%" r) (#:immediate1 ,n))
      )))

(define (riscv64:r+r info)
  ;; add r+r, which is the same as (* 2 r)
  (let ((r (get-r info)))
    `(("# riscv64:r+r")
      (,(rd r) ,(rs1 r) ,(rs2 r) "ADD")))) ;,(string-append "add____%" r ",%" r)

(define (riscv64:not-r info)
  ;; bitwise NOT operation (inverts every bit in the register)
  (let ((r (get-r info)))
    `(("# riscv64:not-r")
      ("RD_T6" ,(rs1 r) (#:immediate -1) "XORI"))));,(string-append "not____%" r)

(define (riscv64:r0-xor-r1 info)
  ;; XOR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("RD_T6" ,(rs1 r0) ,(rs2 r1) "XOR")))) ;;(,(string-append "xor____%" r1 ",%" r0))

(define (riscv64:r0-mem->r1-mem info)
  ;; store datum at location r0 to location r1
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `(("# riscv64:r0-mem->r1-mem")
      (,(rd r2) ,(rs1 r0) "LD") ;; load value at memory location r0 to r2
      (,(rd r1) ,(rs1 r2) "SD") ;; store the value to location in r1
      )))

(define (riscv64:byte-r0-mem->r1-mem info)
  ;; see above
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers))
         ;;(l2 (e->l r2))
         )
    `(("# riscv64:byte-r0-mem->r1-mem")
      (,(rd r2) ,(rs1 r0) "LB") ;; load value at memory location r0 to r2
      (,(rd r1) ,(rs1 r2) "SB") ;; store the value to location in r1
      )))

(define (riscv64:word-r0-mem->r1-mem info)
  ;; see above
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `(("# riscv64:word-r0-mem->r1-mem")
      (,(rd r2) ,(rs1 r0) "LH") ;; load value at memory location r0 to r2
      (,(rd r1) ,(rs1 r2) "SH") ;; store the value to location in r1
      )))

(define (riscv64:r0+value info v)
  ;; add immediate to r0 ?
  (let ((r0 (get-r0 info)))
    `(("# riscv64:r0+value")
      (,(rd r0) (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v) "ADDI"))))

(define (riscv64:value->r0 info v)
  ;; load value into r0
  (let ((r0 (get-r0 info)))
    `(("# riscv64:value->r0")
      (,(rd r0) (#:immediate ,v) "ADDI"))))

(define (riscv64:byte-r->local+n info id n)
  ;; store byte at local+n (base pointer + n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    `(("# riscv64:byte-r->local+n")
      ("RS1_TP" ,(rs2 r)
       (,(if (< (abs n) #x80)
             #:immediate1
             #:immediate)
        ,n)
       "SB"))))

(define (riscv64:word-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    `(("# riscv64:word-r->local+n")
      ("RS1_TP" ,(rs2 r) (#:immediate ,n) "SW"))))
      ;; ,(if (< (abs n) #x80)
      ;;      `(,(string-append "mov____%" x ",0x8(%ebp)") (#:immediate1 ,n))
      ;;      `(,(string-append "mov____%" x ",0x32(%ebp)") (#:immediate ,n)))

(define (riscv64:long-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    `(("# riscv64:long-r->local+n")
      ("RS1_FP" ,(rs2 r) (#:immediate1 ,n) "SD"))))

(define (riscv64:r-and info v)
  ;; AND register immediate
  (let ((r (get-r info)))
    `(("# riscv64:r-and")
      ("RD_T6" ,(rs1 r) (#:immediate ,v) "ANDI")))) ;,(string-append "and____$i32,%" r) (#:immediate ,v)

(define (riscv64:push-r0 info)
  (let ((r0 (get-r0 info)))
    `(("# riscv64:push-r0") ;;
      ("RD_SP !-8 ADDI")    ;; grow stack
      (,(string-append "RD_SP RS1_" r0 " SD"))))) ;; store value

(define (riscv64:r1->r0 info)
  ;; move r1 to r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r1->r0")
      (,(rd r1) ,(rs1 r0) "ADDI"))))

(define (riscv64:pop-r0 info)
  ;; pop stack into r0
  (let ((r0 (get-r0 info)))
    `(("# riscv64:pop-r0")
      (,(rd r0) "RS1_SP LD") ;;,(string-append "pop____%" r0)
      ("RD_SP !8 ADDI")))) ;; shrink stack

(define (riscv64:swap-r-stack info)
  ;; swaps register content with stack value
  (let ((r (get-r info)))
    `(("# riscv64:swap-r-stack")
      (,(string-append "xchg___%" r ",(%esp)")))))

(define (riscv64:swap-r1-stack info)
  ;; swap content of register with top of stack
  ;; name vs register used
  (let ((r0 (get-r0 info))
        (r (get-r info)))
    `(("# riscv64:swap-r1-stack")
      (,(rd r) "RS1_SP" "LD")
      ("RD_SP" ,(rs1 r0) "SD")
      (,(rd r0) ,(rs1 r) "ADDI")))) ;;(,(string-append "xchg___%" r0 ",(%esp)"))

(define (riscv64:r2->r0 info)
  ;; kinda useless on RISC-V with all those registers, no?
  ;; TODO
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


;; comparison instructions
(define (riscv64:eq info)
  ;; sets T6 to 1 if values in the registers are equal
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:eq")
      (,(rd r1) ,(rs1 r1) ,(rs2 r0) "SUB")
      ("RD_T6" ,(rs1 r1) "!0x1 SLTIU"))))

(define (riscv64:ne info)
  ;; sets T6 to 1 if values in the registers are not equal
  ;; same as riscv64:eq but inverts the end result
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:ne")
      (,(rd r1) ,(rs1 r1) ,(rs2 r0) "SUB")
      ("RD_T6" ,(rs1 r1) "!0x1 SLTIU")
      ("RD_T6 RS1_T6 !0x-1 XORI")))) ;; inversion of the result

(define (riscv64:le info)
  ;; compares values in two registers and sets T6 accordingly
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:le")
      ("RD_T6" ,(rs1 r0) ,(rs2 r1)
       (#:immediate1 1)
       "SLTI")
      ;; (,(rd r1 " RS1_" r1 " RS2_" r0) "SUB")
      ;; ("RD_T0" ,(rs1 r1) "!0x1 SLTIU")
      )))

(define (riscv64:lt info)
  ;; compares values in two registers and sets T6 accordingly
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:lt")
      ("RD_T6" ,(rs1 r0) ,(rs2 r1) "SLT"))))

(define (riscv64:gt info)
  ;; compares values in two registers and sets T6 accordingly
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:gt")
      ("RD_T6" ,(rs1 r1) ,(rs2 r0) "SLT"))))

(define (riscv64:ge info)
  ;; compares values in two registers and sets T6 accordingly
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:ge")
      ("RD_T6" ,(rs1 r1) ,(rs2 r0) "SLT"))))

(define (riscv64:beq info v label)
  ;; branches to label if r0 is equal to v
  (let* ((r0 (get-r info))
         (r "T5"))
    `(("# riscv64:beq")
      (,(rd r) (#:immediate ,v) "ADDI") ;; load value into free register
      (,(rs1 r0) ,(rs2 r) (#:address ,label) "BEQ")))) ;; conditional branch to label

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
    (long-r->local+n . ,riscv64:long-r->local+n)
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
    (r->long-label . ,riscv64:r->word-label)
    (r-and . ,riscv64:r-and)
    (r-byte-mem-add . ,riscv64:r-byte-mem-add)
    (r-cmp-value . ,riscv64:r-cmp-value)
    (r-mem-add . ,riscv64:r-mem-add)
    (r-negate . ,riscv64:r-negate)
    (r-word-mem-add . ,riscv64:r-word-mem-add)
    (r-long-mem-add . ,riscv64:r-long-mem-add)
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
    (long-mem->r . ,riscv64:long-mem->r)
    (long-r . ,riscv64:long-r)
    (long-signed-r . ,riscv64:long-signed-r)
    (xor-zf . ,riscv64:xor-zf)
    (zf->r . ,riscv64:zf->r)
    (ne . ,riscv64:ne)
    (eq . ,riscv64:eq)
    (le . ,riscv64:le)
    (lt . ,riscv64:lt)
    (gt . ,riscv64:gt)
    (ge . ,riscv64:ge)
    (beq . ,riscv64:beq)
    (quad-r0->r1-mem . ,riscv64:r0->r1-mem) ;; todo! store quad
    ))
