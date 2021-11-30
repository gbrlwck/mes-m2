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

;; register use:
;;   T6 :: FLAGS replacement
;;   T5 :: ad hoc temp register (no reliance on value in there outside of function defined here)
;;   T4 :: second ad hoc temp reg

(define regsize 8) ;; register size in bytes

(define (rd r)
  (string-append "RD_" r))
(define (rs1 r)
  (string-append "RS1_" r))
(define (rs2 r)
  (string-append "RS2_" r))

;; stack operations
  ;; convention: SP points to last pushed element
(define (riscv64:push-register info r)
  ;; push register content to stack
  `(("# riscv64:push-register")
    ("RD_SP !-0x8 ADDI")
    ("RS1_SP" ,(rs2 r) "SD")))
(define (riscv64:pop-register info r)
  ;; pop from stack into register r
  `(("# riscv64:pop-register")
    (,(rd r) "RS1_SP" "LD")
    ("RD_SP !8 ADDI")))
(define (riscv64:push-r0 info)
  (let ((r0 (get-r0 info)))
    `(("# riscv64:push-r0") ;;
      ("RD_SP !-8 ADDI")    ;; grow stack
      (,(string-append "RD_SP RS1_" r0 " SD"))))) ;; store value
(define (riscv64:pop-r0 info)
  ;; pop stack into r0
  (let ((r0 (get-r0 info)))
    `(("# riscv64:pop-r0")
      (,(rd r0) "RS1_SP LD") ;;,(string-append "pop____%" r0)
      ("RD_SP !8 ADDI")))) ;; shrink stack

(define (riscv64:swap-r1-stack info)
  ;; swap content of register with top of stack
  (let ((r (get-r info)))
    `(("# riscv64:swap-r1-stack")
      ("RD_T5" "RS1_SP" "LD")
      ("RS1_SP" ,(rs2 r) "SD")
      (,(rd r) "RS1_T5" "ADDI"))))

;; register operations
(define (riscv64:value->r0 info v)
  ;; load value into r0
  (let ((r0 (get-r0 info)))
    `(("# riscv64:value->r0")
      (,(rd r0) (#:immediate ,v) "ADDI"))))
(define (riscv64:value->r info v)
  ;; store value in register
  ;; if the immediate exceeds 12 bit values, we should implement a two-instruction assembly: AUI, ADDI
  ;; how can a number be split up / right shifted?
  (let (;; (val (if (> (abs v) #x7ff)
        ;;          (#:immediate ,v)
        ;;          )) ;; get immediate string
        (r (get-r info)))
    (if #f ;;(or (> v #x7ff) (< v 0))
        `(("# riscv64:value->r")
          (,(rd r) (#:immediate ,v) "AUI"))
        `(("# riscv64:value->r")
          (,(rd r) (#:immediate1 ,v) "ADDI")))))
(define (riscv64:r0+value info v)
  ;; add immediate to r0 ?
  (let ((r0 (get-r0 info)))
    `(("# riscv64:r0+value")
      (,(rd r0) (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v) "ADDI"))))
(define (riscv64:r0->r1 info)
  ;; load value in register r0 into r1 -- simple mov
  ;; practically useless on RISC-V
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0->r1")
      (,(rd r0) ,(rs1 r1) "ADDI"))))
(define (riscv64:r1->r0 info)
  ;; move r1 to r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r1->r0")
      (,(rd r1) ,(rs1 r0) "ADDI"))))
(define (riscv64:r2->r0 info)
  ;; kinda useless on RISC-V with all those registers, no?
  ;; TODO
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(,(rd r0) ,(rs1 r1) "ADDI")))
(define (riscv64:swap-r0-r1 info)
  ;; swaps content of registers
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:swap-r0-r1")
      ("RD_T5" ,(rs1 r1) "ADDI") ;; move r1 to r
      (,(rd r1) ,(rs1 r0) "ADDI") ;; move r0 to r1
      (,(rd r0) "RS1_T5" "ADDI")))) ;; move r to r0

;; function calling
(define (riscv64:function-preamble . rest)
  '(("# riscv64:function-preamble")
    ;; push frame pointer to stack
    ("RD_SP RS1_SP !-16 ADDI")  ;; grow stack
    ("RS1_SP RS2_FP SD")        ;; save fp
    ("RS1_SP RS2_RA !8 SD")     ;; save ra
    ("RD_FP RS1_SP !16 ADDI"))) ;; copy previous stack pointer to fp
(define (riscv64:function-locals . rest)
  ;; allocate stack space for local variables
  ;; #:immediate parses hex2 ready 32-bit value -- though ADDI only manages 12-bit immediates
  `(("# riscv64:function-locals")
    ("RD_SP" (#:immediate
              ,(+ (* 4 1025) (* 20 regsize))) "ADDI"))) ; 4*1024 buf, 20 local vars

(define (riscv64:r->arg info i)
  ;; push register on stack -- argument passing
  (let ((r (get-r info)))
    `(("# riscv64:r->arg")
      ("RD_SP !-0x8 ADDI")
      ("RD_SP" ,(rs1 r) "SD"))))
(define (riscv64:label->arg info label i)
  ;; store global onto stack for argument passing
  `(("# riscv64:label->arg")
    ("RD_SP !-0x8 ADDI") ;; increment stack pointer
    ("RD_T5" (#:address ,label) "ADDI") ;; store value in temp register
    ("RS1_SP RS2_T5 SD")))

(define (riscv64:call-r info n)
  ;; call address in register; set ra register

  ;; Saves procedure linking information on the stack and branches to the called procedure specified using the target operand. The target operand specifies the address of the first instruction in the called procedure. The operand can be an immediate value, a general-purpose register, or a memory location.
  (let ((r (get-r info)))
    `(("# riscv64:call-r")
      ("RD_RA" ,(rs1 r) "JALR")))) ;; jump to address in r, set link register to current position
(define (riscv64:return->r info)
  ;; write return value to register A0
  (let ((r (get-r info)))
    (if (equal? r "A0")
        '() ;; nothing to be done, return value is at the right position
        `(("# riscv64:return->r")
          ("RD_A0" ,(rs1 r) "ADDI")))))
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
    ("RS1_RA JALR")))      ;; no need to set ra

(define (riscv64:call-label info label n)
  ;; jump to label
  ;; not sure what n is for ? offset maybe?
  ;; TODO :: n ?
  ;; does ra have to be set when calling a label?
  `(("# riscv64:call-label")
    ("RD_RA" (#:address ,label) "JALR")))

;; arithmetic operations
(define (riscv64:r+value info v)
  ;; add immediate to register
  ;; TODO : if bigger than 12bit -> aui, addi
  (let ((r (get-r info)))
    `(("# riscv64:r+value")
      (,(rd r) ,(rs1 r) (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v) "ADDI"))))
(define (riscv64:shl-r info n)
  ;; shift left ;; not the same as << ! ?
  ;; what's the difference?
  (let ((r (get-r info)))
    `(("# riscv64:shl-r")
      (,(rd r) (#:immediate1 ,n) "SLLI"))))
(define (riscv64:r0+r1 info)
  ;; add r1 to r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(rd r0) ,(rs1 r0) ,(rs2 r1) "ADD"))))
(define (riscv64:r0-r1 info)
  ;; subtract r1 from r0
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SUB"))))
(define (riscv64:r0*r1 info)
  ;; simple multiplication, stores the result in r0
  (let (;; (allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0*r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "MUL"))))
(define (riscv64:r0/r1 info signed?)
  ;; simple division
  (let (;;(allocated (.allocated info))
        (r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0/r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) ,(if signed? "DIV" "DIVU")))))
(define (riscv64:r0%r1 info signed?)
  ;; modulo, aka REMainder function
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0%r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "REM"))))
(define (riscv64:r0<<r1 info)
  ;; left-shift
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0<<r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SLL"))))
(define (riscv64:r0>>r1 info)
  ;; right-shift
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0>>r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "SRL"))))
(define (riscv64:r-negate info)
  ;; negate a number
  ;; sete: https://stackoverflow.com/questions/53011701/what-does-the-instruction-sete-do-in-assembly
  ;;  sets argument register to 1 of zero flag is set
  ;; movzbl: https://stackoverflow.com/questions/9317922/what-does-the-movzbl-instruction-do-in-ia-32-att-syntax#9318005
  (let* ((r (get-r info)))
    `(("# riscv64:r-negate")
      (,(rd r) ,(rs2 r) "SUB")))) ;; x0 reference by omitting rs1

;; logical operations
(define (riscv64:r-and info v)
  ;; AND register immediate
  (let ((r (get-r info)))
    `(("# riscv64:r-and")
      ("RD_T6" ,(rs1 r) (#:immediate ,v) "ANDI"))))
(define (riscv64:r0-or-r1 info)
  ;; logical OR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-or-r1")
      ("RD_T6" ,(rs1 r0) ,(rs2 r1) "OR"))))
(define (riscv64:not-r info)
  ;; bitwise NOT operation (inverts every bit in the register)
  (let ((r (get-r info)))
    `(("# riscv64:not-r")
      ("RD_T6" ,(rs1 r) (#:immediate -1) "XORI"))))
(define (riscv64:r0-and-r1 info)
  ;; bitwise! AND is bitwise logical operation
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("# riscv64:r0-and-r1")
      (,(rd r0) ,(rs1 r0) ,(rs2 r1) "AND"))))
(define (riscv64:r0-xor-r1 info)
  ;; XOR
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `(("RD_T6" ,(rs1 r0) ,(rs2 r1) "XOR"))))

;; tests
(define (riscv64:r-zero? info)
  ;; test whether register is equal to zero -> store 1 or 0 in T6
  (let ((r (get-r info)))
    `(("# riscv64:r-zero?")
      ("RD_T6" ,(rs1 r) (#:immediate1 1) "SLTIU"))))

;; memory operations
  ;; loads
(define (riscv64:local->r info n)
  ;; load local variable into register
  (let ((r (get-r info))
        (n (- (* regsize n))))
    `(("# riscv64:local->r")
      (,(rd r)
       "RS1_FP"
       (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n)
       "LD"))))
(define (riscv64:label->r info label)
  ;; load label (address) into register
  ;; TODO -- should we expect that to be bigger than 12bit wide?!
  ;; AUI, ADDI
  (let ((r (get-r info)))
    `(("# riscv64:label->r")
      (,(rd r) (#:address ,label) "ADDI"))))
(define (riscv64:local-ptr->r info n)
  ;; add immediate to frame pointer and store in r (aka pointer to a local variable)
  (let ((r (get-r info))
        (n (- (* regsize n))))
    `(("# riscv64:local-ptr->r")
      (,(rd r)
       "RS1_FP"
       (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n)
       "ADDI"))))
(define (riscv64:byte-mem->r info)
  ;; load byte from memory at r to register r
  ;; sign extension is the default in RISC-V
  (let ((r (get-r info)))
    ;; move byte to word with zero extension
    `(("# riscv64:byte-mem->r")
      (,(rd r) ,(rs1 r) "LBU"))))
(define (riscv64:byte-r info)
  ;; load byte and zero-extend to double
  ;; this is unnecessary in riscv, as we have word-size all the time
  (let* ((r (get-r info)))
    `(("# riscv64:byte-r")
      (,(rd r) ,(rs1 r) "LBU"))))
(define (riscv64:byte-signed-r info)
  ;; load byte+sign extend (this is the default for riscv64)
  (let* ((r (get-r info)))
    `(("# riscv64:byte-signed-r")
      (,(rd r) ,(rs1 r) "LB"))))
(define (riscv64:word-r info)
  (let* ((r (get-r info)))
    `(("# riscv64:word-r")
      (,(rd r) ,(rs1 r) "LWU"))))
(define (riscv64:word-signed-r info)
  (let* ((r (get-r info)))
    `((,(rd r) ,(rs1 r) "LW"))))
(define (riscv64:long-r info)
  ;; load (unsigned) double into register
  (let* ((r (get-r info)))
    `((,(rd r) ,(rs1 r) "LD"))))
(define (riscv64:long-signed-r info)
  ;; load signed long into register
  ;; no unsigned instruction -- nothing gets extended
  (let* ((r (get-r info)))
    `(("# riscv64:long-signed-r")
      (,(rd r) ,(rs1 r) "LD"))))
(define (riscv64:label-mem->r info label)
  ;; load value from label (in memory) to register
  ;; TODO : needs check if label is > 12bit address away (is this possible!?) ?? bytevectors?
  ;; do we need some hex2 hack here!?
  (let ((r (get-r info)))
    `(("# riscv64:label-mem->r")
      (,(rd r) (#:address ,label) "ADDI")
      (,(rd r) ,(rs1 r) "LD"))))
(define (riscv64:word-mem->r info)
  ;; load word size value at address in register into same register
  (let ((r (get-r info)))
    `(("# riscv64:word-mem->r")
      (,(rd r) ,(rs1 r) "LWU"))))
(define (riscv64:long-mem->r info)
  ;; move zero-extended word to long
  (let ((r (get-r info)))
    `(("# riscv64:long-mem->r")
      (,(rd r) ,(rs1 r) "LD"))))
(define (riscv64:mem->r info)
  ;; load value of address in register
  (let ((r (get-r info)))
    `(("# riscv64:mem->r")
      (,(rd r) ,(rs1 r) "LD"))))

  ;; stores
(define (riscv64:r->local info n)
  ;; save content in register r at (- fp (* n 8))
  ;; XLEN=64, hence local variables take 8 bytes of space
  (or n (error "invalid value: riscv64:r->local: " n))
  (let ((r (get-r info))
        (n (- 0 (* regsize n)))) ;; calculate location of local variable
    `(("# riscv64:r->local")
      ("RS1_FP" ,(rs2 r) (#:immediate ,n) "SD"))))
(define (riscv64:r->local+n info id n)
  ;; lookup local variable / get from stack
  ;; save register as local variable ?
  ;; WHICH ONE IS IT!?
  ;; TODO
  (let ((n (+ (- (* 4 id)) n))
        (r (get-r info)))
    `(("# riscv64:r->local+n")
      (,(rd r) "RS1_FP"
       (,(if (< (abs n) #x80) #:immediate1 #:immediate) ,n)
       "LD"))))
(define (riscv64:byte-r0->r1-mem info)
  ;; store lowest byte in r0 to address in r1
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:byte-r0->r1-mem")
      (,(rs1 r1) ,(rs2 r0) "SB"))))
(define (riscv64:r0->r1-mem info)
  ;; store value in r0 at address r1
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
(define (riscv64:long-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    `(("# riscv64:long-r->local+n")
      ("RS1_FP" ,(rs2 r) (#:immediate1 ,n) "SD"))))

(define (riscv64:r->label info label)
  ;; store value in r to address at label (aka save global)
  (let ((r (get-r info)))
    `(("# riscv64:r->label")
      ("RD_T5" (#:address ,label) "ADDI")
      (,(rs2 r) "RS1_T5" "SD"))))
(define (riscv64:r->byte-label info label)
  ;; store byte-wide in r to address at label (global)
  (let* ((r (get-r info)))
    `(("# riscv64:r->byte-label")
      ("RD_T5" (#:address ,label) "ADDI")
      ("RD_T5" ,(rs1 r) "SB"))))
(define (riscv64:r->word-label info label)
  ;; see above
  (let* ((r (get-r info)))
    `(("# riscv64:r->word-label")
      ("RD_T5" (#:address ,label) "ADDI")
      ("RD_T5" ,(rs1 r) "SH"))))
(define (riscv64:r->long-label info label)
  ;; see above
  (let* ((r (get-r info)))
    `(("# riscv64:r->long-label")
      ("RD_T5" (#:address ,label) "ADDI") ;; load label into register t0
      ("RD_T5" ,(rs1 r) "SD")))) ;; store content of r in address (t0)

  ;; advanced
(define (mem-add info v size)
  (let ((r (get-r info)))
    `(("# mem-add")
      ("RD_T5" ,(rs1 r) ,(string-append "L" size))
      ("RD_T5" (#:immediate ,v) "ADDI")
      (,(rs1 r) "RS2_T5" ,(string-append "S" size)))))
(define (riscv64:r-mem-add info v)
  (mem-add info v "D"))
(define (riscv64:r-byte-mem-add info v)
  (mem-add info v "B"))
(define (riscv64:r-word-mem-add info v)
  (mem-add info v "H"))
(define (riscv64:r-long-mem-add info v)
  (riscv64:r-mem-add info v))

(define (riscv64:local-add info n v)
  ;; add v to local variable at BP-n
  (let ((n (- (* regsize n)))
        (c (and (< (abs n) #x80)
                (< (abs v) #x80))))
    `(("# riscv64:local-add") ;; store var addr in r
      ("RD_T5" "RS1_FP" (,(if c #:immediate1 #:immediate) ,n) "LD") ;; load local var to t5
      ("RD_T5" "RS1_T5" (,(if c #:immediate1 #:immediate) ,v) "ADDI") ;; add immediate
      ("RD_FP" "RS1_T5" (,(if c #:immediate1 #:immediate) ,n) "SD")))) ;; save back
(define (riscv64:label-mem-add info label v)
  ;; add value v to memory stored at label
  `(("# riscv64:label-mem-add")
    ("RD_T5" (#:address ,label) "ADDI") ;; write label address to register
    ("RD_T4 RS1_T5 LD") ;; load value at address
    ("RD_T4 RS1_T4"
     (,(if (< (abs v) #x80) #:immediate1 #:immediate) ,v)
     "ADDI") ;; add offset to label address
    ("RS1_T5 RS2_T4 SD"))) ;; store augmented value back to memory

(define (riscv64:r0-mem->r1-mem info)
  ;; store datum at location r0 to location r1
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info)))
    `(("# riscv64:r0-mem->r1-mem")
      ("RD_T5" ,(rs1 r0) "LD")
      (,(rd r1) "RS1_T5" "SD"))))
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
      (,(rd r1) ,(rs1 r2) "SB")))) ;; store the value to location in r1
(define (riscv64:word-r0-mem->r1-mem info)
  ;; see above
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `(("# riscv64:word-r0-mem->r1-mem")
      (,(rd r2) ,(rs1 r0) "LH") ;; load value at memory location r0 to r2
      (,(rd r1) ,(rs1 r2) "SH")))) ;; store the value to location in r1

;; jumps / branches ?
(define (riscv64:jump info label)
  ;; unconditional jump
  `(("# riscv64:jump")
    ("RD_T5" (#:offset ,label) "ADDI")
    ("RS1_T5" "RD_RA" "JALR")))
(define (riscv64:jump-z info label)
  ;; jump if t6 is zero
  `(("# riscv64:jump-z")
    ("RS1_T6" (#:offset ,label) "BEQZ")))
(define (riscv64:jump-nz info label)
  ;; jump if t6 is not zero
  `(("RS1_T6" (#:offset ,label) "BNEZ")))
(define (riscv64:jump-byte-z info label)
  (riscv64:jump-z info label))

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
  (let* ((r0 (get-r info)))
    `(("# riscv64:beq")
      ("RD_T5" (#:immediate ,v) "ADDI") ;; load value into free register
      (,(rs1 r0) "RS2_T5" (#:address ,label) "BEQ")))) ;; conditional branch to label


;; varia
(define (riscv64:nop info)
  '(("# NOP")))
(define (riscv64:test-r info)
  ;; janneke: it's true if r is zero
  ;; https://en.wikipedia.org/wiki/TEST_(x86_instruction)
  ;; bit-wise AND for the two operands
  ;; ?????
  ;; TODO
  (let ((r (get-r info)))
    `(("# riscv64:test-r")
      ("RD_T6" ,(rs1 r) "SLTIU"))))


(define riscv64:instructions
  `((byte-mem->r . ,riscv64:byte-mem->r)
    (byte-r . ,riscv64:byte-r)
    (byte-r->local+n . ,riscv64:byte-r->local+n)
    (byte-r0->r1-mem . ,riscv64:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,riscv64:byte-r0-mem->r1-mem)
    (byte-signed-r . ,riscv64:byte-signed-r)
    (call-label . ,riscv64:call-label)
    (call-r . ,riscv64:call-r)
    (function-locals . ,riscv64:function-locals)
    (function-preamble . ,riscv64:function-preamble)
    (jump . ,riscv64:jump)
    (jump-byte-z . ,riscv64:jump-byte-z)
    (jump-nz . ,riscv64:jump-nz)
    (jump-z . ,riscv64:jump-z)
    (label->arg . ,riscv64:label->arg)
    (label->r . ,riscv64:label->r)
    (label-mem->r . ,riscv64:label-mem->r)
    (label-mem-add . ,riscv64:label-mem-add)
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
    (ne . ,riscv64:ne)
    (eq . ,riscv64:eq)
    (le . ,riscv64:le)
    (lt . ,riscv64:lt)
    (gt . ,riscv64:gt)
    (ge . ,riscv64:ge)
    (beq . ,riscv64:beq)
    (quad-r0->r1-mem . ,riscv64:r0->r1-mem))) ;; todo! store quad
