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
;;; We're following this Chapter 18 of the RISC-V Reference (Calling Convention):
;;; https://riscv.org/wp-content/uploads/2015/01/riscv-calling.pdf 
;;; and the explanation of LP64 here: https://en.wikipedia.org/wiki/64-bit_computing#64-bit_data_models

;;; Initialize MesCC as RISC-V64 compiler

;;; Code:

(define-module (mescc riscv64 info)
  #:use-module (mescc info)
  #:use-module (mescc riscv64 as)
  #:export (riscv64-info))

;; register dedication:
;; RA (x1) :: return address
;; SP (x2) :: stack pointer
;; FP/S0 (x8) :: frame/base pointer
;; A0 (x10) :: function argument / return value
;; A1 (x11) :: function argument / return value
;; A2 (x12) :: function argument
;; ..
;; A7 (x17) :: function argument
;; T4 (x29) :: comparison temporary (where results of subtractions get to be) ? necessary?
;; T5 (x30) :: jump destination
;; T6 (x31) :: for FLAG register replacement


(define (riscv64-info)
  (make <info>
    #:types riscv64:type-alist
    #:registers riscv64:registers
    #:instructions riscv64:instructions))

(define riscv64:registers
  ;; these are meant to be freely allocateable, general use registers
  '("A0" "T0" "T1" "T2" "T3"))
;; the first of these registers is used to pass return values (equivalent to eax in x86)
;; T4: comparison temp storage ? necessary?
;; T5: jump destination register
;; T6: truth storage (after comparison operations, like flag register on x86)

(define riscv64:type-alist
  `(("char" . ,(make-type 'signed 1 #f))
    ("short" . ,(make-type 'signed 2 #f))
    ("int" . ,(make-type 'signed 4 #f))
    ("long" . ,(make-type 'signed 8 #f))
    ("default" . ,(make-type 'signed 4 #f))
    ("*" . ,(make-type 'unsigned 8 #f))
    ("long long" . ,(make-type 'signed 8 #f))
    ("long long int" . ,(make-type 'signed 8 #f))

    ("void" . ,(make-type 'void 1 #f))
    ("signed char" . ,(make-type 'signed 1 #f))
    ("unsigned char" . ,(make-type 'unsigned 1 #f))
    ("unsigned short" . ,(make-type 'unsigned 2 #f))
    ("unsigned" . ,(make-type 'unsigned 4 #f))
    ("unsigned int" . ,(make-type 'unsigned 4 #f))
    ("unsigned long" . ,(make-type 'unsigned 4 #f))

    ("unsigned long long" . ,(make-type 'unsigned 8 #f))
    ("unsigned long long int" . ,(make-type 'unsigned 8 #f))

    ("float" . ,(make-type 'float 4 #f))
    ("double" . ,(make-type 'float 8 #f))
    ("long double" . ,(make-type 'float 16 #f))

    ("short int" . ,(make-type 'signed 2 #f))
    ("unsigned short int" . ,(make-type 'unsigned 2 #f))
    ("long int" . ,(make-type 'signed 4 #f))
    ("unsigned long int" . ,(make-type 'unsigned 4 #f))))
