#! /usr/bin/env guile
--no-auto-compile -e main -L /usr/local/share/guile/site/2.2 -C /usr/local/lib/guile/2.2/site-ccache -s
!#
;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(cond-expand
 (mes)
 (guile
  (define %arch (car (string-split %host-type #\-)))
  (define %kernel (car (filter
                        (compose not
                                 (lambda (x) (member x '("pc" "portbld" "unknown"))))
                        (cdr (string-split %host-type #\-)))))))

(define %prefix (or (getenv "MES_PREFIX")
                      (if (string-prefix? "@prefix" "@prefix@")
                          "."
                          "@prefix@")))

(define %includedir (or (getenv "includedir")
                        (string-append %prefix "/include")))

(define %libdir (or (getenv "libdir")
                    (string-append %prefix "/lib")))

(define %version (if (string-prefix? "@VERSION" "@VERSION@") "git"
                     "@VERSION@"))

(define %arch (if (string-prefix? "@mes_cpu" "x86") %arch
                  "x86"))

(define %kernel (if (string-prefix? "@mes_kernel" "linux") %kernel
                    "linux"))

(define %numbered-arch? (if (getenv "numbered_arch") (and=> (getenv "numbered_arch")
                                                            (lambda (x) (equal? x "true")))
                            (if (string-prefix? "@numbered_arch" "@numbered_arch@") #f
                                (equal? "@numbered_arch@" "true"))))

(setenv "%prefix" %prefix)
(setenv "%includedir" %includedir)
(setenv "%libdir" %libdir)
(setenv "%version" %version)
(setenv "%arch" %arch)
(setenv "%kernel" %kernel)
(setenv "%numbered_arch" (if %numbered-arch? "true" "false"))

(cond-expand
 (mes
  (mes-use-module (mescc))
  (mescc:main (command-line)))
 (guile
  (use-modules (mescc)
               (system vm trace))))

(define TRACE #t)

(define (main args)
  (if TRACE
      (call-with-trace (lambda _ (mescc:main args)))
      (mescc:main args)))
