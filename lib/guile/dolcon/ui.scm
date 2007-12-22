;  Dolda Connect - Modular multiuser Direct Connect-style client
;  Copyright (C) 2007 Fredrik Tolf <fredrik@dolda2000.com>
;  
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;  
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(define-module (dolcon ui))

(export dc-connect dc-disconnect dc-connected dc-select dc-getresp dc-extract dc-intresp dc-qcmd dc-loginasync dc-lexsexpr dc-checkproto)

(load-extension "libdolcon-guile" "init_guiledc")

(define-public dc-login
  (lambda (useauthless . username)
    (let ((done #f) (errsym #f))
      (dc-loginasync
       (lambda (err reason)
	 (set! errsym err)
	 (set! done #t))
       useauthless (if (pair? username) (car username) #f))
      (while (not done) (dc-select))
      errsym)))

(define-public dc-must-connect
  (lambda (host . version)
    (let* ((fd (dc-connect host))
	   (ores (do ((resp (dc-getresp) (dc-getresp)))
		     ((and resp
			   (equal? (cdr (assoc 'cmd (dc-extract resp))) ".connect"))
		      resp)
		   (dc-select)))
	   (resp (dc-extract ores)))
      (if (not (= (cdr (assoc 'code resp)) 201))
	  (throw 'bad-return (cdr (assoc 'code resp)) (cadr (assoc 'resp resp)))
	  (if (dc-checkproto ores (if (pair? version) (car version) dc-latest))
	      fd
	      (throw 'bad-protocol ores))
	  )
      )
    )
  )

(define-public dc-c&l
  (lambda (verbose host useauthless)
    (let ((fd -1) (print (lambda (obj) (if verbose (display obj (if (port? verbose) verbose (current-error-port)))))))
      (print "connecting...\n")
      (set! fd (dc-must-connect host))
      (print "authenticating...\n")
      (let ((ret (dc-login useauthless)))
	(if (not (eq? ret 'success))
	    (throw 'login-failure ret)))
      (print "authentication success\n")
      fd)
    )
  )

(define-public dc-ecmd
  (lambda args
    (let ((tag (dc-qcmd args)))
      (do ((resp (dc-getresp tag) (dc-getresp tag)))
	  (resp resp)
	(dc-select))
      )
    )
  )

(define-public dc-ecmd-assert
  (lambda (code . args)
    (let* ((resp (apply dc-ecmd args)) (eresp (dc-extract resp)))
      (if (not (if (list? code)
		   (memq (cdr (assoc 'code eresp)) code)
		   (= (cdr (assoc 'code eresp)) code)))
	  (throw 'bad-return (cdr (assoc 'code eresp)) (cadr (assoc 'resp eresp)))
	  )
      resp
      )
    )
  )

(define-public dc-intall
  (lambda (resp)
    (let ((retlist '()))
      (do ((ires (dc-intresp resp) (dc-intresp resp))) ((not ires) retlist)
	(set! retlist (append retlist (list ires)))))))
