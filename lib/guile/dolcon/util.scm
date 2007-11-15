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

(define-module (dolcon util))
(use-modules (dolcon ui))

(define fnetnodes '())
(define loop-procs '())
(define fn-procs '())
(define msg-procs '())
(define timeouts '())

(define-public dc-fn-update
  (lambda ()
    (set! fnetnodes
	  (let ((resp (dc-ecmd "lsnodes")) (er #f))
	    (if (and resp (begin (set! er (dc-extract resp)) er) (= (cdr (assq 'code er)) 200))
		(map (lambda (o)
		       (apply (lambda (id net name users state uid)
				(cons id
				      (list (cons 'id id)
					    (cons 'net net)
					    (cons 'name name)
					    (cons 'users users)
					    (cons 'state (list-ref '(syn hs est dead) state))
					    (cons 'uid uid))))
			      o))
		     (dc-intall resp))
		'())))
    fnetnodes))

(define-public dc-fn-getattrib
  (lambda (id attrib)
    (if (not (assq id fnetnodes))
	(dc-fn-update))
    (let ((aform (assq id fnetnodes)))
      (if aform
	  (cdr (assq attrib (cdr aform)))
	  #f))))

(define-public dc-fn-getname
  (lambda (id)
    (dc-fn-getattrib id 'name)))

(define-public dc-getfnetnodes
  (lambda ()
    (map (lambda (o) (car o))
	 fnetnodes)))

(define fn-updattr
  (lambda (id attr val)
    (let ((aform (assq id fnetnodes)))
      (if aform
	  (set-cdr! (assq attr (cdr aform)) val)
	  #f))))

(define-public dc-fnproc-reg
  (lambda (event proc)
    (set! fn-procs (cons (list event proc)
			 fn-procs))))

(define dc-handle-fn
  (lambda ()
    (dc-fn-update)
    (let* ((notify (lambda (event data) (for-each (lambda (o) (if (eq? event (car o)) ((cadr o) data))) fn-procs)))
	   (ua (lambda (r a) (let* ((ires (dc-intresp r))
				    (hubform (assq (car ires) fnetnodes)))
			       (if hubform
				   (begin (fn-updattr (car ires) a (cadr ires))
					  (notify a (cdr (assq (car ires) fnetnodes)))))))))
      (dc-loop-reg ".notify" 601 (lambda (r er) (let* ((ires (dc-intresp r))
						       (hubform (assq (car ires) fnetnodes)))
						  (if hubform
						      (begin (fn-updattr (car ires) 'state (list-ref '(syn hs est dead) (cadr ires)))
							     (notify 'state (cdr hubform)))))))
      (dc-loop-reg ".notify" 602 (lambda (r er) (ua r 'name)))
      (dc-loop-reg ".notify" 605 (lambda (r er) (ua r 'users)))
      (dc-loop-reg ".notify" 604 (lambda (r er)
				   (let* ((ires (dc-intresp r))
					  (new (list (cons 'id (car ires))
						     (cons 'net (cadr ires))
						     (cons 'name "")
						     (cons 'users 0)
						     (cons 'state 'syn))))
				     (set! fnetnodes
					   (cons (cons (car ires) new)
						 fnetnodes))
				     (notify 'creat new))))
      (dc-loop-reg ".notify" 603 (lambda (r er)
				   (let* ((ires (dc-intresp r)) (nform (assq (car ires) fnetnodes)))
				     (notify 'dstr (cdr nform))
				     (set! fnetnodes (delq nform fnetnodes))))))))

(define-public dc-msgproc-reg
  (lambda (proc)
    (set! msg-procs (cons proc msg-procs))))

(define dc-handle-msg
  (lambda ()
    (dc-loop-reg ".notify" 640 (lambda (r er)
				 (let ((sender (cadadr (assq 'resp er)))
				       (message (cddadr (assq 'resp er))))
				   (for-each (lambda (o) (o sender message))
					     msg-procs))))))

(define-public dc-util-handle
  (lambda what
    (for-each (lambda (o)
		(case o
		  ((fn) (dc-handle-fn))
		  ((msg) (dc-handle-msg))))
	      what)))

(define-public dc-timeout
  (lambda (rel timeout proc)
    (let* ((tf (gettimeofday))
	   (t (+ (car tf) (/ (cdr tf) 1000000))))
      (set! timeouts (merge timeouts (list (cons (if rel (+ timeout t) timeout) proc))
			    (lambda (a b) (< (car a) (car b))))))))

(define-public dc-loop-reg
  (lambda (cmd code proc)
    (set! loop-procs (cons (cons (cons cmd code) proc)
			   loop-procs))))

(define-public dc-loop
  (lambda ()
    (while #t
	   (dc-select (if (eq? timeouts '())
			  10000
			  (let* ((tf (gettimeofday))
				 (t (+ (car tf) (/ (cdr tf) 1000000)))
				 (dt (- (caar timeouts) t)))
			    (if (< dt 0) 0 (truncate (inexact->exact (* dt 1000)))))))
	   (while (let ((resp (dc-getresp)))
		    (if resp
			(let* ((er (dc-extract resp)) (code (cdr (assq 'code er))) (cmd (cdr (assq 'cmd er))))
			  (for-each
			   (lambda (o)
			     (if (and (or (not (caar o)) (equal? cmd (caar o)))
				      (or (not (cdar o)) (equal? code (cdar o))))
				 ((cdr o) resp er)))
			   loop-procs))
			#f))
		  #f)
	   (while (and (not (eq? timeouts '()))
		       (let* ((tf (gettimeofday))
			     (t (+ (car tf) (/ (cdr tf) 1000000))))
			 (>= t (caar timeouts))))
		  ((cdar timeouts))
		  (set! timeouts (cdr timeouts)))
	   (for-each (lambda (o)
		       (if (equal? (caar o) ".periodic")
			   ((cdr o))))
		     loop-procs))))
