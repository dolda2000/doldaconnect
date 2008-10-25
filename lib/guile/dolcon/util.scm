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
(define transfers '())
(define loop-procs '())
(define fn-procs '())
(define trn-procs '())
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

(define-public dc-tr-update
  (lambda ()
    (set! transfers
	  (let* ((resp (dc-ecmd "lstrans")) (er (dc-extract resp)))
	    (if (= (cdr (assq 'code er)) 200)
		(map (lambda (o)
		       (apply (lambda (id dir state peerid peernick path size curpos hash)
				(cons id
				      (list (cons 'id id)
					    (cons 'dir (if (= dir 1) 'up 'down))
					    (cons 'state (list-ref '(wait hs main done) state))
					    (cons 'peer peerid)
					    (cons 'nick peernick)
					    (cons 'path path)
					    (cons 'size size)
					    (cons 'pos curpos)
					    (cons 'hash hash))))
			      o))
		     (dc-intall resp))
		'())))
    transfers))

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
    (map cdr fnetnodes)))

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

(define-public dc-trnproc-reg
  (lambda (event proc)
    (set! trn-procs (cons (list event proc)
			  trn-procs))))

(define-public dc-gettransfers
  (lambda ()
    (map cdr transfers)))

(define dc-handle-trn
  (lambda ()
    (dc-tr-update)
    (let* ((notify (lambda (event data) (for-each (lambda (o) (if (or (not (car o)) (eq? event (car o))) ((cadr o) data))) trn-procs)))
	   (update (lambda (tf attr val)
		     (set-cdr! (assq attr (cdr tf)) val)
		     (notify attr (cdr tf))))
	   (ua (lambda (r a) (let* ((ires (dc-intresp r))
				    (trform (assq (car ires) transfers)))
			       (if trform (update trform a (cadr ires)))))))
      (dc-loop-reg ".notify" 610 (lambda (r er) (let* ((ires (dc-intresp r))
						       (new (list (cons 'id (car ires))
								  (cons 'dir (if (= (cadr ires) 1) 'up 'down))
								  (cons 'state 'wait)
								  (cons 'peer (caddr ires))
								  (cons 'nick (caddr ires))
								  (cons 'path (cadddr ires))
								  (cons 'size -1)
								  (cons 'pos 0)
								  (cons 'hash ""))))
						  (set! transfers
							(cons (cons (car ires) new)
							      transfers))
						  (notify 'creat new))))
      (dc-loop-reg ".notify" 611 (lambda (r er) (let* ((ires (dc-intresp r))
						       (trform (assq (car ires) transfers)))
						  (if trform (update trform 'state (list-ref '(wait hs main done) (cadr ires)))))))
      (dc-loop-reg ".notify" 612 (lambda (r er) (ua r 'nick)))
      (dc-loop-reg ".notify" 613 (lambda (r er) (ua r 'size)))
      (dc-loop-reg ".notify" 614 (lambda (r er) (let* ((ires (dc-intresp r))
						      (trform (assq (car ires)) transfers))
						 (if trform (notify 'error (cons (cdr trform) (list-ref '(#f notfound noslots) (cadr ires))))))))
      (dc-loop-reg ".notify" 615 (lambda (r er) (ua r 'pos)))
      (dc-loop-reg ".notify" 616 (lambda (r er) (ua r 'path)))
      (dc-loop-reg ".notify" 617 (lambda (r er) (let* ((ires (dc-intresp r))
						      (trform (assq (car ires) transfers)))
						 (if trform
						     (begin (notify 'dstr (cons (cdr trform) (cadr ires)))
							    (set! transfers (delq trform transfers)))))))
      (dc-loop-reg ".notify" 618 (lambda (r er) (ua r 'hash))))))

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
		  ((msg) (dc-handle-msg))
		  ((trn) (dc-handle-trn))))
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
