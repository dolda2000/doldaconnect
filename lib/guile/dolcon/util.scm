(define-module (dolcon util))
(use-modules (dolcon ui))

(define fnetnodes '())

(define-public dc-fn-update
  (lambda ()
    (set! fnetnodes
	  (let ((resp (dc-ecmd "lsnodes")) (er #f))
	    (if (and resp (begin (set! er (dc-extract resp)) er) (= (cdr (assq 'code er)) 200))
		(map (lambda (o)
		       (apply (lambda (id net name users state)
				(cons id
				      (list (cons 'id id)
					    (cons 'net net)
					    (cons 'name name)
					    (cons 'users users)
					    (cons 'state (list-ref '(syn hs est dead) state)))))
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
