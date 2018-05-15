(defun myLength(list)
	(if list
		(+ 1 (myLength (cdr list)))
		0
	)
)

(defun printList(list)
	(if list
		(progn
			(print (car list))
			(printList (cdr list))
		)
	
	)
)


(defun sumlist(list)
	(if list
		(+ (car list) (sumlist (cdr list)))
		0
	)
)


(defun dupli(list)
	(if list
		(list (list (car list) (car list)
			(dupli (cdr list))))
		
	
	
	)
)

(mapcar (lambda(x) (+ (* (/ 9 5) x) 32)) `(10 40 36))

#|
(defun sumNumeric(list)
	(if list
		(if (numberp (car list))
			(+ (car list) (sumNumeric (cdr list)))
			(sumNumeric (cdr list))
		)
	)		
)
	

(defun sumNumeric(list)
	(cond
		((null list)
			0
			((numberp (car list))
				(+ (car list) (sumNumeric (cdr list)))
			(t (sumNumeric (cdr list)))
	)
)))
|#
		
(print (myLength `(1 2 3 4)))
(print (printList `(1 2 3 4)))
(print (sumlist `(1 2 3 4)))
