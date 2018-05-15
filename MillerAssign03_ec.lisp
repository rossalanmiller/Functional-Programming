;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
	Author:			Ross Miller
	Class:			CS 530
	Program Name:	MillerAssign03_ec.lisp
	Purpose:		Introduction to Lisp and functional programming
	Date:			April 2, 2017
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun infix(a_list)
	
	;not sure if good use of lambda, please criticize
	(defun atom_or_list(element)	
		((lambda(x) 
			(if (atom x)
				x			;if the element is a atom then return (base condition)
				(infix x)   ;if the element is a list then run infix on element
			)
		)element)
	)
	
	(list
		(atom_or_list (cadr  a_list))
		(atom_or_list (car   a_list))
		(atom_or_list (caddr a_list))
	)
)



(defun prefix(a_list) 			;prefix and postfix are basically doing the			
	(infix a_list)				;same thing, just swapping the first element
)								;with the second element so we can implement
								;prefix as infix.



(defun apply-ops(a_list) 
	(eval (prefix a_list))		;the eval function takes a list literal and tries 
)								;to evaluate it as a function if it can


(defun keep-order-move(a_list)
	
	;rotates the first element of a_list to the end of a_list
	(defun shift-left-once(a_list)
		(append (cdr a_list) (list (car a_list)))			
	)
	
	;luup recursively constructs a list of lists (alternatively it uses magic)
	(defun luup(a_list n)		
		(if (> n 0)
			(cons 
				(shift-left-once a_list) 					;first arg of cons
				(luup (shift-left-once a_list) (- n 1)) 	;second arg of cons, the recursive call
			)
			nil
		)
	)
	
	(luup a_list (length a_list))
)



(print (infix '(+ (+ 1 2) (+ 3 4))))
(print (infix '(+ (- 9 3) (+ (- 3 2) 12))))
(print (prefix '((1 + 2) + (3 + 4))))
(print (prefix '((9 - 3) + ((3 - 2) + 12))))
(print (apply-ops '(2 + 3)))
(print (apply-ops '(27 - 6)))
(print (keep-order-move '(a b c d)))
