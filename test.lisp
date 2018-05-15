


(defun myDelete(an_atom a_list)
	(mapcan (lambda(x) (if (equal x an_atom)
							nil
							(list x)
						)) a_list)

)


#|
(print (myDelete 'a '(a d c)))
(print (myDelete 'c '(b c d c f d c a)))
(print (myDelete 'a '(b c)))
|#


(defun censor-word(symb a_list)

	(mapcar (lambda(x) (if (equal x symb)
							'XXXXX
							x
						)) a_list)

)


(print (censor-word 'COBOL '(I AM A COBOL PROGRAMMER)))