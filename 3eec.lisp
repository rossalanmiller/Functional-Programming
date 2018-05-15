#|  multPolynomials
##################################
#	Multiplies two polynomials 
#	returns a polynomial
##################################
#	returns a polynomial		 |#
(defun multPolynomials(poly_1 poly_2)


	#| multTerms
	###################################################
	#	Each term of a polynomial can be defined by
	#	two components, it's coefficient and it's power
	#	so for example 4(x^2) has coefficient 4 and
	#	power 2. 
	####################################################
	#	returns a polynomial term					  |#
	(defun multTerms(term_1 term_2)
		(list (* (car term_1) (car term_2)) (+ (cadr term_1) (cadr term_2)))		;multiply the coefficients and add the powers
	)
	;end function multTerms

	(if poly_1 (progn
		(append 
			(mapcar   (lambda(x) (multTerms (car poly_1) x))  poly_2) ; multiply each term of the first polynomial to every term of the second
			(multPolynomials (cdr poly_1) poly_2)
		))
		nil ;else
	)
)
;end function multPolynomials


#|	powPolynomial
###################################################
#	takes a polynomial to some power n, it uses
#	multPolynomials successively to accomplish this
####################################################
#	returns a polynomial						  |#
(defun powPolynomial(poly pow)


	#| powPolynomial2
	###################################################
	#	Serves as a placeholder for powPolynomial
	#	so that we don't recursively simplify the 
	#	polynomial's power during it's recursive evaluation
	####################################################
	#	returns a polynomial						  |#
	(defun powPolynomial2(poly pow)	
		(if (> pow 1)
			(multPolynomials poly (powPolynomial2 poly (- pow 1)))
			poly
		)
	)
	;call powPolynomial2 to evaluate the power
	;then simplify the polynomial to avoid
	;recursively simplifying it
	(simplifyPolynomial (powPolynomial2 poly pow))
)
;end function powPolynomials



#|	simplifyPolynomial
##########################################################
#	simplifyPolynomial succesfively adds like terms 
#	together to simplify a polynomial as you would 
#	in real life
#########################################################
#	returns a simplified polynomial					   |#
(defun simplifyPolynomial(a_polynomial)


	#|	simplifyFirstTerm
	#############################################################################
	# 	simplifyFirstTerm takes the first term of a polynomial and adds 
	# 	all the like terms to the first term, it returns the summed term.
	# 	For example:
	#
	#		   2(x^2) + 3(x^2) + 1(x^1) + 2(x^1)    =>   5(x^2)
	#  (addSame((2 2)    (3 2)    (1 1)    (2 1)))  =>   '(5 2)
	##############################################################################
	|#
	(defun simplifyFirstTerm(a_polynomial)
		(reduce 
			(lambda(x y) 
			(if (= (cadr y) (cadar a_polynomial))		;if the powers match
				(addTerms x y)					;add the two terms
				x								;else leave the current term as is
			)
			)a_polynomial
		)
	)
	;end simplifyFirstTerm
	
	#|	addTerms
	#######################################################
	#	adds two polynomial terms together
	#	when we add terms we add the coefficients
	#	and leave the powers alone. 
	#######################################################
	#   returns a polynomial term						 |#
	(defun addTerms(term_1 term_2)
		(list (+ (car term_1) (car term_2)) (cadr term_1))  ;returns: '(coef pow)
	)
	;end addTerms
	
	(if a_polynomial
	    (cons (simplifyFirstTerm a_polynomial) 
			  (simplifyPolynomial(remove-if 
				(lambda(y) (= (cadr y) (cadar a_polynomial))) (cdr a_polynomial))) 		;after adding up all the terms of the
		)	    																		;same power those terms are removed from
		nil		;else																	;the list before moving on to the next term
	)																 					;with a different power
)
;end function simplifyPolynomial



#|	addPolynomials
##########################################################
#	adds two polynomials together
#########################################################
#	returns a polynomial							   |#
(defun addPolynomials(poly_1 poly_2)
	(simplifyPolynomial (append poly_1 poly_2))
)
;end function addPolynomials



#|	aksPoly
##########################################################
#	constructs the acs polynomial for testing
#	weather number p is prime or not
#########################################################
#	returns a polynomial							   |#
(defun aksPoly(p)

	#|	removeZeroes
	##########################################################
	#	removes all the zero coeficients from a polynomial
	#########################################################
	#	returns a polynomial							   |#
	(defun removeZeroes(poly)
		(remove-if (lambda(x) (= (car x) 0)) poly)
	)


	(removeZeroes
		(addPolynomials 
		(powPolynomial '((1 1) (-1 0)) p) 
		`((-1 ,p) (1 0))
		)
	)
)
;end function aksPoly


;(print (aksPoly 5))



#|	isPrime
##########################################################
#	returns true if p is a prime
#	nil otherwise
#########################################################
#	returns a polynomial							   |#
(defun isPrime(p)

	(defun divides(x y)
		(= (mod y x) 0)
	)
	(reduce (lambda(x y) (and x (divides p (car y) ))) (aksPoly p) :initial-value T) ;returns true if p divides every coefficient of the aks poly 
)																					 ;generated by two


(print (isPrime 19))
;(print (isPrime 1223))


;this way of doing things is complicated and sucks, it can only determine primes up to 19 before it starts to get really really slow


