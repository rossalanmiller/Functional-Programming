;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
	Author:			Ross Miller
	Class:			CS 530
	Program Name:	MillerAssign03_eec.lisp
	Purpose:		Fun extra credit (hmmm)
	Date:			April 2, 2017
	
	Note:			No lambdas or mapcars included
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;given a sequential list of primes p (exluding 1 becuse 1 divides anything)
;addNext adds returns a list including the next prime in the sequence
;so (addNextPrime '(2 3) => '(2 3 5)
(defun addNextPrime(p_list)

	(defun nextNumber(a_list)
		(+ 2 (lastElem a_list))
	)
	
	(defun lastElem(a_list)
		(car (last a_list))
	)
	
	(defun isNotDividedBy(x y)
		(not (= (mod x y) 0))
	)
	
	
	;checks weather number x satisfies the boolean function f for all elements in a list.
	(defun if_forAll(x f a_list)
		(if a_list
			(and (funcall f x (car a_list)) (if_forALL x f (cdr a_list)))	;could be made better with short circuiting
			T
		)
	)
	
	(defun addNextPrime2(p_list n)
		(if	(if_forALL n #'isNotDividedBy p_list)		;could be optimized by only checking up to the sqrt(n) but it's 1:27 on thursday...
			(append p_list `(,n))						;if n is not prime
			(addNextPrime2 p_list (+ n 2))				;then n is not a prime so check n + 2
		)
	)
	
	
	(addNextPrime2 p_list (nextNumber p_list))	
)
;end function addNextPrime



;generates the first n primes and
;returns them as a list.
;with the current implementation of 
;addNextPrime we can generates
;approximately the first 2700 primes
;but there is definitely room for improvement
(defun genPrimes(n)
	
	(defun genPrimes2(p_list n)
		(if	(> n 3)
			(addNextPrime(genPrimes2 p_list (- n 1) ))
			p_list
		)
	)
	
	
	(case n
		(1 '(1))
		(2 (cons 1 '(2)))
		(3 (cons 1 '(2 3)))
		(otherwise (cons 1 (genPrimes2 '(2 3) n)))
	)
)
;end function gen primes



;prints all the primes sums of an 
;even number n
(defun even-prime-sums(n)
	
	;returns a list all elements of a list less then n
	(defun lessThan(a_list n)
		(if (> n (car a_list))
			(cons (car a_list) (lessThan (cdr a_list) n))
			nil
		)
	)
	
	;return a list of all primes less then n
	(defun primesLessThan(n)
		(lessThan (genPrimes (floor n 2)) n)
	)
	
	(defun printSums(p_list n)
		(if (cdr p_list)
			(progn
				(if (member (- n (car p_list)) p_list)
					(progn
						(format t "~s + ~s = ~s~%" (- n (car p_list)) (car p_list) n)
						(printSums (cdr p_list) n)
					)
					(printSums (cdr p_list) n)
				)
			)
		)
	)
	
	(printSums (primesLessThan n) n)
)
;end function even-prime-sums

(even-prime-sums 28)
(format t "~%")
(even-prime-sums 102)