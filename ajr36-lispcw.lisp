;;;; 
;;;; ajr36 - Alex Remedios
;;;;
;;;; CM20214/221
;;;; Advanced Programming Principles/Programming II Assessed Coursework
;;;; Assignment 1 - Using Common Lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;                           MY REPRESENTATION                               ;;
;;                                                                           ;;
;; Single term = (coefficient ((var1 power1)(var2 power2) ...(varn powern))) ;;
;;                                                                           ;;
;; Polynomial  = (term1 term2 ...termn)                                      ;;
;;                                                                           ;;
;; Terms containing constants will express the constants as coefficients of  ;;
;; ("x" 0) (or possible use a different symbol depending on the input)       ;;
;;                                                                           ;;
;; Valid input is anything using the structure above, with coefficients and  ;;
;; powers being numbers, and vars being strings                              ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOTE: Througout the documentation I refer to variable-power pairs e.g. 
;;; ("x" 2) as 'variable's. I refer to the actual 'variable' as the
;;; 'symbol'.
;;;
;;; NOTE 2: p+, p- and p* are designed to accept well formed i.e. already
;;; simplified polynomials, if you put in polynomials which are not 
;;; simplified they will still undergo the operations you desire, however
;;; the chances are they will come out still partially unsimplified.
;;;
;;; NOTE 3: format-polynomial will simplify a polynomial, I am not sure if
;;; this was required, however I made it because I thought p= should be 
;;; able to except unsimplified polynomials.
;;;
;;; NOTE 4: A lot of my test cases use terribly formed polynomials, this is
;;; mostly to show that the code will still perform mathematical correct 
;;; operations, even if the formatting doesn't look as nice as it could do
;;; afterwards.
;;;


(defun p- (a b)
    "Subtracts polynomial b from polynomial a, returns polynomial"

    ;; This function uses the already implemented p+ and p*
    ;; b is multiplied by -1 then added to a
    (p+ a (p* b '((-1 (("x" 0)))))))

(defun p+ (poly-one poly-two)
    "Adds polynomial b to polynomial a, returns polynomial"

    (cond
        ;; If at any point poly-one becomes null due to elimination of terms
        ;; replace its value with a canonical representation of zero

        ;; Base: If there is no second polynomial, return the first unchanged
        ((null poly-two) (if (null poly-one) '((0 (("x" 0)))) poly-one))

        ;; Recursion: Add head of poly-2 to poly-1, then recurse with the new
        ;; polynomial as poly-1 and the tail of poly-2 as poly-2
        (t (p+ (add-term-to-polynomial (if (null poly-one) '((0 (("x" 0)))) 
                                                           poly-one)
                                       (car poly-two))
               (cdr poly-two)))))

(defun sort-variables (variables)
    "Sorts list of variable-power cons pairs so that they can be compared using
    equal"

    (sort (copy-list variables) ; Sort is destructive, so give it a copy
          ; Variables are sorted to string-less order
          (lambda (a b) (if (string-lessp (car b)(car a)) t nil))))

(defun add-term-to-polynomial (polynomial input-term)
    "Searches polynomial for a term which matches input term, if there is a 
    match, the matching terms are combined, if not the new term is added on the
    end"
    
    ;; Bind coefficient and variable lists for the first term in polynomial
    ;; and the input term
    (let ((coefficient-a (caar polynomial)) 
         (variables-a (car (cdar polynomial)))
         (coefficient-b (car input-term))  
         (variables-b (car (cdr input-term))))

        (cond
            ;; If the variable compatibility predicate says they can be added..
            ((compatible-variables-p (sort-variables variables-a) 
                                     (sort-variables variables-b))
                    
                (if (= (+ coefficient-a coefficient-b) 0)
                    ;; New coeff is zero? There will be no new term, just
                    ;; return the rest of the terms
                    (cdr polynomial)

                    ;; return new term and the rest of the terms
                    (append (list (append (list (+ coefficient-a 
                                                   coefficient-b))
                                          (list variables-a)))
                            (cdr polynomial))))

            ;; Base: If this is the only term left in the polynomial, and the
            ;; input term does not match it, then the input term must be added 
            ;; to the end of the polynomial
            ((null (cdr polynomial)) (list (car polynomial) input-term))

            ;; Recursion: If the input term cannot be added to the polynomial
            ;; term, list the polynomial term with the rest of itself added to 
            ;; the input term.
            (t (cons (car polynomial)
                     (add-term-to-polynomial (cdr polynomial) input-term))))))

(defun p* (a b)
    "Returns a polynomial which is the product of polynomials a and b"

    (cond
        ;; Base: This condition occurs when there are no terms left in b to 
        ;; multiply a by
        ((null b) nil)

        ;; Base: If b is a single term (i.e the car of the polynomial b)
        ;; return the product of term b and polynomial a
        ((termp b) (multiply-term-by-polynomial b a))

        ;; Recursion: Add up the polynomials resulting from the product of
        ;; polynomial a with each term from polynomial b, this causes the
        ;; polynomial to simplify whilst multiplying.
        (t (p+ (p* a (car b)) (p* a (cdr b))))))

(defun multiply-term-by-polynomial (term polynomial)
    "accepts a term and a polynomial, returns a polynomial which is the product 
    of them"
    (cond 
        ;; Base: This condition occurs when there are no terms left in the
        ;; polynomial to multiply by
        ((null polynomial) nil)

        ;; Base: If polynomial is a single term (i.e the car of polynomial)
        ;; return the product of polynomial and term
        ((termp polynomial)  (list (multiply-terms polynomial term)))
    
        ;; Recursion: Append the results of the head of polynomial * term, and
        ;; the tail of polynomial * term, resulting in a single resultant
        ;; polynomial
        (t (append (multiply-term-by-polynomial (car polynomial) term)
                   (multiply-term-by-polynomial term (cdr polynomial))))))

(defun multiply-terms (term-a term-b)
    "accepts two terms, returns one term - their product"
    (let ((variables-a (cadr term-a)) (variables-b (cadr term-b))
          (new-coefficient (* (car term-a) (car term-b))))
        
        (list new-coefficient (combine-variables variables-a 
                                                        variables-b))))

(defun combine-variables (variables-a variables-b)
    "Accepts two lists containing variable-power pairs, returns one list with
    them merged as they would be in a multiplication operation"
    (cond
        ;; Base: If there is no variable-power list to be added, just return 
        ;; the first
        ((null variables-b) (if (null variables-a) '(("x" 0)) variables-a))

        ;; Recursion: Add the first term of variables-b to variables-a then 
        ;; recurse with the tail of variables-b
        (t (combine-variables 
                (add-variable-to-variable-list (if (null variables-a) 
                                                    '(("x" 0)) 
                                                    variables-a)
                                               (car variables-b))
                (cdr variables-b)))))

(defun add-variable-to-variable-list (variable-list input-variable)
    "Integrates the input-variable into the given variable-list, returns this
    new variable list"

    ;; Example input-variable: ("x" 2)
    ;; Example variable-list: (("x" 3) ("x" 2) ("y" 1))

    ;; Bind symbol and power values for the first variable list in variable
    ;; list, and for input-variable
    (let ((symbol-a (caar variable-list)) (power-a (cadr (car variable-list)))
          (symbol-b (car input-variable)) (power-b (cadr  input-variable)))

        (cond
            ;; If both variables are constants, then eliminate them
            ((and (= power-a 0) (= power-b 0)) (cdr variable-list))

            ;; Base: If the symbols are equal e.g. both "x", then the powers
            ;; can be added
            ((string= symbol-a symbol-b) 
                ;; If resultant power is zero, then variable is eliminated,
                ;; otherwise add the powers
                (if (= (+ power-a power-b) 0)
                    (cdr variable-list)
                    (append (list (list symbol-a (+ power-a power-b))) 
                                                 (cdr variable-list))))

            ;; Base: If there are no more variables in variable-list to attempt
            ;; to match after this one, add the unmatched variable to the end 
            ;; of the variable list
            ((null (cdr variable-list))  (list (car variable-list) 
                                               input-variable))

            ;; Recursion: If we haven't found a matching variable, add what we 
            ;; have attempted to match, to the variables that will be matched 
            ;; in future
            (t (cons (car variable-list)
                     (add-variable-to-variable-list (cdr variable-list) 
                                                    input-variable))))))

(defun p= (a b)
    "Returns true if poly a - poly b is zero, otherwise false"
    ;; If the difference of the polynomials is equal to the canonical
    ;; representation of zero, they are p=

    (if (equal (p- (format-polynomial a) (format-polynomial b)) 
               '((0 (("x" 0))))) t nil))

(defun termp (term)
    "Term predicate: Returns true if the car is a number and the cdr is a list"
    (if (and (numberp (car term)) (listp (cdr term))) t nil))

(defun compatible-variables-p (vars-one vars-two)
    "Returns true if vars-one and vars-two are equal - i.e. a term with vars
    one can be added to a term with vars two to create one resultant term"

    ;; Terms can be added when all of their non-constant variables are equal

    (cond 
        ;; Base: If both are null they must have matched correctly
        ((and (null vars-one) (null vars-two)) t)
        
        ;; If vars-one is out of variables, the rest of vars-two must be
        ;; constants for them to match
        ((null vars-one)
            ;; Recurse: Watches for constant on vars-two
            (if (= (cadr (car vars-two)) 0)  ; if power is zero
                (compatible-variables-p vars-one (cdr vars-two)) nil))

        ;; If vars-two is out of variables, the rest of vars-one must be
        ;; constants for them to match
        ((null vars-two)
            ;; Recurse: If a constant appears, ignore it by moving on
            (if (= (cadr (car vars-one)) 0)  ; if power is zero
                (compatible-variables-p (cdr vars-one) vars-two) nil))

        ;; Recurse: If they are equal, continue
        ((equal (car vars-one) (car vars-two))
            (compatible-variables-p (cdr vars-one) (cdr vars-two)))
            
        ;; Recurse: If either var is a constant, it can be ignored 
        ((= (cadr (car vars-two)) 0)
            (compatible-variables-p vars-one (cdr vars-two)))

        ((= (cadr (car vars-one)) 0)
            (compatible-variables-p (cdr vars-one) vars-two))
        ;; Base: If none of the above conditions are met, vars do not match
        (t nil)))

(defun format-polynomial (polynomial &optional (sorted '((0 (("x" 0))))))
    "Ensures the polynomial is as simplified as possible and each term's 
    variables are in alphabetical order"
    (cond
        ;; Base: If there is only one term, no simplifications is needed,
        ;; just add 0 to ensure vars are sorted
        ((null (cdr polynomial)) (add-term-to-polynomial sorted 
                                                         (car polynomial)))

        ;; Recursion: If there is more than one term in polynomial, add
        ;; the first term to sorted, then recurse.
        (t (format-polynomial (cdr polynomial) 
                              (add-term-to-polynomial sorted 
                                                      (car polynomial))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;                                TEST CASES                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; p+
;#|

;; 1. Same variables - coefficients add
(print (p+ '((1 (("x" 1)))) 
           '((2 (("x" 1))))))

;;((3 (("x" 1)))) 


;; 2. Different variables - terms append
(print (p+ '((1 (("x" 2)))) 
           '((2 (("x" 1))))))

;;((1 (("x" 2))) (2 (("x" 1)))) 

;; 3. Resultant coefficient is zero - term is eliminated 
(print (p+ '((1 (("x" 1)))) 
           '((-1 (("x" 1))))))

;;((0 (("x" 0)))) 

;; 4. Multi-term addition on well formed (already simplified) polynomials
(print (p+ '((1 (("x" 2))) (2 (("x" 1))))
           '((3 (("x" 2))) (1 (("x" 0)))))) 

;;((4 (("x" 2))) (2 (("x" 1))) (1 (("x" 0)))) 

;; 5. Last 4 test cases combined.
(print (p+ '((1 (("x" 2))) (2 (("x" 1))))
           '((3 (("x" 2))) (-2 (("x" 1))) (1 (("x" 0)))) ))

;;((4 (("x" 2))) (1 (("x" 0)))) 

;; 6. Adding variables using different symbols.
(print (p+ '((1 (("y" 2))) (2 (("y" 1))))
           '((3 (("x" 2))) (1 (("y" 1))))))

;;((1 (("y" 2))) (3 (("y" 1))) (3 (("x" 2)))) 


;; 7. Adding constants which have different symbols.
(print (p+ '((1 (("x" 0))))
           '((3 (("y" 0))))))

;;((4 (("x" 0)))) 


;; 8. Adding terms with different combinations of variables.
(print (p+ '((1 (("y" 2)("x" 1))) (2 (("y" 1))))
           '((3 (("y" 2)("x" 1))) (1 (("var" 1)("z" 3))))))

;;((4 (("y" 2) ("x" 1))) (2 (("y" 1))) (1 (("var" 1) ("z" 3)))) 
 
;; 9. Adding terms with variables in different orders.
(print (p+ '((1 (("y" 2)("x" 1))) (2 (("y" 1))))
           '((3 (("x" 1)("y" 2))) (1 (("var" 1)("z" 3))))))

;;((4 (("y" 2) ("x" 1))) (2 (("y" 1))) (1 (("var" 1) ("z" 3)))) 

;; 10. Last 4 test cases combined.
(print (p+ '((1 (("y" 2)("x" 1))) (2 (("x" 0)("y" 1))))
           '((3 (("y" 2)("x" 1))) (1 (("y" 1)("z" 0))))))

;;((4 (("y" 2) ("x" 1))) (3 (("x" 0) ("y" 1))))


;; 11. Combining test cases 5 and 10.
(print (p+ '((1 (("y" 2)("x" 1))) (2 (("x" 0)("y" 1))) (2 (("x" 1))))
           '((3 (("y" 2)("x" 0))) (1 (("y" 1)("z" 0))) (-2 (("x" 1))))))

;;((1 (("y" 2) ("x" 1))) (3 (("x" 0) ("y" 1))) (3 (("y" 2) ("x" 0)))) 

;|#

;; p*
;#|
;; 1. Linear * constant, coefficients multiply
(print (p* '((1 (("x" 1)))) 
           '((2 (("x" 0))))))

;;((2 (("x" 1)))) 

;; 2. Identical variables - powers add
(print (p* '((1 (("x" 1)))) 
           '((2 (("x" 1))))))

;;((2 (("x" 2)))) 

;; 3. Resultant power is zero - variable eliminated (powers can subtract)
(print (p* '((1 (("x" 1)))) 
           '((2 (("x" -1))))))

;;((2 (("x" 0)))) 

;; 4. Multi-term polynomial multiplication
(print (p* '((1 (("x" 1)))(1 (("x" 0)))) 
           '((2 (("x" 1)))(1 (("x" 0))))))

;;((2 (("x" 2))) (3 (("x" 1))) (1 (("x" 0)))) 

;; 5. Last 4 cases combined.
(print (p* '((1 (("x" 2)))(1 (("x" 1)))) 
           '((2 (("x" 2)))(2 (("x" -1))))))

;;((2 (("x" 4))) (2 (("x" 3))) (2 (("x" 1))) (2 (("x" 0)))) 

;; 6. Multiplication with different variables
(print (p* '((1 (("x" 1)))) 
           '((2 (("x" 1)))(1 (("x" 0)))))) 

;;((2 (("x" 2))) (1 (("x" 1)))) 

;; 7. Multiply constants using different symbols
(print (p* '((1 (("y" 0)))) 
           '((2 (("x" 1)))(1 (("x" 0)))))) 

;;((2 (("x" 1) ("y" 0))) (1 (("x" 0)))) 


;; 8. Multiply terms with multiple variables
(print (p* '((1 (("y" 0)("x" 1)))) 
           '((2 (("z" 0)))(1 (("x" 0))))))

;;((3 (("x" 1)))) 

;; 9. Multiply by zero
(print (p* '((1 (("y" 0)))) 
           '((0 (("z" 0))))))

;;((0 (("x" 0)))) 

;; 10. Last 4 cases combined
(print (p* '((1 (("y" 2)("x" 1)))(1 (("x" 0)))) 
           '((2 (("z" 0)))(0 (("x" 0))))))

;;((2 (("z" 0) ("y" 2) ("x" 1))) (2 (("x" 0)))) 

;; 11. Adding cases 5 and 10
(print (p+ (p* '((1 (("y" 2)("x" 1)))(1 (("x" 0)))) 
               '((2 (("z" 0)))(0 (("x" 0)))))

           (p* '((1 (("x" 2)))(1 (("x" 1)))) 
               '((2 (("x" 2)))(2 (("x" -1)))))))

;;((2 (("z" 0) ("y" 2) ("x" 1))) (4 (("x" 0))) (2 (("x" 4))) (2 (("x" 3) (2 (("x" 1)))) 

;|#

;; p- (this is just p+ and p* used together)
;#|
;; 1. Same variables - coefficients subtract
(print (p- '((1 (("x" 1)))) 
           '((2 (("x" 1))))))

;;((-1 (("x" 1)))) 

;; 2. Different variables - terms append
(print (p- '((1 (("x" 2)))) 
           '((2 (("x" 1))))))

;;((1 (("x" 2))) (-2 (("x" 1)))) 

;; 3. Resultant coefficient is zero - term is eliminated 
(print (p- '((1 (("x" 1)))) 
           '((1 (("x" 1))))))

;;((0 (("x" 0)))) 

;; 4. Multi-term subtraction on well formed (already simplified) polynomials
(print (p- '((1 (("x" 2))) (2 (("x" 1))))
           '((3 (("x" 2))) (1 (("x" 0)))))) 

;;((-2 (("x" 2))) (2 (("x" 1))) (-1 (("x" 0)))) 

;; 5. Last 4 test cases combined.
(print (p- '((1 (("x" 2))) (2 (("x" 1))))
           '((3 (("x" 2))) (2 (("x" 1))) (1 (("x" 0)))) ))

;;((-2 (("x" 2))) (-1 (("x" 0)))) 


;; 6. Subtracting variables using different symbols.
(print (p- '((1 (("y" 2))) (2 (("y" 1))))
           '((3 (("x" 2))) (1 (("y" 1))))))

;;((1 (("y" 2))) (1 (("y" 1))) (-3 (("x" 2)))) 

;; 7. Subtracting constants which have different symbols.
(print (p- '((1 (("x" 0))))
           '((3 (("y" 0))))))

;;((-2 (("x" 0)))) 

;; 8. Adding terms with different combinations of variables.
(print (p- '((1 (("y" 2)("x" 1))) (2 (("y" 1))))
           '((3 (("y" 2)("x" 1))) (1 (("var" 1)("z" 3))))))

;;((-2 (("y" 2) ("x" 1))) (2 (("y" 1))) (-1 (("x" 0) ("var" 1) ("z" 3))))

;|#





;; p=


;; 1. Single term polynomials: Should resolve to T
(print (p= '((3 (("y" 2))))
           '((3 (("y" 2))))))

;; 2. Single term polynomials: Should resolve to NIL
(print (p= '((3 (("y" 2))))
           '((2 (("y" 2))))))

;; 3. Single term polynomials: Should resolve to NIL
(print (p= '((3 (("y" 3))))
           '((3 (("y" 2))))))

;; 4. Single term polynomials: Should resolve to NIL
(print (p= '((3 (("x" 2))))
           '((2 (("y" 2))))))

;; 5. Multi term polynomials: Should resolve to T
(print (p= '((3 (("x" 2)("z" 0)("y" 2))))
           '((3 (("y" 2)("x" 2))))))

;; 6. Unsimplified multi term polynomials: Should resolve to T
(print (p= '((3 (("x" 2))) (2 (("x" 2))))
           '((5 (("x" 2))))))

;; 7. Unsimplified multi term polynomials: Should resolve to T
(print (p= '((3 (("x" 2)("y" 0))) (2 (("x" 2))))
           '((5 (("z" 0)("x" 2))))))

;; 8. Compare results from addition: Should resolve to NIL
(print (p= (p+ '((3 (("y" 2)("x" 1)("z" 1))))
               '((2 (("y" 2)("x" 1)))))
           (p+ '((2 (("y" 2)("x" 1))))
               '((3 (("y" 2)("x" 1)("z" 0)))))))

;; 9. Compare results from addition: Should resolve to T
(print (p= (p+ '((3 (("y" 2)("x" 1)("z" 0))))
               '((2 (("y" 2)("x" 1)("a" 0)))))
           (p+ '((2 (("y" 2)("h" 0)("x" 1))))
               '((3 (("y" 2)("x" 1)))))))

;; 10. Compare results from multiplication: Should resolve to T
(print (p= (p* '((1 (("y" 2)("x" 1)))(1 (("x" 0)))) 
               '((2 (("z" 0)))(0 (("x" 0)))))
           (p* '((1 (("y" 2)("x" 1)))(1 (("x" 0)))) 
               '((2 (("z" 0)))(0 (("x" 0)))))))

;; 11. Compare results from subtraction: Should resolve to T
(print (p= (p- '((1 (("y" 2)("x" 1))) (2 (("y" 1))))
           '((3 (("y" 2)("x" 1))) (1 (("var" 1)("z" 3)))))
           (p- '((1 (("y" 2)("x" 1))) (2 (("y" 1))))
           '((3 (("y" 2)("x" 1))) (1 (("var" 1)("z" 3)))))))
