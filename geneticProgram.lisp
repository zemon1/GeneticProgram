;Jeff Haak
;Create an expression

;TODO
;
;-
;
;-
;
;-
;
;-
;
;/TODO

(defconstant CHROMOSIZE 10)
(defconstant POPSIZE 5)

(defvar OpArgCount 
    '((* 2) (/ 2) (+ 2) (- 2) (expt 2) (sqrt 1) (% 2) (floor 1) (round 1) (ceiling 1) (isqrt 1) (abs 1)))

(defvar possibleOps 
    '(* / + - expt sqrt % floor round ceiling isqrt abs))

(defvar possibleNums
    '(1 2 3 4 5 6 7 8 9 .1 .2 .3 .4 .5 .6 .7 .8 .9))

(defvar allItems 
    (append possibleOps possibleNums))

;Done
(defun randomItem ()
    (list (elt allItems (random (length allItems))))
)

(defun runGA (goal)
    ;(printPop (fitnessPop (createPop POPSIZE ()) nil goal))
    ;(printPop (fitnessPop (createPop POPSIZE ()) nil goal))
    (chromoParens (createPop POPSIZE ()) nil) 
)

;Done
(defun createPerson (id chromosome)
    "Creates a single person"
    (if (equal chromosome nil)
        ;Call a random chromo function
        
        ;(getChromo '(1 2 3 / 5) -1 ())
        (let ((res (randomChromo CHROMOSIZE ()))) 
            ;(print res)
            ;(print (countOps res 0))
        ;    (getChromo res -1 ())
            res
        )
        ;(randomChromo CHROMOSIZE ())
        chromosome
    )     
)

;Done
(defun createPop (thePop people)
    "Creates the original population"
    (if (equal 0 thePop)
        people
        (createPop (- thePop 1) (push (createPerson thePop nil) people))
    )
)

;Done
(defun randomChromo (size chromo)
    (if (equal 0 size)
        chromo
        (randomChromo (- size 1) (append chromo (randomItem)))
    )    
)

;Done
(defun getChromo (chromo opReq result)
    "Explanation"
    (let ((op (first chromo)))
 
        ;If base return
        (if (equal 0 opReq)
            result
            ;If gene is an operation add to requirement ect
            (if (member op possibleOps)
                ;Is this my first run? if so get rid of that -1
                (if (equal -1 opReq)  
                    (getChromo 
                        (cdr chromo) 
                        (+ opReq (second (assoc op opArgCount)) 1) 
                        (append result (list op))
                    )
                    (getChromo 
                        (cdr chromo) 
                        (+ opReq (second (assoc op opArgCount))) 
                        (append result (list op))
                    )
                )
                ;If not, its a number so are we in an expression?
                (if (> opReq 0)
                    ;We are, so reduce the required numbers and add it to the result list
                    (getChromo 
                        (cdr chromo)
                        (- opReq 1) 
                        (append result (list op))
                    )
                    ;We aren't and we need to be so keep looking for an op
                    (getChromo 
                        (cdr chromo)
                        -1 
                        result
                    )
                )
            )
        )
    )
)

;Done
(defun countOps (chromo result)
    "Counts the number of operators in the chromo"
    (
    let ((front (first chromo)))
        (if (equal chromo nil)
            result
            (if (member front possibleOps)
                (countOps (cdr chromo) (+ result 1))
                (countOps (cdr chromo) result)
            )
        )
    )
)

(defun fitnessPerson (goal person)
    ""
    (let ((opChromo (getChromo person -1 ())))
        ;(print opChromo)
        (if (member nil person)
            ;If there is a nil in it's formula make it an exteremly bad mate
            (list person 1000000000 750000)
            ;(list person (- goal (eval opChromo)) (countOps opChromo 0))
            (list person opChromo (countOps opChromo 0))
        )
    )
)

(defun fitnessPop (people res goal)
    (if (equal people nil)
        res
        (fitnessPop (cdr people) (append res (fitnessPerson goal (first people))) goal)
    )    
)

(defun chromoParens (people res)
    "Explain"
    (print (first people))
    (if (equal people nil)
        res
        (chromoParens (cdr people) (append res (getChromo (list (car people)) -1 ())))
    )
)

;Need to add parens to expressions
(defun printPop (people)
    "Prints ever population member seperated by new lines"
    (print (first people))
    ;(format t "~%")
    (if (equal nil people)
        nil
        (printPop (cdr people))
    )
)


