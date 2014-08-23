;Jeff Haak
;Create an optimal expression using a genetic algortihm

;TODO
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
(defun randomOp()
    (list (elt possibleOps (random (length possibleOps)))))

;Done
(defun randomItem()
    (list (elt allItems (random (length allItems)))))

;Done
(defun createPerson (id chromosome)
    "Creates a single person"
    (if (equal chromosome nil)
        (list id (randomChromo CHROMOSIZE ()))
        (list id chromosome)
    )
)

;Done
(defun createPop (size people)
    (if (equal 0 size)
        people
        (createPop (- size 1) (push (createPerson size nil) people))
    )
)

;Done
(defun randomChromo (size chromo)
    (if (equal 0 size)
        chromo
        ;If it is the first item, make it an operation
        (if (equal size CHROMOSIZE)
            (randomChromo (- size 1) (append chromo (randomOp)))
            (randomChromo (- size 1) (append chromo (randomItem)))
        )
    )
)































