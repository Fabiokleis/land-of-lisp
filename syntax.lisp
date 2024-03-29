;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(defun square (n)
  (* n n))

;; symbols are case insensitive
(eq 'fooo 'FoOo)

;; the numbers 1 and 1.0 are two different entities in Common Lisp.
;; math operations with both integer and floating-point number will return float number.
(+ 1 1.0)

;; rational number will be returned
(/ 4 6)

;; would return 0.6666667
(/ 4.0 6)

;; strings
(princ "Tutti Frutti")
(princ "He yelled \"Stop that thief!\" from the busy street.")

;; code mode
;; a form is simply a list with a special command at the begining---typcally a function
;; (foo bla bla bla)
(expt 2 3)
(expt 2 (+ 3 4))

;; data mode
;; single quote in front of the list
;; the single quote tells Lisp to treat the subsequent form as a chunk of data---simply a list of items. This is called quoting.
'(expt 2 3)

;; lists in Lisp
;; lists are what hold all Lisp code (as well as data) together.
;; symbol (expt) and two numbers, all tied together as a list, indicated by the parentheses.
(expt 2 3)

;; cons cells
;; lists in lisp are held together with cons cells.
;; A cons cell can point to another cons cell or another type of Lisp data.
;; By being able to point to two different things, it's possible to link cons cells together into lists.
;; lists in Lisp are just an abstract illusion---all of them are actually composed of cons cells.

'(1 2 3)
;; this lists is represented as three cons cells:
;;  [][]
;; /   \
;;1    [][]
;;    /   \
;;   2    [][]
;;       /   \
;;      3     nil

;; list functions
;; There are three basic functions for manipulating cons cells (and hence lists) in Lisp:
;; cons, car and cdr.

;; const function

;; this function returns a single object, the cons cell, represented by parentheses and
;; a dot between the two connected items. (CHICKEN . CAT)
(cons 'chicken 'cat)
;; nil does not show in the output (CHICKEN)
;; nil is a special symbol that is used to terminate a list in Lisp.
(cons 'chicken 'nil)
(cons 'chicken ()) ;; empty list `()` can ne used interchangeably with the nil symbol.

(cons 'pork '(beef chicken))
;; will return (PORK BEEF CHICKEN)
;; to add pork to the front of a list.
;; In this example we consed pork to a list containing beef and chicken.
(cons 'pork (cons 'beef (cons 'chicken ()))) ;; (PORK BEEF CHICKEN)
;; Lisp, a chain of cons cells and a list are exactly the same thing.

;; the car and cdr functions
;; list are just long chains of two-item cells.

;; car function is used for getting the thing out of the first slot of a cell.
;; so grabs head of the list
(car '(pork beef chicken)) ;; (PORK)

;; cdr function is used to grab the value out of the second slot, or the remainder of a list
(cdr '(pork beef chicken)) ;; (BEEF CHICKEN)

;; we can chain car and cdr functions like that:
(car (cdr '(pork beef chicken))) ;; (BEEF)
;; mix car and cdr -> cadr
;; grab the second item in the list.
(cadr '(pork beef chicken)) ;; (BEEF)

;; the list function
;; Common Lisp has many functions built on top of the basic three---cons, car and cdr.
;; list function does the dirty work of creating all the cons cells and builds our list.

(list 'pork 'beef 'chicken) ;; (PORK BEEF CHICKEN)

(cons 'pork (cons 'beef (cons 'chicken ())))

'(pork beef chicken)

;; ^- all the same

;; nested lists
;; lists can contain other lists:
'(cat (duck bar) ant) ;; (CAT (DUCK BAT) ANT)

;; nested list has are made of cons cells, so car will grab the first item which is a list.
(car '((peas carrots tomatoes) (pork beef chicken))) ;; (PEAS CARROTS TOMATOES)
(cdr '(peas carrots tomatoes)) ;; (CARROTS TOMATOES)
(cdr (car '((peas carrots tomatoes) (pork beef chicken)))) ;; (CARROTS TOMATOES
(cdar '((peas carrots tomatoes) (pork beef chicken))) ;; (CARROTS TOMATOES)

;; Common Lisp already defines all these functions for you. You can use any function with  the name c*r right out of the box, up to four levels deep.
