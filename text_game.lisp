;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; Land of Lisp - chapter 5: Building a text game engine

;; the wizard's adventure game

;; our game world
;;                (living-room)
;;        ladder /^            \^ door
;;              /v              \v
;;            (attic)       (garden)

;; plays move between nodes by traveling along the edges in either direction.
;; wherever the players are, they can interact with various objects around them.

;; basic requirements
;; our game code will need to handle a few basic things:
;; - looking around
;; - walking to different locations
;; - picking up objects
;; - performing actions on the objects picked up

;; when looking around in our game world, you will be able to "see" three kinds of things from any location:
;; - basic scenery
;; - one or more paths to other locations
;; - objects that you can pick up and manipulate
;; lets add features for these one at time

(defparameter *nodes* '((living-room (you are in the living-room.
				     a wizard is snoring loudly on the couch.))
		      (garden (you are in a beautiful garden.
				   there is a well in front of you.))
		      (attic (you are in the attic.
				  there is a giant welding torch in the corner.))))

(assoc 'garden *nodes*) ;; (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN.))

(defun  describe-location (location nodes)
  (cadr (assoc location nodes)))

(describe-location 'garden *nodes*) ;; (YOU ARE IN A BEAUTIFUL GARDEN.)

;; describing the paths
;; we'll create a second variable, *edges*, that contains the paths that players can take to move between places on our map.

(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; quasiquoting
(describe-path '(garden west door)) ;; (THERE IS A DOOR GOING WEST FROM HERE.)

;; how quasiquoting works
;; to enable quasiquoting, you must use a backquote [`] not single quote [']
;; when switching from code to data mode. The describe-path function has just
;; such a backquote in it.
;; both the single quote and backquote in Lisp "flip" a piece of code into data
;; mode, but only a backquote can albo be unquoted using the comma character,
;; to flip back into code mode.
;;

;; v-- flip     v-- flop            v--  flop
;; `(there is a ,(caddr edge) going ,(cadr edge) from here.)

;; describing multiple paths at once
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(describe-paths 'living-room *edges*)
;; (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)

;; the describe-paths function takes the following steps:
;; 1. find the relevant edges.
;; 2. convert the edges to descriptions.
;; 3. join the descriptions.

;; let's see how it performs each of these steps.

;; finding the relevant edges
;; we use assoc again to lookup the location in our list of edges:
;; (cdr (assoc 'living-room *edges*))
;; ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)

;; converting the edges to descriptions
;; next, the edges are converted to descriptions. here is just the code to accomplish
;; this, shown in isolation:
(mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))
;;((THERE IS A DOOR GOING WEST FROM HERE.)
;; (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))

;; the mapcar function is used frequently by Lispers. this function takes another
;; function and a list, and them applies this function to every member of a list.
;; here's an example:
(mapcar #'sqrt '(1 2 3 4 5))
;; this example passes sqrt function, along with the (1 2 3 4 5) list, into mapcar.
;; as a result, the function generates a list of the square roots of the original
;; numbers by applying sqrt to every member of the list and creating a new list.

;; functions that take other functions as parameters, such as mapcar, are very
;; useful and a distinguishing feature of Lisp. Such functions are called higher-order functions.
;; Here is another example:
(mapcar #'car '((foo bar) (baz qux)))
;; (foo baz)
;; this time, our source list contains two smaller lists. the car function, which
;; grabs the first item in a list, causes mapcar to return the first items from each
;; smaller list, foo and baz.

;; #' this symbol sequence is a shorthand for the function operator.
;; the Lisp reader (the part of your Lisp environment that reads the code)
;; will convert the previous example into the following longer version:
(mapcar (function car) '((foo bar) (baz qux)))
;; (foo baz)

;; Common Lisp requires you to use the function operator when referring
;; to a function as a value directly like this, because the name of a function may
;; conflict with other named items in a program, causing unpredictable errors.
;; example:
(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux))))
;; (foo baz)
;; to evaluate car as builtin Lisp function, not as local variable declared in let
;; we need to use the function operator, so there is no confusion about which car
;; we are talking about.

;; Now let's look at the describe-paths function again:
;;(defun describe-paths (location edges)
;;  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
;;
;; append and describe-path functions are passed in as values
;; to the apply and mapcar functions, which are designed to receive
;; and use the functions.
;; Common Lisp tracks function names differently from variable names. It
;; has multiple namespaces, including one for variables and one for functions.
;; Scheme, the other popular Lisp dialect, doesn't force you to mark functions with a
;; function operator when using them as values, Sheme has only one namespace for both.
;; Because of this difference in the number of namespaces, Scheme is sometimes called
;; a Lisp-1, whereas Common Lisp is sometimes referred to as a Lisp-2.

;; joining the descriptions


;; append receive items to append as separated values
(append '(mary had) '(a) '(little lamb))
;; (MARY HAD A LITTLE LAMB)

;; using apply function we can pass every item of list as argument to a function
(apply #'append '((mary had) (a) (little lamb)))
;; (MARY HAD A LITTLE LAMB)

;; you can see how apply enables the describe-paths function to build one long list
;; describing all paths leading from a single location.
(apply #'append '((THERE IS A DOOR GOING WEST FROM HERE.)
		  (THERE IS A LADDER GOING UPSTAIRS FROM HERE.)))
;; (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)

;; now that we've looked at each part of the describe-paths function, let's review how it works:
;; (defun describe-paths (location edges)
;;   (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; the function taks two parameters: the current player's location, as well as a list
;; of edge/paths for the game map. first, it uses assoc to look up the correct location
;; from the edge list. Since assoc returns both the key and the value from the list, we
;; call cdr to retrieve only the value. Next, we use mapcar to map the describe-path
;; function against each edge that we found. finally, we concatenate the lists for
;; describing all the paths into one long list by applying append against the list.

;; the programming style used by describe-path is very typical for Lisp code.
;; It involves passing along a complicated chunk of data and manipulating it in
;; several steps, often using higher-order functions.

;; describing objects at a specific location
;; listing visible objects
(defparameter *objects* '(whiskey bucket frog chain))
;; we can also create a second variable, *object-locations*, to track the location
;; of each object in the form of an list:
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; lists the objects visible from a given location:
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; at-loc-p taks the symbol for an object and returns t or nil,
;; depending on whether that object exists at the location loc.
;; It does this by looking up the object in the obj-locs list.
;; why did we name this function at-loc-p? When a function returns
;; nil or a truth value, it's a Common Lisp convention to append
;; a p to the end of that function's name.

;; the remove-if-not function in the last line of the listing, as you
;; might expect, removes all things from a list which a passed-in function
;; (in this case, at-loc-p) doesn't return true. Essentially, it returns a
;; filtered list of objects consisting of those items for which at-loc-p is true.

(objects-at 'living-room *objects* *object-locations*)
;; (WHISKEY BUCKET)

;; describing visible objects
;; now we can write a function to describe the objects visible at a given location:
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; describe-objects first creates the describe-obj function. this function
;; generates a pretty sentence stating that a given object is on the floor,
;; using quasiquoting. the main part of the function conssists of calling
;; objects-at to find the objects at the current location, mapping describe-obj across
;; this list of objects, and finally appending the descriptions into a single list.
(describe-objects 'living-room *objects* *object-locations*)
;; (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)

;; describing it all
;; now we'll tie all of these description functions into one easy command
;; called look. Because this will be the actual command players can enter
;; to look around them in the game, look will need to know a player's current
;; location. so, we need a variable to track the player's current position.
(defparameter *location* 'living-room)
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))
;; look function uses global variable names (such as *location*, *nodes*m and so on),
;; the player won't need to pass in any funky values in order to look out at the world.
;; however, this also means that the look function in programming style reference
;; only parameters or variables declared in the function itself. *location* and its
;; ilk are global variables, so the look function doesn't hold up muster.
(look)
;; (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH. THERE IS A
;; DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU SEE
;; A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)

