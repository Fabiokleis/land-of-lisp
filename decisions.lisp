;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;; chapter 4
;; making decisions with conditions

;; symmetry of nil and ()
;; empty equals false
;; Since the Lisp philosophy strongly emphasizes the use of lists to store and
;; manipulate information, it will come as no surprise that the design of Common Lisp
;; favors dehaviors that make it easy to slice and dice such lists.

;; I-AM-FALSE
(if '()
    'i-am-true
    'i-am-false)

;; I-AM-TRUE
(if '(1)
    'i-am-true
    'i-am-false)

;; if evaluates empty list () as a false value, whereas a list that contains an item evaluates true.

;; Because we can easily detect an empty list, we can process lists using recursion.
;; With this technique, we can take items from the front of a list and send the rest
;; of the list back to the same function until the list is empty.


;; classical Lisp style list length calculation.
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

;; tail recursive one haha
(defun my-length-tail (list acc)
  (if list
      (my-length-tail (cdr list) (+ 1 acc))
      acc))

;; the four disguises of ()
;; Not only does the empty list evaluate to false, but it is the only false value in Common Lisp. Any value not equivalent to an empty list will be considered a true value.

(eq '() nil)  ;; T
(eq '() ())   ;; T
(eq '() 'nil) ;; T

;; the conditionals: if and beyond

;; one thing at a time with if
;; YUP
(if (= (+ 1 2) 3)
    'yup
    'nope)
;; NOPE
(if (= (+ 1 2) 4)
       'yup
       'nope)

;; if command can also be used to check whether a list is empty:
;; THE-LIST-HAS-STUFF-IN-IT
(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)
;; THE-LIST-IS-EMPTY
(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)

;; usually, when a function is executed in Lisp, all the expressions after the function name are evaluated, before the function itself is evaluated. However, if does not follow these rules. To see this, consider the following example:
(if (oddp 5)
    'odd-number
    (/ 1 0))   ;; ODD-NUMBER

;; progn
(defvar *number-was-odd* nil)
(if (oddp 5)
    (progn (setf *number-was-odd* t)
	   'odd-number)
    'even-number) ;; ODD-NUMBER, *number-was-odd* ==> T

;; when
(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number) ;; ODD-NUMBER, *number-is-odd* ==> T

;; unless
(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number) ;; EVEN-NUMBER, *number-is-odd* ==> NIL

;; the command that does it all: cond
;; the cond form is the classic way to do branching in Lisp.
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(t
	  '(why you eat my pudding stranger? ))))

;; > (pudding-eater 'johnny)
;; (I HOPE YOU CHOKED ON MY PUDDING JOHNNY)

;; branching with case
;; using case, we can rewrite the previous example as follows:
(defun pudding-eater (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger?))))
;; case lets you supply a value to compare
;; but cannot be used to branch on string values, only for symbols.


;; (and (oddp 5) (oddp 7) (oddp 9)) ==> T
;; (or (oddp 4) (oddp 7) (oddp 8)) ==> T
;; (or (oddp 5) (setf *is-it-even* t)) ==> T ;; *is-it-even* ==> NIL
(if *file-modified*
    (if (ask-user-about-saving)
	(save-file)))
;; we could write this instead:
(and *file-modified* (ask-user-about-saving) (save-file))
(if (and *file-modified*
	 (ask-user-about-saving))
    (save-file))

;; using functions that return more than just the truth
(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list) ;; ONE-IS-IN-THE-LIST

(member 1 '(3 4 1 5))     ;; (1 5)
(member nil '(3 4 nil 5)) ;; (NIL 5)

(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list) ;; NIL-IS-IN-THE-LIST

(find-if #'oddp '(2 4 5 6)) ;; 5

;; # -> hof definition
(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-no-odd-number) ;; 'there-is-an-odd-number


;; null function returns true for any of the nil values, correctly finds the nil.
;; unfortunately, in this one annoying case, we would not want to use find-if inside a
;; conditional statement, because a correctly found value still returns a result that
;; evaluates as false. The symmetry has been broken.
(find-if #'null '(2 4 nil 6)) ;; NIL


;; comparing stuff: eq, equal, and more
