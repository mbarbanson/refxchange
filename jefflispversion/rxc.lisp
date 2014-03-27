; (load "rxc.lisp")

;;; This will automatically write a letter by calling (write-letter!)
;;; at the end. You need to either comment that out, or something, to
;;; stop it. 

;;; If you want to reset the vars just (setq *var-bindings* nil)
;;; before (write-letter!), and it will re-ask.

(defparameter *start-letter-token* :ref-letter)
(defvar *my-course* nil)

(defparameter *grammar* nil) ;; set later bcs of fwd refs for fns.

(defun course-grade (rec)
  (if rec
      (cdr (assoc *my-course* (student-courses rec) :test #'string-equal))
    (vval :course-grade)))

(defun my-course (rec) *my-course*)

(defstruct student :id :full-name :brief-name :gpa :ecas :courses :gender)

(defparameter *course-cataglog* 
  '(("symsys245" . "Interaction Analysis")
    ("phys105" . "Quantum Physics of Cats")
    ("CS106B" . "Why there is Air")))

(defvar *vars* nil) ;; These will get loaded out of the grammar at init time, based on grammatical elements that have ? in the second position.

(defvar *student-recs* nil)

(defun load-student-recs ()
  (setq *student-recs*
	(with-open-file
	 (i "student-db.lisp")
	 (loop for rec = (read i nil nil)
	       until (null rec)
	       collect (make-student :id (getf rec :id)
				     :full-name (getf rec :full-name)
				     :brief-name (getf rec :brief-name)
				     :gpa (getf rec :gpa)
				     :ecas (getf rec :ecas)
				     :courses (getf rec :courses)
				     :gender (getf rec :gender)
				     )))))

(defun find-student (&key id-or-name)
  (unless *student-recs* (load-student-recs))
  (let ((rec (loop for rec in *student-recs*
	when (or (and (numberp id-or-name) (= id-or-name (student-id rec)))
		 (and (stringp id-or-name) (string-equal id-or-name (student-full-name rec))))
	do (return rec))))
    (if rec 
	(format t "Found and using record for ~a...~%" id-or-name)
      (format t "No record was found for ~a. You'll need to fill in the info...~%" id-or-name))
    rec))
	
(defvar *var-bindings* nil)

(defun get-vars (&key id-or-name course)
  "Rip the vars out of the grammar, and pull everything that we can from the student record, and ask for the rest."
  ;; First rip them out of the grammar and shove them into
  ;; *vars*. This is just a convenience, these actually could be left
  ;; in place.
  (setq *vars* 
	(loop for expr in *grammar*
	      if (eq '? (second expr))
	      collect expr))
  ;; This is really ugly, using *var-bindings* both as a local
  ;; looking down, and as a global looks up!
  (setq *var-bindings* 
	(let* ((student (find-student :id-or-name id-or-name))
	       (*var-bindings* ;; computations can use these values!
		(loop for (var nil prompt accessor) in *vars* ;; second is always ? for vars
		      with result = nil
		      collect 
		      `(,var 
			,(or (and accessor student (funcall accessor student))
			     (progn (princ prompt) (read-line)))))))
	  (append *var-bindings*
		  (loop for (key fn) in *computed-vars* 
			collect `(,key ,(funcall fn student)))
		  )))
  )

(defun vval (var &optional (bindings *var-bindings*))
  (or (cadr (assoc var bindings))
      (error "In VVAL, tried to lookup ~a, which isn't a valid var." var)))

;;; These computed var vals get given the student record, but can also
;;; use the input values from the global *var-bindings*

(defun short-name-or-pronoun (rec)
  (if (zerop (random 2))
      (if (string-equal "m" (vval :gender *var-bindings*)) "he" "she")
      (student-short-name  *var-bindings*)))
(defun student-short-name (rec)
  (let* ((fn (vval :student-full-name *var-bindings*)))
    (subseq fn 0 (position #\space fn))))
(defun time-known-unit (rec)
  (let* ((tf (vval :time-known *var-bindings*)))
    (subseq tf (position #\space tf))))
(defun time-known-number (rec)
  (let* ((tf (vval :time-known *var-bindings*)))
    (subseq tf 0 (position #\space tf))))

(defun write-letter! (&key id-or-name course)
  (terpri) (terpri)
  (setq *my-course* course)
  (get-vars :id-or-name id-or-name :course course) ;; Side effects *var-bindings*
  (recursive-write-part *start-letter-token*))

(defun charp (c)
  (member (type-of c) 
	  '(STANDARD-CHAR CHARACTER)))

;;; Here's the core function:

(defun recursive-write-part (token)
  (cond	((functionp token) (lprint (funcall token)))
	((listp token)
	 (cond ((eq :alt (car token))
		(recursive-write-part
		 (nth (random (1- (length token))) (cdr token))))
	       (t (mapcar #'recursive-write-part token))))
	((or (charp token) (stringp token)) (lprint token))
	((keywordp token) 
	 (if (assoc token *computed-vars*)
	     (lprint (vval token))
	   (let ((expr (assoc token *grammar*)))
	     (if (eq '? (second expr)) ;; A var
		 (if (vval token) 
		     (lprint (vval token))
		   (error "In recursive-write-part, shouldn't get here. Token= ~s" token))
	       (mapcar #'recursive-write-part (cdr (assoc token *grammar*)))))))
	 (t (error "In recursive-write-part, shouldn't get here. Token= ~s" token))
	))

(defun lprint (string)
  (princ string))

(defun course-full-name (rec)
  (cdr (assoc *my-course* *course-cataglog* :test #'string-equal)))

(defparameter *computed-vars* 
  `((:short-name-or-pronoun ,#'short-name-or-pronoun)
    (:student-short-name ,#'student-short-name)
    (:time-known-unit ,#'time-known-unit)
    (:time-known-number ,#'time-known-number)
    (:my-course ,#'my-course)
    (:course-grade ,#'course-grade)
    (:course-full-name ,#'course-full-name)
    ))

(defparameter *grammar*
  `((:ref-letter :salutation :intro :known-time :way-known :course-details :comments)
    (:salutation "Dear " :to-person-salutation ", ")
    (:intro "It is my " :pleasure/honor " to write in support of " :student-full-name "'s application to the " :program ". ")
    (:known-time "I have known " :student-short-name " for " :time-known-number :time-known-unit ". ")
    (:way-known :short-name-or-pronoun " attended my course \'" :course-full-name "\' (aka, "  :my-course "). ")
    (:pleasure/honor (:alt "pleasure" "honor"))
    (:comments :pos :neg)
    (:pos "On the positive side " :short-name-or-pronoun " " :nice-phrase ", ")
    (:neg "on the otherhand " :short-name-or-pronoun " " :neg-phrase ". ")
    (:course-details "In " :my-course " " :short-name-or-pronoun " received a " :course-grade ". ")
    ;; Vars are indicated by a ? in second position. These are ripped out and put into *vars* at init. 
    ;; Optional fn in third pos must take only a student record.
    ;; These can actually go anywhere. I just cluster them here for convenience.
    (:to-person-salutation ? "How should the salutation read, for example: 'Dr. Smith'? ")
    (:student-full-name ? "What is the student's full name, as 'Jane Doe'? " ,#'student-full-name)
    (:time-known ? "How long have you known this student, for example '3 months'? ")
    (:program ? "What is the name of the program to which the student is applying? ")
    (:gender ? "Gender (m/f): " ,#'student-gender)
    (:course-attended-title ? "The name of the course that the student took from you? " ,#'my-course)
    (:course-grade ? "The grade that this student received in your course (e.g., A-): " ,#'course-grade)
    (:student-in-top-% ? "What percentile was this student (for example, a student in the top 5% you would say: '5')? ")
    (:nice-phrase ? "Say something positive about this student (e.g., 'is a quick study')")
    (:neg-phrase ? "Say something slightly negative about this student (e.g., 'was often late to course')")
    ))

(format t "~%~%=== Writing for a known student, 12345 ===~%~%")
(setq *student-recs* nil) (setq *var-bindings* nil)
(write-letter! :id-or-name 12345 :course "symsys245")
(format t "~%~%=== Writing for a UNknown student! ===~%~%")
(setq *student-recs* nil) (setq *var-bindings* nil)
(write-letter! :id-or-name 00000 :course "symsys245")
(format t "~%~%=== DONE ===~%~%")
