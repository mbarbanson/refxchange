; (load "rxc.lisp")

;;; This will automatically write a letter by calling (write-letter!)
;;; at the end. You need to either comment that out, or something, to
;;; stop it. 

;;; If you want to reset the vars just (setq *default-bindings* nil)
;;; before (write-letter!), and it will re-ask.

(defparameter *start-letter-token* :ref-letter)
(defvar *my-course* nil)

(defparameter *grammar* nil) ;; set later bcs of fwd refs for fns.

(defun course-grade (rec)
  (cdr (assoc *my-course* (student-courses rec) :test #'string-equal)))

(defun my-course (rec) *my-course*)

(defstruct student :id :full-name :brief-name :gpa :ecas :courses :gender)

(defparameter *vars*  ;; The fns in third pos must take only a student record
  `((:to-person-salutation "How should the salutation read, for example: 'Dr. Smith'? ")
    (:student-full-name "What is the student's full name, as 'Jane Doe'? " ,#'student-full-name)
    (:time-known "How long have you known this student, for example '3 months'? ")
    (:program "What is the name of the program to which the student is applying? ")
    (:gender "Gender (m/f): " ,#'student-gender)
    (:course-attended-title "The name of the course that the student took from you? " ,#'my-course)
    (:course-grade "The grade that this student received in your course (e.g., A-): " ,#'course-grade)
    (:student-in-top-% "What percentile was this student (for example, a student in the top 5% you would say: '5')? ")
    (:nice-phrase "Say something positive about this student (e.g., 'is a quick study')")
    (:neg-phrase "Say something slightly negative about this student (e.g., 'was often late to course')")
    ))

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
  (loop for rec in *student-recs*
	when (or (and (numberp id-or-name) (= id-or-name (student-id rec)))
		 (and (stringp id-or-name) (string-equal id-or-name (student-full-name rec))))
	do (return rec)))
	
(defvar *default-bindings* nil)

(defun get-vars (&key id-or-name course)
  "Pull everything that we can from the student record, and ask for the rest."
  ;; This is really ugly, using *default-bindings* both as a local looking down, and as a global looks up!
  (setq *default-bindings* 
	(let* ((student (find-student :id-or-name id-or-name))
	       (*default-bindings* ;; computations can use these values!
		(loop for (var prompt accessor) in *vars*
		      with result = nil
		      collect 
		      `(,var 
			,(or (and accessor student (funcall accessor student))
			     (progn (princ prompt) (read-line)))))))
	  (append *default-bindings*  
		  (loop for (key fn) in *computed-var-vals* 
			collect `(,key ,(funcall fn student)))
		  ))))

(defun vval (var &optional (bindings *default-bindings*))
  (or (cadr (assoc var bindings))
      (error "In VVAL, tried to lookup ~a, which isn't a valid var." var)))

;;; These computed var vals get given the student record, but can also
;;; use the input values from the global *default-bindings*

(defun short-name-or-pronoun (rec)
  (if (zerop (random 2))
      (if (string-equal "m" (vval :gender *default-bindings*)) "he" "she")
      (student-short-name  *default-bindings*)))
(defun student-short-name (rec)
  (let* ((fn (vval :student-full-name *default-bindings*)))
    (subseq fn 0 (position #\space fn))))
(defun time-known-unit (rec)
  (let* ((tf (vval :time-known *default-bindings*)))
    (subseq tf (position #\space tf))))
(defun time-known-number (rec)
  (let* ((tf (vval :time-known *default-bindings*)))
    (subseq tf 0 (position #\space tf))))

(defun write-letter! (&key id-or-name course)
  (terpri) (terpri)
  (setq *my-course* course)
  (setq *default-bindings* (get-vars :id-or-name id-or-name :course course))
  (recursive-write-part *start-letter-token*))

(defun charp (c)
  (member (type-of c) 
	  '(STANDARD-CHAR CHARACTER)))

(defun recursive-write-part (token)
  (cond	((functionp token) (lprint (funcall token)))
	((listp token)
	 (cond ((eq :alt (car token))
		(recursive-write-part
		 (nth (random (1- (length token))) (cdr token))))
	       (t (mapcar #'recursive-write-part token))))
	((or (charp token) (stringp token)) (lprint token))
	((keywordp token) 
	 (cond ((assoc token *grammar*)
		(mapcar #'recursive-write-part (cdr (assoc token *grammar*))))
	       ((vval token) (lprint (vval token)))
	       (t (error "In recursive-write-part, shouldn't get here. Token= ~s" token))))
	(t (error "In recursive-write-part, shouldn't get here. Token= ~s" token))
	))

(defun lprint (string)
  (princ string))

(defparameter *computed-var-vals* 
  `((:short-name-or-pronoun ,#'short-name-or-pronoun)
    (:student-short-name ,#'student-short-name)
    (:time-known-unit ,#'time-known-unit)
    (:time-known-number ,#'time-known-number)
    (:my-course ,#'my-course)
    (:course-grade ,#'course-grade)))


(defparameter *grammar*
  `((:ref-letter :salutation :intro :known-time :course-details :way-known :comments)
    (:salutation "Dear " :to-person-salutation ", ")
    (:intro "It is my " :pleasure/honor " to write in support of " :student-full-name "'s application to the " :program ". ")
    (:known-time "I have known " :student-short-name " for " :time-known-number :time-known-unit ". ")
    (:way-known :short-name-or-pronoun " attended my course entitled \'" :course-attended-title "\'. ")
    (:pleasure/honor (:alt "pleasure" "honor"))
    (:comments :pos :neg)
    (:pos "On the positive side " :short-name-or-pronoun " " :nice-phrase ", ")
    (:neg "on the otherhand " :short-name-or-pronoun " " :neg-phrase ". ")
    (:course-details "In my course, " :my-course " " :short-name-or-pronoun " received a " :course-grade ". ")
    ))

(format t "=== Writing for a known student, 12345 ===~%")
(setq *student-recs* nil) (setq *default-bindings* nil)
(write-letter! :id-or-name 12345 :course "symsys245")
(format t "=== Writing for a UNknown student! ===~%")
(setq *student-recs* nil) (setq *default-bindings* nil)
(write-letter! :id-or-name 00000 :course "symsys245")
