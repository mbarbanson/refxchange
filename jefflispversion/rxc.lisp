; (load "rxc.lisp")

;;; This will automatically write a letter by calling (write-letter!)
;;; at the end. You need to either comment that out, or something, to
;;; stop it. 

;;; If you want to reset the vars just (setq *default-bindings* nil)
;;; before (write-letter!), and it will re-ask.

(defparameter *start-letter-token* :ref-letter)

(defparameter *grammar* nil) ;; set later bcs of fwd refs for fns.

(defparameter *vars* 
  '((:to-person-salutation "How should the salutation read, for example: 'Dr. Smith'? ")
    (:student-full-name "What is the student's full name, as 'Jane Doe'? ")
    (:time-known "How long have you known this student, for example '3 months'? ")
    (:program "What is the name of the program to which the student is applying? ")
    (:gender "Gender (m/f): ")
    (:course-attended-title "The name of the class that the student took from you? ")
    (:student-in-top-% "What percentile was this student (for example, a student in the top 5% you would say: '5')? ")
    (:nice-phrase "Say something positive about this student (e.g., 'is a quick study')")
    (:neg-phrase "Say something slightly negative about this student (e.g., 'was often late to class')")
    ))

(defstruct student :id :full-name :brief-name :gpa :ecas :classes)

(defvar *student-recs* nil)

(defun load-student-recs ()
  (setq *student-recs*
	(with-open-file
	 (i "studnet-db.lisp")
	 (loop for rec = (read i nil nil)
	       until (null rec)
	       collect (make-student :id (getf rec :id)
				     :full-name (getf rec :full-name)
				     :brief-name (getf rec :brief-name)
				     :gpa (getf rec :gpa)
				     :ecas (getf rec :ecas)
				     :classes (getf rec :classes)
				     )))))

(defun find-student (id-or-name)
  (unless *student-recs* (load-student-recs))
  (loop for rec in *student-recs*
	when (or (and (numberp id) (= id-or-name (student-id rec)))
		 (and (stringp id) (string-equal id-or-name (student-full-name rec))))
	do (return rec)))
	
(defvar *default-bindings* nil)

(defun get-vars (&key id-or-name class)
  "Pull everything that we can from the student record, and ask for the rest."
  (let ((student (find-student :id-or-name id-or-name)))
    (setq *default-bindings* 
	  (loop for (var prompt) in *vars*
		do (princ prompt)
		collect `(,var ,(read-line))))))

(defun vval (var &optional (bindings *default-bindings*))
  (or (cadr (assoc var bindings))
      (error "In VVAL, tried to lookup ~a, which isn't a valid var." var)))

(defun short-name-or-pronoun (&optional (bindings *default-bindings*))
  (if (zerop (random 2))
      (if (string-equal "m" (vval :gender bindings)) "he" "she")
      (student-short-name bindings)))

(defun student-short-name (&optional (bindings *default-bindings*))
  (let* ((fn (vval :student-full-name bindings)))
    (subseq fn 0 (position #\space fn))))

(defun time-known-unit (&optional (bindings *default-bindings*))
  (let* ((tf (vval :time-known bindings)))
    (subseq tf (position #\space tf))))
(defun time-known-number (&optional (bindings *default-bindings*))
  (let* ((tf (vval :time-known bindings)))
    (subseq tf 0 (position #\space tf))))

(defun write-letter! (&key id-or-name class)
  (terpri) (terpri)
  (unless *default-bindings*
    (get-vars :id-or-name id :class class))
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

(setq *grammar*
  `((:ref-letter :salutation :intro :known-time :way-known :comments)
    (:salutation "Dear " :to-person-salutation ", ")
    (:intro "It is my " :pleasure/honor " to write in support of " :student-full-name "'s application to the " :program ". ")
    (:known-time "I have known " :student-short-name " for " :time-known-number :time-known-unit ". ")
    (:way-known :short-name-or-pronoun " attended my course entitled \'" :course-attended-title "\'. ")
    (:pleasure/honor (:alt "pleasure" "honor"))
    (:comments :pos :neg)
    (:pos "On the positive side " :short-name-or-pronoun " " :nice-phrase ", ")
    (:neg "on the otherhand " :short-name-or-pronoun " " :neg-phrase ". ")
    (:short-name-or-pronoun ,#'short-name-or-pronoun)
    (:student-short-name ,#'student-short-name)
    (:time-known-unit ,#'time-known-unit)
    (:time-known-number ,#'time-known-number)
    ))

#|
(setq *default-bindings* 
  '((:to-person-salutation "Dr. Smith")
    (:student-full-name "Jane Doe")
    (:time-known "3 months")
    (:program "The Stanford MBA program")
    (:gender "f ")
    (:course-attended-title "Business As Usual (BS101)")
    (:student-in-top-% 5)
    (:nice-phrase "is a quick study")
    (:neg-phrase "was often late to class")
    ))
|#

(write-letter! :id-or-name 12345 :class "symsys245")
