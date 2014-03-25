; (load "convert-scigen-rules.lisp")

(defun convert (infile outfile)
  (with-open-file 
   (i infile)
   (with-open-file 
    (o outfile :direction :output :if-exists :supersede)
    (loop for line = (read-line i nil nil)
	  with {level = 0
	  with new-{level = 0
	  until (null line)
	  do 
	  (unless (zerop (length line))
	    (let ((line (lispify-line line)))
	    (incf new-{level (+ (count #\{ line) (- (count #\} line))))
	    (cond ((> new-{level {level) (format o "(~s~%" line))
		  ((< new-{level {level) (format o "~s)~%" line))
		  ((= new-{level {level) (format o "(~s)~%" line)))
	    (setq {level new-{level)))))))

(defun lispify-line (line &aux (cc " 	"))
  (mapcan #'(lambda (e) (string-split (string-trim cc e) :delimiter #\space))
	  (string-split line :delimiter #\tab)))
		  
(defun string-split (string &key (delimiter #\space) (copy t))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
             (push (if copy
                       (subseq string last i)
                     (make-array (- i last)
                                 :element-type 'character
                                 :displaced-to string
                                 :displaced-index-offset last))
              substrings)))
      (dotimes (i length)
        (when (eq (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings))))

(defun test ()
  (convert "scirules.in" "scirules.out"))