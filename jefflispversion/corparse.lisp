; (load "corparse.lisp")

;;; Parses letters from the corpus and creates grammars from them.

(defvar *triples* nil)

(defun corparse-file (path)
  (setq *triples*
	(with-open-file
	 (i path)
	 (triplify 
	  (loop for line = (read-line i nil nil)
	       until (null line)
	       append (string-split line)
	       )))))

(defun triplify (l)
  (loop for l+ on l
	collect (list (car l+) (cadr l+) (caddr l+))))

(defun string-split (string &key (delimiter #\space) (copy t) num-values)
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
      (if num-values
          (pad-or-truncate num-values (nreverse substrings))
        (nreverse substrings)))))

(corparse-file "~/refxchange/corpus/jeff/201403251428.txt")
(pprint *triples*)

(defun mgen (&key (len 100))
  (loop for l below len
	with key = nil
	as triple = (find-random-keyed-triple key)
	do (setq key (car (last triple)))
	collect triple
	))

(defun find-random-keyed-triple (key)
  (let ((triples 
	 (loop for triple in *triples*
	       when (or (null key) (string-equal (first triple) key))
	       collect triple)))
    (nth (random (length triples)) triples)))

(loop for (a b c) in (mgen)
      do (princ b) (princ #\space) (princ c) (princ #\space)
)
