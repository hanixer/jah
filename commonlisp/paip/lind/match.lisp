(defparameter fail nil "indicates pat-match fail")
(defparameter no-bindindings '((t . t)))

(defun getbased (x)
  (+ x 1))

(defun simple-equal (x y)
  "Are x and y equal?"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

(defun pat-match (pattern input &optional (bindings no-bindindings))
  "Does pattern match input?"
  (cond
    ((eq bindings fail) fail)
    ((variable-p pattern)
     (match-variable pattern input bindings))
    ((equal pattern input) bindings)
    ((segment-pattern-p pattern)
     (segment-matcher pattern input bindings))
    ((single-pattern-p pattern)
     (single-matcher pattern input bindings))
    ((and (consp pattern) (consp input))
     (pat-match (rest pattern) (rest input)
		(pat-match (first pattern) (first input)
			   bindings)))
    (t fail)))

(defun match-variable (pattern input bindings)
  "Matches variable in input and returns new bindings"
  (let ((binding (get-binding pattern bindings)))
    (cond
      ((not binding) 
       (extend-bindings pattern input bindings))
      ((equal input (binding-val binding))
       bindings)
      (t fail))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in bindings."
  (assoc var bindings))

(defun binding-val (binding)
  "Return the value of the binding."
  (cdr binding))

(defun lookup (var bindings)
  "Return the value of binding in bindings list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (cons var val) 
	(if (eq bindings no-bindindings)
	    nil
	    bindings)))

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this type of matcher"
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun segment-match-fn (x)
  "Get the segment matching function for x,
   if it's a symbol that has one."
  (and (symbolp x) (get x 'segment-match)))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?"
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun single-matcher (pattern input bindings)
  "Call the right function for this type of matcher"
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun single-match-fn (x)
  "Get the single matching function for x"
  (and (symbolp x) (get x 'single-match)))

(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun match-is (var-and-pred input bindings)
  "Succeed and bind variable if the input satisfied
   pred, where var-and-pred is a list (var pred)"
  (let* ((var (first var-and-pred))
	 (pred (second var-and-pred))
	 (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
	    (not (funcall pred input)))
	fail
	new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match input."
  (cond 
    ((eq bindings fail) fail)
    ((null patterns) bindings)
    (t (match-and (rest patterns) input
		  (pat-match (first patterns) input bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any pattern matches input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
				     input bindings)))
	(if (eq new-bindings fail)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match input."
  (if (null patterns)
      t
      (match-not (rest patterns) input bindings)))
		 


(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against the input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
	(let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match
			 pat (subseq input pos)
			 (match-variable var (subseq input 0 pos)
					 bindings))))
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun first-match-pos (patl input start)
  "Find the first position that patl could possible match."
  (cond
    ((and (atom patl) (not (variable-p patl)))
     (position patl input :start start :test #'equal))
    ((< start (length input)) start)
    (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one and more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))
  
(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables."
  (print pattern)
  (and (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
	 (progn 
	   (print (mapcar #'car bindings))
	   (print (mapcar #'cdr bindings))
	   (eval (second (first pattern)))))
       (pat-match (rest pattern) input bindings)))
	   
	   
