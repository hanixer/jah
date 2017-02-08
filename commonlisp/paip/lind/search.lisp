(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states
   and search according to successors and combiner."
  (print states)
  (cond
    ((null states) nil)
    ((funcall goal-p (first states)) (first states))
    (t (tree-search
	(funcall combiner
		 (funcall successors (first states))
		 (rest states))
	goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors 
	       #'append))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(defun prepend (x y) (append y x))
	
(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun finite-binary-tree (n)
  "Returns a function that generates a binary tree with n nodes."
  #'(lambda (x)
      (remove-if #'(lambda (child) (> child n))
		 (binary-tree x))))

(defun diff (num)
  "Returns a function that finds the difference between number and argument."
  #'(lambda (x) (abs (- num x))))

(defun sorter (cost-fn)
  "Returns a combiner function that sorts the list according to the cost-fn."
  #'(lambda (x y)
      (sort (append x y) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lower cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))
