(ns small.curch)

(defn church [n]
  (if (zero? n)
    (fn [s]
      (fn [z]
        z))
    (fn [s]
      (fn [z]
        (s (((church (dec n)) s) z))))))

(defn unchurch [m]
  ((m inc) 0))

(defn add [m n]
  (fn [s]
    (fn [z]
      ((m s) ((n s) z)))))

(defn mul [m n]
  (fn [s]
    (fn [z]
      ((m 
       (fn [x] 
         ((n s) x)) )
       z))))

(defn po [m n]
  (fn [s]
    (fn [z]
      ((((n
          (fn [x]
            (mul m x)))
         (s z))
        s) z))))

(defn prd [m]
  (fn [s]
    (fn [z]
      (first ((m
       (fn [[_ x]]
         [x (s x)]))
       [z z])))))

(defn sub [m n]
  (fn [s]
    (fn [z]
      ((((n
          prd)
         m) s) z))))
    
(defn iszro [m]
  (fn [t]
    (fn [f]
      ((m 
        (fn [x]
          f))
       t))))

(defn tcons [x l]
  (fn [c]
    (fn [n]
  
      (c x ((l c) n)))))

(def tnil
  (fn [c]
    (fn [n]
  
      n)))

(defn thead [l]
  ((l (fn [x r]
        (println "hir")
        (println x)
        (println r)
        x)) tnil))

(defn totcons [seq]
  (reduce 
   (fn [acc x]
  
     (tcons x acc))
   tnil
   seq))

(defn fromtcons [tc]
  
  ((tc (fn [x r] (conj r x))) []))
