(ns small.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def rcr 
  (fn [x]
    (if (zero? x) x
        (rcr (dec x)))))
