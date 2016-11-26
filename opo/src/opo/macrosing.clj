(ns opo.macrosing)

(defmacro when1 [test & body]
  (list 'if test
        (concat (list 'do)
                body)))

(when1 (+ 1 2) 
       (println "That's good enough")
       (println "Cause it works"))
(when1 nil 
       (println "That's good enough")
       (println "Cause it works"))

(cond 
  (= 1 2) (println "1 eq 2")
  (#{1 2 3} 4) (println "one is almost begin")
  (+ 1 2) (println "that's right"))

(defmacro cond1 [& forms]
  (when forms
    (list 'if (first forms)
          (if (next forms)
            (second forms)
            (throw (java.lang.IllegalArgumentException. "cond requires bla bla bla")))
          (cons 'cond1 (drop 2 forms)))))

(cond1
  (= 1 2) (println "1 eq 2")
  (#{1 2 3} 4) (println "one is almost begin")
  (+ 1 2) (println "that's right"))

(if (= 1 2)
  (println "jio")
  (if (= 1 3)
    (println "jierwijowjr")
    (if (+ 1 2)
      (println "jiwefjojrtiotj"))))

(defn f [acc x]
    (conj acc [x (* x x)]))

(defn reduce-b [f v coll]
  (letfn [(g [[x & xs :as coll]]
            (if (empty? coll)
              v
              (f (g xs) x)))]
    (g coll)))
  
(reduce-b
 f
 []
 [1 2 3])

(defmacro assert1 [x]
  (when *assert*
    (list 'when-not x
          (list 'throw
                (list 'new 'java.lang.AssertionError
                      (list 'quote x))))))

(defmacro assert2 [x]
  (when *assert*
    `(when-not ~x
       (throw (new java.lang.AssertionError '~x)))))

(assert2 (= 3 (+ 1 2)))

(defmacro make-adder [x]
  (let [y-sym (gensym "y")]
    (list 'fn [y-sym]
          (list `+ x y-sym))))

(defmacro make-adder2 [x]
  (let [y-sym (gensym "y")]
    `(fn [~y-sym]
          (+ ~x ~y-sym))))

(defmacro make-adder3 [x]
    `(fn [y#]
       (+ ~x y#)))

(make-adder2 5)
(make-adder3 5)

;; It's wrong i think
(defmacro and1
  ([] true)
  ([x] `(if ~x true false))
  ([x & other]

   (print (first x) " " (second x))
   `(if ~x 
      (and1 ~@other)
      false)))

(defmacro info-about-caller []
  (clojure.pprint/pprint {:form &form :env &env})
  `(println "You've just called a macro!"))

(defmacro inspect-caller-locals []
  (into {} (map (fn [k] [(list 'quote k) k]) (keys &env))))

(let [x 10] (inspect-caller-locals))

(defn f [x]
  (info-about-caller))

'(letter 5)
'(let [x1 1]
   (let [x2 2]
     (let [x3 3]
       (let [x4 4]
         (let [x5 5]
           [x1 x2 x3 x4 x5])))))

'(letter 1)
'(let [x1 1]
   [x1])

'(letter 2)
'(let [x1 1]
   (let [x2 2]
     [x1 x2]))

(defmacro letter [n]
  (letfn [(g [m acc]
            (let [x-sym (gensym)]
            `(let [~x-sym ~m]
               ~(if (< m n)
                  (g (inc m) (conj acc x-sym))
                  (conj acc x-sym)))))]
    (g 1 [])))

'(letter2 1)
'(let [x1 1]
   [x1])

'(letter2 2)
'(let [x1 1
       x2 2]
   [x1 x2])

(defmacro letter2 [n]
  (letfn [(g [m bindings symbols]
            (let [x-sym (gensym)
                  b (into bindings [x-sym m])
                  s (conj symbols x-sym)]
              (if (< m n)
                (g (inc m) b s)
                `(let ~b ~s))))]
    (g 1 [] [])))
