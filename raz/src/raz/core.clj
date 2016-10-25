(ns raz.core)
(import java.lang.Character)

(refer 'clojure.set :only '[union])
(require ['clojure.walk :as 'w])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn char-range-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def ident-initial (union (char-range-set \A \Z) (char-range-set \a \z) #{\_}))

(def ident-subseq (union ident-initial (char-range-set \0 \9)))

(defn get-token [[c & other :as all]]
  (if (nil? c)
    {:type :eof}
    (do
      (cond 
        (ident-initial c)
        (do
          (loop [[c & other] all
                 value []]
            (if (or (nil? c)
                    (not (ident-subseq c)))
              {:type :ident :value value :other other}
              (recur other (conj value c)))))

        (java.lang.Character/isDigit (char c))
        (loop [[c & other] all
               value []]
          (if (or (nil? c)
                  (not (java.lang.Character/isDigit (char c))))
            {:type :number :value value :other other}
            (recur other (conj value c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generatorz

; ((a|b)(c|d))|(
(defmacro deflex [name & defs]
  (list 'quote (map rest defs)))

(defn- altr? [x] (list? x))
(defn- star? [x] (and (vector? x) (= (first x) :star)))
(defn- conc? [x] (and (vector? x) (not (star? x))))

(defn- build-tree [pattern ] nil)

(def ^:dynamic *state-counter* nil)

(defn replace-final [x y]   )

(declare pattern->smachine)

(defn char->smachine [ch]
  (let [result {:start *state-counter*
                *state-counter* [{:chars [ch] :to :final}]}]
    (set! *state-counter* (inc *state-counter*))
    result))

(defn conc->smachine [pattern]
  (let [child-smachines (vec (map pattern->smachine pattern))]
    (reduce
     (fn [acc x]
       (if (empty? acc)
         x
         (merge (w/prewalk #(if (= :final %) (:start x) %) acc)
                (dissoc x :start))))
     {}
     child-smachines)))

(defn altr->smachine [pattern]
  (let [child-smachines (map pattern->smachine pattern)]
    (reduce
     (fn [acc x]
       (if (empty? acc)
         x
         (let [x-start (:start x)
               start-smachine (x x-start)]
           (merge-with into
                       acc
                       (assoc 
                        (dissoc
                         (dissoc x :start) x-start)
                        (:start acc)
                        start-smachine)))))
     {}
     child-smachines)))

(defn pattern->smachine [pattern]
  (cond
    (char? pattern) (char->smachine pattern)
    (conc? pattern) (conc->smachine pattern)
    (altr? pattern) (altr->smachine pattern)))

(defn transform-pattern [pattern]
  (binding [*state-counter* 1]
    (pattern->smachine pattern)))





(defn- alteration->state [pattern]
  nil
  )

(defn- patter->state [pattern]
  (cond
    (altr? pattern) 1
    (conc? pattern) 2
    (star? pattern) 3
    ))

;; a*

  

(deflex f
  (:a [(\a \b) \c])
  (:b [:star \a (\b \c) \d]))

(defn rand-max []
  (rand-int java.lang.Integer/MAX_VALUE))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimenting
(defn many-pluses [n]
  (reduce (fn [acc x]
            (list '+ acc x))
          1
          (repeat n 1)))

(defmacro many-plus [n]
  (many-pluses n))


(defn number-all [tree]
  (letfn [(handle-elem [{last-count :last-count acc :acc} elem]
            (if (not (coll? elem))
              {:last-count last-count :acc (conj acc elem)}
              (let [child-res (traverse elem last-count)]
                {:last-count (:last-count child-res)
                 :acc (conj acc (:node child-res))})))

          (traverse [tree count]
            (let [{last-count :last-count acc :acc}
                  (reduce handle-elem
                          {:last-count (inc count) :acc (empty tree)}
                          tree)]
              {:last-count last-count
               :node {:number count :tree (if (list? acc) (reverse acc) acc)}}))]
               

    (:node (traverse tree 1))))

(defn node [number & transitions]
  {:number number
   :trans (reduce (fn [acc [chars to]]
                    (conj acc {:chars chars :to to}))
                  []
                  (partition 2 transitions))})

(defn replace-previous [seq-of-seqs]
  (letfn [(go [prev curr nexts]
              (replace {nil (first curr)} prev))])
  )
