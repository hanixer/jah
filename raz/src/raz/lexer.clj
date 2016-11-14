(ns raz.lexer)

(require ['clojure.walk :as 'w])

(def ppr clojure.pprint/pprint)






(defn rand-num []
  (.hashCode (java.util.UUID/randomUUID)))

(let [counter (atom 0)]
  (defn rand-num []
    (swap! counter inc)
    @counter))

(defn char->nfa [ch]
  (let [start (rand-num)
        final (rand-num)]
    {:start start
     :final #{final}
     :states {start [[ch final]]}}))

(defn conc->nfa 
  ([nfa1 nfa2]
   (let [link-transitions (reduce
                          (fn [acc state]
                            (conj acc
                                  [state [[:empty (:start nfa2)]]]))
                          {}
                          (:final nfa1))]
    {:start (:start nfa1)
     :final (:final nfa2)
     :states
     (merge-with into (:states nfa1) link-transitions (:states nfa2))}))
  ([nfa1 nfa2 & tail]
   (if (empty? tail)
     (conc->nfa nfa1 nfa2)
     (apply conc->nfa (conc->nfa nfa1 nfa2) (first tail) (rest tail)))))

(defn add-final-trans-to [nfa new-final]
  (merge-with
   into
   (:states nfa)
   (into {}
         (for [s (:final nfa)] [s [[:empty new-final]]]))))

(defn altr->nfa 
  ([nfa1 nfa2]
  (let [start (rand-num)
        final (rand-num)
        states {start [[:empty (:start nfa1)]
                       [:empty (:start nfa2)]]}
        nfa1-additional (add-final-trans-to nfa1 final)
        nfa2-additional (add-final-trans-to nfa2 final)]
    {:start start
     :final #{final}
     :states (merge-with into  states nfa1-additional nfa2-additional)}))
  ([nfa1 nfa2 & tail]
   (if (empty? tail)
     (altr->nfa nfa1 nfa2)
     (apply altr->nfa (altr->nfa nfa1 nfa2) (first tail) (rest tail)))))

(defn star->nfa [nfa]
  (let [start (rand-num)
        final #{start}
        states (merge-with into
                           (add-final-trans-to nfa (:start nfa))
                           {start [[:empty (:start nfa)]]})]
    {:start start
     :final (into (:final nfa) final)
     :states states}))


(defn empty-transitions [transitions state]
  (map second (filter #(= (first %) :empty)
                      (transitions state))))

(defn immediate-states-for-single-state [transitions state]
  (letfn [(impl [state visited]
            (if (visited state)
              []
              (let [e-states (empty-transitions transitions state)
                    visited (conj visited state)]
                (reduce (fn [acc x]
                          (into acc (impl x visited)))
                        #{state}
                      e-states))))]
    (impl state #{})))

(defn immediate-states-for-multiple-states [transitions states]
  (reduce (fn [acc x]
            (into acc (immediate-states-for-single-state transitions x)))
          #{}
          states))

(defn add-transition [nfa transitions [symbol state]]
  (if (not (= symbol :empty))
    (update transitions symbol
            (fn [states s] (if states (into states s) s))
            (immediate-states-for-single-state (:states nfa) state))
    transitions))

(defn out-transitions [nfa dfa-state]
    (reduce (fn [acc x]
              (merge-with 
               into acc 
              (reduce 
               #(add-transition nfa %1 %2)
               {}
               (get-in nfa [:states x]))))
           {}
           dfa-state))
   
(defn nfa->dfa-states 
  ([nfa dfa-state]
   (nfa->dfa-states nfa dfa-state #{}))
  ([nfa dfa-state visited]
  (if (visited dfa-state) 
    {}
    (let [trans (out-transitions nfa dfa-state)]
    (reduce 
     (fn [acc [symbol state]]
       (merge acc (nfa->dfa-states nfa state (conj visited dfa-state))))
     {dfa-state trans}
     trans)))))

(defn all-states [dfa-states]
  (reduce 
   (fn [acc [state trans]]
     (into (conj acc state) (map second trans)))
   #{}
   dfa-states))

(defn dfa-final-states [nfa dfa-states]
  (set (filter #(some (:final nfa) %) (all-states dfa-states))))

(defn nfa->dfa [nfa]
  (let [start (immediate-states-for-single-state
               (:states nfa)
               (:start nfa))
        states (nfa->dfa-states nfa start)]
    {:start start
     :final (dfa-final-states nfa states)
     :states states}))

(defn replace-in-map [m mapping]
  (let [mapping-keys (keys mapping)]
    (reduce
     (fn [acc [k v]]
       (merge acc
              {(if (mapping k) (mapping k) k)
               (cond
                 (map? v) (replace-in-map v mapping)
                 (mapping v) (mapping v)
                 :else v)}))
     {}
     m)))
     
(defn simplify-dfa [dfa]
  (let [all (all-states (:states dfa))
        mapping (zipmap all (range))]
    {:start (mapping (:start dfa))
     :final (set (replace mapping (:final dfa)))
     :states (replace-in-map (:states dfa) mapping)}))

(def nfa->dfa-s 
  (comp simplify-dfa nfa->dfa))

(defn new-state [dfa s c]
  (get-in dfa [:states s c]))

(defn dfa-accepts? [dfa s]
  (loop [state (:start dfa)
         s s]
    (cond
      (and (empty? s) ((:final dfa) state)) true
      (not (empty? s)) (recur (new-state dfa state (first s)) (rest s))
      :else false)))

(defn pattern->nfa [pattern]
  (cond
    (vector? pattern) (apply conc->nfa (map pattern->nfa pattern))
    (list? pattern) (apply altr->nfa (map pattern->nfa pattern))
    (set? pattern) (apply star->nfa (map pattern->nfa pattern))
    (symbol? pattern) (-> pattern name first char->nfa)
    (char? pattern) (char->nfa pattern)
    :else (throw (Exception. "Wrong pattern"))))

(defn pattern->dfa [pattern]
  (-> pattern pattern->nfa nfa->dfa-s))

(defn apply-conc [operands operators]
  {:type :conc :value [(peek (pop operators)) (peek operators)]})

(defn apply-op [{args :args ops :ops}]
  (let [op (peek ops)]
    (cond
      (or (= op :conc) (= op :altr))
      (let [x (first (rest args))
            y (first args)]
        {:args (conj (drop 2 args) 
                     ((if (= op :conc) conc->nfa altr->nfa) x y))
         :ops (pop ops)})
      
      (= op :star)
      {:args (conj (pop args) (star->nfa (peek args)))
       :ops (pop ops)}

      :else
      (throw (Exception. (str "Wrong operation " op))))))

(defn apply-paren [{args :args ops :ops :as stack}]
  (cond 
      (= (peek ops) :paren) 
      {:args args :ops (pop ops)}

      (#{:conc :altr :star} (peek ops))
      (apply-paren (apply-op {:args args :ops ops}))

      :else
      (throw (Exception. (str "Wrong operation " (peek ops))))))

(defn apply-to-end [{args :args ops :ops :as stack}]
  (cond 
    (empty? ops) stack

    (#{:conc :altr :star} (peek ops))
    (apply-to-end (apply-op stack))

    :else
    (throw (Exception. (str "Wrong operation " (peek ops))))))            

(defn re->nfa [pattern]
  (let [result 
        (loop [[c & cs :as input] pattern
               stack {:args (list) :ops (list)}]
          (if (empty? input)
            (apply-to-end stack)
            (cond 
              (= c \|)
              (if (#{:altr :conc} (peek (:ops stack)))
                (recur cs (update (apply-op stack) :ops #(conj % :altr)))
                (recur cs (update stack :ops #(conj % :altr))))

              (= c \@)
              (if (= (peek (:ops stack)) :conc)
                (recur cs (update (apply-op stack) :ops #(conj % :conc)))
                (recur cs (update stack :ops #(conj % :conc))))

              (= c \*)
              (recur cs (apply-op (update stack :ops conj :star)))

              (= c \()
              (recur cs (update stack :ops #(conj % :paren)))

              (= c \))
              (recur cs (apply-paren stack))

              :else 
              (recur cs (update stack :args conj (char->nfa c))))))]
    (-> result :args first)))

(defn adapt-re [pattern]
  "Adds '@' characters in places where
   concatenation is assumed:
     abc => a@b@c
     (a|b)(c|d) => (a|b)@(c|d)"
  (loop [[c1 c2 & cs :as input] pattern
         result []]
    (cond
      (empty? input) (clojure.string/join result)

     (and (not (#{\| \(} c1))
          (not (nil? c2))
          (not (#{\| \* \)} c2)))
     (recur (rest input) (into result [c1 \@]))

     :else
     (recur (rest input) (conj result c1)))))

(defn re->dfa [pattern]
  (-> pattern adapt-re re->nfa nfa->dfa-s))      
           
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
(def example-nfa {:start 1
                  :final #{2 4}
                  :states {1 [[:empty 2] [\a 4]]
                           3 [[\b 4] [:empty 1] [:empty 2]]}})
(def nfa3 {:start 1
           :final #{4 5 6 8}
           :states {1 [[:empty 2] [\a 4]]
                    2 [[:empty 3] [\b 5] [:empty 8]]
                    3 [[:empty 6]]}})

(def nfa4 {:start 1
           :final #{7}
           :states {1 [[:empty 2] [:empty 5]]
                    2 [[\a 3] [\b 4]]
                    3 [[:empty 8]]
                    5 [[\a 6] [\c 7]]}})

(def nfa5 {:start 1
           :final #{7}
           :states {1 [[:empty 2] [\a 7]]
                    2 [[\a 3] [\b 4]]
                    4 [[\a 5] [\b 6]]
                    7 [[\b 8] [\c 11]]
                    8 [[:empty 9] [:empty 12]]
                    9 [[\a 10]]
                    11 [[:empty 12]]}})

(def nfa6 {:start 1
           :final #{7}
           :states {1 [[:empty 2] [\a 7]]
                    2 [[\a 3] [\b 4]]
                    3 [[\c 1]]
                    4 [[\a 5] [\b 6]]
                    7 [[\b 8] [\c 11]]
                    8 [[:empty 9] [:empty 12]]
                    9 [[\a 10]]
                    11 [[:empty 12]]}})

(def a (char->nfa \a))
(def b (char->nfa \b))
(def ab (conc->nfa a b))
(def c (char->nfa \c))
(def d (char->nfa \d))
(def ab-c (altr->nfa ab c))

(def nfa7 
  (conc->nfa (conc->nfa (conc->nfa (star->nfa (altr->nfa a b)) a) b) b))
(def dfa7 (nfa->dfa nfa7))
(def dfa8 (nfa->dfa-s (star->nfa (altr->nfa a b))))
(def dfa9 (nfa->dfa-s (conc->nfa (star->nfa a) b)))
(def dfa1 (nfa->dfa-s (conc->nfa (conc->nfa (star->nfa a) b) b)))

(def n1 (star->nfa (char->nfa \a)))
(def d1 (nfa->dfa-s n1))

(def n2 (conc->nfa n1 (char->nfa \a)))
(def d2 (nfa->dfa-s n2))

(def n3 (conc->nfa n2 (char->nfa \b)))
(def d3 (nfa->dfa-s n3))

(def n4 (conc->nfa n3 (char->nfa \a)))
(def d4 (nfa->dfa-s n4))

;(def d5 (pattern->dfa '[(a b c _) #{(a b c _ \1 \2 \3)}]))
