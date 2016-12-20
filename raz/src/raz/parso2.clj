(ns raz.parso2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here, 'g' means Grammar - a set of rules:
;; [[:E1 :E2]
;;  [:E2 t :E2]]
;; State - collection of items. This is the state
;; of DFA.
;; Item - pair [r pos], where r is rule of grammar,
;; pos - position of dot in this rule.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn drop-nonlazy [n l]
  (cond
    (empty? l) ()
    (> n 0) (drop-nonlazy (dec n) (pop l))
    :else l))
    

(defn rule [g r]
  (g r))

(defn is-nonterm [g s]
  (some (fn [x] (= (first x) s))
        g))

(defn init-state [g]
  (set (zipmap (range (count g)) (repeat (count g) 1))))

(defn next-symbol [g [r pos]]
  (let [rule (rule g r)]
    (if (< pos (count rule))
      (rule pos)
      nil)))

(defn refreshed-syms [g sym-parent]
  (letfn [(impl [g sym-parent result]
            (if (result sym-parent) 
              result
              (let [result (conj result sym-parent)]
                (loop [[r & rs :as g-curr] g
                       result result]
                  (cond
                    (empty? g-curr) 
                    result

                    (= (first r) sym-parent)
                    (recur rs (impl g (second r) result))

                    :else 
                    (recur rs result))))))]

    (impl g sym-parent #{})))
                

(defn refreshed-items [g item]
  (if-let [sym (next-symbol g item)]
    (do 
      (filter (fn [[r _]] 
                ((refreshed-syms g sym) (first (rule g r))))
            (init-state g)))))

(defn next-transitions [g state]
  (reduce 
   (fn [acc [r pos :as item]]
     (if-let [s (next-symbol g item)]
       (let [next-item [r (inc pos)]]
         (merge-with into acc 
                   {s (into #{next-item} (refreshed-items g next-item))}))
       acc))
   {}
   state))

(defn explore-state [g table state]
  (let [transitions (next-transitions g state)]
    (let [table (update table state 
                        #(merge % transitions))
          table (update table :visited 
                        #(conj % state))]
      (reduce
       (fn [acc [_ state]]
         (if (not ((:visited acc) state))
           (explore-state g acc state)
           acc))
       table
       transitions))))

(defn make-transition-table [g]
  (let [init-state (init-state g)]
    (explore-state g {:start init-state
                      :visited #{init-state}}
                   init-state)))

(defn transition-state [table curr-state sym]
  (get-in table [curr-state sym]))

(defn find-reducable-item [g state]
  (some 
   (fn [[r pos :as item]] (if (= (count (rule g r)) pos) item)) 
   state))

(defn find-shiftable-item [g state t]
  (some
   (fn [item] (if (= (next-symbol g item) t) item))
   state))

(defn parse-complete? [g state]
  (some
   (fn [[r pos]] (and (zero? r)
                      (= pos (count (rule g r)))))
   state))

(defn reduce-rule [g trees state]
  (let [item (find-reducable-item g state)        
        rhs-count (dec (second item))
        sym (first (rule g (first item)))
        reduced {:tag sym :content (vec (reverse (take rhs-count trees)))}]
    (conj (drop-nonlazy rhs-count trees) reduced)))

(def output true)

(defn lr-parse [g input]
  (let [table (make-transition-table g)]
  (loop [states (list (init-state g))
         trees (list)
         [t & ts :as input] input]
    (if output
      (do '(clojure.pprint/pprint states)
          '(visualize-state g (peek states))
          (clojure.pprint/pprint trees)
          (clojure.pprint/pprint input)
          (println "***\n")))

    (cond 
      (parse-complete? g (peek states)) 
      (first (reduce-rule g trees (peek states)))

      (and (not (empty? input)) 
           (find-shiftable-item g (peek states) t))
      (recur (conj states (transition-state table (peek states) t))
             (conj trees t)
             ts)

      (find-reducable-item g (peek states))
      (let [item (find-reducable-item g (peek states))
            rhs-count (dec (second item))
            sym (first (rule g (first item)))
            reduced {:tag sym :content (vec (reverse (take rhs-count trees)))}
            states (drop-nonlazy rhs-count states)
            trees (conj (drop-nonlazy rhs-count trees) reduced)]
        (recur states trees input))

      (= (count states) (count trees))
      (recur (conj states 
                   (transition-state 
                    table (peek states) (:tag (peek trees))))
             trees input)

      (empty? input) nil
      
      

      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For viewing data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn visualize-state [g state]
  (doseq [[r pos] state]
    (let [[c1 c2] (split-at pos (rule g r))]
      (let [c11 (concat [(first c1) :->] (rest c1))]
        (println (map name (concat c11 [:!] c2)))))))

(defn visualize-transitions [g transitions]
  (doseq [[s state] transitions]
    (println (name s))
    (visualize-state g state)
    (println)))

(defn first-transitions [g] (next-transitions g (init-state g)))

(defn visualize-first-table-iter [g]
  (visualize-transitions g (next-transitions g (init-state g))))

(defn print-tt [g]
  (let [result (make-transition-table g)]
    (clojure.pprint/pprint (:start result))
    (println)
    (clojure.pprint/pprint (apply dissoc result [:start :visited]))))

(defn print-parse [g input]
  (clojure.pprint/pprint (lr-parse g input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(def g1
  [[:S :E :$]
   [:E :E :+ :E]
   [:E :d]])

(def g2
  [[:S :E :$]
   [:E :E :+ :d]
   [:E :d]])

(def g3
  [[:S :E :$]
   [:E :E :+ :T]
   [:E :T]
   [:T :T :* :P]
   [:T :P]
   [:P :lp :E :rp]
   [:P :d]])

(def g4
  [[:S :E :$]
   [:E :E :+ :T]
   [:E :T]
   [:T :T :* :d]
   [:T :d]])
(def g5
  [[:S :E \$]
   [:E :E \+ :T]
   [:E :T]
   [:T :T \* \d]
   [:T \d]])

(def g6
  [[:S :E :$]
   [:E :E :- :T]
   [:E :T]
   [:T :n]
   [:T :lp :E :rp]])
   

(def is-g1 (init-state g1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions table
;;
;; It has the following structure
;; {state1 {symbol1 shift-entry
;;          symbol2 shift-entry
;;          :reductions [reduce-entry] ;; <<-- optional
;;  state2 {}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shift-entry [state]
  {:type :shift :state state})

(defn reduce-entry [sym length]
  {:type :reduce :sym sym :length length})

(defn reduction-item? [g [r pos]]
  (= (count (rule g r)) pos))

(defn add-reductions [g state]
  {:reductions 
   (reduce 
    (fn [acc [r pos :as item]]
      (if (reduction-item? g item)
        (let [rule (rule g r)]
          (conj acc 
                (reduce-entry (first rule)
                              (dec (count rule)))))
        acc))
    []
    state)})

(defn make-action-table [g]
  (let [tran-table (apply dissoc (make-transition-table g) [:start :visited])]
    (reduce 
     (fn [acc [state transitions]]
       (merge acc
              {state (merge transitions 
                            (add-reductions g state))}))
     {}
     tran-table)))

(defn print-at [g]
  (let [at (make-action-table g)]
    (doseq [[state x] at]
      (println "State:")
      (visualize-state g state)
      (println "Transitions:")
      (visualize-transitions g (dissoc x :reductions))
      (println "Reductions:")
      (clojure.pprint/pprint (:reductions x))
      (println "====================\n\n"))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mk-nd [name links]
  {:name name :links links})


