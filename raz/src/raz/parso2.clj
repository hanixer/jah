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

(defn refreshed-items [g item]
  (if-let [sym (next-symbol g item)]
    (do 
    (filter (fn [[r _]] (= (first (rule g r)) sym))
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
  (clojure.pprint/pprint (make-transition-table g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(def g1
  [[:S :E :$]
   [:E :E :+ :E]
   [:E :d]])

(def is-g1 (init-state g1))
