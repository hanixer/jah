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

(defn init-state [g]
  (zipmap (range (count g)) (repeat (count g) 1)))

(defn next-symbol [g [r pos]]
  (let [rule (rule g r)]
    (if (< pos (count rule))
      (rule pos)
      nil)))

(defn next-transitions [g state]
  (reduce 
   (fn [acc [r pos :as item]]
     (if-let [s (next-symbol g item)]
       (merge-with into acc {s [[r (inc pos)]]})
       acc))
   {}
   state))

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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(def g1
  [[:S :E :$]
   [:E :E :+ :E]
   [:E :d]])

(def is-g1 (init-state g1))
