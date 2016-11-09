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

(defn conc->nfa [nfa1 nfa2]
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

(defn add-final-trans-to [nfa new-final]
  (merge-with
   into
   (:states nfa)
   (into {}
         (for [s (:final nfa)] [s [[:empty new-final]]]))))

(defn altr->nfa [nfa1 nfa2]
  (let [start (rand-num)
        final (rand-num)
        states {start [[:empty (:start nfa1)]
                       [:empty (:start nfa2)]]}
        nfa1-additional (add-final-trans-to nfa1 final)
        nfa2-additional (add-final-trans-to nfa2 final)]
    {:start start
     :final #{final}
     :states (merge-with into  states nfa1-additional nfa2-additional)}))

(defn star->nfa [nfa]
  (let [start (rand-num)
        final #{start}
        states (merge-with into
                           (add-final-trans-to nfa (:start nfa))
                           {start [[:empty (:start nfa)]]})]
    {:start start
     :final (into (:final nfa) final)
     :states states}))

(defn nfa->dfa [nfa]
  )

(defn empty-transitions [nfa state]
  (map second (filter #(= (first %) :empty)
                      ((:states nfa) state))))

(defn immediate-states-for-single-state [nfa state]
  (letfn [(impl [nfa state visited]
            (if (visited state)
              []
              (let [e-states (empty-transitions nfa state)
                    visited (conj visited state)]
                (reduce (fn [acc x]
                          (into acc (impl nfa x visited)))
                        [state]
                      e-states))))]
    (impl nfa state #{})))

(defn immediate-states-for-multiple-states [nfa states]
  (reduce (fn [acc x]
            (into acc (immediate-states-for-single-state nfa x)))
          []
          states))

(defn expand-e-transitions [nfa states]
  (let [expand-state
        (fn [state]
          (let [immediate-states (empty-transitions nfa state)]
            )
          
          )]
    )
  )



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

(def a (char->nfa \a))
(def b (char->nfa \b))
(def ab (conc->nfa a b))
(def c (char->nfa \c))
(def d (char->nfa \d))
(def ab-c (altr->nfa ab c))
