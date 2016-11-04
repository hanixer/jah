(ns raz.lexer)

(require ['clojure.walk :as 'w])

(def ppr clojure.pprint/pprint)

(def example-nfa {:start 1
                  :final #{2 4}
                  :states {1 [[:empty 2] [\a 4]]
                           3 [[\b 4]]}})


(defn rand-num []
  (.hashCode (java.util.UUID/randomUUID)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
(def a (char->nfa \a))
(def b (char->nfa \b))
(def ab (conc->nfa a b))
(def c (char->nfa \c))
(def d (char->nfa \d))
(def ab-c (altr->nfa ab c))
