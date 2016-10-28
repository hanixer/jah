(ns raz.lexer)

(require ['clojure.walk :as 'w])

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

(defn altr->nfa [nfa1 nfa2]
  (let [start (rand-num)
        final (rand-num)
        states {start [[:empty (:start nfa1)]
                       [:empty (:start nfa2)]]}
        nfa1-additional (merge-with
                         into
                         (:states nfa1)
                         (into {}
                               (for [s (:final nfa1)] [s [[:empty final]]])))
        nfa2-additional (merge-with
                         into
                         (:states nfa2)
                         (into {}
                               (for [s (:final nfa2)] [s [[:empty final]]])))]
    {:start start
     :final #{final}
     :states (merge-with into  states nfa1-additional nfa2-additional)}))
