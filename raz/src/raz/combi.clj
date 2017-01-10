(ns raz.combi)

(def input "abc")
(def inp-length (count input))

(defn term [x]
  (fn [lr-ctxt pos]
    (if (= (nth input pos) x)
      [nil #{(inc pos)}]
      [nil #{}])))

(defn <+> [p q]
  (fn [lr-ctxt pos]
    (let [[cuts1 pos-set1] (p lr-ctxt pos)
          [cuts2 pos-set2] (q lr-ctxt pos)]
      [(clojure.set/union cuts1 cuts2)
       (clojure.set/union pos-set1 pos-set2)])))

(defn *> [p q]
  (fn [lr-ctxt pos]
    (let [[cuts1 pos-set1] (p lr-ctxt pos)]
      (reduce
       (fn [[cuts end-pos-set] end-pos1]
         (let [[cuts2 pos-set2] (q lr-ctxt end-pos1)]
           [(clojure.set/union cuts cuts2)
            (clojure.set/union end-pos-set pos-set2)]))
       [cuts1 #{}]
       pos-set1))))

(defn save-results [memo cuts pos-set lr-ctxt pos id]
  (let [lr-ctxt-save 
        (reduce 
         (fn [ctxt [id pos :as cut]]
           (assoc-in ctxt [pos id] (get-in lr-ctxt [pos id])))
         {}
         cuts)]
    (swap! memo assoc-in [pos id] [lr-ctxt-save [cuts pos-set]])))

(defn can-reuse-result [lr-ctxt-stored lr-ctxt-curr]
  (every? (fn [[pos id-count-map]]
            (every? (fn [[id count]]
                      (<= count (get-in lr-ctxt-curr [pos id])))
                    id-count-map))
          lr-ctxt-stored))

(defn should-cut? [lr-ctxt pos id]
  (if-let [counter (get-in lr-ctxt [pos id])]
    (> counter (inc (- inp-length pos)))
    false))

(defn inc-counter [lr-ctxt pos id]
  (let [counter (or (get-in lr-ctxt [pos id]) 0)]
    (assoc-in lr-ctxt [pos id] (inc counter) )))

(let [memo (atom {})]
  (defn memoparser [id p]
    (fn [lr-ctxt pos]
      (let [[lr-ctxt-stored result :as stored] (get-in @memo [id pos])]
      (cond 
        (and stored (can-reuse-result lr-ctxt-stored lr-ctxt))
        result

        (should-cut? lr-ctxt pos id)
        [#{[id pos]} #{}]

        :else
        (let [lr-ctxt-updated (inc-counter lr-ctxt pos id)
              [cuts pos-set :as result] (p lr-ctxt-updated pos)]
          (save-results memo cuts pos-set lr-ctxt-updated pos id)
          result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pp (<+> (term \a ) (term \b)))

(declare A)

(def S (memoparser 1 (<+> A
                          (term \a))))

(def A (memoparser 2 (*> S (term \a))))

(defn parse [p]
  (p {} 0))
