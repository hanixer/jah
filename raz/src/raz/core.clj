(ns raz.core)
(import java.lang.Character)

(refer 'clojure.set :only '[union])
(require ['clojure.walk :as 'w])

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(refer 'clojure.set :only '[union])

(defn char-range-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def ident-initial (union (char-range-set \A \Z) (char-range-set \a \z) #{\_}))

(def ident-subseq (union ident-initial (char-range-set \0 \9)))

(defn update-lex [lex token source]
  (assoc (update lex :tokens conj token) :source source))

(defn scan-identifier [lex]
  (assert (ident-initial (first (:source lex))))
  (loop [[c & cs :as source] (rest (:source lex))
         value [(first (:source lex))]]
    (if (ident-subseq c)
      (recur cs (conj value c))
      (update-lex lex {:type :identifier :value value} source))))

(defn scan [{tokens :tokens [c & cs :as source] :source :as lex}]
  (cond
    (Character/isWhitespace c) (assoc lex :source cs)
    (ident-initial c) (scan-identifier lex)))

(defn tokenize [source]
  (loop [lex {:tokens [] :source source}]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (scan lex)))))

(defn measure-tokenizer [n]
  (let [s (clojure.string/join (repeat n "abcdef "))]
    (time (tokenize s))
    (* n (count "abcdef "))))


(defn update-lex-transient [lex token source]
  (assoc! 
   (assoc! lex :tokens 
          (conj! (:tokens lex) token))
   :source source))

(defn scan-identifier-transient [lex]
  (assert (ident-initial (first (:source lex))))
  (loop [[c & cs :as source] (rest (:source lex))
         value (transient [(first (:source lex))])]
    (if (ident-subseq c)
      (recur cs (conj! value c))
      (update-lex-transient lex {:type :identifier :value (persistent! value)} source))))

(defn scan-number [source]
  (loop [[c & other :as all] source
         value []]
    (if (or (nil? c)
            (not (java.lang.Character/isDigit (char c))))
      {:type :number :value value :other all}
      (recur other (conj value c)))))



(defn scan-transient [{tokens :tokens [c & cs :as source] :source :as lex}]
  (cond
    (Character/isWhitespace c) (assoc! lex :source cs)
    (ident-initial c) (scan-identifier-transient lex)
    (java.lang.Character/isDigit (char c)) '(scan-number all)))

(defn tokenize-transient [source]
  (loop [lex (transient {:tokens (transient []) :source source})]
    (if (empty? (:source lex))
      (persistent! (:tokens lex))
      (recur (scan-transient lex)))))



(def tokenize tokenize-transient)

'(defn scan2 [lex]
  (loop [state 1
         value []
         [c & cs :as source] (:source lex)]
    (cond
      (empty? source) (assoc lex :source source)
      (and (= state 1) (Character/isWhitespace c)) (recur state value cs)
      (and (= state 1) (ident-initial c)) (recur 3 (conj value c) cs)
      (and (= state 3) (ident-subseq c)) (recur 3 (conj value c) cs)
      :else (update-lex lex {:type :identifier :value value} source))))

'(defn tokenize [source]
  (loop [lex {:tokens [] :source source}]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (scan2 lex)))))
  


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
