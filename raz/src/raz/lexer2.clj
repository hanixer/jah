(ns raz.lexer2)
(refer 'clojure.set :only '[union])

(defn char-range-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def ident-initial (union (char-range-set \A \Z) (char-range-set \a \z) #{\_}))

(def ident-subseq (union ident-initial (char-range-set \0 \9)))

'(let [a (int \a)
      z (int \z)
      A (int \A)
      Z (int \Z)
      zer (int \0)
      nin (int \9)]
  (defn ident-initial [c]
    (or (<= A (int c) Z)
        (<= a (int c) z)
        (= \_ c)))
  (defn ident-subseq [c]
    (or (<= A (int c) Z)
        (<= a (int c) z)
        (<= zer (int c) nin)
        (= \_ c))))

(defn empty-source [lex]
  (>= (.get (:index lex) 0) (count (:source lex))))

(defn update-lex [lex token index]
  (.add (:tokens lex) token)
  (.set (:index lex) 0 index)
  lex)

(defn scan-identifier [lex]
  ;(assert (ident-initial (first (:source lex))))
  (loop [index (inc (.get (:index lex) 0))
         value [(nth (:source lex) (dec index))]]
    (if (and (< index (count (:source lex)))
             (ident-subseq (nth (:source lex) index)))
      (recur (inc index) (conj value (nth (:source lex) index)))
      (update-lex lex {:type :identifier :value value} index))))

(defn scan [{tokens :tokens source :source index :index :as lex}]
  (cond
    (Character/isWhitespace ^char (nth source (.get index 0))) 
    (do
      (.set index 0 (inc (.get index 0)))
      lex)

    (ident-initial (nth source (.get index 0))) (scan-identifier lex)))



(defn tokenize [source]
  
  (let [index (java.util.ArrayList.)]
    (.add index 0)
    (loop [lex {:tokens (java.util.ArrayList.) :source source :index index}]
          (if (empty-source lex)
            (:tokens lex)
            (recur (scan lex))))))

(defn measure-tokenizer [n]
  (let [s (clojure.string/join (repeat n "abcde "))]
    (time (tokenize s))
    (* n (count "abcde "))))
