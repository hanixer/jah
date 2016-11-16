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

(defn update-lex [lex token source]
  (.add (:tokens lex) token)
  (assoc lex :source source))

(defn scan-identifier [lex]
  (assert (ident-initial (first (:source lex))))
  (loop [[c & cs :as source] (rest (:source lex))
         value [(first (:source lex))]]
    (if (ident-subseq c)
      (recur cs (conj value c))
      (update-lex lex {:type :identifier :value value} source))))

(defn scan [{tokens :tokens [c & cs :as source] :source :as lex}]
  (cond
    (Character/isWhitespace ^char c) (assoc lex :source cs)
    (ident-initial c) (scan-identifier lex)))

(defn tokenize [source]
  (loop [lex {:tokens (java.util.ArrayList.) :source source}]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (scan lex)))))

(defn measure-tokenizer [n]
  (let [s (clojure.string/join (repeat n "abcde "))]
    (time (tokenize s))
    (* n (count "abcde "))))
