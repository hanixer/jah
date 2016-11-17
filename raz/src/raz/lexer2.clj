(ns raz.lexer2)
(refer 'clojure.set :only '[union])

(defn char-range-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def ident-initial (union (char-range-set \A \Z) (char-range-set \a \z) #{\_}))

(def ident-subseq (union ident-initial (char-range-set \0 \9)))

(defrecord Token [type value])
(defrecord Lex [tokens source])

(defn update-lex [^Lex lex ^Token token source]
  (assoc (update lex :tokens conj token) :source source))

(defn scan-identifier [^Lex lex]
  (let [[x & xs] (:source lex)]
    (loop [[c & cs :as source] xs
           value               [x]]
      (if (ident-subseq c)
        (recur cs (conj value c))
        (update-lex lex (Token. :identifier value) source)))))

(defn scan [^Lex lex]
  (let [[c & cs] (:source lex)
        tokens   (:tokens lex)]
    (cond
      (Character/isWhitespace ^char c) (assoc lex :source cs)
      (ident-initial c)                (scan-identifier lex))))

(defn tokenize [source]
  (loop [lex (Lex. [] source)]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (scan lex)))))

(use 'criterium.core)

(defn measure-tokenizer [n]
  (let [s (clojure.string/join (repeat n "abcde "))]
    (bench (tokenize s))
    (* n (count "abcde "))))


