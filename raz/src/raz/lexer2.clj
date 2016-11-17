(ns raz.lexer2)

(refer 'clojure.set :only '[union])

(defn char-range-set
  "Generate set containing all characters in the range [from; to]"
  [from to]
  (set (map char (range (int from) (inc (int to))))))

(def ident-initial (union (char-range-set \A \Z) (char-range-set \a \z) #{\_}))
(def ident-subseq (union ident-initial (char-range-set \0 \9)))
(def hexadecimal-digit (union (char-range-set \a \f) 
                      (char-range-set \A \F)
                      (char-range-set \0 \9)))
(def octal-digit (char-range-set \0 \7))
(def nonzero-decimal (char-range-set \1 \9))
(def integer-suffix #{\u \U \l \L})
(def exponent-suffix #{\e \E})
(def floating-suffix #{\f \F \l \L})
(def exponent-sign #{\+ \-})

(defn update-lex [lex type value source]
  (assoc (update lex :tokens conj 
                 {:type type :value value}) 
         :source source))

(defn scan-identifier [lex]
  (let [source-initial (:source lex)]
    (assert (ident-initial (first source-initial)))
    (loop [[c & cs :as source] (rest source-initial)
           value [(first source-initial)]]
    (if (ident-subseq c)
      (recur cs (conj value c))
      (update-lex lex :identifier value source)))))

(defn scan-integer-suffix [lex source value]
  (let [[c1 c2 & cs] source]
    (if (or (and (or (= c1 \u) (= c1 \U))
                 (or (= c2 \l) (= c2 \L)))
            (and (or (= c1 \l) (= c1 \L))
                 (or (= c2 \u) (= c2 \U))))
      (update-lex lex :number (into value [c1 c2]) cs)
      (update-lex lex :number value source))))

(defn scan-hexadecimal [lex]
  (let [source-initial (:source lex)]
    (loop [[c & cs :as source] (drop 2 source-initial)
           value (vec (take 2 source-initial))]
      (if (hexadecimal-digit c)
        (recur cs (conj value c))
        (scan-integer-suffix lex source value)))))    

(defn scan-octal [lex] 
  (let [source-initial (:source lex)]
    (loop [[c & cs :as source] (rest source-initial)
           value [(first source-initial)]]
      (if (octal-digit c)
        (recur cs (conj value c))
        (scan-integer-suffix lex source value)))))

(defn scan-exponent [lex value source-initial]
  (let [number-to-drop (if (exponent-sign (second source-initial)) 2 1)]
    (loop [[c & cs :as source] (drop number-to-drop source-initial)
           value (into value (vec (take number-to-drop source-initial)))]
      (cond
        (empty? source) (update-lex lex :number value source)
        (Character/isDigit ^char c) (recur cs (conj value c))
        (floating-suffix c) (update-lex lex :number (conj value c) cs)
        :else (update-lex lex :number value source)))))

(defn scan-floating [lex value source-initial] 
  (loop [[c & cs :as source] (rest source-initial)
         value (conj value (first source-initial))]
         (cond 
           (empty? source) (update-lex lex :number value source)
           (Character/isDigit ^char c) (recur cs (conj value c))
           (floating-suffix c) (update-lex lex :number (conj value c) cs)
           (exponent-suffix c) (scan-exponent lex value source)
           :else (update-lex lex :number value source))))

(defn scan-decimal [lex] 
  (let [source-initial (:source lex)]
    (loop [[c & cs :as source] (rest source-initial)
           value [(first source-initial)]]
      (cond 
        (empty? source) (update-lex lex :number value source)
        (Character/isDigit ^char c) (recur cs (conj value c))
        (integer-suffix c) (scan-integer-suffix lex source value)
        (= c \.) (scan-floating lex value source)
        (floating-suffix c) (update-lex lex :number (conj value c) cs)
        (exponent-suffix c) (scan-exponent lex value source)
        :else (update-lex lex :number value source)))))

(defn scan-number [lex]
  (let [source (:source lex)
        [c1 c2 & _] source]
    (assert (Character/isDigit ^char c1))
    (cond 
      (= c1 \0) (cond 
                  (or (= c2 \x) (= c2 \X)) (scan-hexadecimal lex)
                  (octal-digit c1) (scan-octal lex)
                  (= c2 \.) (scan-floating lex)
                  :else (scan-integer-suffix lex (:source lex) [\0]))
      (or (nonzero-decimal c1) (= c1 \.)) (scan-decimal lex)
      :else (throw (Exception. "Wrong number token")))))

(defn scan [lex]
  (let [[c & cs] (:source lex)]        
    (cond
      (Character/isWhitespace ^char c) (assoc lex :source cs)
      (ident-initial c) (scan-identifier lex)
      (Character/isDigit ^char c) (scan-number lex))))

(defn tokenize [source]
  (loop [lex {:tokens [] :source source}]
    (if (empty? (:source lex))
      (:tokens lex)
      (recur (scan lex)))))

(use 'criterium.core)


(defn measure-tokenizer [n]
  (let [s (clojure.string/join (repeat n "abcde "))]
    (bench (tokenize s))
    (* n (count "abcde "))))

'(measure-tokenizer 1000)
