(ns raz.parso)
(import Lexer)

(defn tokens [source]
  (let [lex (Lexer. source)]
    (.getTokens lex)))

(defn success [v]
  (fn [inp] [[v, inp]]))

(defn failure [inp]
  [])

(defn item [inp]
  (if (empty? inp)
    []
    [[(first inp) (rest inp)]]))

(defn parse [p inp]

  (p inp))

(defn >>= [p f]
  (fn [inp]
    (let [result (parse p inp)]
      (if (empty? result)
        []
        (parse (f (ffirst result)) (second (first result)))))))

(defn +++ [p q]
  (fn [inp]
    (let [result (parse p inp)]
      (if (empty? result)
        (parse q inp)
        result))))

(defn sat [pred]
  (>>= item 
       (fn [v] (if (pred v)
                 (success v)
                 failure))))

(def digit (sat (fn [c] (Character/isDigit ^char c))))
(def lower (sat (fn [c] (Character/isLowerCase ^char c))))
(def upper (sat (fn [c] (Character/isUpperCase ^char c))))
(def letter (sat (fn [c] (Character/isAlphabetic ^char c))))
(def alphanum (sat (fn [c] (Character/isLetterOrDigit ^char c))))
(defn chara [c] (sat (fn [c2] (= c c2))))

(defn string-parse [s]
  (if (empty? s)
    (success [])
    (let [[c & cs] s]
      (>>= (chara c)
           (fn [v1] (>>= (string-parse cs)
                         (fn [v2] (success (cons c cs)))))))))

(defn many [p]
  (+++ (>>= p 
            (fn [v] (>>= (many p)
                         (fn [vs] (success (conj vs v))))))
       (success ())))

(defn many1 [p]
  (>>= p
       (fn [v] 
         (>>= (many p)
              (fn [vs] (success (conj vs v)))))))

(def ident (>>= lower
                (fn [x] (>>= (many alphanum)
                             (fn [xs] (success (conj xs x)))))))

(def nat (>>= (many1 digit)
              (fn [xs]  (success (-> xs clojure.string/join Integer.)))))

(def space (>>= (many (sat (fn [c] (Character/isSpace c))))
                (fn [v] (success nil))))

(defn token [p]
  (>>= space
       (fn [_] (>>= p
                    (fn [v] (>>= space
                                 (fn [_] (success v))))))))

(def identifier (token ident))
(def natural (token nat))
(defn symb [s]
  (token (string-parse s)))

(declare term)
(declare expr)

(def factor 
  (+++ (>>= (symb "(")
            (fn [_] (>>= expr 
                         (fn [v] (>>= (symb ")")
                                      (fn [_] (success v)))))))
       nat))

(def term 
  (+++ (>>= factor
            (fn [v1] (>>= (symb "*")
                          (fn [_] (>>= term
                                       (fn [v2] (success (* v1 v2))))))))
       (>>= factor
            (fn [v] (success v)))))

(def expr 
  (+++ (>>= term
            (fn [v1] (>>= (symb "+")
                          (fn [_] (>>= expr
                                       (fn [v2] (success (+ v1 v2))))))))
       (>>= term
            (fn [v] (success v)))))

(defn f [x]
  (Math/sqrt x))

(defn g [x]
  (/ (Math/cos x) (+ 22 (Math/sin x))))

(defn f_ [x]
  [(f x) "f was called."])

(defn g_ [x]
  [(g x) "g was called."])

(defn f_g_ [x]
  (let [[t s] (g_ x)]
    (let [[t2 s2] (f_ t)]
      [t2 (str s s2)])))

(defn bind [f]
  (fn [[x s]]
    (let [[fx fs] (f x)]
      [fx (str s fs)])))

(defn <++> [f g]
  (let [f_ (bind f)]
    (fn [x] (f_ (g x)))))

(defn unit [x]
  [x ""])

(defn lift [f]
  (comp unit f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handwritten parser - which is not working...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns raz.parso.hand)

(declare expr)

(defn symb [which inp]
(println "symb "  inp (str "'" which "'"))
  (let [res
        (loop [[c & cs :as s] inp]
          (if (or (empty? s)
                  (not (Character/isWhitespace ^char c)))
            s
            (recur cs)))]
    (if (or (empty? res)
            (not (= which  (first res))))
      nil
      (let [res
            (loop [[c & cs :as s] (next res)]
              (if (or (empty? s)
                      (not (Character/isWhitespace ^char c)))
                s
                (recur cs)))]
        [which res]))))

(defn number [inp]
  (println "number " inp)
  (let [res
        (loop [[c & cs :as s] inp
               acc []]
          (if (or (empty? s)
                  (not (Character/isDigit ^char c)))
            [acc s]
            (recur cs (conj acc c))))]
    (if (empty? (first res))
      nil
      [(Integer. (clojure.string/join (first res))) (second res)])))

(defn ws-skip [f inp]
(println "ws-skip " inp)
  (let [res
        (loop [[c & cs :as s] inp]
          (if (or (empty? s)
                  (not (Character/isWhitespace ^char c)))
            s
            (recur cs)))]
    (if res
      (if-let [[v tail] (f res)]
        (let [res
              (loop [[c & cs :as s] tail]
                (if (or (empty? s)
                        (not (Character/isWhitespace ^char c)))
                  s
                  (recur cs)))]
          [v res])
        nil)
      nil)))

(defn factor [inp]
  (println "factor " inp)
  (if-let [[lparen tail] (symb \( inp)]
    (if-let [[v tail] (ws-skip expr tail)]
      (if-let [[rparen tail] (symb \) inp)]
        [v tail]
        nil)
      nil)
    
    (if-let [[num tail] (ws-skip number inp)]
      [num tail]
      nil)))

(defn term [inp]
  (println "term " inp)
  (if-let [[fctr tail] (ws-skip factor inp)]
    (if-let [[star tail] (symb \* tail)]
      (if-let [[trm tail] (ws-skip term tail)]
        [(* fctr trm) tail]
        nil)
      [fctr tail])
    nil))

(defn expr [inp]
(println "expr " inp)
  (if-let [[trm tail] (ws-skip term inp)]
    (if-let [[plus tail] (symb \+ tail)]
      (if-let [[e tail] (ws-skip expr tail)]
        [(+ trm e) tail]
        nil)
      [trm tail])
    nil))

(defn generate-expr [n]
  (let [res
        (loop [s (str "(" n " + " (inc n) " )")
               m 1]
          (if (< m n)
            (recur (str "( " s " + " m " ) ") (inc m))
            (str s " * " (inc (inc n)))))]
    res))
          
    
    
