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
  '(print inp)
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

(def digit (sat (fn [c] (Character/isDigit c))))
(def lower (sat (fn [c] (Character/isLowerCase c))))
(def upper (sat (fn [c] (Character/isUpperCase c))))
(def letter (sat (fn [c] (Character/isAlphabetic c))))
(def alphanum (sat (fn [c] (Character/isLetterOrDigit c))))
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

(def factor (+++ (>>= (symb "(")
                      (fn [_] (>>= expr 
                                   (fn [v] (>>= (symb ")")
                                                (fn [_] (success v)))))))
                 nat))

(def term (+++ (>>= factor
                    (fn [v1] (>>= (symb "*")
                                  (fn [_] (>>= term
                                               (fn [v2] (success (* v1 v2))))))))
               (>>= factor
                    (fn [v] (success v)))))

(def expr (+++ (>>= term
                    (fn [v1] (>>= (symb "+")
                                  (fn [_] (>>= expr
                                               (fn [v2] (success (+ v1 v2))))))))
               (>>= term
                    (fn [v] (success v)))))
