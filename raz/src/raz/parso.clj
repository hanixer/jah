(ns raz.parso)
(import Lexer)

  

(defn tokens [source]
  (let [lex (Lexer. source)]
    (.getTokens lex)))
