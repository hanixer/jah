(ns sandbox)
(require '[instaparse.core :as insta])

(def ambi
  (insta/parser 
   "S = E
E = 'a'"))

(def arithmetic
  (insta/parser
   "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))
