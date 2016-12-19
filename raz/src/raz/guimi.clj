(ns clj-xml.core
  (:require [clojure.xml :as xml]))

(def bookshelf
  (xml/element :books {}
               (xml/element :book {:author "Stuart Halloway"}
                            "Programming Clojure")
               (xml/element :book {:author "Christian Queinnec"}
                            "Lisp in Small Pieces")
               (xml/element :book {:author "Harold Abelson, Gerald Jay Sussman"}
                            "Structure and Interpretation of Computer Programs")))

q
