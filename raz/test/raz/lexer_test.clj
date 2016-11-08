(ns raz.lexer-test
  (:require [clojure.test :refer :all]
            [raz.lexer :refer :all]))

(deftest empty-expansions
  (testing "Empty transitions without cycles"
    (let [nfa {:start 1
                :final #{4 5 6 8}
                :states {1 [[:empty 2] [\a 4]]
                         2 [[:empty 3] [\b 5] [:empty 8]]
                         3 [[:empty 6]]}}]
    (is (= [1 2 3 6 8]
         (immediate-states nfa (:start nfa)))))))
