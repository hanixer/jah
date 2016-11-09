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
           (immediate-states nfa (:start nfa))))))

  (testing "Empty transitions with cycles"
    (let [nfa {:start 1
               :final #{4 5 6 8}
               :states {1 [[:empty 2] [\a 4]]
                        2 [[:empty 3] [\b 5] [:empty 8]]
                        3 [[:empty 6]]
                        6 [[:empty 2]]}}]
      (is (= [1 2 3 6 8]
             (immediate-states nfa (:start nfa))))))

  (testing "Empty transitions for multiple states"
    (let [nfa {:start 1
               :final #{2}
               :states {1 [[\a 3] [:empty 2]]
                        3 [[:empty 4] [:empty 5]]
                        4 [[\b 6] [\c 7]]}}]
      (is (= [1 2 3 4 5]
             (immediate-states-for-multiple-states nfa #{1 3})))
      (is (= [1 2 4]
             (immediate-states-for-multiple-states nfa #{1 4}))))))
