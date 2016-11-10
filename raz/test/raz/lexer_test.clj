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
    (is (= #{1 2 3 6 8}
           (immediate-states-for-single-state (:states nfa) (:start nfa))))))

  (testing "Empty transitions with cycles"
    (let [nfa {:start 1
               :final #{4 5 6 8}
               :states {1 [[:empty 2] [\a 4]]
                        2 [[:empty 3] [\b 5] [:empty 8]]
                        3 [[:empty 6]]
                        6 [[:empty 2]]}}]
      (is (= #{1 2 3 6 8}
             (immediate-states-for-single-state (:states nfa) (:start nfa))))))

  (testing "Empty transitions for multiple states"
    (let [nfa {:start 1
               :final #{2}
               :states {1 [[\a 3] [:empty 2]]
                        3 [[:empty 4] [:empty 5]]
                        4 [[\b 6] [\c 7]]}}]
      (is (= #{1 2 3 4 5}
             (immediate-states-for-multiple-states (:states nfa) #{1 3})))
      (is (= #{1 2 4}
             (immediate-states-for-multiple-states (:states nfa) #{1 4}))))))

(deftest nfa->dfa-test
  (testing "nfa->dfa without loops"
    (let [nfa {:start 1
           :final #{7}
           :states {1 [[:empty 2] [\a 7]]
                    2 [[\a 3] [\b 4]]
                    4 [[\a 5] [\b 6]]
                    7 [[\b 8] [\c 11]]
                    8 [[:empty 9] [:empty 12]]
                    9 [[\a 10]]
                    11 [[:empty 12]]}}]
      (is (= {:start #{1 2}
              :final #{#{7 3}}
              :states
              {#{1 2} {\a #{7 3} \b #{4}}
               #{7 3} {\b #{12 9 8} \c #{12 11}}
               #{12 9 8} {\a #{10}}
               #{10} {}
               #{12 11} {}
               #{4} {\a #{5} \b #{6}}
               #{5} {}
               #{6} {}}}
             (nfa->dfa nfa)))))

  (testing "nfa->dfa with loops"
    (let [nfa {:start 1
               :final #{7}
               :states {1 [[:empty 2] [\a 7]]
                        2 [[\a 3] [\b 4]]
                        3 [[\c 1]]
                        4 [[\a 5] [\b 6]]
                        7 [[\b 8] [\c 11]]
                        8 [[:empty 9] [:empty 12]]
                        9 [[\a 10]]
                        11 [[:empty 12]]}}]
      (is (= {:start #{1 2}
              :final #{#{7 3}}
              :states
              {#{1 2} {\a #{7 3} \b #{4}}
               #{7 3} {\b #{12 9 8} \c #{12 11 2 1}}
               #{12 9 8} {\a #{10}}
               #{10} {}
               #{1 12 2 11} {\a #{7 3} \b #{4}}
               #{4} {\a #{5} \b #{6}}
               #{5} {}
               #{6} {}}}
             (nfa->dfa nfa))))))
