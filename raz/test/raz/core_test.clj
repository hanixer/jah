(ns raz.core-test
  (:require [clojure.test :refer :all]
            [raz.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest tokens-test
  (testing "token number"
    (let [{ type :type} (get-token "12345")]
      (is (= (:type (get-token "12345")) :number)))))

;(def char->smachine #'char->smachine)

(deftest machines-test
  (testing "machine from char"
    (binding [*state-counter* 1]
      (let [sm (char->smachine \a)]
        (println (sm 1))
        (is (= (sm 1)
               [{:chars [\a] :to :final}]))))))
