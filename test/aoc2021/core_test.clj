(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

;; Day 2 - Dive!
(def aoc3-test-input [["forward" 5] ["down" 5] ["forward" 8] ["up" 3] ["down" 8] ["forward" 2]])

(deftest aoc3-test
  (testing "destination calculations"
    (is (= {:hor 15, :depth 10} (destination aoc3-test-input))))
  (testing "final result"
    (is (= 150 (aoc3 aoc3-test-input)))))

(deftest aoc4-test
  (testing "destination calculations"
    (is (= {:hor 15, :depth 60, :aim 10} (aoc4-dest aoc3-test-input))))
  (testing "final result"
    (is (= 900 (aoc4 aoc3-test-input)))))
