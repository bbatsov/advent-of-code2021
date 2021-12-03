(ns aoc2021.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-lines [file]
  (let [file (io/resource file)]
    (str/split-lines (slurp file))))

(defn read-numbers [file]
  (map read-string (read-lines file)))

;; day1 - sonar sweep
(defn aoc1 [input]
  (->> input
       (partition 2 1)
       (map (fn [[x y]] (> y x)))
       (filter true?)
       count))

(defn aoc2 [input]
  (let [input (->> input
                   (partition 3 1)
                   (map #(apply + %)))]
    (aoc1 input)))

;; day2 - dive!

(def aoc3-input (read-lines "input2.txt"))

(defn nav-commands [input]
  (map (fn [[x y]] [x (Integer/parseInt y)]) (map #(str/split % #" ") input)))

(defn destination [input]
  (reduce (fn [res [cmd val]]
            (case cmd
              "forward" (update res :hor (partial + val))
              "down" (update res :depth (partial + val))
              "up" (update res :depth #(- % val))))
          {:hor 0 :depth 0}
          input))

(defn aoc3 [input]
  (let [dest (destination input)]
    (* (:hor dest) (:depth dest))))

(defn aoc4-dest [input]
  (reduce (fn [res [cmd val]]
            (case cmd
              "forward" (-> res (update :hor (partial + val)) (update :depth #(+ % (* (:aim res) val))))
              "down" (update res :aim (partial + val))
              "up" (update res :aim #(- % val))))
          {:hor 0 :depth 0 :aim 0}
          input))

(defn aoc4 [input]
  (let [dest (aoc4-dest input)]
    (* (:hor dest) (:depth dest))))

;; day3 - binary diagnostic

(def day3-test-input
  [
   "00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"
   ])

(def day3-input (read-lines "input3.txt"))

(defn bin-gamma [input]
  (->> input
       (apply map vector)
       (map frequencies)
       (map #(if (> (get % \0) (get % \1)) \0 \1))))

(defn bin-epsilon [gamma]
  (map #(if (= % \1) \0 \1) gamma))

(defn bin-to-dec [seq]
  (read-string (str "2r" (apply str seq))))

(defn aoc5 [input]
  (let [bgamma (bin-gamma input)
        bepsilon (bin-epsilon bgamma)
        gamma (bin-to-dec bgamma)
        epsilon (bin-to-dec bepsilon)]
    (* gamma epsilon)))

(defn bit-freqs [input]
  (->> input
       (apply map vector)
       (map frequencies)))

(defn o2-filter-1 [ch]
  (= \1 ch))

(defn o2-filter-0 [ch]
  (= \0 ch))

(defn o2-rating [input pos]
  (if (= 1 (count input))
    (first input)
    (let [freq (nth (bit-freqs input) pos)
          ones (get freq \1 0)
          zeroes (get freq \0 0)
          filtered (filter #(if (>= ones zeroes) (= \1 (nth % pos)) (= \0 (nth % pos))) input)]
      (o2-rating filtered (inc pos))
      )))

(defn co2-rating [input pos]
  (if (= 1 (count input))
    (first input)
    (let [freq (nth (bit-freqs input) pos)
          ones (get freq \1 0)
          zeroes (get freq \0 0)
          filtered (filter #(if (< ones zeroes) (= \1 (nth % pos)) (= \0 (nth % pos))) input)]
      (co2-rating filtered (inc pos))
      )))

(defn aoc6 [input]
  (let [o2 (bin-to-dec (o2-rating input 0))
        co2 (bin-to-dec (co2-rating input 0))]
    (* o2 co2)))
