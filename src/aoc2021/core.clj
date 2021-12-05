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

;; day4 - bingo

(def day4-test-numbers [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1])

(def day4-test-boards
  "22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def day4-real-numbers [14,30,18,8,3,10,77,4,48,67,28,38,63,43,62,12,68,88,54,32,17,21,83,64,97,53,24,2,60,96,86,23,20,93,65,34,45,46,42,49,71,9,61,16,31,1,29,40,59,87,95,41,39,27,6,25,19,58,80,81,50,79,73,15,70,37,92,94,7,55,85,98,5,84,99,26,66,57,82,75,22,89,74,36,11,76,56,33,13,72,35,78,47,91,51,44,69,0,90,52])

(def day4-real-boards (read-boards (slurp (io/resource "input4-boards.txt"))))

(defn read-boards [input]
  (->> (str/split-lines input)
       (remove #(= % ""))
       (map #(str/split % #"\s+"))
       (map #(remove (fn [s] (= s "")) %))
       (map (fn [line] (map read-string line)))
       (partition 5)))

(defn mark-line [num line]
  (map #(if (= num %) :mark %) line))

(defn mark-num [num board]
  (->> board
       (map #(mark-line num %))))

(defn rotate-board [board]
  (apply map vector board))

(defn bingo? [line]
  (every? #(= % :mark) line))

(defn winner? [board]
  (or (some bingo? board)
      (some bingo? (rotate-board board))))

(defn unmarked [board]
  (filter #(not (= :mark %)) (flatten board)))

;; aoc5
(defn play [nums boards]
  (let [num (or (first nums) -1)
        boards (map #(mark-num num %) boards)]
      (cond
        (not nums) nil
        (some winner? boards) (* num (apply + (unmarked (first (filter winner? boards)))))
        :else (play (next nums) boards))))

(defn aoc6 [nums prev boards winners]
  (let [num (or (first nums) -1)
        winners (concat winners (filter winner? boards))
        boards (map #(mark-num num %) (filter (complement winner?) boards))]
      (cond
        (empty? nums) (* prev (apply + (unmarked (last winners))))
        (empty? boards) (* prev (apply + (unmarked (last winners))))
        :else (play6 (next nums) num boards winners))))
