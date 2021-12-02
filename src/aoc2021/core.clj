(ns aoc2021.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-lines [file]
  (let [file (io/resource file)]
    (str/split-lines (slurp file))))

(defn read-numbers [file]
  (map read-string (read-lines file)))

;; sonar sweep
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

;; dive!

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
