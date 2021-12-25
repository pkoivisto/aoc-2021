(ns pkoivisto.aoc-2021.day-1
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/day-1.txt")
                (str/split-lines)
                (into [] (map #(Integer/parseInt %)))))

(defn increasing-pairs [xs]
  (->> xs
       (partition-all 2 1)
       (take-while #(= 2 (count %)))
       (filter (fn [[x y]] (< x y)))))

(defn part-one []
  (->> input
       (increasing-pairs)
       (count)))

(defn part-two []
  (let [sums (->> (partition-all 3 1 input)
                  (take-while #(= 3 (count %)))
                  (map #(apply + %)))]
    (->> sums
         (increasing-pairs)
         (count))))

(comment
  (part-one)
  (part-two)
  )