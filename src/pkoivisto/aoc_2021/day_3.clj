(ns pkoivisto.aoc-2021.day-3
  (:require [clojure.string :as str]))

(def input
  (->> "resources/day-3.txt"
       (slurp)
       (str/split-lines)
       (map str/trim)))

(def input-line-length 12)

(defn char->bit [c]
  (case c
    \0 0
    \1 1))

(defn input-position->bit-counts [input]
  (let [zero-positions (->> (range input-line-length)
                            (map (juxt identity (constantly {0 0
                                                             1 0})))
                            (into {}))
        positions+bits (->> (for [line input]
                              (map-indexed (fn [i c] [i (char->bit c)]) line))
                            (mapcat identity))]
    (reduce (fn [acc [k v]]
              (update-in acc [k v] inc)) zero-positions positions+bits)))

(defn most-common-bits [position->bit-counts]
  (->> (keys position->bit-counts)
       (sort)
       (map position->bit-counts)
       (map (fn [bit-counts]
              (if (<= (bit-counts 0) (bit-counts 1))
                1
                0)))))

(defn least-common-bits [position->bit-counts]
  (->> (keys position->bit-counts)
       (sort)
       (map position->bit-counts)
       (map (fn [bit-counts]
              (if (<= (bit-counts 0) (bit-counts 1))
                0
                1)))))

(defn bits->int [bits]
  (-> (str/join "" bits)
      (Integer/parseInt 2)))

(defn part-one []
  (let [position->bit-counts (input-position->bit-counts input)
        most-common-bits (most-common-bits position->bit-counts)
        gamma-rate (bits->int most-common-bits)
        least-common-bits (least-common-bits position->bit-counts)
        epsilon-rate (bits->int least-common-bits)]
    (* gamma-rate epsilon-rate)))

(defn bit-frequencies-at [^long position input]
  (reduce (fn [acc val]
            (update acc (.charAt val position) inc))
          {\0 0, \1 0}
          input))

(defn oxygen-generator-rating [input]
  (loop [i 0
         candidates input]
    (cond
      (= 1 (count candidates))
      (first candidates)
      :else
      (let [bit-frequencies (bit-frequencies-at i candidates)
            most-common-bit (if (<= (get bit-frequencies \0)
                                    (get bit-frequencies \1))
                              \1
                              \0)]
        (recur
          (inc i)
          (into [] (filter #(= most-common-bit (.charAt % i))) candidates))))))

(defn co2-scrubber-rating [input]
  (loop [i 0
         candidates input]
    (cond
      (= 1 (count candidates))
      (first candidates)
      :else
      (let [bit-frequencies (bit-frequencies-at i candidates)
            least-common-bit (if (<= (get bit-frequencies \0)
                                     (get bit-frequencies \1))
                               \0
                               \1)]
        (recur
          (inc i)
          (into [] (filter #(= least-common-bit (.charAt % i))) candidates))))))

(defn part-two []
  (let [oxygen-generator-rating-num (-> input
                                        (oxygen-generator-rating)
                                        (Integer/parseInt 2))
        co2-scrubber-rating-num (-> input
                                    (co2-scrubber-rating)
                                    (Integer/parseInt 2))]
    (* oxygen-generator-rating-num co2-scrubber-rating-num)))

(comment
  (->> input
       (input-position->bit-counts)
       (most-common-bits)
       (str/join ""))

  (def most-common-str (->> input
                            (input-position->bit-counts)
                            (most-common-bits)
                            (str/join "")))

  (def least-common-str (->> input
                             (input-position->bit-counts)
                             (least-common-bits)
                             (str/join "")))

  (->> input
       (input-position->bit-counts)
       (least-common-bits))

  (oxygen-generator-rating input)
  (co2-scrubber-rating input)

  (part-one)
  (part-two)
  )