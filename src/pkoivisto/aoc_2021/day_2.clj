(ns pkoivisto.aoc-2021.day-2
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/day-2.txt")
                (str/split-lines)
                (map (fn [line]
                       (let [[direction amount] (str/split line #" ")]
                         [(keyword direction) (Integer/parseInt amount)])))))

(def origin {:x 0 :y 0})

(defn forward [previous-position amount]
  (update previous-position :x + amount))

(defn down [previous-position amount]
  (update previous-position :y + amount))

(defn up [previous-position amount]
  (update previous-position :y - amount))

(defn apply-command [position [direction amount]]
  (case direction
    :down
    (down position amount)
    :up
    (up position amount)
    :forward
    (forward position amount)))

(defn part-one []
  (let [{x :x
         y :y} (reduce apply-command origin input)]
    (* x y)))

(def origin+aim {:x 0, :y 0, :aim 0})

(defn down-2 [position+aim amount]
  (update position+aim :aim + amount))

(defn up-2 [position+aim amount]
  (update position+aim :aim - amount))

(defn forward-2 [position+aim amount]
  (-> position+aim
      (update :x + amount)
      (update :y + (* (:aim position+aim)
                      amount))))

(defn apply-command-2 [position [direction amount]]
  (case direction
    :down
    (down-2 position amount)
    :up
    (up-2 position amount)
    :forward
    (forward-2 position amount)))

(defn part-two []
  (let [{x :x
         y :y} (reduce apply-command-2 origin+aim input)]
    (* x y)))

(comment
  (let [test-input [[:forward 5]
                    [:down 5]
                    [:forward 8]
                    [:up 3]
                    [:down 8]
                    [:forward 2]]
        {x :x
         y :y}
        (reduce apply-command-2 origin+aim test-input)]
    (= 900 (* x y))))


(comment
  (part-one)

  (part-two)
  )