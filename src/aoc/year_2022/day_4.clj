(ns year-2022.day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(defonce INPUT (delay (aoc/get-my-input)))

(defn- is-A-contained-in-B? [[A1 A2] [B1 B2]]
  (and (<= A1 B1)
       (<= B2 A2)))

(defn- intersect? [[[A1 A2] [B1 B2]]]
  (let [A (set (range A1 (inc A2)))
        B (set (range B1 (inc B2)))]
    (boolean (seq (set/intersection A B)))))

(defn- encompassing-intersect?
  [[A B]]
  (or (is-A-contained-in-B? A B)
      (is-A-contained-in-B? B A)))

(defn- parse
  "Part One"
  [in]
  (->> (s/split in #"\n")
       (map #(s/split % #","))
       (map (fn [pairs]
              (map #(s/split % #"-")
                   pairs)))
       (map (fn [pairs]
              (map #(map parse-long %)
                   pairs)))))

(defn answer [input]
  (as-> input *
    (parse *)
    (map encompassing-intersect? *)
    (frequencies *)
    (get * true)))

(defn answer-2 [input]
  (as-> input *
    (parse *)
    (map intersect? *)
    (frequencies *)
    (get * true)))

(comment

  (answer @INPUT)
  ;; => 571

  (answer-2 @INPUT)
  ;; => 917

  (println @INPUT)

  (parse @INPUT)

  (encompassing-intersect? [[2 4] [6 8]])

  (encompassing-intersect? [[5 7] [7 9]])

  (encompassing-intersect? [[2 8] [3 7]])

  (encompassing-intersect? [[6 6] [4 6]])

  (encompassing-intersect? [[2 6] [4 8]])

  (intersect? [[2 6] [4 8]])

  (intersect? [[2 4] [6 8]])

  )
