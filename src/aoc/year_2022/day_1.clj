(ns year-2022.day-1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [aoc.client :as aoc]))

(def INPUT (delay (aoc/get-my-input)))

(defn rcompare
  "Just compare in reverse"
  [a b]
  (compare b a))

(defn parse [in]
  (->> (s/split in #"\n\n")
       (map s/split-lines)
       (map #(map parse-long %))))

(defn answer [input]
  (->> (parse input)
       ;; Sum Each of Items
       (map (partial apply +))
       ;; Get Max
       (apply max)))

(defn answer-2 [input]
  (->> (parse input)
       ;; Sum Each of Items
       (map (partial apply +))
       ;; Take the top 3
       (sort rcompare)
       (take 3)
       ;; Sum them
       (apply +)))

(comment

  (parse @INPUT)

  (answer @INPUT)
  ;; => 70509

  (answer-2 @INPUT)
  ;; => 208567

  )
