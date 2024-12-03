(ns aoc.year-2024.day-2
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(defn- parse-input [in]
  (->> in
       s/split-lines
       (map #(s/split % #"\W"))
       (map #(map parse-long %))))

(defn answer-1 [input]
  )

(defn answer-2 [input]
  )

(comment

  (println INPUT)

  (def sample "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

  (parse-input sample)

  (parse-input INPUT)

  (answer-1 INPUT)

  (answer-2 INPUT)

  ;;
  )
