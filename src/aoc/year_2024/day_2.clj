(ns aoc.year-2024.day-2
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def STRAY-MARGIN 3)

(defn- parse-input [in]
  (->> in
       s/split-lines
       (map #(s/split % #"\W"))
       (map #(map parse-long %))))

(defn- seq-stray?
  "Given seq of numbers, determine if element to
  element, any abs value difference is greater than
  the given margin."
  [margin coll]
  (->> coll
       (partition 2 1)
       (map (partial apply -))
       (map abs)
       (some (partial < margin))))

(defn- safe-report? [report]
  (boolean
    (and
      ;; Make sure we're monotonically inc/dec,
      ;; not counting "remaining constant"
      (or (apply > report)
          (apply < report))
      ;; Make sure stray inc/dec is within margin
      (not (seq-stray? STRAY-MARGIN report)))))

(defn answer-1 [input]
  (->> input
       parse-input
       (filter safe-report?)
       count))

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

  (parse-input INPUT)

  (answer-1 INPUT)
  ;; => 369

  (answer-2 INPUT)

  ;;
  )
