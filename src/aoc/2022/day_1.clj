(ns day-1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

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

  (def example
    "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000

")



  (parse example)

  (answer example)

  (let [input (slurp (io/resource "inputs/2022/day_1"))]
    (time (answer input)))
  ;; => 70509 (my correct answer)


  (answer-2 example)

  (let [input (slurp (io/resource "inputs/2022/day_1"))]
    (time (answer-2 input)))
;; => 208567

  )

;;
