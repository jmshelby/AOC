(ns aoc.year-2023.day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.math :refer [pow]]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))


(defn answer-1 [in]
  (->> in
       s/split-lines
       (map s/trim)
       (map #(re-find #"Card +\d+: (.+) \| (.+)$" %))
       (map (fn [[full winning scratched]]
              (map (fn [raw]
                     (->> (s/split raw #" +")
                          (keep parse-long)
                          set))
                   [winning scratched])))
       (map (fn [[winning scratched]]
              (let [wins
                    (set/intersection winning scratched)
                    factor (->> wins
                                count
                                dec
                                (max 0))
                    points (if (zero? (count wins))
                             0
                             (pow 2 factor))]
                {:winning   winning
                 :scratched scratched
                 :wins      wins
                 :factor    factor
                 :points    points})))
       (map :points)
       (apply +)))

(comment

  (def sample
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
     Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
     Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
     Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
     Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
     Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")


  (answer-1 sample);; => 13.0


  (answer-1 INPUT);; => 32609.0





  (println INPUT)

  )
