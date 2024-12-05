(ns aoc.year-2024.day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def KEY [\X \M \A \S])

(defn- vec-search [search subject]
  (let [search-c (count search)]
    (loop [ixs []
           idx 0
           in  subject]
      (let [seg-comp (take search-c in)]
        ;; TODO - this can be optimized if there isn't enough items left
        (if (seq seg-comp)
          ;; Compare segment w/search key
          (if (= seg-comp search)
            ;; We've found a match, append idx,
            ;; keep checking very next idx
            ;; (this might not be preferred in all cases, and we _could_ skip ahead by search-c)
            (recur (conj ixs idx) (inc idx) (rest in))
            ;; No match, keep checking very next idx
            (recur ixs (inc idx) (rest in)))
          ;; No more left to check, break
          ixs)))))

(defn answer-1 [input]
  )

(defn answer-2 [input]
  )

(comment

  (println INPUT)

  (def sample "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

  (->> sample
       s/split-lines
       (map (partial apply vector))
       )


  (->> INPUT
       s/split-lines
       ;; first
       count
       )

  (take 5 [  ])


  (let [in     "somestuff jke some other stuff jake again other things"
        in     (apply vector in)
        search (apply vector "jake")
        ]
    (vec-search search in)
    )
;; => [10 32]



  (answer-1 INPUT)

  (answer-2 INPUT)

  ;;
  )
