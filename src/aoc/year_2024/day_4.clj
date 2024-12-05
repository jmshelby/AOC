(ns aoc.year-2024.day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
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

(defn- rotate-grid [grid]
  (let [;; Initial cols, empty rows of cols
        init (repeat (count (first grid)) [])]
    (reduce #(map conj %1 %2) init grid)))

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

  ;; pprint/pprint


  ;; Main view
  (->> sample
       s/split-lines
       (map (partial apply vector))
       (map (partial vec-search KEY ))
       )
;; => ([5] [] [] [] [0] [] [] [] [] [5])

  ;; Reversed view
  (->> sample
       s/split-lines
       (map (partial apply vector))
       (map reverse)
       (map (partial vec-search KEY ))
       )
  ;; => ([] [5] [] [] [3] [] [] [] [] [])

  ;; Column view
  (->> sample
       s/split-lines
       (map (partial apply vector))
       rotate-grid
       (map (partial vec-search KEY ))
       )
  ;; => ([] [] [] [] [] [] [] [] [] [3])

  ;; Column, reversed view
  (->> sample
       s/split-lines
       (map (partial apply vector))
       rotate-grid
       (map reverse)
       (map (partial vec-search KEY ))
       )
  ;; => ([] [] [] [] [] [] [5] [] [] [0])


  (->> sample
       s/split-lines
       (map (partial apply vector))
       )


  (->> INPUT
       s/split-lines
       first

       )

  (take 5 [  ])




  (answer-1 INPUT)

  (answer-2 INPUT)

  ;;
  )
