(ns aoc.year-2024.day-4
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def KEY [\X \M \A \S])

;; Readability aliases for getting coords
(def TOP first)
(def LEFT second)

(defn- inc-diag [cell]
  ;; Just down/left movement
  (->> cell
       (map inc)
       (apply vector)))

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

  (let [sample       [[1 2 3]
                      [4 5 6]
                      [7 8 9]]
        ;; ====
        grid         sample
        ;; Mark some important cells
        top-right    [0 (-> grid first count dec)]
        bottom-left  [(-> grid count dec) 0]
        ;; Fns
        conj-in-last (fn [coll x]
                       (let [last-idx (-> coll count dec)]
                         (update coll last-idx conj x)))
        ]


    (update [[1 2] [3 4] [5 6]] 2 conj 'a)

    (loop [;; Start w/one empty row
           rows [[]]
           ;; Start at top/right most cell
           cell top-right
           ]
      ;; Check if we should continue,
      ;; stop _after_ bottom/left most cell
      (if (= cell (inc-diag bottom-left))
        ;; Done, Return our diaganol rows
        rows

        ;; Determine if we're still in the grid,
        ;; (if cell doesn't exist)
        (if (get-in grid cell)
          ;; Add cell value to latest row,
          ;; continue with next cell diag/down
          (recur (conj-in-last rows (get-in grid cell))
                 (inc-diag cell))
          ;; Start new diag-row
          (recur (conj rows [])
                 ;; Determine next cell

                 (cond
                   ;; Square, start going down,
                   ;; (middle point, where we're currently off the board bottom-right most)
                   (apply = cell)
                   [1 0] ;; happens to be static

                   ;; Continue going down
                   (> (TOP cell) (LEFT cell))
                   [(inc (TOP cell)) 0]

                   ;; Continue going left
                   (> (LEFT cell) (TOP cell))
                   [0 (dec (LEFT cell))]

                   ;; Not sure why we're here
                   :else (throw (Exception. "huh?")))

                 )))))



  (inc-diag [0 0])

  (->> INPUT
       s/split-lines
       first

       )

  (take 5 [  ])




  (answer-1 INPUT)

  (answer-2 INPUT)

  ;;
  )



((\M \S \A \M \x \X \S \M \M \M)
 (\A \S \M \S \M \X \M \A \S \M \J  )
 (\M \M \A \A \M \X \S \X \M \A)
 (\X \M \S \M \S \A \M \A \S \M)
 (\M \M \A \X \M \A \S \A \M \X)
 (\A \M \A \X \X \M \M \A \X \X    )
 (\S \S \X \S \A \S \M \S \M \S \J    )
 (\A \A \A \S \A \M \A \X \A \S)
 (\m \M \M \M \X \M \M \M \A \M)
 (\X \S \A \M \X \A \X \M \X \M  )
 (         \J)

 )
