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

;; Given a rectangular grid, return new grid rotated 90 degrees
(defn- rotate-grid [grid]
  (let [;; Initial cols, empty rows of cols
        init (repeat (count (first grid)) [])]
    (reduce #(map conj %1 %2) init grid)))

;; Given a rectangular grid, return diaganols as rows
(defn- diag-rows [grid]
  (let [;; Mark some important cells
        top-right    [0 (-> grid first count dec)]
        bottom-left  [(-> grid count dec) 0]
        ;; Fns
        conj-in-last (fn [coll x]
                       (let [last-idx (-> coll count dec)]
                         (update coll last-idx conj x)))]

    (loop [;; Start w/one empty row
           rows [[]]
           ;; Start at top/right most cell
           cell top-right]

      ;; (println {:rows rows :cell cell})

      ;; Check if we should continue,
      ;; stop _after_ bottom/left most cell
      (if (or (< 100 (count rows)) ;; failsafe
              (= cell (inc-diag bottom-left)))
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

                   ;; Continue going down (magic/special math)
                   (> (TOP cell) (LEFT cell))
                   ;; TOP - LEFT + 1 (not sure why yet...)
                   [(inc (- (TOP cell) (LEFT cell))) 0]

                   ;; Continue going left (magic math)
                   (> (LEFT cell) (TOP cell))
                   ;; LEFT - TOP - 1 (not sure why yet...)
                   [0 (- (LEFT cell) (TOP cell) 1)]

                   ;; Not sure why we're here
                   :else (throw (Exception. "huh?")))))))))

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
       (apply vector)
       diag-rows
       )


  (answer-1 INPUT)

  (answer-2 INPUT)

  ;;
  )
;; 1/10 -> 0/8
;; 2/10 -> 0/7
;; 3/10 -> 0/6
;; 4/10 -> 0/5
;; 5/10 -> 0/4
;; 6/10 -> 0/3
;; 7/10 -> 0/2
;; 8/10 -> 0/1
;; 9/10 -> 0/0

;; 10/9 -> 2/0
;; 10/8 -> 3/0

;; ([\M \M \M \S \X \* \m \A \s \M]
;;  [\- \S \A \M \X \M \S \m \S \A \%   ]
;;  [\A \- \X \S \X \M \A \A \m \M]
;;  [\M \S \- \M \A \S \M \S \M \x]
;;  [\X \M \A \- \A \M \X \A \M \M \*   ]
;;  [\X \X \A \M \- \X \X \A \M \A]
;;  [\S \M \S \M \S \- \S \X \S \S]
;;  [\S \A \X \A \M \A \- \A \A \A]
;;  [\M \A \M \M \M \X \M \- \M \M]
;;  [\M \X \M \X \A \X \M \A \- \X]
;;  [                           \J ]
;;  )
