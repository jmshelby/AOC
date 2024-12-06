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

(defn- reverse-grid [grid]
  ;; Simple reverse of each row,
  ;; returned as a vector
  (mapv #(-> % reverse vec) grid))

;; Given a rectangular grid, return new grid
;; rotated 90 degrees *counter clockwise*
(defn- rotate-grid [grid]
  (let [;; Initial cols, empty rows of cols
        init (repeat (count (first grid)) [])]
    (reduce #(mapv conj %1 (reverse %2)) init grid)))

;; Given a rectangular grid, return diaganols as rows,
;; rotating grid 45 degress *counter clockwise*
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
      (if (or (< 100000 (count rows)) ;; failsafe
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

(defn- *answer-1 [input]
  (let [;; Parse outer example
        grid (->> input
                  s/split-lines
                  (map (partial apply vector))
                  (apply vector))
        ;; Prep our translations
        prep {:main         grid
              :translations {:t1-right      grid
                             :t1R-left      (reverse-grid grid)
                             :t2-down       (rotate-grid grid)
                             :t2R-up        (-> grid rotate-grid reverse-grid)
                             :t3-down-right (-> grid diag-rows)
                             :t3R-up-left   (-> grid diag-rows reverse-grid)
                             :t4-down-left  (-> grid reverse-grid diag-rows)
                             :t4R-up-right  (-> grid reverse-grid diag-rows reverse-grid)}}]

    ;; Search each, and attach positions
    (assoc prep :search (->> prep :translations
                             (map (fn [[t rows]]
                                    ;; For each translation, run the search
                                    [t (map (partial vec-search KEY) rows)]))
                             (into {})))))

(defn answer-1 [input]
  (let [analysis (*answer-1 input)]
    ;; We'll just return the total count of found sub-sequences
    (->> analysis
         :search
         vals
         flatten
         count)))

;; ============================

;; Return the pair of coord sequences of a cell's X neighbors,
;; down-left first, then down-right. Includes cell in each
(defn- x-bors-coords [[top left :as cell]]
  [;; Down-left
   [[(dec top) (dec left)]
    cell
    [(inc top) (inc left)]]
   ;; Down-right
   [[(dec top) (inc left)]
    cell
    [(inc top) (dec left)]]])


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

  (->> sample
       s/split-lines
       (map (partial apply vector))
       (apply vector)
       diag-rows
       )

  (->> sample
       ;; *answer-1
       answer-1
       )

  (def sample-2 ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........")



  (let [grid (->> sample-2
                  s/split-lines
                  (map (partial apply vector))
                  vec)]
    (for [top  (range (count grid))
          left (range (count grid))
          ]
      [[top left]
       (when (= \A (get-in grid [top left]))
         (x-bors-coords [top left])
         )]
      ))

    (answer-1 INPUT)
    ;; => 2551

    (answer-2 INPUT)

    ;;
    )
