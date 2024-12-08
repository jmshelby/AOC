(ns aoc.year-2024.day-6
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def guard-sym->dir {\v :down
                     \^ :up
                     \< :left
                     \> :right})

(def dir->right-turn {:up    :right
                      :down  :left
                      :left  :up
                      :right :down})

(defn- grid-filter [grid pred]
  ;; Get set of coords that have:
  ;; (= true (pred (coord-value)))
  (for [top   (range (-> grid count))
        left  (range (-> grid first count))
        :when (pred (get-in grid [top left]))]
    [top left]))

(defn- parse-input [in]
  (->> in
       s/split-lines
       (map vec)
       vec))

(defn- initial-board [grid]
  (let [;; Find guard coords/direction
        guard-pos (first (grid-filter grid #{\v \^ \< \>}))
        guard     {:pos guard-pos
                   :dir (guard-sym->dir (get-in grid guard-pos))}
        ;; Find barrier coords
        barriers  (grid-filter grid (partial = \#))]
    {:height            (count grid)
     :width             (-> grid first count)
     :barriers          (set  barriers)
     :guard             guard
     ;; Initialize empty historys
     :guard-history     []
     :guard-pos-history []}))

(defn- *advance-player-coord
  [{:keys [pos dir]}]
  (let [[top left] pos]
    (case dir
      :up    [(dec top) left]
      :down  [(inc top) left]
      :left  [top (dec left)]
      :right [top (inc left)])))

(def advance-player-coord (memoize *advance-player-coord))

(defn- advance-board [state]
  ;; Either move forward, or turn...
  ;; First, attempt to move the guard forward
  (let [next-forward (advance-player-coord (:guard state))]
    (-> state
        (assoc :guard
               ;; If there's a barrier in the way,
               ;; turn right, *don't* advance
               (if ((:barriers state) next-forward)
                 ;; Replace new direction
                 (assoc (:guard state)
                        :dir (dir->right-turn (-> state :guard :dir)))
                 ;; No barrier
                 ;; go forward, stay same direction
                 (assoc (:guard state)
                        :pos next-forward)))
        ;; Preserve current guard orientation
        (update :guard-history conj (:guard state))
        ;; Preserve current coord/pos of guard
        (update :guard-pos-history conj (-> state :guard :pos)))))

(defn- *on-board? [h w [top left]]
  (and (< -1 top h)
       (< -1 left w)))

(def on-board? (memoize *on-board?))

(defn answer-1 [input]
  (->> input
       parse-input
       initial-board
       (iterate advance-board)
       (take-while #(->> % :guard :pos
                         (on-board? (:height %) (:width %))))
       last
       :guard-pos-history
       set
       count
       (+ 1) ;; Add 1 to include last step before going off board
       ))

(defn answer-2 [input]

  )

(comment

  (println INPUT)

  (def sample "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

  (println sample)

  (answer-1 sample)

  (let [input     INPUT
        grid      (parse-input input)
        init      (initial-board  grid)
        ;; Get all open coords to attempt barriers
        open      (vec (grid-filter grid (partial = \.)))
        ;; Fn to tell us if the guard is bound to loop endlessly
        is-stuck? (fn [state]
                    ;; Is the gaurd in a previous orientation?
                    ((-> state :guard-history set)
                     (:guard state)))]
    (->> open
         ;; Map to each all "end states"
         (pmap (fn [potential]
                 (println "Trying potential " potential " (of " (count open)  " )")
                 (->> potential
                      ;; Add different barrier
                      (update init :barriers conj)
                      ;; Playout guard duty..
                      (iterate advance-board)
                      ;; ..until "end state"
                      (take-while (fn [state]
                                    ;; Failsafe (commented out for speed)
                                    ;; (when (< 100000 (count (:guard-history state)))
                                    ;;   (println "Possible endless loop on board state:")
                                    ;;   (pprint/pprint state)
                                    ;;   (throw (Exception. "Endless loop fail safe")))
                                    (and (not (is-stuck? state))
                                         (->> state :guard :pos
                                              (on-board? (:height state) (:width state))))))
                      last
                      ;; Advance board one more, to get to that "end state" (take-while is *before* that)
                      advance-board
                      ;; Include this potential barrier
                      (vector potential))))
         ;; Filter to *good* blocking new barriers
         (filter (fn [[_ state]] (is-stuck? state)))
         count
         ((fn [t]
            (println "Found a count! : " t)
            t))
         )
    )
;; 1703

  (time
    (answer-1 INPUT))
  ;; => 4602

  (answer-2 INPUT)

    ;;
    )
