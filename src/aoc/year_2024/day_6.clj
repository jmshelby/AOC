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
       (map vec)))

(defn- initial-board [grid]
  (let [;; Find guard
        guard-pos (first (grid-filter grid #{\v \^ \< \>}))
        ;; Find barrier coords
        barriers  (grid-filter grid (partial = \#))
        ]
    {:height        (count grid)
     :width         (-> grid first count)
     :barriers      (set  barriers)
     :guard-history [guard-pos]
     :guard         {:pos guard-pos
                     :dir (guard-sym->dir (get-in grid guard-pos))}}))

(defn- advance-player-coord
  [{:keys [pos dir]}]
  (let [[top left] pos]
    (case dir
      :up    [(dec top) left]
      :down  [(inc top) left]
      :left  [top (dec left)]
      :right [top (inc left)])))

(def dir->right-turn {:up    :right
                      :down  :left
                      :left  :up
                      :right :down})

(defn- advance-board [state]
  ;; Either move forward, or turn
  (let [;; Attempt to move the guard forward
        next-forward (advance-player-coord (:guard state))]
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
        (assoc :guard-history (conj (:guard-history state) (-> state :guard :pos)))
        )))

(defn- on-board? [h w [top left]]
  (and (< -1 top h)
       (< -1 left w)))

(defn answer-1 [input]
  (let [grid  (->> input
                   parse-input
                   vec)
        state (initial-board grid)]
    (->> (iterate advance-board state)
         (take-while (fn [state]
                       (->> state :guard :pos (on-board? (:height state) (:width state)))))
         last
         :guard-history
         set
         count
         (+ 1) ;; Add 1 to include last step before going off board
         )))

(defn answer-2 [input]

  )

(comment

  (println INPUT)

  (answer-1 INPUT)
  ;; => 4602

  (answer-2 INPUT)

  ;;
  )
