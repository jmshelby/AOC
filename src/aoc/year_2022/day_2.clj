(ns year-2022.day-2
  (:require [clojure.string :as s]
            [aoc.client :as aoc]))

(defonce INPUT (delay (aoc/get-my-input)))

(def translation
  "Part One"
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def encoded-outcome
  "Part Two"
  {"X" :lose
   "Y" :draw
   "Z" :win})

(def rules #{[:rock #_beats :scissors]
             [:scissors #_beats :paper]
             [:paper #_beats :rock]})

(def score {:rock 1
            :paper 2
            :scissors 3
            :lose 0
            :draw 3
            :win 6})

(defn- round->outcome [them me]
  (cond
    (= them me) :draw
    (rules [them me]) :lose
    (rules [me them]) :win
    :else (println "pair doesn't result in outcome: " [them me])
    ))

(defn what-should-my-shape-be?
  "Part Two"
  [them outcome]
  (case outcome
    :draw them
    :lose (some (fn [[beats that]]
                          (when (= them beats)
                            that))
                       rules)
    :win (some (fn [[beats that]]
                 (when (= them that)
                   beats))
                      rules)))

(defn round->points
  "Part One"
  [them me]
  (let [me            (translation me)
        them          (translation them)
        shape-point   (get score me)
        outcome       (round->outcome them me)
        outcome-point (get score outcome)]
    (+ shape-point
       outcome-point)))

(defn round->points2
  "Part Two"
  [them me]
  (let [them (translation them)
        me (->> me
                encoded-outcome
                (what-should-my-shape-be? them))
        shape-point (get score me)
        outcome (round->outcome them me)
        outcome-point (get score outcome)]
    (+ shape-point
       outcome-point)))

(defn parse [in]
  (->> in
       s/split-lines
       (map #(s/split % #"\W"))
       ;; (map #(map translation %))
       ))

(defn answer [input]
  (->> input
       parse
       (map (partial apply round->points))
       (apply +)))

(defn answer-2 [input]
  (->> input
       parse
       (map (partial apply round->points2))
       (apply +)))

(comment

  (parse @INPUT)

  (answer @INPUT)
  ;; => 13526

  (answer-2 @INPUT)
  ;; => 14204

  )
