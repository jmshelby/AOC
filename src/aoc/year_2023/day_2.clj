(ns aoc.year-2023.day-2
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def bag-counts {:red   12
                 :green 13
                 :blue  14})

(defn- parse-games [lines]
  (->> lines
       (re-seq #"Game (\d+): (.*)")
       (map (fn [parts]
              (let [[_ id raw] parts]
                {:id    (parse-long id)
                 :maxes (->> (s/split raw #";")
                             (map  #(re-seq #"(\d+) (\w+)(?:,|$)" %))
                             (map (fn [blocks]
                                    (->> blocks
                                         (map (fn [[_ num color]]
                                                [(keyword color)
                                                 (parse-long num)]))
                                         (into {}))))
                             (apply merge-with max))})))))

(defn- bag> [bagA bagB]
  (some (fn [[_ [A B]]]
          (> A B))
        (merge-with vector bagA bagB)))

(defn answer-1 [input]
  (->> input
       parse-games
       (remove (fn [{hand :maxes}]
                 (bag> hand bag-counts )))
       (map :id)
       (apply +)))

(defn answer-2 [input]
  (->> input
       parse-games
       (map #(assoc % :power (->> % :maxes vals (apply *))))
       (map :power)
       (apply +)))

(comment

  (def sample "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")


  ;; => ({:id 1, :maxes {:blue 6, :red 4, :green 2}}
  ;;     {:id 2, :maxes {:blue 4, :green 3, :red 1}}
  ;;     {:id 3, :maxes {:green 13, :blue 6, :red 20}}
  ;;     {:id 4, :maxes {:green 3, :red 14, :blue 15}}
  ;;     {:id 5, :maxes {:red 6, :blue 2, :green 3}})




  (re-seq #"Game (\d+): (?:(\d+) (\w+)(?:\W|$))+"  sample)

  (re-seq #"Game (\d+): ((((\d+) .+),?)+;?)+"  sample)

  (re-seq #"Game (\\d+): ((\\d+) (\\W+),?);?" sample)

  (re-seq #"\d+," "1234, 4567,")

  (println INPUT)

  (answer-1 INPUT)
  ;; => 2256


  (answer-2 INPUT)
  ;; => 74229



  ;;
  )
