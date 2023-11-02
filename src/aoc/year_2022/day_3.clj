(ns year-2022.day-3
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(defonce INPUT (delay (aoc/get-my-input)))

(defn- range* [a b]
  (map char
       (range (int a) (inc (int b)))))

(defn- start [n]
  (drop n (range)))


(def item->priority
  (zipmap (concat (range* \a \z)
                  (range* \A \Z))
            (start 1)))

(defn- common-item [sets]
  (first
    (apply set/intersection
           (map set sets))))

(defn parse-compartments
  "Part One"
  [in]
  (->> (s/split in #"\n")
       (map #(partition (quot (count %) 2)
                        %))))

(defn parse-groups
  "Part Two"
  [in]
  (->> (s/split in #"\n")
       (map set)
       (partition 3)))

(defn- answer* [parser input]
  (->> input
       parser
       (map common-item)
       (map item->priority)
       (apply +)))

(def answer (partial answer*
                     parse-compartments))

(def answer-2 (partial answer*
                      parse-groups))

(comment

  (parse-compartments @INPUT)

  (parse-groups @INPUT)

  (answer @INPUT)
  ;; => 7967

  (answer-2 @INPUT)
  ;; => 2716

  )
