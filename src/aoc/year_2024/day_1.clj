(ns aoc.year-2024.day-1
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))


(defn- unzip [zipped]
  (reduce (fn [acc [a-next b-next]]
            (-> acc
                (update :a-list conj a-next)
                (update :b-list conj b-next)))
          {:a-list [] :b-list []}
          zipped))

(defn- parse-input [in]


  )

(defn answer-1 [input]
  )

(defn answer-2 [input]
  )

(comment


  (println INPUT)

  (->> INPUT
       (re-seq #"(\d+)\W+(\d+)")
       (map (fn [[_ a b]]
              [a b]))
       (map #(map parse-long %))
       unzip
       ()
       )


  ;;
  )
