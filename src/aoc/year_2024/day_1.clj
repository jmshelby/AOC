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
  (->> in
       (re-seq #"(\d+)\W+(\d+)")
       (map (fn [[_ a b]]
              [a b]))
       (map #(map parse-long %))))

(defn answer-1 [input]
  (as-> input *
    (parse-input *)
    (unzip *)
    (update * :a-list sort)
    (update * :b-list sort)
    (interleave (:a-list *) (:b-list *))
    (partition 2 *)
    (map (partial apply -) *)
    (map abs *)
    (apply + *)))

(defn answer-2 [input]
  (let [{:keys [a-list b-list]}
        (->> input
             parse-input
             unzip
             )
        b-freq (frequencies b-list)]
    (->> a-list
         (map #(* % (get b-freq % 0)))
         (apply +)
         )
    )
  )

(comment


  (println INPUT)

  (answer-1 INPUT)
  ;; => 2192892

  (answer-2 INPUT)
  ;; => 22962826



  ;;
  )
