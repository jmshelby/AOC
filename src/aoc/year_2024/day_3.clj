(ns aoc.year-2024.day-3
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(defn answer-1 [input]
  (->> input
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (fn [[_ a b]]
              (* (parse-long a)
                 (parse-long b))))
       (apply +)))

(defn answer-2 [input]
  (let [insts
        (->> input
             (re-seq #"don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\)")
             (map (fn [[f a b]]
                    {:fn   (cond (s/starts-with? f "mul")   :mult
                                 (s/starts-with? f "don't") :dont
                                 (s/starts-with? f "do")    :do)
                     :args (when a (map parse-long [a b]))
                     })))]
    (loop [total 0
           flag  true
           insts insts]
      (if (seq insts)
        (let [inst (first insts)]
          ;; Next
          (cond
            ;; Allowed to multiply
            (and flag
                 (= :mult (:fn inst)))
            ;; Mult
            (recur (+ total (apply * (:args inst))) flag (rest insts))

            ;; Not allowed to multiply
            (and (not flag)
                 (= :mult (:fn inst)))
            ;; Don't Mult
            (recur total flag (rest insts))

            ;; Change flag - off
            (= :dont (:fn inst))
            (recur total false (rest insts))

            ;; Change flag - on
            (= :do (:fn inst))
            (recur total true (rest insts))))
        ;; Term
        total))))

(comment


  (answer-1 INPUT)
  ;; => 167650499

  (answer-2 INPUT)
  ;; => 95846796

  ;;
  )
