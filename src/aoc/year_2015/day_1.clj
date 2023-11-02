(ns year-2015.day-1
  (:require [clojure.string :as s]
            [aoc.client :as aoc]))

(def INPUT (delay (aoc/get-my-input)))

(def dir->op
  {\( inc
   \) dec})

(defn take-while-good [directions good?]
  (loop [dir-count 0
         floor 0
         dirs directions]
    (if (or (good? floor dirs)
            (empty? dirs))
      [dir-count floor]
      (let [next-floor ((dir->op (first dirs))
                        floor)]
        (recur (inc dir-count)
               next-floor
               (rest dirs))))))

(defn answer [input]
  (second
    (take-while-good input
                     (constantly false))))

(defn answer-2 [input]
  (first
    (take-while-good input
                     (fn [f _]
                       (= -1 f)))))

(comment

  (answer @INPUT)
  ;; => 138

  (answer-2 @INPUT)
  ;; => 1771

  )
