(ns year-2015.day-2
  (:require [clojure.string :as s]
            [aoc.client :as aoc]))

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
               (rest dirs)))
      )))

(comment

  (take-while-good (aoc/get-first-input 2015 1)
                   (constantly false))

  (take-while-good (aoc/get-first-input 2015 1)
                   (fn [f _]
                     (= -1 f)))


  )
