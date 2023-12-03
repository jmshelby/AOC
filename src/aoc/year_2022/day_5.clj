(ns year-2022.day-5
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))



(defn- parse
  "Part One"
  [in]
  #_(->> (s/split in #"\n")
         (map #(s/split % #","))
         (map (fn [pairs]
                (map #(s/split % #"-")
                     pairs)))
         (map (fn [pairs]
                (map #(map parse-long %)
                     pairs)))))

(defn answer [input]
  )

(defn answer-2 [input]
  #_(as-> input *
      (parse *)
      (map intersect? *)
      (frequencies *)
      (get * true)))

(comment

  ;; (answer @INPUT)

  ;; (answer-2 @INPUT)

  (println INPUT)

  (let [[stacks moves] (s/split INPUT #"\n\n")

        ;; Parse Stacks
        stacks (-> stacks
                   s/split-lines)
        stacks (->> stacks
                    (map-indexed vector)
                    (mapcat (fn [[row# row]]
                              (->> row
                                   (map-indexed vector)
                                   (filter (fn [[col#]] (zero? (mod (dec col#) 4))))
                                   (map second)
                                   (map-indexed vector)
                                   (remove (fn [[_ v]] (= v \space)))
                                   (map #(conj % row#)))))
                    (group-by first)
                    )
        ;; stacks (reduce )
        ;; Parse moves
        moves  nil
        ]
    stacks
    )

    (mod (dec 9) 4)

    (parse INPUT)

    ;;  0123456789111111111122
    ;;            012345678901
    ;;   1   5   9   3   7
    ["[J]             [F] [M]            "
     "[Z] [F]     [G] [Q] [F]            "
     "[G] [P]     [H] [Z] [S] [Q]        "
     "[V] [W] [Z] [P] [D] [G] [P]        "
     "[T] [D] [S] [Z] [N] [W] [B] [N]    "
     "[D] [M] [R] [J] [J] [P] [V] [P] [J]"
     "[B] [R] [C] [T] [C] [V] [C] [B] [P]"
     "[N] [S] [V] [R] [T] [N] [G] [Z] [W]"
     " 1   2   3   4   5   6   7   8   9 "]

    )
