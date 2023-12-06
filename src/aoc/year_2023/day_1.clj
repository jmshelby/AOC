(ns aoc.year-2023.day-1
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

(def STRINGS {"1" 1 "one" 1
              "2" 2 "two" 2
              "3" 3 "three" 3
              "4" 4 "four" 4
              "5" 5 "five" 5
              "6" 6 "six" 6
              "7" 7 "seven" 7
              "8" 8 "eight" 8
              "9" 9 "nine" 9})

(defn parse [input]
  (->> input
       s/split-lines
       (map #(map str %))))

(defn answer-1 [input]
  (let [digits (->> (range 1 10)
                    (map str)
                    set)]
    (->> input
         parse
         (map #(filter digits %))
         (map (juxt first last))
         (map (partial apply str))
         (map parse-long )
         (apply +))))

(defn answer-2 [input]
  (let [re-string (->> STRINGS
                       keys
                       (s/join "|"))
        re (re-pattern
            (str "(?=(" re-string "))."))
        ]
    (->> input
         s/split-lines
         (map (fn [line]
                (map second (re-seq re line))))
         (map (juxt first last))
         (map #(map STRINGS %))
         (map (partial apply str))
         (map parse-long)
         (apply +))))

(comment

  (def sample "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  (parse sample)

  (def sample2 "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")


  ;; "(?=(" + regex + "))."

  (let [re-string (->> STRINGS
                       keys
                       (s/join "|"))
        re (re-pattern
            (str "(?=(" re-string "))."))
        ]

    (->> "nineight"
         (re-seq re)
         (map second)
         )
    )

  (let [re-string (->> STRINGS
                keys
                (s/join "|"))
        re (re-pattern
            (str "(?=(" re-string "))."))
        ]
    (->> sample2
         s/split-lines
         (map (fn [line]
                (map second (re-seq re line))))
         (map (juxt first last))
         (map #(map STRINGS %))
         (map (partial apply str))
         (map parse-long)
         (apply +)
         )
    )

(println INPUT)

(answer-1 INPUT)
;; => 56042

(answer-2 INPUT)
;; => 55362 - #1 wrong ...
;; => 55358 - #2 maybe?? (yep, correct)




 "" )
