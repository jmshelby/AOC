(ns year-2023.day-3
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

;; An `updated` that allows for missing keys
(defn- update-if [m k f default]
  (if (contains? m k)
    (assoc m k (f (get m k)))
    (assoc m k (f default))))

(defn parse [input]
  (loop [source input
         coord  [0 0]
         syms   {}
         parts  {}
         eating nil]
    (let [char (first source)]
      (println "char: " char)
      (cond

        ;; Check for end of source
        (not char)
        {:syms syms :parts parts}

        ;; Check for end of the line
        (= char \newline)
        (recur (rest source)
               ;; Next coord, down 1, at the start
               [(inc (first coord)) 0]
               syms parts nil)

        ;; Check for a symbol - add to syms map
        (re-find #"[^\d.]" (str char))
        (recur (rest source)
               [(first coord) (inc (second coord))]
               (assoc syms coord char) parts nil)

        ;; Check for a number
        (re-find #"\d" (str char))
        (recur (rest source)
               [(first coord) (inc (second coord))]
               syms
               (update-if parts
                          (or eating coord) ;; If
                          #(conj % char) ;; Add to part number
                          []) ;; Default, if first number
               (or eating coord))

        ;; Probably just a period
        :else
        (recur (rest source)
               [(first coord) (inc (second coord))]
               syms parts nil)

        ))))


(defn answer-1 [input]
  (->> input
       ))

(defn answer-2 [input]
  (->> input
       ))

(comment


  (def sample
    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")







  (first "1234")

  (next "1234")

  (rest "1234")

  (first
    (rest "1"))

  (first
    (next "1"))

  (update {} :jake (fn [c] (inc c)) 0)

  (update {} [0 1] (fn [c] (conj c 4)) [])

  (update {[0 1] [4]} [0 1] (fn [c] (conj c 8)) nil)

  (conj nil \1)

  (re-find #"[^\d.]" (str \newline))

        (re-matches #"[^\d.]" ".")


        (->> sample
             s/split-lines
             (map #(map-indexed vector %))
             (map-indexed vector)
             )


        (->> sample
             s/split-lines
             (map-indexed vector)
             (mapcat (fn [[row-i row-raw]]
                       (map (fn [[col-i char]]
                              )
                            (map-indexed vector row-raw))
                       ))
             )



        (println INPUT)

        )
