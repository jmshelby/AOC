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
  (let [initial
        (loop [source input
               coord  [0 0]
               syms   {}
               parts  {}
               eating nil]
          (let [char (first source)]
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
                     syms parts nil))))]
    ;; Pre-parse part number values
    (update initial :parts
            (fn [parts]
              (->> parts
                   (map (fn [[coord digits]]
                          [coord
                           {:chars digits
                            :value (->> digits (apply str) parse-long)}]))
                   (into {}))))))

(defn neighbors-of-coord
  [[top left]]
  (->> ;; Each of the neighbors
    [[(dec top) left]
     [(inc top) left]
     [top (inc left)]
     [top (dec left)]
     [(inc top) (inc left)]
     [(dec top) (dec left)]
     [(inc top) (dec left)]
     [(dec top) (inc left)]]))

(defn- neighbors-of-partno
  [[top left] chars]
  (->> (range (count chars))
       (map #(vector top (+ % left)))
       (mapcat neighbors-of-coord)
       set))

(defn- add-neighbors [schem]
  (update schem :parts
          (fn [parts]
            (->> parts
                 (map (fn [[coord part]]
                        [coord
                         (assoc part :neighbors (neighbors-of-partno coord (:chars part)))]))
                 (into {})))))

(defn- add-adjacent-symbols [syms schem]
  (update schem :parts
          (fn [parts]
            (->> parts
                 (map (fn [[coord part]]
                        [coord
                         (assoc part :adjacent-symbols
                                (set/intersection
                                  (-> syms keys set)
                                  (:neighbors part)))]))
                 (into {})))))

(defn answer-1 [input]
  (let [parsed (parse input)]
    (->> parsed
         ;; Add neighbors to all potential part numbers
         add-neighbors
         ;; Add symbol status (is a symbol near?)
         (add-adjacent-symbols (:syms parsed))
         ;; Filter to *real* part numbers
         :parts vals
         (filter (comp seq :adjacent-symbols))
         ;; Get the part number's value
         (map :value)
         ;; Sum
         (apply +))))

(defn answer-2 [input]
  (let [parsed     (-> input parse add-neighbors)
        ;; Gather star coords, add their adjacent parts
        stars      (->> parsed :syms
                        (filter (fn [[_ sym]] (= \* sym)))
                        (map first)
                        (map (fn [star-coord]
                               {:coord          star-coord
                                :adjacent-parts (->> parsed :parts
                                                     (map second)
                                                     (filter (fn [part]
                                                               (contains? (:neighbors part) star-coord)))
                                                     (map :value))})))
        ;; Just filter to at least 2 adjacent parts
        gears      (filter #(< 1 (count (:adjacent-parts %))) stars)
        ;; The product of each gear's adjacent part number values
        ratios     (->> gears
                        (map :adjacent-parts)
                        (map #(apply * %)))
        ;; Sum of the ratios
        ratio-sum  (->> ratios
                        (apply + ))
        components {:ratio-sum ratio-sum
                    :ratios    ratios
                    :stars     stars
                    :gears     gears
                    :board     parsed}]
    (:ratio-sum components)))

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

  (parse sample)

  (parse INPUT)





  (answer-1 sample)
  ;; => 4361

  (answer-1 INPUT)
  ;; => 537732

  (answer-2 sample)
  ;; => 467835

  (answer-2 INPUT)
  ;; => 84883664

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
