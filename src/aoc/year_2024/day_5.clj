(ns aoc.year-2024.day-5
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [aoc.client :as aoc]))

(def INPUT (aoc/get-my-input))

;; Given a list length, return all
;; ordered-pair start and end indexes
(defn- combos [len]
  (for [s     (range len)
        e     (range len)
        :when (< s e)]
    [s e]))

;; Given a list, return all ordered pair elements
(defn- ordered-pairs [coll]
  (->> (combos (count coll))
       (mapv #(mapv coll %))))

(defn- parse-input [in]
  (let [[rules pages]
        (->> in
             s/split-lines
             (split-with #(not= % "")))
        ;; Take off blank row
        pages (rest pages)]
    (-> {:rules rules
         :pages pages}
        (update :rules (fn [rule]
                         (->> rule
                              (mapv #(s/split % #"\|"))
                              (mapv #(mapv parse-long %))
                              set)))
        (update :pages (fn [page]
                         (->> page
                              (mapv #(s/split % #"\,"))
                              (mapv #(mapv parse-long %))))))))

(defn answer-1 [input]
  )

(defn answer-2 [input]
  )

(comment

  (println INPUT)

  (def sample "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

  ;; pprint/pprint


  (let [coll '[a b c d e f g h]]
    (ordered-pairs coll)
    )


  (let [params (parse-input sample)]
    (assoc params :rule-set #{})
    )




  (println INPUT)

  (answer-1 INPUT)

  (answer-2 INPUT)


;;
)
