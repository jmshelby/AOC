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

;; Parse input, analize page updates and violations, return details
(defn- *answer-1 [input]
  (let [{:keys [rules pages]
         :as   params}
        (parse-input input)]
    (assoc params :analysis
           (->> pages
                (map (fn [page-seq]
                       (let [rev-pairs (-> page-seq
                                           reverse vec
                                           ordered-pairs
                                           set)]
                         {:page-seq
                          page-seq
                          :rule-violations
                          (set/intersection rules rev-pairs)
                          })))))))

(defn get-middle-elem [coll]
  (when (-> coll count even?)
    (throw (Exception. "Can't get the middle element of an even counted list")))
  (get coll
       (/ (dec (count coll)) 2)))

(defn answer-1 [input]
  ;; Analyze, filter out page order violations,
  ;; sum the middle page of each
  (->> (*answer-1 input)
       :analysis
       (filter #(zero? (count (:rule-violations %))))
       (map :page-seq)
       (map get-middle-elem)
       (apply +)))

;; ===========================================

;; Find first exact element, return index
(defn- search-vector [coll key]
  (->> coll
       (map-indexed vector)
       (some (fn [[i elem]]
               (when (= key elem)
                 i)))))

;; Remove element from vector, by index
(defn- remove-elem [coll idx]
  (vec (concat (subvec coll 0 idx)
               (subvec coll (inc idx)))))

;; Add element to vector, injecting it at
;; given index, shifting the remaining items
(defn- inject-elem [coll idx elem]
  (vec (concat (subvec coll 0 idx)
               [elem]
               (subvec coll idx))))

;; Ensure a is before b, if not,
;; move b to directly after a
(defn- ensure-order [coll [a b]]
  (let [aidx (search-vector coll a)
        bidx (search-vector coll b)]
    ;; Only if b is before a...
    (if (< bidx aidx)
      ;; Move elements around
      (let [;; Remove b
            removed (remove-elem coll bidx)]
        ;; Inject b directly after a,
        ;; same aidx as original since
        ;; it's shifted after removal
        (inject-elem removed aidx b))
      ;; Return untouched
      coll)))

(defn answer-2 [input]
  ;; Prep/Analyze same as answer 1
  (->> (*answer-1 input)
       :analysis
       ;; Filter to the incorrect page sequences
       (filter #(not (zero? (count (:rule-violations %)))))
       ;; Attach a "corrected" versions, using violations
       (map (fn [{:keys [page-seq rule-violations]
                  :as   m}]
              (assoc m :page-seq-corrected
                     (reduce ensure-order page-seq (sort rule-violations)))))
       ;; Extract middle elements and sum for answer checksum
       (map :page-seq-corrected)
       (map get-middle-elem)
       (apply +)))

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

  (println INPUT)


  (sort #{1 3 5 2 6 9})

  (answer-1 INPUT)
  ;; => 5248

  (answer-2 INPUT)
  ;; => 4755

  ;;
  )
