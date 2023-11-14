(ns aoc.client
  (:require [clojure.string :as s]
            [clj-http.client :as ht]))

(def cookie "session=53616c7465645f5f90d2182abe425ef84f90ce779fb3635192ca0691ecd00a56b6c5eb513633c9fe3d188bf44124a59d4fcfdcbcd68e5e1d32e3de1d72f0cc59; _gat=1;")

(defn get-input [year day]
  (->>
    (ht/get (str "https://adventofcode.com/" year "/day/" day "/input")
            {:headers {"Cookie" cookie}})
    :body))

(defn get-problem [year day]
  (->>
    (ht/get (str "https://adventofcode.com/" year "/day/" day)
            {:headers {"Cookie" cookie}})
    :body))

;; TODO - prompt for session cookie if not valid, store it in a file
(defn get-my-input []
  (let [ns (str *ns*)
        [found? year day]
        (re-matches #"year-(\d{4})\.day-(\d{1,2})" ns)]
    (if found?
      (get-input year day)
      (throw (ex-info (str "Can't determine your current year/day for 'advent of code'. "
                           "Is your namespace in the form: 'year-[XXXX].day-XX'?")
                      {:your-ns ns})))))

(comment


  (println
    (get-input 2022 4))

  (println
    (get-problem 2022 4))


  )
