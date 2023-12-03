(ns aoc.client
  (:require [clojure.string :as s]
            [clj-http.client :as ht]))

(defn get-cookie []
  (str "session=" (slurp ".cookie-session")))

(defn get-input [year day]
  (->>
    (ht/get (str "https://adventofcode.com/" year "/day/" day "/input")
            {:headers {"Cookie" (get-cookie)}})
    :body))

(defn get-problem [year day]
  (->>
    (ht/get (str "https://adventofcode.com/" year "/day/" day)
            {:headers {"Cookie" (get-cookie)}})
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

  (get-cookie)

  )
