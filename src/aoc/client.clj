(ns aoc.client
  (:require [clojure.string :as s]
            [clj-http.client :as ht]))

(def cookie "session=53616c7465645f5f90d2182abe425ef84f90ce779fb3635192ca0691ecd00a56b6c5eb513633c9fe3d188bf44124a59d4fcfdcbcd68e5e1d32e3de1d72f0cc59; _gat=1;")

(defn get-input [year day]
  (->>
    (ht/get (str "https://adventofcode.com/" year "/day/" day "/input")
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

  (def day2
    (get-first-input 2022 2))

  (-> day2
      (s/split-lines)
      )

  (println
    day2)

  (->>
    (ht/get "https://adventofcode.com/2022/day/2/input"
            {:headers {"Cookie" cookie}})
    :body
    count
    )




  ;; http -v https://adventofcode.com/2022/day/2/input 'Cookie:session=53616c7465645f5f90d2182abe425ef84f90ce779fb3635192ca0691ecd00a56b6c5eb513633c9fe3d188bf44124a59d4fcfdcbcd68e5e1d32e3de1d72f0cc59; _gat=1;'


  )


;; (s/defn stacktrace-info :- [tsk/KeyMap] ; #todo make cljs version
;;        "Returns a map with the caller's namespace and function names as strings, like:
;;            {:ns-name 'tst.demo.core' :fn-name 'funky'} "
;;        [throwable :- Throwable]
;;        (let [stacktrace      (.getStackTrace throwable)
;;              stacktrace-info (t/distinct-using #(grab :class-name %)
;;                                (for [st-elem stacktrace]
;;                                  (let [class-name  (.getClassName st-elem)
;;                                        file-name   (.getFileName st-elem)
;;                                        method-name (.getMethodName st-elem)
;;                                        line-num    (.getLineNumber st-elem)

;;                                        ; class-name-caller is (usually) like "tst.demo.core$funky".
;;                                        ; if no `$` is present, don't crash!
;;                                        idx        (str/index-of class-name \$)
;;                                        ns-name     (t/cond-it-> class-name
;;                                                      (t/not-nil? idx) (subs class-name 0 idx))
;;                                        fn-name     (if (t/not-nil? idx)
;;                                                      (subs class-name idx)
;;                                                      "")]
;;                                    (vals->map class-name file-name method-name line-num ns-name fn-name))))]
;;          stacktrace-info))
