(ns curve-analysis.util
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.text Normalizer Normalizer$Form]))

(defn parse-int
  [string]
  (try (Integer/parseInt string)
       (catch Exception _ nil)))

(defn parse-double
  [string]
  (try (Double/parseDouble string)
       (catch Exception _ nil)))

(defn deaccent
  [string]
  (let [normalized (Normalizer/normalize string Normalizer$Form/NFD)]
    (-> normalized
        (str/replace #"\p{InCombiningDiacriticalMarks}+" "")
        (str/replace #"ø" "o")
        (str/replace #"Ø" "O"))))

(comment
  (Normalizer/normalize "bøe filip" Normalizer$Form/NFD)
  (deaccent "bøe filip"))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn correct-200m-for-wind
  "constants & equation from https://maximmoinat.github.io/windCalculator.html"
  [time wind]
  (+ time (+ (* 0.09 wind)
             (* -0.01 (Math/pow wind 2)))))
