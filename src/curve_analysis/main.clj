(ns curve-analysis.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [curve-analysis.pdf :as pdf]
            [clojure.edn :as edn]
            [curve-analysis.util :as util]
            [curve-analysis.worldathletics :as wa])
  (:import [java.io File]))


(def results-file "output/all-results.edn")
(defn parse-pdfs
  []
  (let [f (io/file results-file)]
    (if (.exists f)
      (edn/read-string (slurp f))
      (let [all-results (->> (file-seq (io/file "pdfs"))
                             (filter #(str/ends-with? (.getName ^File %) ".pdf"))
                             (map #(.getAbsolutePath %))
                             (sort)
                             (mapcat pdf/extract-200m-times-from-pdf)
                             (map #(assoc % :corrected (util/correct-200m-for-wind (:time %) (:wind-speed %))))
                             vec)]
        (spit f all-results)
        all-results))))

(defn add-seasons-best-information
  [{:keys [athlete-name meet-date corrected] :as result}]
  (assert (inst? meet-date) (format "meet date for %s is not a date!" result))
  (let [race-year (+ 1900 (.getYear meet-date))
        athlete-id (wa/athlete-name->athlete-id athlete-name)
        fastest-time (wa/fastest-time-by-year athlete-id race-year)]
    (assert (some? fastest-time) (format "fastest time for %s (%s) in %s not found!" athlete-name athlete-id race-year))
    (assert (>= corrected fastest-time) (format "fastest time for %s in %s (%s) is slower than reported time %s!" athlete-name race-year fastest-time corrected))
    (assoc result
      :fastest-time fastest-time
      :diff (- corrected fastest-time))))

(defn -main
  []
  (let [all-results (->> (parse-pdfs)
                         (sort-by :lane)
                         (map add-seasons-best-information)
                         vec)
        csv (with-out-str
              (println "Lane,Time,Diff From Season's Best")
              (doseq [{:keys [lane time diff]} all-results]
                (println (format "%s,%s,%s" lane time diff))))]
    (spit "output/all-results.csv" csv)))

(comment
  (do
    (io/delete-file results-file true)
    (-main))


  (-main))
