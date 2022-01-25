(ns curve-analysis.pdf
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [curve-analysis.util :as util])
  (:import [org.apache.pdfbox.pdmodel PDDocument]
           [org.apache.pdfbox.text PDFTextStripper]
           [java.text SimpleDateFormat]))

(def ignored-strings
  #{"ar" "wr" "nr" "mr" "wl" "sb" "pb" "ms" "q" "yc" "tr" "16.5.3" "17.3.1"})

(def year-only-pdfs
  #{"Results__Oslo_2021.pdf"})

(def pdf->expected-results
  {"Results_Rome_2017.pdf"  3                               ;; missing DoB
   "Results_Rabat_2016.pdf" 4                               ;; missing month/year in DoB
   "Results_Oslo_2017.pdf"  6                               ;; missing DoB
   "Results_Paris_2016.pdf" 5
   "Results_Paris_2018.pdf" 6                               ;; only 6 competitors
   })

(defn skip-string?
  [s]
  (or (contains? ignored-strings s)
      (str/starts-with? s "(")
      (str/starts-with? s "=")))

(defn get-text
  [path-to-file]
  (with-open [pdf (PDDocument/load (io/file path-to-file))]
    (let [stripper (doto (PDFTextStripper.)
                     (.setSortByPosition true))]
      (mapv (comp str/trim str/lower-case) (str/split-lines (.getText stripper pdf))))))

(defn parse-result-line
  ([line] (parse-result-line false line))
  ([year-only? line]
   (let [split (->> (str/split line #"\s+") (remove skip-string?) vec)]
     (if year-only?
       (when (and (int? (util/parse-int (first split))) (>= (count split) 8))
         (try
           (let [athlete-name (-> split (subvec 1 (- (count split) 5)))
                 [nationality year lane reaction-time time :as rest] (subvec split (- (count split) 5))]
             {:athlete-name  (str/join " " athlete-name)
              :athlete-dob   (let [year (Integer/parseInt year)]
                               (.parse (SimpleDateFormat. "yyyy") (str (if (< year 100) (+ 1900 year) year))))
              :reaction-time (Double/parseDouble reaction-time)
              :lane          (Integer/parseInt lane)
              :time          (Double/parseDouble time)})
           (catch Exception _)))
       (when (and (int? (util/parse-int (first split))) (>= (count split) 10))
         (try
           (let [athlete-name (-> split (subvec 1 (- (count split) 7)))
                 [nationality day month year lane reaction-time time :as rest] (subvec split (- (count split) 7))]
             {:athlete-name  (str/join " " athlete-name)
              :athlete-dob   (.parse (SimpleDateFormat. "dd MMM yyyy") (format "%s %s %s" day month year))
              :reaction-time (Double/parseDouble reaction-time)
              :lane          (Integer/parseInt lane)
              :time          (Double/parseDouble time)})
           (catch Exception _)))))))

(comment
  (parse-result-line "1 xie zhenye chn 17 aug 1993 9 0.168 19.88 ar pb"))

(def formats
  (->> ["dd.MM.yyyy"
        "d MMMM yyyy"]
       (map (fn [fmt] (SimpleDateFormat. fmt)))))

(defn extract-date-from-pdf-text
  [text]
  (->> text
       (keep (fn [line]
               (let [normalized (cond-> line
                                        (some? (str/index-of line "-")) (subs (inc (str/index-of line "-")))
                                        true (str/trim)
                                        true (str/replace #"(\d)(st|nd|rd|th)" "$1"))]
                 (->> formats
                      (keep (fn [^SimpleDateFormat sdf]
                              (try (.parse sdf normalized)
                                   (catch Throwable _))))
                      first))))
       first))

(comment
  (extract-date-from-pdf-text ["20th - 21st august 2021"] ))

(def pdf->date
  {"pdfs/Eugene-2018-Full-Results.pdf" #inst "2018-04-28"
   "pdfs/Results_Brussels_2021.pdf" #inst "2021-08-03"})

(defn extract-200m-times-from-pdf
  [path-to-file]
  (let [text (get-text path-to-file)
        event-start (->> text
                         ;; move to start of 200m
                         (drop-while #(not= % "200m men")))
        filename (.getName (io/file path-to-file))
        meet-date (or (pdf->date filename) (extract-date-from-pdf-text text))
        year-only? (contains? year-only-pdfs filename)
        results (->> event-start
                     ;; move to start of results
                     (drop-while #(not (str/starts-with? % "rank")))
                     (drop-while #(not (str/starts-with? % "1")))
                     (map (partial parse-result-line year-only?))
                     (take-while some?))
        wind-line (->> event-start
                       (take (count results))
                       (filter #(str/includes? % "wind: "))
                       first)]
    (when (not-empty event-start)
      (assert (>= (count results) (pdf->expected-results filename 7)) (format "had only %s results for pdf %s" (count results) path-to-file))
      (assert (some? wind-line) (format "couldn't parse wind for pdf %s" path-to-file))
      (assert (some? meet-date) (format "couldn't parse meet date for pdf %s" path-to-file))
      (let [wind-speed (some->> wind-line (re-find #"wind: ([+-\\d\\.]+)") second Double/parseDouble)]
        (assert (some? wind-speed) (format "couldn't parse wind for pdf %s" path-to-file))
        (->> results
             (mapv #(assoc % :wind-speed wind-speed :meet-date meet-date)))))))

(comment
  (second (re-find #"wind: ([+-\\d\\.]+)" "start time:  20:40 wind: -3.0 m/s"))
  (extract-200m-times-from-pdf "pdfs/Results__Oslo_2021.pdf")
  (take 10 (get-text "pdfs/Results_Eugene_2021.pdf"))
  (extract-date-from-pdf-text ["20th - 21st august 2021"] ))
