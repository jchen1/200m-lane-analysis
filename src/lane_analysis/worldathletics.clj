(ns lane-analysis.worldathletics
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [lane-analysis.util :as util]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def headers
  {"x-amz-user-agent" "aws-amplify/3.0.2"
   "x-api-key" "da2-kgm3anvg4zfhlgpc5hfpkpvtt4"})

(def endpoint "https://pvgc4hmgxngb7n5ant4ucszemq.appsync-api.eu-west-1.amazonaws.com/graphql")
(def search-query "query SearchCompetitors($query: String, $gender: GenderType, $disciplineCode: String, $environment: String, $countryCode: String) {\n  searchCompetitors(query: $query, gender: $gender, disciplineCode: $disciplineCode, environment: $environment, countryCode: $countryCode) {\n    aaAthleteId\n    familyName\n    givenName\n    birthDate\n    disciplines\n    iaafId\n    gender\n    country\n    urlSlug\n  }\n}\n")
(def fastest-time-query "query GetSingleCompetitorResultsDiscipline($id: Int, $resultsByYearOrderBy: String, $resultsByYear: Int) {\n  getSingleCompetitorResultsDiscipline(id: $id, resultsByYear: $resultsByYear, resultsByYearOrderBy: $resultsByYearOrderBy) {\n    parameters {\n      resultsByYear\n      resultsByYearOrderBy\n      __typename\n    }\n    activeYears\n    resultsByEvent {\n      indoor\n      disciplineCode\n      disciplineNameUrlSlug\n      typeNameUrlSlug\n      discipline\n      withWind\n      results {\n        date\n        competition\n        venue\n        country\n        category\n        race\n        place\n        mark\n        wind\n        notLegal\n        resultScore\n        remark\n        __typename\n      }\n      __typename\n    }\n    __typename\n  }\n}\n")

(def cache-dir "cache")

(def athlete-name->athlete-id-overrides
  {"titi ncincihli"                 14417761,
   "johansen matias hove"           14652141,
   "desalu eseosa"                  14403082,
   "hove johansen andreas"          14652141,
   "camilo de oliveira paulo andré" 14699143,
   "koffi wilfried"                 14185652,
   "zeze mickael-meba"              14389351,
   "kyeremeh stefan skogheim"       14774584,
   "zeze méba-mickaël"              14389351})

(defn athlete-name->athlete-id
  [athlete-name]
  (or
    (athlete-name->athlete-id-overrides athlete-name)
    (let [cache-file (io/file cache-dir "search" (format "%s.edn" (util/md5 athlete-name)))
          normalized-name (-> athlete-name util/deaccent str/lower-case)
          search-name-set (-> normalized-name (str/split #"\s+") set)
          {:keys [status body] :as response} (when-not (.exists cache-file)
                                               (http/post endpoint
                                                          {:body         (json/write-str
                                                                           {:operationName "SearchCompetitors"
                                                                            :variables     {:query          normalized-name
                                                                                            :gender         "male"
                                                                                            :disciplineCode "200"
                                                                                            :environment    "outdoor"}
                                                                            :query         search-query})
                                                           :content-type :json
                                                           :accept       :json
                                                           :headers      headers}))
          _ (assert (or (.exists cache-file) (= 200 status)) (format "got status %s when searching for %s" status athlete-name))
          results (if (.exists cache-file)
                    (-> cache-file slurp edn/read-string)
                    (-> (json/read-str body :key-fn keyword) :data :searchCompetitors))]
      (spit cache-file results)
      (some->> results
               (filter #(= search-name-set (-> (format "%s %s" (:familyName %) (:givenName %)) util/deaccent str/lower-case (str/split #"\s+") set)))
               (first)
               :aaAthleteId
               (Integer/parseInt)))))

(comment
  (athlete-name->athlete-id "reid leon"))

(defn fastest-time-by-year
  [athlete-id year]
  (let [cache-file (io/file cache-dir "results" (format "%s.edn" (util/md5 (str athlete-id year))))
        {:keys [status body] :as response} (when-not (.exists cache-file)
                                             (http/post endpoint
                                                        {:body (json/write-str
                                                                 {:operationName "GetSingleCompetitorResultsDiscipline"
                                                                  :variables {:id athlete-id
                                                                              :resultsByYear year
                                                                              :resultsByYearOrderBy "discipline"}
                                                                  :query fastest-time-query})
                                                         :content-type :json
                                                         :accept :json
                                                         :headers headers}))
        _ (assert (or (.exists cache-file) (= 200 status)) (format "got status %s when looking for fastest times for %s %s" status athlete-id year))
        results (if (.exists cache-file)
                  (-> cache-file slurp edn/read-string)
                  (-> (json/read-str body :key-fn keyword) :data :getSingleCompetitorResultsDiscipline :resultsByEvent))]
    (spit cache-file results)
    (some->> results
             (filter #(and (not (:notLegal %))
                           (= (:disciplineCode %) "200")
                           (not (:indoor %))))
             (first)
             :results
             (keep (fn [{:keys [mark wind]}]
                     (let [mark (util/parse-double mark)
                           wind (util/parse-double wind)]
                       (when (and (double? mark) (double? wind))
                         (util/correct-200m-for-wind mark wind)))))
             (sort)
             (first))))

(comment
  (fastest-time-by-year (athlete-name->athlete-id "noah lyles") 2021))
