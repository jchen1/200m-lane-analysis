(ns lane-analysis.viz
  (:require [oz.core :as oz]
            [oz.server :as oz-server]))

(def oz-port 10666)
(def graph-width 700)

(defn ensure-server-started!
  []
  (oz-server/start-server! oz-port))

(defn stop-server!
  []
  (oz-server/stop!))

(defn view!
  [plot & {:keys [host port mode :as config]}]
  (apply oz/view! plot config))

(defn export!
  [doc filepath & opts]
  (apply oz/export! doc filepath opts))

(defn plot-results-by-lane
  [all-results]
  {:title    "200m Times by Lane"
   :data     {:values all-results}
   :width    graph-width
   :encoding {:x {:field :lane :type :ordinal :title "Lane"}
              :y {:field :corrected :aggregate :mean :scale {:domain [19.5 21.5]} :title "Time (s)"}}
   :layer [{:mark :bar}
           {:mark {:type :text
                   :align :left
                   :dx -10
                   :dy -1
                   :baseline :bottom}
            :encoding {:text {:field :corrected
                              :aggregate :mean
                              :format ".2f"
                              :type :quantitative}}}]})

(defn box-whisker-plot
  [all-results]
  {:title    "200m Times by Lane"
   :data     {:values (->> all-results (filter #(< (:corrected %) 25)))}
   :width    graph-width
   :encoding {:x {:field :lane :type :ordinal :title "Lane"}
              :y {:field :corrected :type :quantitative :scale {:domain [19.4 23.8]} :title "Time (s)"}}
   :layer    [{:mark {:type :boxplot}}]})

(defn regression-from-best
  [all-results]
  {:title "Diff from Season's Best"
   :data  {:values (->>
                     all-results
                     ;; filter out likely injuries
                     (filter #(< (:diff %) 3))
                     (group-by :lane)
                     (map (fn [[lane vs]]
                            {:lane lane
                             :diff (/ (->> vs (map :diff) (apply +)) (count vs))})))}
   :width graph-width
   :layer [{:mark     {:type :point
                       :size 100
                       :filled true}
            :encoding {:x {:field :lane :type :ordinal :title "Lane"}
                       :y {:field :diff :type :quantitative :scale {:domain [0 0.6]} :title "Diff from season's best (s)"}}}
           {:mark      {:type  :line
                        :color :firebrick}
            :transform [{:regression :diff
                         :on         :lane}]
            :encoding  {:x {:field :lane
                            :type  :ordinal}
                        :y {:field :diff
                            :type  :quantitative}}}
           {:transform [{:regression :diff
                         :on         :lane
                         :params     true}
                        {:calculate "'R²: '+format(datum.rSquared, '.2f')"
                         :as        :R2}]
            :mark      {:type  :text
                        :color :firebrick
                        :x     :width
                        :align :right
                        :y     -5}
            :encoding  {:text {:type  :nominal
                               :field :R2}}}
           {:transform [{:regression :diff
                         :on         :lane
                         :params     true}
                        {:calculate "'y = ' + format(datum.coef[1], '.4f') + 'x + ' + format(datum.coef[0], '.3f')"
                         :as        :equation}]
            :mark      {:type  :text
                        :color :firebrick
                        :x     :width
                        :align :right
                        :y     -20}
            :encoding  {:text {:type  :nominal
                               :field :equation}}}]})

(defn histogram
  [all-results]
  {:title    "Heatmap"
   :data     {:values (->> all-results (filter #(< (:diff %) 1)))}
   :width    300
   :height   300
   :mark     :rect
   :encoding {:x     {:type  :ordinal
                      :field :lane
                      :title "Lane"}
              :y     {:bin   {:maxbins 10}
                      :field :diff
                      :title "Diff from season's best"}
              :color {:aggregate :count
                      :type      :quantitative
                      :title     "Number of results"
                      :field     :diff}}})

(defn all-graphs
  [all-results]
  [:main
   [:h1 "200m lane analysis"]
   [:vega-lite (plot-results-by-lane all-results)]
   [:vega-lite (box-whisker-plot all-results)]
   [:vega-lite (regression-from-best all-results)]
   [:vega-lite (histogram all-results)]])

(comment
  (ensure-server-started!)

  (view! (all (clojure.edn/read-string (slurp "output/all-results.edn"))))

  (first (clojure.edn/read-string (slurp "output/all-results.edn")))

  (stop-server!))
