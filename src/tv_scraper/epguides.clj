(ns tv-scraper.epguides
  (:use [clojure.string :only [replace lower-case] :rename {replace str-replace}])
  (:use [clj-time.format :only [parse formatter] :rename {parse date-parse}])
  (:import java.net.URL)
  (:use tv-scraper.poor-mans-mechanize.form)
  (:use tv-scraper.collection-helper)
  (:use net.cgrand.enlive-html))

(def ^:private config
  {:url "http://epguides.com/"
   :title-regex #"^(.*) \(a Titles \& Air Dates Guide\) \- Epguides\.com$"
   :episode-regex #"^(\d+)\s+(\d+)-(\d+)(\s+\d+)?\s+(\d{1,2}\/\w{3}\/\d{2})\s*$"
   :season-headings-regex [#"^\s*•\s*Season (\d+)\s*$" #"^(Other Episodes)$"]
   })

(defn interpret-search-results [html-content]
  (let [results (select html-content [:#search [:li :.g]])
        title-regex #(re-matches (config :title-regex) %)
        extract-title #(-> % (select [:h3]) first text title-regex last)
        extract-url #(-> % (select [:h3 :a]) first :attrs :href)]
    (map (fn [x] {:title (extract-title x) :url (extract-url x)}) results)))

(defn search-results-for [show-name]
  (let [search-form (-> (config :url) URL. html-resource (select [:body :form]) first)
        submit-button [[(attr= :type "submit") (attr= :name "btnG")]]]
    (interpret-search-results (submit-form (set-value search-form {:#googleSearch show-name}) submit-button))))

(defn find-show-url [show-name]
  (-> show-name search-results-for first :url))

(defn split-by-newlines [array]
  (let [split #(if (string? %) (clojure.string/split-lines %) [%])]
    (remove #(and (string? %) (clojure.string/blank? %)) (mapcat split array))))

(defn split-into-seasons [page-data extract-season]
  (let [cleaned-up (-> page-data first :content split-by-newlines)
        season-number #(-> % extract-season lower-case (str-replace " " "_") keyword)
        season-headline? #(not (nil? (extract-season %)))
        remove-meta-data #(into {} (for [[k v] % :when (not (nil? k))] [k v]))]
    (dissoc (convert-keys season-number (remove-meta-data (split-map season-headline? cleaned-up))) :other_episodes)))

(defn build-episode [regex [string links]]
  {:pre [(string? string)]}
  (let [fragments (re-matches regex string)
        to-int #(Integer/parseInt %)
        to-date #(date-parse (formatter "dd/MMM/yy") %)]
    {(-> fragments (nth 3) to-int str keyword)
     {:date (-> fragments last to-date)
      :title (-> links first :content first)}}))

(defn build-episodes [lines]
  (let [regex (config :episode-regex)]
    (apply merge (map #(build-episode regex %) (split-on #(and (string? %) (re-matches regex %)) lines)))))

(defn split-into-episodes [seasons]
  (into {} (for [[k v] seasons] [k {:episodes (build-episodes v)}])))

(defn ^:private extract-season-headings [string]
  (->> (config :season-headings-regex)
    (map #(re-matches % string))
    (reduce #(or %1 %2))
    last))

(defn parse-seasons [page]
  (-> page
    (select [:#eplist :pre])
    (split-into-seasons #(if (string? %) (extract-season-headings %) nil))
    split-into-episodes))

(defn parse-show-page [url]
  (let [extract-title #(-> % (select [:#header :h1]) first text)
        page (-> url URL. html-resource)]
    {:title (extract-title page)
     :seasons (parse-seasons page)}))
