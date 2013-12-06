(ns tv-scraper.imdb
  (:use [clojure.string :only [split]])
  (:use [clj-time.format :only [parse formatter] :rename {parse date-parse}])
  (:import java.net.URL)
  (:use tv-scraper.poor-mans-mechanize.network)
  (:use tv-scraper.collection-helper)
  (:use net.cgrand.enlive-html))

(defn ^:private search-url [show-name]
  (-> "http://www.imdb.com/find" (url-for {:q show-name :s "tt" :ttype "tv"}) URL.))

(defn ^:private correct-imdb-url [url]
  (-> (str "http://www.imdb.com" url) (split #"(\?|\&)ref") first))

(defn ^:private interpret-search-results [html]
  (let [build-result #(hash-map :url (-> % :attrs :href correct-imdb-url) :title (-> % :content first))]
    (map build-result (select html [:.findResult :.result_text :a]))))

(defn search-results-for [show-name]
    (-> show-name search-url html-resource interpret-search-results))

(defn find-show-url [show-name]
  (-> show-name search-results-for first :url))

(defn extract-seasons-urls [html]
  (let [blocks (-> html (select [:#titleTVSeries :.txt-block]))
        season-block (first (filter #(.startsWith (-> % (select [:h4]) first text) "Season") blocks))
        links (select season-block [:a])
        build-hash #(hash-map (-> % text keyword) (-> % :attrs :href correct-imdb-url))]
    (->> links (map build-hash) (reduce merge))))

(defn retrieve-seasons [html]
  (extract-seasons-urls html))

(defn parse-show-page [url]
  (let [html (-> url URL. html-resource)
        extract-title #(-> % (select [:.header :.itemprop]) first text)]
    {:title (extract-title html)
     :seasons (retrieve-seasons html)}))
