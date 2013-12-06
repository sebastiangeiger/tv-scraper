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
  (-> (str "http://www.imdb.com" url) (split #"\?") first))

(defn ^:private interpret-search-results [html]
  (let [build-result #(hash-map :url (-> % :attrs :href correct-imdb-url) :title (-> % :content first))]
    (map build-result (select html [:.findResult :.result_text :a]))))

(defn search-results-for [show-name]
    (-> show-name search-url html-resource interpret-search-results))

(defn find-show-url [show-name]
  (-> show-name search-results-for first :url))
