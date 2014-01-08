(ns tv-scraper.wikipedia
  (:require [tv-scraper.poor-mans-mechanize.network :refer [url-for]]
            [net.cgrand.enlive-html :refer :all])
  (:import java.net.URL))

(defn search-url [show-name]
  (let [params {:action "query"
                :list "search"
                :srsearch (str show-name " TV series")
                :format "xml"}]
    (-> "http://en.wikipedia.org/w/api.php" (url-for params) URL.)))

(defn search-results-for [show-name]
  (->>
    (-> show-name search-url xml-resource (select [:api :search :p]))
    (map #(-> % :attrs (select-keys [:title])))))

(defn find-show-page-title [show-name]
  (-> show-name search-results-for first :title))
