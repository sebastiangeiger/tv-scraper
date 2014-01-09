(ns tv-scraper.wikipedia
  (:require [tv-scraper.poor-mans-mechanize.network :refer [url-for]]
            [net.cgrand.enlive-html :refer :all])
  (:import java.net.URL))

(defn ^:private wikipedia-url [params]
  (-> "http://en.wikipedia.org/w/api.php" (url-for params) URL.))
(defn ^:private search-url [show-name]
  (wikipedia-url {:action "query"
                  :list "search"
                  :srsearch (str show-name " TV series")
                  :format "xml"}))

(defn search-results-for [show-name]
  (->>
    (-> show-name search-url xml-resource (select [:api :search :p]))
    (map #(-> % :attrs (select-keys [:title])))))

(defn find-show-page-title [show-name]
  (-> show-name search-results-for first :title))

(defn ^:private episode-list-page [show-name]
  (wikipedia-url {:action "query"
                  :titles show-name
                  :prop "revisions"
                  :rvprop "content"
                  :format "xml"}))

(defn parse-show-page [show-name]
  (let [page-content (-> show-name
                       episode-list-page
                       xml-resource
                       (select [:api :pages]) first
                       (select [:revisions :rev]) first
                       :content)
        info-box ""]
    info-box))

(defn list-of-episodes-page [page-content]
  (->> (clojure.string/split page-content #"\|")
    (map #(->> % (re-matches #"^list_episodes\s+=(.*)$") last))
    (filter identity)
    (map clojure.string/trim)
    first))
