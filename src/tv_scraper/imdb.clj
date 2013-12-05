(ns tv-scraper.imdb
  (:use [clojure.string :only [replace lower-case] :rename {replace str-replace}])
  (:use [clj-time.format :only [parse formatter] :rename {parse date-parse}])
  (:import java.net.URL)
  (:use tv-scraper.poor-mans-mechanize.form)
  (:use tv-scraper.collection-helper)
  (:use net.cgrand.enlive-html))

(defn find-show-url [show-name])
