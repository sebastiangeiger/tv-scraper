(ns tv-scraper.imdb-test
  (:require [clojure.test :refer :all]
            [tv-scraper.imdb :refer :all]
            [clj-time.core :refer [date-time]]))


(deftest ^:network find-a-show-url
         (is (= (find-show-url "Sons of Anarchy") "http://www.imdb.com/title/tt1124373/"))
         (is (= (find-show-url "Hank 1965") "http://www.imdb.com/title/tt0058811/"))
         (is (= (find-show-url "The Wire") "http://www.imdb.com/title/tt0306414/")))

(deftest ^:network parse-a-season-page
         (is (= (-> "http://www.imdb.com/title/tt0805663/episodes?season=1" parse-season-page :3 :date) (date-time 2006 10 4)))
         (is (= (-> "http://www.imdb.com/title/tt0805663/episodes?season=1" parse-season-page :3 :title) "Four Horsemen")))

(deftest ^:network parsing-a-show
         (is (= (-> "http://www.imdb.com/title/tt0411008/" parse-show-page :title) "Lost"))
         (is (= (-> "http://www.imdb.com/title/tt0805663" parse-show-page :seasons keys set) #{:1 :2}))
         (is (= (-> "http://www.imdb.com/title/tt0805663" parse-show-page :seasons :1) "http://www.imdb.com/title/tt0805663/episodes?season=1"))
         (is (= (-> "http://www.imdb.com/title/tt1441135" parse-show-page :seasons :1 :episodes :14 :title) "Better Angels")))
