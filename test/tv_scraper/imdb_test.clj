(ns tv-scraper.imdb-test
  (:require [clojure.test :refer :all]
            [tv-scraper.imdb :refer :all]
            [clj-time.core :refer [date-time]]))


(deftest ^:network find-a-show-url
         (is (= (find-show-url "Sons of Anarchy") "http://www.imdb.com/title/tt1124373/"))
         (is (= (find-show-url "Hank 1965") "http://www.imdb.com/title/tt0058811/"))
         (is (= (find-show-url "The Wire") "http://www.imdb.com/title/tt0306414/")))

(deftest ^:network parse-a-season-page
         (let [jericho-season-1 (-> "http://www.imdb.com/title/tt0805663/episodes?season=1" parse-season-page)]
           (is (= (-> jericho-season-1 :3 :date) (date-time 2006 10 4)))
           (is (= (-> jericho-season-1 :3 :title) "Four Horsemen"))))

(defn keyword-range [& args]
  (map (comp keyword str) (apply range args)))

(deftest ^:network parsing-a-show
         (let [jericho (-> "http://www.imdb.com/title/tt0805663" parse-show-page)]
           (is (= (-> "http://www.imdb.com/title/tt1441135" parse-show-page :title) "FlashForward"))
           (is (= (-> jericho :seasons keys set) #{:1 :2}))
           (is (= (-> jericho :seasons :1 :episodes keys set) (set (keyword-range 1 23))))
           (is (= (-> jericho :seasons :1 :episodes :14 :title) "Heart of Winter"))))
