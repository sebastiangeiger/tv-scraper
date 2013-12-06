(ns tv-scraper.imdb-test
  (:require [clojure.test :refer :all]
            [tv-scraper.imdb :refer :all]
            [clj-time.core :refer [date-time]]))


(deftest ^:network find-a-show-url
         (is (= (find-show-url "Sons of Anarchy") "http://www.imdb.com/title/tt1124373/"))
         (is (= (find-show-url "Hank 1965") "http://www.imdb.com/title/tt0058811/"))
         (is (= (find-show-url "The Wire") "http://www.imdb.com/title/tt0306414/")))

(deftest ^:wip parsing-a-show
         (is (= (-> "http://www.imdb.com/title/tt0411008/" parse-show-page :title) "Lost")))
