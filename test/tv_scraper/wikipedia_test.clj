(ns tv-scraper.wikipedia-test
  (:require [clojure.test :refer :all]
            [tv-scraper.wikipedia :refer :all]
            [clj-time.core :refer [date-time]]))

(deftest find-a-show-url
         (is (= (find-show-page-title "Sons of Anarchy") "Sons of Anarchy"))
         (is (= (find-show-page-title "Hank 1965") "Hank (1965 TV series)"))
         (is (= (find-show-page-title "Wire") "The Wire")))

(deftest the-search-results-for-a-term
         (is (= (search-results-for "Wire")
                 [{:title "The Wire"}
                 {:title "Wired (TV series)"}
                 {:title "State University of New York at Oneonta"}
                 {:title "Police procedural"}
                 {:title "The Greeks of The Wire"}
                 {:title "Dick Grayson"}
                 {:title "The Hitchhiker's Guide to the Galaxy"}
                 {:title "Batcave"}
                 {:title "The Legend of Zelda"}
                 {:title "Edgar & Ellen"}])))

(deftest finding-the-list-of-episodes-page
         (is (= (-> "test/tv_scraper/fixtures/the_wire" slurp list-of-episodes-page)
                "List of The Wire episodes")))
                "List of The Wire episodes")))
