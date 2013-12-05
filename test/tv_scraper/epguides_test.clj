(ns tv-scraper.epguides-test
  (:require [clojure.test :refer :all]
            [tv-scraper.epguides :refer :all]
            [clj-time.core :refer [date-time]]))


(deftest ^:google find-a-show-url
         (is (= (find-show-url "Sons of Anarchy") "http://epguides.com/SonsofAnarchy/"))
         (is (= (find-show-url "Hank 1965") "http://epguides.com/Hank_1965/"))
         (is (= (find-show-url "The Wire") "http://epguides.com/Wire/")))

(deftest ^:google the-search-results-for-a-term
         (is (= (search-results-for "Wire")
                [{:title "The Wire" :url "http://epguides.com/Wire/"}
                 {:title "Wire in the Blood" :url "http://epguides.com/WireintheBlood/"}])))

(deftest ^:network parsing-a-show-page
         (is (= (-> "http://epguides.com/Futurescape" parse-show-page :title) "Futurescape"))
         (is (= (-> "http://epguides.com/Lost" parse-show-page :seasons keys set) #{:1 :2 :3 :4 :5 :6}))
         (is (= (-> "http://epguides.com/Futurescape" parse-show-page :seasons :1 :episodes :6 :title) "How to Be a Superhuman"))
         (is (= (-> "http://epguides.com/Lost" parse-show-page :seasons :2 :episodes :17 :title) "Lockdown"))
         )

(deftest parsing-the-season-body
         (is (= (build-episodes
                  ["21     1-21      121       04/May/05   "
                   {:tag :a, :attrs {:title "Lost season 1 episode 21", :href "http://www.tvrage.com/Lost/episodes/104131"}, :content ["The Greater Good"]}
                   {:tag :span, :attrs {:class "Trailers"}, :content ["[" {:tag :a, :attrs {:href "http://www.tvrage.com/Lost/episodes/104131/?trailer=1#trailer"}, :content ["Trailer"]} "]"]}
                   "22     1-22      122       11/May/05   "
                   {:tag :a, :attrs {:title "Lost season 1 episode 22", :href "http://www.tvrage.com/Lost/episodes/104132"}, :content ["Born to Run"]}
                   {:tag :span, :attrs {:class "Trailers"}, :content ["[" {:tag :a, :attrs {:href "http://www.tvrage.com/Lost/episodes/104132/?trailer=1#trailer"}, :content ["Trailer"]} "]"]}])
                {:21 {:date (date-time 2005 5 4) :title "The Greater Good"}
                 :22 {:date (date-time 2005 5 11) :title "Born to Run"}})))

(deftest splitting-an-array-by-newlines
         (is (= (split-by-newlines ["1\n2\n\n3" {:ignore "this"} "4 5\n6"])
               ["1" "2" "3" {:ignore "this"} "4 5" "6"]))
         (is (= (split-by-newlines ["1\n2\n\n3" "4\n5\n6"])
               ["1" "2" "3" "4" "5" "6"])))
