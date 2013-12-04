(ns tv-scraper.core-test
  (:require [clojure.test :refer :all]
            [tv-scraper.core :refer :all]
            [tv-scraper.epiguides :refer :all]))

;;@network
;; (deftest find-a-show-url
;;          (is (= (find-show-url "Sons of Anarchy") "http://epguides.com/SonsofAnarchy/"))
;;          (is (= (find-show-url "Hank 1965") "http://epguides.com/Hank_1965/"))
;;          (is (= (find-show-url "The Wire") "http://epguides.com/Wire/")))

;; @network
;; (deftest the-search-results-for-a-term
;;          (is (= (search-results-for "Wire")
;;                 [{:title "The Wire" :url "http://epguides.com/Wire/"}
;;                  {:title "Wire in the Blood" :url "http://epguides.com/WireintheBlood/"}])))

;;@network
;; (deftest parsing-a-show-page
         ;; (is (= (-> "http://epguides.com/Futurescape" parse-show-page :title) "Futurescape"))
         ;; (is (= (-> "http://epguides.com/Lost" parse-show-page :seasons keys set) #{:1 :2 :3 :4 :5 :6 :other_episodes}))
         ;; (is (= (-> "http://epguides.com/Lost" parse-show-page :seasons :1) #{:1 :2 :3 :4 :5 :6}))
         ;; )

(deftest parsing-the-season-body
         (is (= (build-episodes
                  ["1      1-01                16/Nov/13   "
                   {:tag :a, :attrs {:title "Futurescape season 1 episode 1", :href "http://www.tvrage.com/shows/id-38547/episodes/1065424259"}, :content ["I Know What You're Thinking"]}
                   "2      1-02                19/Nov/13   "
                   {:tag :a, :attrs {:title "Futurescape season 1 episode 2", :href "http://www.tvrage.com/shows/id-38547/episodes/1065424258"}, :content ["Robot Revolution"]}])
                [{:episode 1 :season 1 :date "16/Nov/13" :title "I Know What You're Thinking"}
                 {:episode 2 :season 1 :date "19/Nov/13" :title "Robot Revolution"}]))
         (is (= (build-episodes
                  ["21     1-21      121       04/May/05   "
                   {:tag :a, :attrs {:title "Lost season 1 episode 21", :href "http://www.tvrage.com/Lost/episodes/104131"}, :content ["The Greater Good"]}
                   {:tag :span, :attrs {:class "Trailers"}, :content ["[" {:tag :a, :attrs {:href "http://www.tvrage.com/Lost/episodes/104131/?trailer=1#trailer"}, :content ["Trailer"]} "]"]}
                   "22     1-22      122       11/May/05   "
                   {:tag :a, :attrs {:title "Lost season 1 episode 22", :href "http://www.tvrage.com/Lost/episodes/104132"}, :content ["Born to Run"]}
                   {:tag :span, :attrs {:class "Trailers"}, :content ["[" {:tag :a, :attrs {:href "http://www.tvrage.com/Lost/episodes/104132/?trailer=1#trailer"}, :content ["Trailer"]} "]"]}])
                [{:episode 21 :season 1 :date "04/May/05" :title "The Greater Good"}
                 {:episode 22 :season 1 :date "11/May/05" :title "Born to Run"}])))


(deftest splitting-an-array-by-newlines
         (is (= (split-by-newlines ["1\n2\n\n3" {:ignore "this"} "4 5\n6"])
               ["1" "2" "3" {:ignore "this"} "4 5" "6"]))
         (is (= (split-by-newlines ["1\n2\n\n3" "4\n5\n6"])
               ["1" "2" "3" "4" "5" "6"])))
