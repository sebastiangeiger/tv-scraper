(ns tv-scraper.core-test
  (:require [clojure.test :refer :all]
            [tv-scraper.core :refer :all]
            [tv-scraper.epiguides :refer :all]))


;;@network
;; (deftest find-a-show-url
;;          (is (= (find-show-url "Sons of Anarchy") "http://epguides.com/SonsofAnarchy/"))
;;          (is (= (find-show-url "Hank 1965") "http://epguides.com/Hank_1965/"))
;;          (is (= (find-show-url "The Wire") "http://epguides.com/Wire/")))
;;
;;@network
;; (deftest the-search-results-for-a-term
;;          (is (= (search-results-for "Wire")
;;                 [{:title "The Wire" :url "http://epguides.com/Wire/"}
;;                  {:title "Wire in the Blood" :url "http://epguides.com/WireintheBlood/"}])))

(deftest setting-value-in-a-field
         (is (=
               (set-value
                 {:tag :form, :content [{:tag :input, :attrs {:size "40", :value "", :id "googleSearch", :name "q", :type "text"}, :content []}]}
                 {:#googleSearch "The Wire"})
               {:tag :form, :content [{:tag :input, :attrs {:size "40", :value "The Wire", :id "googleSearch", :name "q", :type "text"}, :content []}]})))

(deftest encoding-params
         (is (=
               (encode-params [{:hl "en"} {:q "allintitle:" :test "more"} {:q "site:epguides.com"} {:q "The Wire"}])
               "hl=en&test=more&q=allintitle%3A&q=site%3Aepguides.com&q=The+Wire")))

(deftest extracting-keys-and-values-from-a-form
         (is (=
               (extract-key-values {:tag :form, :content [{:tag :input, :attrs {:value "en", :name "hl", :type "hidden"}, :content nil} {:tag :input, :attrs {:value "allintitle:", :name "q", :type "hidden"}, :content nil} {:tag :input, :attrs {:value "site:epguides.com", :name "q", :type "hidden"}, :content nil} {:tag :input, :attrs {:size "40", :value "", :id "googleSearch", :name "q", :type "text"}, :content nil} {:tag :input, :attrs {:value "Search", :name "btnG", :type "submit"}, :content nil}]})
               [{:hl "en"} {:q "allintitle:"} {:q "site:epguides.com"} {:q ""}])))

;;@network
(deftest parsing-a-show-page
         (is (= (-> "http://epguides.com/Futurescape" parse-show-page :title) "Futurescape"))
         (is (= (-> "http://epguides.com/Lost" parse-show-page :seasons keys set) #{1 2 3 4 5 6}))
         )

(deftest splitting-an-array-by-newlines
         (is (= (split-by-newlines ["1\n2\n\n3" {:ignore "this"} "4 5\n6"])
               ["1" "2" "3" {:ignore "this"} "4 5" "6"]))
         (is (= (split-by-newlines ["1\n2\n\n3" "4\n5\n6"])
               ["1" "2" "3" "4" "5" "6"])))

(deftest splitting-an-array-into-a-map
         (is (= (split-map #(= (rem % 4) 0) [4 5 6 7 8 9 10 11 12])
                {4 [5 6 7] 8 [9 10 11] 12 []}))
         (is (= (split-map #(= (rem % 4) 0) [1 2 3 4 5 6 7 8 9 10 11 12])
                {nil [1 2 3] 4 [5 6 7] 8 [9 10 11] 12 []})))

(deftest converting-keys
         (is (= (convert-keys #(* 2 %) {1 "a" 2 "b" 3 "c"})
                {2 "a" 4 "b" 6 "c"})))
