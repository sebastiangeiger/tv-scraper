(ns tv-scraper.poor-mans-mechanize-test
  (:require [clojure.test :refer :all]
            [tv-scraper.poor-mans-mechanize.form :refer :all]
            [tv-scraper.poor-mans-mechanize.network :refer :all]))

(deftest setting-value-in-a-field
         (is (=
               (set-value
                 {:tag :form, :content [{:tag :input, :attrs {:size "40", :value "", :id "googleSearch", :name "q", :type "text"}, :content []}]}
                 {:#googleSearch "The Wire"})
               {:tag :form, :content [{:tag :input, :attrs {:size "40", :value "The Wire", :id "googleSearch", :name "q", :type "text"}, :content []}]})))

(deftest encoding-urls
         (is (=
               (url-for "http://www.imdb.com/find" {:q "The Wire" :s "tt"} {:ttype "tv"})
               "http://www.imdb.com/find?q=The+Wire&s=tt&ttype=tv"))
         (is (=
               (url-for "http://www.imdb.com/find")
               "http://www.imdb.com/find")))

(deftest encoding-params
         (is (= (encode-params []) nil))
         (is (=
               (encode-params [{:hl "en"} {:q "allintitle:" :test "more"} {:q "site:epguides.com"} {:q "The Wire"}])
               "hl=en&test=more&q=allintitle%3A&q=site%3Aepguides.com&q=The+Wire")))

(deftest extracting-keys-and-values-from-a-form
         (is (=
               (extract-key-values {:tag :form, :content [{:tag :input, :attrs {:value "en", :name "hl", :type "hidden"}, :content nil} {:tag :input, :attrs {:value "allintitle:", :name "q", :type "hidden"}, :content nil} {:tag :input, :attrs {:value "site:epguides.com", :name "q", :type "hidden"}, :content nil} {:tag :input, :attrs {:size "40", :value "", :id "googleSearch", :name "q", :type "text"}, :content nil} {:tag :input, :attrs {:value "Search", :name "btnG", :type "submit"}, :content nil}]})
               [{:hl "en"} {:q "allintitle:"} {:q "site:epguides.com"} {:q ""}])))

