(ns tv-scraper.core-test
  (:require [clojure.test :refer :all]
            [tv-scraper.core :refer :all]
            [tv-scraper.imdb]
            [tv-scraper.debug-output :as debug]
            [tv-scraper.epguides]))
(deftest producing-episode-lists
         (let [show {:seasons {:1 {:episodes {:1 {:title "S01E01"}}}
                               :2 {:episodes {:1 {:title "S02E01"}}}}}]
           (is (= (episode-list show)
                  #{{:seasons :1 :episodes :1 :title "S01E01"}
                    {:seasons :2 :episodes :1 :title "S02E01"}})))
         )

(deftest testing-flat?
         (is (flat? {}))
         (is (flat? {:hello 'world}))
         (is (flat? [{:hello 'world}{:and 'something}]))
         (is (not (flat? {:hello {:beautiful 'world} :and 'something})))
         )

(deftest testing-paths-through
         (let [show {:seasons {:1 {:episodes {:1 {:title "S01E01"}}}
                               :2 {:episodes {:1 {:title "S02E01"}
                                              :2 {:title "S02E02"}}}}}
               simple {:k1 {:inner :map} :k2 {:something :else}}
               two {:k1 {:k11 {:inner :map} :k12 {:something :else}}
                    :k2 {:k21 {:inner :map} :k22 {:something :else} :k23 {:and :this}}
                    :k3 {:k31 {:only :this}}}]
           (is (= (paths-through {:inner :map}) #{}))
           (is (= (paths-through simple) #{[:k1][:k2]}))
           (is (= (paths-through two) #{[:k1 :k11][:k1 :k12]
                                        [:k2 :k21][:k2 :k22][:k2 :k23]
                                        [:k3 :k31]}))
           (is (= (paths-through show) #{[:seasons :1 :episodes :1]
                                         [:seasons :2 :episodes :1]
                                         [:seasons :2 :episodes :2]}))))

(deftest checking-if-they-produce-the-same-results
  (let [imdb (-> "http://www.imdb.com/title/tt0805663" tv-scraper.imdb/parse-show-page)
        epguides (-> "http://epguides.com/Jericho/"    tv-scraper.epguides/parse-show-page)]
         (is (= epguides imdb) (debug/report-difference epguides imdb))))
