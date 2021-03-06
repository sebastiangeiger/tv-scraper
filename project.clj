(defproject tv-scraper "0.1.0-SNAPSHOT"
  :description "Scraping TV Websites to get an authorative database"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main tv-scraper.core
  :test-selectors {:default (complement :google)
                   :google :google
                   :wip :wip
                   :failing :failing
                   :all (constantly true)}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [enlive "1.1.4"]
                 [clj-time "0.6.0"]])
