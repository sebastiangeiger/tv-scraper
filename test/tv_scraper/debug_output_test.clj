(ns tv-scraper.debug-output-test
  (:require [clojure.test :refer :all]
            [tv-scraper.debug-output :refer :all]
            [tv-scraper.debug-output.colorize :refer :all]))

(deftest testing-strip-color
         (is (= (uncolorize (str \u001b "[31m" "Some Color" \u001b "[0m")) "Some Color"))
         (is (= (uncolorize (str \u001b "[31;1m" "Some Color" \u001b "[0m")) "Some Color")))

(deftest testing-report-difference
         (is (= (uncolorize (report-difference {:1 :2} {:3 :4}))
                "<{:1 :2}|{:3 :4}>"))
         (is (= (uncolorize (report-difference {:1 :2} {:1 :3}))
                "{:1 <:2|:3>}"))
         (is (= (report-difference {:1 :2} {:1 :3})
                (str "{:1 <" \u001b "[32;1m:2" \u001b "[0m|" \u001b "[31;1m:3" \u001b "[0m>}")))
         (is (= (uncolorize (report-difference {:1 :2} {:1 :2})) "{:1 :2}"))
         (is (= (uncolorize (report-difference {:1 :2 :3 :4} {:1 :3 :3 :4}))
                "{:1 <:2|:3> :3 :4}"))
         (is (= (uncolorize (report-difference {:1 :2} {}))
                "<{:1 :2}|{}>"))
         (is (= (uncolorize (report-difference {:1 :2} {:1 nil}))
                "{:1 <:2|>}"))
         (is (= (uncolorize (report-difference {} {:1 :2}))
                "<{}|{:1 :2}>"))
         (is (= (uncolorize (report-difference {:1 {:2 :3}} {:1 {:2 :4}}))
                "{:1 {:2 <:3|:4>}}"))
         (is (= (uncolorize (report-difference {:1 {:2 :3}} {:1 {:3 :4}}))
                "{:1 <{:2 :3}|{:3 :4}>}")))

(deftest testing-maps
         (is (maps? {}))
         (is (maps? {} {:a :b}))
         (is (not (maps? {} {:a :b} 1))))
