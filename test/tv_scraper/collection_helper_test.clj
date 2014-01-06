(ns tv-scraper.collection-helper-test
  (:require [clojure.test :refer :all]
            [tv-scraper.collection-helper :refer :all]))

(deftest splitting-an-array-into-a-subarrays
         (is (= (split-on #(= (rem % 4) 5) [4 5 6 7 8 9 10 11 12])
                [[nil [4 5 6 7 8 9 10 11 12]]]))
         (is (= (split-on #(= (rem % 4) 0) [4 5 6 7 8 9 10 11 12])
                [[4 [5 6 7]] [8 [9 10 11]] [12 []]]))
         (is (= (split-on #(= (rem % 4) 0) [])
                []))
         (is (= (split-on #(= (rem % 4) 0) [1 2 3 4 5 6 7 8 9 10 11 12])
                [[nil [1 2 3]] [4 [5 6 7]] [8 [9 10 11]] [12 []]])))

(deftest splitting-an-array-into-a-map
         (is (= (split-map #(= (rem % 4) 0) [4 5 6 7 8 9 10 11 12])
                {4 [5 6 7] 8 [9 10 11] 12 []}))
         (is (= (split-map #(= (rem % 4) 0) [1 2 3 4 5 6 7 8 9 10 11 12])
                {nil [1 2 3] 4 [5 6 7] 8 [9 10 11] 12 []})))

(deftest converting-keys
         (is (= (convert-keys #(* 2 %) {1 "a" 2 "b" 3 "c"})
                {2 "a" 4 "b" 6 "c"})))
