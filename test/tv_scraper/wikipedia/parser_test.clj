(ns tv-scraper.wikipedia.parser-test
  (:require [clojure.test :refer :all]
            [tv-scraper.wikipedia.parser :refer :all]))

(deftest test-tokenize
         (is (= (tokenize "{{name|args}}")
                [:template-start "name" :pipe "args" :template-end])))

(deftest test-tokenize-step
         (is (= (tokenize-step "" "{{name|args}}" [])
                ["{" "{name|args}}" []]))
         (is (= (tokenize-step "{" "{name|args}}" [])
                ["" "name|args}}" [:template-start]]))
         (is (= (tokenize-step "name" "|args}}" [:template-start])
                ["" "args}}" [:template-start "name" :pipe]]))
         (is (= (tokenize-step "args" "}}" [:template-start "name" :pipe])
                ["}" "}" [:template-start "name" :pipe "args"]]))
         (is (= (tokenize-step "}" "}" [:template-start "name" :pipe "args"])
                ["" "" [:template-start "name" :pipe "args" :template-end]]))
         )

(deftest test-any-start-with
         (is (not (starts-with? "bcd" "a")))
         (is (starts-with? "bcd" "b")))
