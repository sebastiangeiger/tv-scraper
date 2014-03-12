(ns tv-scraper.wikipedia.parser-test
  (:require [clojure.test :refer :all]
            [tv-scraper.wikipedia-test :refer [load-wikitext]]
            [tv-scraper.wikipedia.parser :refer :all]))

(deftest ^:wip test-tokenize
         (is (= (tokenize "single-word")
                ["single-word"]))
         (is (= (tokenize "{{name|args}}")
                [:template-start "name" :pipe "args" :template-end]))
         (is (= (tokenize "{{DISPLAYTITLE:List of ''Jericho'' episodes}}")
                [:template-start "DISPLAYTITLE:List of ''Jericho'' episodes" :template-end]))
         (is (= (tokenize "and {{this is a|\"test\"}}?")
                ["and " :template-start "this is a" :pipe "\"test\"" :template-end "?"]))
         (is (= (tokenize "=Headline=")
                [:h1 "Headline" :h1]))
         (is (= (tokenize "==Headline==")
                [:h2 "Headline" :h2]))
         ;; Just checking these don't throw an error
         (is (tokenize "[[File:Jericho.tvseries.jpg|275px|thumb|The ''Jericho'' intertitle, written in static[[wikt:-esque|esque]] font, is accompanied by [[Morse code]] specific to each episode.|alt=The word \"Jericho\" in a gray/black font that looks like static on a black background.]]"))
         (is (-> "list_of_jericho_episodes" load-wikitext tokenize))
         )

(deftest test-tokenize-step
         (defn tokenize-step* [memory text result]
           (let [[memory' text' result'] (tokenize-step memory text result)]
             [memory' (apply str text') result']))

         (is (= (tokenize-step* "name" "|args}}" [:template-start])
                ["" "args}}" [:template-start "name" :pipe]]))
         (is (= (tokenize-step* "" "==Headline==" [])
                ["=" "=Headline==" []]))
         (is (= (tokenize-step* "" "{{name|args}}" [])
                ["{" "{name|args}}" []]))
         (is (= (tokenize-step* "{" "{name|args}}" [])
                ["{{" "name|args}}" []]))
         (is (= (tokenize-step* "{{" "name|args}}" [])
                ["n" "ame|args}}" [:template-start]]))
         (is (= (tokenize-step* "args" "}}" [:template-start "name" :pipe])
                ["}" "}" [:template-start "name" :pipe "args"]]))
         (is (= (tokenize-step* "}" "}" [:template-start "name" :pipe "args"])
                ["}}" "" [:template-start "name" :pipe "args"]]))
         (is (= (tokenize-step* "}}" "" [:template-start "name" :pipe "args"])
                ["" "" [:template-start "name" :pipe "args" :template-end]]))
         (is (= (tokenize-step* "b" "" ["a" :pipe])
                ["" "" ["a" :pipe "b"]]))
         (is (= (tokenize-step* "=" "Headline=" [])
                ["H" "eadline=" [:h1]]))
         (is (= (tokenize-step* "=" "=Headline==" [])
                ["==" "Headline==" []]))
         (is (= (tokenize-step* "==" "Headline==" [])
                ["H" "eadline==" [:h2]]))
         )

(deftest test-any-start-with
         (is (not (starts-with? "bcd" "a")))
         (is (starts-with? "bcd" "b")))
