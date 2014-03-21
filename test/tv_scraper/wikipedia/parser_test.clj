(ns tv-scraper.wikipedia.parser-test
  (:require [clojure.test :refer :all]
            [tv-scraper.wikipedia-test :refer [load-wikitext]]
            [tv-scraper.wikipedia.parser :refer :all]))

(def jericho-image-file "[[File:Jericho.tvseries.jpg|275px|thumb|The ''Jericho'' intertitle, written in static[[wikt:-esque|esque]] font, is accompanied by [[Morse code]] specific to each episode.|alt=The word \"Jericho\" in a gray/black font that looks like static on a black background.]]")

(deftest test-parse
           (is (= (-> "=Heading 1=" tokenize parse)
                  [{:h1 {:content ["Heading 1"]}}]))
           (is (= (-> "=Heading 1= Hello" tokenize parse)
                  [{:h1 {:content ["Heading 1"]}} " Hello"]))
           (is (= (-> "{{DISPLAYTITLE:List of ''Jericho'' episodes}}" tokenize parse)
                  [{:template {:content ["DISPLAYTITLE:List of ''Jericho'' episodes"]}}]))
           (is (= (-> "=Heading 1= This is more text ==Heading 2== And some more" tokenize parse)
                  [{:h1 {:content ["Heading 1"]}} " This is more text " {:h2 {:content ["Heading 2"]}} " And some more"]))
           (is (= (-> jericho-image-file tokenize parse)
                  [{:something {:content ["File:Jericho.tvseries.jpg|275px|thumb|The ''Jericho'' intertitle, written in static"
                                          {:something {:content ["wikt:-esque|esque"]}}
                                          " font, is accompanied by " {:something {:content ["Morse code"]}}
                                          " specific to each episode.|alt=The word \"Jericho\" in a gray/black font that looks like static on a black background."]}}]))
           ;; (is (=
           ;;       (prn (nth (-> "list_of_jericho_episodes" load-wikitext tokenize parse) 2))
                 )

(deftest test-tokenize
         (is (= (tokenize "single-word")
                ["single-word"]))
         (is (= (tokenize "{{name|args}}")
                ["{{" "name|args" "}}"]))
         (is (= (tokenize "{{DISPLAYTITLE:List of ''Jericho'' episodes}}")
                ["{{" "DISPLAYTITLE:List of ''Jericho'' episodes" "}}"]))
         (is (= (tokenize "and {{this is a|\"test\"}}?")
                ["and " "{{" "this is a|\"test\"" "}}" "?"]))
         (is (= (tokenize "=Headline=")
                ["=" "Headline" "="]))
         (is (= (tokenize "==Headline==")
                ["==" "Headline" "=="]))
         ;; Just checking these don't throw an error
         (is (tokenize jericho-image-file))
         (is (-> "list_of_jericho_episodes" load-wikitext tokenize))
         )

(deftest ^:wip test-join-consecutive-strings
         (is (= (join-consecutive-strings [])
                []))
         (is (= (join-consecutive-strings ["single"])
                ["single"]))
         (is (= (join-consecutive-strings ["join " "these"])
                ["join these"]))
         (is (= (join-consecutive-strings ["join " "these " "together"])
                ["join these together"]))
         (is (= (join-consecutive-strings [{:some ["map"]} {:some ["more"]} {:something ["else"]}])
                [{:some ["map"]} {:some ["more"]} {:something ["else"]}]))
         (is (= (join-consecutive-strings ["Join " "this" {:h1 ["not this"]}])
                ["Join this" {:h1 ["not this"]}]))
         (is (= (join-consecutive-strings
                  ["Join " "this" {:h1 ["and " "this " "as well"] :deeper {:nesting {:is ["supported" " as " "well"]}}}])
                ["Join this" {:h1 ["and this as well"] :deeper {:nesting {:is ["supported as well"]}}}]))
         )

(deftest test-is-token?
         (is (is-token? "=" nil))
         (is (not (is-token? "=" "[[")))
         (is (is-token? "[[" nil))
         (is (not (is-token? "[" nil))))

(deftest test-tag-name
         (is (= (tag-name "=") :h1))
         (is (= (tag-name "{{") :template)))

(deftest test-conj-concat
         (is (= (conj-concat [1 2] [3 4]) [1 2 3 4]))
         (is (= (conj-concat [1 2] []) [1 2])))

(deftest test-tokenize-step
         (defn tokenize-step* [memory text result]
           (let [[memory' text' result'] (tokenize-step memory text result)]
             [memory' (apply str text') result']))

         (is (= (tokenize-step* "" "==Headline==" [])
                ["=" "=Headline==" []]))
         (is (= (tokenize-step* "" "{{name|args}}" [])
                ["{" "{name|args}}" []]))
         (is (= (tokenize-step* "{" "{name|args}}" [])
                ["{{" "name|args}}" []]))
         (is (= (tokenize-step* "{{" "name|args}}" [])
                ["n" "ame|args}}" ["{{"]]))
         (is (= (tokenize-step* "args" "}}" ["{{" "name|"])
                ["}" "}" ["{{" "name|" "args"]]))
         (is (= (tokenize-step* "}" "}" ["{{" "name|" "args"])
                ["}}" "" ["{{" "name|" "args"]]))
         (is (= (tokenize-step* "}}" "" ["{{" "name|" "args"])
                ["" "" ["{{" "name|" "args" "}}"]]))
         (is (= (tokenize-step* "b" "" ["a|"])
                ["" "" ["a|" "b"]]))
         (is (= (tokenize-step* "=" "Headline=" [])
                ["H" "eadline=" ["="]]))
         (is (= (tokenize-step* "=" "=Headline==" [])
                ["==" "Headline==" []]))
         (is (= (tokenize-step* "==" "Headline==" [])
                ["H" "eadline==" ["=="]]))
         )

(deftest test-any-start-with
         (is (not (starts-with? "bcd" "a")))
         (is (starts-with? "bcd" "b")))
