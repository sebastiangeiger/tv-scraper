(ns tv-scraper.wikipedia.parser
  (:require [clojure.string :refer [blank?]] ))

(def tokens
  {"{{"    :template-start
   "|"     :pipe
   "}}"    :template-end
   "[["    :something-start
   "]]"    :something-end
   "="     :h1
   "=="    :h2
   "==="   :h3
   "===="  :h4
   "=====" :h5})
(def start-end-regex #"^(.*)\-(end|start)$")

(defn tag-name [token]
  (keyword (nth (re-matches start-end-regex (name token)) 1 (name token))))

(defn starts-with? [string prefix]
  (let [beginning (subs string 0 (min (count string) (count prefix)))]
    (= beginning prefix)))

(defn any-start-with? [coll key]
  (> (count (filter #(starts-with? % key) (keys coll))) 0))

(defn one-starts-with? [coll key]
  (= (count (filter #(starts-with? % key) (keys coll))) 1))

(defn none-start-with? [coll key]
  (= (count (filter #(starts-with? % key) (keys coll))) 0))

;; TODO: tokens could be nicely curried, then just pass in memory and joined
(defn ^:private substitute-tokens [memory current]
  (let [joined (str memory current)
        current (str current)]
    (cond
      (and
        (contains? tokens current)
        (one-starts-with? tokens current))  ["" [memory (tokens current)]]
      (and
        (contains? tokens memory)
        (none-start-with? tokens joined)) [current [(tokens memory)]]
      (and
        (none-start-with? tokens memory)
        (any-start-with? tokens current)) [current [memory]]
      :else                               [joined []])))

(defn conj-concat [a b]
  "Recursively uses conj to concat lists a and b.
   Usage: (conj-concat [1 2] [3 4 5]) #=> [1 2 3 4 5]
   Better than concat in some cases, especially when you don't want lazy lists."
  (loop [a          a
         [head & b] b]
    (if (nil? head)
      a
      (recur (conj a head) b))))

(defn tokenize-step [memory [current & remainder] result]
  (let [[new-memory result-addition] (substitute-tokens memory current)
        result-addition (remove #(= "" %) result-addition)]
    (if (nil? current)
      ["" "" (conj-concat result [(or (tokens memory) memory)])] ;;Forcing evaluation in last step
      [new-memory remainder (conj-concat result result-addition)])))

(defn tokenize [text]
  (loop [memory ""
         text   text
         result []]
    (if (and (empty? text) (empty? memory))
      result
      (let [[memory' text' result']
            (tokenize-step memory text result)]
        (recur memory' text' result')))))

(defn is-token? [token]
  (contains? (set (vals tokens)) token))

(defn debug-print [value]
  (do
    (prn value)
    value))

(defn are-start-end-pair? [start end]
  (let [regex         #(->> % name (re-matches start-end-regex))
        start-matches (regex start)
        end-matches   (regex end)]
    (and
      (-> start-matches last (= "start"))
      (-> end-matches   last (= "end"))
      (= (nth start-matches 1)
         (nth end-matches   1)))))

(defn is-closing? [open-tag candidate]
  (and
    open-tag
    (= (tag-name open-tag) (tag-name candidate))
    (or
      (= open-tag candidate)
      (are-start-end-pair? open-tag candidate))))


(defn parse-helper [result [current & remainder :as tokens] open-tag]
  (cond
    ;; End condition
    (empty? tokens)
    [result tokens open-tag]

    ;; Closing the current level
    (and (is-token? current) (is-closing? open-tag current))
    [result remainder open-tag]

    ;; Opening a new level, then integrating the subresult
    (is-token? current)
    (let
      [[subresult remainder'] (parse-helper [] remainder current)
       tag                    (tag-name current)
       result'                (conj result {tag {:content subresult}})]
       (parse-helper result' remainder' open-tag))

    :else
    (parse-helper (conj result current) remainder open-tag)))

(defn parse [tokens]
  (first (parse-helper [] tokens nil)))
