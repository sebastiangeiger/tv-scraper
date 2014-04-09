(ns tv-scraper.wikipedia.parser
  (:require [clojure.string :refer [blank? join] :rename {join str-join}] ))

(def tokens
  {"{{" "{{"
   "}}" "}}"
   "[[" "[["
   "]]" "]]"
   "{|" "{|"
   "|}" "|}"
   "=" "="
   "==" "=="
   "===" "==="
   "====" "===="
   "=====" "====="})

(def restrictions
  {"=" #(or (= % nil) (= % "="))})

(def names
  {"{{"    :template-start
   "}}"    :template-end
   "[["    :something-start
   "]]"    :something-end
   "{|"    :table-start
   "|}"    :table-end
   "="     :h1
   "=="    :h2
   "==="   :h3
   "===="  :h4
   "=====" :h5})

(def pairs
  {"{{"    "}}"
   "[["    "]]"
   "{|"    "|}"
   "="     "="
   "=="    "=="
   "==="   "==="
   "===="  "===="
   "=====" "====="})

(def start-end-regex #"^(.*)\-(end|start)$")

(defn tag-name [token]
  (let [tag-part-name (name (get names token))]
    (keyword (nth (re-matches start-end-regex tag-part-name) 1 tag-part-name))))

(defn starts-with? [string prefix]
  (let [beginning (subs string 0 (min (count string) (count prefix)))]
    (= beginning prefix)))

(defn any-start-with? [coll key]
  (> (count (filter #(starts-with? % key) (keys coll))) 0))

(defn one-starts-with? [coll key]
  (= (count (filter #(starts-with? % key) (keys coll))) 1))

(defn none-start-with? [coll key]
  (= (count (filter #(starts-with? % key) (keys coll))) 0))

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

(defn is-token? [token open-tag]
  (and
    (contains? (set (vals tokens)) token)
    (or
      (nil? (get restrictions token))
      ((get restrictions token) open-tag))))

(defn debug-print [value]
  (do
    (prn value)
    value))

(defn are-start-end-pair? [start end]
  (= (get pairs start) end))

(defn is-closing? [open-tag candidate]
  (and
    open-tag
    (are-start-end-pair? open-tag candidate)))

(defn parse-helper [result [current & remainder :as tokens] open-tag]
  (cond
    ;; End condition
    (empty? tokens)
    (if (nil? open-tag)
      [result tokens open-tag]
      (throw (Exception. (str "Open tag not closed: " open-tag)))
      )

    ;; Closing the current level
    (and (is-token? current open-tag) (is-closing? open-tag current))
    [result remainder open-tag]

    ;; Opening a new level, then integrating the subresult
    (is-token? current open-tag)
    (let
      [[subresult remainder'] (parse-helper [] remainder current)
       tag                    (tag-name current)
       result'                (conj result {tag {:content subresult}})]
       (parse-helper result' remainder' open-tag))

    :else
    (parse-helper (conj result current) remainder open-tag)))

(defn join-consecutive-strings [array]
  "Steps through the entire tree and joins consecutive strings whenever it
  encounters them in an array.
  The fundamental structure of the tree remains intact, only arrays are
  changed and then inserted at their old position.

  Example: (replace quotes for valid syntax)
  (join-consecutive-strings [{:this {:is ['a ' 'nested ' 'string']}} 'this ' 'not'])
  #=> [{:this {:is ['a nested string']}} 'this not']"

  (defn integrate [a b]
    (let [mid (last a)
          dle (first b)]
      (if (and (string? mid) (string? dle))
        (concat (butlast a) [(str-join [mid dle])] (rest b))
        (concat a b))))
  (defn treat-map [m f]
    (into {} (for [[k v] m] [k (f v)])))

  (cond
    ;; Divide and conquer
    (> (count array) 1)
    (reduce integrate (map join-consecutive-strings (split-at (/ (count array) 2) array)))
    (and (= (count array) 1) (map? array))
    (treat-map array join-consecutive-strings)
    (and (= (count array) 1) (map? (first array)))
    [(treat-map (first array) join-consecutive-strings)]
    :else
    array))

(defn parse [tokens]
  (join-consecutive-strings (first (parse-helper [] tokens nil))))
