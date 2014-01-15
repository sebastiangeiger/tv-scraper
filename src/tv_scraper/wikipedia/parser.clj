(ns tv-scraper.wikipedia.parser)

(def tokens
  {"{{" :template-start
   "|"  :pipe
   "}}" :template-end
   "[[" :something-start
   "]]" :something-end})

(defn starts-with? [string prefix]
  (let [beginning (subs string 0 (min (count string) (count prefix)))]
    (= beginning prefix)))

(defn any-start-with? [coll prefix]
  (some #(starts-with? % prefix) coll))

(defn ^:private substitute-tokens [memory current]
  (let [joined (str memory current)
        current (str current)]
    (cond
      (contains? tokens current)    ["" [memory (tokens current)]]
      (contains? tokens joined)     ["" [(tokens joined)]]
      (any-start-with? (keys tokens) current) [current [memory]]
      :else [joined []])))

(defn tokenize-step [memory [current & remainder] result]
  (let [[new-memory result-addition] (substitute-tokens memory current)
        result-addition (remove #(= "" %) result-addition)]
    [new-memory (apply str remainder) (concat result result-addition)]))

(defn tokenize [text]
  (loop [memory "" text text result []]
    (if (empty? text)
      (if (empty? memory)
        result
        (concat result [memory]))
      (let [[new-memory new-text new-result] (tokenize-step memory text result)]
        (recur new-memory new-text new-result)))))