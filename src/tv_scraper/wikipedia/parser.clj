(ns tv-scraper.wikipedia.parser)

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
        (one-starts-with? tokens current)) (do (println 1) ["" [memory (tokens current)]])
      (and
        (contains? tokens memory)
        (none-start-with? tokens joined)) (do (println 2) [current [(tokens memory)]])
      (and
        (none-start-with? tokens memory)
        (any-start-with? tokens current)) (do (println 3) [current [memory]])
      :else                                (do (println 4) [joined []]))))

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
