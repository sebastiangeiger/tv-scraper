(ns tv-scraper.debug-output.colorize)

(defn uncolorize [string]
  (let [pattern (re-pattern (str \u001b "\\[\\d{1,2}(;\\d{1,2})*m"))]
    (clojure.string/replace string pattern "")))

(defn colorize [string color]
  (str \u001b "[" color string \u001b "[0m"))

(defn green [string]
  (colorize string "32;1m"))

(defn red [string]
  (colorize string "31;1m"))


(ns tv-scraper.debug-output
  (:require [clojure.set :refer [union]])
  (:refer clojure.string :only [join] :rename {join str-join})
  (:require [tv-scraper.debug-output.colorize :refer [red green]]))

(defn ^:private mark-diff [a b]
  (let [f #(str-join " " %)]
    (str "<" (green (f a))  "|" (red (f b)) ">")))

(defn ^:private map-diff [k map-1 map-2]
  (let [v-1 (k map-1)
        v-2 (k map-2)]
    (if (and (contains? map-1 k) (contains? map-2 k) )
      (if (= v-1 v-2)
        (str k " " v-1)
        (str k " " (mark-diff [v-1] [v-2])))
      (if (contains? map-1 k)
        (mark-diff [k v-1] [])
        (mark-diff [] [k v-2])))))

(defn report-difference [a b]
  (let [all-keys (union (set (keys a)) (set (keys b)))
        fragments (map #(map-diff % a b) all-keys)]
    (str "{" (str-join " " fragments) "}")))
