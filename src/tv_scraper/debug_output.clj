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
  (:require [tv-scraper.core :refer [flat?]])
  (:require [clojure.set :refer [union intersection]])
  (:refer clojure.string :only [join] :rename {join str-join})
  (:require [tv-scraper.debug-output.colorize :refer [red green]]))

(defn ^:private mark-diff [a b]
  (let [f #(str-join " " %)]
    (str "<" (green (f a))  "|" (red (f b)) ">")))

(defn keys-in-common? [a b]
  (and (map? a)
       (map? b)
       (seq (intersection (set (keys a)) (set (keys b))))))

(defn maps? [& args]
  (->> args (map map?) (reduce #(and %1 %2))))

(defn report-difference [a b]
  (if (maps? a b)
    (if (keys-in-common? a b)
      (let [all-keys (union (set (keys a)) (set (keys b)))
            values   (map #(report-difference (% a) (% b)) all-keys)]
        (str "{" (str-join " " (mapcat vector all-keys values)) "}"))
      (mark-diff [a] [b]))
    (if (= a b)
      a
      (mark-diff [a] [b]))))
