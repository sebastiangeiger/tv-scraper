(ns tv-scraper.collection-helper)
(defn ^:private correct-first [pred [[first-key first-values] & remainder :as array]]
    (if (pred first-key)
      array
      (cons [nil (cons first-key first-values)] remainder)))

(defn split-on
  ([pred coll] (split-on pred coll []))
  ([pred [current & coll] array]
   (let [[fitting remaining] (split-with #(not (pred %)) coll)]
     (if (nil? current)
       (correct-first pred array)
       (recur pred remaining (conj array [current fitting]))))))

(defn split-map [pred coll]
  (apply hash-map (apply concat (split-on pred coll))))

(defn convert-keys [f the-map]
  (zipmap (map f (keys the-map)) (vals the-map)))

