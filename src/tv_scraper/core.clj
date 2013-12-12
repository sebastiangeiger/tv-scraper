(ns tv-scraper.core)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn flat? [element]
  (or
    (empty? element)
    (not (map? element))
    (not (reduce #(or %1 %2) (map map? (vals element))))))

(defn paths-through [nested-map]
  (let [merge-in (fn [element more-elements]
                   (if (empty? more-elements)
                     [[element]]
                     (map #(cons element %) more-elements)))]
    (if (flat? nested-map)
      #{}
      (->> nested-map (mapcat (fn [[k v]] (merge-in k (paths-through v)))) set))))

(defn episode-list [show]
  (let [episodes-only #(and (= (first %) :seasons) (= (nth % 2) :episodes))]
    (->> show paths-through (filter episodes-only)
      (map #(merge (apply hash-map %) (get-in show %)) ,,,)
      set)))
