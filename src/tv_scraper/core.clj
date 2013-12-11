(ns tv-scraper.core)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn flat? [element]
  (or (not (map? element)) (not (reduce #(or %1 %2) (map map? (vals element))))))

(defn paths-through [nested-map]
  (let [merge-in (fn [element more-elements]
                   (if (empty? more-elements)
                     [[element]]
                     (map #(cons element %) more-elements)))]
    (if (flat? nested-map)
      #{}
      (->> nested-map (mapcat (fn [[k v]] (merge-in k (paths-through v)))) set))))

(defn report-difference [map-1 map-2]
  (defn map-diff [k map-1 map-2]
    (let [v-1 (k map-1)
          v-2 (k map-2)]
      (if (and (contains? map-1 k) (contains? map-2 k) )
        (if (= v-1 v-2)
          (str k " " v-1)
          (str k " <" v-1 "|" v-2 ">"))
        (if (contains? map-1 k)
          (str "<" k " " v-1 "|>")
          (str "<|" k " " v-2 ">")))))
  (let [all-keys (clojure.set/union (set (keys map-1)) (set (keys map-2)))
        fragments (map #(map-diff % map-1 map-2) all-keys)]
    (str "{" (clojure.string/join " " fragments) "}")))

(defn episode-list [show]
  (let [episodes-only #(and (= (first %) :seasons) (= (nth % 2) :episodes))]
    (->> show paths-through (filter episodes-only)
      (map #(merge (apply hash-map %) (get-in show %)) ,,,)
      set)))
