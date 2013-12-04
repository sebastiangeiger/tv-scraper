(ns tv-scraper.core)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(ns tv-scraper.epiguides
  (:use [clojure.string :only [replace lower-case] :rename {replace str-replace}])
  (:import java.net.URL)
  (:use tv-scraper.poor-mans-mechanize.form)
  (:use net.cgrand.enlive-html))

(defn interpret-search-results [html-content]
  (let [results (select html-content [:#search [:li :.g]])
        title-regex #(last (re-matches #"^(.*) \(a Titles \& Air Dates Guide\) \- Epguides\.com$" %))
        extract-title #(-> % (select [:h3]) first text title-regex)
        extract-url #(-> % (select [:h3 :a]) first :attrs :href)]
    (map (fn [x] {:title (extract-title x) :url (extract-url x)}) results)))

(defn search-results-for [show-name]
  (let [search-form (-> "http://epguides.com/" URL. html-resource (select [:body :form]) first)
        submit-button [[(attr= :type "submit") (attr= :name "btnG")]]]
    (interpret-search-results (submit-form (set-value search-form {:#googleSearch show-name}) submit-button))))

(defn find-show-url [show-name]
  (get (first (search-results-for show-name)) :url))

(defn split-by-newlines [array]
  (let [split #(if (string? %) (clojure.string/split-lines %) [%])]
    (remove #(and (string? %) (clojure.string/blank? %)) (mapcat split array))))

(defn correct-first [pred [first-key first-value & remainder :as array]]
    (if (pred first-key)
      array
      (concat (vector nil (cons first-key first-value)) remainder)))

(defn split-on
  ([pred coll] (split-on pred coll []))
  ([pred [current & coll] array]
   (let [[fitting remaining] (split-with #(not (pred %)) coll)]
     (if (nil? current)
       (correct-first pred array)
       (recur pred remaining (concat array [current] [fitting]))))))

(defn split-map [pred coll]
  (apply hash-map (split-on pred coll)))

(defn convert-keys [f the-map]
  (zipmap (map f (keys the-map)) (vals the-map)))

(defn split-into-seasons [page-data extract-season]
  (let [cleaned-up (-> page-data first :content split-by-newlines)
        season-number #(-> % extract-season lower-case (str-replace " " "_") keyword)
        season-headline? #(not (nil? (extract-season %)))
        remove-meta-data #(into {} (for [[k v] % :when (not (nil? k))] [k v]))]
    (convert-keys season-number (remove-meta-data (split-map season-headline? cleaned-up)))))

(defn parse-seasons [page]
  (-> page
    (select [:#eplist :pre])
    (split-into-seasons #(if (string? %) (last (or (re-matches #"^\s*•\s*Season (\d+)\s*$" %) (re-matches #"^(Other Episodes)$" %))) nil))))

(defn parse-show-page [url]
  (let [extract-title #(-> % (select [:#header :h1]) first text)
        page (-> url URL. html-resource)]
    {:title (extract-title page)
     :seasons (parse-seasons page)}))
