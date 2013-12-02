(ns tv-scraper.core)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(ns tv-scraper.epiguides
  (:use clojure.test)
  (:use net.cgrand.enlive-html)
  (:import java.net.URL)
  (:import java.net.URLEncoder))

;; To be extracted into a library
(defn input-stream [url]
  (-> url URL. .openConnection
    (doto (.setRequestProperty "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.57 Safari/537.36"))))

(defn set-value [form-tag change-instruction]
  (let [the-key (key (first change-instruction))
        new-value (val (first change-instruction))]
    (first (at form-tag
               [the-key] (set-attr :value new-value)))))

(defn extract-key-values [form-tag]
  (let [input-elements (remove #(= (-> % :attrs :type) "submit") (-> form-tag (select [:input])))]
    (map (fn [x] {(keyword (-> x :attrs :name)) (-> x :attrs :value)}) input-elements)))

(defn encode-params [values]
  (let [encode #(URLEncoder/encode % "UTF-8")
        to-segments #(map (fn [[k v]] (str (encode (name k)) "=" (encode v))) %)]
    (reduce #(str %1 "&" %2) (mapcat to-segments values))))

(defn encode-get-url [form-tag params]
  (let [base-url (-> form-tag :attrs :action)]
    (str base-url "?" (encode-params params))))

(defn follow-redirects [stream]
  (let [status (.getHeaderField stream 0)]
    (if (re-find #"302" status)
      (let [new-url (.getHeaderField stream "location")]
        (recur (input-stream new-url)))
      (.getContent stream))))

(defn submit-form [form-tag submit-selector]
  {:pre [(= (-> form-tag :tag) :form)
         (= (-> form-tag :attrs :method) "get")
         (not (empty? (select form-tag submit-selector)))]}
  (let [url (encode-get-url form-tag (extract-key-values form-tag))]
    (with-open [stream (follow-redirects (input-stream url))]
      (html-resource stream))))

;; Implementation
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
