(ns tv-scraper.poor-mans-mechanize.form
  (:use tv-scraper.poor-mans-mechanize.network)
  (:use [net.cgrand.enlive-html :only [at select set-attr html-resource]]))

(defn extract-key-values [form-tag]
  (let [input-elements (remove #(= (-> % :attrs :type) "submit") (-> form-tag (select [:input])))]
    (map (fn [x] {(keyword (-> x :attrs :name)) (-> x :attrs :value)}) input-elements)))

(defn set-value [form-tag change-instruction]
  (let [the-key (key (first change-instruction))
        new-value (val (first change-instruction))]
    (first (at form-tag
               [the-key] (set-attr :value new-value)))))

(defn encode-get-url [form-tag params]
  (let [base-url (-> form-tag :attrs :action)]
    (str base-url "?" (encode-params params))))

(defn submit-form [form-tag submit-selector]
  {:pre [(= (-> form-tag :tag) :form)
         (= (-> form-tag :attrs :method) "get")
         (not (empty? (select form-tag submit-selector)))]}
  (let [url (encode-get-url form-tag (extract-key-values form-tag))]
    (with-open [stream (follow-redirects (input-stream url))]
      (html-resource stream))))

