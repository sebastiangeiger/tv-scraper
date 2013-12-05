(ns tv-scraper.poor-mans-mechanize.network
  (:import java.net.URL)
  (:import java.net.URLEncoder))

(defn input-stream [url]
  (-> url URL. .openConnection
    (doto (.setRequestProperty "User-Agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.57 Safari/537.36"))))

(defn encode-params [values]
  (let [encode #(URLEncoder/encode % "UTF-8")
        to-segments #(map (fn [[k v]] (str (encode (name k)) "=" (encode v))) %)]
    (reduce #(str %1 "&" %2) (mapcat to-segments values))))

(defn url-for [base-url & maps]
  (str base-url "?" (encode-params maps)))

(defn follow-redirects [stream]
  (let [status (.getHeaderField stream 0)]
    (if (re-find #"302" status)
      (let [new-url (.getHeaderField stream "location")]
        (recur (input-stream new-url)))
      (.getContent stream))))
