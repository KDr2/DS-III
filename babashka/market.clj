(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {org.babashka/http-client {:mvn/version "0.3.11"}}})

(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(require '[babashka.http-client :as http])
(require '[cheshire.core :as json])
(import clojure.lang.ExceptionInfo)

(def licence-premium
  (-> (str (System/getenv "HOME") "/.config/byapi.key") slurp str/trim))
(def licence licence-premium)

(def api-prefix "n") ;; api/n/b

;; API End-Points
(def url-stock-list (str "https://" api-prefix ".biyingapi.com/hslt/list/" licence))

(defn url-norm-history [code]
  (str "https://" api-prefix ".biyingapi.com/hszbl/fsjy/" code "/dq/" licence))

(defn url-macd-history [code]
  (str "https://" api-prefix ".biyingapi.com/hszbl/macd/" code "/dq/" licence))

(defn url-boll-history [code]
  (str "https://" api-prefix ".biyingapi.com/hszbl/boll/" code "/dq/" licence))

;; Utilities
(defn api-data
  ([url] (api-data url nil))
  ([url checker]
   (let [data (try (-> url (http/get) (:body) (json/parse-string) (reverse))
                   (catch ExceptionInfo e
                     (let [ed (.getData e)
                           status (:status ed 0)]
                       (if (not= status 404) (.println *err* (str "Error: get data from " url " code:" status))))
                     [{}])
                   (catch Exception e
                     (.println *err* (str "Error: get data from " url " E:" e))
                     [{}]))]
     (if checker
       (if (checker data) data [{}])
       data))))

(defn get-num [data key]
  (get data key 0.0))

(defn stock-info [info]
  (str (get info "mc") ": https://xueqiu.com/S/" (get info "jys") (get info "dm")))

(declare latest-date)
(defn date-checker-0 [key]
  #(= (get (first %) key) @latest-date))
(def date-checker (memoize date-checker-0))

;; Basic data
(def all-stocks
  (delay
    (-> url-stock-list (http/get) (:body) (json/parse-string))))

(defn norm-data [code]
  (-> code (url-norm-history) (api-data (date-checker "d"))))

(def latest-date
  (delay (-> "000001" (url-norm-history) (api-data) (first) (get "d"))))

;; normal pred
(defn pred [stock & rest]
  (let [preds (map (fn [p] (if (seq? p) #((first p) % (second p)) p)) rest)]
    ((apply every-pred preds) stock)))

(defn name-pred [stock]
  (let [name (get stock "mc")]
    (and (not (str/includes? name "ST"))
         (not (str/includes? name "退")))))

;; MACD deep-turn
(defn macd-data [code]
  (-> code (url-macd-history) (api-data (date-checker "t"))))

(defn macd-diff [macd] (get-num macd "diff"))

(defn deep-turn
  ([values]
   (and (> (count values) 30) ;; at least has data of 30 days
        (< (nth values 2) 0) ;; diff 2 days ago < 0
        (let [dmax (apply max values)
              dmin (apply min values)
              drop (- (nth values 1) (nth values 5)) ;; 4-day drop
              inc1 (- (nth values 0) (nth values 1))]
          (and (< drop 0)
               (< 0 inc1)
               (> (abs drop) (* 0.15 (abs (- dmax dmin))))))))
  ([values ndays-ago] (deep-turn (drop ndays-ago values))))

(defn macd-pred-0 [data ndays indicator-fn predictor]
  (->> data (take ndays) (map indicator-fn) (predictor)))

(defn macd-pred
  ([stock] (macd-pred stock 0))
  ([stock ndays-ago]
   (-> stock (get "dm") (macd-data) (macd-pred-0 365 macd-diff #(deep-turn % ndays-ago)))))

;; BOLL red drill-down
(defn boll-data [code]
  (-> code (url-boll-history) (api-data (date-checker "t"))))

(defn boll-drill-down
  ([norm boll] (boll-drill-down norm boll 0))
  ([norm boll ndays-ago]
   (let [n1 (nth norm ndays-ago {})
         b1 (nth boll ndays-ago {})
         n2 (nth norm (+ 1 ndays-ago) {})
         b2 (nth boll (+ 1 ndays-ago) {})
         n2-o (get-num n2 "o")
         n2-c (get-num n2 "c")
         b2-d (get-num b2 "d")
         n1-o (get-num n1 "o")
         n1-c (get-num n1 "c")]
     (and (> n2-c n2-o) (< n2-o b2-d) (> n1-c n1-o)))))

(defn boll-pred
  ([stock] (boll-pred stock 0))
  ([stock ndays-ago]
   (let [code (get stock "dm")]
     (boll-drill-down (norm-data code) (boll-data code) ndays-ago))))

;; minmax
(defn fix-zero [z] (if (< z 0.001) 0.001 z))
(defn minmax [stock]
  (let [code (get stock "dm")
        data (norm-data code)
        closep (map #(get-num % "c") data)
        ;; closep (or (nthnext closep 35) [0]) ;; see before 9.25
        cmin (apply min closep)
        cmax (apply max closep)
        recent-max (apply max (take 25 closep))
        curr (nth closep 0 0.0)
        d-total (fix-zero (- cmax cmin))
        d-curr (- curr cmin)
        d-recent (- recent-max curr)]
    (if (and (> (count closep) 200) (< curr 20)
             (< (/ d-curr d-total) 0.1)
             (> (/ d-recent d-total) 0.08)
             )
      (do (print cmin cmax curr)
          true)
      false)))

;; main

(defn thr-all [& f-preds]
  (doall (for [stk @all-stocks]
           (do
             (Thread/sleep 25) ;; 3000 per 60 sec => 1 per 20ms
             (if (apply pred stk f-preds)
               (-> stk (stock-info) (println)))))))
(defn main []
  ;; (thr-all name-pred `(~boll-pred 1))
  (thr-all name-pred `(~macd-pred 0))
  ;; (thr-all name-pred minmax)
  )

(main)
