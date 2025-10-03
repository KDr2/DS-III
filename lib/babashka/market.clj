(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {org.babashka/http-client {:mvn/version "0.3.11"}}})

(require '[clojure.edn :as edn])
(require '[clojure.string :as str])
(require '[clojure.java.io :as io])
(require '[babashka.http-client :as http])
(require '[cheshire.core :as json])

(import java.util.Date)
(import clojure.lang.ExceptionInfo)

;; Configuration
(def config
  (-> (str (System/getenv "HOME") "/.config/kz-private.edn") slurp edn/read-string :investment))

;; Lark Utilities

(defn lark-clear-records [raw-data]
  (let [data (get-in raw-data ["data" "items"])]
    (->> data
         (map #(get % "fields"))
         (filter #(and (get % "Code") (= (get % "Status") "On")))
         (map #(let [code (-> % (get "Code") first (get "text"))
                     name (-> % (get "Name") first (get "text"))]
                 {:code code :name name :lb (get % "LowerBound") :ub (get % "UpperBound")})))))

(def lark-url-map
  {:root ""
   :tenant-token "/auth/v3/tenant_access_token/internal"
   :msg "/im/v1/messages?receive_id_type=chat_id"
   :stocks "/bitable/v1/apps/Z9cFbVqcOaCJSksgJVWcbTuTncd/tables/tblSB2Fw92iAGRIH/records/search?page_size=100"})

(defprotocol IM
  (url [this key])
  (tenant-token [this])
  (send-msg [this text])
  (target-stocks [this]))

(deftype Lark [api-root url-map]
  IM
  (url [this key]
    (str (.api-root this) (key (.url-map this))))

  (tenant-token [this]
    (let [url (url this :tenant-token)
          resp (http/post url
                          {:headers {:content-type "application/json"}
                           :body (json/encode {:app_id (:feishu-app-id config) :app_secret (:feishu-secret config)})})
          data (-> resp :body json/parse-string)]
      (get data "tenant_access_token")))

  (send-msg [this text]
    (let [msg (json/encode {:text text})
          data {:content msg :msg_type "text" :receive_id "oc_8c02eefd0812b071c152c0d48edd0807"}
          token (tenant-token this)
          url (url this :msg)]
      (http/post url
                 {:headers {:content-type "application/json" :authorization (str "Bearer " token)}
                  :body (json/encode data)})))
  (target-stocks [this]
    (let [token (tenant-token this)
          url (url this :stocks)
          resp (http/post url
                          {:headers {:content-type "application/json" :authorization (str "Bearer " token)}
                           :body (json/encode {})})
          data (-> resp (:body) (json/parse-string))]
      (lark-clear-records data))))

(def lark (delay (->Lark "https://open.feishu.cn/open-apis" lark-url-map)))

;; Biying Utilities

(def by-url-map
  {:licence (:biying-licence config)
   :root "https://n.biyingapi.com"      ; api/n/b
   })

(defmulti mkt-url :type)
(defmethod mkt-url :stk-list [_]
  (str (:root by-url-map) "/hslt/list/" (:licence by-url-map)))
(defmethod mkt-url :rt-index [_]
  (str (:root by-url-map) "/zs/sssj/sh000001/" (:licence by-url-map)))
(defmethod mkt-url :rt-stock [stk]
  (str (:root by-url-map) "/hsrl/ssjy/" (:code stk) "/" (:licence by-url-map)))
(defmethod mkt-url :h-norm [stk]
  (str (:root by-url-map) "/hszbl/fsjy/" (:code stk) "/dq/" (:licence by-url-map)))
(defmethod mkt-url :h-macd [stk]
  (str (:root by-url-map) "/hszbl/macd/" (:code stk) "/dq/" (:licence by-url-map)))
(defmethod mkt-url :h-boll [stk]
  (str (:root by-url-map) "/hszbl/boll/" (:code stk) "/dq/" (:licence by-url-map)))

(defn api-data-raw-retry [url times]
  (try (-> url (http/get) (:body) (json/parse-string) (reverse))
       (catch ExceptionInfo e
         (let [ed (.getData e)
               status (:status ed 0)]
           (if (not= status 404) (.println *err* (str "Error: get data from " url " code:" status))))
         [{}])
       (catch Exception e
         (if (> times 0)
           (api-data-raw-retry url (- times 1))
           (do
             (.println *err* (str "Error: get data from " url " E:" e))
             [{}])))))

(defn api-data
  ([url] (api-data url nil))
  ([url checker]
   ;; (println url)
   (let [data (api-data-raw-retry url 3)]
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
    (-> {:type :stk-list} mkt-url http/get :body json/parse-string)))

(defn norm-data [code]
  (-> {:type :h-norm :code code} mkt-url (api-data (date-checker "d"))))

(def latest-date
  (delay (-> {:type :h-norm :code "000001"} mkt-url api-data first (get "d"))))

;; screen notification
(defn time-near [num-tm margin]
  (let [now (Date.)
        h (.getHours now)
        m (.getMinutes now)
        num-now (+ (* 100 h) m)]
    (< (abs (- num-tm num-now)) margin)))

(defn screen-notify []
  (if (time-near 910 6)
    (send-msg @lark "I am on standby!"))
  (doall (for [stk (target-stocks @lark)]
           (let [data (into {} (-> {:type :rt-stock :code (:code stk)} mkt-url api-data))
                 curr (get-num data "p")
                 sym (cond
                       (<= curr (:lb stk)) "_"
                       (>= curr (:ub stk)) "^"
                       :else "")]
             (if (and (> curr 0.1) (seq sym))
               (send-msg @lark (str sym "[" (:name stk) "(" (:code stk) ")] = " curr "!")))))))

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
  (-> {:type :h-macd :code code} mkt-url (api-data (date-checker "t"))))

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
  (-> {:type :h-boll :code code} mkt-url (api-data (date-checker "t"))))

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
             (> (/ d-recent d-total) 0.08))
      (do (print cmin cmax curr)
          true)
      false)))

(defn thr-all [& f-preds]
  (doall (for [stk @all-stocks]
           (do
             (Thread/sleep 50) ;; 3000 per 60 sec => 1 per 20ms
             (if (apply pred stk f-preds)
               (-> stk (stock-info) (println)))))))

;; test
(defn test [args]
  (println args)
  (println (time-near 1409 8)))

;; main

(defn main []
  (let [cmd (first *command-line-args*)]
    (cond
      (= cmd "boll") (thr-all name-pred `(~boll-pred 1))
      (= cmd "macd") (thr-all name-pred `(~macd-pred 0))
      (= cmd "minmax") (thr-all name-pred minmax)
      (= cmd "msg") (send-msg @lark (nth *command-line-args* 1))
      (= cmd "notify") (screen-notify)
      (= cmd "test") (test *command-line-args*)
      :else (println "I need a proper command."))))

(main)
