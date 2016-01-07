(ns callanalyzer.core
  (:require [clojurewerkz.elastisch.rest :as esr]
            [clojurewerkz.elastisch.rest.document :as esd]
            [clojurewerkz.elastisch.query :as q]
            [clojurewerkz.elastisch.rest.response :as esrsp]
            [clojure.string :as str]
            [callanalyzer.common :as c])
  (:use [slingshot.slingshot :only [try+ throw+]]
        [taoensso.timbre :only [spy info warn] :as timbre]
        [clojure.inspector :only [inspect-tree]]))

(def es-page-size 1000)
(defonce es-conn (atom nil))
(def no-requestid-exception {:type :no-request-id :message "request-id not found"})
(def multiple-request-ids-exception {:type :multiple-request-ids :message "multiple request-ids found"})

(defn make-es-conn [endpoint]
  (info "Creating elastichsearch pool connection")
  (esr/connect
   endpoint {:conn-manager (clj-http.conn-mgr/make-reusable-conn-manager {:timeout 300})}))

(defn make-es-conn! [endpoint]
  (info "Setting elasticsearch connection" endpoint)
  (reset! es-conn (make-es-conn endpoint)))

(defn initialize! [{:keys [es-endpoint]}]
  {:pre [es-endpoint (clojure.java.io/as-url es-endpoint)]}
  (make-es-conn! es-endpoint))

(defn es-search [query]
  (info "Searching with elasticsearch:" query)

  (when-not @es-conn
    (throw+ (c/make-illegal-state-exception #'es-conn)))
  (let [res (esd/search-all-types @es-conn "_all" :query query :size es-page-size :from 0)
        n (esrsp/total-hits res)
        hits (esrsp/hits-from res)]
    (info "Query " query " returned " n " results")
    (-> hits
        ((partial map #(dissoc % :_score))))))

(defn post-process-ids [ids]
  (remove nil? (distinct ids)))

(defn get-request-id [logs]
  (let [r (post-process-ids (map c/get-request-id logs))]
    (cond
      (empty? r) (throw+ no-requestid-exception)
      (> (count r) 1) (throw+ multiple-request-ids-exception)
      :else (first r))))

(defn get-vcap-request-ids [logs] (post-process-ids (map c/get-vcap-request-id logs)))

(defmulti search (fn [query] (:field query)))

(defmethod search :request-id [{:keys [value]}]
  {:pre [(or (c/valid-request-id? value)
             (throw+ (c/make-illegal-argument-exception
                      value "is not a valid request-id")))]}
  (let [query {:bool {:should [{:term {:requestid value}}
                               {:term {:hybris_request_id value}}]}}]
    (es-search query)))

(defmethod search :vcap-request-ids [{:keys [value]}]
  {:pre [(or (coll? value)
             (throw+ (c/make-illegal-argument-exception
                      value "is not a collection")))
         (or (not (some #(not %) (map c/valid-vcap-request-id? value)))
             (throw+ (c/make-illegal-argument-exception
                      value "contains a valid vcap-request-id")))]}
  (if (c/not-empty? value)
    (let [query {:bool {:should (flatten (map #(vector
                                                {:term {:vcaprequestid %}}
                                                {:term {:vcap_request_id %}}) value))}}]
      (es-search query))
    []))

(defmethod search :vcap-request-id [{:keys [value]}]
  (search {:field :vcap-request-ids :value [value]}))

(defmethod search :default [{:keys [field]}]
  (warn "Searching for" field "not supported"))

(defn search* [query]
  (info "Searching:" query)
  (search query))

(defmulti search-with-deps :field)

(defmethod search-with-deps :request-id [query]
  (info "Searching with dependencies (request-id):" query)
  (let [res1 (search* query)
        ids (get-vcap-request-ids res1)
        res2 (search* {:field :vcap-request-ids :value ids})]
    (distinct (concat res1 res2))))

(defmethod search-with-deps :vcap-request-id [query]
  (info "Searching with dependencies (vcap-request-id):" query)
  (let [res (search* query)
        rid (get-request-id res)]
    (search-with-deps {:field :request-id :value rid})))

(defmethod search-with-deps :default [{:keys [field]}]
  (warn "Searching for" field "not supported"))

(defn log-compare [x y]
  (or (some #(when (not= 0 %) %)
            (map #(compare (% x) (% y)) [:timestamp
                                         :hop 
                                         c/get-response-time
                                         c/get-es-timestamp]))
      0))

(defn nest-search-with-deps [res] 
  (let [apps (filter c/app? res)
        rtrs (filter c/rtr? res)]
    (->> rtrs
         (map #(let [nested (filter (partial c/equal-vcap-request-id? (c/get-vcap-request-id %)) apps)
                     all (concat [%] nested)]
                 (-> %
                     (assoc :nested (sort-by c/get-timestamp nested))
                     (assoc :request-id (some c/get-request-id all))
                     (assoc :tenant (some c/get-tenant all))
                     (assoc :hop (or (some c/get-hop all) 0))
                     (assoc :service (some c/get-service all))
                     (assoc :timestamp (c/get-timestamp %))
                     (assoc :client (some c/get-client all)))))
         (sort-by identity log-compare))))

(defn search-with-deps* [query]
  (info "Searching with dependencies:" query)
  (->
   (search-with-deps query)
   nest-search-with-deps))
