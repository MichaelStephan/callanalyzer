(ns callanalyzer.common
  (:require [clojure.string :as str]))

(defn make-illegal-state-exception [obj]
  {:type :illegal-state :object obj :message (str "`" obj "` has invalid state")})

(defn make-illegal-argument-exception [obj msg]
  {:type :illegal-argument :object obj :message (str "`" obj "` " msg)})

(def not-empty? (complement empty?))

(def not-blank? (complement str/blank?))

(defn valid-request-id? [obj]
  (and
    (string? obj)
    (not-blank? obj)
    (str/blank? (str/replace obj #"[a-zA-Z0-9-]" ""))))

(defn valid-vcap-request-id? [obj]
  (valid-request-id? obj))

(defn get-type [log]
  (if-let [type (:_type log)]
    (-> type (.toLowerCase)) ""))

(defn rtr? [log]
  (= "rtr" (get-type log)))

(defn app? [log]
  (= "app" (get-type log)))

(defn get-request-id [log]
  {:pre [log]}
  (or
    (-> log :_source :message :hybris_request_id)
    (-> log :_source :message :log :requestid)))

(defn get-tenant [log]
  {:pre [log]}
  (-> log :_source :message :log :tenant))

(defn get-hop [log]
  {:pre [log]}
  (-> log :_source :message :log :hop))

(defn get-service [log]
  {:pre [log]}
  (-> log :_source :message :service))

(defn get-client [log]
  {:pre [log]}
  (-> log :_source :message :log :client))

(defn get-vcap-request-id [log]
  {:pre [log]}
  (or
    (-> log :_source :message :log :vcaprequestid)
    (-> log :_source :message :vcap_request_id)))

(defn equal-vcap-request-id? [vcap-request-id log]
  (= vcap-request-id (get-vcap-request-id log)))

(defn get-timestamp [log]
  {:pre [log]}
  (-> log :_source :message :timestamp))