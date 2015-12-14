(ns callanalyzer.web
  (:require [org.httpkit.server :as http]
            [environ.core :refer [env]]
            [callanalyzer.core :as core]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [taoensso.sente :as sente]
            [clojure.core.async :as a :refer [>! <! >!! <!! go-loop go chan buffer close! thread alts! alts!! timeout]]
            [taoensso.sente.server-adapters.http-kit :refer (sente-web-server-adapter)]
            [taoensso.timbre.appenders.core :as appenders]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.params :as params])
  (:use [slingshot.slingshot :only [try+]]
        [taoensso.timbre :only [error tracef debugf info infof warn warnf errorf] :as timbre]
        [clojure.inspector :only [inspect inspect-tree]])
  (:gen-class))

(def es-endpoint "http://localhost:19200")
(defonce server (atom nil))

(defn get-port [] (try
                    (read-string (:port env))
                    (catch Exception e
                      (warn "No PORT environment variable set, using default")
                      8081)))

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn connected-uids]}
      (sente/make-channel-socket! sente-web-server-adapter {:user-id-fn #(:client-id %)})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def connected-uids connected-uids))

(defmulti event-msg-handler :id)

(defmethod event-msg-handler :chsk/ws-ping [ev-msg])

(defmethod event-msg-handler :chsk/uidport-open [ev-msg])

(defmethod event-msg-handler :chsk/uidport-close [ev-msg])

(defmethod event-msg-handler :search/request-id [{:keys [event]}]
  (core/search-with-deps* {:field :request-id :value event}))

(defmethod event-msg-handler :search/vcap-request-id [{:keys [event]}]
  (core/search-with-deps* {:field :vcap-request-id :value event}))

(defmethod event-msg-handler :default [{:keys [?reply-fn] :as ev-msg}]
  (warnf "Unhandled event: %s" ev-msg)
  (when ?reply-fn
    (?reply-fn [:error "Invalid search type"])))

(defn event-msg-handler* [{:keys [?reply-fn event] :as ev-msg}]
  (when ?reply-fn
    (?reply-fn
      (try+
        [:success (event-msg-handler (assoc ev-msg :event (second event)))]
        (catch [:type :illegal-argument] _ [:error "Illegal argument"])
        (catch Object e (do
                          (error "Unexpected error" e)
                          [:error "Unexpected error"]))))))

(defroutes app-routes
           (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
           (POST "/chsk" req (ring-ajax-post req))
           (route/resources "/"))

(def app (-> app-routes
             ring.middleware.keyword-params/wrap-keyword-params
             ring.middleware.params/wrap-params))

(defn get-es-endpoint []
  (let [endpoint (or (:es-endpoint env) es-endpoint)]
    (info "Using elastichsearch endpoint " endpoint)
    endpoint))

(defn start-server! []
  (if (:vcap-application env)
    (info "Running as cloud foundry application")
    (do
      (timbre/merge-config! {:appenders {:spit-appender (appenders/spit-appender {:fname "./out.log"})}})
      (info "Running as standalone application")))

  (core/initialize! {:es-endpoint (get-es-endpoint)})
  (reset! server (http/run-server #'app {:port (get-port)}))
  (sente/start-chsk-router! ch-chsk event-msg-handler*))

(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 1000)
    (reset! server nil)))

(defn -main [& args]
  (start-server!))