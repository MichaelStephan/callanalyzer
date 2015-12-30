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
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]])
  (:use [slingshot.slingshot :only [try+]]
        [taoensso.timbre :only [error tracef debugf info infof warn warnf errorf] :as timbre]
        [clojure.inspector :only [inspect inspect-tree]])
  (:gen-class))

(def es-endpoint "http://localhost:19200")
(defonce server_ (atom nil))

(def passwords {"user" "secret"})

(defn get-port [] (try+
                   (read-string (:port env))
                   (catch Object _
                     (warn "No PORT environment variable set, using default")
                     8081)))

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn connected-uids]}
      (sente/make-channel-socket! sente-web-server-adapter)]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def connected-uids connected-uids))

(defmulti event-msg-handler :id)
(defmethod event-msg-handler :chsk/ws-ping [_])
(defmethod event-msg-handler :chsk/uidport-open [_])
(defmethod event-msg-handler :chsk/uidport-close [_])

(defmethod event-msg-handler :search/request-id [{:keys [event]}]
  (core/search-with-deps* {:field :request-id :value event}))

(defmethod event-msg-handler :search/vcap-request-id [{:keys [event]}]
  (core/search-with-deps* {:field :vcap-request-id :value event}))

(defmethod event-msg-handler :default [{:keys [?reply-fn] :as ev-msg}]
  (warnf "Unhandled event: %s" ev-msg)
  (when-let [reply-fn ?reply-fn]
    (reply-fn [:error "Invalid search type"])))

(defn event-msg-handler* [{:keys [?reply-fn event ring-req] :as ev-msg}]
  (let [session (:session ring-req)
        uid (:uid session)]
    (when-let [reply-fn ?reply-fn]
      (reply-fn
        (if uid
          (try+
            [:success (event-msg-handler (assoc ev-msg :event (second event)))]
            (catch [:type :illegal-argument] _ [:error "Illegal argument"])
            (catch Object e [:error "Unexpected error"]))
          [:error "Access denied"])))))

(defn check-password [user-id password]
  (= (get passwords user-id) password))

(defn login! [req]
  (let [{:keys [session params]} req
        {:keys [user-id password]} params]
    (if (check-password user-id password)
      {:status 200 :session (assoc session :uid user-id)}
      {:status 403})))

(defn logout! [req]
  (let [{:keys [session]} req]
    {:status 200 :session (dissoc session :uid)}))

(defroutes app-routes
  (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (ring-ajax-post req))
  (POST "/login" req (login! req))
  (POST "/logout" req (logout! req))
  (route/resources "/"))

(def ring-handler
  (-> app-routes
      wrap-keyword-params
      wrap-params
      wrap-session))

(defn get-es-endpoint []
  (let [endpoint (or (:es-endpoint env) es-endpoint)]
    (info "Using elasticsearch endpoint " endpoint)
    endpoint))

(defn stop-server! []
  (when-let [server @server_]
    (server :timeout 1000)
    (reset! server_ nil)))

(defn start-server! []
  (stop-server!)
  (if (:vcap-application env)
    (info "Running as cloud foundry application")
    (do
      (timbre/merge-config! {:appenders {:spit-appender (appenders/spit-appender {:fname "./out.log"})}})
      (info "Running as standalone application")))

  (core/initialize! {:es-endpoint (get-es-endpoint)})
  (reset! server_ (http/run-server #'ring-handler {:port (get-port)}))
  (sente/start-chsk-router! ch-chsk event-msg-handler*))

(defn -main [& args]
  (start-server!))
