(ns callanalyzer.core
  (:require
    [taoensso.sente :as sente :refer (cb-success?)]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [reagent.core :as r]
    [cljs.reader :as reader]))

(enable-console-print!)

(def sente-endpoint "/chsk")
(def login-endpoint "/login")
(def logout-endpoint "/logout")

(defonce router_ (atom nil))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! sente-endpoint  {:type :ajax})]
  (defonce chsk chsk)
  (defonce ch-chsk ch-recv)
  (defonce chsk-send! send-fn)
  (defonce chsk-state state))

(defmulti event-msg-handler :id)
(defmethod event-msg-handler :chsk/handshake [_])
(defmethod event-msg-handler :chsk/state [_])

(defn event-msg-handler* [ev-msg] (event-msg-handler ev-msg))

(defmethod event-msg-handler :default
  [{:keys [event]}]
  (errorf "Unhandled event: %s" event))

(defn stop-router! [] (when-let [stop-fn @router_] (stop-fn)))

(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-chsk-router! ch-chsk event-msg-handler*)))

(defn start! []
  (start-router!))

(defn ui-header [title]
  [:div>h1 title])

(defn ui-input [value]
  [:div
   [:input {:type      "text"
            :value     @value
            :on-change #(reset! value (-> % .-target .-value))}]])

(defn ^:export login [user-id password]
  (sente/ajax-call login-endpoint {:method :post
                             :params     {:user-id  user-id
                                          :password password}
                             :timeout-ms 5000}
                   (fn [{:keys [?status ?error ?content]}]
                     (try
                       (cond
                         (= ?status 200) (if-let [{:keys [uid]}
                                                  (apply hash-map (some-> ?content
                                                                          reader/read-string))]
                                           (sente/chsk-reconnect! chsk)
                                           (throw "Login failed, invalid server response"))
                         (= ?status 403) (throw "Login denied, invalid credentials")
                         :else (throw "Loging failed, an unknown error occured"))
                       (catch :default e
                         (js/alert e)))))
  nil)

(defn ^:export logout []
  (sente/ajax-call logout-endpoint {:method :post
                                    :timeout-ms 5000}
                   (fn [_]))
  nil)

(defn ^:export search [option value status results]
  (chsk-send! [@option @value] (* 60 1000) #(do
                                             (case %
                                               :chsk/timeout (js/alert "An timeout error occured")
                                               :chsk/error (js/alert "A technical error occured")
                                               (let [[s payload] %]
                                                 (case s
                                                   :success (swap! results conj payload)
                                                   (js/alert (str "A technical error occured: " payload)))))
                                             (reset! status "")))
  (reset! value ""))

(defn ui-search-option [option]
  (let [sri :search/request-id svri :search/vcap-request-id]
    [:div
     [:label [:input {:type      "radio" :name "option" :value "request-id"
                      :on-change #(reset! option sri)
                      :checked   (= @option sri)}] "request-id"]
     [:label [:input {:type      "radio" :name "option" :value "vcap-request-id"
                      :on-change #(reset! option svri)
                      :checked   (= @option svri)}] "vcap-request-id"]]))

(defn reset-search [value results]
  (reset! value "")
  (reset! results []))

(defn ui-search-reset-button [value results]
  [:div
   [:input {:type     "button"
            :value    "Reset"
            :on-click #(reset-search value results)}]])

(defn ui-search-button [option value status results]
  [:div
   [:input {:type     "button"
            :value    "Search"
            :on-click #(do (search option value status results)
                           (reset! status "searching"))}]])

(defn ui-search-status [status]
  [:p @status])

(defn ui-search [results]
  (let [option (r/atom :search/request-id)
        value (r/atom "")
        status (r/atom "")]
    [:div
     [ui-input value]
     [ui-search-status status]
     [ui-search-option option]
     [ui-search-button option value status results]
     [ui-search-reset-button value results]]))

(defn ui-app [i]
  [:ul (for [j (:nested i)]
         ^{:key (gensym)} [:li (let [s (:_source j) m (:message s) l (:log m)]
                                 (str (:level m) ": " (:message l) (if-let [st (:stacktrace l)] st)))])])

(defn ui-rtr [i]
  ^{:key (gensym)} [:li (let [hop (:hop i) s (:_source i) m (:message s)]
                          (str hop "> " (:client i) " calls " (:service i) (:request m) " (" (:response m) "/" (* 1000.0 (:response_time m)) ")"))
                    (ui-app i)])

(defn ui-search-results [results]
  [:div "Search results:"
   [:div (for [r @results]
           ^{:key (gensym)} [:ul (for [i r]
                                   (ui-rtr i))])]])

(defonce search-results (r/atom []))

(defn app []
  [:div
   [ui-header "YaaS Call Analyzer - alpha version"]
   [ui-search search-results]
   [ui-search-results search-results]])

(defn render-app []
  (r/render-component [app] (js/document.getElementById "app")))

(defn fig-reload []
  (infof "reloading code")
  (r/force-update-all))

(defn ^:export run [& args]
  (start!)
  (render-app))


