(ns callanalyzer.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require
    [taoensso.sente :as sente :refer (cb-success?)]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [reagent.core :as r]
    [cljs.reader :as reader]
    [clojure.string :as cstr]
    [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                                path
                                                register-sub
                                                dispatch
                                                dispatch-sync
                                                subscribe]]
            [cljsjs.d3]))

(enable-console-print!)

(def sente-endpoint "/chsk")
(def login-endpoint "/login")
(def logout-endpoint "/logout")

(defn fig-reload []
  (infof "reloading code")
  (r/force-update-all))

(defonce router_ (atom nil))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! sente-endpoint  {:type :ajax})]
  (def chsk chsk)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def chsk-state state))

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

(defn ^:export search [option value]
  (dispatch [:search-status :searching])
  (chsk-send! [option value] (* 60 1000) #(do
                                            (case %
                                               :chsk/timeout (js/alert "An timeout error occured")
                                               :chsk/error (js/alert "A technical error occured")
                                               (let [[s payload] %]
                                                 (case s
                                                   :success (dispatch [:search-result payload])
                                                   (js/alert (str "A technical error occured: " payload)))))
                                            (dispatch [:search-status :idle]))))

(defn ui-header []
  (fn []
    [:div>h1 "YaaS Call Analyzer - alpha version"]))

(defn ui-input []
  (fn []
    (let [search-term (subscribe [:search-term])]
      [:div
       [:input {:type      "text"
                :value     @search-term
                :on-change #(dispatch [:search-term (-> % .-target .-value)])}]])))

(defn ui-search-option []
  (fn []
    (let [option (subscribe [:search-option])
          sri :search/request-id
          svri :search/vcap-request-id]
      [:div
       [:label [:input {:type      "radio" :name "option" :value "request-id"
                        :on-change #(dispatch [:search-option sri])
                        :checked   (= @option sri)}] "request-id"]
       [:label [:input {:type      "radio" :name "option" :value "vcap-request-id"
                        :on-change #(dispatch [:search-option svri])
                        :checked   (= @option svri)}] "vcap-request-id"]])))

(defn ui-search-reset-button []
  (fn []
    [:div
     [:input {:type     "button"
              :value    "Reset"
              :on-click #(dispatch [:reset])}]]))

(defn ui-search-button []
  (fn []
    [:div
     [:input {:type     "button"
              :value    "Search"
              :on-click #(dispatch [:search])}]]))

(defn ui-search-status []
  (fn []
    (let [status (subscribe [:search-status])]
      [:p (case @status
            :idle "idle"
            :searching "searching"
            :default "unknown")])))

(defn ui-search []
  (fn [] 
      [:div
       [ui-input]
       [ui-search-status]
       [ui-search-option]
       [ui-search-button]
       [ui-search-reset-button]]))

(defn ui-app [i]
  [:ul (for [j (:nested i)]
         ^{:key (gensym)} [:li (let [s (:_source j) m (:message s) l (:log m)]
                                 (str (:level m) ": " (:message l) (if-let [st (:stacktrace l)] st)))])])

(defn get-timestamp [log]
  {:pre [log]}
  (or (-> log :_source :message :timestamp)
      (-> log :_source :timestamp)))

(defn ui-rtr [i]
  ^{:key (gensym)} [:li (let [hop (:hop i) s (:_source i) m (:message s)]
                          (str (get-timestamp i) " " (cstr/join (repeatedly (if (number? hop)
                                                                           hop
                                                                           0)
                                                                         (fn [] "-") ))
                               "> " (:client i) " calls " (:service i) (:request m) " (" (:response m) "/" (* 1000.0 (:response_time m)) ")"))
                    (ui-app i)])

(defn ui-search-results []
  (fn []
    (let [results (subscribe [:search-result])]
    [:div "Search results:"
     [:div ^{:key (gensym)} [:ul (for [i @results]
                                   (ui-rtr i))]]])))

(def bar-height 20)

(defn d3-render [data]
  (let [d3data (clj->js data)
                                    min (apply min (map #(get-in % [:_source :message :timestamp]) data))
                                    max (apply max (map #(+ (get-in % [:_source :message :timestamp])
                                                            (* 10000000 (get-in % [:_source :message :response_time]))) data))
                                    nmin (- max min)
                                    x (.. (js/d3.scale.linear)
                                          (range (clj->js [0 400]))
                                          (domain (clj->js [0 nmin])))
                                    bar (.. js/d3
                                            (select "svg")
                                            (selectAll "g")
                                            (data d3data)
                                            enter
                                            (append "g")
                                            (attr "transform" (fn [d, i] (str "translate(0," (* i bar-height) ")"))))]
                                (.. bar
                                    (append "rect")
                                    (attr "x" (fn [d] (x (- (get-in (js->clj d) ["_source" "message" "timestamp"]) min))))
                                    (attr "width" (fn [d] (x (* 10000000 (get-in (js->clj d) ["_source" "message" "response_time"])))))
                                    (attr "height" (fn [d] bar-height)))
                                ))

(defn d3-inner [data]
   (reagent/create-class
      {:reagent-render (fn [] [:div [:svg {:width 400 :height 400}]])

       :component-did-mount (fn [] (d3-render data))

       :component-did-update (fn [this]
                               (let [[_ data] (reagent/argv this)
                                     d3data (clj->js data)]
                                 (d3-render data)))}))

(defn app []
  (let [data (subscribe [:search-result])]
    (fn []
      [:div {:class "container"}
        [:div {:class "row"}
          [:div {:class "col-md-5"}
           [ui-header]
           [ui-search]
           [ui-search-results]]]
       [:div {:class "row"}
        [:div {:class "col-md-5"}
         [d3-inner @data]
         ]]])))
  
(def app-state {:search {:term ""
                         :option :search/request-id 
                         :status :idle
                         :result []}})

(register-handler
  :initialize-db
  (fn [_ _]
    app-state))

(register-handler :search
  (fn [db _]
    (let [{:keys [option term]} (:search db)]
      (search option term))
    db))

(register-handler :search-status
  (fn [db [_ status]]
    (assoc-in db [:search :status] status)))

(register-handler :search-term
  (fn [db [_ term]]
    (assoc-in db [:search :term] term)))

(register-handler :search-option
  (fn [db [_ option]]
    (assoc-in db [:search :option] option)))

(register-handler :search-result
  (fn [db [_ result]]
    (assoc-in db [:search :result] result)))

(register-handler :reset
  (fn [db _]
    app-state))

(register-sub :search-option
  (fn [db _]
    (reaction (get-in @db [:search :option]))))

(register-sub :search-status
  (fn [db _]
    (reaction (get-in @db [:search :status]))))

(register-sub :search-term
  (fn [db _]
    (reaction (get-in @db [:search :term]))))

(register-sub :search-result
  (fn [db _]
    (reaction (get-in @db [:search :result]))))

(defn render-app []
  (r/render-component [app] (js/document.getElementById "app")))

(defn ^:export run [& args]
  (dispatch-sync [:initialize-db])
  (render-app)
  (start!))
