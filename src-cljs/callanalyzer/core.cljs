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

(defn show-login [] (dispatch [:show-login true]))
(defn hide-login [] (dispatch [:show-login false]))
(defn show-config [] (dispatch [:show-config true]))
(defn hide-config [] (dispatch [:show-config false]))

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
                                           (do
                                             (dispatch [:authenticated? true])
                                             (hide-login)
                                             (sente/chsk-reconnect! chsk))
                                           (throw "Login failed, invalid server response"))
                         (= ?status 403) (throw "Login denied, invalid credentials")
                         :else (throw "Loging failed, an unknown error occured"))
                       (catch :default e
                         (js/alert e)))))
  nil)

(defn ^:export logout []
  (sente/ajax-call logout-endpoint {:method :post
                                    :timeout-ms 5000}
                   (fn [_]
                     (dispatch [:authenticated? false])))
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

(defn ui-input []
  (fn []
    (let [search-term (subscribe [:search-term])
          option (subscribe [:search-option])]
      [:input {:class "form-control" 
               :placeholder (str "enter " (if (= @option :search/request-id)
                                            "hybris-request-id"
                                            "vcap-request-id"))
               :type "text"
               :value  @search-term
               :on-change #(dispatch [:search-term (-> % .-target .-value)])}])))

(defn ui-search-option []
  (fn []
    (let [option (subscribe [:search-option])
          sri :search/request-id
          svri :search/vcap-request-id]
      [:a {:href "#"
           :class "list-group-item"}
       [:h4 {:class "list-group-item-heading"} "Search by:"]
       [:div {:class "radio"}
        [:label [:input {:class "radio"
                         :type "radio" :name "option" :value "hybris-request-id"
                         :on-change #(dispatch [:search-option sri])
                         :checked (= @option sri)}] "hybris-request-id"]]
       [:div {:class "radio"}
        [:label [:input {:class "radio"
                         :type "radio" :name "option" :value "vcap-request-id"
                         :on-change #(dispatch [:search-option svri])
                         :checked   (= @option svri)}] "vcap-request-id"]]])))

(defn hide-search-status []
  (dispatch [:search-status :idle]))

(defn ui-search-status []
  (fn []
    (let [status (subscribe [:search-status])]
      (when (= @status :searching)
        [:div {:class "modal show"
               :tabIndex -1
               :role "dialog"}
         [:div {:class "modal-dialog"}
          [:div {:class "modal-content"}
           [:div {:class "modal-header"}
            [:button {:type "button"
                      :on-click hide-search-status 
                      :class "close"
                      :data-dismiss "modal"
                      :aria-label "Close"}
             [:span {:aria-hidden "true"} "×"]]
            [:h3 {:class "modal-title"} "Searching ..."]]
           [:div {:class "modal-body"}
            [:div {:class "progress"}
             [:div {:class "progress-bar progress-bar-striped active"
                    :role "progressbar"
                    :aria-valuenow "45"
                    :aria-valuemin "0"
                    :aria-valuemax "100"
                    :style {:width "100%"}}]]]
           [:div {:class "modal-footer"}
            [:button {:type "button"
                      :class "btn btn-default"
                      :data-dismiss "modal"
                      :on-click hide-search-status} "Cancel"]]]]]))))

(defn ui-login []
  (fn []
    (let [config (subscribe [:show-login])]
      (when @config
        [:div {:class "modal show"
               :tabIndex -1
               :role "dialog"}
         [:div {:class "modal-dialog"}
          [:div {:class "modal-content"}
           [:div {:class "modal-header"}
            [:button {:type "button"
                      :class "close"
                      :data-dismiss "modal"
                      :aria-label "Close"
                      :on-click hide-login}
             [:span {:aria-hidden "true"} "×"]]
            [:h3 {:class "modal-title"} "Login"]]
           [:div {:class "modal-body"}
            [:form 
             [:div {:class "form-group"}
              [:label {:for "username"} "Username:"]
              [:input {:id "username"
                       :type "input"
                       :class "form-control"
                       :on-change #(dispatch [:username (-> % .-target .-value)])}]]
             [:div {:class "form-group"}
              [:label {:for "password"} "Password:"]
              [:input {:id "password"
                       :type "password"
                       :class "form-control"
                       :on-change #(dispatch [:password (-> % .-target .-value)])}]]]
           [:div {:class "modal-footer"}
            [:button {:type "button"
                      :class "btn btn-primary"
                      :data-dismiss "modal"
                      :on-click (fn []
                                  (let [username (subscribe [:username])
                                        password (subscribe [:password])]
                                    (login @username @password)))} "Login"]
            [:button {:type "button"
                      :class "btn btn-default"
                      :data-dismiss "modal"
                      :on-click hide-login} "Cancel"]]]]]]))))

(defn ui-config []
  (fn []
    (let [config (subscribe [:show-config])]
      (when @config
        [:div {:class "modal show"
               :tabIndex -1
               :role "dialog"}
         [:div {:class "modal-dialog"}
          [:div {:class "modal-content"}
           [:div {:class "modal-header"}
            [:button {:type "button"
                      :class "close"
                      :data-dismiss "modal"
                      :aria-label "Close"
                      :on-click hide-config}
             [:span {:aria-hidden "true"} "×"]]
            [:h3 {:class "modal-title"} "Config"]]
           [:div {:class "modal-body"}
            [:div {:class "list-group"}
             [ui-search-option]]]
           [:div {:class "modal-footer"}
            [:button {:type "button"
                      :class "btn btn-default"
                      :data-dismiss "modal"
                      :on-click hide-config} "Close"]]]]]))))

(defn ui-search []
  (fn [] 
      [:div {:class "row"}
       [:div {:class "col-md-4 col-md-offset-4"}
        [:div {:class "input-group" :style {:margin-left "20px"
                                            :width "27em"}}
         [ui-input]
         [:span {:class "input-group-btn"}
          [:input {:class "btn btn-default"
                   :type "button"
                   :value "Search"
                   :on-click #(dispatch [:search])}]]
        ]]]
    ))

(defn ui-app [i]
  [:ul (for [j (:nested i)]
         ^{:key (gensym)} [:li (let [s (:_source j) m (:message s) l (:log m)]
                                 (str (:level m) ": " (:message l) (if-let [st (:stacktrace l)] st)))])])

(defn ui-search-results []
  (fn []
    (let [results (subscribe [:search-result])]
      (when-not (empty? @results)
        [:div {:class "row" :style {:margin-top "20px"}}
          [:div {:class "col-md-12"}
           [:table {:class "table table-hover table-condensed table-striped"}
            [:thead
             [:th "Timestamp"]
             [:th "Status"]
             [:th "Duration (msecs)"]
             ; [:th "Duration (%)"]
             [:th "Hop"]
             [:th "Client"]
             [:th "Service"]
             [:th "Verb"]
             [:th "Request"]
             [:th "Owner"]]
             [:tbody
               (for [i @results]
                 (let [hop (:hop i) s (:_source i) m (:message s)]
                   [:tr {:key (gensym)}
                    [:td (.. (js/Date. (/ (:timestamp m) 1000000)) toTimeString)]
                    [:td (:response m)]
                    [:td (.. (* 1000.0 (:response_time m)) (toFixed 2))]
                    ; [:td "TODO"]
                    [:td hop]
                    [:td (or (:client i) "-")]
                    [:td (or (:service i) "-")]
                    [:td (:verb m)]
                    [:td (:request m)]
                    [:td (:space m)]]))]]]]))))

(defn sec->psec [sec]
  (* 1000000000 sec))

(defn psec->sec [psec]
  (/ psec 1000000000))

(defn response->color [response]
  (cond
    (< response 300) "green"
    (and (>= response 300) (< response 400)) "green"
    (and (>= response 400) (< response 500)) "orange"
    (>= response 500) "red"
    :else "black"))

(defn d3-render [data new]
  (let [d3data (clj->js data)
        c (count data)
        max-bar-height 20
        min-bar-height 2
        bar-margin 1
        width 1024
        bar-height (if (< c 50)
                     max-bar-height
                     min-bar-height)
        height (+ 50 (* c (+ bar-height bar-margin)))

        min (apply min (map #(get-in % [:_source :message :timestamp]) data))
        max (apply max (map #(+ (get-in % [:_source :message :timestamp])
                                (sec->psec (get-in % [:_source :message :response_time]))) data))
        x (.. (js/d3.scale.linear)
              (range #js [0 (- width 200)]) ; TODO
              (domain #js [0 (psec->sec (- max min))]))
        xaxis (.. (js/d3.svg.axis)
                (scale x)
                (orient "bottom")
                (tickPadding 10)
                (innerTickSize (- height))
                (outerTickSize 0))
        svg (.. js/d3
                (select "svg"))]
    (if new
      (.. svg
          (append "g")
          (attr "class" "data")))

    (.. svg
        (attr "height" height))

    (let [data-selection  (.. svg
                              (selectAll ".data")
                              (selectAll "g")
                              (data d3data, (fn [d]
                                              (get (js->clj d) "_id"))))]
      (.. data-selection
         exit
         remove)

      (.. data-selection 
          enter
          (append "g")
          (on "click"
              (fn [] (this-as self (..
                                     (js/d3.select self)
                                     (selectAll "rect")
                                     (attr "fill" "black")))))
          (attr "transform"
                (fn [d, i]
                  (str "translate(0," (* i (+ bar-height bar-margin)) ")")))
          (append "rect")
          (attr "fill"
                (fn [d]
                  (let [response (get-in (js->clj d) ["_source" "message" "response"])]
                    (response->color response))))
          (attr "x"
                (fn [d]
                  (let [timestamp (get-in (js->clj d) ["_source" "message" "timestamp"])]
                    (x (psec->sec (- timestamp min))))))
          (attr "width"
                (fn [d]
                  (let [response-time (get-in (js->clj d) ["_source" "message" "response_time"])
                        width (x response-time)]
                    (if (> width 1)
                      width
                      5))))
          (attr "height"
                (fn [d]
                  bar-height)))
      (.. svg
          (selectAll ".axis")
          remove)
      (.. svg
          (append "g")
          (attr "class" "x axis")
          (attr "transform" (str "translate(0," (- height 40)")"))
          (call xaxis)))))

(defn d3-inner [data]
  (let [width 1024 
        height 600]
    (reagent/create-class
      {:reagent-render (fn []
                         [:div [:svg {:width width :height height}]])
       :component-did-mount (fn []
                              (d3-render data true))
       :component-did-update (fn [this]
                               (let [[_ data] (reagent/argv this)]
                                 (d3-render data false)))})))

(defn ui-menu []
  (fn []
    (let [authenticated? (subscribe [:authenticated?])]
      [:div {:class "row"}
            [:div {:class "col-md-4 btn-group btn-group-xs"
                   :role "group"}
             (if @authenticated?
               [:button {:type "button"
                         :class "btn btn-link"
                         :on-click logout} "logout"]
               [:button {:type "button"
                         :class "btn btn-link"
                         :on-click show-login} "login"])
             [:button {:type "button"
                       :class "btn btn-link"
                       :on-click show-config} "config"]]])))

(defn app []
  (let [data (subscribe [:search-result])]
    (fn []
      [:div {:class "container"} 
       [ui-search-status]
       [ui-login]
       [ui-config]
       [ui-menu]
       [:div {:class "row"}
        [:div {:class "col-md-6 col-md-offset-4"}
        [:h1 "YaaS Call Analyzer - v1.0"]]]
       [ui-search]
       [ui-search-results]
       [:div
        [:div
         [d3-inner @data]
         ]]])))
  
(def app-state {:search {:term ""
                         :option :search/request-id 
                         :status :idle
                         :result []}
                :show {:login false
                       :config false}
                :authentication {:credentials {:username ""
                                               :password ""}
                                 :authenticated? false}})

(register-handler
  :initialize-db
  (fn [_ _]
    app-state))

(register-handler :search
  (fn [db _]
    (let [{:keys [option term]} (:search db)]
      (search option term))
    db))

(register-handler :search-status (fn [db [_ status]] (assoc-in db [:search :status] status)))
(register-handler :search-term (fn [db [_ term]] (assoc-in db [:search :term] term)))
(register-handler :search-option (fn [db [_ option]] (assoc-in db [:search :option] option)))
(register-handler :search-result (fn [db [_ result]] (assoc-in db [:search :result] result)))
(register-handler :show-config (fn [db [_ show]] (assoc-in db [:show :config] show)))
(register-handler :show-login (fn [db [_ show]] (assoc-in db [:show :login] show)))
(register-handler :reset (fn [db _] app-state))
(register-handler :username (fn [db [_ username]] (assoc-in db [:authentication :credentials :username] username)))
(register-handler :password (fn [db [_ password]] (assoc-in db [:authentication :credentials :password] password)))
(register-handler :authenticated? (fn [db [_ authenticated]] (assoc-in db [:authentication :authenticated?] authenticated)))

(register-sub :search-option (fn [db _] (reaction (get-in @db [:search :option]))))
(register-sub :search-status (fn [db _] (reaction (get-in @db [:search :status]))))
(register-sub :search-term (fn [db _] (reaction (get-in @db [:search :term]))))
(register-sub :search-result (fn [db _] (reaction (get-in @db [:search :result]))))
(register-sub :show-config (fn [db _] (reaction (get-in @db [:show :config]))))
(register-sub :show-login (fn [db _] (reaction (get-in @db [:show :login]))))
(register-sub :username (fn [db _] (reaction (get-in @db [:authentication :credentials :username]))))
(register-sub :password (fn [db _] (reaction (get-in @db [:authentication :credentials :password]))))
(register-sub :authenticated? (fn [db _] (reaction (get-in @db [:authentication :authenticated?]))))

(defn render-app []
  (r/render-component [app] (js/document.getElementById "app")))

(defn ^:export run [& args]
  (dispatch-sync [:initialize-db])
  (render-app)
  (start!))
