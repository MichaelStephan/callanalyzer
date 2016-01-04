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
  (let [results (subscribe [:search-result])]
  [:div "Search results:"
   [:div ^{:key (gensym)} [:ul (for [i @results]
                                 (ui-rtr i))]]]))

(defn app []
  [:div
   [ui-header]
   [ui-search]
   [ui-search-results]])

(def app-state {:search-term ""
                :search-option :search/request-id 
                :search-status :idle
                :search-result []
                ;:circles [{:name "circle 1"
                     ; :x 10
                     ; :y 10
                     ; :r 10
                     ; :color "black"}
                     ;{:name "circle 2"
                     ; :x 35
                     ; :y 35
                     ; :r 15
                     ; :color "red"}
                     ;{:name "circle 3"
                      ;:x 100
                      ;:y 100
                      ;:r 30
                      ;:color "blue"}]
                })

;; define your app data so that it doesn't get over-written on reload
;;---- Event handlers-----------
(register-handler
  :initialize-db
  (fn [_ _]
    app-state))

(register-handler :search
  (fn [db _]
    (let [{:keys [search-option search-term]} db]
      (search search-option search-term))
    db))

(register-handler :search-status
  (fn [db [_ status]]
    (assoc-in db [:search-status] status)))

(register-handler :search-term
  (fn [db [_ term]]
    (assoc-in db [:search-term] term)))

(register-handler :search-option
  (fn [db [_ option]]
    (assoc-in db [:search-option] option)))

(register-handler :search-result
  (fn [db [_ result]]
    (assoc-in db [:search-result] result)))

(register-handler :reset
  (fn [db _]
    app-state))

;(register-handler
;  :update
;  (fn
;    [db [_ idx param val]]
;    (println "idx " idx "param " param "val " val)
;    (assoc-in db [:circles idx param ] val)))

;;---- Subscription handlers-----------
(register-sub
  :search-option
  (fn [db _]
    (reaction (:search-option @db))))

(register-sub
  :search-status
  (fn [db _]
    (reaction (:search-status @db))))

(register-sub
  :search-term
  (fn [db _]
    (reaction (:search-term @db))))


(register-sub
  :search-result
  (fn [db _]
    (reaction (:search-result @db))))

;(register-sub
;  :circles
;  (fn
;    [db _]
;    (reaction (:circles @db))))

;(defn d3-inner [data]
; (reagent/create-class
;    {:reagent-render (fn [] [:div [:svg {:width 400 :height 800}]])
;
;     :component-did-mount (fn []
;                             (println "called 1x")
;                            (let [d3data (clj->js data)]
;                              (.. js/d3
;                                  (select "svg")
;                                  (selectAll "circle")
;                                  (data d3data)
;                                  enter
;                                  (append "circle")
;                                  (attr "cx" (fn [d] (.-x d)))
;                                  (attr "cy" (fn [d] (.-y d)))
;                                  (attr "r" (fn [d] (.-r d)))
;                                  (attr "fill" (fn [d] (.-color d))))))
;
;     :component-did-update (fn [this]
;                             (println "called 2")
;                             (let [[_ data] (reagent/argv this)
;                                   d3data (clj->js data)]
;                               (.. js/d3
;                                   (selectAll "circle")
;                                   (data d3data)
;                                   (attr "cx" (fn [d] (.-x d)))
;                                   (attr "cy" (fn [d] (.-y d)))
;                                   (attr "r" (fn [d] (.-r d))))))}))


;(defn slider [param idx value]
;  [:input {:type "range"
;           :value value
;           :min 0
;           :max 500
;           :style {:width "100%"}
;           :on-change #(dispatch [:update idx param (-> % .-target .-value)])}])

;(defn sliders [data]
;    [:div (for [[idx d] (map-indexed vector data)]
;            ^{:key (str "slider-" idx)}
;            [:div
;             [:h3 (:name d)]
;             "x " (:x d) (slider :x idx (:x d))
;             "y " (:y d) (slider :y idx (:y d))
;             "r " (:r d) (slider :r idx (:r d))])])

;(defn app2 []
;  (let [data (subscribe [:circles])]
;    (fn []
;      [:div {:class "container"}
;        [:div {:class "row"}
;          [:div {:class "col-md-5"}
;            [d3-inner @data]]
;          [:div {:class "col-md-5"}
;            [sliders @data]]]]
;      )))

(defn render-app []
  (r/render-component [app] (js/document.getElementById "app")))

(defn ^:export run [& args]
  (dispatch-sync [:initialize-db])
  (render-app)
  (start!))
