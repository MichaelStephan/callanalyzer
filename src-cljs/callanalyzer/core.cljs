(ns callanalyzer.core
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require
    [cljs.core.async :as async :refer (<! <! >! put! chan)]
    [taoensso.sente :as sente :refer (cb-success?)]
    [taoensso.timbre :as timbre :refer-macros (tracef debugf infof warnf errorf)]
    [reagent.core :as r]))

(enable-console-print!)

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" {:type :ajax})]
  (def chsk chsk)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn)
  (def chsk-state state))

(defmulti event-msg-handler :id)

(defn event-msg-handler* [{:as ev-msg :keys [event]}]
  (infof "Received event: %s" event)
  (event-msg-handler ev-msg))

(defmethod event-msg-handler :default
  [{:as ev-msg :keys [event]}]
  (errorf "Unhandled event: %s" event))

(sente/start-chsk-router! ch-chsk event-msg-handler*)

(defn ui-header [title]
  [:div>h1 title])

(defn ui-input [value]
  [:div
   [:input {:type      "text"
            :value     @value
            :on-change #(reset! value (-> % .-target .-value))}]])

(defn ^:export search [option value status results]
  (chsk-send! [@option @value] (* 60 1000) #(do
                                             (case %
                                               :chsk/timeout (js/alert "timeout")
                                               :chsk/error (js/alert "some error")
                                               (let [[s payload] %]
                                                 (case s
                                                   :success (swap! results conj payload)
                                                   (js/alert payload))))
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
                          (str (clojure.string/join "" (repeat (if (number? hop)
                                                                 hop
                                                                 1) "-"))
                               "> " (:client i) " calls " (:service i) (:request m) " (" (:response m) "/" (* 1000.0 (:response_time m)) ")"))
                    (ui-app i)])

(defn ui-search-results [results]
  [:div "Search results:"
   [:div (for [r @results]
           ^{:key (gensym)} [:ul (for [i r]
                                   (ui-rtr i))])]])

(defn app []
  (let [search-results (r/atom [])]
    [:div
     [ui-header "YaaS Call Analyzer - alpha version"]
     [ui-search search-results]
     [ui-search-results search-results]]))

(defn render-app []
  (r/render-component [app] (js/document.getElementById "app")))

(defn ^:export run []
  (render-app))


