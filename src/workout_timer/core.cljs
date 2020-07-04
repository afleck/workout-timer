(ns workout-timer.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   [re-frame.core :as rf]
   [reagent-material-ui.components :as mui]
   [reagent-material-ui.icons.pause :refer [pause]]
   [reagent-material-ui.icons.play-arrow :refer [play-arrow]]))
  

(def beep (js/Audio. "beep.wav"))

(rf/reg-event-db
 :timer
 (fn [db [_ new-time]]
   (assoc db :time new-time)))

(rf/reg-event-db
 :set-start
 (fn [db [_ new-start]]
   (assoc db :start new-start)))

(rf/reg-event-db
 :set-timer-id
 (fn [db [_ new-id]]
   (assoc db :timer-id new-id)))

(rf/reg-event-db
 :set-pause-time
 (fn [db [_ new-pause]]
   (assoc db :pause-time new-pause)))

(rf/reg-event-db
 :set-paused
 (fn [db [_ state]]
   (assoc db :is-paused state)))

(rf/reg-event-db
 :set-workout-length
 (fn [db [_ workout-length]]
   (assoc db :workout-length workout-length)))

(rf/reg-event-db
 :set-rest-length
 (fn [db [_ rest-length]]
   (assoc db :rest-length rest-length)))

(rf/reg-event-db
 :set-working-out
 (fn [db [_ working-out?]]
   (assoc db :working-out? working-out?)))

(rf/reg-event-db
 :set-workout-counter
 (fn [db [_ workout-counter]]
   (assoc db :workout-counter workout-counter)))

(rf/reg-event-db
 :set-workout-number
 (fn [db [_ workout-number]]
   (assoc db :workout-number workout-number)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:time (js/Date.)
    :start (js/Date.)
    :timer-id nil
    :is-paused false
    :pause-time 0
    :workout-length 5
    :rest-length 3
    :working-out? false
    :workout-counter 0
    :workout-number 5}))

(rf/reg-sub
 :time
 (fn [db _]
   (:time db)))

(rf/reg-sub
 :start
 (fn [db _]
   (:start db)))

(rf/reg-sub
 :timer-id
 (fn [db _]
   (:timer-id db)))

(rf/reg-sub
 :is-paused
 (fn [db _]
   (:is-paused db)))

(rf/reg-sub
 :pause-time
 (fn [db _]
   (:pause-time db)))

(rf/reg-sub
 :workout-length
 (fn [db _]
   (:workout-length db)))

(rf/reg-sub
 :rest-length
 (fn [db _]
   (:rest-length db)))

(rf/reg-sub
 :working-out?
 (fn [db _]
   (:working-out? db)))

(rf/reg-sub
 :workout-counter
 (fn [db _]
   (:workout-counter db)))

(rf/reg-sub
 :workout-number
 (fn [db _]
   (:workout-number db)))


;; TIMER FUNCTIONS
;; ---------------
(defn start-timer []
  (let [old-timer-id @(rf/subscribe [:timer-id])
        paused? @(rf/subscribe [:is-paused])
        pause-time @(rf/subscribe [:pause-time])
        start @(rf/subscribe [:start])]
    (if paused?
      (do
        (rf/dispatch [:set-paused false])
        (rf/dispatch [:set-pause-time 0])
        (rf/dispatch [:set-start (js/Date. (+ (.getTime start) (- (js/Date.) pause-time)))])
        (rf/dispatch [:timer (js/Date.)])
        (rf/dispatch [:set-timer-id (js/setInterval #(rf/dispatch [:timer (js/Date.)]) 100)]))
      
      
      (if-not old-timer-id
        (do
          (rf/dispatch [:set-paused false])
          (rf/dispatch [:set-pause-time 0])
          (rf/dispatch [:set-start (js/Date.)])
          (rf/dispatch [:timer (js/Date.)])
          (rf/dispatch [:set-timer-id (js/setInterval #(rf/dispatch [:timer (js/Date.)]) 100)]))))))

(defn stop-timer []
  (let [cur-timer @(rf/subscribe [:timer-id])
        paused? @(rf/subscribe [:is-paused])]
    (if (and cur-timer (not paused?))
      (do
        (js/clearInterval cur-timer)
        (rf/dispatch [:set-paused true])
        (rf/dispatch [:set-pause-time (js/Date.)])
        (rf/dispatch [:set-timer-id nil])))))

(defn reset-timer []
  (let [old-timer @(rf/subscribe [:timer-id])
        new-time (js/Date.)
        paused? @(rf/subscribe [:is-paused])]
    (if (or old-timer paused?)
      (do
        (js/clearInterval old-timer)
        (rf/dispatch-sync [:set-timer-id nil])
        (rf/dispatch-sync [:set-paused false])
        (rf/dispatch-sync [:set-pause-time 0])))
    (rf/dispatch-sync [:timer new-time])
    (rf/dispatch-sync [:set-start new-time])))

(defn timer []
  (let [cur (-> @(rf/subscribe [:time])
                .getTime)
        start (-> @(rf/subscribe [:start])
                  .getTime)]
    [mui/grid {:item true}
    [mui/typography {:style {:font-size "4em"}}
     (js/Math.trunc (/ (- cur start) 1000))]]))

;; WORKOUT FUNCTIONS
;; -----------------
(defn start-workout []
  (reset-timer)
  (rf/dispatch-sync [:set-workout-counter (js/Number @(rf/subscribe [:workout-number]))])
  (rf/dispatch-sync [:set-working-out true])
  (start-timer))

(defn reset-workout []
  (reset-timer)
  (rf/dispatch-sync [:set-workout-counter 0])
  (rf/dispatch-sync [:set-working-out false]))

(defn start-workout-segment []
  (reset-timer)
  (rf/dispatch-sync [:set-working-out true])
  (start-timer))

(defn start-rest-segment []
  (reset-timer)
  (rf/dispatch-sync [:set-working-out false])
  (start-timer))

(defn workout-display []
  (let [workout-counter @(rf/subscribe [:workout-counter])
        working-out? @(rf/subscribe [:working-out?])
        workout-length @(rf/subscribe [:workout-length])
        rest-length @(rf/subscribe [:rest-length])
        timer (/ (- (-> @(rf/subscribe [:time])
                        .getTime)
                    (-> @(rf/subscribe [:start])
                        .getTime))
                 1000)]
    (if (> workout-counter 0)
      (do
        (if working-out?
          (if (>= timer workout-length)
            (do (rf/dispatch [:set-workout-counter (- workout-counter 1)])
                (.play beep)
                (start-rest-segment)))
          (if (>= timer rest-length)
            (do (start-workout-segment)
                (.play beep))))
        [mui/grid {:item true}
         [mui/typography "workouts left: " workout-counter]
         [mui/typography "working out?: " (if working-out? "true" "false")]])
      (do
        (reset-timer)
        [mui/grid {:item true}
         [mui/typography "you're done with the workout? start another!"]]))))

(defn workout-length-input
  []
  (let [gettext (fn [e] (-> e .-target .-value))
        emit    (fn [e] (rf/dispatch [:set-workout-length (js/Number (gettext e))]))]
    [mui/grid {:item true}
     [mui/text-field {:type "number"
                      :label "workout length (sec)"
                      :value @(rf/subscribe [:workout-length])
                      :on-change emit}]]))

(defn rest-length-input
  []
  (let [gettext (fn [e] (-> e .-target .-value))
        emit    (fn [e] (rf/dispatch [:set-rest-length (js/Number (gettext e))]))]
    [mui/grid {:item true}
     [mui/text-field {:type "number"
                      :label "rest length (sec)"
                      :value @(rf/subscribe [:rest-length])
                      :on-change emit}]]))

(defn workout-number-input
  []
  (let [gettext (fn [e] (-> e .-target .-value))
        emit    (fn [e] (rf/dispatch [:set-workout-number (js/Number (gettext e))]))]
    [mui/grid {:item "true"}
     [mui/text-field {:type "number"
                      :label "number of workouts"
                      :value @(rf/subscribe [:workout-number])
                      :on-change emit}]]))



(defn timer-controls []
  (let [workout-counter @(rf/subscribe [:workout-counter])
        paused? @(rf/subscribe [:is-paused])]
    (when-not (= workout-counter 0)
  [mui/grid {:item true}
   (if paused?
   [mui/button {:on-click #(start-timer)} [play-arrow]]
   [mui/button {:on-click #(stop-timer)} [pause]])])))

(defn workout-controls []
  [mui/grid {:item true}
   [mui/button-group {:color "primary"}
    [mui/button {:on-click #(start-workout)} "start new workout"]
    [mui/button {:on-click #(reset-workout)} "reset workout"]]])

(defn home-page []
  [mui/grid {:container true
             :direction "column"
             :spacing 3
             :justify "center"
             :align-items "center"}

   [timer]
   [timer-controls]
    [workout-length-input]
    [rest-length-input]
    [workout-number-input]
    [workout-display]
    [workout-controls]])
;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (rf/dispatch-sync [:initialize])
  (reset-timer)
  (mount-root))
