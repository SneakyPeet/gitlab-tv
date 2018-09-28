(ns gitlab-tv.core
  (:require [cemerick.url :as url]
            [rum.core :as rum]
            [clojure.string :as string]
            [gitlab-tv.gitlab :as gitlab]
            [gitlab-tv.queries :as queries]
            [promesa.core :as p]
            [clojure.contrib.humanize :as humanize]))


(enable-console-print!)


(-> (url/url "http://google.co.za")
    str)


;;;; CONFIG

(def fields
  [{:id :token
    :description "Your Gitlab Api Token"
    :value ""}
   {:id :path
    :description "Path to your Gitlab"
    :value "https://gitlab.com"}
   {:id :group-id
    :description "Add your gitlab group id to see only group projects"
    :value ""
    :optional? true}
   {:id :debug
    :description "set to true to view log"
    :value "true"
    :optional? true}])


(defn prep-fields-map [href]
  (let [url (url/url href)
        query (:query url)
        fields (->> fields
                    (map (fn [{:keys [id value] :as field}]
                           (update field :value #(get query (name id) %)))))]
    (->> fields
         (map (juxt :id identity))
         (into {}))))


(defn prep-config [href fields]
  (let [field-config
        (->> fields
             (map (juxt :id identity))
             (map (fn [[k v]]
                    [k (:value v)]))
             (into {}))]
    (-> field-config
        (assoc :href href
               :refresh-rate-seconds 60)
        (update :debug #(= "true" %)))))


(defn tv-url [state]
  (let [{:keys [config fields]} state
        href (url/url (:href config))
        query (->> fields
                   vals
                   (map (juxt :id :value))
                   (map (fn [[k v]]
                          [(name k) v]))
                   (into {}))]
    (-> href
        (assoc :query query)
        str)))


;;;; STATE

(defonce *state (atom {:page :init
                       :projects {}
                       :logs '()
                       :job-poll-id nil}))


(defn log [message]
  (prn message)
  (swap! *state update :logs (fn [logs]
                               (->> (conj logs message)
                                    (take 30)))))

(defn initialize []
  (let [href js/window.location.href
        fields-map (prep-fields-map href)
        config (prep-config href (vals fields-map))
        token (:token config)
        state
        (cond-> @*state
          true (assoc :fields fields-map :config config :initialized? true)
          (not (string/blank? token)) (assoc :page :tv))]
    (reset! *state state)))


(defn initialized? [] (:initialized? @*state))


(defn update-config-field [id value]
  (swap! *state
         assoc-in
         [:fields id :value]
         value))


(defn set-page [page]
  (swap! *state assoc :page page))


(defn set-loading-error [error]
  (swap! *state assoc :error error :page :init))


(defn set-job-poll-id [id]
  (swap! *state assoc :job-poll-id id))


(defn merge-projects [projects]
  (let [projects (->> projects
                      (map (juxt :id identity)))]
    (swap! *state update :projects merge projects)))


(defn merge-jobs [project jobs]
  (let [jobs (->> jobs
                  (map #(assoc % :project-id (:id project)))
                  (map (juxt :id identity)))]
    (swap! *state update :jobs merge jobs)))


;;;; PROCESS

(defn fetch-job [project]
  (let [{:keys [name_with_namespace id]} project
        config (:config @*state)
        fetch (gitlab/api log config)
        request (gitlab/jobs id)]
    (log (str "Fetching jobs for " name_with_namespace))
    (-> (fetch request)
        (p/then (fn [jobs]
                  (log (str "Received " (count jobs) " for " name_with_namespace))
                  (merge-jobs project jobs)))
        (p/catch (fn [error]
                   (let [m (str error " while fetching jobs")]
                     (set-loading-error m)))))))


(defn fetch-jobs []
  (let [projects (:projects @*state)]
    (log (str "Fetching jobs for " (count projects) " projects"))
    (doseq [project (vals projects)]
      (fetch-job project))))


(defn start-job-polling []
  (log "Starting Job Polling")
  (let [interval (get-in @*state [:config :refresh-rate-seconds])
        interval (* 1000 interval)
        job-id (js/setInterval fetch-jobs interval)]
    (swap! *state assoc :job-poll-id job-id)))


(defn stop-job-polling []
  (log "Stopping Job Polling")
  (if-let [job-poll-id (:job-poll-id @*state)]
    (do
      (js/clearInterval job-poll-id)
      (set-job-poll-id nil)
      (log (str "Stopped Job Polling (id: " job-poll-id ")")))
    (log "Job Not Running")))


(defn fetch-projects []
  (let [config (:config @*state)
        fetch (gitlab/api log config)
        group-id (:group-id config)
        request (if (string/blank? group-id)
                  (gitlab/projects)
                  (gitlab/group-projects group-id))
        url (:url request)
        path (:path config)]
    (-> (fetch request)
        (p/then (fn [projects]
                  (if (empty? projects)
                    (set-loading-error "Zero Projects Where Loaded")
                    (do
                      (log (str "Received " (count projects) " Projects"))
                      (merge-projects projects)
                      (fetch-jobs)
                      (start-job-polling)))))
        (p/catch (fn [error]
                   (let [m (str error " while fetching " url " from " path)]
                     (set-loading-error m)))))))


;;;; VIEWS

(defmulti render-page :page)

(defmethod render-page :init [state]
  (let [fields (:fields state)
        tv-url (tv-url state)
        error (:error state)]
    [:div
     [:div.hero.is-primary
      [:div.hero-body
       [:div.container
        [:h1.title "GITLAB TV"]
        [:h2.subtitle "Configure your settings and hit load to watch some gitlab tv"]]]]
     [:div.section
      [:div.container
       (when error
         [:article.message.is-danger
          [:div.message-body
           error]])
       (->>
        fields
        vals
        (map-indexed
         (fn [i {:keys [id value optional? description]}]
           (let [input-class
                 (when (= :token id)
                   (if (string/blank? value) "is-danger" "is-success"))]
             [:div.field {:key i}
              [:label.label (name id) (when optional? [:small " (optional)"])]
              [:div.control
               [:input.input
                {:class input-class
                 :type "text"
                 :value value
                 :on-change #(update-config-field id (.. % -target -value))
                 }]]
              (when description
                [:p.help {:class input-class} description])]))))
       [:div.field.has-addons
        [:p.control.is-expanded
         [:input.input {:type "text"
                        :disabled true
                        :value tv-url}]]
        [:p.control
         [:a.button.is-primary
          {:href tv-url}
          "Load"]]]]]]))


(rum/defc poll <
  {:did-mount (fn [s]
                (fetch-projects)
                s)
   :will-unmount (fn [s]
                   (stop-job-polling)
                   s)}
  [content]
  content)


(defn icon [class] [:span.icon.is-size-7 [:i {:class class}]])

(defn pipeline-status-class [status]
  (case status
    "running" "has-background-link"
    "pending" "has-background-info"
    "success" "has-background-success"
    "failed" "has-background-danger"
    "canceled" "has-background-warning"
    "skipped" "has-background-grey-light"
    "has-background-black"))


(defn pipeline-status-icon [status]
  (let [icon
        (case status
          "pending" "far fa-clock"
          "running" "fas fa-spinner fa-pulse fa-sync-alt"
          "failed" nil
          "success" nil
          "canceled" nil
          "skipped" nil
          "manual" nil
          nil)]
    (when icon
      [:span.icon.is-size-7
       [:i {:class icon}]])))


(defn job-status-icon [i job]
  (let [icon
        (case (:status job)
          "created" "far fa-circle"
          "pending" "far fa-clock"
          "running" "fas fa-spinner fa-pulse fa-sync-alt"
          "failed" "far fa-times-circle"
          "success" "far fa-check-circle"
          "canceled" "far fa-circle"
          "skipped" "far fa-circle"
          "manual" "far fa-circle"
          "far fa-circle")]
    [:span.icon.is-size-7.is-small {:key i :on-mouse-over #(js/tippy
                                                            (.. % -target)
                                                            (clj->js {:content (:name job)}))}
     [:i {:class icon}]]))


(defmethod render-page :tv [state]
  (let [{:keys [config logs projects jobs]} state
        build-history (queries/build-history projects jobs)
        latest-builds (queries/latest-builds build-history)
        failed-builds (queries/failed-builds latest-builds)]
    (poll
     [:div.tv.has-background-dark
      [:div.columns.is-gapless
       [:div.column
        [:table.table.is-fullwidth.has-text-white
         [:tbody
          (->> build-history
               (take 30)
               (map-indexed
                (fn [i {:keys [ref status name commit-user commit last_action stages]}]
                  [:tr {:class (pipeline-status-class status)}
                   [:td
                    [:ul
                     [:li.has-text-weight-bold.is-size-5 (pipeline-status-icon status) name]
                     [:li status]]]
                   [:td
                    [:ul
                     [:li.has-text-weight-bold (icon "fas fa-spinner fa-code-branch") ref]
                     [:li.has-text-weight-semibold (icon "fas fa-user") commit-user]
                     [:li (icon "far fa-file")(humanize/truncate commit 50)]]
                    ]
                   [:td
                    [:ul.is-pulled-left
                     (->> stages
                          (map-indexed
                           (fn [i {:keys [stage jobs]}]
                             [:li {:key i} stage])))]
                    [:ul.is-pulled-left.job-icons
                     (->> stages
                          (map-indexed
                           (fn [i {:keys [stage jobs]}]
                             [:li {:key i}
                              (->> jobs
                                   (map-indexed
                                    (fn [i job]
                                      (job-status-icon i job))))])))]]
                   [:td
                    [:ul
                     [:li (icon "fas fa-clock") (humanize/datetime (js/Date. last_action))]
                     [:li (icon "fas fa-calendar-alt") (subs last_action 0 10)]]
                    ]])))]]]
       (when-not (empty? failed-builds)
         [:div.column.is-narrow {:style {:min-width "300px"}}
          (when-not (empty? failed-builds)
            [:div
             [:div.card.has-background-dark.is-shadowless
              [:div.card-content {:style {:padding-bottom "0"}}
               [:div.content
                [:h3.title.is-marginless.has-text-white "Failed Builds"]]]]
             (->> failed-builds
                  (map-indexed
                   (fn [i {:keys [name ref commit-user-avatar commit-user commit] :as p}]
                     [:div.card.has-background-dark.has-text-white.is-shadowless
                      [:div.card-content
                       [:div.media
                        [:div.media-left
                         [:figure.image.is-48x48
                          [:img {:src commit-user-avatar}]]]
                        [:div.media-content
                         [:p.title.is-5.has-text-white name]
                         [:p.subtitle.is-6.has-text-white ref]]]
                       [:div.content
                        [:p [:strong.has-text-white "Blame: "] commit-user
                         [:br] (humanize/truncate commit 50)]]]])))])])]
      (when (:debug config)
        [:div.notification {:style {:position "fixed" :bottom "0" :right "50px"}}
         [:ul
          (->> logs
               reverse
               (map-indexed
                (fn [i m]
                  [:li {:key i} m])))]])
      [:button.button.is-dark
       {:on-click #(set-page :init)
        :style {:position "fixed" :bottom "5px" :right "5px"}} (icon "fas fa-cog")]])))


(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    (render-page current-state)))


;;;; STARTquarters

(when-not (initialized?)
  (initialize)
  #_(doto js/window
    (aset "onblur" #(set-page :init))
    (aset "onfocus" #(set-page :tv))))


(rum/mount (app *state) (.getElementById js/document "app"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! *state update-in [:__figwheel_counter] inc)
)
