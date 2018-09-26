(ns gitlab-tv.core
  (:require [cemerick.url :as url]
            [rum.core :as rum]
            [clojure.string :as string]
            [gitlab-tv.gitlab :as gitlab]
            [promesa.core :as p]))


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
    (assoc field-config
           :href href)))


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
                       :projects {}}))

(defn initialize [state]
  (let [href js/window.location.href
        fields-map (prep-fields-map href)
        config (prep-config href (vals fields-map))
        token (:token config)]
    (cond-> state
      true (assoc :fields fields-map :config config :initialized? true)
      (not (string/blank? token)) (assoc :page :tv))))


(defn set-loading-error [error]
  (swap! *state assoc :error error :page :init))


;;;; DATA

(defn merge-projects [projects]
  (let [projects (->> projects
                      (map (juxt :id identity)))]
    (swap! *state update :projects merge projects)))


(defn fetch-projects []
  (let [config (:config @*state)
        fetch (gitlab/api config)
        group-id (:group-id config)
        request (if (string/blank? group-id)
                  (gitlab/projects)
                  (gitlab/group-projects group-id))
        url (:url request)]
    (-> (fetch request)
        (p/then merge-projects)
        (p/catch (fn [error]
                   (set-loading-error
                    (str error " while fetching projects from " url)))))))


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
                 :on-change
                 (fn [e]
                   (swap! *state
                          assoc-in
                          [:fields id :value]
                          (.. e -target -value)))}]]
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
                s)}
  [content]
  content)

(defmethod render-page :tv [state]
  (poll
   [:div
    [:ul
     (->> state
          :projects
          vals
          (map :name)
          (map-indexed
           (fn [i p]
             [:li {:key i} p])))]
    [:button.button {:on-click #(swap! *state assoc :page :init)} "Config"]]))

(rum/defc app < rum/reactive [state]
  (let [current-state (rum/react state)]
    (render-page current-state)))


;;;; START

(when-not (:initialized? @*state)
  (swap! *state initialize))


(rum/mount (app *state) (.getElementById js/document "app"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! *state update-in [:__figwheel_counter] inc)
)
