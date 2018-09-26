(ns gitlab-tv.gitlab
  (:require [httpurr.client :as http]
            [httpurr.status :as s]
            [httpurr.client.xhr :as xhr]
            [promesa.core :as p]))


(defn projects []
  {:method :get
   :url "/projects"
   :query-params {:simple true
                  :per_page 20
                  :order-by "last_activity_at"}})


(defn group-projects [group-id]
  {:method :get
   :url (str "/groups/" group-id "/projects")
   :query-params {:simple true
                  :per_page 20
                  :order-by "last_activity_at"}})

(defn jobs [project-id]
  {:method :get
   :url (str "/projects/" project-id "/jobs")
   :query-params {:per_page 1}})


(defn- wrap-api-path [request path]
  (update request :url #(str path "/api/v4" %)))


(defn- wrap-token [request token]
  (assoc-in request [:query-params :private_token] token))


(defn decode
  [response]
  (-> response
      :body
      js/JSON.parse
      (js->clj :keywordize-keys true)))


(defn- process-response
  [response]
  (condp = (:status response)
    s/ok           (p/resolved (decode response))
    s/not-found    (p/rejected :not-found)
    s/unauthorized (p/rejected :unauthorized)))


(defn- api [config]
  (fn [request]
    (let [request (-> request
                      (wrap-api-path (:path config))
                      (wrap-token (:token config)))]
      (prn (str "api " (name (:method request)) " " (:url request)))
      (p/then
       (http/send! xhr/client request)
       process-response))))
