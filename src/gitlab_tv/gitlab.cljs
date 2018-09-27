(ns gitlab-tv.gitlab
  (:require [httpurr.client :as http]
            [httpurr.status :as s]
            [httpurr.client.xhr :as xhr]
            [promesa.core :as p]
            [clojure.string :as string]))


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
   :query-params {:per_page 100}})


(defn- wrap-api-path [request path]
  (update request :url #(str path "/api/v4" %)))


(defn- wrap-token [request token]
  (assoc-in request [:query-params :private_token] token))


(defn- decode
  [response]
  (-> response
      :body
      js/JSON.parse
      (js->clj :keywordize-keys true)))


(defn- process-response
  [response]
  (if (= s/ok (:status response))
    (p/resolved (decode response))
    (if (string/blank? (:body response))
      (p/rejected (str "Status " (:status response)))
      (p/rejected (:message (decode response))))))


(defn api [log config]
  (fn [request]
    (let [request (-> request
                      (wrap-api-path (:path config))
                      (wrap-token (:token config)))]
      (log (str "API " (name (:method request)) " " (:url request)))
      (p/then
       (http/send! xhr/client request)
       process-response))))


(comment
  (let [api (api {:token "" :path "https://gitlab.com"})]
    (->
     (p/then (api (group-projects ""))
             (fn [projects]
               (p/resolved (-> projects (nth 4) :id))))
     (p/then (fn [p] (api (jobs p))))
     (p/then (fn [r] (prn r)))
     (p/catch (fn [err]
                (prn "err")
                (prn err)))))
  )
