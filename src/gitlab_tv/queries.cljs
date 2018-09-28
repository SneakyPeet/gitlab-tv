(ns gitlab-tv.queries)


(defn build-history [projects jobs]

  (let [jobs (->> jobs
                  vals
                  (group-by :pipeline)
                  (map
                   (fn [[pipeline jobs]]
                     (let [{:keys [project-id commit user]} (first jobs)
                           project (get projects project-id)
                           date (->> jobs
                                     (map (juxt :created_at :started_at))
                                     (reduce into)
                                     sort
                                     last)]
                       (merge
                        (select-keys pipeline [:ref :status])
                        (select-keys project [:name])
                        {:commit-user (:author_name commit)
                         :commit (:title commit)
                         :last_action date}))))
                  (sort-by :last_action)
                  reverse
                  (take 50)
                  #_(sort-by :created_at)
                  #_(take 50)
                  #_(map
                   (fn [{:keys [project-id] :as job}]
                     (assoc job :project (get projects project-id)))))]
    jobs))
