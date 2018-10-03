(ns gitlab-tv.queries)

(defn latest-jobs-for-pipeline [jobs]
  (->> jobs
       (group-by :name)
       (map (fn [[_ jobs]]
              (->> jobs
                   (sort-by :created_at)
                   last)))))


(defn build-history [projects jobs]
  (let [jobs (->> jobs
                  vals
                  (group-by :pipeline)
                  (map
                   (fn [[pipeline jobs]]
                     (let [{:keys [project-id commit user tag]} (first jobs)
                           project (get projects project-id)
                           date (->> jobs
                                     (map (juxt :created_at :started_at))
                                     (reduce into)
                                     sort
                                     last)
                           stages (->> jobs
                                       latest-jobs-for-pipeline
                                       (group-by :stage)
                                       (map
                                        (fn [[stage jobs]]
                                          {:stage stage
                                           :jobs jobs})))]
                       (merge
                        (select-keys pipeline [:ref :status])
                        (select-keys project [:name :project-id])
                        {:commit-user (:author_name commit)
                         :commit-user-avatar (:avatar_url user)
                         :commit (:title commit)
                         :short-hash (:short_id commit)
                         :last_action date
                         :stages stages
                         :tag? tag}))))
                  (sort-by :last_action)
                  reverse)]
    jobs))


(defn latest-builds [build-history]
  (->> build-history
       (group-by (juxt :project-id :ref))
       (map (fn [[_ pipelines]]
              (->> pipelines
                   (filter #(not (contains? #{"pending" "running" "canceled" "created"} (:status %)) ))
                   (sort-by :last_action)
                   last)))))


(defn failed-builds [latest-builds]
  (->> latest-builds
       (filter #(= "failed" (:status %)))
       (filter #(not (true? (:tag? %))))))


(defn job-stats [jobs]
  (->> jobs
       vals
       (filter #(or (= "failed" (:status %)) (= "success" (:status %))))
       (map #(update % :created_at subs 0 10))
       (group-by :created_at)
       (map (fn [[day jobs]]
              (let [statusses (->> (group-by :status jobs)
                                   (map (fn [[s jobs]]
                                          [(keyword s) (count jobs)]))
                                   (into {}))]
                (assoc statusses :day day))))
       (sort-by :day)))
