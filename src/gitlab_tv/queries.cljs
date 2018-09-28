(ns gitlab-tv.queries)


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
