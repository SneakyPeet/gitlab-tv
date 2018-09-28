# gitlab-tv

An app for watching your gitlab pipelines.

![Gitlab TV](/readme.png)

Watch some TV here [https://sneakypeet.github.io/gitlab-tv/](https://sneakypeet.github.io/gitlab-tv/)

Simply add your gitlab api access token and hit Load to start watching your pipelines.
(note: the token is only set as a url param and never stored anywhere)

Built with Figwheel and Clojurescript

# Why?

Although similar dashboards like [Gitlab Pipelines TV Dashboard](https://github.com/KSF-Media/gitlab-dashboard) exist, I wanted more.

* An easier way to initialize the tv
* A better view on the overall status of builds
* A better view on pipeline stages (and possibly the ability to trigger builds)
* Viewing projects related to group-id's
* I did not have time to learn purescript to modify existing dashboards

# Development

This is a simple Fighweel and Clojurescript app built using Lein.
