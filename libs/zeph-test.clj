(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.2"]])

(load-file "./zephyros.clj")


(bind "d" ["cmd" "shift"]
      (fn []
        (let [win (get-focused-window)]
          (set-frame win
                     (-> (get-frame win)
                         (update-in [:x] + 20)
                         (update-in [:y] + 20)
                         (update-in [:w] - 40)
                         (update-in [:h] - 40)))

          (alert "ok done" 1))))

(bind "m" ["cmd" "shift"]
      (fn []
        (let [win (get-focused-window)]
          (maximize win))))

(println "ready")

(while true)
