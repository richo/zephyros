## Zephyros - Clojure API

#### Setup

* Install leiningen if need be: `brew install leiningen`
* Add this to ~/.lein/profiles.clj: `{:user {:plugins [[lein-exec "0.3.0"]]}}`

#### Sample file

* Put this in `my-zeph.clj` somewhere:

```clojure
(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/data.json "0.2.2"]])

(load-file "/Applications/Zephyros.app/Contents/Resources/libs/zephyros.clj")

(bind "D" ["Cmd" "Shift"]
      (fn []
        (alert "hello world" 1)))

@listen-for-callbacks ;; necessary when you use (bind) or (listen)
```

#### Run

```bash
lein run path/to/my-zeph.clj
```

#### API

```clojure
;; top level

(defn bind [key mods f])
(defn listen [event f])

(defn get-focused-window [])
(defn get-visible-windows [])
(defn get-all-windows [])

(defn get-main-screen [])
(defn get-all-screens [])

(defn get-running-apps [])

(defn alert [msg duration])
(defn log [msg])
(defn choose-from [list title f]) ;; f receives chosen idx or nil if canceled

(defn relaunch-config [])
(defn get-clipboard-contents [])


;; window

(defn get-window-title [window])

(defn get-frame [window])       ;; takes {:x, :y, :w, :h}
(defn get-size [window])        ;; takes {:w, :h}
(defn get-top-left [window])    ;; takes {:w, :h}

(defn set-frame [window f])     ;; returns {:x, :y, :w, :h}
(defn set-size [window s])      ;; returns {:w, :h}
(defn set-top-left [window tl]) ;; returns {:x, :y}

(defn minimize [window])
(defn maximize [window])
(defn un-minimize [window])

(defn get-app-for-window [window])
(defn get-screen-for-window [window])

(defn focus-window [window]) ;; returns bool
(defn focus-window-left [window])
(defn focus-window-right [window])
(defn focus-window-up [window])
(defn focus-window-down [window])

(defn normal-window? [window])
(defn minimized? [window])


;; app

(defn visible-windows-for-app [app])
(defn all-windows-for-app [app])

(defn get-app-title [app])
(defn app-hidden? [app])

(defn show-app [app])
(defn hide-app [app])

(defn kill-app [app])
(defn kill9-app [app])


;; screeen

(defn screen-frame-including-dock-and-menu [screen]) ;; returns {:x, :y, :w, :h}
(defn screen-frame-without-dock-or-menu [screen])    ;; returns {:x, :y, :w, :h}

(defn next-screen [screen])
(defn previous-screen [screen])
```
