(ns background
  (:require [shadow.cljs.modern :refer [js-await]]
            [com.rpl.specter :refer [ATOM setval]]))

(defonce port
  (js/chrome.runtime.connectNative "host"))

(def state
  (atom {}))

(defn handle-tab-update
  [_ _ tab]
  (setval [ATOM :status] (:status (js->clj tab :keywordize-keys true)) state))

(defn take-screenshot
  []
  (js-await [screenshot (js/chrome.tabs.captureVisibleTab)]
            (if (and (= "complete" (:status @state))
                     (= screenshot (:screenshot @state)))
              (do ((:stop @state))
                  (setval ATOM {} state))
              (setval [ATOM :screenshot] screenshot state))))

(defn handle-host
  [url]
  (js/console.log "Message from host:")
  (js/console.log url)
  (js-await [tab (js/chrome.tabs.create (clj->js {}))]
            (js/chrome.tabs.onUpdated.addListener handle-tab-update
                                                  (clj->js {:tabId (:id (js->clj tab :keywordize-keys true))}))
            (setval [ATOM :stop]
                    (juxt #(js/chrome.tabs.onUpdated.removeListener handle-tab-update)
                          (partial js/clearInterval (js/setInterval take-screenshot 100)))
                    state)
            (js/chrome.tabs.update (:id (js->clj tab :keywordize-keys true)) (clj->js {:url url}))))

(port.onMessage.addListener handle-host)

(defn init []
  (js/console.log "Hello, World!"))