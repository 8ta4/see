(ns background
  (:require [shadow.cljs.modern :refer [js-await]]))

(defonce port
  (js/chrome.runtime.connectNative "host"))

(def state
  (atom nil))

(defn handle-tab-update
  [_ _ tab]
  (reset! state (:status (js->clj tab :keywordize-keys true))))

(defn handle-host
  [message]
  (js/console.log "Message from host:")
  (js/console.log message)
  (js-await [tab (js/chrome.tabs.create (clj->js {}))]
            (js/chrome.tabs.onUpdated.addListener handle-tab-update (clj->js {:tabId (:id (js->clj tab :keywordize-keys true))}))))

(port.onMessage.addListener handle-host)

(defn init []
  (js/console.log "Hello, World!"))