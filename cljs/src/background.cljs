(ns background
  (:require [shadow.cljs.modern :refer [js-await]]))

(defonce port
  (js/chrome.runtime.connectNative "host"))

(defn handle-host
  [message]
  (js/console.log "Message from host:")
  (js/console.log message)
  (js-await [tab (js/chrome.tabs.create (clj->js {}))]
            (:id (js->clj tab :keywordize-keys true))))

(port.onMessage.addListener handle-host)

(defn init []
  (js/console.log "Hello, World!"))