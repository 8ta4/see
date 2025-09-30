(ns background)

(defonce port
  (js/chrome.runtime.connectNative "host"))

(defn handle-host
  [message]
  (js/console.log "Message from host:")
  (js/console.log message)
  (js/chrome.tabs.create (clj->js {})))

(port.onMessage.addListener handle-host)

(defn init []
  (js/console.log "Hello, World!"))