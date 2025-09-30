(ns background)

(defonce port
  (js/browser.runtime.connectNative "host"))

(port.onMessage.addListener (fn [message]
                              (js/console.log "Message from host:")
                              (js/console.log message)))

(defn init []
  (js/console.log "Hello, World!"))