(ns background)

(defonce port
  (js/browser.runtime.connectNative "host"))

(port.onMessage.addListener (fn [message]
                              (println "Message from host:")
                              (println message)))

(defn init []
  (println "Hello, World!"))