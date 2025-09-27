(ns background)

(defonce port
  (js/browser.runtime.connectNative "host"))

(defn init []
  (println "Hello, World!"))