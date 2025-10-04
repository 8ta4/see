(ns background
  (:require ["/content.js" :refer [getText]]
            [com.rpl.specter :refer [ATOM setval]]
            [shadow.cljs.modern :refer [js-await]]))

(defonce port
  (js/chrome.runtime.connectNative "host"))

(def state
  (atom {}))

(defn handle-tab-update
  [_ _ tab]
  (setval [ATOM :status] (:status (js->clj tab :keywordize-keys true)) state))

(defn take-screenshot
  [id]
  #(js-await [screenshot (js/chrome.tabs.captureVisibleTab)]
             (if (and (= "complete" (:status @state))
                      (= screenshot (:screenshot @state)))
               (do (js/console.log "Screenshot didn't change")
                   (js-await [results (js/chrome.scripting.executeScript (clj->js {:func getText
                                                                                   :target {:tabId id}}))]
                             (->> (js->clj results :keywordize-keys true)
                                  first
                                  :result
                                  port.postMessage))
                   ((:stop @state))
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
                    (juxt (partial js/clearInterval (js/setInterval (take-screenshot (:id (js->clj tab :keywordize-keys true))) 100))
                          #(js/chrome.tabs.onUpdated.removeListener handle-tab-update))
                    state)
            (js/chrome.tabs.update (:id (js->clj tab :keywordize-keys true)) (clj->js {:url url}))))

(port.onMessage.addListener handle-host)

(defn init []
  (js/console.log "Hello, World!"))