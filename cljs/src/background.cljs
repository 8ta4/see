(ns background
  (:require ["/content.js" :refer [getText]]
            ["@stdlib/random-base-lognormal" :as lognormal]
            [com.rpl.specter :refer [ATOM setval]]
            [shadow.cljs.modern :refer [js-await]]))

(defonce port
  (js/chrome.runtime.connectNative "host"))

(defonce state
  (atom {}))

(defn handle-tab-update
  [_ _ tab]
  (setval [ATOM :status] (:status (js->clj tab :keywordize-keys true)) state))

(defn finalize
  [id]
  (js-await [results (js/chrome.scripting.executeScript (clj->js {:func getText
                                                                  :target {:tabId id}}))]
            (->> (js->clj results :keywordize-keys true)
                 first
                 :result
                 (.postMessage port)))
  ((:stop @state))
  (setval ATOM {} state)
  (js/setTimeout (partial js/chrome.tabs.remove id) (lognormal 10 1)))

(defn take-screenshot
  [id]
  (js-await [screenshot (js/chrome.tabs.captureVisibleTab)]
            (if (and (= "complete" (:status @state))
                     (= screenshot (:screenshot @state)))
              (do (js/console.log "Screenshot didn't change")
                  (finalize id))
              (setval [ATOM :screenshot] screenshot state))))

(defn handle-host
  [url]
  (js/console.log "Message from host:")
  (js/console.log url)
  (js-await [tab (js/chrome.tabs.create (clj->js {}))]
            (js/chrome.tabs.onUpdated.addListener handle-tab-update
                                                  (clj->js {:tabId (:id (js->clj tab :keywordize-keys true))}))
            (setval [ATOM :stop]
                    (juxt (partial js/clearInterval (js/setInterval (partial take-screenshot (:id (js->clj tab :keywordize-keys true))) 100))
                          #(js/chrome.tabs.onUpdated.removeListener handle-tab-update)
                          (partial js/clearTimeout (js/setTimeout (partial finalize (:id (js->clj tab :keywordize-keys true))) 10000)))
                    state)
            (js/chrome.tabs.update (:id (js->clj tab :keywordize-keys true)) (clj->js {:url url}))))

(defn init []
  (js/console.log "Hello, World!")
  (.addListener port.onMessage handle-host))