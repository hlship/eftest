(ns eftest.report.progress
  "A test reporter with a progress bar."
  (:require [clj-commons.ansi :as ansi]
            [clojure.test :as test]
            [eftest.report :as report]
            [eftest.report.pretty :as pretty]
            [progrock.core :as prog]))

(def ^:private clear-line (apply str "\r" (repeat 80 " ")))

(defn- colored-format [state]
  (ansi/compose ":progress/:total   :percent% ["
                [(when state (pretty/*fonts* state)) ":bar"]
                "]  ETA: :remaining"))

(defn- print-progress [{:keys [bar state]}]
  (prog/print bar {:format (colored-format state)}))

(defn- set-state [old-state new-state]
  (case [old-state new-state]
    [nil   :pass]  :pass
    [nil   :fail]  :fail
    [:pass :fail]  :fail
    [nil   :error] :error
    [:pass :error] :error
    [:fail :error] :error
    old-state))

(defmulti report :type)

(defmethod report :default [m])

(defmethod report :begin-test-run [m]
  (test/with-test-out
    (newline)
    (print-progress (reset! report/*context* {:bar (prog/progress-bar (:count m))}))))

(defmethod report :pass [m]
  (test/with-test-out
    (pretty/report m)
    (print-progress (swap! report/*context* update-in [:state] set-state :pass))))

(defmethod report :fail [m]
  (test/with-test-out
    (print clear-line)
    (binding [pretty/*divider* "\r"] (pretty/report m))
    (newline)
    (print-progress (swap! report/*context* update-in [:state] set-state :fail))))

(defmethod report :error [m]
  (test/with-test-out
    (print clear-line)
    (binding [pretty/*divider* "\r"] (pretty/report m))
    (newline)
    (print-progress (swap! report/*context* update-in [:state] set-state :error))))

(defmethod report :end-test-var [m]
  (test/with-test-out
    (print-progress (swap! report/*context* update-in [:bar] prog/tick))))

(defmethod report :summary [m]
  (test/with-test-out
    (print-progress (swap! report/*context* update-in [:bar] prog/done))
    (pretty/report m)))

(defmethod report :long-test [m]
  (test/with-test-out
    (print clear-line)
    (binding [pretty/*divider* "\r"] (pretty/report m))
    (newline)
    (print-progress @report/*context*)))
