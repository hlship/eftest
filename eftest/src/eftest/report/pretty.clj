(ns eftest.report.pretty
  "A test reporter with an emphasis on pretty formatting."
  (:require [clojure.test :as test]
            [clojure.data :as data]
            [clj-commons.ansi :as ansi]
            [clj-commons.format.exceptions :as exception]
            [clj-commons.pretty.repl :as repl]
            [puget.printer :as puget]
            [fipp.engine :as fipp]
            [eftest.output-capture :as capture]
            [eftest.report :as report]
            [clojure.string :as str]))

(def ^:dynamic *fonts*
  "The ANSI codes to use for reporting on tests."
  {:exception     :red
   :message       :italic
   :property      :bold
   :source        :italic
   :function-name :blue
   :clojure-frame :white
   :java-frame    nil
   :omitted-frame nil
   :pass          :green
   :fail          :red
   :error         :red
   :divider       :yellow})

(def ^:dynamic *divider*
  "The divider to use between test failure and error reports."
  "\n")

(defn- testing-scope-str
  "Produce a composed string identifying the testing path, scope, and file and line if known."
  [{:keys [file line]}]
  (let [[ns scope] report/*testing-path*]
    (list
     (cond
       (keyword? scope)
       (list
         [(:clojure-frame *fonts*) (ns-name ns)]
         " during "
         [(:function-name *fonts*) scope])

       (var? scope)
       (list
         [(:clojure-frame *fonts*) (ns-name ns) "/"
          [(:function-name *fonts*) (:name (meta scope))]]))
     (when (or file line)
       (list
         " ("
         [(:source *fonts*) file ":" line]
         ")")))))

(defn- diff-all [expected actuals]
  (map vector actuals (map #(take 2 (data/diff expected %)) actuals)))

(defn- pretty-printer []
  (puget/pretty-printer {:print-color true
                         :print-meta false}))

(defn- pprint-document [doc]
  (fipp/pprint-document doc {:width 80}))

(defn- equals-fail-report [{:keys [actual]}]
  (let [[_ [_ expected & actuals]] actual
        p (pretty-printer)]
    (doseq [[actual [a b]] (diff-all expected actuals)]
      (pprint-document
        [:group
         [:span "expected: " (puget/format-doc p expected) :break]
         [:span "  actual: " (puget/format-doc p actual) :break]
         (when (and (not= expected a) (not= actual b))
           [:span "    diff: "
            (if a
              [:span "- " (puget/format-doc p a) :break])
            (if b
              [:span
               (if a  "          + " "+ ")
               (puget/format-doc p b)])])]))))

(defn- predicate-fail-report [{:keys [expected actual]}]
  (let [p (pretty-printer)]
    (pprint-document
      [:group
       [:span "expected: " (puget/format-doc p expected) :break]
       [:span "  actual: " (puget/format-doc p actual)]])))

(defn- print-stacktrace [t]
  (binding [exception/*traditional* true
            exception/*fonts* *fonts*]
    (repl/pretty-print-stack-trace t test/*stack-trace-depth*)))

(defn- error-report [{:keys [expected actual]}]
  (if expected
    (let [p (pretty-printer)]
      (pprint-document
       [:group
        [:span "expected: " (puget/format-doc p expected) :break]
        [:span "  actual: " (with-out-str (print-stacktrace actual))]]))
    (print-stacktrace actual)))

(defn- print-output [output]
  (let [divider (:divider *fonts*)]
    (when-not (str/blank? output)
      (ansi/pcompose [divider "---"] " Test output " [divider "---"]
                     \n
                     (str/trim-newline output)
                     \n
                     [divider "-------------------"]))))

(defmulti report
  "A reporting function compatible with clojure.test. Uses ANSI colors and
  terminal formatting to produce readable and 'pretty' reports."
  :type)

(defmethod report :default [m])

(defmethod report :pass [m]
  (test/with-test-out (test/inc-report-counter :pass)))

(defmethod report :fail [{:keys [message expected] :as m}]
  (test/with-test-out
    (test/inc-report-counter :fail)
    (print *divider*)
    (ansi/pcompose [(:fail *fonts*) "FAIL"] " in " (testing-scope-str m))
    (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
    (when message (println message))
    (if (and (sequential? expected)
             (= (first expected) '=))
      (equals-fail-report m)
      (predicate-fail-report m))
    (print-output (capture/read-test-buffer))))

(defmethod report :error [{:keys [message expected actual] :as m}]
  (test/with-test-out
    (test/inc-report-counter :error)
    (print *divider*)
    (ansi/pcompose [(:error *fonts*) "ERROR"] " in " (testing-scope-str m))
    (when (seq test/*testing-contexts*) (println (test/testing-contexts-str)))
    (when message (println message))
    (error-report m)
    (some-> (capture/read-test-buffer) (print-output))))

(defn- pluralize [word count]
  (if (= count 1) word (str word "s")))

(defn- format-interval [duration]
  (format "%.3f seconds" (double (/ duration 1e3))))

(defmethod report :long-test [{:keys [duration] :as m}]
  (test/with-test-out
    (print *divider*)
    (ansi/pcompose [(:fail *fonts*) "LONG TEST"] " in " (testing-scope-str m))
    (when duration (println "Test took" (format-interval duration) "seconds to run"))))

(defmethod report :summary [{:keys [test pass fail error duration]}]
  (let [total (+ pass fail error)
        color (if (= pass total) (:pass *fonts*) (:error *fonts*))]
    (test/with-test-out
      (ansi/pcompose *divider*)
       "Ran " test " tests in" (format-interval duration)
      [color
       total " " (pluralize "assertion" total) ", "
       fail " " (pluralize "failure" fail) ", "
       error " " (pluralize "error" error) "."])))
