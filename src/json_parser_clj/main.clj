(ns json-parser-clj.main
  (:gen-class)
  (:require [json-parser-clj.lexer-ref-version :as ref-version-lexer])
  (:use [json-parser-clj.lexer]
        [json-parser-clj.parser]
        [json-parser-clj.util]
        [clojure.data.json :as json]
        )
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn json-parse [^String json-path]
  (let [json-str (slurp json-path)]
    (let [[tokens] (tokenize json-str)] (parse tokens))))

(defn json-stringify [ast]
  (json-parser-clj.util/stringify ast))

(defn json-prettify [ast]
  (json-parser-clj.util/prettify ast 2))


(def check-directory (clojure.java.io/file "./data/jsonchecker"))

(defn validate []
  (let [json-files (.listFiles ^File check-directory)
        check-json (fn [file] (let [path (str file)
                                    filename-pass? (or (.contains path "pass") (.endsWith path "EXCLUDE.json"))
                                    [pass? e] (try
                                                (do (json-parse path) [true, nil])
                                                (catch Exception e [false, e]))
                                    match? (= filename-pass? pass?)
                                    ] (if (not match?)
                                        (do
                                          (println path)
                                          (if (not (nil? e)) (do (println (.getMessage ^Exception e))))
                                          (println)
                                          ))))]
    (mapv check-json json-files)))

(def test-files ["./data/twitter.json" "./data/citm_catalog.json" "./data/canada.json"])

(defn p [filename]
  (let [[tokens] (tokenize (slurp (str "./data/jsonchecker/" filename)))
        ast (parse tokens)]
    (println (json-prettify ast))))

(defn -main []
  (do
    (validate)
    ; warmup
    ;(time (dotimes [_ 10] (let [asts (mapv json-parse test-files)
    ;                            _ (mapv json-stringify asts)
    ;                            _ (mapv json-prettify asts)
    ;                            ] nil)))
    (println)
    ; test
    (dotimes [_ 3] (do
                     (let [asts (time (mapv json-parse test-files))
                           _ (time (mapv json-stringify asts))
                           _ (time (mapv json-prettify asts))
                           ] nil)
                     (println)))

    (dotimes [_ 3] (do
                     (time (mapv (fn [path] (let [json-str (slurp path)]
                                              (json/read-str json-str :key-fn keyword)
                                              )) test-files))
                     (println)))
    )
  )
