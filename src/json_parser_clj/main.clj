(ns json-parser-clj.main
  (:gen-class)
  (:require [json-parser-clj.lexer-ref-version :as ref-version-lexer])
  (:use [json-parser-clj.lexer]
        [json-parser-clj.parser]
        [json-parser-clj.util]))

(set! *warn-on-reflection* true)


;(let [json-str (slurp json-path)]
;  (do
;    (let [[tokens] (tokenize json-str)
;          ast (parse tokens)] (do
;                                (println (json-minify ast))
;                                (println (json-stringify ast 2))))
;
;    (let [tokens (ref-version-lexer/tokenize json-str)
;          ast (parse tokens)] (do
;                                (println (json-minify ast))
;                                (println (json-stringify ast 2))))
;
;    (time (dotimes [_ 100] (tokenize json-str)))
;    (time (dotimes [_ 100] (ref-version-lexer/tokenize json-str)))
;    ))

(defn json-parse [^String json-path]
  (let [json-str (slurp json-path)]
    (let [[tokens] (tokenize json-str)] (parse tokens))))

(defn json-stringify [^String ast]
  (json-parser-clj.util/stringify ast))

(defn json-prettify [^String ast]
  (json-parser-clj.util/prettify ast 2))


(def check-directory (clojure.java.io/file "./data/jsonchecker"))

(defn validate []
  (let [json-files (.listFiles check-directory)
        check-json (fn [file] (let [path (str file)
                                    _ (try
                                        (do (json-parse path)
                                            (println (str path " passed!")))
                                        (catch Exception e
                                          (println (str path " failed!"))))
                                    ] nil))]
    (mapv check-json json-files)))

(def test-files ["./data/twitter.json" "./data/citm_catalog.json" "./data/canada.json"])

(defn -main []
  (do
    ;(validate)
    ; warmup
    (time (dotimes [_ 10] (let [asts (mapv json-parse test-files)
                               _ (mapv json-stringify asts)
                               ;_ (mapv json-prettify asts)
                               ] nil)))
    (println)
    ; test
    (dotimes [_ 3] (do
                     (let [asts (time (mapv json-parse test-files))
                           _ (time (mapv json-stringify asts))
                           _ (time (mapv json-prettify asts))
                           ] nil)
                     (println)))
    )
  )
