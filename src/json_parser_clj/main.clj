(ns json-parser-clj.main
  (:use [json-parser-clj.lexer]
        [json-parser-clj.parser]
        [json-parser-clj.util]))

(set! *warn-on-reflection* true)

(defn -main
  []
  (let [json-str (slurp "./test.json")]
    (do
      (let [[tokens] (tokenize json-str)
            ast (parse tokens)] (do
                                  (println (json-minify ast))
                                  (println (json-stringify ast 2))))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      (time (dotimes [_ 1000] (tokenize json-str)))
      ))
  )
