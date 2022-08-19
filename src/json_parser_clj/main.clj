(ns json-parser-clj.main
  (:use [json-parser-clj.lexer]
        [json-parser-clj.parser]
        [json-parser-clj.util]))


(defn -main
  []
  (let [json-str (slurp "./test.json")]
    (do
      ;(println ast)
      (dotimes [_ 5000] (tokenize json-str))
      (time (dotimes [_ 1000] (tokenize json-str)))
      ;(println (json-minify ast))
      ;(println (json-stringify ast 2))
      ))
  )
