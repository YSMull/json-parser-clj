(ns json-parser-clj.main
  (:use [json-parser-clj.lexer]
        [json-parser-clj.parser]))


(defn -main
  []
  (let [[tokens] (tokenize (slurp "./test.json"))
        ast (parse tokens)]
    (do
      ;(println ast)
      (println (json-minify ast))
      (println (json-stringify ast 4))
      )))
