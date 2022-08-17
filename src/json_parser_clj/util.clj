(ns json-parser-clj.util)

(defn json-minify [ast]
  (case (first ast)
    :j-number (second ast)
    :j-string (str \" (second ast) \")
    :j-bool (second ast)
    :j-null "null"
    :j-array (let [elements (second ast)]
               (str \[ (clojure.string/join "," (map json-minify elements)) \]))
    :j-object (let [kvs (second ast)]
                (str \{ (clojure.string/join "," (map (fn [[k v]] (str \" k \" \: (json-minify v))) kvs)) \}))))

(defn json-stringify [ast indent-width]
  (let [gen-ident (fn [width depth] (apply str (repeat (* depth width) " ")))
        helper (fn helper [depth force-indent? ast]
                 (cond->> (case (first ast)
                            :j-number (str (second ast))
                            :j-string (str \" (second ast) \")
                            :j-bool (second ast)
                            :j-null "null"
                            :j-array (let [elements (second ast)]
                                       (if (empty? elements)
                                         "[]"
                                         (str \[ "\n" (clojure.string/join ",\n" (map (partial helper (inc depth) true) elements)) "\n" (gen-ident indent-width depth) \])))
                            :j-object (let [kvs (second ast)]
                                        (if (empty? kvs)
                                          "{}"
                                          (str \{ "\n"
                                               (clojure.string/join ",\n" (map (fn [[k v]] (str (gen-ident indent-width (inc depth)) \" k \" \: " " ((partial helper (inc depth) false) v))) kvs))
                                               "\n" (gen-ident indent-width depth) \}))))
                          force-indent? (str (gen-ident indent-width depth)))
                 )]
    (helper 0 false ast)))