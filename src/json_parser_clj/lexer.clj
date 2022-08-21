(ns json-parser-clj.lexer
  (:use [json-parser-clj.token]))

(defn- t-null [string]
  (if (= "null" (clojure.string/lower-case (apply str (take 4 string))))
    [(make-token :NULL) (drop 4 string)]
    (throw (Exception. (str "null error:" string)))))

(defn- t-bool [string]
  (let [maybe-true (clojure.string/lower-case (apply str (take 4 string)))
        maybe-false (clojure.string/lower-case (apply str (take 5 string)))]
    (cond
      (= "true" maybe-true) [(make-token :BOOL true) (drop 4 string)]
      (= "false" maybe-false) [(make-token :BOOL false) (drop 5 string)]
      :else (throw (Exception. (str "bool error:" string))))))

(defn- t-string [string]
  (loop [token (StringBuilder. "")
         s (rest string)]
    (let [[x & xs] s]
      (cond
        (or (= x \newline) (= x \tab) (nil? x)) (throw (Exception. (str "string error:" (apply str string))))
        (= x \") [(make-token :STRING (.toString token)) xs]
        (and (= x \\)) (let [c (first xs)]
                         (if (or (= c \\) (= c \/) (= c \b) (= c \f) (= c \n) (= c \r) (= c \t) (= c \u) (= c \"))
                           (recur (-> token
                                      (.append \\)
                                      (.append c)) (rest xs))
                           (throw (Exception. (str "string error:" (apply str string))))))
        :else (recur (.append token x) xs)))))

(defn- t-number [string]
  (loop [token (StringBuilder. "")
         [x & xs :as cur] string]
    (if (or (Character/isDigit ^char x) (= x \-) (= x \+) (= x \.) (= x \e) (= x \E))
      (recur (.append token x) xs)
      (let [num-str (.toString token)
            leading-zeros? (let [int-part (first (clojure.string/split num-str #"\."))]
                             (if (= 1 (count int-part))
                               false
                               (.startsWith int-part "0")))
            _ (when leading-zeros?
                (throw (Exception. (str "leading zeros:" num-str))))
            double? (some (fn [c] (or (= c \.) (= c \e) (= c \E))) num-str)]
        [(make-token :NUMBER (if double?
                               (Double/parseDouble (.toString token))
                               (Long/parseLong (.toString token))))
         cur]))))

(defn tokenize [string]
  (if (empty? string)
    []
    ;else
    (loop [tokens (transient [])
           [x & xs :as cur] string]
      (cond
        (nil? x) [(persistent! tokens) nil]
        (or (= \space x) (= \newline x) (= \tab x) (= \return x)) (recur tokens xs)
        (= x \{) (recur (conj! tokens (make-token :L-CURLY-BRACKET)) xs)
        (= x \}) (recur (conj! tokens (make-token :R-CURLY-BRACKET)) xs)
        (= x \[) (recur (conj! tokens (make-token :L-SQUARE-BRACKET)) xs)
        (= x \]) (recur (conj! tokens (make-token :R-SQUARE-BRACKET)) xs)
        (= x \,) (recur (conj! tokens (make-token :COMMA)) xs)
        (= x \:) (recur (conj! tokens (make-token :COLON)) xs)
        (= x \n) (let [[token res] (t-null cur)] (recur (conj! tokens token) res))
        (or (= x \t) (= x \f)) (let [[token res] (t-bool cur)] (recur (conj! tokens token) res))
        (= x \") (let [[token res] (t-string cur)] (recur (conj! tokens token) res))
        (or (= x \-) (Character/isDigit ^char x)) (let [[token res] (t-number cur)] (recur (conj! tokens token) res))
        :else (do
                (throw (Exception. (str "string error:" cur))))))))