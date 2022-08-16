(ns json-parser-clj.lexer
  (:use [json-parser-clj.token]))

(defn- t-null [string]
  (let [[a b c d & xs] string]
    (cond
      (= "null" (clojure.string/lower-case (str a b c d))) [(make-token :NULL) xs]
      :else (throw (Exception. (str "null error:" string)))
      )))

(defn- t-bool [string]
  (let [maybe-true (clojure.string/lower-case (apply str (take 4 string)))
        maybe-false (clojure.string/lower-case (apply str (take 5 string)))]
    (cond
      (= "true" maybe-true) [(make-token :BOOL "true") (drop 4 string)]
      (= "false" maybe-false) [(make-token :BOOL "false") (drop 5 string)]
      :else (throw (Exception. (str "bool error:" string)))
      )))

(defn- t-string [string]
  (loop [token ""
         s (rest string)]
    (let [[x & xs] s]
      (cond
        (nil? x) (throw (Exception. (str "string error:" string)))
        (= x \") [(make-token :STRING token) xs]
        (= x \\) (recur (str token \\ (first xs)) (rest xs))
        :else (recur (str token x) xs)
        ))))

(defn- t-number [strs]
  (loop [token ""
         has-dot false
         [x & xs :as cur] strs]
    (cond
      (nil? x) [(make-token (Double/parseDouble token) nil)]
      (Character/isDigit ^char x) (recur (str token x) has-dot xs)
      (= (str x) ".") (if has-dot
                        (throw (Exception. (str "number error:" strs)))
                        (recur (str token x) true xs))
      :else [(make-token :NUMBER (if has-dot (Double/parseDouble token) (Long/parseLong token))) cur]
      )))

(defn tokenize [strs]
  (if (empty? strs)
    []
    ;else
    (loop [tokens []
           [x & xs :as cur] strs]
      (cond
        (nil? x) [tokens nil]
        (re-matches #"[ \r\t\n]" (str x)) (recur tokens xs)
        (= x \{) (recur (conj tokens (make-token :L-CURLY-BRACKET)) xs)
        (= x \}) (recur (conj tokens (make-token :R-CURLY-BRACKET)) xs)
        (= x \[) (recur (conj tokens (make-token :L-SQUARE-BRACKET)) xs)
        (= x \]) (recur (conj tokens (make-token :R-SQUARE-BRACKET)) xs)
        (= x \,) (recur (conj tokens (make-token :COMMA)) xs)
        (= x \:) (recur (conj tokens (make-token :COLON)) xs)
        (= x \n) (let [[token res] (t-null cur)] (recur (conj tokens token) res))
        (or (= x \t) (= x \f)) (let [[token res] (t-bool cur)] (recur (conj tokens token) res))
        (= x \") (let [[token res] (t-string cur)] (recur (conj tokens token) res))
        (Character/isDigit ^char x) (let [[token res] (t-number cur)] (recur (conj tokens token) res))
        :else (do
                (throw (Exception. (str "string error:" cur))))
        ))))