(ns probability.core)

(defn factorial
  ([n] (factorial n 1))
  ([n accum] (if (pos? n)
               (recur (dec n) (* n accum))
               accum)))
(defn ! [n] (factorial (bigint n)))

(defn choose [n r] (/ (! n) (* (! r) (! (- n r)))))

(defmacro summa [[var-name from to] & body] `(apply + (map (fn [~var-name] ~(if (seq? body) (first body) body)) (range ~from ~(inc to)))))
