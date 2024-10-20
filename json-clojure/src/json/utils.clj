(ns json.utils)

(defn illegal-state [& messages]
  (throw (IllegalStateException. (str messages))))

