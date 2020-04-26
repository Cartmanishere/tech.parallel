(ns tech.parallel.utils)


(defn qualified-symbol?
  [x] (boolean (and (symbol? x) (namespace x) true)))


(defn- serialized-require
  [& args]
  (locking (Object.)
    (apply require args)))


(defn requiring-resolve
  [sym]
  (if (qualified-symbol? sym)
    (or (resolve sym)
        (do (-> sym namespace symbol serialized-require)
            (resolve sym)))
    (throw (IllegalArgumentException. (str "Not a qualified symbol: " sym)))))


(defmacro export-symbols
  [src-ns & symbol-list]
  `(do
     (require '~src-ns)
     ~@(->> symbol-list
            (mapv
             (fn [sym-name]
               `(let [varval# (requiring-resolve (symbol ~(name src-ns)
                                                         ~(name sym-name)))
                      var-meta# (meta varval#)]
                  (when (:macro var-meta#)
                    (throw
                     (ex-info
                      (format "Cannot export macros as this breaks aot: %s"
                              '~sym-name)
                      {:symbol '~sym-name})))
                  (def ~(symbol (name sym-name)) @varval#)
                  (alter-meta! #'~(symbol (name sym-name))
                               merge
                               (select-keys var-meta#
                                            [:file :line :column
                                             :doc
                                             :column :tag
                                             :arglists]))))))))
