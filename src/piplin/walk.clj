(ns piplin.walk
  (:refer-clojure :exclude [compile cast])
  (:use [piplin types protocols]))

(defn walk
  "Takes an expr, a function to reduce over with a depth
  first postwalk, and an initial value and reduces the expr."
  [expr f init])
  

(defn compile
  "Takes an expr, a function that takes an expr and a map
  of exprs to names and returns the form for the expr and
  its generated name, and it returns a list of all the
  forms in order.

  You can provide pre-initialized name-lookup or form list."
  [expr expr->name+body name-table body]
  (letfn [(render-expr [expr name-table body]
            (if (contains? name-table expr)
              [name-table body]
              (let [[name body']
                    (expr->name+body expr name-table)]
                (if name
                  [(assoc name-table
                          expr
                          name)
                   (concat body body')]
                  [name-table body]))))]
    (if (pipinst? expr)
      (render-expr expr name-table body)
      (let [args (vals (-> expr value :args))
            [name-table body]
            (if (seq args)
              (reduce
                (fn [[name-table body] expr]
                  (compile expr
                           expr->name+body
                           name-table
                           body))
                [name-table body]
                args)
              [name-table body])]
        (render-expr expr name-table body)))))
