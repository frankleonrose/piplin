(ns piplin.modules
  (:use [piplin types protocols])
  (:use [clojure.string :only [join]]
        [clojure.set :only [map-invert]])
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :exclude [replace cast])
  (:use [clojure.string :only [join replace split]])
  (:use [swiss-arrows.core :only [-<> -<>>]])
  (:use [clojure.pprint :only [pprint]])
  (:use [piplin.types.bundle])
  (:use [piplin.types.uintm])
  (:require [plumbing.graph :as graph]
            [plumbing.core :as plumb]))

(defn make-port*
  "Takes a keyword hierarchical name, a piplin-type, and
  the port's type and returns the port."
  [name piplin-type port-type]
  (alter-value (mkast piplin-type :port []
                      #(throw+
                         (error "make-port* no longer using sim-fn")))
               merge
               {:port name
                :port-type port-type}))

(declare make-input-map)

(defn- walk-expr
  [expr visit combine]
  (let [x (visit expr)]
    (if-let [args (:args (value expr))]
      (let [subexprs (vals args)]
        (reduce combine x
                (map #(walk-expr % visit combine)
                     subexprs)))
      x)))

(def ^:dynamic *sim-state*)

(defn make-sim-fn
  "Takes a map-zipper of the ast of an expr
  and walks along the expr. It returns a function
  that computes the expr
  and takes no args (needed ports come via binding).
  The function collects its args into a map which it
  binds before invoking the function so that the
  ports can get their values at the bottom."
  [expr]
  (let [[my-sim-fn my-args]
        (if (pipinst? expr)
          [#(identity expr) []]
          (-> expr
            meta
            :sim-factory))]
    (let [args (:args (value expr))
          arg-fns (map #(make-sim-fn (val %)) args)
          arg-map (zipmap (keys args)
                          arg-fns)
          fn-vec (map #(get arg-map %) my-args)]
      (if (= (:op (value expr))
             :port)
        (case (:port-type (value expr))
          :register (fn []
                      (get *sim-state* (:port (value expr))))
          (throw (ex-info "Invalid :port-type" (value expr))))
        (fn []
          (apply my-sim-fn (map #(%) fn-vec)))))))

(defn- push-down-map
  "Takes a map and a keyword maps all values maps with the
  key as the keyword and the value as the old value."
  [m kw]
  (plumb/for-map [[k v] m]
                 k {kw v}))

;;This stores the path of the current module--used for outputing hierarchical data
(def ^:dynamic *current-module* [])
;;This is an atom containing a map of state element paths to a map
;;of their sim fns (::fn) and initial values (::init)
(def ^:dynamic *state-elements*)

(defn module-name [m] (::module-name (meta m)))

;TODO: automatically cast the return values of fnks
;that are being assigned to regs
(defn modulize
  "Creates a keyword function that when executed against an input map 
    will return a mapping from hierarchical names `[:module123 :output-var]`
    to maps that represent AST register ports (with :init, :fn, and :port keys)
    or simple AST functions of existing ports (with just :fn key).
    As a side effect, execution of the returned function will update 
    *state-elements* to include registers defined in the module."
  ([computation state]
   (assert (map? computation)
           "You forgot to include the register map")
   (modulize :module computation state))
  ([module-name computation state]
   ; Look at all state keys that aren't references to nested state (:arrays).
   ; Assert that these keys exist in the computation map. That is, assert
   ; that this module calculates a new value for each of its registers.
   (doseq [[k v] state
           :when (not= (kindof v) :array)]
     (assert (computation k)
             (str "Missing computation for register " k)))
   (let [mname (keyword (gensym (name module-name)))]
     ^::module ^{::module-name mname}
     (fn [& inputs]
       (assert (every? keyword? (take-nth 2 inputs)))
       (binding [*current-module* (conj *current-module* mname)]
         (let [; Rename computation output keys in order to distinguish pre-clock
               ; (input) state from post-clock (output) state.
               state-renames (plumb/for-map [k (keys state)]
                                            k (keyword (name (gensym))))
               reverse-renames (map-invert state-renames)
               renamed-computation
               (plumb/map-keys #(or (state-renames %) %)
                               computation)
               
               ; Create map of state key to AST port with hierarchical name
               register-ports (plumb/for-map [[k v] state]
                                             k (make-port*
                                                (conj *current-module* k)
                                                (typeof v)
                                                :register))

               ; Generate map of AST functions calculating next state values.
               ; Un-rename keys at the end to yield post-clock outputs
               ; in original names.
               result (-<>> (graph/run
                             renamed-computation
                             ; Graph input is merge of module inputs and pre-calc registers
                             ; TODO(frankleonrose): Assert on input overwriting register?
                             (if (seq inputs)
                               (apply assoc register-ports inputs)
                               register-ports))
                            (plumb/map-keys
                             #(or (reverse-renames %) %)))
               ; Filter out operations that have no type or are of type :array-store (RAM?)
               ; That is, focus on operations that generate output values.
               result-fns (plumb/for-map [[k v] result
                                          :when (and (typeof v)
                                                     (not= (-> v value :op) :array-store))]
                                         k v)
               ;We actually want to refer to registers, not their inputs,
               ;in this map
               module-result (merge result-fns register-ports)]
           ; And when *state-elements* is not bound, that is, when we're not in the process of compiling? 
           ; Then we don't collect state elements of this module. Does that mean that *state-elements*
           ; is updated only for the root module?
           (when (bound? #'*state-elements*)
             (let [init-map (push-down-map state ::init)
                   fn-map (push-down-map result-fns ::fn)
                   store-fns (plumb/for-map [[k v] result
                                             :when (= (-> v value :op) :array-store)
                                             :let [{:keys [array index write-enable v]}
                                                   (-> v value :args)]]
                                            k {::index index
                                               ::write-enable? write-enable
                                               ::value v
                                               ::dest array})
                   port-map (push-down-map register-ports ::port)
                   state-elements (->> (merge-with
                                        merge
                                        init-map
                                        fn-map
                                        store-fns
                                        port-map)
                                       (plumb/map-keys
                                        #(conj *current-module* %)))]
               (swap! *state-elements* merge state-elements)))
           
           module-result))))))

(defn primitive?
  "Returns true if the given map represents a primitive ast node"
  [value]
  (= :piplin.primitives/primitive (::op value)))

(defn- store?
  "Returns true if the given map represents a store ast node"
  [value]
  (contains? value ::write-enable?))

(defn register?
  "Returns true if the given map represents a register ast node"
  [value]
  (contains? value ::port)) 

(defn wire?
  "Returns true if the given map represents a wire ast node"
  [value]
  (not (or (store? value) (register? value) (primitive? value))))

(defn compile-root
  [module & inputs]
  (assert (::module (meta module)) "Must pass a module as first argument")
  (binding [*state-elements* (atom {})]
    (apply module inputs)
    (with-meta @*state-elements* {::compiled true})))

(defn- find-exprs
  [compiled-module pred]
  (apply concat
         (map #(-> % second
                 ::fn
                 (walk-expr (fn [expr]
                              (if (and expr (pred expr))
                                [expr]))
                            concat))
              compiled-module)))

(defn find-inputs
  [compiled-module]
  (find-exprs compiled-module
              #(= :input (-> % value :port-type))))

(def ^{:arglists (list '[name type])}
  input #(make-port* %1 %2 :input))

(defn- make-port->mem-name
  "Takes a module and return a map from port to
  the memory's keyword name."
  [compiled-module]
  (let [mems (->> compiled-module
                  (filter #(-> % second ::init))
                  (filter #(-> % second ::init kindof
                               (= :array))))]
    (plumb/for-map [[name {port ::port}] mems]
                   port name)))

(defn- compute-store-fns
  "Given a map of memory registers and a list of store ops,
  make a map from memory names to their simulation-store fns."
  [registers stores]
  ;;TODO: check that registers is a subset of arrays in stores
  (let [stores (map (fn [{dest ::dest :as reg}]
                      (assoc reg
                             ::dest
                             (-> dest value :port)))
                    stores)
        reg->stores (group-by ::dest stores)]
    (plumb/for-map [[reg {f ::fn}] registers
                    :let [stores (->> (reg->stores reg)
                                      (map (comp
                                             (partial map make-sim-fn)
                                             (juxt ::index
                                                   ::write-enable?
                                                   ::value))))
                          f (make-sim-fn f)]]
                   reg (fn []
                         (reduce (fn [mem [i we v]]
                                     (if (we)
                                       (assoc mem (i) (v))
                                       mem))
                                 (*sim-state* reg)
                                 stores)))))

(defn module-keys-by-type
  "Takes a compiled module and returns a map containing
   reg-keys, store-keys, and wire-keys. These are useful
   to compilers, as they're the 3 kinds of runnable code."
  [compiled-module]
  (let [reg-keys (->> compiled-module
                      (filter (comp #(and (register? %) (not= :primitive ((comp :port-type value ::port) %))) second))
                      (map first))
        store-keys (->> compiled-module
                        (filter (comp store? second))
                        (map first))
        wire-keys (->> compiled-module
                       (filter (comp wire? second))
                       (map first))]
    {:reg-keys reg-keys
     :store-keys store-keys
     :wire-keys wire-keys}))

(defn sim
  "Simulate a module for a given number of cycles.
  Critical elements extracted from compiled module:
  - Reg init values - ::init key for registers
  - Wire fns - ::fn for each wire key, fn that maps reg state to val
  - Reg fns - ::fn for registers, fn that maps wire state to val
  - Store fns - "
  [compiled-module cycles]
  (assert (::compiled (meta compiled-module))
          "Module must be compiled")
  (assert (empty? (find-inputs compiled-module))
          "Cannot have any input ports during simulation")
  (let [{:keys [reg-keys store-keys wire-keys]} (module-keys-by-type compiled-module)
        wire-fns (plumb/map-vals (comp make-sim-fn ::fn)
                                 (select-keys compiled-module wire-keys))
        reg-fns (plumb/for-map [[k {f ::fn}] (select-keys compiled-module reg-keys)
                                :when f]
                               k (make-sim-fn f))
        store-fns (compute-store-fns (select-keys compiled-module reg-keys)
                                     (vals (select-keys compiled-module store-keys)))
        port->mem-name (make-port->mem-name compiled-module)
        reg-inits (plumb/map-vals
                   ::init
                   (select-keys compiled-module reg-keys))
        inits (binding [*sim-state* reg-inits]
                (merge reg-inits
                       (plumb/map-vals #(%) wire-fns)))]
    (reductions 
     (fn [state _]
       (let [reg-state (binding [*sim-state* state]
                         (merge state
                                (plumb/map-vals #(%) store-fns)
                                (plumb/map-vals #(%) reg-fns)))]  
         (binding [*sim-state* reg-state]
           (merge reg-state
                  (plumb/map-vals #(%) wire-fns)))))
     inits (range cycles))))
