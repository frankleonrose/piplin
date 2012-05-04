(ns piplin.modules
  (:use [piplin sim types])
  (:use [clojure.string :only [join]])
  (:use [slingshot.slingshot :only [throw+]])
  (:require [clojure.zip :as z]))

(comment
  The syntax for a module is

; (module [:inputs
           [a type
            b type]
           :outputs
           [o1 init-val]
           :feedback
           [x init-val]
           :modules
           [sub1 (instantiate)
            sub2 (instantiate) :only [:port1 :port2]]
;   body...)

  The body should include connections for everything that
  needs one.

  There are 4 sections, provided in this order, each of
  which should have a nonempty list of symbol/value pairs,
  like "let". The value varies from being a type to an
  initialization value to a module instance.

  Module instances can use :exclude and :only like for imports
  to only refer to certain ports and leave the rest unconncted.

  The body can contain connections between registers. It's the
  responsibility of a semantic validation/error checking pass
  over the tree to verify that inputs and outputs are fully
  connected.

  The :inputs section is for inputs to the process. The
  :outputs section is for registered outputs from the
  process. The :feedback section is for registers that
  aren't outputs (kind of like a private :output). :modules
  is for other submodules, and then all the specified ports
  (no spec = all ports) must be connected somewhere in
  the body.
  )

(comment
  All exprs must list their subexprs that should be checked
  as an :args map. This map's keys are used by the synth/sim
  phases to do something specific, but the values are implied
  to be exprs that should be walked and checked.
  )

(let [module-name-cache (atom {})]
 (defn gen-module-name
  "Generates a unique module name out of the given basename
  and arguments. Tries to make the name human-readable."
  [name & args]
   (let [x (let [k (apply vector name args)]
     (if-let [id (get @module-name-cache k)]
       id 
       (let [id (str name "__" (join "_" args))]
         (swap! module-name-cache assoc k id)
         id)))]
     (clojure.pprint/pprint ["x is" x])
     x)))

(defn make-port*
  "Takes a keyword name, an owning module token, and
  the port's type and returns the port."
  [name type]
  (alter-value (mkast type :port []
                      #(throw+
                         (error "no longer using sim-fn")))
               merge
               {:port name}))

(defn connect
  {:dynamic true}
  [reg expr]
  (if (:token reg)
    (throw+ (error "Must call connect within a module"))
    (throw+ (error "Must connect to a register"))))

;TODO: this should probably just be a simple map, not an ASTNode
(defn connect-impl
  "This connects a register to an expr"
  [reg expr]
  (piplin.types.ASTNode. :connection 
            {:args {:reg reg 
                    :expr expr}}
            {}))

(defn module*
  "Takes the module's name, input, output, feedback,
  submodules, and connections. The name is optional, and the
  other 5 arguments are keyword args: :inputs :outputs
  :feedback :modules :connections. The inputs are a map from keywords,
  which are the inputs' names, to their types The outputs and
  feedbacks are the same, but the values of the map are the
  initial values of those state elements. The connections
  are a list of functions which, when evaluated,
  will call connect to populate all the necessary connections."
  [& args]
  (let [mod-name (first args)
        ;`str?` holds whether the module has a given name
        sym? (symbol? mod-name)
        args (if sym? (rest args) args)
        mod-name (symbol (str *ns*)
                         (if sym?
                           (name mod-name)
                           ;TODO: make the number be the line number
                           (name (gensym "module"))))
        
        ;parses keyword arguments, defaulting them to {}
        {:keys [inputs outputs feedback modules connections]}
        (->> (apply hash-map args)
          (merge-with #(or %2 %1)
                      {:inputs {} :outputs {} :feedbock {}
                       :modules {} :connections []})) 
        
        ;We define port objects for each input and state
        ;element. These are given to the functions to make
        ;the deferred ASTs that will be used in
        ;simulation/synthesis.
       
        ;start by merging and rejecting duplicated keywords 
        ;TODO: refactor to use port generation function
        ports (->> (merge-with
                    #(throw+ (error "Duplicate names:" %1 %2))
                    inputs outputs feedback)
                ;Next, we convert all the outputs and feedback to
                ;their types (easier now than doing 2 merges).
                (map #(if (pipinst? %) (typeof %) %))
                ;We finally make each one a port
                (map #(make-port* (key %) (val %))))

        ;To make the connections, we must define a root connect
        ;function that stores to an atom. Then, we can invoke
        ;all of the thunks to populate the connection. Afterwards,
        ;we can do error checking
        body (atom [])
        _ (binding [connect (fn [reg expr]
                              ;TODO: could pass up to parent
                              ;on unmatched tokens (but I think
                              ;I'm removing tokens from ports)
                              (swap! body conj
                                     (connect-impl reg expr)))]
            (doseq [f connections] (f)))]
    {:type :module
     :token mod-name
     :inputs inputs
     :outputs outputs
     :feedback feedback
     :modules modules
     :ports ports
     :body @body}
    ))

(defmacro module
  "TODO: must check for repeated declarations"
  [module-name config & body]
  (let [has-name? (symbol? module-name)
        body (if has-name? body (cons config body))
        config (if has-name? config module-name)
        module-name (if has-name? module-name (gensym "module"))
        ;First, we extract the 4 sections
        {:keys [inputs outputs feedback modules]}
        (into {:inputs [] :outputs []
               :feedback [] :modules []}
              (apply hash-map config))
        
        ;Next, we construct a map from symbols to
        ;their types
        reg-types (map (fn [[k v]]
                         [k `(typeof ~v)])
                       (partition 2
                         (concat outputs feedback)))
        sym->type (concat reg-types
                          (partition 2 inputs))
        
        ;Now we can create the port declarations
        port-decls (mapcat (fn [[k v]]
                          `(~k (make-port* ~(keyword k) ~v)))
                        sym->type)
        
        ;Finally, we need to make the arguments compatible with
        ;module*. To do this, we must keywordize all the symbol
        ;names, and put them into maps
        keywordize (fn [macro-format]
                     (into {} (map (fn [[k v]]
                                     [(keyword k) v])
                                   (partition 2 macro-format))))
        inputs (keywordize inputs)  
        outputs (keywordize outputs)  
        feedback (keywordize feedback)  
        modules (keywordize modules)]
    `(let [~@port-decls]
       (module* '~module-name
                :inputs ~inputs
                :outputs ~outputs
                :feedback ~feedback
                :modules ~modules
                :connections [(fn [] ~@body)]))))

(defmacro defmodule
  "Same as module, but conveniently defs it at the same time"
  [name params & args]
  `(defn ~name ~params (module (symbol (gen-module-name ~name ~params)) ~@args)))

(def ^:dynamic *module-path* [])
(defn walk-modules
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce across the values
  given by (visit module) for every module."
  [ast visit combine]
  (let [x (visit ast)]
    (if (seq (:modules ast))
      (reduce combine x
              (map (fn [[name mod]]
                     (binding [*module-path* (conj *module-path* name)] 
                      (walk-modules mod visit combine)))
                   (:modules ast)))
      x)))

(defn walk-connects
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce operation
  across the values given by (visit connect)
  for every connection in every module"
  [ast visit combine]
  (walk-modules
   ast 
    (fn [ast]
      (doall ;note: force eagerness here for *module-path* correctness
        (map visit
             (filter #(= (typeof %)
                         :connection)
                     (:body ast)))))
    combine))

(defn walk-expr
  [expr visit combine]
  (let [x (visit expr)]
    (if-let [args (:args (value expr))]
      (let [subexprs (vals args)]
        (reduce combine x
                (map #(walk-expr % visit combine)
                     subexprs)))
      x)))

(comment
  How to get the combinational function for ports.

  We find all ports and build the arglist.
  The arglist is going to get used to fill in a map that'll
  be set into a binding when the function is executed.
  As the function is built, all termini will be const or
  ports. The ports' access fn will read the value from
  the binding.
  
  This is done by make-connection and make-sim-fn.)

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
      (if (= (:op (value expr)) :port) 
        (let [path (conj *module-path* (:port (value expr)))]
          (fn []
            (get *sim-state* path)))
        (fn []
          (apply my-sim-fn (map #(%) fn-vec)))))))

(defn make-connection
  "Takes a map-zipper of the ast of a connection
  and returns a pair of [f states]. f is a function
  that takes the needed ports as arguments (listed in
  states), binds them to the sim-fn-arg binding, and
  invokes the no-arg sim-fn to get the result."
  [connection]
  (let [reg (get-in (value connection) [:args :reg])
        expr (get-in (value connection) [:args :expr])
        sim-fn (make-sim-fn expr)
        reg-state (conj *module-path* (:port (value reg))) 
        ports (walk-expr connection 
                         #(if-let [port (:port (value %))]
                            [(conj *module-path* port)]
                            nil)
                         concat)]
    (every-cycle
       (fn [& vals]
         (binding [*sim-state* (zipmap ports vals)]
           (sim-fn)))
       ports
       reg-state)))

(defn- get-qual-state
  "This function takes a module and returns a
  map whose keys are [token port] pairs (token
  is a gensym unique to the module instance and
  port is a keyword) and whose values are the
  initial values of the registers of the module
  (from the :feedback and :outputs sections).
  This state can be used by the simulation
  engine."
  [module]
  (let [token (:token module)
        regs (merge (:outputs module)
                    (:feedback module))]
    (apply hash-map (mapcat (fn [[k v]]
                              [(conj *module-path* k) v])
                            regs))))

(defn make-sim
  "Takes an elaborated hierarchy of modules and returns a
  pair of [state fns] that can be simulated with
  exec-sim. See exec-sim for details."
  [root]
  (let [initial-state (walk-modules root get-qual-state
                                    merge)
        connections (walk-connects root make-connection
                                   concat)
        connections (->> connections
                      (apply concat)
                      (apply hash-map))]
    [initial-state connections]))

(comment
  First make nested modules connect and information hiding
  work properly. Next, add a semantic check to verify
  that everything was connected. This should involve
  several semantic checks and useful errors.

  Next, I must write more functions, like inc, dec, slice,
  bits, concat, and the bits type. I'll also need to write
; if/mux and case.

  At this point we can either try for toVerilog or
  implement structs or vectors, including pattern
  matching aka destructuring. )
