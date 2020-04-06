(ns piplin.primitives
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
  (:use [piplin.verilog])
  (:require [plumbing.graph :as graph]
            [plumbing.core :as plumb]))

; (defn import-module
;   "Creates a keyword function that represents an imported Verilog module
;     and can be used like the result of `modulize` for `sim`ulation and 
;     generation of Verilog.
;   "
;   [])

; (defn import-fn
;   "Creates an AST node representing a function that is implemented by
;     existing Verilog code.
;     For example, to use existing DSP module defined for Ice40 
;     (https://github.com/YosysHQ/yosys/blob/master/techlibs/ice40/dsp_map.v)
;     `module \$__MUL16X16 (input [15:0] A, input [15:0] B, output [31:0] Y);`
;   "
;   [name import-name])

(defn primitive-parameter [[k v]]
  (when (and (some? v) (not= ::unconnected v))
    (let [parameter-value
          (cond
            (keyword? v) (str \" (name v) \")
            :else (piplin.verilog/verilog-repr v))]
      (str "." (name k) "(" parameter-value ")"))))

(defn primitive-input [name-table [k v]]
  (when (and (some? v) (not= ::unconnected v))
    (let [input-value
          (cond
            (= ::clock v) "clock"
            :else (piplin.verilog/lookup-expr name-table v))]
      (str "." (name k) "(" input-value ")"))))

(defn primitive-output [instance-name k]
  (str "." (name k) "(" (str instance-name "_" (name k)) ")"))

(defn primitive-verilog [primitive-name instance-name parameters inputs outputs]
  (let [path-name (join "_" (map name piplin.modules/*current-module*))]
    (fn [name-table]
      (str
        "  " primitive-name " #(\n    "
        ; TODO if type is set, check valid and stringize value. 
        ; If type is bits, lookup expression to get constant
        (join ",\n    " (filter some? (map primitive-parameter parameters)))
        ")\n"
        "    " (name instance-name) " (\n    "
        (join ",\n    " (filter some? (concat (map (partial primitive-input name-table) inputs)
                                              (map (partial primitive-output path-name) outputs))))
        ");\n"))))

(defn make-primitive
  "Takes a keyword hierarchical name and sim function and returns the primitive
  AST node."
  [primitive-name instance-name parameters input-map outputs output-type sim-fn]
  ; The bundle in the following statement should represent the output values of the primitive?
  (clojure.pprint/pprint parameters)
  
  (when-not (every? #(or (keyword? (second %)) (pipinst? (second %))) parameters)
    (throw+ (str "Some parameters of " instance-name " are not constants.")))

  (alter-value (mkast output-type ::primitive-instance [] sim-fn)
               merge
               {::primitive-verilog (primitive-verilog primitive-name instance-name parameters input-map outputs)}))
                

;  SB_IO #(
;         .PIN_TYPE (SB_IO_TYPE_SIMPLE_INPUT) 
;         .PULLUP (1 'b1)
;  )
;  user_1_io (
;   .PACKAGE_PIN (user_1)
;   .OUTPUT_ENABLE (1 'b0)
;   .INPUT_CLK (clk)
;   .D_IN_0 (user_1_pulled)
;  );

(defmacro device-primitive
  "Define a device-specific primitive.
    For example, the SB_IO on the ICE40.

  (device-primitive \"SB_IO\"
    {
      :PIN_TYPE     [type #b000000]
      :PULLUP       #b0
      :NEG_TRIGGER  #b0
      :IO_STANDARD  \"SB_LVCMOS\"
    }
    {
      :PACKAGE_PIN        :inout
      :LATCH_INPUT_VALUE  :input
      :CLOCK_ENABLE       :input   
      :INPUT_CLK          :input   
      :OUTPUT_CLK         :input   
      :OUTPUT_ENABLE      :input   
      :D_OUT_0            :input   
      :D_OUT_1            :input   
      :D_IN_0             :output  
      :D_IN_1             :output
    }
    (fn [{:keys []}])
  )
  
  Once defined, like
  (def io_primitive (device-primitive ...))
  use primitive within modules like
  any function:
  (modulize
    (let [io_1 (io_primitive parameters)]
      {:module-wire (:output-name (io_1 :input-name 123))
      }))
  "
  ; For all of the parameters, assign them at instantiation
  ;   collect their type from default and match to incoming.
  ; For wire parameters, create a union of all the outputs 
  ;   as the output of the function.
  ; Make function expect all the inputs as inputs
  [primitive-name parameter-defs wire-defs sim-fn]
  (let [inputs (map first (filter (comp #{:input} second) wire-defs))
        input-symbols (map symbol inputs)
        input-map (into {} (map (juxt identity symbol) inputs))
        outputs (vec (map first (filter (comp #{:output} second) wire-defs)))]
    `(fn [parameters#]
      (let [parameter-check# (fn [[p# pv#]]
                              (cond
                                (and (set? (p# ~parameter-defs)) (not ((p# ~parameter-defs) pv#)))
                                (str "Parameter " p# " has value " pv# ". Value must be one of " (p# ~parameter-defs))
                                :else nil))]
        (if (some parameter-check# parameters#)
          (throw+ (error ~primitive-name " parameter errors " (remove nil? (map parameter-check# parameters#)))))
        (clojure.pprint/pprint ["Function capturing parameters returning fnk" parameters#])
        (plumb/fnk [~@input-symbols]
                  (clojure.pprint/pprint ["Fnk taking input and binding primitive" ~@input-symbols])
                  (let [instance-name# (keyword (gensym (str ~primitive-name "_")))
                        output-type# (bundle (into {} (map #(identity [(first %) (uintm 1)]) ; TODO(frankleonrose): Replace (uintm 1) with actual output type
                                                           (filter (comp #{:output :inout} second) ~wire-defs))))]
                    (binding [piplin.modules/*current-module* (conj piplin.modules/*current-module* instance-name#)]
                      (let [instance# (make-primitive ~primitive-name instance-name# parameters# ~input-map ~outputs output-type# ~sim-fn)
                            _# (clojure.pprint/pprint ["Type of:" (typeof instance#)])
                            state-elements# {piplin.modules/*current-module* 
                                             {:piplin.modules/fn instance#}}
                            output-map# (apply merge (map #(identity {%
                                                                      (piplin.modules/make-port* 
                                                                        (conj piplin.modules/*current-module* %)
                                                                        (uintm 1)
                                                                        ::primitive-port)}) ~outputs))  
                            result# (promote output-type# output-map#)]
                        (when (bound? #'piplin.modules/*state-elements*)
                          (clojure.pprint/pprint ["Primitive state-elements:" instance-name# state-elements#])
                          (swap! piplin.modules/*state-elements* merge state-elements#))
                        result#))))))))

; (device-primitive 
;   "SB_IO"
;   {:PACKAGE_PIN        :inout})

