(ns piplin.test.primitives
  (:use clojure.test)
  (:refer-clojure :exclude [not= + - * inc dec < > <= >= = cast not bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:require [clojure.core :as clj])
  (:use [piplin.types boolean numbers core-impl binops uintm])
  (:use plumbing.core)
  (:use piplin.primitives)
  (:use [piplin types math modules])
  (:use piplin.test.util
        piplin.verilog
        plumbing.core))

(deftest device-primitive-test
  (let [_ (clojure.pprint/pprint "Defining io_device")
        io_device (device-primitive "SB_IO" {} {:in-value :input :out-value :output} #(identity 1))
        _ (clojure.pprint/pprint "Defining io_1")
        io_1 (io_device {:PIN_TYPE "SB_IO_TYPE_SIMPLE_INPUT" :PULL_UP 123})
        _ (clojure.pprint/pprint "Defining mod")
        mod (modulize :root
                      {:x (fnk [x] (inc x))
                       :y (fnk [] (io_1 {:in-value 123}))}
                      {:x ((uintm 8) 0)})
        _ (clojure.pprint/pprint "Compiling mod")
        compiled (compile-root mod)
        _ (clojure.pprint/pprint ["Compiled: " compiled])
        verilog (->verilog compiled {})
        _ (prn ["Verilog: " verilog])]
    (is (= verilog ""))))
  ; (icarus-test (verify mod 100))))
  ; (are [x] (= x (get (last (sim m x)) [(module-name mod) :x]))
  ;   0 5 10)))

;(deftest device-primitive-verilog-test
  ; (let [_ (clojure.pprint/pprint (counter 9))
  ;       _ (clojure.pprint/pprint (compile-root (counter 7)))
  ;       _ (clojure.pprint/pprint (compile-root (nested-counter 11)))]
    ; (icarus-test (verify (counter 8) 100))))

