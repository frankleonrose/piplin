(ns piplin.test.primitives
  (:use clojure.test)
  (:refer-clojure :exclude [not= + - * inc dec < > <= >= = cast not bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:require [clojure.core :as clj])
  (:use [piplin.types boolean numbers core-impl binops uintm])
  (:use piplin.primitives)
  (:use [piplin types math modules])
  (:use piplin.test.util
        piplin.types.bits
        piplin.verilog
        plumbing.core)
  (:import clojure.lang.ExceptionInfo))

(deftest device-primitive-test
  (let [_ (clojure.pprint/pprint "Defining io-device")
        io-device (device-primitive "SB_IO" 
                                    {
                                     :PIN_TYPE (bits 6)
                                     :PULLUP (bits 1)
                                     :NEG_TRIGGER (bits 1)
                                     :IO_STANDARD #{:SB_LVCMOS :SB_SSTL2_CLASS_2 :SB_SSTL2_CLASS_1
                                                    :SB_SSTL18_FULL :SB_SSTL18_HALF :SB_MDDR10
                                                    :SB_MDDR8 :SB_MDDR4 :SB_MDDR2}}
                                    {                    
                                    ; :PACKAGE_PIN         :inout
                                    ; :LATCH_INPUT_VALUE   :input
                                     :CLOCK_ENABLE        :input
                                    ; :INPUT_CLK           :input
                                    ; :OUTPUT_CLK          :input
                                    ; :OUTPUT_ENABLE       :input
                                     :D_OUT_0             :input
                                    ; :D_OUT_1             :input
                                     :D_IN_0              :output}
                                    ; :D_IN_1              :output
                                    
                                    #(identity 1)) ; TODO: Make a real sim function

        _ (clojure.pprint/pprint "Defining io-1")
        io-1 (io-device {:PIN_TYPE #b000000 :PULLUP #b0 :IO_STANDARD :SB_LVCMOS})

        _ (clojure.pprint/pprint "Defining mod")
        mod (modulize :root
                      {:x (fnk [x] (inc x))
                       :y (fnk [x] (io-1 { :D_OUT_0 x 
                                          :CLOCK_ENABLE :piplin.primitives/unconnected
                                          :INPUT_CLK :piplin.primitives/clock}))}
                      ;;  :y (io-1 { :D_OUT_0 :x 
                      ;;             :CLOCK_ENABLE :piplin.primitives/unconnected
                      ;;             :INPUT_CLK :piplin.primitives/clock})}
                      {:x ((uintm 8) 0)})
        _ (clojure.pprint/pprint "Compiling mod")
        compiled (compile-root mod)
        _ (clojure.pprint/pprint ["Compiled: " compiled])
        verilog (->verilog compiled {})
        verilog (clojure.string/replace verilog #"x\d+" "x1")
        verilog (clojure.string/replace verilog #"G__\d+" "G__2")
        verilog (clojure.string/replace verilog #"SB_IO_\d+" "SB_IO_3")
        _ (prn ["Generated Verilog: " verilog])
        _ (prn ["Target Verilog: " (slurp "test/resources/primitives.v")])]
      (is (= verilog (slurp "test/resources/primitives.v")))))
          ; (icarus-test (verify mod 100))))
          ; (are [x] (= x (get (last (sim m x)) [(module-name mod) :x]))
          ;   0 5 10)))

;(deftest device-primitive-verilog-test
  ; (let [_ (clojure.pprint/pprint (counter 9))
  ;       _ (clojure.pprint/pprint (compile-root (counter 7)))
  ;       _ (clojure.pprint/pprint (compile-root (nested-counter 11)))]
    ; (icarus-test (verify (counter 8) 100))))

(deftest parameter-check
  (let [test-device (device-primitive "TEST"
                                    {:STRING_PARAMETER #{:X :Y :Z}}
                                    {:in :input}
                                    #(identity 1))]
    (is (thrown? ExceptionInfo
          (test-device {:STRING_PARAMETER :A})))))
