(ns piplin.test.modules
  (:use clojure.test)
  (:refer-clojure :exclude [not= + - * inc dec < > <= >= = cast not bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:require [clojure.core :as clj])
  (:use [piplin.types boolean numbers core-impl binops uintm])
  (:use plumbing.core)
  (:use [piplin types math modules])
  (:use piplin.test.util
        piplin.verilog
        plumbing.core))

(deftest module-counter'
  (let [mod (modulize :root {:x (fnk [x] (inc x))}
                            {:x ((uintm 8) 0)})
        m (compile-root mod)]
    (are [x] (= x (get (last (sim m x)) [(module-name mod) :x]))
         0 5 10)))

;This test should see a counter that grows arithmatically
(deftest submodule-test
  (let [m (modulize
            {:x (fnk [step x] (+ x step))}
            {:x ((uintm 8) 0)})
        root (modulize :root
               {:step (fnk [step] (inc step))
                :output (fnk [step]
                             (:x (m :step step)))}
               {:step ((uintm 8) 0)})
        system (compile-root root)
        do-sim (fn [cycles]
                 (get (last (sim system cycles)) [(module-name root) :output]))]
    (is (= (do-sim 0) ((uintm 8) 0)))
    (is (= (do-sim 1) ((uintm 8) 0)))
    (is (= (do-sim 2) ((uintm 8) 1)))
    (is (= (do-sim 3) ((uintm 8) 3)))
    (is (= (do-sim 4) ((uintm 8) 6)))
    (is (= (do-sim 5) ((uintm 8) 10)))
    (is (= (do-sim 6) ((uintm 8) 15)))
    (is (= (do-sim 7) ((uintm 8) 21)))
    (is (= (do-sim 8) ((uintm 8) 28)))
    (is (= (do-sim 9) ((uintm 8) 36)))))

(def counter
  (modulize {:x (fnk [x] (inc x))} {:x ((uintm 8) 0)}))

(def delayer
  (modulize {:out (fnk [in] in)} {:out ((uintm 8) 0)}))

(def delayer-holder
  (modulize :root {:out (fnk []
                             (let [counter (counter)
                                   delayer (delayer :in (:x counter))]
                               (:out delayer)))}
            nil))

(deftest delayer-test
  (let [m (compile-root delayer-holder)
        root-name (module-name delayer-holder)]
    (are [cycle value] (= (get (last (sim m cycle)) [root-name :out]) value)
         0 0
         1 0
         2 1
         3 2
         4 3
         5 4
         6 5)))

(deftest sim-fail-test
  (is (thrown? java.lang.AssertionError (sim {} 10)))
  (is (thrown? java.lang.AssertionError
               (sim (compile-root
                      (modulize
                        {:a (fnk [x] x)} {})
                      :x (input "foo" (uintm 8)))
                    10))))

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

