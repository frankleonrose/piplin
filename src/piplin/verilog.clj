(ns piplin.verilog
  (:refer-clojure :exclude [replace cast])
  (:require [piplin.walk :as walk]
            [plumbing.core :as plumb])
  (:use [slingshot.slingshot])
  (:use [clojure.walk :only [postwalk]])
  (:use [clojure.set :only [map-invert]])
  (:use [clojure.string :only [join replace]])
  (:use [piplin [modules :exclude [verilog]] types])
  (:use [swiss-arrows.core :only [-<>>]])
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types.sints :only [sign-extend sints]])
  (:use [piplin.types.uintm :only [uintm]])
  (:use [piplin.types.array :only [array?]])
  (:use [piplin.types.complex :only [real-part imag-part]])
  (:use [piplin.types [bits :only [bit-width-of bits deserialize serialize bit-cat bit-slice]]])
  (:require [piplin.mux :as mux]
            [piplin.types.binops :as binops])
  (:use [piplin [types :only [piplin-clojure-dispatch]] protocols [util :only [let']]]))

(defn sanitize-str
  "Takes a string and makes it safe for verilog."
  [s]
  (replace (munge (name s)) "." "_DOT_"))

(declare render-single-expr)

(defn array-width-decl
  "Takes a width `x` and returns the string `\"[x:0]\"`,
  or `\"\"` if x is one"
  [x]
  (if (= 1 x)
    ""
    (str "[" (dec x) ":0]")))

(defn format-verilog
  "Takes a wire name, its width, and the corresponding
  verilog, and returns the formatted `wire` declaration."
  [width name verilog & args]
  (format "  wire %s %s = %s;\n"
          (array-width-decl width) name (apply format verilog args)))

(defn gen-verilog-name
  "Takes a string, symbol, or keyword, and returns a unique
  similar verilog name."
  ([] (name (gensym)))
  ([base-name]
   (-> base-name
     name
     sanitize-str
     gensym
     name)))

(defn make-union-verilog
  [tag padding value]
  (str "{" tag
       (when (pos? padding)
         (str ", " padding "'b" 0))
       ", " value "}"))

(defmulti verilog-repr
  "Generate the Verilog string for an ::immediate value"
  kindof
  :hierarchy types)
(defmethod verilog-repr :default
  [x]
  (throw+ (error "cannot convert" (kindof x) "to verilog:" x)))

;TODO: why must this method be included?
;I cannot figure out where it fits in the output
;probably used by bit-slice as a const, but not actually
;used so it doesn't appear in output
(defmethod verilog-repr :j-long
  [x]
  (str "j-long is unconvertible {" x \}))

(defmethod verilog-repr :uintm
  [x]
  (let [t (typeof x)
        i (value x)
        w (bit-width-of t)]
    (str w "'d" i)))

(defmethod verilog-repr :sints
  [x]
  (let [t (typeof x)
        i (value x)
        w (bit-width-of t)]
    (str (when (neg? i) "-")
         w "'d"
         (if (neg? i) (- i) i))))

(defmethod verilog-repr :sfxpts
  [x]
  (verilog-repr (serialize x)))

(defmethod verilog-repr :complex
  [x]
  (verilog-repr (serialize x)))

(defmethod verilog-repr :boolean
  [x]
  (if x "1'b1" "1'b0"))

(defmethod verilog-repr :bits
  [x]
  (let [t (typeof x)
        b (value x)
        w (bit-width-of t)]
    (if (zero? w)
      "0'b0"
      (str w "'b" (join b)))))

(defmethod verilog-repr :enum
  [x]
  (let [t (typeof x)
        keymap (:keymap t)
        e (value x)
        w (bit-width-of t)]
    (str (verilog-repr (keymap e)))))

(defmethod verilog-repr :union
  [x]
  (let [t (typeof x)
        {:keys [schema enum]} t
        v (first (value x))
        tag (key v)
        v (val v)
        w (bit-width-of t)
        padding (- w
                   (bit-width-of enum)
                   (bit-width-of (tag schema)))]
    (make-union-verilog
      (verilog-repr (enum tag))
      padding
      (verilog-repr v))))
      

(defmethod verilog-repr :bundle
  [x]
  (verilog-repr (serialize x)))

(defmethod verilog-repr :array
  [x]
  (verilog-repr (serialize x)))

(defn lookup-expr
  [table expr]
  (if (pipinst? expr)
    (verilog-repr expr)
    (if-let [name (get table expr)]
      name
      (do
        #_"UNKNOWN_QUANTITY"
       (throw+ (error expr "not found in" table))))))

(defmulti verilog-of
  "Generate a vector wrapped Verilog string for an AST expression"
  (fn [ast name-lookup] (if (pipinst? ast)
                          ::immediate
                          (:op (value ast)))))

(defmethod verilog-of :default
  [ast name-lookup]
  (throw+ (error "cannot convert" (:op (value ast)) "to verilog:" ast)))

(defmethod verilog-of ::immediate
  [ast name-lookup]
  [(verilog-repr ast)])

(defmethod verilog-of :primitive
  [ast name-lookup]
  [(:verilog ast)])

(defmethod verilog-of :port
  [ast name-lookup]
  [(lookup-expr name-lookup ast)])

(defn get-args
  [ast]
  (:args (value ast)))

(defmacro let-args
  [ast name-lookup argvec & body]
  (let [lookups (mapcat #(vector %
                              `(lookup-expr ~name-lookup ~%))
                     argvec)]
    `(let [{:keys ~argvec} (get-args ~ast)
           ~@lookups]
       ~@body)))

(defmethod verilog-of :make-union
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [schema enum]} t
        {:keys [tag val]} (get-args ast)
        w (bit-width-of t)
        padding (- w
                   (bit-width-of enum)
                   (bit-width-of (tag schema)))]
    [(make-union-verilog
      (lookup-expr name-lookup (enum tag))
      padding
      (lookup-expr name-lookup val))]))

(defmethod verilog-of :get-value
  [ast name-lookup]
  (let [valtype (typeof ast)
        union (:u (get-args ast))
        top  (dec (bit-width-of valtype))]
    [(str (lookup-expr name-lookup union)
         "[" top (when-not (zero? top) ":0") "]")]))

(defmethod verilog-of :get-tag
  [ast name-lookup]
  (let [enum (typeof ast)
        union (:u (get-args ast))
        top (dec (bit-width-of (typeof union)))
        bottom (inc (- top (bit-width-of enum)))]
    [(str (lookup-expr name-lookup union)
         "[" top
         (when-not (= top bottom)
           (str ":" bottom))
         "]")]))

(defmethod verilog-of :make-bundle
  [ast name-lookup]
  (let [schema-ks (keys (:schema (typeof ast)))
        bundle-inst (get-args ast)
        ordered-vals (map #(lookup-expr name-lookup
                                        (get bundle-inst %))
                          schema-ks)]
    [(str "{" (join ", " ordered-vals) "}")]))

(defn compute-key-offsets
  "Returns a map from keys to pairs. The pairs
  are the low (inclusive) to high (inclusive)
  bit indices in the verilog representation."
  [bundle-type]
  (let [schema (:schema bundle-type)
        key-widths (map (comp bit-width-of
                              (partial get schema))
                        (keys schema))
        offsets (reductions + (conj key-widths 0))
        total (reduce + key-widths)
        pairs (partition 2 1 (map (partial - total) offsets))
        pairs (map (fn [[x y]] [(dec x) y]) pairs)]
        
    (into {} (map vector (keys schema) pairs))))

(defmethod verilog-of :bundle-assoc
  [ast name-lookup]
  (let [{:keys [bund k v]} (get-args ast)
        t (typeof ast)
        offsets (compute-key-offsets t)
        w (bit-width-of t)
        [high low] (get offsets k)
        v (lookup-expr name-lookup v)
        bund (lookup-expr name-lookup bund)]
    [(str "{"
         (when-not (= (dec w) high)
           (str bund "[" (dec w) ":" (inc high) "], "))
         v
         (when-not (= low 0)
           (str ", " bund "[" (dec low) ":0]"))
         "}")]))

(defmethod verilog-of :bundle-key
  [ast name-lookup]
  (let [{:keys [bundle key]} (get-args ast)
        offsets (compute-key-offsets (typeof bundle))
        [high low] (get offsets key)]
    [(str (lookup-expr name-lookup bundle) "[" high ":" low "]")]))

(defmethod verilog-of :make-array
  [ast name-lookup]
  (let [keys (->> (typeof ast)
               :array-len
               range
               (map (comp keyword str)))
        array-inst (get-args ast)
        ordered-vals (map #(lookup-expr name-lookup
                                        (get array-inst %))
                          keys)]
    [(str "{" (join ", " ordered-vals) "}")]))

(defmethod verilog-of :array-get
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)
        subtype-width (bit-width-of (:array-type (typeof array)))
        index (lookup-expr name-lookup i)]
    [(str
      (lookup-expr name-lookup array)
      "["
      (lookup-expr name-lookup i)
      "]")]))

(defmethod verilog-of :array-nth
  [ast name-lookup]
  (let [{:keys [array i]} (get-args ast)]
    [(lookup-expr name-lookup (get array (keyword (str i))))]))

#_(defmethod verilog-of :array-assoc
   [ast name-lookup]
   (let [{:keys [array index v]} (get-args ast)
         {:keys [array-len array-type]} (typeof array)
         upper-bound (dec (* (bit-width-of array-type) (inc index)))
         lower-bound (* (bit-width-of array-type) index)
         array-sym (lookup-expr name-lookup array)
         arg-sym (lookup-expr name-lookup v)]
     (str "{"
          (when-not (= (dec (bit-width-of (typeof ast))) high)
            (str bund "[" (dec w) ":" (inc high) "], "))
          v
          (when-not (= low 0)
            (str ", " bund "[" (dec low) ":0]"))
          "}")))

(defmethod verilog-of :slice
  [ast name-lookup]
  (let [{:keys [expr high low]} (get-args ast)]
    [(str (lookup-expr name-lookup expr) "[" (dec high) ":" low "]")]))

(defmethod verilog-of :bit-cat
  [ast name-lookup]
  (let-args ast name-lookup [b1 b2]
    (let [{b1-ast :b1 b2-ast :b2} (get-args ast)
          b1-zero? (zero? (-> b1-ast typeof bit-width-of))
          b2-zero? (zero? (-> b2-ast typeof bit-width-of))]
      (cond
        (and b1-zero? b2-zero?)
        (throw+ (error "Cannot concat 2 zero-width signals"))
        b1-zero? [b2]
        b2-zero? [b1]
        :else
        [(str "{" b1 ", " b2 "}")]))))

(defmethod verilog-of :mux2
  [ast name-lookup]
  (let [t (typeof ast)
        {:keys [sel v1 v2]} (get-args ast)]
    [(str (lookup-expr name-lookup sel) " ? "
         (lookup-expr name-lookup v1) " : "
         (lookup-expr name-lookup v2))]))

(defmethod verilog-of :sign-extend
  [ast name-lookup]
  (let [orig-width (-> ast
                     value
                     :args
                     :num
                     typeof
                     bit-width-of)
        new-width (-> ast
                    typeof
                    bit-width-of)]
    (let-args ast name-lookup [num]
      [(format "{ {%d{%s[%d]}}, %s}"
               (- new-width orig-width)
               num (dec orig-width) num)])))

(defmethod verilog-of :real-part
  [ast name-lookup]
  (let [{:as complex-type
         :keys [imag]} (-> (get-args ast)
                         :complex
                         typeof)]
    (let-args ast name-lookup [complex]
      [(format "%s[%d:%d]"
               complex
               (dec (bit-width-of complex-type))
               (bit-width-of imag))])))

(defmethod verilog-of :imag-part
  [ast name-lookup]
  (let [{:keys [imag]} (-> (get-args ast)
                         :complex
                         typeof)]
    (let-args ast name-lookup [complex]
      [(format "%s[%d:0]"
               complex
               (dec (bit-width-of imag)))])))

(defmethod verilog-of :+
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " + " rhs)]
      :sintm
      [(str "$signed(" lhs ") + $signed(" rhs ")")]
      :complex
      (let' [{:keys [lhs rhs]} (get-args ast)
             r1 (real-part lhs)
             i1 (imag-part lhs)
             r2 (real-part rhs)
             i2 (imag-part rhs)
             r' (impl/+ r1 r2)
             i' (impl/+ i1 i2)
             r'-bits (serialize r')
             i'-bits (serialize i')
             result (bit-cat r'-bits i'-bits)
             [name-lookup' body] (walk/compile
                                   result
                                   render-single-expr
                                   name-lookup [])]
        [(name-lookup' result)
         (vec body)])
      :sfxpts
      (let [{:as type
             :keys [i f]} (typeof ast)
            width (bit-width-of type)
            {:keys [lhs rhs]} (get-args ast)
            lhs-sints (->> (serialize lhs)
                        (deserialize (sints width)))
            rhs-sints (->> (serialize rhs)
                        (deserialize (sints width)))]
        (verilog-of (impl/+ lhs-sints
                            rhs-sints)
                    (assoc name-lookup
                           lhs-sints (name-lookup lhs)
                           rhs-sints (name-lookup rhs))))
        
      :sints
      (let [type (typeof ast)
            width (bit-width-of type)

            lhs-tmp-name (gen-verilog-name "lhs")
            lhs-tmp (format-verilog width lhs-tmp-name lhs)
            rhs-tmp-name (gen-verilog-name "rhs")
            rhs-tmp (format-verilog width rhs-tmp-name rhs)

            extended-sum-name (gen-verilog-name "extended_sum")
            extended-sum (format-verilog
                           (inc width)
                           extended-sum-name
                           "{%s[%d],%s} + {%s[%d],%s}"
                           lhs-tmp-name (dec width) lhs-tmp-name
                           rhs-tmp-name (dec width) rhs-tmp-name)

            extended-sum-msb (str extended-sum-name "[" width "]")
            overflow? (str "(" extended-sum-msb " == "
                           extended-sum-name "[" (dec width) "])")

            overflow-case (str "(" extended-sum-msb " == 1'b0) ? "
                               (verilog-repr
                                 (piplin.types.sints/max-value type))
                               " : "
                               (verilog-repr
                                 (piplin.types.sints/min-value type)))]
        [(str overflow? " ? "
              extended-sum-name (array-width-decl width)
              " : " overflow-case)
         [lhs-tmp rhs-tmp extended-sum]])
      (throw+ (error "Cannot add" (kindof ast))))))

(defmethod verilog-of :-
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " - " rhs)]
      :sints
      (let' [{rhs-ast :rhs lhs-ast :lhs} (get-args ast)
             rhs-type (typeof rhs-ast)
             rhs-width (bit-width-of rhs-type)
             rhs-extended-type (sints (inc rhs-width))
             raw-diff (->> rhs-ast
                        (sign-extend (inc rhs-width))
                        serialize
                        impl/bit-not
                        (deserialize (-> rhs-width
                                       inc
                                       uintm))
                        impl/inc
                        serialize
                        (deserialize rhs-extended-type)
                        (impl/+ (sign-extend
                                  (inc rhs-width)
                                  lhs-ast)))
             difference (-<>> raw-diff
                              serialize
                              (bit-slice <> 0 rhs-width)
                              (deserialize rhs-type))
             msb0 (-> raw-diff
                    serialize
                    (bit-slice rhs-width (inc rhs-width)))
             msb1 (-> raw-diff
                    serialize
                    (bit-slice (dec rhs-width) rhs-width))
             overflow? (->> (impl/bit-xor msb0 msb1)
                         serialize
                         (deserialize
                           (anontype :boolean)))
             min-val (piplin.types.sints/min-value
                       rhs-type)
             max-val (piplin.types.sints/max-value
                       rhs-type)
             result (mux/mux2 overflow?
                      (mux/mux2 (binops/= #b0 msb0)
                        max-val
                        min-val)
                      difference)
             [name-lookup' body] (walk/compile
                                   result
                                   render-single-expr
                                   name-lookup [])]
        [(name-lookup' result)
         (vec body)])
      :sfxpts
      (let [{:as type
             :keys [i f]} (typeof ast)
            width (bit-width-of type)
            {:keys [lhs rhs]} (get-args ast)
            lhs-sints (->> (serialize lhs)
                        (deserialize (sints width)))
            rhs-sints (->> (serialize rhs)
                        (deserialize (sints width)))]
        (verilog-of (impl/- lhs-sints
                            rhs-sints)
                    (assoc name-lookup
                           lhs-sints (name-lookup lhs)
                           rhs-sints (name-lookup rhs)))))))
      

(defmethod verilog-of :*
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
    (condp = (kindof ast)
      :uintm
      [(str lhs " * " rhs)]
      :sfxpts
      (let' [{:as type
              :keys [i f]} (typeof ast)
             width (bit-width-of type)
             {:keys [lhs rhs]} (get-args ast)
             lhs-sints (->> (serialize lhs)
                         (deserialize (sints width))
                         (sign-extend (+ f width)))
             rhs-sints (->> (serialize rhs)
                         (deserialize (sints width))
                         (sign-extend (+ f width)))
             sints-* (impl/* lhs-sints rhs-sints)
             [name-lookup' body] (walk/compile
                                   sints-*
                                   render-single-expr
                                   name-lookup [])

             sints-tmp-name (gen-verilog-name "full_result")
             sints-tmp (format-verilog
                         (+ f width)
                         sints-tmp-name
                         (name-lookup' sints-*))]
        [(format "%s[%d:%d]"
                 sints-tmp-name
                 (+ f (dec width))
                 f)
         (conj (vec body) sints-tmp)])
      :complex
      (let' [{:keys [lhs rhs]} (get-args ast)
             r1 (real-part lhs)
             i1 (imag-part lhs)
             r2 (real-part rhs)
             i2 (imag-part rhs)
             r' (impl/- (impl/* r1 r2) (impl/* i1 i2))
             i' (impl/+ (impl/* i1 r2) (impl/* r1 i2))
             r'-bits (serialize r')
             i'-bits (serialize i')
             result (bit-cat r'-bits i'-bits)
             [name-lookup' body] (walk/compile
                                   result
                                   render-single-expr
                                   name-lookup [])]
        [(name-lookup' result)
         (vec body)])
      :sints
      (let [type (typeof ast)
            width (bit-width-of type)

            lhs-tmp-name (gen-verilog-name "lhs")
            lhs-tmp (format-verilog width lhs-tmp-name lhs)
            rhs-tmp-name (gen-verilog-name "rhs")
            rhs-tmp (format-verilog width rhs-tmp-name rhs)

            prod-sym (gen-verilog-name "prod")
            prod (format-verilog
                   width
                   prod-sym
                   "%s * %s"
                   lhs-tmp-name rhs-tmp-name)

            lhs-pos?-sym (gen-verilog-name "lhs_pos")
            rhs-pos?-sym (gen-verilog-name "rhs_pos")
            lhs-pos? (format-verilog 1 lhs-pos?-sym "~%s[%d]" lhs-tmp-name (dec width))
            rhs-pos? (format-verilog 1 rhs-pos?-sym "~%s[%d]" rhs-tmp-name (dec width))

            lhs-leading-0s-sym (gen-verilog-name "lhs_leading_zeros")
            lhs-leading-0s-clauses (->> (range (dec width))
                                     (map #(format "~|%s[%d:%d] ? %d :"
                                                   lhs-tmp-name
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            lhs-leading-0s (format-verilog
                             (log2 width) lhs-leading-0s-sym
                             lhs-leading-0s-clauses)
            rhs-leading-0s-sym (gen-verilog-name "rhs_leading_zeros")
            rhs-leading-0s-clauses (->> (range (dec width))
                                     (map #(format "~|%s[%d:%d] ? %d :"
                                                   rhs-tmp-name
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            rhs-leading-0s (format-verilog
                             (log2 width) rhs-leading-0s-sym
                             rhs-leading-0s-clauses)

            lhs-leading-1s-sym (gen-verilog-name "lhs_leading_ones")
            lhs-leading-1s-clauses (->> (range (dec width))
                                     (map #(format "&%s[%d:%d] ? %d :"
                                                   lhs-tmp-name
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            lhs-leading-1s (format-verilog
                             (log2 width) lhs-leading-1s-sym
                             lhs-leading-1s-clauses)
            rhs-leading-1s-sym (gen-verilog-name "rhs_leading_ones")
            rhs-leading-1s-clauses (->> (range (dec width))
                                     (map #(format "&%s[%d:%d] ? %d :"
                                                   rhs-tmp-name
                                                   (dec width) %
                                                   (- width %)))
                                     (join " ")
                                     (format "%s 1"))
            rhs-leading-1s (format-verilog
                             (log2 width) rhs-leading-1s-sym
                             rhs-leading-1s-clauses)

            pos-pos-ovf?-sym (gen-verilog-name "pos_pos_ovf")
            pos-pos-ovf? (format-verilog
                           1 pos-pos-ovf?-sym
                           "(%s + %s < %d) | ((%s + %s == %d) & %s[%d])"

                           lhs-leading-0s-sym rhs-leading-0s-sym width
                           lhs-leading-0s-sym rhs-leading-0s-sym width
                           prod-sym (dec width))

            neg-neg-ovf?-sym (gen-verilog-name "neg_neg_ovf")
            neg-neg-ovf? (format-verilog
                           1 neg-neg-ovf?-sym
                           "(%s + %s < %d) | ((%s + %s == %d || %s + %s == %d) & (%s[%d] || %s == %s))"
                           lhs-leading-1s-sym rhs-leading-1s-sym width
                           lhs-leading-1s-sym rhs-leading-1s-sym width
                           lhs-leading-1s-sym rhs-leading-1s-sym (inc width)
                           prod-sym (dec width)
                           prod-sym (verilog-repr
                                      ((piplin.types.bits/bits width)
                                       (vec (repeat width 0)))))

            negative-leading-sym (gen-verilog-name "neg_leading")
            negative-leading (format-verilog
                               (log2 width) negative-leading-sym
                               "~%s ? %s : %s"
                               lhs-pos?-sym
                               lhs-leading-1s-sym rhs-leading-1s-sym)
            positive-leading-sym (gen-verilog-name "pos_leading")
            positive-leading (format-verilog
                               (log2 width) positive-leading-sym
                               "~%s ? %s : %s"
                               lhs-pos?-sym
                               rhs-leading-0s-sym lhs-leading-0s-sym)
            neg-pos-ovf?-sym (gen-verilog-name "neg_pos_ovf")
            neg-pos-ovf? (format-verilog
                           1 neg-pos-ovf?-sym
                           "(%s + %s < %d) | ~%s[%d]"
                           negative-leading-sym positive-leading-sym
                           width prod-sym (dec width))

            zero-sym (format "%d'd0" width)
            has-zero?-sym (gen-verilog-name "zero")
            has-zero? (format-verilog
                        1 has-zero?-sym
                        "%s == %s || %s == %s"
                        lhs-tmp-name zero-sym
                        rhs-tmp-name zero-sym)

            min-val (verilog-repr
                      (piplin.types.sints/min-value type))
            max-val (verilog-repr
                      (piplin.types.sints/max-value type))]
            
        [(format "%s ? %s : ((%s & %s & %s) | (~%s & ~%s & %s)) ? %s : ((%s ^ %s) & %s) ? %s : %s"
                 has-zero?-sym zero-sym
                 lhs-pos?-sym rhs-pos?-sym pos-pos-ovf?-sym
                 lhs-pos?-sym rhs-pos?-sym neg-neg-ovf?-sym
                 max-val
                 lhs-pos?-sym rhs-pos?-sym neg-pos-ovf?-sym
                 min-val
                 prod-sym)
                 
         [lhs-tmp rhs-tmp has-zero? prod lhs-pos? rhs-pos? lhs-leading-0s lhs-leading-1s rhs-leading-0s rhs-leading-1s
          pos-pos-ovf? neg-neg-ovf? negative-leading positive-leading neg-pos-ovf?]]))))
        

;; TODO: comparson operations don't work for signed values
(defmethod verilog-of :>
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " > " rhs)]))

(defmethod verilog-of :>=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " >= " rhs)]))

(defmethod verilog-of :<
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " < " rhs)]))

(defmethod verilog-of :<=
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " <= " rhs)]))

(defmethod verilog-of :bit-shift-left
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " << " rhs)]))

(defmethod verilog-of :bit-shift-right
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " >> " rhs)]))

(defmethod verilog-of :bit-and
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " & " rhs)]))

(defmethod verilog-of :bit-or
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " | " rhs)]))

(defmethod verilog-of :bit-xor
  [ast name-lookup]
  (let-args ast name-lookup [lhs rhs]
            [(str lhs " ^ " rhs)]))

(defmethod verilog-of :bit-not
  [ast name-lookup]
  (let-args ast name-lookup [x]
            [(str "~" x)]))

(defmethod verilog-of :not
  [ast name-lookup]
  (let-args ast name-lookup [x]
    [(str "~" x)]))

(defmethod verilog-of :and
  [ast name-lookup]
  (let-args ast name-lookup [x y]
    [(str x " & " y)]))

(defmethod verilog-of :or
  [ast name-lookup]
  (let-args ast name-lookup [x y]
    [(str x " | " y)]))

(defmethod verilog-of :=
  [ast name-lookup]
  [(let [{:keys [x y]} (get-args ast)]
    (str (lookup-expr name-lookup x) " == " (lookup-expr name-lookup y)))])

(defn verilog-noop-passthrough
  ([ast name-lookup]
   (verilog-noop-passthrough ast name-lookup :expr))
   
  ([ast name-lookup passthrough]
   (let [args (get-args ast)]
     (when-not (contains? args passthrough)
       (throw+ (error (str "must have an " passthrough)
                      args)))
     (when (not= 1 (count args))
       (throw+ (error "must have exactly one expr")))
     (verilog-of (passthrough args) name-lookup))))

(defmethod verilog-of :cast
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :noop
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :serialize
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup))
(defmethod verilog-of :deserialize
  [ast name-lookup]
  (verilog-noop-passthrough ast name-lookup :bits))

(defn render-single-expr_ 
  "Takes an expr and a name table and
  returns an updated name table and a string
  to add to the text of the verilog"
  [expr name-table]
  (cond
    (pipinst? expr)
    [(verilog-repr expr) ""]
    (= :use-core-impl (piplin-clojure-dispatch expr))
    (do
      (println "WARNING: trying to render clojure type, skipping")
      [nil ""])
    :else
    (let [name (if-let [let-name (-> expr meta :let-name)]
                 (gen-verilog-name let-name)
                 (gen-verilog-name))
          bit-width (bit-width-of (typeof expr))
          [body structural] (verilog-of expr name-table)]
      [name
       (conj (vec structural)
             (format-verilog bit-width name body))])))

(defn render-single-expr [expr name-table]
  (let [ret (render-single-expr_ expr name-table)
        [_ vlog] ret] 
    ; (prn "Expr:" expr)
    ; (prn "Verilog:" vlog)
    ret))

(defn verilog
  ([expr name-table]
   (verilog expr name-table ""))
  ([expr name-table text]
   (let [[name-table body]
         (walk/compile expr render-single-expr
                       name-table [])]
    ;  (prn "All verilog:" (str text (join body)))
     [name-table (str text (join body))])))

(defn assert-hierarchical
  [indent dut-name var val cycle]
  (let [dut-var var
        assertion-str (str dut-var " !== " val)]
    (str indent "if (" assertion-str ") begin\n"
         indent "  $display(\"Cycle " cycle ": failed assertion: " assertion-str ", instead is %b\", " dut-var ");\n"
         indent "  $finish;\n"
         indent "end\n")))

(defn assert-hierarchical-cycle
  "indent is prepended as indentation (usually whitespace).

  dut-name is the name of the dut module; registers will
  be asserted hierarchically rooted at the dut.

  cycle-map is a map from vectors of keywords representing
  the hierarchical path to a register (excluding the dut's name)
  to the values."
  [indent dut-name cycle-map cycle]
  (reduce (fn [text [path val]]
            (when-not (vector? path)
              (throw+ "Path must be a vector"))
            (let [array? (= :array (kindof val))]
              (str text
                   (if-not array?
                     (assert-hierarchical indent dut-name
                                          (join \_ (map sanitize-str path))
                                          (verilog-repr val)
                                          cycle)
                     "" #_(->> (map (fn [val index]
                                     (assert-hierarchical
                                       indent dut-name
                                       (str (join \. (map sanitize-str path))
                                            \[ index \])
                                       (verilog-repr val)
                                       cycle))
                                val (range))
                           (join "\n"))))))
          ""
          cycle-map))

(defn register-declarations
  "This takes a map from register port objects to verilog names,
  and a seq of register configurations (i.e. maps containing the keys
  `:piplin.modules/port` and `:piplin.modules/init`) and returns a
  string which is a block of `reg [3:0] foo = 4'd7;`-style verilog
  register declarations."
  [port-names module-regs]
  (join (map
          ;The anonymous fn groups the initial value and port object
         (fn [{init :piplin.modules/init
               port :piplin.modules/port}]
            ;declare a register of proper bit width, name,
            ;and default value
           (if (= (kindof init) :array)
              ;TODO some arrays are written with stores,
              ;but others are written all at once. We should
              ;compile the latter to normal registers instead
              ;of verilog memories.
             (let [{:keys [array-type array-len]} (typeof init)
                   inits (map #(format "    %s[%d] = %s;\n"
                                       (port-names port)
                                       %2
                                       (verilog-repr %1))
                              init
                              (range))]
               (format
                "  reg %s %s %s;\n  initial begin\n%s  end\n"
                (-> array-type
                    bit-width-of
                    array-width-decl)
                  ;looks up gensym name
                (port-names port)
                  ;array width
                (array-width-decl array-len)
                (join inits)))
             (format
              "  reg %s %s = %s;\n"
              (-> init
                  typeof
                  bit-width-of
                  array-width-decl)
                ;looks up gensym name
              (port-names port)
                ;must be using an immediate by contract
              (verilog-repr init))))
         module-regs)))

(defn register-assignments
  "This takes a name-table and a seq of register configurations
  (i.e. maps containing the keys `:piplin.modules/port` and
  `:piplin.modules/fn`) and returns a string which is a block of
  `  foo <= my_value48529`\n`-style verilog register assignments."
  [name-table module-regs]
  (join (map
          ;each module-regs entry gets the following:
          ; - convert the register conf to the pair of port+fn
          ; - take that pair and lookup their true names in verilog
          ; - make proper decl
          (comp (fn [[n v]]
                  (format "    %s <= %s;\n" n v))
                (juxt (comp name-table :piplin.modules/port)
                      (comp name-table :piplin.modules/fn)))
          module-regs)))

(defn memory-stores
  "This takes a name-table and a seq of store operations
   and returns a string which is a block of gated
   `  foo <= my_value48529`\n`-style verilog register assignments."
  [name-table stores]
  (join (map
          (fn [{i :piplin.modules/index
                 we :piplin.modules/write-enable?
                 v :piplin.modules/value
                 mem :piplin.modules/dest}]
            (format "    if (%s) %s[%s] <= %s;\n"
                    (name-table we)
                    (name-table mem)
                    (name-table i)
                    (name-table v)))
          stores)))

(defn ->verilog
  [compiled-module outputs]
  (let [primitives (into {} (remove (comp (partial not= :primitive) :op value :piplin.modules/fn second) compiled-module))
        [_ primitive-instances] (reduce (fn [[name-table text] [_ expr]] 
                                          [name-table (str text (:piplin.modules/primitive (value (:piplin.modules/fn expr))))])
                                  [{} ""] primitives)
                                          ; (:piplin.modules/verilog (meta (:piplin.modules/fn expr) name-table text))])
        ; primitive-instances "primitives"
        compiled-module (into {} (remove (comp (partial = :primitive) :op value :piplin.modules/fn second) compiled-module))
        outputs (into {} (remove (fn [[k _]] (not (compiled-module k))) outputs))
        ;seq of all the input ports used in the module
        module-inputs (find-inputs compiled-module)
        ;map from register port object to their verilog names
        port-names (plumb/for-map
                    [[name v] compiled-module]
                    (:piplin.modules/port v) (gen-verilog-name (last name)))
        ;map from input port object to their verilog name
        input-names (plumb/for-map [port module-inputs
                                    :let [name (-> port value :port)]]
                                   port name)

        ;seqs of the keys that are registers, wires, and stores
        {:keys [reg-keys store-keys wire-keys]} (module-keys-by-type compiled-module)

        ;string containing the register decls
        regs-inits (register-declarations
                    port-names
                    (map compiled-module reg-keys))

        ;Compile the main logic
        ;collect all scalar roots
        scalar-roots (map (comp :piplin.modules/fn second) compiled-module)
        ; scalar-roots (remove (comp (partial = :primitive) :op value) scalar-roots)
        memory-roots (mapcat (comp (juxt
                                    :piplin.modules/index
                                    :piplin.modules/write-enable?
                                    :piplin.modules/value
                                    :piplin.modules/dest)
                                   second)
                             compiled-module)
        [name-table code]
        (->> (concat scalar-roots memory-roots)
             ;TODO remove this once nil is synthesizable
             (remove nil?)
             ;could insert a filter here to allow DCE to work
             (reduce (fn [[name-table text] expr]
                       (verilog expr name-table text))
                     [(merge port-names
                             input-names) ""]))
        ;string containing the register assigns
        reg-assigns (register-assignments
                     name-table
                     (->> reg-keys
                          (map compiled-module)
                          (remove (comp array?
                                        :piplin.modules/init))
                          (remove (comp (partial = :primitive) :op value :piplin.modules/fn))))

        ;string containing the memory stores
        memory-stores (memory-stores
                       name-table
                       (map compiled-module store-keys))

        ;string containing the input wire decls
        input-decls (join
                     (for [[port name] input-names
                           :let [width (-> port
                                           typeof
                                           piplin.types.bits/bit-width-of)]]
                       (format "  input wire %s %s;\n"
                               (array-width-decl width) name)))
        ;string containing the output wire decls
        output-decls
        (join (map
               (fn [[path verilog-name]]
                 (let [{function :piplin.modules/fn 
                        port :piplin.modules/port} (value (compiled-module path))]
                   (format
                    "  output wire %s %s;\n"
                    (-> function
                        typeof
                        bit-width-of
                        array-width-decl)
                    verilog-name)))
               outputs))
        ;string containing the assigns linking the output names to the
        ;internal, gensym'ed names
        output-assigns (->> outputs
                            (map (fn [[path verilog-name]]
                                   (let [{p :piplin.modules/port
                                          f :piplin.modules/fn} (compiled-module path)
                                         ;Output gets the value of the
                                         ;register this cycle, not what it will
                                         ;be next cycle
                                         lookup-name (name-table (or p f))]
                                     (format
                                      "  assign %s = %s;\n"
                                      verilog-name
                                      lookup-name))))
                            join)
        ;string containing the port names for pre-decl in module arg list
        port-names (->> (merge input-names outputs)
                        vals
                        (map #(str "  " % ",\n"))
                        join)]
    (str "module piplin_module(\n"
      "  input clock,\n"
      port-names
      ");\n"
      "  input wire clock;\n"
      "  //Input and output declarations\n"
      input-decls
      output-decls
      "\n  //Registers\n"
      regs-inits
      "\n  // Primitives\n"
      primitive-instances
      "\n  //Main code\n"
      code
      "\n  //Assignments to outputs\n"
      output-assigns
      "\n"
      "  always @(posedge clock) begin\n"
      reg-assigns
      memory-stores
      "  end\n"
      "endmodule\n")))
         

(defn verify
  [module cycles]
  (let [compiled (compile-root module)
        compiled (into {} (remove (comp (partial = :primitive) :op value :piplin.modules/fn second) compiled))
        compiled (with-meta compiled {:piplin.modules/compiled true})
        ;These are the keys in the `compiled` map
        ;that correspond to scalar outputs
        scalar-output-keys (->> compiled
                                (filter
                                 (fn [[k v]]
                                   (or (wire? v)
                                       (and (register? v)
                                            (-> v
                                                :piplin.modules/init 
                                                array?
                                                not)))))
                                (map first))
        flattened-string-keys (map (comp
                                    sanitize-str
                                    #(join "_" (map name %)))
                                   scalar-output-keys)
        plain-path->gensym-path (plumb/for-map [path flattened-string-keys]
                                               path (gen-verilog-name path))
        gensym-path->plain-path (plumb/for-map [[k v] plain-path->gensym-path]
                                               v k)
        output-renames (zipmap scalar-output-keys
                               (map plain-path->gensym-path
                                    flattened-string-keys))
        samples (sim compiled cycles)
        dut-name "dut"]
    (str
      (->verilog compiled output-renames)
      "\n"
      "module test;\n"
      "  reg clk = 0;\n"
      "  always #5 clk = !clk;\n"
      "\n"
      (join "\n"
            (map #(str "  wire " (-> (key %)
                                   compiled
                                   :piplin.modules/fn
                                   typeof
                                   bit-width-of
                                   array-width-decl)
                       \space
                       (gensym-path->plain-path (val %))
                       \;)
                 output-renames))
      "\n"
      "  piplin_module " dut-name "(\n"
      "    .clock(clk),\n"
      (join ",\n"
              (map #(str "    ." % \( (gensym-path->plain-path %) \))
                   (vals output-renames)))
      "\n  );\n"
      "\n"
      "  initial begin\n"
      ;"    $dumpfile(\"dump.vcd\");\n"
      "    $dumpvars;\n"
      "    #1\n"
      (->> samples

        (map #(assert-hierarchical-cycle "    "
                                         dut-name
                                         %2 %1)
             (range))
        (map #(str % "    #10\n"))
        join)
      "    $display(\"test passed\");\n"
      "    $finish;\n"
      "  end\n"
      "endmodule")))
