(ns piplin.types
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:use [clojure.string :only [join]])
  (:refer-clojure :exclude [cast])
  (:use piplin.protocols)
  (:require [clojure.pprint]))

(comment
;  TODO: email jim@dueys.net questions about monads
  since the error handling is really quite monadic)

(defonce types (atom (make-hierarchy)))

(defn derive-type
  [child parent]
  (swap! types derive child parent))

(defn isa-type?
  [type unknown]
  (isa? @types unknown type))

(defrecord CompilerError [msg])

(derive-type CompilerError :error)

(defn error
  "Convenience function to define a compiler error"
  [& args]
  (CompilerError. (apply print-str args)))

(comment
    ;TODO: running an experiment to see if this
    ;could return "this" and make it easier to have
    ;if and cond support arbitrary objects at sim time
  (value [this] (throw (IllegalArgumentException.
                        (str "Cannot get value of "
                             this)))))

(extend-protocol ITyped
  Object
  (typeof [this] (:type this))
  (value [this] this)
  (pipinst? [this] false))

(defn kindof [a]
  (when-not (isa-type? :piplin-type (class (typeof a)))
    (when (:kind a)
      (throw+ (error a "might be a type, but this function takes piplin objects")))
    (throw+ (error (typeof a) "is not a piplin type! val =" a)))
  (-> a typeof :kind))

(defmulti promote
  "Produces an instance of the given
  obj with the type changed. Takes
  a type instance and an object to be
  casted."
  (fn [type obj] (:kind type))
  :hierarchy types)

(defmethod promote :default
  [type obj]
  (throw+ (error type "is not a valid type instance")))

(defn error?
  "True iff a is a reportable error"
  [a]
  (or (isa-type? :error (class a))
      (and (coll? a) (seq a) (every? error? a))))

(defmacro try-errors
  "Executes the forms in an implicit do and
  returns the result or errors."
  [& forms]
  `(try+
     ~@forms
     (catch error? ~'e
       ~'e)))

(defn type-unify
  "Takes a target kind and two other
  objects. Determines which of them is the
  target kind, then promotes the other
  to that type instance. Returns errors
  objects if any errors occurred, or if
  any of the objects were errors"
  [target-kind a b]
  (cond
    (= (kindof a) target-kind)
    (let [b (promote (typeof a) b)]
      [a b])
    (= (kindof b) target-kind)
    (let [[b a] (type-unify target-kind b a)]
      [a b])
    :else
    (throw+ (error "Neither" a "nor" b "is of kind" target-kind))))

(defmulti constrain
  "Takes a type and a value and constrains the value to
  the type's range."
  (fn [type val] (:kind type))
  :hierarchy types)

(defmethod constrain
  :default
  [a b] b)

(defmulti check
  "Takes an instance and verifies that it meets the
  constraints of its type"
  kindof
  :hierarchy types)

(defmethod check
  :default
  [a] a)

(defn- unsupported
  [& args]
  (throw (UnsupportedOperationException. (str "Not supported! " (first args)))))

(defmacro def-multi-dispatch
  "This defines a multimethod that throw an informative error by default,
  but allows for ASTNodes to be extended by kind to support various
  internal clojure interfaces, to enable destructuring, mapping, etc."
  [basename]
  (let [name (symbol (str basename "-multi"))]
    `(do
       (defmulti ~name
         (fn [type# & args#]
           (kindof type#))
         :hierarchy types)
       (defmethod ~name :default
         [type# & args#]
         (throw+ (str type# " doesn't support " ~(str basename)))))))

(def-multi-dispatch nth)
(def-multi-dispatch valAt)
(def-multi-dispatch seq)
(def-multi-dispatch cons)
(def-multi-dispatch empty)
(def-multi-dispatch assoc)
(def-multi-dispatch without)
(def-multi-dispatch containsKey)
(def-multi-dispatch entryAt)
(def-multi-dispatch count)

(deftype ASTNode [type map metamap]
  java.lang.Object
  (equals [this other]
    (and (instance? ASTNode other)
         (= (.type ^ASTNode other) type)
         (= (.map ^ASTNode other) map)))
  (hashCode [this]
    (int (mod (+ (.hashCode type) (* 17 (.hashCode map)))
          Integer/MAX_VALUE)))
  (toString [this]
    (print-str "(ASTNode" type map ")"))

  clojure.lang.ILookup
  (valAt
    [this key]
    (valAt-multi this key))
  (valAt
    [this key notfound]
    (valAt-multi this key notfound))

  clojure.lang.IPersistentCollection
  (equiv [this other]
    (.equals this other))
  (cons [this o]
    (cons this o))
  (empty [this]
    (empty-multi this))

  clojure.lang.IPersistentMap
  (assoc [this key val]
    (assoc-multi this key val))
  (assocEx [this key val]
    (assoc-multi this key val))
  (without [this key]
    (without-multi this key))

  clojure.lang.Associative
  (containsKey [this key]
    (containsKey-multi this key))
  (entryAt [this key]
    (entryAt-multi this key))

  clojure.lang.Counted
  (count [this]
    (count-multi this))

  java.lang.Iterable
  (iterator [this]
    (seq this))

  clojure.lang.Seqable
  (seq [this]
    (seq-multi this))

  clojure.lang.Indexed
  (nth [this i]
    (nth-multi this i))
  (nth [this i notfound]
    (nth-multi this i notfound))

  clojure.lang.IMeta
  (meta [this] metamap)
  clojure.lang.IObj
  (withMeta [this metamap] (ASTNode. type map metamap))

  ITyped
  (typeof [this] type)
  (value [this] map)
  (pipinst? [this]
    (let [x ((get metamap :pipinst?) this)]
      x)))

(defmethod clojure.core/print-method ASTNode
  [^ASTNode o ^java.io.Writer w]
  (.write w "AST(")
  (clojure.core/print-method (.type o) w)
  (.write w ", ")
  (clojure.core/print-method (.map o) w)
  (.write w ")"))

(defmethod clojure.pprint/simple-dispatch ASTNode
  [^ASTNode node]
  (.write ^java.io.Writer *out* "AST")
  (clojure.pprint/pprint-logical-block
    :prefix "(" :suffix ")"
    (clojure.pprint/pprint-logical-block
      (.write ^java.io.Writer *out* "type: ")
      (clojure.pprint/pprint-newline :miser)
      (.write ^java.io.Writer *out* (print-str (.type node)))
      (.write ^java.io.Writer *out* " ")
      (clojure.pprint/pprint-newline :linear))
    (clojure.pprint/pprint-logical-block
      (.write ^java.io.Writer *out* "data: ")
      (clojure.pprint/pprint-newline :linear)
      (clojure.pprint/write-out (.map node))
      (.write ^java.io.Writer *out* " ")
      (clojure.pprint/pprint-newline :linear))
    (clojure.pprint/pprint-logical-block
      (.write ^java.io.Writer *out* "meta: ")
      (clojure.pprint/pprint-newline :linear)
      (clojure.pprint/write-out (meta node))
      (.write ^java.io.Writer *out* " "))))
  

(defn instance
  "Creates an instance of the type with value val"
  [type val & more]
  (when (instance? clojure.lang.PersistentArrayMap type)
    (throw+ (error "barf"))) ;TODO: be more helpful
  (when (nil? type)
    (throw+ (error "Type probably shouldn't be nil. value =" val)))
  (let [val (if (some #{:constrain} more)
              (constrain type val)
              val)
        inst (ASTNode. type val
                       {:pipinst?
                        (get (meta type)
                             :pipinst?
                             identity)
                        :sim-factory
                        (get (meta val)
                             :sim-factory
                             [(fn []
                                (apply instance
                                       type
                                       val
                                       more)) []])})
        checked (check inst)]
    (if checked
      inst
      (throw+ (error "Didn't pass check:" type val)))))

(defmacro defpiplintype
  "Creates a piplin type record that implements
  the necessary protocols and has the vector of
  members as the record's members."
  [name args]
  `(do
     (defrecord ~name ~args
       clojure.lang.IFn
       (invoke [~'this ~'x]
         (instance ~'this ~'x))
       (applyTo [~'this  ~'argseq]
         (when-not (= (count ~'argseq) 1)
           (throw (IllegalArgumentException. "1 argument only")))
         (instance ~'this (first ~'argseq))))
     (defmethod clojure.core/print-method ~name [~'o ^java.io.Writer w#]
       (.write w# (str ~(str name)
                       \[
                       (join ", "
                             (map pr-str [~@(map (fn [arg]
                                                  `(~(keyword arg) ~'o)) args)]))
                       \])))
     (derive-type ~name :piplin-type)))

(defn alter-value
  "Takes an ASTNode and alters its value"
  [^ASTNode astnode f & more]
  (ASTNode. (.type astnode)
            (apply f (.map astnode) more)
            (.metamap astnode)))

(defpiplintype AnonType [kind])

(defn anontype
  "Returns an anonymous type of
  the given kind. Useful to promote
  to jvm types."
  [kind]
  (AnonType. kind))

(defn filter-map
  "Returns a map by filtering the values with
  the given predicate"
  [pred map]
  (->> map
    (filter (fn [[k v]] (pred v)))
    (apply concat)
    (apply hash-map)))

(defn immediate-fragment-filter
  "Takes a map and returns 2 maps: one containing
  elements of the initial map whose values are pipinsts,
  and the other containing the non-pipinst values (i.e.
  AST fragments."
  [map]
  [(filter-map pipinst? map)
   (filter-map (comp not pipinst?) map)])

(defmacro mkast-explicit-keys
  "Takes the type, op, args, and function and
  returns an ast fragment."
  [type op kwargs argmap f]
  `(let [[imms# frags#] (immediate-fragment-filter ~argmap)
         kwargs# (filter #((-> frags# keys set) %) ~kwargs)
         constargs# (filter #(not ((-> frags# keys set) %))
                            ~kwargs)
         permargs# (concat constargs# kwargs#)
         type-hack# ~kwargs
         perm# (map #(.indexOf ^java.util.List type-hack# %) permargs#)
         const-vec# (map #(get imms# %) constargs#)
         f# (fn [& args#]
              (let [jumbled-args# (concat const-vec# args#)
                    final-args#
                    (reduce #(assoc %1
                                    (first %2)
                                    (second %2))
                            (vec (repeat (count jumbled-args#) nil))
                            (map vector perm# jumbled-args#))]
                (apply ~f final-args#)))]
     (ASTNode. ~type
               {:op ~op
                :args ~argmap}
               {:pipinst? (fn [& ~'a] false)
                :sim-factory [f# kwargs#]})))

(defmacro mkast
  "Takes the type, op, args, and function and
  returns an ast fragment."
  [type op args f]
  (let [kwargs (vec (map (comp keyword name) args))
        argmap (zipmap kwargs args)]
    `(mkast-explicit-keys ~type ~op ~kwargs ~argmap ~f)))

(defn uninst
  "Takes a pipinst and makes it into an AST frag"
  [pipinst]
  (when-not (pipinst? pipinst)
    (throw+ (error pipinst "must be a pipinist")))
  (let [expr pipinst
        pipinst (mkast (typeof pipinst)
                       :noop
                       [expr]
                       (fn [_] pipinst))]
    (vary-meta pipinst
               assoc :pipinst? (fn [x] false))))

(defn assoc-dist-fn
  "Takes an ast frag and returns a new ast frag w/
  the given distribution fn"
  [ast f]
  (vary-meta ast assoc :distribute f))

(defn log2
  "Log base 2"
  [v]
  (let [log2v (-> (value v)
                Math/log
                (/ (Math/log 2))
                Math/ceil
                int)]
    (promote (typeof v) log2v)))

(defn piplin-clojure-dispatch
  "Returns the kind of the piplin type or
  :use-core-impl if it has no piplin type"
  [x]
  (try+
    (kindof x)
    (catch piplin.types.CompilerError ce
      :use-core-impl)))

;Allow the about types to participate in ITyped
(defn cast
  "Converts the given expr to the given type.
  If the expr is immediate, this returns the
  same thing as (promote type expr). If the
  expr is a runtime value, this returns an
  astfrag."
  [type expr]
  (if (typeof expr)
    (cond
      (= (typeof expr) type)
      expr
      (pipinst? expr)
      (promote type expr)
      (-> expr meta :distribute)
      ((-> expr meta :distribute) type)
      :else ;TODO: fail before making an ast if it'll never succeed
      ;this happens if you try to (cast (bits 2) (uninst #b0))
      (mkast type :cast [expr] (partial cast type)))
    (promote type expr)))

(defn ast-error
  [t msg]
  (mkast t :error [] #(throw (IllegalStateException. "fail"))))
