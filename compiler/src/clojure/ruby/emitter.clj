(ns clojure.ruby.emitter
  (:require [clojure.string]
            [clojure.tools.analyzer.utils :refer [resolve-var]]
            [clojure.ruby.runtime         :refer [var->constant lookup-var-in-module]]
            [clojure.ruby.util            :refer :all]))

(defn- endl-separate [exprs]
  (clojure.string/join "\n" exprs))

(defn- indent [s]
  (endl-separate
    (map
      #(str "  " %)
      (clojure.string/split s #"\n"))))

(defn- comma-separate [exprs]
  (clojure.string/join ", " exprs))

(defn wrap-parens [expr]
  (format "(%s)" expr))

(defn equals [x y]
  (format "%s == %s" x y))

(defn- wrap-quotes [s]
  (format "\"%s\"" s))

(defmulti -emit (fn [{:keys [op] :as ast} _] op))

(defn- emit-var [{:keys [ns runtime]} sym root]
  (let [module-name (namespace->module (name ns))
        const-name   (or (lookup-var-in-module runtime module-name ns sym)
                       (clojure.string/upper-case (name (gensym "var"))))
        symbol (format "Clojure::Core::Symbol.new(%s)"
                       (comma-separate
                         (map wrap-quotes [(name ns)
                                            (.getName sym)
                                            (str (name ns) "/" (.toString sym))])))
        new-var (format "Clojure::Core::Var.new(%s)" (comma-separate [symbol root]))]
    (format "%s.const_set(\"%s\", %s)" module-name const-name new-var)))

(defn- emit-type [ns name fields interfaces methods]
  (let [arg-names (map :name fields)
        arglist (clojure.string/join ", " arg-names)
        ivar   #(format "@%s = %s" % %)
        ivars (endl-separate (map ivar arg-names))
        attr-reader #(format "attr_reader :%s" %)
        attr-readers (endl-separate (map attr-reader arg-names))
        constructor (format "def initialize(%s)\n%s\nend" arglist (indent ivars))
        body (endl-separate [attr-readers constructor])]
    (format "%s.const_set(\"%s\", Class.new do\n%s\nend)"
            (namespace->module (clojure.core/name ns))
            name
            (indent body))))

(defmethod -emit :deftype [{:keys [name fields methods interfaces env] :as ast} frame]
  (emit-type (:ns env) name fields interfaces methods))

(defmethod -emit :create-type [{:keys [name fields] :as ast} frame]
  (let [arg-names (map #(-emit % frame) fields)
        arglist (clojure.string/join ", " arg-names)
        ivar   #(format "@%s = %s" % %)
        ivars (endl-separate (map ivar arg-names))
        attr-reader #(format "attr_reader :%s" %)
        attr-readers (endl-separate (map attr-reader arg-names))
        constructor (format "def initialize(%s)\n%s\nend" arglist (indent ivars))
        body (endl-separate [attr-readers constructor])
        const-name (str (clojure.string/upper-case (gensym "type")))
        const [const-name (format "Class.new do\n%s\nend" (indent body))]]
    (swap! (:constants frame) (fn [old] (vec (conj old const))))
    const-name))

(defmethod -emit :binding [{:keys [name arg-id local variadic?] :as ast} frame]
  (case local
    :field
    (str name)
    :arg
    (let [varname (str (gensym "arg"))]
      (swap! (:emitted-locals frame)
             (fn [old]
               (assoc old name varname)))
      {:name varname
       :variadic? variadic?})))

(defn- emit-arg [{:keys [name variadic?]}]
  (if variadic?
    (str "*" name)
    name))

(defn- emit-class-method [{:keys [name args body method-type]}]
  (let [self-dot (if (= method-type :class) "self." "")]
    (format "def %s%s(%s)\n%s\nend"
            self-dot
            name
            (comma-separate (map emit-arg args))
            (indent body))))

(defn- emit-constants [constants]
  (endl-separate
    (map (fn [[name body]]
           (format "%s = %s" name body))
         constants)))

(defn emit-class [frame constants methods]
  (let [public-methods (filter #(not (:private %)) methods)
        private-methods (filter #(:private %) methods)
        methods (endl-separate [(endl-separate (map emit-class-method public-methods))
                                "private"
                                (endl-separate (map emit-class-method private-methods))])
        class-body (format "Class.new do\n%s\nend"
                           (indent (endl-separate [(emit-constants constants) methods])))
        const-name (clojure.string/upper-case (name (gensym "clj")))]
    (swap! (:constants frame) (fn [old] (vec (conj old [const-name class-body]))))
    const-name))

(defmethod -emit :fn-method [{:keys [params body loop-id fixed-arity] :as ast} frame]
  (let [new-frame (assoc frame :emitted-locals (atom {}))
        args (doall (map #(-emit % new-frame) params))]
    {:name loop-id
     :args args
     :private true
     :method-type :instance
     :fixed-arity fixed-arity
     :variadic? (some #(:variadic? %) params)
     :body (-emit body new-frame)}))

(defn- raise-wrong-number-of-args [methods]
  (let [arities (sort (map :fixed-arity methods))]
    (format "raise ArgumentError.new(\"wrong number of arguments (#{args.size} for %s)\")"
            (if (= (count methods) 1)
              (first arities)
              (format "%s or %s"
                      (comma-separate (butlast arities))
                      (last arities))))))

(defn emit-fn-dispatch [methods]
  (let [non-variadic (filter #(not (:variadic? %)) methods)
        variadic (first (filter #(:variadic? %) methods))
        whens (map #(format "when %s\n%s"
                            (:fixed-arity %)
                            (indent (str (:name %) "(*args)"))) non-variadic)
        whens (conj (vec whens)
                    (format "else\n%s"
                            (indent
                              (if variadic
                                (str (:name variadic) "(*args)")
                                (raise-wrong-number-of-args methods)))))]
    (format "case args.size\n%s\nend" (endl-separate whens))))

(defmethod -emit :fn [{:keys [methods env] :as ast} frame]
  (let [const-name (clojure.string/upper-case (name (gensym "fn")))
        new-frame (assoc frame :constants (atom []))
        emitted-methods (doall (map #(-emit % new-frame) methods))
        methods (concat [{:name "initialize"
                          :body ""
                          :private false
                          :method-type :instance
                          :args []}
                         {:name "invoke"
                          :body (emit-fn-dispatch emitted-methods)
                          :private false
                          :method-type :instance
                          :args [{:name "args"
                                  :variadic? true}]}] emitted-methods)]
    (format "%s.new" (emit-class frame @(:constants new-frame) methods))))

(defmethod -emit :local [{:keys [name] :as ast} {:keys [emitted-locals] :as frame}]
  (get @emitted-locals name))

(defmethod -emit :const [{:keys [val type] :as ast} frame]
  (case type
    :string (wrap-quotes val)
    :number (str val)
    :bool   (str val)))

(defmethod -emit :var [{:keys [var env] :as ast} frame]
  (var->constant (:runtime env) var))

(defmethod -emit :invoke [{:keys [fn args] :as ast} frame]
  (format "%s.invoke(%s)"
          (-emit fn frame)
          (comma-separate (map #(-emit % frame) args))))

(defmethod -emit :new [{:keys [args env class] :as ast} frame]
  (format "%s.new(%s)"
          (-emit {:op :maybe-class
                  :env env
                  :form class
                  :class class})
          (comma-separate (map -emit args))))

(defmethod -emit :if [{:keys [test then else] :as ast} frame]
  (let [else-expr (-emit else)]
    (if (= "" else-expr)
      (format "if %s\n%s\nend"
              (-emit test)
              (indent (-emit then)))
      (format "if %s\n%s\nelse\n%s\nend"
              (-emit test)
              (indent (-emit then))
              (indent else-expr)))))

(defmethod -emit :do [{:keys [statements ret env] :as ast} frame]
  (endl-separate (map #(-emit % frame) (conj (vec statements) ret))))

(defmethod -emit :throw [{:keys [exception] :as ast} frame]
  (format "raise %s" (-emit exception)))

;(defmethod -emit :=* [{:keys [args]} frame]
;  (if (> (count args) 1)
;    (loop [[x y & more] args exprs []]
;      (let [expr (wrap-parens (equals (-emit x) (-emit y)))]
;        (if (empty? more)
;          (if (empty? exprs)
;            expr
;            (wrap-parens
;              (clojure.string/join
;                " && "
;                (conj exprs expr))))
;          (recur (cons y more) (vec (conj exprs expr))))))
;    "true"))

(defn emit [ast]
  (let [frame {:constants (atom [])}
        body (-emit ast frame)]
    (endl-separate
      [(emit-constants @(:constants frame))
       body])))
