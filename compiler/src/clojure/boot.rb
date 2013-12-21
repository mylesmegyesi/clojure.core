
begin

  class PersistentMap
  end

  class CljKeyword
    attr_reader :sym

    def initialize(sym)
      @sym = sym
    end

    def ==(other)
      other.sym == self.sym
    end

    def to_s
      sym.to_s
    end
  end

  class CljSymbol
    attr_reader :ns, :name

    def initialize(ns, name)
      @ns   = ns
      @name = name
    end

    def ==(other)
      other.ns == self.ns &&
        other.name == self.name
    end

    def to_s
      if ns
        "#{ns}/#{name}"
      else
        name
      end
    end

    def rbsym
      to_s.to_sym
    end

    def self.intern(s)
      string = s.to_s
      if i = string.rindex("/")
        ns = string[0...i]
        self.new(ns, string[(i+1)..-1])
      else
        self.new(nil, string)
      end
    end
  end

  class Namespace
    NAMESPACES = {}

    attr_reader :sym

    def self.current_ns
      @@current_ns || raise("No current ns")
    end

    def self.set_current_ns(ns)
      @@current_ns = ns
    end

    def self.find_or_create_ns(sym)
      NAMESPACES[sym.rbsym] ||= Namespace.new(sym)
    end

    def self.find_ns(sym)
      NAMESPACES[sym.rbsym]
    end

    def ==(other)
      other.sym == self.sym
    end

    def initialize(sym)
      @sym      = sym
      @mappings = {}
    end

    def reference(sym, val)
      raise ArguementError.new("Can't intern namespace-qualified symbol") if sym.ns
      @mappings[sym.rbsym] = val
      val
    end

    def find_interned_var(sym)
      @mappings[sym.rbsym]
    end

    def resolve(str)
      sym = CljSymbol.intern(str)
      ret = self.find_interned_var(sym) || (
        if ns = self.class.find_ns(CljSymbol.intern(sym.ns))
          ns.find_interned_var(CljSymbol.intern(sym.name))
        end
      )
      raise "Could not resolve var: #{str}" unless ret
      ret.value
    end

    def mappings
      @mappings.reduce({}) do |acc, (rbsym, val)|
        acc[CljSymbol.intern(rbsym)] = val
        acc
      end
    end

    set_current_ns(nil)
  end

  class Var

    class Unbound
      def initialize(var)
        @var = var
      end

      def invoke(*args)
        raise ArguementError.new("Attempting to call unbound fn: #{var}")
      end

      private

      attr_reader :var
    end

    def initialize(namespace, sym, root = nil)
      @namespace = namespace
      @sym = sym
      @root = root || Unbound.new(self)
      @meta = PersistentMap.new
    end

    def value
      root
    end

    attr_reader :root
  end

  _clojure_core_sym = CljSymbol.new(nil, "clojure.core")
  clojure_core = Namespace.find_or_create_ns(_clojure_core_sym)

  _def_fn = Class.new do
    def invoke(sym, val = nil)
      var = Var.new(Namespace.current_ns, sym, val)
      Namespace.current_ns.reference(sym, var)
    end
  end.new
  _def_sym = CljSymbol.new(nil, "def")
  _def_var = Var.new(clojure_core, _def_sym, _def_fn)
  _def_var.set_meta(CljKeyword.new(CljSymbol.new(nil, "macro")), true)

  clojure_core.reference(_def_sym, _def_var)

  _symbol_fn = Class.new do
    def invoke(str)
      CljSymbol.intern(str)
    end
  end.new
  _symbol_sym = CljSymbol.new(nil, "symbol")
  _symbol_var = Var.new(clojure_core, _symbol_sym, _symbol_fn)

  clojure_core.reference(_symbol_sym, _symbol_var)

  _ns_fn = Class.new do
    def invoke(sym)
      ns = Namespace.find_or_create_ns(sym)
      Namespace.set_current_ns(ns)
    end
  end.new
  _ns_sym = CljSymbol.new(nil, "ns")
  _ns_var = Var.new(clojure_core, _ns_sym, _ns_fn)

  clojure_core.reference(_ns_sym, _ns_var)

  _refer_fn = Class.new do
    def invoke(ns_sym)
      current_ns = Namespace.current_ns
      Namespace.find_ns(ns_sym).mappings.each do |name, val|
        current_ns.reference(name, val)
      end
    end
  end.new
  _refer_sym = CljSymbol.new(nil, "refer")
  _refer_var = Var.new(clojure_core, _refer_sym, _refer_fn)

  clojure_core.reference(_refer_sym, _refer_var)

  _user_ns = Namespace.find_or_create_ns(CljSymbol.new(nil, "user"))
  Namespace.set_current_ns(_user_ns)
  _refer_fn.invoke(_clojure_core_sym)

end

