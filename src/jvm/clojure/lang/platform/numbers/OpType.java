package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class OpType {

  public static final Ops findOpType(Number x, Number y) {
    if ((x instanceof Long) && (y instanceof Long)) {
      return Ops.LONG;
    } else if ((x instanceof Double) || (x instanceof Float) || (y instanceof Double) || (y instanceof Float)) {
      return Ops.DOUBLE;
    } else if (x instanceof BigDecimal || y instanceof BigDecimal) {
      return Ops.BIGDECIMAL;
    } else if (x instanceof Ratio || y instanceof Ratio) {
      return Ops.RATIO;
    } else if (x instanceof BigInt || x instanceof BigInteger || y instanceof BigInt || y instanceof BigInteger) {
      return Ops.BIGINT;
    } else {
      return Ops.LONG;
    }
  }

}

