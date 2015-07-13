package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Comparison {

  public static final boolean lessThan(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Comparison.doubleLessThan(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Comparison.bigDecimalLessThan(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Comparison.ratioLessThan(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Comparison.bigIntLessThan(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Comparison.longLessThan(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  public static boolean lessThanEqualTo(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Comparison.doubleLessThanEqualTo(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Comparison.bigDecimalLessThanEqualTo(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Comparison.ratioLessThanEqualTo(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Comparison.bigIntLessThanEqualTo(Coercion.toBigInteger(x), Coercion.toBigInteger(y));
    } else {
      return Comparison.longLessThanEqualTo(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  private static boolean doubleLessThan(double x, double y) {
    return x < y;
  }

  private static boolean doubleLessThanEqualTo(double x, double y) {
    return x <= y;
  }

  private static boolean bigDecimalLessThan(BigDecimal x, BigDecimal y) {
    return x.compareTo(y) < 0;
  }

  private static boolean bigDecimalLessThanEqualTo(BigDecimal x, BigDecimal y) {
    return x.compareTo(y) <= 0;
  }

  private static boolean ratioLessThan(Ratio x, Ratio y) {
    return lessThan(x.getNumerator().multiply(y.getDenominator()),
                    y.getNumerator().multiply(x.getDenominator()));
  }

  private static boolean ratioLessThanEqualTo(Ratio x, Ratio y) {
    return lessThanEqualTo(x.getNumerator().multiply(y.getDenominator()),
                           y.getNumerator().multiply(x.getDenominator()));
  }

  private static boolean bigIntLessThan(BigInt x, BigInt y) {
    return x.lt(y);
  }

  private static boolean bigIntLessThanEqualTo(BigInteger x, BigInteger y) {
    return x.compareTo(y) <= 0;
  }

  private static boolean longLessThan(long x, long y) {
    return x < y;
  }

  private static boolean longLessThanEqualTo(long x, long y) {
    return x <= y;
  }
}
