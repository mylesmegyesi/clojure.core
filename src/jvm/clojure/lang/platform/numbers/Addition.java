package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Addition {

  public static Number numberAdd(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Addition.doubleAdd(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Addition.bigDecimalAdd(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Addition.ratioAdd(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Addition.bigIntAdd(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Addition.longAdd(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  // Special case for hash map
  public static int hashMapIntegerAdd(int x, int y) {
    return x + y;
  }

  public static Number longAdd(long x, long y) {
    long ret = x + y;
    if ((ret ^ x) < 0 && (ret ^ y) < 0) {
      throw new ArithmeticException("integer overflow");
    } else {
      return (Number) ret;
    }
  }

  public static Number bigIntAdd(BigInt x, BigInt y) {
    return (Number) x.add(y);
  }

  public static Number ratioAdd(Ratio x, Ratio y) {
    BigInteger ynXxd = ((BigInteger) y.getNumerator()).multiply((BigInteger) x.getDenominator());
    BigInteger xnXyd = ((BigInteger) x.getNumerator()).multiply((BigInteger) y.getDenominator());
    BigInteger ydXxd = ((BigInteger) y.getDenominator()).multiply((BigInteger) x.getDenominator());
    return (Number) Division.bigIntegerDivide((BigInteger) ynXxd.add(xnXyd), (BigInteger) ydXxd);
  }

  public static Number bigDecimalAdd(BigDecimal x, BigDecimal y) {
    return (Number) x.add(y);
  }

  public static Number doubleAdd(double x, double y) {
    return (Number) Double.valueOf(x + y);
  }

}

