package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.platform.BigInt;
import clojure.lang.platform.Ratio;

public final class Remainder {

  public static Number numberRemainder(Number x, Number y) {
    if (Zero.numberIsZero(y)) {
      throw new ArithmeticException("Divide by zero");
    }

    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Remainder.doubleRemainder(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Remainder.bigDecimalRemainder(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Remainder.ratioRemainder(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Remainder.bigIntRemainder(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Remainder.longRemainder(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  public static Number longRemainder(long x, long y) {
    return Long.valueOf(x % y);
  }

  public static Number bigIntRemainder(BigInt x, BigInt y) {
    return x.remainder(y);
  }

  public static Number ratioRemainder(Ratio x, Ratio y) {
    BigInteger q = x.getNumerator().multiply(y.getDenominator()).divide(
                    x.getDenominator().multiply(y.getNumerator()));
    return BigInt.fromBigInteger(q);
  }

  public static Number bigDecimalRemainder(BigDecimal x, BigDecimal y) {
    return x.remainder(y);
  }

  public static Number doubleRemainder(double x, double y) {
    double q = x / y;
    if (q <= Long.MAX_VALUE && q >= Long.MIN_VALUE) {
      return (x - ((long) q) * y);
    } else {
      return new BigDecimal(q).toBigInteger().doubleValue();
    }
  }

}
