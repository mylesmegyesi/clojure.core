package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Quotient {

  public static Number numberQuotient(Number x, Number y) {
    if (Zero.numberIsZero(y)) {
      throw new ArithmeticException("Divide by zero");
    }

    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Quotient.doubleQuotient(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Quotient.bigDecimalQuotient(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Quotient.ratioQuotient(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Quotient.bigIntQuotient(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Quotient.longQuotient(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  public static Number longQuotient(long x, long y) {
    return Long.valueOf(x / y);
  }

  public static Number bigIntQuotient(BigInt x, BigInt y) {
    return x.quotient(y);
  }

  public static Number ratioQuotient(Ratio x, Ratio y) {
    BigInteger q = x.getNumerator().multiply(y.getDenominator()).divide(
                    x.getDenominator().multiply(y.getNumerator()));
    return BigInt.fromBigInteger(q);
  }

  public static Number bigDecimalQuotient(BigDecimal x, BigDecimal y) {
    return x.divide(y);
  }

  public static Number doubleQuotient(double x, double y) {
    double q = x / y;
    if (q <= Long.MAX_VALUE && q >= Long.MIN_VALUE) {
      return (double)(long) q;
    } else {
      return new BigDecimal(q).toBigInteger().doubleValue();
    }
  }

}
