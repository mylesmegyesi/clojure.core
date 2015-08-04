package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Division {

  public static Number numberDivide(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Division.doubleDivide(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Division.bigDecimalDivide(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Division.ratioDivide(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Division.bigIntegerDivide(Coercion.toBigInteger(x), Coercion.toBigInteger(y));
    } else {
      return Division.longDivide(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  private static long gcd(long x, long y) {
    while (y != 0) {
      long rem = x % y;
      x = y;
      y = rem;
    }
    return x;
  }

  private final static long LONG_ZERO = Long.valueOf(0);
  public static Number longDivide(long x, long y) {
    if (y == LONG_ZERO) {
      throw new ArithmeticException("Divide by zero");
    }

    long gcd = gcd(x, y);
    if (gcd == 0) {
      return LONG_ZERO;
    }

    long num = x / gcd;
    long denom = y / gcd;
    if (denom == 1) {
      return Long.valueOf(num);
    }
    if (denom < 0) {
      num = -num;
      denom = - denom;
    }

    return new Ratio(BigInteger.valueOf(num), BigInteger.valueOf(denom));
  }

  public static Number bigIntegerDivide(BigInteger x, BigInteger y) {
    if (y.equals(BigInteger.ZERO)) {
      throw new ArithmeticException("Divide by zero");
    }

    BigInteger gcd = x.gcd(y);
    if (gcd.equals(BigInteger.ZERO)) {
      return BigInt.ZERO;
    }

    x = x.divide(gcd);
    y = y.divide(gcd);
    if (y.equals(BigInteger.ONE)) {
      return BigInt.fromBigInteger(x);
    } else if (y.equals(BigInteger.ONE.negate())) {
      return BigInt.fromBigInteger(x.negate());
    } else {
      if (y.signum() < 0) {
        return new Ratio(x.negate(), y.negate());
      } else {
        return new Ratio(x, y);
      }
    }
  }

  public static Number ratioDivide(Ratio x, Ratio y) {
    return bigIntegerDivide(y.getDenominator().multiply(x.getNumerator()),
                            y.getNumerator().multiply(x.getDenominator()));
  }

  public static Number bigDecimalDivide(BigDecimal x, BigDecimal y) {
    if (y.signum() == 0) {
      throw new ArithmeticException("Divide by zero");
    }

    return x.divide(y);
  }

  public static Number doubleDivide(double x, double y) {
    return Double.valueOf(x / y);
  }

}
