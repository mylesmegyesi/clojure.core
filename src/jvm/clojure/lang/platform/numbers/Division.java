package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Division {

  private static long gcd(long x, long y) {
    while (y != 0) {
      long rem = x % y;
      x = y;
      y = rem;
    }
    return x;
  }

  private final static long LONG_ZERO = Long.valueOf(0);
  public static Number longDivide(Long x, Long y) {
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

  public static Number bigIntDivide(BigInteger x, BigInteger y) {
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
    return (Number) bigIntDivide(((BigInteger) y.getDenominator()).multiply(((BigInteger) x.getNumerator())),
                                 ((BigInteger) y.getNumerator()).multiply(((BigInteger) x.getDenominator())));
  }

  public static Number bigDecimalDivide(BigDecimal x, BigDecimal y) {
    return (Number) x.divide(y);
  }

  public static Number doubleDivide(Double x, Double y) {
    return (Number) Double.valueOf(x / y);
  }

}
