package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Division {

  // LongOps

  private static long gcd(long x, long y) {
    while (y != 0) {
      long rem = x % y;
      x = y;
      y = rem;
    }
    return x;
  }

  private final static long LONG_ZERO = Long.valueOf(0);
  public static Number divide(Long x, Long y) {
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

  public static Number divide(Integer x, Integer y) {
    return (Number) divide(((Integer) x).longValue(), ((Integer) y).longValue());
  }

  public static Number divide(Integer x, Long y) {
    return (Number) divide(((Integer) x).longValue(), y);
  }

  public static Number divide(Long x, Integer y) {
    return (Number) divide(x, ((Integer) y).longValue());
  }

  // BigIntOps

  public static Number divide(BigInteger x, BigInteger y) {
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

  public static Number divide(BigInteger x, Integer y) {
    return (Number) divide(x, Coercion.toBigInteger(x));
  }

  public static Number divide(Integer x, BigInteger y) {
    return (Number) divide(Coercion.toBigInteger(x), y);
  }

  public static Number divide(BigInteger x, Long y) {
    return (Number) divide(x, Coercion.toBigInteger(y));
  }

  public static Number divide(Long x, BigInteger y) {
    return (Number) divide(Coercion.toBigInteger(x), y);
  }

  public static Number divide(BigInt x, BigInt y) {
    return (Number) divide(Coercion.toBigInteger(x), Coercion.toBigInteger(y));
  }

  public static Number divide(BigInteger x, BigInt y) {
    return (Number) divide(x, Coercion.toBigInteger(y));
  }

  public static Number divide(BigInt x, BigInteger y) {
    return (Number) divide(Coercion.toBigInteger(x), y);
  }

  // RatioOps

  public static Number divide(Ratio x, Ratio y) {
    return (Number) divide(((BigInteger) y.getDenominator()).multiply(((BigInteger) x.getNumerator())),
                           ((BigInteger) y.getNumerator()).multiply(((BigInteger) x.getDenominator())));
  }

  public static Number divide(Ratio x, Integer y) {
    return (Number) divide(x, Coercion.toRatio(y));
  }

  public static Number divide(Integer x, Ratio y) {
    return (Number) divide(Coercion.toRatio(x), y);
  }

  public static Number divide(Ratio x, Long y) {
    return (Number) divide(x, Coercion.toRatio(y));
  }

  public static Number divide(Long x, Ratio y) {
    return (Number) divide(Coercion.toRatio(x), y);
  }

  public static Number divide(Ratio x, BigInteger y) {
    return (Number) divide(x, Coercion.toRatio(y));
  }

  public static Number divide(BigInteger x, Ratio y) {
    return (Number) divide(Coercion.toRatio(x), y);
  }

  public static Number divide(Ratio x, BigInt y) {
    return (Number) divide(x, Coercion.toRatio(y));
  }

  public static Number divide(BigInt x, Ratio y) {
    return (Number) divide(Coercion.toRatio(x), y);
  }

  // BigDecimalOps

  public static Number divide(BigDecimal x, BigDecimal y) {
    return (Number) x.divide(y);
  }

  public static Number divide(BigDecimal x, Integer y) {
    return (Number) divide(x, Coercion.toBigDecimal(y));
  }

  public static Number divide(Integer x, BigDecimal y) {
    return (Number) divide(Coercion.toBigDecimal(x), y);
  }

  public static Number divide(BigDecimal x, Long y) {
    return (Number) divide(x, Coercion.toBigDecimal(y));
  }

  public static Number divide(Long x, BigDecimal y) {
    return (Number) divide(Coercion.toBigDecimal(x), y);
  }

  public static Number divide(BigDecimal x, BigInteger y) {
    return (Number) divide(x, Coercion.toBigDecimal(y));
  }

  public static Number divide(BigInteger x, BigDecimal y) {
    return (Number) divide(Coercion.toBigDecimal(x), y);
  }

  public static Number divide(BigDecimal x, BigInt y) {
    return (Number) divide(x, Coercion.toBigDecimal(y));
  }

  public static Number divide(BigInt x, BigDecimal y) {
    return (Number) divide(Coercion.toBigDecimal(x), y);
  }

  public static Number divide(BigDecimal x, Ratio y) {
    return (Number) divide(x, Coercion.toBigDecimal(y));
  }

  public static Number divide(Ratio x, BigDecimal y) {
    return (Number) divide(Coercion.toBigDecimal(x), y);
  }

  // DoubleOps

  public static Number divide(Double x, Double y) {
    return (Number) Double.valueOf(x / y);
  }

  public static Number divide(Double x, Number y) {
    return (Number) divide(x, ((Number) y).doubleValue());
  }

  public static Number divide(Number x, Double y) {
    return (Number) divide(((Number) x).doubleValue(), y);
  }

  public static Number divide(Float x, Number y) {
    return (Number) divide(((Float) x).doubleValue(), ((Number) y).doubleValue());
  }

  public static Number divide(Number x, Float y) {
    return (Number) divide(((Number) x).doubleValue(), ((Float) y).doubleValue());
  }

  // Fallback to LongOps

  public static Number divide(Number x, Number y) {
    return (Number) divide(((Number) x).longValue(), ((Number) y).longValue());
  }

}
