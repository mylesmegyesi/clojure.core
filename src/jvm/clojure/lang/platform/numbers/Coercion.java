package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Coercion {

  public static long toLong(Number x) {
    return x.longValue();
  }

  public static BigInteger toBigInteger(Number x) {
    if (x instanceof BigInteger) {
      return (BigInteger) x;
    } else if (x instanceof BigInt) {
      return ((BigInt) x).toBigInteger();
    } else {
      return BigInteger.valueOf(x.longValue());
    }
  }

  public static BigInt toBigInt(Number x) {
    if (x instanceof BigInt) {
      return (BigInt) x;
    } else if (x instanceof BigInteger) {
      return BigInt.fromBigInteger((BigInteger) x);
    } else {
      return BigInt.fromLong(x.longValue());
    }
  }

  public static Ratio toRatio(Number x) {
    if (x instanceof Ratio) {
      return (Ratio) x;
    } else {
      return new Ratio(Coercion.toBigInteger(x), BigInteger.ONE);
    }
  }

  public static BigDecimal toBigDecimal(Number x) {
    if (x instanceof BigDecimal) {
      return (BigDecimal) x;
    } else if (x instanceof BigInt) {
      if (((BigInt) x).bipart == null) {
        return BigDecimal.valueOf(((BigInt) x).lpart);
      } else {
        return new BigDecimal(((BigInt) x).bipart);
      }
    } else if (x instanceof BigInteger) {
      return new BigDecimal((BigInteger) x);
    } else if (x instanceof Ratio) {
      return ((Ratio) x).bigDecimalValue();
    } else {
      return BigDecimal.valueOf(x.longValue());
    }
  }

  public static double toDouble(Number x) {
    return x.doubleValue();
  }

  public static long toBitOperand(Object x) {
    if (x instanceof Long) {
      return (Long) x;
    } else if (x instanceof Byte ||
               x instanceof Short ||
               x instanceof Integer) {
      return toLong((Number) x);
    } else {
      throw new IllegalArgumentException("bit operation not supported for: " + x.getClass());
    }
  }

}

