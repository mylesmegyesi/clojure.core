package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.platform.BigInt;
import clojure.lang.platform.Ratio;

public final class Decrement {

  public static Number numberDecrement(Number x) {
    if (x instanceof Long) {
      return Decrement.longDecrement(x.longValue());
    } else if ((x instanceof Float) || (x instanceof Double)) {
      return Decrement.doubleDecrement(Coercion.toDouble(x));
    } else if (x instanceof BigDecimal) {
      return Decrement.bigDecimalDecrement((BigDecimal) x);
    } else if (x instanceof Ratio) {
      return Decrement.ratioDecrement((Ratio) x);
    } else if ((x instanceof BigInt) || (x instanceof BigInteger)) {
      return Decrement.bigIntDecrement(Coercion.toBigInteger(x));
    } else {
      return Decrement.longDecrement(Coercion.toLong(x));
    }
  }

  public static Number longDecrement(long x) {
    long xDup = x;
    xDup--;
    return (Number) xDup;
  }

  public static Number bigIntDecrement(BigInteger x) {
    return (Number) BigInt.fromBigInteger(x.subtract(BigInteger.ONE));
  }

  public static Number ratioDecrement(Ratio x) {
    return (Number) Subtraction.numberSubtract(x, 1);
  }

  public static Number bigDecimalDecrement(BigDecimal x) {
    return (Number) x.subtract(BigDecimal.ONE);
  }

  public static Number doubleDecrement(double x) {
    return (Number) Double.valueOf(x - 1);
  }

  public static Number numberUncheckedDecrement(long x) {
    return x - 1;
  }

  public static Number numberUncheckedDecrement(double x) {
    return x - 1;
  }

  public static Number numberUncheckedDecrement(Number x) {
    return numberDecrement(x);
  }

  public static Number numberPrecisionDecrement(Number x) {
    Categories category = CategoryType.findCategoryType(x);
    if (category == Categories.INT) {
      long lx = x.longValue();
      if (lx == Long.MIN_VALUE) {
        return (Number) Decrement.numberDecrement(Coercion.toBigInt(x));
      } else {
        return (Number) (lx - 1);
      }
    } else {
      return (Number) Decrement.numberDecrement(x);
    }
  }

}

