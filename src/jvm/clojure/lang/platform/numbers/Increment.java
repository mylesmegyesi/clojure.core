package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Increment {

  public static Number numberIncrement(Number x) {
    if ((x instanceof Float) || (x instanceof Double)) {
      return Increment.doubleIncrement(Coercion.toDouble(x));
    } else if (x instanceof BigDecimal) {
      return Increment.bigDecimalIncrement((BigDecimal) x);
    } else if (x instanceof Ratio) {
      return Increment.ratioIncrement((Ratio) x);
    } else if ((x instanceof BigInt) || (x instanceof BigInteger)) {
      return Increment.bigIntIncrement(Coercion.toBigInteger(x));
    } else {
      return Increment.longIncrement(Coercion.toLong(x));
    }
  }

  public static Number longIncrement(long x) {
    long xDup = x;
    xDup++;
    return (Number) xDup;
  }

  public static Number bigIntIncrement(BigInteger x) {
    return (Number) BigInt.fromBigInteger(x.add(BigInteger.ONE));
  }

  public static Number ratioIncrement(Ratio x) {
    return (Number) Addition.ratioAdd(x, new Ratio(BigInteger.ONE, BigInteger.ONE));
  }

  public static Number bigDecimalIncrement(BigDecimal x) {
    return (Number) x.add(BigDecimal.ONE);
  }

  public static Number doubleIncrement(double x) {
    return (Number) Double.valueOf(x + 1);
  }

}

