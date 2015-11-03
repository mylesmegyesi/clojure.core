package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.platform.BigInt;
import clojure.lang.platform.Ratio;

public final class PositiveNegative {

  public static final boolean numberIsPositive(Number x) {
    if (x instanceof Long) {
      return x.longValue() > 0;
    } else if ((x instanceof Double) || (x instanceof Float)) {
      return x.doubleValue() > 0;
    } else if (x instanceof BigDecimal) {
      return ((BigDecimal) x).signum() > 0;
    } else if (x instanceof Ratio) {
      return ((Ratio) x).getNumerator().signum() > 0;
    } else if ((x instanceof BigInt) || (x instanceof BigInteger)) {
      BigInt bx = Coercion.toBigInt(x);
      if (bx.getBipart() == null) {
        return bx.getLpart() > 0;
      } else {
        return bx.getBipart().signum() > 0;
      }
    } else {
      return x.longValue() > 0;
    }
  }

  public static final boolean numberIsNegative(Number x) {
    if (x instanceof Long) {
      return x.longValue() < 0;
    } else if ((x instanceof Double) || (x instanceof Float)) {
      return x.doubleValue() < 0;
    } else if (x instanceof BigDecimal) {
      return ((BigDecimal) x).signum() < 0;
    } else if (x instanceof Ratio) {
      return ((Ratio) x).getNumerator().signum() < 0;
    } else if ((x instanceof BigInt) || (x instanceof BigInteger)) {
      BigInt bx = Coercion.toBigInt(x);
      if (bx.getBipart() == null) {
        return bx.getLpart() < 0;
      } else {
        return bx.getBipart().signum() < 0;
      }
    } else {
      return x.longValue() < 0;
    }
  }

}

