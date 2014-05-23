package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Zero {

  public static boolean numberIsZero(Number x) {
    if ((x instanceof Double) || (x instanceof Float)) {
      return x.doubleValue() == 0;
    } else if (x instanceof Ratio) {
      return ((BigInteger) (((Ratio) x).getNumerator())).signum() == 0;
    } else if (x instanceof BigDecimal) {
      return ((BigDecimal) x).signum() == 0;
    } else if ((x instanceof BigInt) || (x instanceof BigInteger)) {
      return Zero.bigIntIsZero(Coercion.toBigInt(x));
    } else {
      return x.longValue() == 0;
    }
  }

  public static boolean bigIntIsZero(BigInt x) {
    if (x.bipart == null) {
      return x.lpart == 0;
    } else {
      return x.bipart.signum() == 0;
    }
  }

}

