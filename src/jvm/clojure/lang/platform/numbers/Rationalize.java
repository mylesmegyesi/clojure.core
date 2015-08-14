package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;

public final class Rationalize {

  public static Number numberRationalize(Number x) {
    if (x instanceof Float || x instanceof Double) {
      return numberRationalize(BigDecimal.valueOf(x.doubleValue()));
    } else if (x instanceof BigDecimal) {
      BigDecimal bd = (BigDecimal) x;
      BigInteger bi = bd.unscaledValue();
      int scale = bd.scale();
      if (scale < 0) {
        return BigInt.fromBigInteger(bi.multiply(BigInteger.TEN.pow(-scale)));
      } else {
        return Division.numberDivide(bi, BigInteger.TEN.pow(scale));
      }
    } else {
      return x;
    }
  }

}
