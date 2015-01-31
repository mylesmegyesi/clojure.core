package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Subtraction {

  public static Number numberSubtract(Number x, Number y) {
    return Addition.numberAdd((Number) x, (Number) Negation.numberNegate(y));
  }

  // Special Case, needs to be removed at some point
  public static int integerPreserveSubtract(int x, int y) {
    return x - y;
  }

  public static Number numberPrecisionSubtract(Number x, Number y) {
    return Addition.numberPrecisionAdd((Number) x, (Number) Negation.numberPrecisionNegate(y));
  }

}

