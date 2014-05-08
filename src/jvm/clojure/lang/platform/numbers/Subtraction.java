package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Subtraction {

  public static Number numberSubtract(Number x, Number y) {
    return Addition.numberAdd((Number) x, (Number) Negation.numberNegate(y));
  }

  public static int hashMapIntegerSubtract(int x, int y) {
    return x - y;
  }

}

