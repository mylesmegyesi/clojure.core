package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Addition {

  public static Number longAdd(Long x, Long y) {
    long ret = x + y;
    if ((ret ^ x) < 0 && (ret ^ y) < 0)
      throw new ArithmeticException("integer overflow");
    return (Number) ret;
  }

  public static Number bigIntAdd(BigInt x, BigInt y) {
    return (Number) x.add(y);
  }

  public static Number ratioAdd(Ratio x, Ratio y) {
    BigInteger ynXxd = ((BigInteger) y.getNumerator()).multiply((BigInteger) x.getDenominator());
    BigInteger xnXyd = ((BigInteger) x.getNumerator()).multiply((BigInteger) y.getDenominator());
    BigInteger ydXxd = ((BigInteger) y.getDenominator()).multiply((BigInteger) x.getDenominator());
    return (Number) Division.bigIntegerDivide((BigInteger) ynXxd.add(xnXyd), (BigInteger) ydXxd);
  }

  public static Number bigDecimalAdd(BigDecimal x, BigDecimal y) {
    return (Number) x.add(y);
  }

  public static Number doubleAdd(Double x, Double y) {
    return (Number) Double.valueOf(x + y);
  }

}
