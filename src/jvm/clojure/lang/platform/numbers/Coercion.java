package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Coercion {

  public static BigInteger toBigInteger(BigInt x) { return ((BigInt) x).toBigInteger(); }
  public static BigInteger toBigInteger(Number x) { return BigInteger.valueOf(((Number) x).longValue()); }

  public static BigInt toBigInt(BigInteger x) { return BigInt.fromBigInteger(((BigInteger) x)); }
  public static BigInt toBigInt(Number x) { return BigInt.fromLong(((Number) x).longValue()); }

  public static Ratio toRatio(Number x) { return new Ratio(toBigInteger(x), BigInteger.ONE); }

  public static BigDecimal toBigDecimal(BigInt x) {
    if (x.bipart == null) {
      return BigDecimal.valueOf(x.lpart);
    } else {
      return new BigDecimal(x.bipart);
    }
  }
  public static BigDecimal toBigDecimal(BigInteger x) { return new BigDecimal(((BigInteger) x)); }
  public static BigDecimal toBigDecimal(Ratio x) {
    return (BigDecimal) Division.bigDecimalDivide((BigDecimal) new BigDecimal((BigInteger) x.getNumerator()), (BigDecimal) new BigDecimal((BigInteger) x.getDenominator()));
  }
  public static BigDecimal toBigDecimal(Number x) { return BigDecimal.valueOf(((Number) x).longValue()); }

}
