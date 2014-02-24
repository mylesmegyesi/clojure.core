package clojure.lang.platform;

import java.math.BigInteger;
import java.math.BigDecimal;

public final class Ratio extends Number {

  final private BigInteger numerator;
  final private BigInteger denominator;

  public Ratio(BigInteger _numerator, BigInteger _denominator) {
    numerator   = _numerator;
    denominator = _denominator;
  }

  public Number getNumerator() {
    return numerator;
  }

  public Number getDenominator() {
    return denominator;
  }

  public int hashCode(){
    return numerator.hashCode() ^ denominator.hashCode();
  }

  public Ratio ratioValue() {
    return this;
  }

  public int intValue() {
    return (int) doubleValue();
  }

  public long longValue() {
    return bigIntegerValue().longValue();
  }

  public float floatValue() {
    return (float) doubleValue();
  }

  public double doubleValue() {
    BigDecimal bigDecimalNumerator   = new BigDecimal(numerator);
    BigDecimal bigDecimalDenominator = new BigDecimal(denominator);
    return bigDecimalNumerator.divide(bigDecimalDenominator, java.math.MathContext.DECIMAL64).doubleValue();
  }

  public BigDecimal bigDecimalValue() {
    BigDecimal bigDecimalNumerator   = new BigDecimal(numerator);
    BigDecimal bigDecimalDenominator = new BigDecimal(denominator);
    return bigDecimalNumerator.divide(bigDecimalDenominator, java.math.MathContext.UNLIMITED);
  }

  public BigInteger bigIntegerValue() {
    return numerator.divide(denominator);
  }

  public boolean equals(Object other) {
    if ((other != null) && (other instanceof Ratio)) {
      return numerator.equals(((Ratio) other).getNumerator())
              && denominator.equals(((Ratio) other).getDenominator());
    } else {
      return false;
    }
  }

}
