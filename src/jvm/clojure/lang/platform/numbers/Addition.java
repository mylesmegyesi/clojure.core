package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Addition {

  // LongOps

  public static Number add(Long x, Long y) {
    long ret = x + y;
    if ((ret ^ x) < 0 && (ret ^ y) < 0)
      throw new ArithmeticException("integer overflow");
    return (Number) ret;
  }

  public static Number add(Integer x, Integer y) {
    return (Number) add(((Integer) x).longValue(), ((Integer) y).longValue());
  }

  public static Number add(Integer x, Long y) {
    return (Number) add(((Integer) x).longValue(), y);
  }

  public static Number add(Long x, Integer y) {
    return (Number) add(x, ((Integer) y).longValue());
  }

  // BigIntOps

  public static Number add(BigInt x, BigInt y) {
    return (Number) x.add(y);
  }

  public static Number add(BigInteger x, Integer y) {
    return (Number) add(Coercion.toBigInt(x), Coercion.toBigInt(y));
  }

  public static Number add(Integer x, BigInteger y) {
    return (Number) add(Coercion.toBigInt(x), Coercion.toBigInt(y));
  }

  public static Number add(BigInteger x, Long y) {
    return (Number) add(Coercion.toBigInt(x), Coercion.toBigInt(y));
  }

  public static Number add(Long x, BigInteger y) {
    return (Number) add(Coercion.toBigInt(x), Coercion.toBigInt(y));
  }

  public static Number add(BigInteger x, BigInteger y) {
    return (Number) add(Coercion.toBigInt(x), Coercion.toBigInt(y));
  }

  public static Number add(BigInt x, Integer y) {
    return (Number) add(x, Coercion.toBigInt(y));
  }

  public static Number add(Integer x, BigInt y) {
    return (Number) add(Coercion.toBigInt(x), y);
  }

  public static Number add(BigInt x, Long y) {
    return (Number) add(x, Coercion.toBigInt(y));
  }

  public static Number add(Long x, BigInt y) {
    return (Number) add(Coercion.toBigInt(x), y);
  }

  public static Number add(BigInt x, BigInteger y) {
    return (Number) add(x, Coercion.toBigInt(y));
  }

  public static Number add(BigInteger x, BigInt y) {
    return (Number) add(Coercion.toBigInt(x), y);
  }

  // RatioOps

  public static Number add(Ratio x, Ratio y) {
    BigInteger ynXxd = ((BigInteger) y.getNumerator()).multiply((BigInteger) x.getDenominator());
    BigInteger xnXyd = ((BigInteger) x.getNumerator()).multiply((BigInteger) y.getDenominator());
    BigInteger ydXxd = ((BigInteger) y.getDenominator()).multiply((BigInteger) x.getDenominator());
    return (Number) Division.divide((BigInteger) ynXxd.add(xnXyd), (BigInteger) ydXxd);
  }

  public Number add(Ratio x, Integer y) {
    return (Number) add(x, Coercion.toRatio(y));
  }

  public Number add(Integer x, Ratio y) {
    return (Number) add(Coercion.toRatio(x), y);
  }

  public Number add(Ratio x, Long y) {
    return (Number) add(x, Coercion.toRatio(y));
  }

  public Number add(Long x, Ratio y) {
    return (Number) add(Coercion.toRatio(x), y);
  }

  public Number add(Ratio x, BigInt y) {
    return (Number) add(x, Coercion.toRatio(y));
  }

  public Number add(BigInt x, Ratio y) {
    return (Number) add(Coercion.toRatio(x), y);
  }

  public Number add(Ratio x, BigInteger y) {
    return (Number) add(x, Coercion.toRatio(y));
  }

  public Number add(BigInteger x, Ratio y) {
    return (Number) add(Coercion.toRatio(x), y);
  }

  // BigDecimalOps

  public static Number add(BigDecimal x, BigDecimal y) {
    return (Number) x.add(y);
  }

  public static Number add(BigDecimal x, Integer y) {
    return (Number) add(x, Coercion.toBigDecimal(y));
  }

  public static Number add(Integer x, BigDecimal y) {
    return (Number) add(Coercion.toBigDecimal(x), y);
  }

  public static Number add(BigDecimal x, Long y) {
    return (Number) add(x, Coercion.toBigDecimal(y));
  }

  public static Number add(Long x, BigDecimal y) {
    return (Number) add(Coercion.toBigDecimal(x), y);
  }

  public static Number add(BigDecimal x, BigInt y) {
    return (Number) add(x, Coercion.toBigDecimal(y));
  }

  public static Number add(BigInt x, BigDecimal y) {
    return (Number) add(Coercion.toBigDecimal(x), y);
  }

  public static Number add(BigDecimal x, BigInteger y) {
    return (Number) add(x, Coercion.toBigDecimal(y));
  }

  public static Number add(BigInteger x, BigDecimal y) {
    return (Number) add(Coercion.toBigDecimal(x), y);
  }

  public static Number add(BigDecimal x, Ratio y) {
    return (Number) add(x, Coercion.toBigDecimal(y));
  }

  public static Number add(Ratio x, BigDecimal y) {
    return (Number) add(Coercion.toBigDecimal(x), y);
  }

  // DoubleOps

  public static Number add(Double x, Double y) {
    return (Number) Double.valueOf(x + y);
  }

  public static Number add(Double x, Number y) {
    return (Number) add(x, ((Number) y).doubleValue());
  }

  public static Number add(Number x, Double y) {
    return (Number) add(((Number) x).doubleValue(), y);
  }

  public static Number add(Float x, Number y) {
    return (Number) add(((Float) x).doubleValue(), ((Number) y).doubleValue());
  }

  public static Number add(Number x, Float y) {
    return (Number) add(((Number) x).doubleValue(), ((Float) y).doubleValue());
  }

  // Cast and fallback to LongOps

  public static Number add(Number x, Number y) {
    Class xclass = x.getClass();

    if(xclass == Integer.class)
      x = (Integer) x;
    else if(xclass == Long.class)
      x = (Long) x;
    else if(xclass == BigInt.class)
      x = (BigInt) x;
    else if(xclass == BigInteger.class)
      x = (BigInteger) x;
    else if(xclass == Ratio.class)
      x = (Ratio) x;
    else if(xclass == BigDecimal.class)
      x = (BigDecimal) x;
    else if(xclass == Float.class)
      x = (Float) x;
    else if(xclass == Double.class)
      x = (Double) x;
    else
      x = (Long) ((Number) x).longValue();

    Class yclass = y.getClass();

    if(yclass == Integer.class)
      y = (Integer) y;
    else if(yclass == Long.class)
      y = (Long) y;
    else if(yclass == BigInt.class)
      y = (BigInt) y;
    else if(yclass == BigInteger.class)
      y = (BigInteger) y;
    else if(yclass == Ratio.class)
      y = (Ratio) y;
    else if(yclass == BigDecimal.class)
      y = (BigDecimal) y;
    else if(yclass == Double.class)
      y = (Double) y;
    else if(yclass == Float.class)
      y = (Float) y;
    else
      y = (Long) ((Number) y).longValue();

    return (Number) add(x, y);
  }

}
