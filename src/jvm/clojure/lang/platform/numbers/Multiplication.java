package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Multiplication {

  public static Number numberMultiply(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    return Multiplication.numberMultiplyWithOpType(type, x, y);
  }

  public static Number numberMultiplyWithOpType(Ops type, Number x, Number y) {
    if (type == Ops.DOUBLE) {
      return Multiplication.doubleMultiply(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Multiplication.bigDecimalMultiply(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Multiplication.ratioMultiply(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Multiplication.bigIntMultiply(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Multiplication.longMultiply(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  // Special Case, needs to be removed at some point
  public static int integerPreserveMultiply(int x, int y) {
    return x * y;
  }

  public static Number longMultiply(long x, long y) {
    if (x == Long.MIN_VALUE && y < 0)
      throw new ArithmeticException("integer overflow");
    long ret = x * y;
    if (y != 0 && ret/y != x)
      throw new ArithmeticException("integer overflow");
    return (Number) ret;
  }

  public static Number bigIntMultiply(BigInt x, BigInt y) {
    return (Number) x.multiply(y);
  }

  public static Number ratioMultiply(Ratio x, Ratio y) {
    BigInteger ynXxn = ((BigInteger) y.getNumerator()).multiply((BigInteger) x.getNumerator());
    BigInteger ydXxd = ((BigInteger) y.getDenominator()).multiply((BigInteger) x.getDenominator());
    return Division.bigIntegerDivide(ynXxn, ydXxd);
  }

  public static Number bigDecimalMultiply(BigDecimal x, BigDecimal y) {
    return (Number) x.multiply(y);
  }

  public static Number doubleMultiply(double x, double y) {
    return (Number) Double.valueOf(x * y);
  }

  public static Number numberPrecisionMultiply(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.LONG) {
      return Multiplication.longPrecisionMultiply(Coercion.toLong(x), Coercion.toLong(y));
    } else {
      return Multiplication.numberMultiplyWithOpType(type, x, y);
    }
  }

  public static Number longPrecisionMultiply(long x, long y) {
    if (x == Long.MIN_VALUE && y < 0) {
      return Multiplication.bigIntMultiply(Coercion.toBigInt(x), Coercion.toBigInt(y));
    }
    long ret = x * y;
    if (y != 0 && ret/y != x) {
      return Multiplication.bigIntMultiply(Coercion.toBigInt(x), Coercion.toBigInt(y));
    }
    return (Number) ret;
  }

}

