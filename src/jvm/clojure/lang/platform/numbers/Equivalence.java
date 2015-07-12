package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Equivalence {

  public static boolean numberEqual(Number x, Number y) {
    if (CategoryType.findCategoryType(x) == CategoryType.findCategoryType(y)) {
      return numbersEquivalent(x, y);
    } else {
      return false;
    }
  }

  public static boolean numbersEquivalent(Number x, Number y) {
    Ops type = OpType.findOpType(x, y);
    if (type == Ops.DOUBLE) {
      return Equivalence.doubleEquivalent(Coercion.toDouble(x), Coercion.toDouble(y));
    } else if (type == Ops.BIGDECIMAL) {
      return Equivalence.bigDecimalEquivalent(Coercion.toBigDecimal(x), Coercion.toBigDecimal(y));
    } else if (type == Ops.RATIO) {
      return Equivalence.ratioEquivalent(Coercion.toRatio(x), Coercion.toRatio(y));
    } else if (type == Ops.BIGINT) {
      return Equivalence.bigIntEquivalent(Coercion.toBigInt(x), Coercion.toBigInt(y));
    } else {
      return Equivalence.longEquivalent(Coercion.toLong(x), Coercion.toLong(y));
    }
  }

  public static boolean longEquivalent(long x, long y) {
    return x == y;
  }

  public static boolean bigIntEquivalent(BigInt x, BigInt y) {
    return x.equals(y);
  }

  public static boolean ratioEquivalent(Ratio x, Ratio y) {
    return x.getNumerator().equals(y.getNumerator()) &&
           x.getDenominator().equals(y.getDenominator());
  }

  public static boolean bigDecimalEquivalent(BigDecimal x, BigDecimal y) {
    return x.compareTo(y) == 0;
  }

  public static boolean doubleEquivalent(double x, double y) {
    return x == y;
  }

}

