package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.platform.BigInt;
import clojure.lang.platform.Ratio;

public final class CategoryType {

  public static final Categories findCategoryType(Number x) {
    if (x instanceof Long) {
      return Categories.INT;
    } else if ((x instanceof Double) || (x instanceof Float)) {
      return Categories.FLOAT;
    } else if (x instanceof BigDecimal) {
      return Categories.DECIMAL;
    } else if (x instanceof Ratio) {
      return Categories.RATIO;
    } else {
      return Categories.INT;
    }
  }

}

