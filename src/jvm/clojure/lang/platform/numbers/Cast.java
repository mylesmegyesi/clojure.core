package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.BigInt;
import clojure.lang.platform.Ratio;

public final class Cast {

  public static byte castToByte(Object x) {
    if (x instanceof Byte) {
      return ((Byte) x).byteValue();
    }
    long lx = Cast.castToLong(x);
    if (lx < Byte.MIN_VALUE || lx > Byte.MAX_VALUE) {
      throw new IllegalArgumentException("Value out of range for byte: " + x);
    }
    return (byte) lx;
  }

  public static short castToShort(Object x) {
    if (x instanceof Short) {
      return ((Short) x).shortValue();
    }
    long lx = Cast.castToLong(x);
    if (lx < Short.MIN_VALUE || lx > Short.MAX_VALUE) {
      throw new IllegalArgumentException("Value out of range for byte: " + x);
    }
    return (short) lx;
  }

  public static long castToLong(Object x) {
    if ((x instanceof Long) || (x instanceof Integer) ||
        (x instanceof Short) || (x instanceof Byte)) {
      return ((Number) x).longValue();
    } else if (x instanceof Double) {
      if(((Double) x) < Long.MIN_VALUE || ((Double) x) > Long.MAX_VALUE) {
        throw new IllegalArgumentException("Value out of range for long: " + x);
      }
      return ((Number) x).longValue();
    } else if (x instanceof Float) {
      if(((Float) x) < Long.MIN_VALUE || ((Float) x) > Long.MAX_VALUE) {
        throw new IllegalArgumentException("Value out of range for long: " + x);
      }
      return ((Number) x).longValue();
    } else if (x instanceof BigInteger) {
      return Cast.toLongFromBigInteger((BigInteger) x);
    } else if (x instanceof Ratio) {
      return Cast.toLongFromBigInteger(((Ratio) x).bigIntegerValue());
    } else if (x instanceof BigInt) {
      if (((BigInt) x).bipart == null) {
        return ((BigInt) x).lpart;
      } else {
        throw new IllegalArgumentException("Value out of range for long: " + x);
      }
    } else if (x instanceof Character) {
      return (long) ((Character) x).charValue();
    } else {
      return ((Number) x).longValue();
    }
  }

  private static long toLongFromBigInteger(BigInteger x) {
    if(x.bitLength() < 64) {
      return x.longValue();
    } else {
      throw new IllegalArgumentException("Value out of range for long: " + x);
    }
  }

  public static int castToInt(Object x) {
    if ((x instanceof Integer) || (x instanceof Short) || (x instanceof Byte)) {
      return ((Number) x).intValue();
    } else if (x instanceof Long) {
      long lx = ((Long) x).longValue();
      int ix = (int) lx;
      if(ix != lx) {
        throw new IllegalArgumentException("Value out of range for int: " + x);
      }
      return ix;
    } else if (x instanceof Double) {
      if(((Double) x) < Integer.MIN_VALUE || ((Double) x) > Integer.MAX_VALUE) {
        throw new IllegalArgumentException("Value out of range for int: " + x);
      }
      return ((Number) x).intValue();
    } else if (x instanceof Float) {
      if(((Float) x) < Integer.MIN_VALUE || ((Float) x) > Integer.MAX_VALUE) {
        throw new IllegalArgumentException("Value out of range for int: " + x);
      }
      return ((Number) x).intValue();
    } else if (x instanceof BigInteger) {
      return (int) Cast.toLongFromBigInteger((BigInteger) x);
    } else if (x instanceof Ratio) {
      return (int) Cast.toLongFromBigInteger(((Ratio) x).bigIntegerValue());
    } else if (x instanceof BigInt) {
      if (((BigInt) x).bipart == null) {
        return (int) ((BigInt) x).lpart;
      } else {
        throw new IllegalArgumentException("Value out of range for int: " + x);
      }
    } else if (x instanceof Character) {
      return (int) ((Character) x).charValue();
    } else {
      return ((Number) x).intValue();
    }
  }

  public static double castToDouble(Object x) {
    return ((Number) x).doubleValue();
  }

  public static float castToFloat(Object x) {
    if (x instanceof Float) {
      return ((Float) x).floatValue();
    }

    double n = ((Number) x).doubleValue();
    if (n < -Float.MAX_VALUE || n > Float.MAX_VALUE) {
      throw new IllegalArgumentException("Value out of range for float: " + x);
    }

    return (float) n;
  }

}

