package clojure.lang.platform.numbers;

import java.math.BigInteger;
import java.math.BigDecimal;
import clojure.lang.platform.BigInt;
import clojure.lang.platform.Ratio;

public final class Cast {

  public static Number castToNumber(Object x) {
    return (Number) x;
  }

  public static Number castToNumber(long x) {
    return Long.valueOf(x);
  }

  public static Number castToNumber(float x) {
    return Float.valueOf(x);
  }

  public static Number castToNumber(double x) {
    return Double.valueOf(x);
  }

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

  public static byte uncheckedCastToByte(Object x) {
    return ((Number) x).byteValue();
  }

  public static byte uncheckedCastToByte(byte x) {
    return x;
  }

  public static byte uncheckedCastToByte(short x) {
      return (byte) x;
  }

  public static byte uncheckedCastToByte(int x) {
    return (byte) x;
  }

  public static byte uncheckedCastToByte(long x) {
    return (byte) x;
  }

  public static byte uncheckedCastToByte(float x) {
    return (byte) x;
  }

  public static byte uncheckedCastToByte(double x) {
    return (byte) x;
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

  public static short uncheckedCastToShort(Object x) {
    return ((Number) x).shortValue();
  }

  public static short uncheckedCastToShort(byte x) {
    return (short) x;
  }

  public static short uncheckedCastToShort(short x) {
    return x;
  }

  public static short uncheckedCastToShort(int x) {
    return (short) x;
  }

  public static short uncheckedCastToShort(long x) {
    return (short) x;
  }

  public static short uncheckedCastToShort(float x) {
    return (short) x;
  }

  public static short uncheckedCastToShort(double x) {
    return (short) x;
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
      if (((BigInt) x).getBipart() == null) {
        return (int) ((BigInt) x).getLpart();
      } else {
        throw new IllegalArgumentException("Value out of range for int: " + x);
      }
    } else if (x instanceof Character) {
      return (int) ((Character) x).charValue();
    } else {
      return ((Number) x).intValue();
    }
  }

  public static int uncheckedCastToInt(Object x) {
    return ((Number) x).intValue();
  }

  public static int uncheckedCastToInt(byte x) {
    return (int) x;
  }

  public static int uncheckedCastToInt(short x) {
    return (int) x;
  }

  public static int uncheckedCastToInt(int x) {
    return x;
  }

  public static int uncheckedCastToInt(long x) {
    return (int) x;
  }

  public static int uncheckedCastToInt(float x) {
    return (int) x;
  }

  public static int uncheckedCastToInt(double x) {
    return (int) x;
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
      if (((BigInt) x).getBipart() == null) {
        return ((BigInt) x).getLpart();
      } else {
        throw new IllegalArgumentException("Value out of range for long: " + x);
      }
    } else if (x instanceof Character) {
      return (long) ((Character) x).charValue();
    } else {
      return ((Number) x).longValue();
    }
  }

  public static long uncheckedCastToLong(Object x) {
    return ((Number) x).longValue();
  }

  public static long uncheckedCastToLong(byte x) {
    return (long) x;
  }

  public static long uncheckedCastToLong(short x) {
    return (long) x;
  }

  public static long uncheckedCastToLong(int x) {
    return (long) x;
  }

  public static long uncheckedCastToLong(long x) {
    return x;
  }

  public static long uncheckedCastToLong(float x) {
    return (long) x;
  }

  public static long uncheckedCastToLong(double x) {
    return (long) x;
  }

  private static long toLongFromBigInteger(BigInteger x) {
    if(x.bitLength() < 64) {
      return x.longValue();
    } else {
      throw new IllegalArgumentException("Value out of range for long: " + x);
    }
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

  public static float uncheckedCastToFloat(Object x) {
    return ((Number) x).floatValue();
  }

  public static float uncheckedCastToFloat(byte x) {
    return (float) x;
  }

  public static float uncheckedCastToFloat(short x) {
    return (float) x;
  }

  public static float uncheckedCastToFloat(int x) {
    return (float) x;
  }

  public static float uncheckedCastToFloat(long x) {
    return (float) x;
  }

  public static float uncheckedCastToFloat(float x) {
    return x;
  }

  public static float uncheckedCastToFloat(double x) {
    return (float) x;
  }

  public static double castToDouble(Object x) {
    return ((Number) x).doubleValue();
  }

  public static double uncheckedCastToDouble(Object x) {
    return ((Number) x).doubleValue();
  }

  public static double uncheckedCastToDouble(byte x) {
    return (double) x;
  }

  public static double uncheckedCastToDouble(short x) {
    return (double) x;
  }

  public static double uncheckedCastToDouble(int x) {
    return (double) x;
  }

  public static double uncheckedCastToDouble(long x) {
    return (double) x;
  }

  public static double uncheckedCastToDouble(float x) {
    return (double) x;
  }

  public static double uncheckedCastToDouble(double x) {
    return x;
  }

  public static char castToChar(byte x) {
    char c = (char) x;
    if (c != x) {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    } else {
      return c;
    }
  }

  public static char castToChar(short x) {
    char c = (char) x;
    if (c != x) {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    } else {
      return c;
    }
  }

  public static char castToChar(char x) {
    return x;
  }

  public static char castToChar(int x) {
    char c = (char) x;
    if (c != x) {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    } else {
      return c;
    }
  }

  public static char castToChar(long x) {
    char c = (char) x;
    if (c != x) {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    } else {
      return c;
    }
  }

  public static char castToChar(float x) {
    if (x >= Character.MIN_VALUE && x <= Character.MAX_VALUE) {
      return (char) x;
    } else {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    }
  }

  public static char castToChar(double x) {
    if (x >= Character.MIN_VALUE && x <= Character.MAX_VALUE) {
      return (char) x;
    } else {
      throw new IllegalArgumentException("Value out of range for char: " + x);
    }
  }

  public static char castToChar(Object x) {
    if (x instanceof Character) {
      return ((Character) x).charValue();
    } else {
      long l = ((Number) x).longValue();
      if (l < Character.MIN_VALUE || l > Character.MAX_VALUE) {
        throw new IllegalArgumentException("Value out of range for char: " + x);
      } else {
        return (char) l;
      }
    }
  }

}

