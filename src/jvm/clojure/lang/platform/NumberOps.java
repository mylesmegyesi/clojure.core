package clojure.lang.platform;

public class NumberOps {

  public static int intBitAnd(int x, int y) {
    return x & y;
  }

  public static int intBitOr(int x, int y) {
    return x | y;
  }

  public static int intBitXor(int x, int y) {
    return x ^ y;
  }

  public static int intBitShiftLeft(int x, int y) {
    return x << y;
  }

  public static int intBitUnsignedShiftRight(int x, int y) {
    return x >>> y;
  }

  public static int intAdd(int x, int y) {
    return x + y;
  }

  public static int intSubtract(int x, int y) {
    return x - y;
  }

  public static int intMultiply(int x, int y) {
    return x * y;
  }

  public static int intIncrement(int x) {
    return x + 1;
  }

  public static int intDecrement(int x) {
    return x - 1;
  }

  public static long longBitXor(long x, long y) {
    return x ^ y;
  }

  public static long longBitAnd(long x, long y) {
    return x & y;
  }

  public static long longBitOr(long x, long y) {
    return x | y;
  }

  public static long longBitShiftLeft(long x, long y) {
    return x << y;
  }

  public static long longBitUnsignedShiftRight(long x, long y) {
    return x >>> y;
  }
}
