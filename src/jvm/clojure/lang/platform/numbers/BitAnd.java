package clojure.lang.platform.numbers;

public final class BitAnd {

  public static long numberBitAnd(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx & ly;
  }

  public static int integerPreserveBitAnd(int x, int y) {
    return x & y;
  }

}
