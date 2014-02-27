package clojure.lang.platform;

public final class Identity {
  public static boolean areIdentical(Object x, Object y) {
    return x == y;
  }
}
