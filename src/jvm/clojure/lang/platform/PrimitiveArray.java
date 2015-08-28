package clojure.lang.platform;

public final class PrimitiveArray {

  public static boolean[] castToBooleans(Object arr) {
    return (boolean[]) arr;
  }

  public static byte[] castToBytes(Object arr) {
    return (byte[]) arr;
  }

  public static char[] castToChars(Object arr) {
    return (char[]) arr;
  }

  public static short[] castToShorts(Object arr) {
    return (short[]) arr;
  }

  public static float[] castToFloats(Object arr) {
    return (float[]) arr;
  }

  public static double[] castToDoubles(Object arr) {
    return (double[]) arr;
  }

  public static int[] castToInts(Object arr) {
    return (int[]) arr;
  }

  public static long[] castToLongs(Object arr) {
    return (long[]) arr;
  }

  public static byte[] byteArrayForSize(Number size) {
    return new byte[size.intValue()];
  }

  public static short[] shortArrayForSize(Number size) {
    return new short[size.intValue()];
  }

  public static int[] intArrayForSize(Number size) {
    return new int[size.intValue()];
  }

  public static long[] longArrayForSize(Number size) {
    return new long[size.intValue()];
  }

  public static float[] floatArrayForSize(Number size) {
    return new float[size.intValue()];
  }

  public static double[] doubleArrayForSize(Number size) {
    return new double[size.intValue()];
  }

  public static boolean[] booleanArrayForSize(Number size) {
    return new boolean[size.intValue()];
  }

  public static char[] charArrayForSize(Number size) {
    return new char[size.intValue()];
  }

}
