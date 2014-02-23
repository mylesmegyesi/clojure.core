package clojure.lang.platform;

public final class FallBackNumber extends Number {

  private Number num;

  public FallBackNumber(long _num) {
    num = _num;
  }

  public FallBackNumber(double _num) {
    num = _num;
  }

  public int intValue() {
    return ((Number) num).intValue();
  }

  public long longValue() {
    return ((Number) num).longValue();
  }

  public double doubleValue() {
    return ((Number) num).doubleValue();
  }

  public float floatValue() {
    return ((Number) num).floatValue();
  }

}
