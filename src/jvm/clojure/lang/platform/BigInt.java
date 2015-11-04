package clojure.lang.platform;

import java.math.BigInteger;
import java.math.BigDecimal;

public final class BigInt extends Number {

  private final long lpart;
  private final BigInteger bipart;

  final public static BigInt ZERO = new BigInt(0,null);
  final public static BigInt ONE = new BigInt(1,null);

  private BigInt(long lpart, BigInteger bipart) {
    this.lpart = lpart;
    this.bipart = bipart;
  }

  public long getLpart() {
    return lpart;
  }

  public BigInteger getBipart() {
    return bipart;
  }

  public static BigInt fromBigInteger(BigInteger bi) {
    if (bi.bitLength() < 64) {
      return new BigInt(bi.longValue(), null);
    } else {
      return new BigInt(0, bi);
    }
  }

  public BigInteger toBigInteger() {
    if (bipart == null) {
      return BigInteger.valueOf(lpart);
    } else {
      return bipart;
    }
  }

  public static BigInt fromLong(long l) {
    return new BigInt(l, null);
  }

  public static BigInt valueOf(long l) {
    return new BigInt(l, null);
  }

  public BigDecimal toBigDecimal() {
    if (bipart == null) {
      return BigDecimal.valueOf(lpart);
    } else {
      return new BigDecimal(bipart);
    }
  }

  public int bitLength() {
    return toBigInteger().bitLength();
  }

  public byte byteValue() {
    if (bipart == null) {
      return (byte) lpart;
    } else {
      return bipart.byteValue();
    }
  }

  public double doubleValue() {
    if (bipart == null) {
      return lpart;
    } else {
      return bipart.doubleValue();
    }
  }

  public float floatValue() {
    if (bipart == null) {
      return lpart;
    } else {
      return bipart.floatValue();
    }
  }

  public int intValue() {
    if (bipart == null) {
      return (int) lpart;
    } else {
      return bipart.byteValue();
    }
  }

  public long longValue() {
    if (bipart == null) {
      return lpart;
    } else {
      return bipart.longValue();
    }
  }

  public short shortValue() {
    if (bipart == null) {
      return (short) lpart;
    } else {
      return bipart.shortValue();
    }
  }

  public BigInt add(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
      long ret = lpart + y.lpart;
      if ((ret ^ lpart) >= 0 || (ret ^ y.lpart) >= 0) {
          return BigInt.valueOf(ret);
      } else {
        return BigInt.fromBigInteger(this.toBigInteger().add(y.toBigInteger()));
      }
    } else {
      return BigInt.fromBigInteger(this.toBigInteger().add(y.toBigInteger()));
    }
  }

  public BigInt multiply(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
      long ret = lpart * y.lpart;
      if (y.lpart == 0 || (ret / y.lpart == lpart && lpart != Long.MIN_VALUE)) {
        return BigInt.valueOf(ret);
      } else {
        return BigInt.fromBigInteger(this.toBigInteger().multiply(y.toBigInteger()));
      }
    } else {
      return BigInt.fromBigInteger(this.toBigInteger().multiply(y.toBigInteger()));
    }
  }

  public BigInt quotient(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
      if (lpart == Long.MIN_VALUE && y.lpart == -1) {
        return BigInt.fromBigInteger(this.toBigInteger().negate());
      } else {
        return BigInt.valueOf(lpart / y.lpart);
      }
    } else {
      return BigInt.fromBigInteger(this.toBigInteger().divide(y.toBigInteger()));
    }
  }

  public BigInt remainder(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        return BigInt.valueOf(lpart % y.lpart);
    } else {
      return BigInt.fromBigInteger(this.toBigInteger().remainder(y.toBigInteger()));
    }
  }

  public boolean lt(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        return lpart < y.lpart;
    } else {
      return this.toBigInteger().compareTo(y.toBigInteger()) < 0;
    }
  }

  public int hashCode() {
    if (bipart == null) {
      return (int) (lpart ^ (lpart >>> 32));
    } else {
      return bipart.hashCode();
    }
  }

  public boolean equals(Object obj){
    if(this == obj) {
      return true;
    } else if (obj instanceof BigInt) {
      BigInt o = (BigInt) obj;
      if(bipart == null) {
        return o.bipart == null && this.lpart == o.lpart;
      } else {
        return o.bipart != null && this.bipart.equals(o.bipart);
      }
    } else {
      return false;
    }
  }

  public String toString() {
    if(bipart == null) {
      return String.valueOf(lpart);
    } else {
      return bipart.toString();
    }
  }

}

