package clojure.lang.platform;

import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.Callable;

public final class FutureSubmission {
  public static Future submitFutureForceCallable(ThreadPoolExecutor executor, Callable callable) {
    return executor.submit(callable);
  }
}
