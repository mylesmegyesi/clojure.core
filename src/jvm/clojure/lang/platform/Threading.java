package clojure.lang.platform;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicLong;

public class Threading {

  public static ThreadFactory createThreadFactory(final String format, final AtomicLong threadPoolCounter) {
    return new ThreadFactory() {
      @Override
      public Thread newThread(Runnable runnable) {
        Thread thread = new Thread(runnable);
        thread.setName(String.format(format, threadPoolCounter.getAndIncrement()));
        return thread;
      }
    };
  }

}

