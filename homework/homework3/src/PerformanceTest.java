package com.example;

import java.util.ArrayList;
import java.util.List;

public class PerformanceTest {
  private static int[] values = {5, 100, 300};
  private static int[] numThreads = {1, 8, 16, 32, 40};
  private static int nTransitions = 100000000;
  private static boolean[] virtual = {true, false};
  private static ArrayList<String> stateTypes =
      new ArrayList<String>(List.of("AcmeSafe", "Synchronized", "Unsynchronized"));
  private static int nTrials = 3;
  private static CSVLogger logger;

  public static void main(String args[]) {
    System.out.println("Running tests...");
    try {
      logger =
          new CSVLogger(
              "profiling/",
              List.of(
                  "n_values",
                  "n_threads",
                  "virtual",
                  "state_type",
                  "total_time",
                  "avg_time",
                  "n_mismatches"));
    } catch (Exception e) {
      e.printStackTrace();
    }
    runTests();
    System.exit(0);
  }

  private static void runTests() {
    try {
      for (int nvalues : values) {
        for (int nthreads : numThreads) {
          for (boolean virt : virtual) {
            for (String state_type : stateTypes) {
              System.out.format(
                  "\nvalues: %d; nthreads: %d; ntransitions: %d; thread_type: %s",
                  nvalues, nthreads, nTransitions, virt ? "virtual" : "platform");

              State state;
              if (state_type.equals("Null")) {
                state = new NullState(nvalues);
              } else if (state_type.equals("Synchronized")) {
                state = new SynchronizedState(nvalues);
              } else if (state_type.equals("Unsynchronized")) {
                state = new UnsynchronizedState(nvalues);
              } else if (state_type.equals("AcmeSafe")) {
                state = new AcmeSafeState(nvalues);
              } else throw new Exception(state_type);

              // take average over nTrials runs.
              long time = 0;
              long nMismatches = 0;
              for (int i = 0; i < nTrials; i++) {
                time += dowork(virt, nthreads, nTransitions, state);
                nMismatches += test(state.current());
              }
              time /= nTrials;
              nMismatches /= nTrials;

              double dTransitions = nTransitions;

              logger.appendRow(
                  List.of(
                      String.valueOf(nvalues),
                      String.valueOf(nthreads),
                      String.valueOf(virt),
                      String.valueOf(state_type),
                      String.valueOf(time / 1e9), // total time (seconds)
                      String.valueOf(time / dTransitions * nthreads), // avg time (nanoseconds)
                      String.valueOf(nMismatches)));
            }
          }
        }
      }
    } catch (Exception e) {
      // TODO: handle exception
      System.err.println("Error: " + e.getMessage());
    }
  }

  private static long dowork(boolean virtual, int nThreads, long nTransitions, State s)
      throws InterruptedException {
    var builder = virtual ? Thread.ofVirtual() : Thread.ofPlatform();
    var test = new SwapTest[nThreads];
    var t = new Thread[nThreads];
    for (var i = 0; i < nThreads; i++) {
      var threadTransitions = (nTransitions / nThreads + (i < nTransitions % nThreads ? 1 : 0));
      test[i] = new SwapTest(threadTransitions, s);
      t[i] = builder.unstarted(test[i]);
    }
    // times the execution of the nThreads threads
    var realtimeStart = System.nanoTime();
    for (var i = 0; i < nThreads; i++) t[i].start();
    for (var i = 0; i < nThreads; i++) t[i].join();
    var realtimeEnd = System.nanoTime();

    long realtime = realtimeEnd - realtimeStart;
    return realtime;
  }

  private static long test(long[] output) {
    long osum = 0;
    for (var i = 0; i < output.length; i++) osum += output[i];
    return osum;
  }
}
