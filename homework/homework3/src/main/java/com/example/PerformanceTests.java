package com.example;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.List;

class PerformanceTest {
  private static int[] values = {5, 100};
  private static int[] numThreads = {1, 8, 40};
  private static int nTransitions = 100000000;
  private static boolean[] virtual = {true, false};
  private static ArrayList<String> stateTypes =
      new ArrayList<String>(List.of("Null", "Synchronized", "Unsynchronized"));
  private static String logFilePath = "logs/log.txt";
  private static CSVLogger logger;

  public static void main(String args[]) {
    System.out.println("Running tests...");
    try {
      logger =
          new CSVLogger(
              "results.csv",
              List.of(
                  "n_values",
                  "n_threads",
                  "virtual",
                  "state_type",
                  "total_time",
                  "avg_time",
                  "n_mismatches"));
    } catch (Exception e) {
      // TODO: handle exception
    }

    try (BufferedWriter writer = new BufferedWriter(new FileWriter(logFilePath, false))) {
    } catch (Exception e) {
      // TODO: catch exception
    }

    runTests();
    System.exit(0);
  }

  private static void runTests() {
    try {
      for (int val : values) {
        for (int nthread : numThreads) {

          for (boolean virt : virtual) {
            StringBuilder sb = new StringBuilder();
            sb.append("\n");
            // sb.append(
            //     "\n---------------------------------------------------------------------------");
            // sb.append(
            //     String.format(
            //         "\nnvalues: %d, nthreads: %d; ntransitions: %d; thread_type: %s",
            //         val, nthread, nTransitions, virt ? "virtual" : "platform"));
            // sb.append(
            //     "\n---------------------------------------------------------------------------");

            for (String state_type : stateTypes) {
              System.out.format(
                  "\nvalues: %d; nthreads: %d; ntransitions: %d; thread_type: %s",
                  val, nthread, nTransitions, virt ? "virtual" : "platform");

              State state;
              if (state_type.equals("Null")) {
                state = new NullState(val);
              } else if (state_type.equals("Synchronized")) {
                state = new SynchronizedState(val);
              } else if (state_type.equals("Unsynchronized")) {
                state = new UnsynchronizedState(val);
              } else throw new Exception(state_type);

              int nThreads = (int) Math.max(Math.min(nthread, Integer.MAX_VALUE), 1);
              long nTrans = Math.max(Math.min(nTransitions, Long.MAX_VALUE), 0);

              long time = dowork(virt, nThreads, nTrans, state);
              long testResult = test(state.current());
              double dTransitions = nTrans;

              // "n_values",
              // "n_threads",
              // "virtual",
              // "state_type",
              // "total_time",
              // "avg_time",
              // "n_mismatches"));
              logger.appendRow(
                  List.of(
                      String.valueOf(val),
                      String.valueOf(nThreads),
                      String.valueOf(virt),
                      String.valueOf(state_type),
                      String.valueOf(time / 1e9),
                      String.valueOf(time / dTransitions * nThreads),
                      String.valueOf(testResult)));
              //   sb.append(
              //       String.format(
              //           "\n%-17s total (s): %-12g avg (ns): %-12g sum: %d",
              //           state_type, time / 1e9, time / dTransitions * nThreads, testResult));
              // }
              // sb.append(
              //
              // "\n----------------------------------------------------------------------------");
              // try (BufferedWriter writer = new BufferedWriter(new FileWriter(logFilePath, true)))
              // {
              //   writer.write(sb.toString());
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
    // double dTransitions = nTransitions;
    // System.out.format("Total real time %g s\n",
    // realtime / 1e9);
    // System.out.format("Average real swap time %g ns\n",
    // realtime / dTransitions * nThreads);
  }

  private static long test(long[] output) {
    long osum = 0;
    for (var i = 0; i < output.length; i++) osum += output[i];
    // if (osum != 0)
    // error("output sum mismatch", osum, 0);
    return osum;
  }

  private static void usage(Exception e) {
    if (e != null) System.err.println(e);
    System.err.println("Arguments: [Platform|Virtual] nvalues model" + " nthreads ntransitions\n");
    System.exit(1);
  }

  private static void error(String s, long i, long j) {
    System.err.format("%s (%d != %d)\n", s, i, j);
    System.exit(1);
  }
}
