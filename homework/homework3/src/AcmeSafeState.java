package com.example;

import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
  private final AtomicLongArray value;

  AcmeSafeState(int length) {
    value = new AtomicLongArray(length);
  }

  public int size() {
    return value.length();
  }

  public long[] current() {
    long[] snapshot = new long[value.length()];
    for (int i = 0; i < value.length(); i++) {
      snapshot[i] = value.get(i);
    }
    return snapshot;
  }

  public void swap(int i, int j) {
    value.getAndDecrement(i);
    value.getAndIncrement(j);
  }
}
