package com.example;
interface State {
    int size();
    long[] current();
    void swap(int i, int j);
}
