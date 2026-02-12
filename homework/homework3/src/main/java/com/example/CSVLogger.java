package com.example;

import java.io.*;
import java.nio.file.*;
import java.util.List;

public class CSVLogger {

  private final Path path;

  public CSVLogger(String filePath, List<String> headers) throws IOException {
    this.path = Paths.get(filePath);

    if (Files.notExists(path)) {
      try (BufferedWriter w = Files.newBufferedWriter(path)) {
        w.write(String.join(",", headers));
        w.newLine();
      }
    }
  }

  public void appendRow(List<String> values) throws IOException {
    try (BufferedWriter w = Files.newBufferedWriter(path, StandardOpenOption.APPEND)) {
      w.write(String.join(",", values));
      w.newLine();
    }
  }
}
