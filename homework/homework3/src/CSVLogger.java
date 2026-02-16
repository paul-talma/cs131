package com.example;

import java.io.*;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class CSVLogger {

  private final Path path;

  public CSVLogger(String baseFilePath, List<String> headers) throws IOException {
    String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("dd_HH_mm"));
    String fileName = baseFilePath + "_" + timestamp + ".csv";
    this.path = Paths.get(fileName);

    try (BufferedWriter w =
        Files.newBufferedWriter(
            path, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)) {
      w.write(String.join(",", headers));
      w.newLine();
    }
  }

  public void appendRow(List<String> values) throws IOException {
    try (BufferedWriter w = Files.newBufferedWriter(path, StandardOpenOption.APPEND)) {
      w.write(String.join(",", values));
      w.newLine();
    }
  }
}
