SRC_DIR = src
BIN_DIR = bin
MAIN_CLASS = com.example.UnsafeMemory
PERF_CLASS = com.example.PerformanceTest
DIST_DIR = dist
JAR_NAME = jmmplus.jar

SOURCES = $(shell find $(SRC_DIR) -name "*.java")

all: compile

compile:
	mkdir -p $(BIN_DIR)
	javac -d $(BIN_DIR) $(SOURCES)

run: compile
	java -cp $(BIN_DIR) $(MAIN_CLASS) $(ARGS)

time: compile
	time java -cp $(BIN_DIR) $(MAIN_CLASS) $(ARGS)

perf: compile
	java -cp $(BIN_DIR) $(PERF_CLASS)

jar: compile
	mkdir -p $(DIST_DIR)
	jar cfe $(DIST_DIR)/$(JAR_NAME) $(MAIN_CLASS) -C $(BIN_DIR) .

