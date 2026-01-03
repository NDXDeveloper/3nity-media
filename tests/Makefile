# ═══════════════════════════════════════════════════════════════════════════════
# 3nity Media - Tests Makefile
#
# This Makefile manages the test suite for 3nity Media.
# Run from the tests/ directory.
#
# Usage:
#   make build      - Build the test runner
#   make test       - Run all tests
#   make unit       - Run unit tests only
#   make quick      - Alias for unit tests
#   make clean      - Clean test artifacts
# ═══════════════════════════════════════════════════════════════════════════════

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Lazarus build tool
LAZBUILD ?= lazbuild

# Directories
TEST_DIR := $(shell pwd)
PROJECT_ROOT := $(TEST_DIR)/..
SRC_DIR := $(PROJECT_ROOT)/src
BIN_DIR := $(TEST_DIR)/bin
REPORT_DIR := $(TEST_DIR)/reports

# Test runner
TEST_PROJECT := $(TEST_DIR)/TestRunner.lpi
TEST_RUNNER := $(BIN_DIR)/TestRunner

# Platform detection
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    PLATFORM := linux
    EXE_EXT :=
endif
ifeq ($(UNAME_S),Darwin)
    PLATFORM := darwin
    EXE_EXT :=
endif
ifeq ($(OS),Windows_NT)
    PLATFORM := windows
    EXE_EXT := .exe
endif

# Test output format (xml, plain, latex, html)
TEST_FORMAT ?= plain

# Verbosity
VERBOSE ?= 0
ifeq ($(VERBOSE),1)
    TEST_FLAGS := --all --verbose
else
    TEST_FLAGS := --all
endif

# Colors (disabled if not a terminal)
ifneq ($(TERM),)
    COLOR_GREEN := \033[0;32m
    COLOR_RED := \033[0;31m
    COLOR_YELLOW := \033[0;33m
    COLOR_BLUE := \033[0;34m
    COLOR_RESET := \033[0m
else
    COLOR_GREEN :=
    COLOR_RED :=
    COLOR_YELLOW :=
    COLOR_BLUE :=
    COLOR_RESET :=
endif

# ─────────────────────────────────────────────────────────────────────────────
# Main targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: all build test clean help

## Default target: show help
all: help

## Build the test runner
build: $(BIN_DIR)
	@echo "$(COLOR_BLUE)Building test runner...$(COLOR_RESET)"
	@if [ ! -f "$(TEST_PROJECT)" ]; then \
		echo "$(COLOR_RED)Error: TestRunner.lpi not found. Tests not yet implemented.$(COLOR_RESET)"; \
		echo "See docs/TESTING.md for the test implementation roadmap."; \
		exit 1; \
	fi
	$(LAZBUILD) --build-mode=Debug $(TEST_PROJECT)
	@echo "$(COLOR_GREEN)Test runner built successfully.$(COLOR_RESET)"

## Run all tests
test: check-runner
	@echo "$(COLOR_BLUE)Running all tests...$(COLOR_RESET)"
	$(TEST_RUNNER) $(TEST_FLAGS)

## Run unit tests only (fast)
unit: check-runner
	@echo "$(COLOR_BLUE)Running unit tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=Unit

## Alias for unit tests
quick: unit

## Run integration tests
integration: check-runner
	@echo "$(COLOR_BLUE)Running integration tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=Integration

## Run functional (GUI) tests
functional: check-runner
	@echo "$(COLOR_BLUE)Running functional tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=Functional

## Run performance tests
performance: check-runner
	@echo "$(COLOR_BLUE)Running performance tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=Performance

## Run robustness tests
robustness: check-runner
	@echo "$(COLOR_BLUE)Running robustness tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=Robustness

# ─────────────────────────────────────────────────────────────────────────────
# Specific test targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: test-mpv test-visual test-playlist test-config test-radio test-recorder test-cli test-locale

## Test MPV engine
test-mpv: check-runner
	@echo "$(COLOR_BLUE)Running MPV engine tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestMPVEngine

## Test visualizations
test-visual: check-runner
	@echo "$(COLOR_BLUE)Running visualization tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestVisualEffects

## Test playlist manager
test-playlist: check-runner
	@echo "$(COLOR_BLUE)Running playlist tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestPlaylistManager

## Test configuration
test-config: check-runner
	@echo "$(COLOR_BLUE)Running configuration tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestConfig

## Test radio manager
test-radio: check-runner
	@echo "$(COLOR_BLUE)Running radio tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestRadioManager

## Test stream recorder
test-recorder: check-runner
	@echo "$(COLOR_BLUE)Running stream recorder tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestStreamRecorder

## Test CLI parameters
test-cli: check-runner
	@echo "$(COLOR_BLUE)Running CLI tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestCLIParams

## Test localization
test-locale: check-runner
	@echo "$(COLOR_BLUE)Running localization tests...$(COLOR_RESET)"
	$(TEST_RUNNER) --suite=TTestLocale

# ─────────────────────────────────────────────────────────────────────────────
# Verbose and report targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: verbose report report-xml report-html coverage

## Run tests with verbose output
verbose:
	@$(MAKE) test VERBOSE=1

## Generate XML report
report-xml: check-runner $(REPORT_DIR)
	@echo "$(COLOR_BLUE)Generating XML report...$(COLOR_RESET)"
	$(TEST_RUNNER) --all --format=xml --output=$(REPORT_DIR)/results.xml
	@echo "$(COLOR_GREEN)Report saved to $(REPORT_DIR)/results.xml$(COLOR_RESET)"

## Generate HTML report
report-html: check-runner $(REPORT_DIR)
	@echo "$(COLOR_BLUE)Generating HTML report...$(COLOR_RESET)"
	$(TEST_RUNNER) --all --format=html --output=$(REPORT_DIR)/results.html
	@echo "$(COLOR_GREEN)Report saved to $(REPORT_DIR)/results.html$(COLOR_RESET)"

## Generate default report (XML)
report: report-xml

## Generate coverage report (requires fpc compiled with coverage support)
coverage: check-runner $(REPORT_DIR)
	@echo "$(COLOR_YELLOW)Note: Coverage requires FPC compiled with coverage support.$(COLOR_RESET)"
	@echo "$(COLOR_BLUE)Running tests with coverage...$(COLOR_RESET)"
	$(TEST_RUNNER) --all --coverage
	@echo "$(COLOR_GREEN)Coverage report saved to $(REPORT_DIR)/coverage/$(COLOR_RESET)"

# ─────────────────────────────────────────────────────────────────────────────
# Test data targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: testdata check-testdata clean-testdata

## Generate test data files using ffmpeg
testdata:
	@echo "$(COLOR_BLUE)Generating test data files...$(COLOR_RESET)"
	@if ! command -v ffmpeg >/dev/null 2>&1; then \
		echo "$(COLOR_RED)Error: ffmpeg is required to generate test data.$(COLOR_RESET)"; \
		exit 1; \
	fi
	@mkdir -p TestData/audio TestData/video TestData/subtitles TestData/playlists
	@# Audio files
	@echo "  Generating audio test files..."
	@ffmpeg -y -f lavfi -i "sine=frequency=440:duration=30" -ar 44100 -ac 2 \
		TestData/audio/test_44100_stereo.mp3 2>/dev/null || true
	@ffmpeg -y -f lavfi -i "sine=frequency=440:duration=30" -ar 48000 -ac 6 \
		TestData/audio/test_48000_51.mp3 2>/dev/null || true
	@ffmpeg -y -f lavfi -i "sine=frequency=440:duration=60" -q:a 4 \
		TestData/audio/test_vbr.mp3 2>/dev/null || true
	@# Video files
	@echo "  Generating video test files..."
	@ffmpeg -y -f lavfi -i "testsrc=duration=30:size=1280x720:rate=30" \
		-f lavfi -i "sine=frequency=440:duration=30" \
		-c:v libx264 -c:a aac TestData/video/test_720p.mp4 2>/dev/null || true
	@ffmpeg -y -f lavfi -i "testsrc=duration=30:size=1920x1080:rate=30" \
		-f lavfi -i "sine=frequency=440:duration=30" \
		-c:v libx265 -c:a aac TestData/video/test_1080p_hevc.mkv 2>/dev/null || true
	@# Subtitle files
	@echo "  Generating subtitle test files..."
	@echo -e "1\n00:00:01,000 --> 00:00:05,000\nTest subtitle line 1\n\n2\n00:00:06,000 --> 00:00:10,000\nTest subtitle line 2" \
		> TestData/subtitles/test.srt
	@echo -e "WEBVTT\n\n00:00:01.000 --> 00:00:05.000\nTest subtitle line 1\n\n00:00:06.000 --> 00:00:10.000\nTest subtitle line 2" \
		> TestData/subtitles/test.vtt
	@# Playlist files
	@echo "  Generating playlist test files..."
	@echo -e "#EXTM3U\n#EXTINF:30,Test Track 1\nTestData/audio/test_44100_stereo.mp3\n#EXTINF:60,Test Track 2\nTestData/audio/test_vbr.mp3" \
		> TestData/playlists/test.m3u
	@echo -e "[playlist]\nFile1=TestData/audio/test_44100_stereo.mp3\nTitle1=Test Track 1\nLength1=30\nFile2=TestData/audio/test_vbr.mp3\nTitle2=Test Track 2\nLength2=60\nNumberOfEntries=2\nVersion=2" \
		> TestData/playlists/test.pls
	@# Stream URLs file
	@echo -e "# Test stream URLs\nhttp://stream.radioparadise.com/aac-320\nhttp://ice1.somafm.com/groovesalad-256-mp3" \
		> TestData/streams.txt
	@echo "$(COLOR_GREEN)Test data generated successfully.$(COLOR_RESET)"

## Check if test data exists
check-testdata:
	@echo "$(COLOR_BLUE)Checking test data...$(COLOR_RESET)"
	@missing=0; \
	for f in TestData/audio/test_44100_stereo.mp3 \
	         TestData/video/test_720p.mp4 \
	         TestData/subtitles/test.srt \
	         TestData/playlists/test.m3u; do \
		if [ ! -f "$$f" ]; then \
			echo "$(COLOR_YELLOW)  Missing: $$f$(COLOR_RESET)"; \
			missing=1; \
		else \
			echo "$(COLOR_GREEN)  Found: $$f$(COLOR_RESET)"; \
		fi; \
	done; \
	if [ $$missing -eq 1 ]; then \
		echo "$(COLOR_YELLOW)Run 'make testdata' to generate missing files.$(COLOR_RESET)"; \
	else \
		echo "$(COLOR_GREEN)All test data present.$(COLOR_RESET)"; \
	fi

## Clean test data
clean-testdata:
	@echo "$(COLOR_BLUE)Cleaning test data...$(COLOR_RESET)"
	rm -rf TestData/audio/*.mp3
	rm -rf TestData/video/*.mp4 TestData/video/*.mkv
	rm -rf TestData/subtitles/*.srt TestData/subtitles/*.vtt TestData/subtitles/*.ass
	rm -rf TestData/playlists/*.m3u TestData/playlists/*.m3u8 TestData/playlists/*.pls
	rm -f TestData/streams.txt
	@echo "$(COLOR_GREEN)Test data cleaned.$(COLOR_RESET)"

# ─────────────────────────────────────────────────────────────────────────────
# Clean targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: clean clean-all

## Clean build artifacts
clean:
	@echo "$(COLOR_BLUE)Cleaning test artifacts...$(COLOR_RESET)"
	rm -rf $(BIN_DIR)
	rm -rf $(REPORT_DIR)
	rm -rf lib/
	rm -f *.o *.ppu *.compiled
	@echo "$(COLOR_GREEN)Clean complete.$(COLOR_RESET)"

## Clean everything including test data
clean-all: clean clean-testdata

# ─────────────────────────────────────────────────────────────────────────────
# CI/CD target
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: ci

## CI mode: build, generate test data, run tests with XML report
ci: build testdata $(REPORT_DIR)
	@echo "$(COLOR_BLUE)Running CI test suite...$(COLOR_RESET)"
	$(TEST_RUNNER) --all --format=xml --output=$(REPORT_DIR)/results.xml
	@echo "$(COLOR_GREEN)CI tests complete. Results in $(REPORT_DIR)/results.xml$(COLOR_RESET)"

# ─────────────────────────────────────────────────────────────────────────────
# Helper targets
# ─────────────────────────────────────────────────────────────────────────────

## Check if test runner exists
check-runner:
	@if [ ! -f "$(TEST_RUNNER)$(EXE_EXT)" ]; then \
		echo "$(COLOR_YELLOW)Test runner not found. Building...$(COLOR_RESET)"; \
		$(MAKE) build; \
	fi

## Create bin directory
$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

## Create reports directory
$(REPORT_DIR):
	@mkdir -p $(REPORT_DIR)

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: help

## Show this help
help:
	@echo ""
	@echo "$(COLOR_BLUE)3nity Media - Test Suite$(COLOR_RESET)"
	@echo ""
	@echo "$(COLOR_GREEN)Main targets:$(COLOR_RESET)"
	@echo "  make build          - Build the test runner"
	@echo "  make test           - Run all tests"
	@echo "  make unit           - Run unit tests only (fast)"
	@echo "  make quick          - Alias for unit tests"
	@echo "  make integration    - Run integration tests"
	@echo "  make functional     - Run functional (GUI) tests"
	@echo "  make clean          - Clean build artifacts"
	@echo ""
	@echo "$(COLOR_GREEN)Specific tests:$(COLOR_RESET)"
	@echo "  make test-mpv       - MPV engine tests"
	@echo "  make test-visual    - Visualization tests"
	@echo "  make test-playlist  - Playlist manager tests"
	@echo "  make test-config    - Configuration tests"
	@echo "  make test-radio     - Radio manager tests"
	@echo "  make test-recorder  - Stream recorder tests"
	@echo "  make test-cli       - CLI parameters tests"
	@echo "  make test-locale    - Localization tests"
	@echo ""
	@echo "$(COLOR_GREEN)Reports:$(COLOR_RESET)"
	@echo "  make verbose        - Run tests with verbose output"
	@echo "  make report         - Generate XML report"
	@echo "  make report-html    - Generate HTML report"
	@echo "  make coverage       - Generate coverage report"
	@echo ""
	@echo "$(COLOR_GREEN)Test data:$(COLOR_RESET)"
	@echo "  make testdata       - Generate test data files"
	@echo "  make check-testdata - Check if test data exists"
	@echo "  make clean-testdata - Remove test data files"
	@echo ""
	@echo "$(COLOR_GREEN)CI/CD:$(COLOR_RESET)"
	@echo "  make ci             - Full CI run (build + testdata + test)"
	@echo ""
	@echo "$(COLOR_GREEN)Variables:$(COLOR_RESET)"
	@echo "  VERBOSE=1           - Enable verbose output"
	@echo "  LAZBUILD=/path      - Custom lazbuild path"
	@echo ""
