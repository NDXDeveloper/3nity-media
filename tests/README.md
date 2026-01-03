# 3nity Media - Test Suite

This directory contains the complete test suite for 3nity Media, built using the FPCUnit testing framework for Free Pascal/Lazarus.

## Overview

The test suite is organized into several categories:

| Directory | Description | Tests |
|-----------|-------------|-------|
| [Unit/](Unit/) | Unit tests for individual components | 940+ |
| [Integration/](Integration/) | Integration tests for component interactions | 198+ |
| [Mocks/](Mocks/) | Mock implementations for testing | - |
| [Performance/](Performance/) | Performance and benchmark tests | 252+ |
| [Robustness/](Robustness/) | Stress tests and edge case handling | 296+ |
| [Functional/](Functional/) | End-to-end functional tests | TBD |
| [TestData/](TestData/) | Sample media files for testing | - |

**Total: 1686+ tests**

## Requirements

- Free Pascal Compiler (FPC) 3.2.2 or later
- Lazarus IDE 3.0 or later
- FPCUnit testing framework (included with FPC)

## Building the Test Suite

### Using Lazarus IDE

1. Open `TestRunner.lpi` in Lazarus
2. Build the project (Ctrl+F9)
3. The executable will be created in `bin/TestRunner`

### Using Command Line

```bash
# Using lazbuild
lazbuild TestRunner.lpi

# Or using the Makefile
make build
```

## Running Tests

### Run All Tests

```bash
./bin/TestRunner --all
```

### Run Specific Test Suite

```bash
# Run only unit tests
./bin/TestRunner --suite=Unit

# Run only integration tests
./bin/TestRunner --suite=Integration

# Run a specific test class
./bin/TestRunner --suite=TTestPlaylistManager
```

### Output Formats

```bash
# Plain text output (default)
./bin/TestRunner --all --format=plain

# XML output for CI/CD
./bin/TestRunner --all --format=xml

# Verbose output
./bin/TestRunner --all -v
```

### Common Options

| Option | Description |
|--------|-------------|
| `--all` | Run all registered tests |
| `--suite=NAME` | Run specific test suite or class |
| `--format=plain\|xml` | Output format |
| `-v, --verbose` | Verbose output |
| `--list` | List all available tests |
| `--help` | Show help message |

## Test Categories

### Unit Tests (`Unit/`)

Test individual components in isolation:
- Configuration management
- Playlist operations
- MPV engine wrapper
- Radio station management
- Stream recording
- Visual effects
- Keyboard shortcuts
- Localization

### Integration Tests (`Integration/`)

Test component interactions:
- MPV + Playlist integration
- Playback workflows
- Audio/Video handling
- Streaming functionality
- Error scenarios

### Mock Objects (`Mocks/`)

Mock implementations for testing without external dependencies:
- `TMockMPVEngine` - Simulates MPV player behavior
- `TMockPlaylistManager` - In-memory playlist management

### Performance Tests (`Performance/`)

Benchmark and performance validation:
- Startup time
- Media loading speed
- Seek performance
- Memory usage

### Robustness Tests (`Robustness/`)

Stress testing and edge cases:
- Corrupted file handling
- Network timeout recovery
- Rapid command sequences
- Long playback sessions

## Writing New Tests

### Basic Test Structure

```pascal
unit uTestMyComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  TTestMyComponent = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_SomeFeature;
    procedure Test_AnotherFeature;
  end;

implementation

procedure TTestMyComponent.SetUp;
begin
  // Initialize test fixtures
end;

procedure TTestMyComponent.TearDown;
begin
  // Clean up
end;

procedure TTestMyComponent.Test_SomeFeature;
begin
  AssertEquals('Expected value', 42, ActualValue);
  AssertTrue('Condition should be true', SomeCondition);
end;

initialization
  RegisterTest(TTestMyComponent);

end.
```

### Adding Tests to the Project

1. Create your test unit in the appropriate directory
2. Add the unit to `TestRunner.lpr` in the uses clause
3. Add the unit to `TestRunner.lpi` project file
4. Rebuild and run

## Continuous Integration

The test suite is designed to work with CI/CD pipelines:

```bash
# Run tests with XML output for CI
./bin/TestRunner --all --format=xml > test-results.xml

# Check exit code (0 = success, non-zero = failures)
echo $?
```

## Test Data

The `TestData/` directory contains sample files for testing:
- `audio/` - Sample audio files (MP3, FLAC, OGG, WAV, M4A, OPUS)
- `video/` - Sample video files (MP4, MKV, WebM, AVI)
- `playlists/` - Sample playlist files (M3U, M3U8, PLS)
- `subtitles/` - Sample subtitle files (SRT, ASS, VTT)

### Generating Test Files

Test media files can be generated using the provided scripts. These scripts use FFmpeg to create various audio and video formats for testing.

#### Requirements

- **FFmpeg** with libx264, libx265, libmp3lame, libvorbis, libopus encoders

#### Available Scripts

| Script | Platform | Description |
|--------|----------|-------------|
| `generate_test_files.sh` | Linux (bash) | Native bash script |
| `generate_test_files_linux.ps1` | Linux (PowerShell) | PowerShell Core script |
| `generate_test_files_win.ps1` | Windows (PowerShell) | PowerShell 5.1+ script |
| `generate_test_files.bat` | Windows (CMD) | Batch script for older systems |

#### Usage

**Linux (bash):**
```bash
./generate_test_files.sh [--all|--audio|--video|--subtitles|--playlists]
```

**Linux (PowerShell):**
```bash
pwsh ./generate_test_files_linux.ps1 [-All] [-Audio] [-Video] [-Subtitles] [-Playlists]
```

**Windows (PowerShell):**
```powershell
.\generate_test_files_win.ps1 [-All] [-Audio] [-Video] [-Subtitles] [-Playlists]
```

**Windows (CMD):**
```cmd
generate_test_files.bat [all|audio|video|subtitles|playlists]
```

#### Generated Files

| Category | Files Generated |
|----------|-----------------|
| Audio | MP3 (44.1/48kHz, VBR), OGG, FLAC, WAV, M4A, OPUS, silence, short |
| Video | MP4 (720p, 1080p, 4:3), MKV (HEVC, multi-audio), WebM (VP9), AVI |
| Subtitles | SRT (EN, FR), ASS, VTT |
| Playlists | M3U, M3U8, PLS, video playlist, mixed playlist, stream URLs |

## Troubleshooting

### Tests fail with "libmpv not found"

Unit tests use mock objects and don't require libmpv. Integration tests that need the real MPV engine will skip if libmpv is not available.

### Memory leaks reported

The test runner uses heaptrc for memory leak detection. Any leaks will be reported at the end of the test run. A clean run shows:
```
0 unfreed memory blocks : 0
True heap
```

### Tests hang or timeout

Some performance tests have built-in timeouts. If tests hang, check:
- Network connectivity for streaming tests
- Available system resources
- Test data file availability

## License

GPL-2.0 - Same as 3nity Media

## Author

Nicolas DEOUX (NDXDev@gmail.com)
