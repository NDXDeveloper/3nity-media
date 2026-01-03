# Performance Tests

This directory contains performance and benchmark tests for 3nity Media. These tests measure execution time, memory usage, and resource efficiency.

## Test Files

| File | Area | Description |
|------|------|-------------|
| `uTestStartup.pas` | Initialization | Application startup time benchmarks |
| `uTestMediaLoading.pas` | Media | File loading performance tests |
| `uTestSeekPerformance.pas` | Playback | Seek operation benchmarks |
| `uTestMemoryUsage.pas` | Resources | Memory allocation and cleanup tests |

## Test Count

- **Total Performance Tests: 252+**

## Running Performance Tests Only

```bash
./bin/TestRunner --suite=Performance
```

## Test Coverage

### Startup Performance (`uTestStartup.pas`)

Tests application initialization speed:

| Test | Threshold | Description |
|------|-----------|-------------|
| Config Load | < 50ms | Load configuration from INI file |
| Playlist Parse | < 100ms | Parse 100-item M3U playlist |
| Locale Load | < 30ms | Load translation file |
| Shortcut Load | < 20ms | Load keyboard shortcuts |
| Component Init | < 200ms | Initialize all managers |

### Media Loading (`uTestMediaLoading.pas`)

Tests file loading performance:

| Test | Threshold | Description |
|------|-----------|-------------|
| Local Audio | < 500ms | Load local audio file |
| Local Video | < 1000ms | Load local video file |
| Network Stream | < 3000ms | Connect to HTTP stream |
| Large Playlist | < 2000ms | Load 1000-item playlist |
| Metadata Extract | < 200ms | Extract media metadata |

### Seek Performance (`uTestSeekPerformance.pas`)

Tests seek operation speed:

| Test | Threshold | Description |
|------|-----------|-------------|
| Seek Forward | < 100ms | Seek forward 30 seconds |
| Seek Backward | < 100ms | Seek backward 30 seconds |
| Seek to Start | < 50ms | Seek to beginning |
| Seek to End | < 50ms | Seek to end |
| Rapid Seeks | < 500ms | 10 rapid sequential seeks |
| Precision Seek | < 100ms | Seek to exact timestamp |

### Memory Usage (`uTestMemoryUsage.pas`)

Tests resource management:

| Test | Criteria | Description |
|------|----------|-------------|
| Startup Memory | < 50MB | Memory after initialization |
| Playback Memory | < 100MB | Memory during playback |
| Playlist Memory | O(n) | Linear growth with playlist size |
| Track Change | No leak | Memory stable across track changes |
| Long Playback | No growth | Memory stable over time |
| Cache Management | Bounded | Cache size within limits |

## Benchmark Results Format

Performance tests report results with timing information:

```
TTestStartup Time:00.150 N:6 E:0 F:0 I:0
  00.012  Test_ConfigLoad_Under50ms
  00.045  Test_PlaylistParse_Under100ms
  00.008  Test_LocaleLoad_Under30ms
  00.005  Test_ShortcutLoad_Under20ms
  00.080  Test_ComponentInit_Under200ms
```

## Writing Performance Tests

### Timing Test Example

```pascal
procedure TTestStartup.Test_ConfigLoad_Under50ms;
var
  StartTime: QWord;
  ElapsedMs: Integer;
begin
  StartTime := GetTickCount64;

  FConfig := TConfig.Create;
  FConfig.LoadFromFile('test.ini');

  ElapsedMs := GetTickCount64 - StartTime;

  AssertTrue('Config load should be under 50ms, was ' + IntToStr(ElapsedMs) + 'ms',
    ElapsedMs < 50);
end;
```

### Memory Test Example

```pascal
procedure TTestMemoryUsage.Test_NoLeakOnTrackChange;
var
  InitialMem, FinalMem: PtrUInt;
  I: Integer;
begin
  InitialMem := GetHeapStatus.TotalAllocated;

  for I := 1 to 100 do
  begin
    FPlayer.LoadFile('/music/song' + IntToStr(I mod 10) + '.mp3');
    FPlayer.Stop;
  end;

  FinalMem := GetHeapStatus.TotalAllocated;

  AssertTrue('Memory should not grow significantly',
    FinalMem < InitialMem * 1.1);  // Allow 10% variance
end;
```

## Performance Thresholds

Performance thresholds are configurable and may vary by:
- Hardware capabilities
- Operating system
- Media file characteristics
- Network conditions

Default thresholds are set for typical consumer hardware. Adjust as needed for your environment.

## Profiling Integration

For detailed profiling, you can use external tools:

```bash
# Linux - using perf
perf record ./bin/TestRunner --suite=Performance
perf report

# Valgrind - memory profiling
valgrind --tool=massif ./bin/TestRunner --suite=TTestMemoryUsage
```

## Continuous Integration

Performance tests can be used in CI to detect regressions:

```bash
# Run performance tests and check for failures
./bin/TestRunner --suite=Performance --format=xml > perf-results.xml

# Check exit code
if [ $? -ne 0 ]; then
  echo "Performance regression detected!"
  exit 1
fi
```

## Notes

- Performance tests use mock objects where possible to isolate component performance
- Network tests may be skipped if no connectivity is available
- Memory tests use heaptrc for accurate memory tracking
- Results may vary between runs; consider averaging multiple runs for benchmarks
