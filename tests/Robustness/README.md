# Robustness Tests

This directory contains stress tests and edge case handling tests for 3nity Media. These tests verify the application's stability under adverse conditions.

## Test Files

| File | Area | Description |
|------|------|-------------|
| `uTestCorruptedFiles.pas` | File Handling | Tests for handling corrupted/invalid media |
| `uTestNetworkTimeout.pas` | Network | Tests for network error recovery |
| `uTestRapidCommands.pas` | Stability | Tests for rapid command sequences |
| `uTestLongPlayback.pas` | Endurance | Tests for extended playback sessions |

## Test Count

- **Total Robustness Tests: 296+**

## Running Robustness Tests Only

```bash
./bin/TestRunner --suite=Robustness
```

## Test Coverage

### Corrupted Files (`uTestCorruptedFiles.pas`)

Tests handling of invalid or corrupted media files:

| Test Category | Description |
|---------------|-------------|
| Empty Files | Zero-byte files |
| Truncated Files | Incomplete media files |
| Invalid Headers | Wrong file format headers |
| Corrupt Containers | Damaged container format |
| Missing Codecs | Unsupported codec data |
| Partial Metadata | Incomplete or corrupt metadata |

Expected behavior:
- Graceful error reporting
- No crashes or hangs
- Clean resource cleanup
- Ability to continue with other files

### Network Timeout (`uTestNetworkTimeout.pas`)

Tests network error handling and recovery:

| Test Category | Description |
|---------------|-------------|
| Connection Timeout | Server doesn't respond |
| Read Timeout | Stream stalls mid-playback |
| DNS Failure | Invalid hostname |
| Connection Reset | Server drops connection |
| Slow Response | Very slow server response |
| Retry Mechanism | Automatic reconnection attempts |

Expected behavior:
- Appropriate timeout handling
- User notification of errors
- Retry attempts where appropriate
- No resource leaks

### Rapid Commands (`uTestRapidCommands.pas`)

Tests stability under rapid user input:

| Test Category | Description |
|---------------|-------------|
| Rapid Play/Pause | 100+ toggles in sequence |
| Rapid Seek | Many seeks in quick succession |
| Rapid Track Change | Fast track switching |
| Rapid Volume | Quick volume adjustments |
| Rapid Mode Change | Fast mode switching |
| Concurrent Operations | Overlapping commands |

Expected behavior:
- No crashes or deadlocks
- Consistent final state
- All commands processed or properly queued
- No memory leaks

### Long Playback (`uTestLongPlayback.pas`)

Tests extended playback sessions:

| Test Category | Description |
|---------------|-------------|
| Continuous Playback | Hours of uninterrupted play |
| Playlist Loop | Repeated playlist cycling |
| Memory Stability | No memory growth over time |
| Handle Stability | No handle/resource leaks |
| Position Accuracy | Time tracking accuracy |
| Event Consistency | Consistent event delivery |

Expected behavior:
- Stable memory usage
- No resource accumulation
- Accurate time tracking
- Consistent behavior over time

## Example Tests

### Corrupted File Handling

```pascal
procedure TTestCorruptedFiles.Test_TruncatedAudio_HandlesGracefully;
begin
  // Create truncated file
  CreateTruncatedFile('/tmp/truncated.mp3', 1024);

  FPlayer.LoadFile('/tmp/truncated.mp3');

  // Should not crash, should report error
  AssertEquals('Status should be error', Ord(msError), Ord(FPlayer.Status));
  AssertTrue('Should have error message', FPlayer.LastError <> '');
end;
```

### Rapid Command Stability

```pascal
procedure TTestRapidCommands.Test_RapidPlayPause_100Times;
var
  I: Integer;
begin
  FPlayer.LoadFile('/music/test.mp3');
  FPlayer.Play;

  for I := 1 to 100 do
  begin
    FPlayer.Pause;
    FPlayer.Resume;
  end;

  // Should still be in valid state
  AssertTrue('Should be playing or paused',
    (FPlayer.Status = msPlaying) or (FPlayer.Status = msPaused));
end;
```

### Memory Stability

```pascal
procedure TTestLongPlayback.Test_PlaylistLoop_NoMemoryGrowth;
var
  InitialMem, FinalMem: PtrUInt;
  I: Integer;
begin
  FPlaylist.PopulateWithTestData(10);
  InitialMem := GetHeapStatus.TotalAllocated;

  // Loop through playlist 100 times
  for I := 1 to 1000 do
  begin
    FPlaylist.PlayIndex(I mod 10);
    FPlayer.SimulateFileLoad(FPlaylist[I mod 10].FileName, 180.0);
  end;

  FinalMem := GetHeapStatus.TotalAllocated;

  // Allow 5% variance for normal fluctuation
  AssertTrue('Memory should be stable',
    FinalMem < InitialMem * 1.05);
end;
```

## Stress Test Configuration

Some tests have configurable parameters:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `RAPID_ITERATIONS` | 100 | Number of rapid command iterations |
| `LONG_PLAYBACK_CYCLES` | 1000 | Number of playback cycles |
| `NETWORK_TIMEOUT_MS` | 5000 | Network timeout threshold |
| `MEMORY_VARIANCE_PCT` | 5 | Allowed memory variance percentage |

## Running Extended Tests

For more thorough stress testing:

```bash
# Run with extended iterations
RAPID_ITERATIONS=1000 ./bin/TestRunner --suite=TTestRapidCommands

# Run long playback tests
LONG_PLAYBACK_CYCLES=10000 ./bin/TestRunner --suite=TTestLongPlayback
```

## Interpreting Results

### Success Criteria

- **No crashes**: All tests complete without segfaults or exceptions
- **No hangs**: All tests complete within timeout period
- **No leaks**: Memory usage remains stable
- **Correct state**: Final state matches expectations

### Common Failure Patterns

| Pattern | Possible Cause |
|---------|----------------|
| Crash on rapid commands | Race condition or missing locks |
| Memory growth | Resource leak |
| Hang on network test | Missing timeout handling |
| Inconsistent state | State machine bug |

## Best Practices

1. **Run after major changes** - Robustness tests catch subtle regressions
2. **Run on different platforms** - Behavior may vary by OS
3. **Monitor system resources** - Watch CPU/memory during tests
4. **Check logs** - Review application logs for warnings
5. **Repeat flaky tests** - Some issues only appear intermittently
