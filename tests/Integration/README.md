# Integration Tests

This directory contains integration tests that verify the correct interaction between multiple components of 3nity Media.

## Test Files

| File | Components | Description |
|------|------------|-------------|
| `uTestPlayback.pas` | MPV + Playlist | Basic playback workflow tests |
| `uTestAudioVideo.pas` | MPV + Tracks | Audio/video track selection and switching |
| `uTestStreaming.pas` | MPV + Network | HTTP/HTTPS stream handling |
| `uTestVisualization.pas` | MPV + Effects | Visualization integration |
| `uTestMPVPlaylistIntegration.pas` | Mock MPV + Mock Playlist | Component interaction scenarios |
| `uTestErrorScenarios.pas` | All Components | Error handling and recovery |

## Test Count

- **Total Integration Tests: 198+**

## Running Integration Tests Only

```bash
./bin/TestRunner --suite=Integration
```

## Test Coverage

### Playback Integration (`uTestPlayback.pas`)

Tests end-to-end playback workflows:
- Load and play media file
- Pause and resume
- Stop and restart
- Seek operations
- Track completion handling

### Audio/Video Handling (`uTestAudioVideo.pas`)

Tests media track management:
- Audio track enumeration
- Video track switching
- Subtitle track loading
- External subtitle files
- Track metadata display

### Streaming (`uTestStreaming.pas`)

Tests network stream handling:
- HTTP stream loading
- HTTPS stream support
- Stream metadata extraction
- Buffering behavior
- Connection error handling

### Visualization (`uTestVisualization.pas`)

Tests visual effect integration:
- Spectrum analyzer display
- Waveform visualization
- Audio-only media visualization
- Filter chain application

### MPV + Playlist Integration (`uTestMPVPlaylistIntegration.pas`)

Tests component interaction using mocks:

#### Auto-Play Next
- End of file triggers next track
- Stops at playlist end (normal mode)
- Wraps to first track (repeat all mode)
- Repeats current track (repeat one mode)
- Follows shuffle order (shuffle mode)

#### Playlist Navigation
- Play next/previous track
- Play first/last track
- Play specific index
- Double-click behavior

#### Settings Persistence
- Volume persists across tracks
- Mute state persists
- Playback speed persists
- Equalizer settings persist
- Video settings persist

#### Playback Scenarios
- Start from empty state
- Stop mid-playlist
- Clear playlist during playback
- Add track during playback
- Remove current track
- Reorder during playback

#### Position Tracking
- Position updates during playback
- Seek updates position
- Track change resets position
- Duration matches current track

#### Event Sequence
- Correct event ordering on load
- Correct event ordering on navigation
- Correct event ordering on stop

### Error Scenarios (`uTestErrorScenarios.pas`)

Tests error handling and recovery:

#### File Load Errors
- File not found handling
- Corrupt file handling
- Unsupported format handling
- Empty path handling
- Error status transitions

#### Network Errors
- Connection lost recovery
- Timeout handling
- Server not found
- Buffer underrun
- Retry mechanism

#### Invalid Operations
- Seek when stopped
- Pause when stopped
- Invalid volume/speed values
- Invalid playlist index
- Operations on empty playlist

#### Error Recovery
- Skip to next on error
- Retry before skipping
- Stop after repeated errors
- Recover after successful play

#### Edge Cases
- Empty playlist operations
- Single item navigation
- Rapid error sequences
- Error during shutdown
- Operations during error state

## Example Test

```pascal
procedure TTestAutoPlayNext.Test_EndFile_TriggersPlayNext_NormalMode;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');

  FPlaylist.PlayFirst;
  AssertEquals('Should be on track 1', 0, FPlaylist.CurrentIndex);

  FMPV.SimulateEndFile(0);  // Normal end

  AssertEquals('Should advance to track 2', 1, FPlaylist.CurrentIndex);
end;
```

## Mock vs Real Integration

Some integration tests use mock objects (`TMockMPVEngine`, `TMockPlaylistManager`) to test component interaction without requiring external dependencies. Tests that require the real MPV engine will skip gracefully if libmpv is not available.
