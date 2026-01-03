# Unit Tests

This directory contains unit tests for individual components of 3nity Media. Each component is tested in isolation using mock objects where necessary.

## Test Files

| File | Component | Description |
|------|-----------|-------------|
| `uTestCLIParams.pas` | Command Line | Tests for CLI argument parsing |
| `uTestConfig.pas` | Configuration | Tests for settings management, bookmarks, favorites |
| `uTestPlaylistManager.pas` | Playlist | Tests for playlist operations (add, remove, sort, shuffle) |
| `uTestRadioManager.pas` | Radio | Tests for radio station management and Icecast parsing |
| `uTestStreamRecorder.pas` | Recording | Tests for stream recording functionality |
| `uTestVisualEffects.pas` | Visualization | Tests for FFmpeg filter generation |
| `uTestShortcuts.pas` | Shortcuts | Tests for keyboard shortcut parsing and handling |
| `uTestLocale.pas` | Localization | Tests for translation loading and string lookup |
| `uTestMPVConst.pas` | MPV Constants | Tests for MPV property and command constants |
| `uTestMPVEngine.pas` | MPV Engine | Tests for MPV engine wrapper (uses mocks) |
| `uTestMockMPVBehavior.pas` | Mock MPV | Tests verifying mock MPV engine behavior |
| `uTestMockPlaylistBehavior.pas` | Mock Playlist | Tests verifying mock playlist behavior |

## Test Count

- **Total Unit Tests: 940+**

## Running Unit Tests Only

```bash
./bin/TestRunner --suite=Unit
```

## Test Coverage by Component

### CLI Parameters (`uTestCLIParams.pas`)

Tests command-line argument parsing:
- File path extraction
- Flag parsing (`--play`, `--fullscreen`, etc.)
- URL handling
- Multiple file arguments

### Configuration (`uTestConfig.pas`)

Tests settings management:
- Default value handling
- INI file read/write
- Bookmarks (add, remove, navigate)
- Favorites (categorized storage)
- Playback position persistence
- CRC32 hashing for file identification

### Playlist Manager (`uTestPlaylistManager.pas`)

Tests playlist operations:
- Add/Remove items
- Move and reorder
- Sort (by title, artist, duration, path)
- Shuffle and randomize
- Search and filter
- M3U/PLS file parsing
- Duration formatting
- Duplicate detection

### Radio Manager (`uTestRadioManager.pas`)

Tests internet radio functionality:
- Station loading from Icecast directory
- XML parsing
- Genre/Country filtering
- Custom station management
- Favorites synchronization

### Stream Recorder (`uTestStreamRecorder.pas`)

Tests recording functionality:
- Filename generation
- Path sanitization
- Split file handling
- Metadata in filenames

### Visual Effects (`uTestVisualEffects.pas`)

Tests visualization:
- Spectrum analyzer filter generation
- Waveform filter generation
- Combined filter chains
- Audio-only visualization
- FFmpeg filter syntax

### Keyboard Shortcuts (`uTestShortcuts.pas`)

Tests hotkey handling:
- Key name parsing
- Modifier key combinations
- Action lookup
- Default shortcut assignments

### Localization (`uTestLocale.pas`)

Tests translation system:
- Language file loading
- String lookup with fallback
- Placeholder substitution
- Missing translation handling

### MPV Engine (`uTestMPVEngine.pas`, `uTestMockMPVBehavior.pas`)

Tests MPV wrapper:
- Playback control (play, pause, stop, seek)
- Volume and mute
- Speed control
- Video adjustments (brightness, contrast, etc.)
- Equalizer bands
- Track selection (audio, video, subtitles)
- Event handling
- Status transitions

### Mock Playlist Behavior (`uTestMockPlaylistBehavior.pas`)

Tests mock playlist implementation:
- Item management
- Navigation (next, previous, first, last)
- Playback modes (normal, repeat, shuffle)
- Search functionality
- Event triggering
- Call logging for verification

## Example Test

```pascal
procedure TTestPlaylistManager.Test_Add_SingleItem;
var
  Index: Integer;
begin
  Index := FPlaylist.Add('/music/song.mp3');

  AssertEquals('Should return index 0', 0, Index);
  AssertEquals('Count should be 1', 1, FPlaylist.Count);
  AssertEquals('FileName should match', '/music/song.mp3', FPlaylist[0].FileName);
end;
```

## Dependencies

Unit tests use mock objects from the `Mocks/` directory to avoid external dependencies like libmpv or file system access.
