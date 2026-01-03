# 3nity Media - Test Documentation

## Overview

This document describes the testing framework, test structure, and procedures for 3nity Media.

## Current Status

> **Status:** The test framework is **fully implemented** with **1,686 tests** across all categories. All tests pass successfully.

### Implementation Summary

#### Unit Tests (940+ tests)

| Test Unit | Tests | Status |
|-----------|-------|--------|
| uTestCLIParams | 11 | ✅ Implemented |
| uTestConfig | 41 | ✅ Implemented |
| uTestPlaylistManager | 97 | ✅ Implemented |
| uTestRadioManager | 67 | ✅ Implemented |
| uTestStreamRecorder | 87 | ✅ Implemented |
| uTestVisualEffects | 166 | ✅ Implemented |
| uTestShortcuts | 151 | ✅ Implemented |
| uTestLocale | 50 | ✅ Implemented |
| uTestMPVConst | 156 | ✅ Implemented |
| uTestMPVEngine | 114 | ✅ Implemented |
| uTestMockMPVBehavior | ~50 | ✅ Implemented |
| uTestMockPlaylistBehavior | ~50 | ✅ Implemented |
| **Subtotal** | **940+** | **All passing** |

#### Integration Tests (198+ tests)

| Test Unit | Tests | Status |
|-----------|-------|--------|
| uTestPlayback | ~80 | ✅ Implemented |
| uTestAudioVideo | ~60 | ✅ Implemented |
| uTestStreaming | ~30 | ✅ Implemented |
| uTestVisualization | ~28 | ✅ Implemented |
| uTestMPVPlaylistIntegration | ~50 | ✅ Implemented |
| uTestErrorScenarios | ~50 | ✅ Implemented |
| **Subtotal** | **198+** | **All passing** |

#### Performance Tests (252+ tests)

| Test Unit | Tests | Status |
|-----------|-------|--------|
| uTestStartup | ~60 | ✅ Implemented |
| uTestMediaLoading | ~70 | ✅ Implemented |
| uTestSeekPerformance | ~60 | ✅ Implemented |
| uTestMemoryUsage | ~62 | ✅ Implemented |
| **Subtotal** | **252+** | **All passing** |

#### Robustness Tests (296+ tests)

| Test Unit | Tests | Status |
|-----------|-------|--------|
| uTestCorruptedFiles | ~80 | ✅ Implemented |
| uTestNetworkTimeout | ~70 | ✅ Implemented |
| uTestRapidCommands | ~80 | ✅ Implemented |
| uTestLongPlayback | ~66 | ✅ Implemented |
| **Subtotal** | **296+** | **All passing** |

| **TOTAL** | **1,686+** | **100% passing** |

**Existing structure:**
- `tests/Unit/` - 12 unit test files implemented
- `tests/Integration/` - 6 integration test files implemented
- `tests/Mocks/` - 2 mock implementations (TMockMPVEngine, TMockPlaylistManager)
- `tests/Performance/` - 4 performance test files implemented
- `tests/Robustness/` - 4 robustness test files implemented
- `tests/Functional/` - Planned for GUI tests
- `tests/TestData/` - Subdirectories created (audio, video, subtitles, playlists)

## Test Framework

3nity Media uses **FPCUnit**, the unit testing framework integrated with Lazarus/Free Pascal.

```pascal
uses
  fpcunit, testregistry, testutils;

type
  TTestMPVEngine = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Create;
    procedure Test_Initialize;
    procedure Test_PlayLocalFile;
  end;
```

---

## Project Structure

```
3nity-media/
├── tests/
│   ├── TestRunner.lpr ✓            # Main test program
│   ├── TestRunner.lpi ✓            # Test project configuration
│   │
│   ├── Unit/ ✓                     # Unit tests (12 files implemented)
│   │   ├── uTestCLIParams.pas ✓    # CLI parameters tests (11 tests)
│   │   ├── uTestConfig.pas ✓       # INI configuration tests (41 tests)
│   │   ├── uTestPlaylistManager.pas ✓ # Playlist manager tests (97 tests)
│   │   ├── uTestRadioManager.pas ✓ # Icecast radio tests (67 tests)
│   │   ├── uTestStreamRecorder.pas ✓ # Stream recording tests (87 tests)
│   │   ├── uTestVisualEffects.pas ✓ # Audio visualization tests (166 tests)
│   │   ├── uTestShortcuts.pas ✓    # Shortcut manager tests (151 tests)
│   │   ├── uTestLocale.pas ✓       # Localization tests (50 tests)
│   │   ├── uTestMPVConst.pas ✓     # MPV constants tests (156 tests)
│   │   ├── uTestMPVEngine.pas ✓    # MPV engine tests (114 tests)
│   │   ├── uTestMockMPVBehavior.pas ✓ # Mock MPV behavior tests (~50 tests)
│   │   └── uTestMockPlaylistBehavior.pas ✓ # Mock playlist behavior tests (~50 tests)
│   │
│   ├── Integration/ ✓              # Integration tests (6 files implemented)
│   │   ├── uTestPlayback.pas ✓     # Full playback tests (~80 tests)
│   │   ├── uTestAudioVideo.pas ✓   # Audio/video track tests (~60 tests)
│   │   ├── uTestStreaming.pas ✓    # Radio/HTTP stream tests (~30 tests)
│   │   ├── uTestVisualization.pas ✓ # Visualization + playback tests (~28 tests)
│   │   ├── uTestMPVPlaylistIntegration.pas ✓ # MPV + Playlist integration (~50 tests)
│   │   └── uTestErrorScenarios.pas ✓ # Error handling tests (~50 tests)
│   │
│   ├── Performance/ ✓              # Performance tests (4 files implemented)
│   │   ├── uTestStartup.pas ✓      # Startup time benchmarks (~60 tests)
│   │   ├── uTestMediaLoading.pas ✓ # Media loading performance (~70 tests)
│   │   ├── uTestSeekPerformance.pas ✓ # Seek operation benchmarks (~60 tests)
│   │   └── uTestMemoryUsage.pas ✓  # Memory allocation tests (~62 tests)
│   │
│   ├── Robustness/ ✓               # Robustness tests (4 files implemented)
│   │   ├── uTestCorruptedFiles.pas ✓ # Corrupted file handling (~80 tests)
│   │   ├── uTestNetworkTimeout.pas ✓ # Network error recovery (~70 tests)
│   │   ├── uTestRapidCommands.pas ✓ # Rapid command stability (~80 tests)
│   │   └── uTestLongPlayback.pas ✓ # Extended playback tests (~66 tests)
│   │
│   ├── Functional/ ✓               # Functional tests (planned)
│   │   ├── uTestMainForm.pas       # Main window tests
│   │   ├── uTestPlaylistForm.pas   # Playlist window tests
│   │   ├── uTestEqualizerForm.pas  # Equalizer window tests
│   │   ├── uTestOptionsForm.pas    # Options dialog tests
│   │   ├── uTestRadiosForm.pas     # Radio browser tests
│   │   ├── uTestBookmarksForm.pas  # Bookmarks dialog tests
│   │   ├── uTestFavoritesForm.pas  # Favorites dialog tests
│   │   └── uTestMediaInfoForm.pas  # Media info dialog tests
│   │
│   ├── Mocks/ ✓                    # Mock implementations (fully implemented)
│   │   ├── uMockMPVEngine.pas ✓    # MPV engine mock (TMockMPVEngine)
│   │   └── uMockPlaylist.pas ✓     # Playlist manager mock (TMockPlaylistManager)
│   │
│   └── TestData/ ✓                 # Test files
│       ├── audio/ ✓                # Audio test files
│       ├── video/ ✓                # Video test files
│       ├── subtitles/ ✓            # Subtitle test files
│       ├── playlists/ ✓            # Playlist test files
│       └── streams.txt             # Test stream URLs
```

---

## Unit Tests

### uTestLibMPV.pas - Library Loading

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_LoadLibrary` | Load libmpv (mpv-2.dll / libmpv.so.2) | No exception, handle ≠ nil |
| `Test_CreateContext` | `mpv_create()` | Returns valid handle |
| `Test_Initialize` | `mpv_initialize()` | Returns `MPV_ERROR_SUCCESS` (0) |
| `Test_SetOptionString` | `mpv_set_option_string()` | Returns 0 for valid options |
| `Test_Terminate` | `mpv_terminate_destroy()` | No crash, memory freed |
| `Test_ErrorString` | `mpv_error_string()` | Returns non-empty string |

### uTestMPVEngine.pas - Engine Wrapper

#### Initialization

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_Create` | Create TMPVEngine instance | No exception |
| `Test_Initialize` | Initialize(WindowHandle) | Returns True, Status = msNone |
| `Test_InitializeTwice` | Call Initialize twice | No crash, ignores 2nd call |
| `Test_Shutdown` | Shutdown after Initialize | Status = msNone, Handle = nil |
| `Test_IsRunning` | Check IsRunning | False before Play, True after |

#### Playback

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_PlayLocalFile` | PlayMedia(local file) | Status → msPlaying, Duration > 0 |
| `Test_PlayInvalidFile` | PlayMedia(non-existent file) | Status → msError |
| `Test_PlayURL` | PlayMedia(http URL) | Status → msPlaying |
| `Test_Stop` | Stop() during playback | Status → msStopped |
| `Test_Pause` | Pause() during playback | Status → msPaused |
| `Test_Resume` | Resume() after Pause | Status → msPlaying |
| `Test_TogglePause` | TogglePause() twice | msPlaying → msPaused → msPlaying |

#### Seeking

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_SeekAbsolute` | SeekAbsolute(10.0) | Position ≈ 10.0 (±0.5s) |
| `Test_SeekRelative` | SeekRelative(5.0) | Position increases by ~5s |
| `Test_SeekPercent` | SeekPercent(50.0) | Position ≈ Duration/2 |
| `Test_SeekBeyondEnd` | SeekAbsolute(Duration + 10) | No crash, position ≤ Duration |
| `Test_SeekNegative` | SeekAbsolute(-5) | Position = 0 or handled error |

#### Volume

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_SetVolume` | Volume := 50 | GetPropertyInt('volume') = 50 |
| `Test_VolumeRange` | Volume := 0, 100, 150 | Values accepted (0-150) |
| `Test_VolumeClamp` | Volume := 200 | Value clamped to 150 |
| `Test_Mute` | Muted := True | GetPropertyBool('mute') = True |
| `Test_Unmute` | Muted := False after True | Audible volume restored |

#### Equalizer

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_SetEqualizerBand` | SetEqualizerBand(0, 6.0) | Band 31Hz = +6dB |
| `Test_EqualizerRange` | Values -12 to +12 dB | No error |
| `Test_EqualizerPreset` | SetEqualizerPreset("0:0:0:0:0:0:0:0:0:0") | All bands at 0 |
| `Test_ResetEqualizer` | ResetEqualizer() | All bands at 0 |
| `Test_EnableEqualizer` | EnableEqualizer(True/False) | Filter enabled/disabled |

#### Video Properties

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_Brightness` | Brightness := 10 | Value applied |
| `Test_Contrast` | Contrast := -20 | Value applied |
| `Test_Saturation` | Saturation := 30 | Value applied |
| `Test_Hue` | Hue := -50 | Value applied |
| `Test_Gamma` | Gamma := 15 | Value applied |
| `Test_PropertyRange` | Values -100 to +100 | No error |
| `Test_PropertyClamp` | Brightness := 150 | Clamped to 100 |

#### Subtitles

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_LoadSubtitleSRT` | LoadSubtitle("test.srt") | Subtitle added to list |
| `Test_LoadSubtitleASS` | LoadSubtitle("test.ass") | Subtitle added |
| `Test_SetSubtitleTrack` | SetSubtitleTrack(1) | Track selected |
| `Test_SubScale` | SubScale := 1.5 | Size increased |
| `Test_SubDelay` | SubDelay := 0.5 | Delay applied |
| `Test_SubVisible` | SubVisible := False | Subtitles hidden |

#### Audio Tracks

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_AudioTrackCount` | Multi-audio file | AudioTracks.Count > 1 |
| `Test_SetAudioTrack` | SetAudioTrack(2) | Track 2 active |
| `Test_AudioTrackInfo` | Read track info | Language, Codec not empty |

#### Events

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_OnStatusChange` | Listen to status changes | Callback called |
| `Test_OnPositionChange` | Listen to position | Callback called periodically |
| `Test_OnFileLoaded` | Listen to file loading | Callback after PlayMedia |
| `Test_OnEndFile` | Listen to end of file | Callback at the end |
| `Test_OnMetadata` | Listen to metadata | Callback with title/artist |
| `Test_OnVideoResize` | Listen to resize | Callback with dimensions |

### uTestVisualEffects.pas - Visualizations

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_Create` | Create TVisualEffects instance | No exception |
| `Test_BuildSpectrumFilter` | Generate spectrum filter | Valid FFmpeg string containing "showspectrum" |
| `Test_BuildWavesFilter` | Generate waves filter | Valid FFmpeg string containing "showwaves" |
| `Test_BuildVectorFilter` | Generate vector filter | Valid FFmpeg string containing "avectorscope" |
| `Test_BuildHistogramFilter` | Generate histogram filter | Valid FFmpeg string containing "ahistogram" |
| `Test_BuildVolumeFilter` | Generate volume filter | Valid FFmpeg string containing "showvolume" |
| `Test_BuildCombinedFilter` | Generate combined filter | Valid FFmpeg string with multiple filters |
| `Test_GetAudioOnlyFilter` | Generate audio-only filter | Valid FFmpeg lavfi string |
| `Test_ColorSchemeDefault` | Default color scheme | Correct colors |
| `Test_ColorSchemeFire` | Fire color scheme | Red/orange colors |
| `Test_ColorSchemeIce` | Ice color scheme | Blue/cyan colors |
| `Test_ColorSchemeRainbow` | Rainbow color scheme | Rainbow mode enabled |
| `Test_SetDimensions` | Set width/height | Dimensions in filter |
| `Test_EnableDisable` | Enable/disable | Enabled state changes |
| `Test_ModeChange` | Change mode | Mode applied |
| `Test_ApplyPreset` | Apply visualization preset | Preset settings applied |
| `Test_RepeatLastZero` | Verify repeatlast=0 | Present in overlay filter |

### uTestPlaylist.pas - Playlist Manager

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_AddFile` | Add file | Count = 1 |
| `Test_AddMultiple` | Add 10 files | Count = 10 |
| `Test_RemoveFile` | Remove file | Count decreases |
| `Test_Clear` | Clear playlist | Count = 0 |
| `Test_MoveUp` | Move item up | Index changes |
| `Test_MoveDown` | Move item down | Index changes |
| `Test_Shuffle` | Enable shuffle | Random order |
| `Test_Loop` | Loop mode | Returns to beginning |
| `Test_LoadM3U` | Load M3U file | Entries loaded |
| `Test_LoadM3U8` | Load M3U8 file | UTF-8 entries loaded |
| `Test_LoadPLS` | Load PLS file | Entries loaded |
| `Test_SaveM3U` | Save M3U | Valid file created |
| `Test_GetNext` | Get next | Index +1 |
| `Test_GetPrevious` | Get previous | Index -1 |
| `Test_GetNextShuffle` | Next in shuffle | Random index |
| `Test_GetNextLoop` | Next in loop | Returns to 0 after last |

### uTestConfig.pas - Configuration

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_LoadDefaults` | Load default config | Values initialized |
| `Test_LoadFromINI` | Load from INI | Values restored |
| `Test_SaveToINI` | Save to INI | File created |
| `Test_GetString` | Read string | Correct value |
| `Test_GetInteger` | Read integer | Correct value |
| `Test_GetBoolean` | Read boolean | Correct value |
| `Test_SetValue` | Set value | Value stored |
| `Test_SectionExists` | Check section | True/False correct |

### uTestShortcuts.pas - Keyboard Shortcuts

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_LoadDefaults` | Load default shortcuts | Shortcuts initialized |
| `Test_LoadFromINI` | Load from INI file | Custom shortcuts loaded |
| `Test_SaveToINI` | Save to INI | File created/modified |
| `Test_MatchShortcut` | Recognize shortcut | Correct action returned |
| `Test_GetShortcutText` | Get shortcut text | "Ctrl+P" for Play |
| `Test_DuplicateDetection` | Detect duplicates | Error if same shortcut |
| `Test_ResetToDefaults` | Reset to defaults | Default values restored |

### uTestRadioManager.pas - Radio/Streaming

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_LoadIcecastXML` | Parse Icecast XML | Stations loaded |
| `Test_FilterByGenre` | Filter by genre | Filtered results |
| `Test_SearchByName` | Search by name | Results found |
| `Test_GetStationInfo` | Get station info | URL, bitrate, genre |
| `Test_PlayStation` | Play radio stream | Playback starts |
| `Test_ParseICYMetadata` | Parse ICY metadata | Title extracted |

### uTestStreamRecorder.pas - Stream Recording

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_Create` | Create TStreamRecorder instance | No exception |
| `Test_Initialize` | Initialize recorder | Ready state |
| `Test_StartRecording` | Start recording stream | Recording state, file created |
| `Test_StopRecording` | Stop recording | File saved, state reset |
| `Test_PauseResume` | Pause/resume recording | States toggle correctly |
| `Test_GenerateFileName` | Generate output filename | Valid path with timestamp |
| `Test_SanitizeFileName` | Sanitize invalid chars | No invalid characters |
| `Test_ShouldSplitFile` | Check split conditions | Correct threshold detection |
| `Test_SplitRecording` | Split into new file | New file created, index incremented |
| `Test_NotifyTrackChange` | Handle ICY track change | Metadata updated |
| `Test_AudioFormat` | Test audio formats (MP3, AAC, OGG) | Correct extensions |
| `Test_VideoFormat` | Test video formats (MP4, MKV, WebM) | Correct extensions |

### uTestCLIParams.pas - Command Line Parameters

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_HasParam` | Detect parameter presence | True for existing params |
| `Test_GetParamValue` | Get parameter value | Correct value returned |
| `Test_ParseTimeToSeconds` | Parse time strings | "1:30" → 90.0, "1:00:00" → 3600.0 |
| `Test_ParseStartupOptions` | Parse all startup options | All options extracted |
| `Test_GetCLIAction` | Detect CLI action | Correct action (help/version/license) |
| `Test_ExecuteCLIAction` | Execute CLI action | Output generated |
| `Test_InvalidParam` | Handle invalid parameters | Graceful handling |
| `Test_MissingValue` | Handle missing values | Default or error |

### uTestLocale.pas - Localization

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_Create` | Create TLocaleManager instance | No exception |
| `Test_LoadLanguage` | Load language file | Strings loaded |
| `Test_GetString` | Get localized string | Correct translation |
| `Test_DefaultFallback` | Fallback to default | Default returned if key missing |
| `Test_ScanAvailableLanguages` | Scan .lang files | Languages detected |
| `Test_DetectOSLanguage` | Detect OS language | Valid language code |
| `Test_Menu` | Get menu string | Correct menu translation |
| `Test_Dialog` | Get dialog string | Correct dialog translation |
| `Test_T_Function` | Test _T() shortcut | Correct translation |

---

## Integration Tests

> **Status:** All 6 integration test files are **fully implemented** with **198+ tests** passing.

### uTestPlayback.pas - Playback Integration (~80 tests)

Tests MPVEngine + PlaylistManager + ConfigManager integration:

| Test Class | Description | Tests |
|------------|-------------|-------|
| TTestMPVPlaylistIntegration | MPV + Playlist navigation | ~25 |
| TTestMPVConfigIntegration | MPV + Config persistence | ~20 |
| TTestPlaylistConfigIntegration | Playlist + Config + History | ~25 |
| TTestPlaybackModeIntegration | Playback modes (Normal, Repeat, Shuffle) | ~10 |

Key test scenarios:
- Playlist navigation with MPV (Next/Previous/First/Last)
- Volume and mute persistence across sessions
- Playback position save/restore
- History tracking and limits (MAX_HISTORY_ITEMS = 500)
- Recent files integration
- Shuffle order generation and persistence

### uTestAudioVideo.pas - Audio/Video Track Integration (~60 tests)

Tests TMPVEngine + TMPVTrackList + track management:

| Test Class | Description | Tests |
|------------|-------------|-------|
| TTestTrackListIntegration | Track list operations | ~20 |
| TTestAudioTrackIntegration | Audio track selection/info | ~15 |
| TTestVideoPropertiesIntegration | Video properties (brightness, contrast, etc.) | ~15 |
| TTestEqualizerIntegration | Equalizer band management | ~10 |

Key test scenarios:
- Track list Add/Remove/Clear operations
- FindByID with index return value
- Audio/Video/Subtitle track type handling
- Video property ranges (-100 to +100)
- Equalizer 10-band configuration
- External track detection (IsExternal property)

### uTestStreaming.pas - Streaming Integration (~30 tests)

Tests TRadioManager + TStreamRecorder + TConfigManager integration:

| Test Class | Description | Tests |
|------------|-------------|-------|
| TTestRadioConfigIntegration | Radio + Config persistence | ~10 |
| TTestRecorderConfigIntegration | Recorder settings persistence | ~10 |
| TTestStreamMetadata | ICY metadata handling | ~10 |

Key test scenarios:
- Custom station management (AddCustomStation, DeleteCustomStation)
- Favorites persistence
- Recording settings (SplitSizeMB, SplitTimeMinutes)
- Track change notifications (NotifyTrackChange with Artist/Title)
- Recording state machine

### uTestVisualization.pas - Visualization Integration (~28 tests)

Tests TVisualEffects + TConfigManager integration:

| Test Class | Description | Tests |
|------------|-------------|-------|
| TTestVisualizationConfigIntegration | Mode and color scheme integration | ~12 |
| TTestFilterGeneration | FFmpeg filter string generation | ~8 |
| TTestVisualSettings | Visual settings record access | ~8 |
| TTestModeCycling | Mode/scheme cycling | ~4 |
| TTestPresets | Visualization presets | ~3 |
| TTestVisualizationHelpers | Mode/scheme string conversion | ~8 |

Key test scenarios:
- Visual mode switching (vmNone, vmSpectrum, vmWaves, vmVector, vmVolume, vmCombined)
- Color scheme application (vcsDefault, vcsFire, vcsIce, vcsRainbow)
- Filter string generation (requires Enabled = True)
- Settings record modification (Width, Height, BarCount)
- NextMode/PreviousMode/NextColorScheme cycling
- Preset application

---

## Mock Objects

> **Status:** Mock implementations are **fully implemented** and used extensively by unit and integration tests.

### TMockMPVEngine (`Mocks/uMockMPVEngine.pas`)

A complete mock implementation of the MPV engine that simulates playback behavior without requiring libmpv.

**Features:**
- Playback simulation (load, play, pause, stop, seek)
- Property management (volume, speed, video settings, equalizer)
- Track management (audio, video, subtitle tracks)
- Event system (status change, position change, file loaded, end file, errors)
- Call logging for verification

**Key Methods:**
```pascal
procedure SimulateFileLoad(const FileName: string; Duration: Double; HasVideo: Boolean = True);
procedure SimulatePosition(Seconds: Double);
procedure SimulateEndFile(Reason: Integer = 0);
procedure SimulateError(const ErrorMsg: string);
procedure SimulateMetadata(const Title, Artist: string);
```

### TMockPlaylistManager (`Mocks/uMockPlaylist.pas`)

A complete mock implementation of the playlist manager that works entirely in-memory.

**Features:**
- Item management (add, remove, move, clear)
- Navigation (next, previous, first, last, play by index)
- Playback modes (normal, repeat all, repeat one, shuffle)
- Ordering (sort, reverse, randomize)
- Search (find by filename, title, or general search)
- Event system (change, play, current change events)
- Call logging for verification

**Key Methods:**
```pascal
function Add(const AFileName: string): Integer;
procedure Delete(Index: Integer);
procedure Move(FromIndex, ToIndex: Integer);
procedure PlayFirst; procedure PlayLast; procedure PlayNext; procedure PlayPrevious;
procedure Sort(Ascending: Boolean = True);
procedure Randomize;
function Search(const AQuery: string): TIntegerDynArray;
procedure PopulateWithTestData(ItemCount: Integer);
```

### Usage Example

```pascal
procedure TMyTest.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FMPV.OnStatusChange := @OnMPVStatusChange;
  FMPV.OnEndFile := @OnMPVEndFile;
end;

procedure TMyTest.Test_AutoPlayNext;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.PlayFirst;
  FMPV.SimulateFileLoad(FPlaylist.CurrentItem.FileName, 180.0);

  // Simulate end of file
  FMPV.SimulateEndFile(0);

  // Verify next track was triggered
  AssertEquals('Should advance to next', 1, FPlaylist.CurrentIndex);
end;
```

---

## Functional Tests (GUI)

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_OpenFileDialog` | Menu → Open | File loaded |
| `Test_PlayPauseButton` | Click Play/Pause button | State changes |
| `Test_VolumeSlider` | Move volume slider | Volume changes |
| `Test_SeekBar` | Click on progress bar | Position changes |
| `Test_FullscreenToggle` | F or F11 key | Fullscreen mode |
| `Test_EscapeFullscreen` | Escape key | Exits fullscreen |
| `Test_PlaylistDragDrop` | Drag and drop files | Files added |
| `Test_KeyboardShortcuts` | All keyboard shortcuts | Correct actions |
| `Test_ContextMenu` | Right click | Context menu displayed |
| `Test_EqualizerSliders` | Manipulate equalizer sliders | Values change |
| `Test_VisualizationMenu` | Visualization menu | Modes available |

---

## Performance Tests

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_StartupTime` | Application startup time | < 2 seconds |
| `Test_FileLoadTime` | Local file load time | < 1 second |
| `Test_SeekResponseTime` | Seek response time | < 500ms |
| `Test_MemoryUsage` | Memory after 1h playback | < 500 MB |
| `Test_CPUUsage` | CPU during video playback | < 30% (with HW decode) |
| `Test_VisualizationCPU` | CPU with visualization | < 50% |
| `Test_PlaylistLoadTime` | Load 1000 entry playlist | < 2 seconds |

---

## Robustness Tests

| Test | Description | Success Criteria |
|------|-------------|------------------|
| `Test_CorruptedFile` | Corrupted file | Error handled, no crash |
| `Test_NetworkTimeout` | Inaccessible URL | Timeout after 30s, error |
| `Test_RapidCommands` | Spam Play/Pause/Seek | No crash |
| `Test_LargePlaylist` | 10000 entry playlist | Responsive |
| `Test_LongPlayback` | 8h continuous playback | Stable |
| `Test_RapidVisualizationToggle` | Rapidly toggle visualization | No crash |
| `Test_FileNotFound` | File deleted during playback | Error handled |
| `Test_LibMPVMissing` | libmpv not installed | Clear error message |

---

## Running Tests

### Using Make

Tests are managed by a dedicated `Makefile` in the `tests/` directory, separate from the main build `Makefile`.

```bash
# From tests/ directory
cd tests/

# Build test runner
make build

# Run all tests
make test

# Quick tests (unit only)
make quick

# Tests by category
make unit               # Unit tests
make integration        # Integration tests
make functional         # GUI tests
make performance        # Performance tests
make robustness         # Robustness tests

# Specific tests
make test-mpv           # MPVEngine tests
make test-visual        # Visualization tests
make test-playlist      # Playlist tests
make test-config        # Configuration tests
make test-radio         # Radio/Streaming tests

# Verbose mode
make verbose

# Generate HTML report
make report

# Coverage report
make coverage

# Clean test artifacts
make clean
```

### Direct Execution

```bash
# Build test runner
cd tests/
lazbuild TestRunner.lpi

# Run all tests
./TestRunner --all --format=xml --output=results.xml

# Run a category
./TestRunner --suite=Unit
./TestRunner --suite=Integration
./TestRunner --suite=Functional

# Run specific test file
./TestRunner --suite=TTestMPVEngine
./TestRunner --suite=TTestVisualEffects

# Run specific test
./TestRunner --test=Test_PlayLocalFile

# Verbose mode
./TestRunner --all --verbose

# Generate HTML report
./TestRunner --all --format=html --output=results.html
```

---

## Test Data Generation

### Using Make

```bash
# From tests/ directory
cd tests/

# Generate test files
make testdata

# Check test files
make check-testdata
```

### Manual Generation

Run the `tests/generate_test_files.sh` script (to be created):

```bash
cd tests
chmod +x generate_test_files.sh
./generate_test_files.sh
```

### Required Test Files

| File | Description | Source |
|------|-------------|--------|
| `test_44100_stereo.mp3` | MP3 44.1kHz stereo, 30s | Generated (ffmpeg) |
| `test_48000_51.mp3` | MP3 48kHz 5.1, 30s | Generated |
| `test_vbr.mp3` | MP3 VBR, 60s | Generated |
| `test_720p.mp4` | H.264 720p, 30s | Generated |
| `test_1080p_hevc.mkv` | HEVC 1080p, 30s | Generated |
| `test_with_subs.mkv` | MKV + 2 subtitle tracks | Generated |
| `test.srt` | SRT subtitles | Created manually |
| `test.ass` | ASS subtitles | Created manually |
| `test.vtt` | WebVTT subtitles | Created manually |
| `test.m3u` | M3U playlist | Created manually |
| `test.m3u8` | M3U8 UTF-8 playlist | Created manually |
| `test.pls` | PLS playlist | Created manually |

---

## Code Coverage Targets

| Module | Target Coverage |
|--------|-----------------|
| uLibMPV.pas | 90% |
| uMPVEngine.pas | 85% |
| uPlaylistManager.pas | 90% |
| uRadioManager.pas | 80% |
| uVisualEffects.pas | 85% |
| uConfig.pas | 90% |
| uShortcutManager.pas | 85% |
| Forms/*.pas | 70% |
| **Global** | **80%** |

---

## CI/CD Integration

Tests are automatically run via GitHub Actions on:
- Push to `main` or `develop` branches
- Pull requests to `main`

See `.github/workflows/build.yml` for the complete CI/CD configuration.

---

## Notes

### Considerations

- GUI tests require a graphical environment (X11/Wayland)
- Integration tests with MPV require libmpv installed
- Use mocks for isolated unit tests
- Streaming tests require network connection

### Recommended Implementation Order

1. **Phase 1 - Critical tests**
   - uTestLibMPV.pas (library loading)
   - uTestMPVEngine.pas (basic functions)
   - uTestPlaylist.pas (playlist management)

2. **Phase 2 - Functional tests**
   - uTestConfig.pas
   - uTestShortcuts.pas
   - uTestVisualEffects.pas

3. **Phase 3 - Integration tests**
   - uTestPlayback.pas
   - uTestVisualization.pas

4. **Phase 4 - GUI tests**
   - uTestMainForm.pas
   - uTestPlaylistForm.pas
