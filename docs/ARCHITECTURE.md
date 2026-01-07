# Architecture Overview - 3nity Media

This document provides a technical overview of the 3nity Media architecture, including module organization, data flow, and libmpv integration.

## Table of Contents

- [High-Level Architecture](#high-level-architecture)
- [Module Diagram](#module-diagram)
- [Layer Description](#layer-description)
- [Core Modules](#core-modules)
- [Data Flow](#data-flow)
- [DVD and Blu-ray Support](#dvd-and-blu-ray-support)
- [MPV Integration](#mpv-integration)
- [Event System](#event-system)
- [Configuration System](#configuration-system)
- [Localization System](#localization-system)
- [Threading Model](#threading-model)

---

## High-Level Architecture

3nity Media follows a layered architecture pattern with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────────────┐
│                        PRESENTATION LAYER                           │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐        │
│  │MainForm │ │Playlist │ │ Radios  │ │Equalizer│ │ Options │  ...   │
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘        │
└───────┼───────────┼───────────┼───────────┼───────────┼─────────────┘
        │           │           │           │           │
┌───────┴───────────┴───────────┴───────────┴───────────┴─────────────┐
│                         BUSINESS LAYER                              │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐                 │
│  │  MPVEngine   │ │PlaylistMgr   │ │ RadioManager │                 │
│  │              │ │              │ │              │                 │
│  │ - Playback   │ │ - Items      │ │ - Stations   │                 │
│  │ - Tracks     │ │ - Navigation │ │ - Filtering  │                 │
│  │ - Equalizer  │ │ - Shuffle    │ │ - Favorites  │                 │
│  └──────┬───────┘ └──────────────┘ └──────────────┘                 │
│         │                                                           │
│  ┌──────┴───────┐ ┌──────────────┐ ┌──────────────┐                 │
│  │VisualEffects │ │StreamRecorder│ │   Shortcuts  │                 │
│  └──────────────┘ └──────────────┘ └──────────────┘                 │
└───────┬─────────────────────────────────────────────────────────────┘
        │
┌───────┴─────────────────────────────────────────────────────────────┐
│                          COMMON LAYER                               │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌──────────┐ ┌─────────┐       │
│  │ uTypes  │ │ uConfig │ │ uLocale │ │uConstants│ │FileUtils│       │
│  └─────────┘ └─────────┘ └─────────┘ └──────────┘ └─────────┘       │
└───────┬─────────────────────────────────────────────────────────────┘
        │
┌───────┴─────────────────────────────────────────────────────────────┐
│                        INTEGRATION LAYER                            │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                         uLibMPV                              │   │
│  │           Pascal bindings for libmpv C API                   │   │
│  └──────────────────────────┬───────────────────────────────────┘   │
└─────────────────────────────┼───────────────────────────────────────┘
                              │
┌─────────────────────────────┴───────────────────────────────────────┐
│                        EXTERNAL LIBRARIES                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                  │
│  │   libmpv    │  │   Qt5/LCL   │  │   ffprobe   │                  │
│  │  (playback) │  │    (GUI)    │  │ (metadata)  │                  │
│  └─────────────┘  └─────────────┘  └─────────────┘                  │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Module Diagram

### Source Directory Structure

```
src/
├── TrinityMedia.lpr            # Main program entry point
│
├── Core/                       # Core functionality
│   ├── uLibMPV.pas            # libmpv C API bindings
│   ├── uMPVConst.pas          # MPV constants and defaults
│   ├── uMPVEngine.pas         # High-level MPV wrapper (2800+ lines)
│   ├── uPlaylistManager.pas   # Playlist management
│   ├── uRadioManager.pas      # Internet radio management
│   ├── uStreamRecorder.pas    # Stream recording
│   ├── uVisualEffects.pas     # Audio visualizations
│   └── uAppVersion.pas        # Version information
│
├── Forms/                      # GUI forms and dialogs
│   ├── uMainForm.pas          # Main application window
│   ├── uPlaylist.pas          # Playlist window
│   ├── uRadios.pas            # Radio browser
│   ├── uEqualizer.pas         # 10-band equalizer
│   ├── uOptions.pas           # Settings dialog
│   ├── uVideoAdjust.pas       # Video adjustments
│   ├── uMediaInfo.pas         # Media information
│   ├── uHistory.pas           # Playback history
│   ├── uBookmarks.pas         # Bookmarks manager
│   ├── uFavorites.pas         # Favorites manager
│   ├── uGotoTime.pas          # Go to time dialog
│   ├── uSleepTimer.pas        # Sleep timer
│   ├── uShortcutsEditor.pas   # Keyboard shortcuts editor
│   ├── uOpenURL.pas           # URL input dialog
│   ├── uLog.pas               # Debug log viewer
│   └── uAbout.pas             # About dialog
│
├── Common/                     # Shared utilities
│   ├── uTypes.pas             # Type definitions and records
│   ├── uConstants.pas         # Application constants
│   ├── uConfig.pas            # Configuration management
│   ├── uShortcuts.pas         # Keyboard shortcuts system
│   ├── uFileUtils.pas         # File utilities
│   └── uCLIParams.pas         # Command-line parsing
│
├── Locale/                     # Internationalization
│   └── uLocale.pas            # Translation system
│
└── Controls/                   # Custom UI controls
    └── uVssScrollbar.pas      # Custom scrollbar
```

### Module Dependencies

```
                    ┌─────────────────┐
                    │TrinityMedia.lpr │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
              ▼              ▼              ▼
        ┌──────────┐  ┌──────────┐  ┌──────────┐
        │uMainForm │  │ uConfig  │  │ uLocale  │
        └────┬─────┘  └──────────┘  └──────────┘
             │
    ┌────────┼────────┬────────────┬───────────┐
    │        │        │            │           │
    ▼        ▼        ▼            ▼           ▼
┌───────┐┌────────┐┌───────┐  ┌─────────┐ ┌─────────┐
│MPV    ││Playlist││Radio  │  │Equalizer│ │Playlist │
│Engine ││Manager ││Manager│  │  Form   │ │  Form   │
└───┬───┘└────────┘└───────┘  └─────────┘ └─────────┘
    │
    ▼
┌───────────┐
│  uLibMPV  │──────► libmpv.so / mpv-2.dll
└───────────┘
```

---

## Layer Description

### Presentation Layer (`Forms/`)

Handles all user interface components using the Lazarus Component Library (LCL) with Qt5 widget set.

| Form | Purpose |
|------|---------|
| `TfrmMain` | Main window with video display, controls, menus |
| `TfrmPlaylist` | Playlist management with filtering and sorting |
| `TfrmRadios` | Internet radio browser with Icecast directory |
| `TfrmEqualizer` | 10-band audio equalizer with presets |
| `TfrmOptions` | Application settings |
| `TfrmVideoAdjust` | Brightness, contrast, saturation, hue, gamma |
| `TfrmHistory` | Playback history with resume support |
| `TfrmBookmarks` | Position bookmarks per file |
| `TfrmFavorites` | Favorite files and stations |

### Business Layer (`Core/`)

Contains the core application logic, independent of the UI.

| Module | Responsibility |
|--------|----------------|
| `TMPVEngine` | Complete libmpv wrapper with event system |
| `TPlaylistManager` | Playlist items, navigation, shuffle, sorting |
| `TRadioManager` | Icecast station loading, filtering, favorites |
| `TStreamRecorder` | Recording streams to disk |
| `TVisualEffects` | FFmpeg filter chains for visualizations |

### Common Layer (`Common/`)

Shared types, utilities, and services used across all layers.

| Module | Purpose |
|--------|---------|
| `uTypes` | Record definitions (`TPlaylistItem`, `TRadioStation`, etc.) |
| `uConfig` | INI-based configuration persistence |
| `uConstants` | Application-wide constants |
| `uShortcuts` | Keyboard shortcut management |
| `uCLIParams` | Command-line argument parsing |

### Integration Layer (`Core/uLibMPV.pas`)

Pascal bindings for the libmpv C API, providing:
- Type definitions matching `client.h`
- Function declarations for dynamic loading
- Cross-platform library loading (Windows, Linux, macOS)

---

## Core Modules

### TMPVEngine (`uMPVEngine.pas`)

The central component wrapping libmpv functionality.

**Responsibilities:**
- Initialize and manage mpv context
- Handle playback (play, pause, stop, seek)
- Manage audio/video/subtitle tracks
- Apply equalizer and video adjustments
- Process mpv events and dispatch to application
- Handle DVD/Bluray navigation
- Manage cache settings per source type

**Key Properties:**
```pascal
property Status: TMPVStatus;           // Current playback state
property Position: Double;             // Current position in seconds
property Duration: Double;             // Total duration in seconds
property Volume: Integer;              // Volume level (0-100)
property Muted: Boolean;               // Mute state
property Speed: Double;                // Playback speed
property StreamInfo: TMPVStreamInfo;   // Current media information
property AudioTracks: TMPVTrackList;   // Available audio tracks
property SubtitleTracks: TMPVTrackList;// Available subtitle tracks
```

**Key Methods:**
```pascal
procedure Play(const FileName: string);
procedure Pause;
procedure Stop;
procedure Seek(Seconds: Double; Mode: TSeekMode);
procedure SetAudioTrack(ID: Integer);
procedure SetSubtitleTrack(ID: Integer);
procedure SetEqualizerBand(Band: Integer; Value: Double);
procedure SetVideoProperty(Prop: string; Value: Integer);
```

### TPlaylistManager (`uPlaylistManager.pas`)

Manages the playlist with support for various formats and playback modes.

**Features:**
- Add/remove/reorder items
- Parse M3U, M3U8, PLS formats
- Shuffle with Fisher-Yates algorithm
- Repeat modes (one, all, shuffle)
- Search and filter
- Selection management

**Key Events:**
```pascal
property OnChange: TNotifyEvent;
property OnPlay: TPlaylistPlayEvent;
property OnCurrentChange: TPlaylistItemEvent;
```

### TRadioManager (`uRadioManager.pas`)

Handles internet radio station management.

**Features:**
- Parse Icecast XML directory
- Filter by genre, country, name
- Manage custom stations (INI format)
- Favorites persistence

---

## Data Flow

### Media Playback Flow

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    User     │────►│  MainForm   │────►│ MPVEngine   │
│ (Open File) │     │             │     │             │
└─────────────┘     └──────┬──────┘     └──────┬──────┘
                           │                   │
                           │                   ▼
                           │            ┌─────────────┐
                           │            │   libmpv    │
                           │            │             │
                           │            │ - Demuxing  │
                           │            │ - Decoding  │
                           │            │ - Rendering │
                           │            └──────┬──────┘
                           │                   │
                           │     Events        │
                           │◄──────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │  Update UI  │
                    │ - Position  │
                    │ - Duration  │
                    │ - Metadata  │
                    └─────────────┘
```

### Playlist Flow

```
┌──────────────┐
│ User Action  │
│ (Add Files)  │
└──────┬───────┘
       │
       ▼
┌──────────────┐     ┌───────────────┐
│ MainForm     │────►│PlaylistManager│
│ (Drop/Open)  │     │               │
└──────────────┘     └──────┬────────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
              ▼             ▼             ▼
       ┌──────────┐  ┌──────────┐  ┌──────────┐
       │ Add Item │  │ Extract  │  │ Update   │
       │ to Array │  │ Metadata │  │ Shuffle  │
       └──────────┘  │ (ffprobe)│  │ Order    │
                     └──────────┘  └──────────┘
                            │
                            ▼
                     ┌──────────────┐
                     │  OnChange    │
                     │  Event       │
                     └──────┬───────┘
                            │
                            ▼
                     ┌──────────────┐
                     │ Update UI    │
                     │ (Playlist    │
                     │  ListView)   │
                     └──────────────┘
```

### Configuration Flow

```
┌──────────────┐                    ┌──────────────┐
│ Application  │                    │    Disk      │
│   Start      │                    │              │
└──────┬───────┘                    │ config.ini   │
       │                            │ shortcuts.ini│
       ▼                            │ favorites.ini│
┌──────────────┐     Load           │ history.ini  │
│   TConfig    │◄───────────────────┤              │
│              │                    └──────────────┘
│ LoadSettings │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Apply to     │
│ - MPVEngine  │
│ - MainForm   │
│ - Playlist   │
└──────────────┘


┌──────────────┐                    ┌──────────────┐
│ User Changes │                    │    Disk      │
│  Settings    │                    │              │
└──────┬───────┘                    │ config.ini   │
       │                            │              │
       ▼                            │              │
┌──────────────┐     Save           │              │
│   TConfig    │───────────────────►│              │
│              │                    │              │
│ SaveSettings │                    │              │
└──────────────┘                    └──────────────┘
```

---

## DVD and Blu-ray Support

3nity Media supports DVD and Blu-ray folder playback with a two-tier approach: native protocol and fallback mode.

### Disc Structure Detection

```
DVD Structure:                    Blu-ray Structure:
───────────────                   ─────────────────
VIDEO_TS/                         BDMV/
├── VIDEO_TS.IFO                  ├── index.bdmv
├── VIDEO_TS.VOB                  ├── MovieObject.bdmv
├── VTS_01_0.IFO                  ├── PLAYLIST/
├── VTS_01_0.VOB                  │   └── *.mpls
├── VTS_01_1.VOB                  ├── CLIPINF/
├── VTS_01_2.VOB                  │   └── *.clpi
├── VTS_02_0.IFO                  └── STREAM/
├── VTS_02_1.VOB                      ├── 00000.m2ts
└── ...                               ├── 00001.m2ts
                                      └── ...
```

### Playback Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                  DVD/Blu-ray Playback Flow                   │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. User selects folder                                     │
│     │                                                       │
│     ▼                                                       │
│  2. Detect disc type                                        │
│     ├── VIDEO_TS exists? ──► DVD                           │
│     └── BDMV exists? ──────► Blu-ray                       │
│     │                                                       │
│     ▼                                                       │
│  3. Try native protocol                                     │
│     ├── DVD: dvd://path/to/VIDEO_TS                        │
│     └── Blu-ray: bd://path/to/BDMV                         │
│     │                                                       │
│     ▼                                                       │
│  4. If native fails ──► Fallback mode                      │
│     │                                                       │
│     ▼                                                       │
│  5. Fallback: Find and play content files                  │
│     ├── DVD: Detect main title, queue VOB files            │
│     └── Blu-ray: Play largest M2TS file                    │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### DVD Fallback Algorithm

When the native `dvd://` protocol fails (no libdvdnav), the fallback mode:

1. **Scans VIDEO_TS folder** for all VOB files
2. **Groups by title number** - VTS_XX_Y.VOB where XX = title
3. **Selects main title** - Title with largest total size
4. **Queues all parts** - VTS_XX_1.VOB, VTS_XX_2.VOB, etc. to playlist
5. **Plays sequentially** via playlist manager

```pascal
// DVD title detection pseudocode
for each .VOB file in VIDEO_TS:
  if filename matches VTS_XX_Y.VOB pattern:
    extract title number (XX)
    add to TitleFiles[XX]
    accumulate TitleSizes[XX]

BestTitle := title with max TitleSizes[XX]
AddFilesToPlaylist(TitleFiles[BestTitle])
```

**Important:** MPV does not support the `concat:` protocol (unlike ffmpeg). Files must be played sequentially via the playlist.

### Blu-ray Fallback Algorithm

When the native `bd://` protocol fails (no libbluray):

1. **Scans BDMV/STREAM** folder for all M2TS files
2. **Finds largest file** - Typically the main movie
3. **Plays single file** directly

```pascal
// Blu-ray fallback pseudocode
LargestFile := ''
LargestSize := 0

for each .m2ts file in BDMV/STREAM:
  if FileSize > LargestSize:
    LargestFile := filename
    LargestSize := FileSize

PlayFile(LargestFile)
```

### Seeking Configuration

MPEG-2 based formats (VOB, M2TS) require precise seeking:

```pascal
// In TMPVEngine, set hr-seek for disc formats
FileExt := LowerCase(ExtractFileExt(URL));
if (FileExt = '.vob') or (FileExt = '.m2ts') then
  mpv_set_property_string(FHandle, 'hr-seek', 'yes');
```

| Property | Value | Purpose |
|----------|-------|---------|
| `hr-seek` | `yes` | Accurate frame-level seeking for MPEG-2 |

### Related Code Locations

| File | Procedure | Purpose |
|------|-----------|---------|
| `uMainForm.pas` | `OpenDVD` | DVD folder detection and playback |
| `uMainForm.pas` | `OpenBluray` | Blu-ray folder detection and playback |
| `uMPVEngine.pas` | `SetupDVDNavigation` | DVD menu navigation support |
| `uMPVEngine.pas` | `Play` | hr-seek configuration |

---

## MPV Integration

### Library Loading

```pascal
// uLibMPV.pas - Dynamic library loading
const
  {$IFDEF WINDOWS}
  LIBMPV_DLL = 'mpv-2.dll';
  {$ELSE}
  LIBMPV_DLL = 'libmpv.so.2';
  {$ENDIF}

// Load library at runtime
FLibHandle := LoadLibrary(LIBMPV_DLL);
if FLibHandle <> NilHandle then
begin
  mpv_create := GetProcAddress(FLibHandle, 'mpv_create');
  mpv_initialize := GetProcAddress(FLibHandle, 'mpv_initialize');
  // ... load all functions
end;
```

### MPV Context Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│                     MPV LIFECYCLE                           │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Create Context                                          │
│     mpv_create() ──► FHandle                                │
│                                                             │
│  2. Configure Options                                       │
│     mpv_set_option_string("wid", WindowHandle)              │
│     mpv_set_option_string("hwdec", "auto")                  │
│     mpv_set_option_string("cache", "yes")                   │
│                                                             │
│  3. Initialize                                              │
│     mpv_initialize()                                        │
│                                                             │
│  4. Register Event Callback                                 │
│     mpv_set_wakeup_callback()                               │
│                                                             │
│  5. Observe Properties                                      │
│     mpv_observe_property("time-pos", ...)                   │
│     mpv_observe_property("duration", ...)                   │
│     mpv_observe_property("pause", ...)                      │
│                                                             │
│  6. Event Loop (Timer-based)                                │
│     while mpv_wait_event() do                               │
│       HandleEvent()                                         │
│                                                             │
│  7. Cleanup                                                 │
│     mpv_terminate_destroy()                                 │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Command Interface

```pascal
// Execute mpv commands via command interface
procedure TMPVEngine.SendCommand(const Args: array of string);
var
  Cmd: array of PAnsiChar;
  I: Integer;
begin
  SetLength(Cmd, Length(Args) + 1);
  for I := 0 to High(Args) do
    Cmd[I] := PAnsiChar(AnsiString(Args[I]));
  Cmd[High(Cmd)] := nil;

  mpv_command(FHandle, @Cmd[0]);
end;

// Example usage
SendCommand(['loadfile', FileName, 'replace']);
SendCommand(['seek', FloatToStr(Seconds), 'absolute']);
SendCommand(['set', 'volume', IntToStr(Volume)]);
```

### Property System

```pascal
// Get property
function TMPVEngine.GetDoubleProperty(const Name: string): Double;
var
  Value: Double;
begin
  if mpv_get_property(FHandle, PAnsiChar(Name), MPV_FORMAT_DOUBLE, @Value) >= 0 then
    Result := Value
  else
    Result := 0;
end;

// Set property
procedure TMPVEngine.SetIntProperty(const Name: string; Value: Int64);
begin
  mpv_set_property(FHandle, PAnsiChar(Name), MPV_FORMAT_INT64, @Value);
end;

// Observe property changes
mpv_observe_property(FHandle, 0, 'time-pos', MPV_FORMAT_DOUBLE);
mpv_observe_property(FHandle, 0, 'pause', MPV_FORMAT_FLAG);
```

---

## Event System

### MPV Events to Application Events

```
┌────────────────────┐     ┌────────────────────┐     ┌────────────────────┐
│    mpv events      │     │   TMPVEngine       │     │   Application      │
│                    │     │   Handler          │     │   Events           │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_FILE_    │────►│ HandleFileLoaded   │────►│ OnFileLoaded       │
│   LOADED           │     │                    │     │                    │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_END_FILE │────►│ HandleEndFile      │────►│ OnEndFile          │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_PROPERTY │────►│ HandleProperty     │────►│ OnPositionChange   │
│   _CHANGE          │     │   Change           │     │ OnMetadata         │
│   (time-pos)       │     │                    │     │ OnStatusChange     │
├────────────────────┤     ├────────────────────┤     ├────────────────────┤
│ MPV_EVENT_LOG_MSG  │────►│ HandleLogMessage   │────►│ OnLog              │
└────────────────────┘     └────────────────────┘     └────────────────────┘
```

### Event Types in TMPVEngine

```pascal
// Status and playback events
property OnStatusChange: TMPVStatusChangeEvent;
property OnPositionChange: TMPVPositionChangeEvent;
property OnFileLoaded: TMPVFileLoadedEvent;
property OnEndFile: TMPVEndFileEvent;
property OnStartFile: TNotifyEvent;

// Track events
property OnAudioTrackChange: TMPVTrackChangeEvent;
property OnSubtitleTrackChange: TMPVTrackChangeEvent;
property OnVideoTrackChange: TMPVTrackChangeEvent;

// Media information events
property OnMetadata: TMPVMetadataEvent;
property OnVideoResize: TMPVVideoResizeEvent;
property OnChapterChange: TNotifyEvent;

// Navigation events
property OnSeek: TNotifyEvent;
property OnPlaybackRestart: TNotifyEvent;
property OnLoadNextFile: TMPVLoadNextFileEvent;

// Logging
property OnLog: TMPVLogEvent;
```

### Event Handling in MainForm

```pascal
procedure TfrmMain.InitializeMPV;
begin
  FMPVEngine := TMPVEngine.Create(pnlVideo.Handle);

  // Connect events
  FMPVEngine.OnStatusChange := @OnMPVStatusChange;
  FMPVEngine.OnPositionChange := @OnMPVPositionChange;
  FMPVEngine.OnFileLoaded := @OnMPVFileLoaded;
  FMPVEngine.OnEndFile := @OnMPVEndFile;
  FMPVEngine.OnMetadata := @OnMPVMetadata;
  FMPVEngine.OnVideoResize := @OnMPVVideoResize;

  FMPVEngine.Initialize;
end;

procedure TfrmMain.OnMPVPositionChange(Sender: TObject; Position: Double);
begin
  // Update position slider
  tbPosition.Position := Round(Position);

  // Update time display
  lblTime.Caption := FormatTime(Position) + ' / ' + FormatTime(FMPVEngine.Duration);
end;

procedure TfrmMain.OnMPVEndFile(Sender: TObject; Reason: Integer; Error: Integer);
begin
  case Reason of
    MPV_END_FILE_REASON_EOF:
      FPlaylist.PlayNext;  // Auto-advance to next
    MPV_END_FILE_REASON_ERROR:
      ShowError('Playback error: ' + IntToStr(Error));
  end;
end;
```

---

## Configuration System

### Configuration Files

| File | Content |
|------|---------|
| `config.ini` | General settings, video, audio, cache |
| `shortcuts.ini` | Custom keyboard shortcuts |
| `favorites.ini` | Favorite files and stations |
| `history.ini` | Playback history with positions |
| `bookmarks.ini` | Position bookmarks per file |
| `playlist.m3u` | Auto-saved playlist |

### TConfig Structure

```pascal
TConfig = class
private
  FConfigPath: string;
  FSettings: TAppSettings;

public
  // General
  property Language: string;
  property SingleInstance: Boolean;
  property ScreenshotPath: string;

  // Video
  property Brightness: Integer;
  property Contrast: Integer;
  property HWAccel: Boolean;

  // Audio
  property Volume: Integer;
  property EqualizerBands: array[0..9] of Double;

  // Persistence
  procedure LoadSettings;
  procedure SaveSettings;

  // History
  procedure AddToHistory(const Item: THistoryItem);
  function GetHistory: THistoryItems;

  // Bookmarks
  procedure AddBookmark(const Item: TBookmarkItem);
  function GetBookmarksForFile(const FileName: string): TBookmarkItems;

  // Favorites
  procedure AddFavorite(const Item: TFavoriteItem);
  function GetFavorites: TFavoriteItems;
end;
```

---

## Localization System

### Translation Loading

```pascal
// uLocale.pas
procedure LoadLanguage(const LangCode: string);
var
  LangFile: string;
  Ini: TIniFile;
begin
  LangFile := GetLangPath + LangCode + '.lang';
  if FileExists(LangFile) then
  begin
    Ini := TIniFile.Create(LangFile);
    try
      // Load all sections
      LoadSection(Ini, 'Menu');
      LoadSection(Ini, 'Buttons');
      LoadSection(Ini, 'Messages');
      // ...
    finally
      Ini.Free;
    end;
  end;
end;

// Usage
Caption := _T('Menu', 'File');  // Returns translated "File"
```

### Language File Format

```ini
[Main]
Language=English
LanguageCode=en
Author=Nicolas DEOUX

[Menu]
File=File
Open=Open
OpenURL=Open URL
Exit=Exit

[Buttons]
Play=Play
Pause=Pause
Stop=Stop

[Messages]
ErrorFileNotFound=File not found: %s
ConfirmExit=Are you sure you want to exit?
```

---

## Threading Model

### Main Thread

- All UI operations
- Event handling from mpv
- Configuration management
- Playlist management

### MPV Thread

- Managed internally by libmpv
- Audio/video decoding
- Rendering
- Network operations (streaming)

### Timer-Based Event Processing

```pascal
// MainForm timer polls mpv events
procedure TfrmMain.tmrMPVEventsTimer(Sender: TObject);
begin
  if Assigned(FMPVEngine) then
    FMPVEngine.ProcessEvents;  // Non-blocking
end;

// MPVEngine processes queued events
procedure TMPVEngine.ProcessEvents;
var
  Event: Pmpv_event;
begin
  repeat
    Event := mpv_wait_event(FHandle, 0);  // Non-blocking
    if Event^.event_id <> MPV_EVENT_NONE then
      HandleEvent(Event);
  until Event^.event_id = MPV_EVENT_NONE;
end;
```

### Thread Safety

- MPV properties are thread-safe
- UI updates via `Application.QueueAsyncCall` when needed
- Configuration access synchronized on main thread
- Playlist modifications synchronized on main thread

---

## See Also

- [Contributing Guide](CONTRIBUTING.md) - Code style and development guidelines
- [Installation Guide](INSTALL.md) - Building from source
- [User Guide](USER_GUIDE.md) - End-user documentation

---

## Version Information

- **Last updated:** 2026-01-07
- **Applies to:** 3nity Media v0.x and later
