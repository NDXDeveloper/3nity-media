# API Reference - 3nity Media

This document provides a reference for the main classes of 3nity Media. These classes form the core architecture of the application.

## Table of Contents

- [TMPVEngine](#tmpvengine)
- [TPlaylistManager](#tplaylistmanager)
- [TRadioManager](#tradiomanager)
- [TConfigManager](#tconfigmanager)

---

## TMPVEngine

**Unit:** `Core/uMPVEngine.pas`

Complete libmpv wrapper providing all multimedia playback functionality.

### Overview

TMPVEngine is the heart of the media player, wrapping the libmpv library. It handles:
- Video/Audio playback with full format support
- DVD/Bluray navigation
- Track selection (audio, subtitles, video)
- 10-band audio equalizer
- Video properties (brightness, contrast, saturation, hue, gamma)
- Streaming with ICY metadata for radios
- Cache management per source type

### Status Enumeration

```pascal
TMPVStatus = (
  msNone,           // No media loaded
  msOpening,        // Opening media
  msClosing,        // Closing media
  msPlayStarting,   // Starting playback
  msPlaying,        // Currently playing
  msPaused,         // Paused
  msStopped,        // Stopped
  msError,          // Error occurred
  msErrorRetry      // Error with retry
);
```

### Constructor/Destructor

| Method | Description |
|--------|-------------|
| `Create` | Creates a new TMPVEngine instance |
| `Destroy` | Frees resources and shuts down mpv |

### Initialization Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `Initialize` | `AWindowHandle: THandle; AWidth, AHeight: Integer` | `Boolean` | Initialize mpv with a window handle for video output |
| `Shutdown` | - | - | Shut down the mpv engine |
| `ResizeVideoWindow` | `AWidth, AHeight: Integer` | - | Resize the video output area |
| `IsRunning` | - | `Boolean` | Check if mpv is currently running |

### Playback Control Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `PlayMedia` | `URL: string` | Load and play a media file or URL |
| `CloseMedia` | - | Close the current media |
| `Pause` | - | Pause playback |
| `Resume` | - | Resume playback |
| `Stop` | - | Stop playback completely |
| `TogglePause` | - | Toggle between pause and play |
| `SendPlayPause` | - | Send play/pause command |

### Seek Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SeekAbsolute` | `Seconds: Double` | Seek to an absolute position in seconds |
| `SeekRelative` | `Seconds: Double` | Seek relative to current position |
| `SeekPercent` | `Percent: Double` | Seek to a percentage of duration |
| `SeekBy` | `Value: Integer` | Seek by a number of seconds |
| `SeekTo` | `Value: Int64; Method: Integer` | Seek to position with specified method |
| `FrameStep` | - | Advance one frame |
| `FrameBackStep` | - | Go back one frame |

### Audio Control Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SetAudioTrack` | `TrackID: Integer` | Select an audio track |
| `GetAudioID` | - | Get current audio track ID |
| `SetAudioID` | `Value: Integer` | Set audio track by ID |
| `SetAudioOutput` | `Driver: string` | Set audio output driver |
| `SetAudioDevice` | `Device: string` | Set audio output device |
| `SetAudioNormalize` | `Enable: Boolean` | Enable/disable audio normalization |
| `SetAudioChannels` | `Channels: Integer` | Set number of audio channels |
| `GetAudioDeviceList` | - | Get list of available audio devices |

### Equalizer Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SetEqualizerBand` | `Band: Integer; dB: Double` | Set a specific band value (-12 to +12 dB) |
| `GetEqualizerBand` | `Band: Integer` | Get current band value |
| `SetEqualizerPreset` | `Values: string` | Set all bands from a preset string |
| `GetEqualizerPreset` | - | Get current preset as string |
| `ResetEqualizer` | - | Reset all bands to 0 dB |
| `EnableEqualizer` | `Enable: Boolean` | Enable or disable the equalizer |
| `ApplyEqualizer` | - | Apply current equalizer settings |
| `SetPreamp` | `dB: Double` | Set preamplifier gain |

### Subtitle Control Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SetSubtitleTrack` | `TrackID: Integer` | Select a subtitle track |
| `LoadSubtitle` | `FileName: string` | Load an external subtitle file |
| `UnloadSubtitles` | - | Remove all loaded subtitles |
| `SetSubFont` | `FontName: string` | Set subtitle font |
| `SetSubFontSize` | `Size: Integer` | Set subtitle font size |
| `SetSubFontColor` | `Color: Cardinal` | Set subtitle font color |
| `SetSubBold` | `Bold: Boolean` | Set bold style |
| `SetSubItalic` | `Italic: Boolean` | Set italic style |
| `SetSubOutlineColor` | `Color: Cardinal` | Set outline color |
| `SetSubOutlineSize` | `Size: Integer` | Set outline size |
| `SetSubPosition` | `Position: Integer` | Set vertical position (0-100) |
| `SetSubEncoding` | `Encoding: string` | Set character encoding |
| `SetSubAutoLoad` | `AutoLoad: Boolean` | Enable/disable auto-loading |

### Video Control Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SetVideoTrack` | `TrackID: Integer` | Select a video track |
| `SetAspectRatio` | `Ratio: string` | Set aspect ratio (e.g., "16:9") |
| `Screenshot` | `FileName: string` | Take a screenshot |
| `ScreenshotToFile` | `FileName, Mode: string` | Take a screenshot with mode |
| `SetScreenshotDirectory` | `Path: string` | Set screenshot save directory |
| `SetScreenshotFormat` | `Format: string` | Set screenshot format (png/jpg) |
| `ToggleFullscreen` | - | Toggle fullscreen mode |
| `ReloadCurrentFile` | - | Reload the current media file |

### DVD/Bluray Navigation Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `IsDVD` | - | `Boolean` | Check if playing a DVD |
| `IsDVDNav` | - | `Boolean` | Check if DVD navigation is active |
| `IsBluray` | - | `Boolean` | Check if playing a Bluray |
| `DVDGoMenu` | - | `Boolean` | Go to DVD menu |
| `DVDMenu` | - | - | Open DVD menu |
| `DVDMenuSelect` | - | - | Select current menu item |
| `DVDMenuUp/Down/Left/Right` | - | - | Navigate menu |
| `DVDTitle` | `TitleID: Integer` | - | Jump to title |
| `DVDChapter` | `ChapterID: Integer` | - | Jump to chapter |
| `DVDNextChapter` | - | - | Go to next chapter |
| `DVDPrevChapter` | - | - | Go to previous chapter |

### Generic Property Access

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `SendCommand` | `Args: array of string` | - | Send a command with arguments |
| `SendCmd` | `Command: string` | - | Send a command string |
| `SetOption` | `Name, Value: string` | - | Set an mpv option |
| `GetPropertyString` | `Name: string` | `string` | Get a string property |
| `GetPropertyDouble` | `Name: string` | `Double` | Get a double property |
| `GetPropertyInt` | `Name: string` | `Int64` | Get an integer property |
| `GetPropertyBool` | `Name: string` | `Boolean` | Get a boolean property |
| `SetPropertyString` | `Name, Value: string` | - | Set a string property |
| `SetPropertyDouble` | `Name: string; Value: Double` | - | Set a double property |
| `SetPropertyInt` | `Name: string; Value: Int64` | - | Set an integer property |
| `SetPropertyBool` | `Name: string; Value: Boolean` | - | Set a boolean property |

### Read-Only Properties

| Property | Type | Description |
|----------|------|-------------|
| `Handle` | `Pmpv_handle` | Internal mpv handle |
| `Status` | `TMPVStatus` | Current playback status |
| `StreamInfo` | `TMPVStreamInfo` | Information about current stream |
| `RenderInfo` | `TMPVRenderInfo` | Video rendering information |
| `AudioTracks` | `TMPVTrackList` | List of audio tracks |
| `SubtitleTracks` | `TMPVTrackList` | List of subtitle tracks |
| `VideoTracks` | `TMPVTrackList` | List of video tracks |
| `Position` | `Double` | Current position in seconds |
| `Duration` | `Double` | Total duration in seconds |
| `PercentPos` | `Double` | Current position as percentage |
| `TitleCount` | `Integer` | Number of DVD/BD titles |
| `ChapterCount` | `Integer` | Number of chapters |
| `CurrentTitle` | `Integer` | Current title number |
| `CurrentChapter` | `Integer` | Current chapter number |
| `MediaFile` | `string` | Path to current media file |
| `FileLoaded` | `string` | Path to loaded file |
| `Initialized` | `Boolean` | Whether mpv is initialized |
| `LastError` | `string` | Last error message |

### Read-Write Properties

| Property | Type | Description |
|----------|------|-------------|
| `Volume` | `Integer` | Volume level (0-100) |
| `Muted` | `Boolean` | Mute state |
| `Speed` | `Double` | Playback speed (1.0 = normal) |
| `Brightness` | `Integer` | Video brightness (-100 to 100) |
| `Contrast` | `Integer` | Video contrast (-100 to 100) |
| `Saturation` | `Integer` | Video saturation (-100 to 100) |
| `Hue` | `Integer` | Video hue (-100 to 100) |
| `Gamma` | `Integer` | Video gamma (-100 to 100) |
| `SubScale` | `Double` | Subtitle scale factor |
| `SubDelay` | `Double` | Subtitle delay in seconds |
| `SubVisible` | `Boolean` | Subtitle visibility |
| `AudioDelay` | `Double` | Audio delay in seconds |
| `EqualizerEnabled` | `Boolean` | Equalizer on/off |
| `AspectMode` | `Integer` | Aspect ratio mode |
| `AspectFactor` | `Double` | Custom aspect ratio factor |
| `Deinterlace` | `Integer` | Deinterlace mode |
| `DeinterlaceAlg` | `Integer` | Deinterlace algorithm |
| `HWAccel` | `Boolean` | Hardware acceleration on/off |
| `VideoOutput` | `string` | Video output driver |
| `AudioOutput` | `string` | Audio output driver |
| `CacheSize[Index]` | `Integer` | Cache size by source type |

### Events

| Event | Type | Description |
|-------|------|-------------|
| `OnLog` | `TMPVLogEvent` | Log message received |
| `OnLogClear` | `TNotifyEvent` | Log cleared |
| `OnStatusChange` | `TMPVStatusChangeEvent` | Playback status changed |
| `OnPositionChange` | `TMPVPositionChangeEvent` | Position updated |
| `OnAudioTrackChange` | `TMPVTrackChangeEvent` | Audio track changed |
| `OnSubtitleTrackChange` | `TMPVTrackChangeEvent` | Subtitle track changed |
| `OnVideoTrackChange` | `TMPVTrackChangeEvent` | Video track changed |
| `OnMetadata` | `TMPVMetadataEvent` | Metadata received (ICY) |
| `OnVideoResize` | `TMPVVideoResizeEvent` | Video size changed |
| `OnProgress` | `TMPVProgressEvent` | Progress update |
| `OnEndFile` | `TMPVEndFileEvent` | File playback ended |
| `OnStartFile` | `TNotifyEvent` | File started loading |
| `OnFileLoaded` | `TMPVFileLoadedEvent` | File loaded successfully |
| `OnSeek` | `TNotifyEvent` | Seek operation started |
| `OnPlaybackRestart` | `TNotifyEvent` | Playback restarted after seek |
| `OnChapterChange` | `TNotifyEvent` | Chapter changed |
| `OnLoadNextFile` | `TMPVLoadNextFileEvent` | Request for next file |

---

## TPlaylistManager

**Unit:** `Core/uPlaylistManager.pas`

Playlist management with M3U/PLS support, shuffle modes, and metadata extraction.

### Overview

TPlaylistManager handles:
- M3U, M3U8, and PLS playlist formats
- Shuffle and repeat playback modes (Normal, Repeat One, Repeat All, Shuffle, Shuffle Repeat)
- Drag and drop reordering
- Metadata extraction via ffprobe
- Playlist persistence

### Playback Modes

```pascal
TPlaybackMode = (
  pmNormal,        // Play sequentially, stop at end
  pmRepeatOne,     // Repeat current track
  pmRepeatAll,     // Loop entire playlist
  pmShuffle,       // Random order, no repeat
  pmShuffleRepeat  // Random order with repeat
);
```

### Constructor/Destructor

| Method | Description |
|--------|-------------|
| `Create` | Creates a new playlist manager |
| `Destroy` | Frees all resources |

### Item Management Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `Add` | `AFileName: string` | `Integer` | Add a file and extract metadata |
| `Add` | `AItem: TPlaylistItem` | `Integer` | Add a pre-built item |
| `AddFiles` | `FileNames: TStrings` | - | Add multiple files/playlists |
| `Insert` | `Index: Integer; AFileName: string` | - | Insert at position |
| `Delete` | `Index: Integer` | - | Remove item at index |
| `Remove` | `AFileName: string` | - | Remove by filename |
| `Clear` | - | - | Remove all items |

### Item Reordering Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `MoveUp` | `Index: Integer` | Move item one position up |
| `MoveDown` | `Index: Integer` | Move item one position down |
| `Move` | `FromIndex, ToIndex: Integer` | Move item to new position |
| `Swap` | `Index1, Index2: Integer` | Swap two items |
| `Reverse` | - | Reverse playlist order |
| `Sort` | `Ascending: Boolean` | Sort by filename |
| `SortByTitle` | `Ascending: Boolean` | Sort by title metadata |
| `SortByArtist` | `Ascending: Boolean` | Sort by artist metadata |
| `SortByDuration` | `Ascending: Boolean` | Sort by duration |
| `Randomize` | - | Shuffle playlist order |

### Navigation Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `GetNext` | - | `Integer` | Get next index based on mode |
| `GetPrevious` | - | `Integer` | Get previous index based on mode |
| `GetFirst` | - | `Integer` | Get first index (0) |
| `GetLast` | - | `Integer` | Get last index |
| `GetRandom` | - | `Integer` | Get random index |
| `PlayIndex` | `Index: Integer` | - | Play specific index |
| `PlayNext` | - | - | Play next track |
| `PlayPrevious` | - | - | Play previous track |
| `PlayFirst` | - | - | Play first track |
| `PlayLast` | - | - | Play last track |

### Search Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `Find` | `AFileName: string` | `Integer` | Find by filename, returns -1 if not found |
| `FindByTitle` | `ATitle: string` | `Integer` | Find by title |
| `Search` | `AText: string` | `TIntegerDynArray` | Search in all fields |
| `IndexOf` | `AFileName: string` | `Integer` | Alias for Find |
| `Contains` | `AFileName: string` | `Boolean` | Check if file exists |

### Selection Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SelectAll` | - | Select all items |
| `SelectNone` | - | Deselect all items |
| `SelectInvert` | - | Invert selection |
| `SetSelected` | `Index: Integer; Selected: Boolean` | Set item selection |
| `GetSelectedCount` | - | Get number of selected items |
| `GetSelectedIndices` | - | Get array of selected indices |
| `DeleteSelected` | - | Delete all selected items |

### Playlist File I/O Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `LoadFromFile` | `AFileName: string` | `Boolean` | Load M3U or PLS file |
| `SaveToFile` | `AFileName: string` | `Boolean` | Save to M3U or PLS |
| `LoadM3U` | `AFileName: string` | `Boolean` | Load M3U format |
| `LoadPLS` | `AFileName: string` | `Boolean` | Load PLS format |
| `SaveM3U` | `AFileName: string` | `Boolean` | Save as M3U |
| `SavePLS` | `AFileName: string` | `Boolean` | Save as PLS |

### Item Info Update Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `UpdateItemInfo` | `Index, Title, Artist, Album, Duration` | Update item metadata |
| `UpdateItemDuration` | `Index: Integer; Duration: Double` | Update duration only |
| `MarkAsPlayed` | `Index: Integer` | Mark item as played |
| `ResetPlayedStatus` | - | Reset all played flags |

### Statistics Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `GetTotalDuration` | `Double` | Total duration in seconds |
| `GetTotalDurationString` | `string` | Formatted total duration |
| `GetPlayedCount` | `Integer` | Number of played items |
| `GetUnplayedCount` | `Integer` | Number of unplayed items |

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Count` | `Integer` | Number of items |
| `Items[Index]` | `TPlaylistItem` | Access item by index (default property) |
| `CurrentIndex` | `Integer` | Currently playing index |
| `CurrentItem` | `TPlaylistItem` | Currently playing item |
| `PlaybackMode` | `TPlaybackMode` | Current playback mode |
| `IsEmpty` | `Boolean` | True if no items |
| `HasNext` | `Boolean` | True if next exists for current mode |
| `HasPrevious` | `Boolean` | True if previous exists for current mode |
| `Modified` | `Boolean` | True if playlist was modified |
| `FileName` | `string` | Path to loaded playlist file |

### Events

| Event | Type | Description |
|-------|------|-------------|
| `OnChange` | `TPlaylistChangeEvent` | Playlist was modified |
| `OnItemAdded` | `TPlaylistItemEvent` | Item was added |
| `OnItemRemoved` | `TPlaylistItemEvent` | Item was removed |
| `OnCurrentChange` | `TPlaylistItemEvent` | Current index changed |
| `OnPlay` | `TPlaylistPlayEvent` | Play was triggered |
| `OnClear` | `TNotifyEvent` | Playlist was cleared |

### Utility Functions

```pascal
function FormatDuration(Seconds: Double): string;
function IsPlaylistFile(const AFileName: string): Boolean;
function IsSupportedMediaFile(const AFileName: string): Boolean;
```

---

## TRadioManager

**Unit:** `Core/uRadioManager.pas`

Radio station management with Icecast directory support and favorites.

### Overview

TRadioManager provides:
- Icecast directory XML parsing
- Custom station management
- Genre filtering and search
- Favorites management
- Station persistence

### Constructor/Destructor

| Method | Description |
|--------|-------------|
| `Create` | Creates a new radio manager |
| `Destroy` | Frees all resources |

### Loading Station Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `LoadFromIcecast` | `URL: string` | `Boolean` | Load from Icecast directory |
| `LoadFromFile` | `FileName: string` | `Boolean` | Load from local XML file |
| `LoadFromXML` | `XMLContent: string` | `Boolean` | Parse XML content |
| `LoadDefaultStations` | - | - | Load built-in French stations |
| `Clear` | - | - | Clear all stations |

### Filtering and Search Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `FilterByGenre` | `Genre: string` | Show only matching genre |
| `FilterByName` | `SearchText: string` | Search in name and genre |
| `FilterByCountry` | `Country: string` | Filter by country |
| `FilterFavorites` | - | Show only favorites |
| `ClearFilter` | - | Show all stations |
| `Search` | `Text: string` | Search across all fields |

### Custom Station Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `AddCustomStation` | `Name, URL, Genre: string; Bitrate: Integer` | `Integer` | Add custom station |
| `EditCustomStation` | `Index: Integer; Name, URL, Genre: string; Bitrate: Integer` | - | Modify station |
| `DeleteCustomStation` | `Index: Integer` | - | Remove station |
| `GetCustomStation` | `Index: Integer` | `TRadioStation` | Get station by index |

### Favorites Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `AddToFavorites` | `Station: TRadioStation` | Add to favorites |
| `RemoveFromFavorites` | `Index: Integer` | Remove from favorites |
| `IsFavorite` | `AURL: string` | Check if URL is in favorites |
| `ToggleFavorite` | `Station: TRadioStation` | Toggle favorite status |
| `GetFavorite` | `Index: Integer` | Get favorite by index |

### Persistence Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `SaveCustomStations` | `FileName: string` | `Boolean` | Save custom stations |
| `LoadCustomStations` | `FileName: string` | `Boolean` | Load custom stations |
| `SaveFavorites` | `FileName: string` | `Boolean` | Save favorites |
| `LoadFavorites` | `FileName: string` | `Boolean` | Load favorites |

### Play Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `PlayStation` | `Index: Integer` | Play station from full list |
| `PlayFilteredStation` | `Index: Integer` | Play from filtered list |

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Count` | `Integer` | Total station count |
| `FilteredCount` | `Integer` | Filtered station count |
| `CustomCount` | `Integer` | Custom station count |
| `FavoritesCount` | `Integer` | Favorites count |
| `Stations[Index]` | `TRadioStation` | Access station by index |
| `FilteredStations[Index]` | `TRadioStation` | Access filtered station |
| `Genres` | `TStringList` | Available genres |
| `LastError` | `string` | Last error message |
| `Loading` | `Boolean` | True if loading |
| `Modified` | `Boolean` | True if modified |

### Events

| Event | Type | Description |
|-------|------|-------------|
| `OnLoad` | `TRadioLoadEvent` | Stations loaded |
| `OnError` | `TRadioErrorEvent` | Error occurred |
| `OnPlay` | `TRadioPlayEvent` | Station play requested |
| `OnChange` | `TNotifyEvent` | Station list changed |

### Constants

```pascal
ICECAST_DIRECTORY_URL = 'https://dir.xiph.org/yp.xml';
```

---

## TConfigManager

**Unit:** `Common/uConfig.pas`

Application configuration management with INI file persistence.

### Overview

TConfigManager handles:
- Loading and saving settings to INI file
- Window state persistence
- History and recent files management
- Bookmarks and favorites
- Session playlist save/restore

### Constructor/Destructor

| Method | Description |
|--------|-------------|
| `Create` | Creates config manager with defaults |
| `Destroy` | Saves pending changes and frees resources |

### Load/Save Methods

| Method | Description |
|--------|-------------|
| `Load` | Load all settings from INI file |
| `Save` | Save all settings to INI file |

### Settings Accessors

| Method | Returns | Description |
|--------|---------|-------------|
| `GetGeneral` | `TGeneralSettings` | Get general settings |
| `GetVideo` | `TVideoSettings` | Get video settings |
| `GetAudio` | `TAudioSettings` | Get audio settings |
| `GetSubtitles` | `TSubtitleSettings` | Get subtitle settings |
| `GetCache` | `TCacheSettings` | Get cache settings |
| `SetGeneral` | - | Set general settings |
| `SetVideo` | - | Set video settings |
| `SetAudio` | - | Set audio settings |
| `SetSubtitles` | - | Set subtitle settings |
| `SetCache` | - | Set cache settings |
| `SetPlaybackMode` | - | Set playback mode |

### Window State Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `SaveWindowState` | `Section: string; AForm: TForm` | Save window position/size |
| `SaveWindowStateBounds` | `Section: string; Bounds: TRect; State: TWindowState` | Save bounds |
| `LoadWindowState` | `Section: string; AForm: TForm` | Restore window position/size |
| `HasWindowState` | `Section: string` | Check if state exists |

### History Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `AddToHistory` | `FileName, Title: string; Position, Duration: Double` | Add to history |
| `GetHistory` | - | Get all history items |
| `ClearHistory` | - | Clear all history |

### Recent Files Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `AddRecentFile` | `FileName: string` | Add to recent files |
| `GetRecentFiles` | - | Get recent files list |
| `RemoveRecentFile` | `FileName: string` | Remove from recent |
| `ClearRecentFiles` | - | Clear recent files |

### Playback Position Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `SavePlaybackPosition` | `FileName: string; Position: Double` | - | Save resume position |
| `GetPlaybackPosition` | `FileName: string` | `Double` | Get saved position |
| `ClearPlaybackPositions` | - | - | Clear all positions |

### Bookmark Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `AddBookmark` | `FileName, Name: string; Position: Double` | Add bookmark |
| `GetBookmarks` | - | Get all bookmarks |
| `GetBookmarksForFile` | `FileName: string` | Get bookmarks for file |
| `RemoveBookmark` | `FileName: string; Position: Double` | Remove bookmark |
| `ClearBookmarks` | - | Clear all bookmarks |
| `ClearBookmarksForFile` | `FileName: string` | Clear file's bookmarks |

### Favorites Methods

| Method | Parameters | Description |
|--------|------------|-------------|
| `AddFavorite` | `Name, Path: string; FavType; Category: string` | Add favorite |
| `GetFavorites` | - | Get all favorites |
| `GetFavoritesByCategory` | `Category: string` | Filter by category |
| `GetFavoritesByType` | `FavType: TFavoriteType` | Filter by type |
| `IsFavorite` | `Path: string` | Check if path is favorite |
| `RemoveFavorite` | `Path: string` | Remove favorite |
| `UpdateFavoriteLastPlayed` | `Path: string` | Update last played time |
| `ClearFavorites` | - | Clear all favorites |
| `GetFavoriteCategories` | - | Get list of categories |

### Session Playlist Methods

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `SaveSessionPlaylist` | `Items, CurrentIndex, PlaybackMode, CurrentPosition` | - | Save session |
| `LoadSessionPlaylist` | `out Items, CurrentIndex, PlaybackMode, CurrentPosition` | `Boolean` | Restore session |
| `ClearSessionPlaylist` | - | - | Clear saved session |

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Settings` | `TAppSettings` | Full settings record |
| `Modified` | `Boolean` | True if settings changed |
| `ConfigPath` | `string` | Path to config directory |

### Global Functions

```pascal
function GetAppDataDir: string;     // Application data directory
function GetTempDir: string;        // System temp directory
function GetConfigDir: string;      // Configuration directory
function GetUserPicturesDir: string; // User's Pictures folder
```

### Configuration Files

| File | Location | Purpose |
|------|----------|---------|
| `config.ini` | `~/.config/3nity-media/` | Main settings |
| `history.ini` | `~/.config/3nity-media/` | Playback history |
| `bookmarks.ini` | `~/.config/3nity-media/` | Bookmarks |
| `favorites.ini` | `~/.config/3nity-media/` | Favorites |
| `session_playlist.ini` | `~/.config/3nity-media/` | Session restore |

---

## See Also

- [User Guide](USER_GUIDE.md)
- [Configuration Guide](CONFIG.md)
- [Contributing Guide](CONTRIBUTING.md)

---

## Version Information

- **Last Updated:** 2026-01-02
- **Applies to:** 3nity Media v0.x and later
