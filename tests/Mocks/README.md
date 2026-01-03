# Mock Objects

This directory contains mock implementations of core components for testing purposes. Mocks allow testing without external dependencies like libmpv or file system access.

## Mock Implementations

| File | Class | Mocks |
|------|-------|-------|
| `uMockMPVEngine.pas` | `TMockMPVEngine` | MPV media player engine |
| `uMockPlaylist.pas` | `TMockPlaylistManager` | Playlist manager |

## TMockMPVEngine

A complete mock implementation of the MPV engine that simulates playback behavior without requiring libmpv.

### Features

- **Playback Simulation**: Simulates file loading, playback states, seeking
- **Property Management**: Volume, speed, video settings, equalizer
- **Track Management**: Audio, video, and subtitle track lists
- **Event System**: Status change, position change, file loaded, end file, errors
- **Call Logging**: Records all method calls for verification

### Key Methods

```pascal
// Simulation control
procedure SimulateFileLoad(const FileName: string; Duration: Double; HasVideo: Boolean = True);
procedure SimulatePosition(Seconds: Double);
procedure SimulateEndFile(Reason: Integer = 0);
procedure SimulateError(const ErrorMsg: string);
procedure SimulateMetadata(const Title, Artist: string);

// Standard playback
procedure LoadFile(const FileName: string);
procedure Play;
procedure Pause;
procedure Resume;
procedure Stop;
procedure SeekAbsolute(Seconds: Double);
procedure SeekRelative(Seconds: Double);
```

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Status` | `TMPVStatus` | Current playback status |
| `Position` | `Double` | Current position in seconds |
| `Duration` | `Double` | Media duration in seconds |
| `Volume` | `Integer` | Volume level (0-150) |
| `Muted` | `Boolean` | Mute state |
| `Speed` | `Double` | Playback speed (0.1-4.0) |
| `Brightness` | `Integer` | Video brightness (-100 to 100) |
| `Contrast` | `Integer` | Video contrast (-100 to 100) |
| `Saturation` | `Integer` | Video saturation (-100 to 100) |

### Events

```pascal
property OnStatusChange: TMockStatusChangeEvent;    // Status transitions
property OnPositionChange: TMockPositionChangeEvent; // Position updates
property OnFileLoaded: TMockFileLoadedEvent;         // File loaded
property OnEndFile: TMockEndFileEvent;               // Playback ended
property OnError: TMockErrorEvent;                   // Error occurred
property OnMetadata: TMockMetadataEvent;             // Metadata received
```

### Usage Example

```pascal
procedure TMyTest.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FMPV.OnStatusChange := @OnMPVStatusChange;
  FMPV.OnEndFile := @OnMPVEndFile;
end;

procedure TMyTest.Test_PlaybackFlow;
begin
  FMPV.SimulateFileLoad('/music/song.mp3', 180.0);
  AssertEquals('Should be playing', Ord(msPlaying), Ord(FMPV.Status));

  FMPV.SimulatePosition(30.0);
  AssertEquals('Position should be 30', 30.0, FMPV.Position, 0.01);

  FMPV.SimulateEndFile(0);
  AssertEquals('Should be stopped', Ord(msStopped), Ord(FMPV.Status));
end;
```

## TMockPlaylistManager

A complete mock implementation of the playlist manager that works entirely in-memory.

### Features

- **Item Management**: Add, remove, move, clear items
- **Navigation**: Next, previous, first, last, play by index
- **Playback Modes**: Normal, repeat all, repeat one, shuffle
- **Ordering**: Sort, reverse, randomize
- **Search**: Find by filename, title, or general search
- **Selection**: Multi-select support
- **Event System**: Change, play, current change events
- **Call Logging**: Records all method calls for verification

### Key Methods

```pascal
// Item management
function Add(const AFileName: string): Integer;
function Add(const AItem: TMockPlaylistItem): Integer;
procedure Delete(Index: Integer);
procedure Move(FromIndex, ToIndex: Integer);
procedure Clear;

// Navigation
procedure PlayFirst;
procedure PlayLast;
procedure PlayNext;
procedure PlayPrevious;
procedure PlayIndex(Index: Integer);

// Ordering
procedure Sort(Ascending: Boolean = True);
procedure SortByTitle(Ascending: Boolean = True);
procedure SortByArtist(Ascending: Boolean = True);
procedure SortByDuration(Ascending: Boolean = True);
procedure Reverse;
procedure Randomize;

// Search
function Find(const AFileName: string): Integer;
function FindByTitle(const ATitle: string): Integer;
function Search(const AQuery: string): TIntegerDynArray;

// Test helpers
procedure PopulateWithTestData(ItemCount: Integer);
function GetCallLog: TStringList;
```

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `Count` | `Integer` | Number of items |
| `Items[Index]` | `TMockPlaylistItem` | Item at index |
| `CurrentIndex` | `Integer` | Current playing index |
| `CurrentItem` | `TMockPlaylistItem` | Current playing item |
| `PlaybackMode` | `TPlaybackMode` | Playback mode |
| `IsEmpty` | `Boolean` | True if playlist is empty |
| `HasNext` | `Boolean` | True if next track available |
| `HasPrevious` | `Boolean` | True if previous track available |

### Events

```pascal
property OnChange: TNotifyEvent;       // Playlist modified
property OnPlay: TMockPlayEvent;       // Item played
property OnCurrentChange: TNotifyEvent; // Current item changed
```

### Usage Example

```pascal
procedure TMyTest.SetUp;
begin
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.OnPlay := @OnPlaylistPlay;
end;

procedure TMyTest.Test_Navigation;
begin
  FPlaylist.PopulateWithTestData(5);  // Add 5 test items

  FPlaylist.PlayFirst;
  AssertEquals('Should be at index 0', 0, FPlaylist.CurrentIndex);

  FPlaylist.PlayNext;
  AssertEquals('Should be at index 1', 1, FPlaylist.CurrentIndex);

  FPlaylist.PlayLast;
  AssertEquals('Should be at index 4', 4, FPlaylist.CurrentIndex);
end;
```

## Call Logging

Both mocks maintain a call log for verification:

```pascal
procedure TMyTest.Test_CallsAreLogged;
begin
  FPlaylist.Add('/music/song.mp3');
  FPlaylist.PlayFirst;

  AssertTrue('Add should be logged',
    FPlaylist.GetCallLog.IndexOf('Add(/music/song.mp3)') >= 0);
  AssertTrue('PlayFirst should be logged',
    FPlaylist.GetCallLog.IndexOf('PlayFirst') >= 0);
end;
```

## Best Practices

1. **Use mocks for unit tests** - Avoid external dependencies
2. **Check call logs** - Verify correct method calls
3. **Simulate realistic scenarios** - Use appropriate durations and states
4. **Clean up in TearDown** - Free mock objects properly
5. **Test event handlers** - Assign handlers in SetUp
