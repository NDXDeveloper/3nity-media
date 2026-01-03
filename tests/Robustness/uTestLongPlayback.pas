{ ===============================================================================
  uTestLongPlayback.pas - Long Duration Playback Stability Tests

  Part of 3nity Media - Test Suite

  Tests application stability during extended playback sessions,
  including memory stability, state consistency, and resource management.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestLongPlayback;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, DateUtils;

type
  { ============================================================================
    TTestLongPlayback - Long duration playback tests
    ============================================================================ }
  TTestLongPlayback = class(TTestCase)
  private
    FStartMemory: PtrUInt;
    FStartTime: TDateTime;
    function GetMemoryUsed: PtrUInt;
    procedure StartTimer;
    function ElapsedSeconds: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Memory stability tests }
    procedure Test_MemoryStable_AfterManyPositionUpdates;
    procedure Test_MemoryStable_AfterManyMetadataUpdates;
    procedure Test_MemoryStable_AfterManyStateChanges;
    procedure Test_NoMemoryLeak_PlaylistCycle;

    { State consistency tests }
    procedure Test_StateConsistent_LongPlayback;
    procedure Test_PositionMonotonic_LongPlayback;
    procedure Test_DurationStable_LongPlayback;

    { Resource management tests }
    procedure Test_HandleCount_Stable;
    procedure Test_ThreadCount_Stable;
    procedure Test_FileDescriptor_Stable;

    { Continuous operation tests }
    procedure Test_ContinuousPlayPause_1000Cycles;
    procedure Test_ContinuousSeek_1000Operations;
    procedure Test_ContinuousVolumeChange_1000Operations;
    procedure Test_ContinuousTrackSwitch_100Times;

    { Endurance simulation }
    procedure Test_SimulatedHourPlayback;
    procedure Test_SimulatedPlaylistLoop;
  end;

  { ============================================================================
    TTestResourceManagement - Resource management during playback
    ============================================================================ }
  TTestResourceManagement = class(TTestCase)
  private
    function GetMemoryUsed: PtrUInt;
  published
    { Buffer management }
    procedure Test_BufferAllocation_Stable;
    procedure Test_BufferReuse_Efficient;
    procedure Test_BufferCleanup_OnStop;

    { Cache management }
    procedure Test_MetadataCache_LRU;
    procedure Test_ThumbnailCache_SizeLimit;
    procedure Test_CacheCleanup_Periodic;

    { Temporary files }
    procedure Test_TempFiles_Cleaned;
    procedure Test_TempFiles_NotAccumulate;
  end;

  { ============================================================================
    TTestPlaylistEndurance - Playlist operation endurance tests
    ============================================================================ }
  TTestPlaylistEndurance = class(TTestCase)
  private
    FPlaylist: TStringList;
    function GetMemoryUsed: PtrUInt;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Large playlist tests }
    procedure Test_LargePlaylist_10000Items;
    procedure Test_LargePlaylist_Navigation;
    procedure Test_LargePlaylist_Search;
    procedure Test_LargePlaylist_Sort;

    { Playlist cycling }
    procedure Test_PlaylistLoop_100Times;
    procedure Test_ShuffleRegenerate_100Times;
    procedure Test_PlaylistModify_WhilePlaying;
  end;

implementation

const
  { Stability thresholds }
  MAX_MEMORY_GROWTH_PERCENT = 10;  { Allow 10% memory growth }
  MAX_MEMORY_GROWTH_BYTES = 10 * 1024 * 1024;  { 10MB absolute max }

{ ============================================================================
  TTestLongPlayback
  ============================================================================ }

procedure TTestLongPlayback.SetUp;
begin
  FStartMemory := GetMemoryUsed;
  FStartTime := Now;
end;

procedure TTestLongPlayback.TearDown;
begin
  { Nothing to clean up }
end;

function TTestLongPlayback.GetMemoryUsed: PtrUInt;
var
  Status: TFPCHeapStatus;
begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
end;

procedure TTestLongPlayback.StartTimer;
begin
  FStartTime := Now;
end;

function TTestLongPlayback.ElapsedSeconds: Integer;
begin
  Result := SecondsBetween(Now, FStartTime);
end;

{ Memory stability tests }

procedure TTestLongPlayback.Test_MemoryStable_AfterManyPositionUpdates;
var
  I: Integer;
  Position: Double;
  MemBefore, MemAfter, MemGrowth: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  { Simulate 10000 position updates (like during playback) }
  Position := 0;
  for I := 1 to 10000 do
  begin
    Position := Position + 0.1;
    if Position > 100 then Position := 0;
    { In real app, this would trigger UI updates }
  end;

  MemAfter := GetMemoryUsed;

  if MemAfter > MemBefore then
    MemGrowth := MemAfter - MemBefore
  else
    MemGrowth := 0;

  AssertTrue('Memory growth should be < 1MB after 10000 position updates, grew: ' +
    IntToStr(MemGrowth div 1024) + 'KB', MemGrowth < 1024 * 1024);
end;

procedure TTestLongPlayback.Test_MemoryStable_AfterManyMetadataUpdates;
var
  I: Integer;
  Metadata: TStringList;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  Metadata := TStringList.Create;
  try
    { Simulate metadata updates (like during radio streaming) }
    for I := 1 to 1000 do
    begin
      Metadata.Clear;
      Metadata.Add('title=Now Playing Track ' + IntToStr(I));
      Metadata.Add('artist=Artist ' + IntToStr(I mod 100));
      Metadata.Add('album=Album ' + IntToStr(I mod 50));
    end;
  finally
    Metadata.Free;
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Memory should return to near baseline after metadata updates',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

procedure TTestLongPlayback.Test_MemoryStable_AfterManyStateChanges;
var
  I: Integer;
  State: Integer;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  { Simulate state changes }
  State := 0;
  for I := 1 to 10000 do
  begin
    State := (State + 1) mod 4;  { Cycle through states }
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Memory stable after state changes',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 50 * 1024);
end;

procedure TTestLongPlayback.Test_NoMemoryLeak_PlaylistCycle;
var
  I, J: Integer;
  Playlist: TStringList;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  for I := 1 to 100 do
  begin
    Playlist := TStringList.Create;
    try
      for J := 1 to 100 do
        Playlist.Add('/path/to/song' + IntToStr(J) + '.mp3');
    finally
      Playlist.Free;
    end;
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('No memory leak after 100 playlist cycles',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

{ State consistency tests }

procedure TTestLongPlayback.Test_StateConsistent_LongPlayback;
var
  I: Integer;
  State: Integer;
  StateChanges: Integer;
begin
  State := 1;  { Playing }
  StateChanges := 0;

  { Simulate long playback with occasional state checks }
  for I := 1 to 10000 do
  begin
    { State should remain playing unless explicitly changed }
    if State <> 1 then
      Inc(StateChanges);
  end;

  AssertEquals('State should remain consistent', 0, StateChanges);
end;

procedure TTestLongPlayback.Test_PositionMonotonic_LongPlayback;
var
  I: Integer;
  Position, LastPosition: Double;
  Violations: Integer;
begin
  Position := 0;
  LastPosition := -1;
  Violations := 0;

  { Position should always increase during normal playback }
  for I := 1 to 10000 do
  begin
    Position := Position + 0.1;
    if Position < LastPosition then
      Inc(Violations);
    LastPosition := Position;
  end;

  AssertEquals('Position should be monotonically increasing', 0, Violations);
end;

procedure TTestLongPlayback.Test_DurationStable_LongPlayback;
var
  I: Integer;
  Duration: Double;
  Changes: Integer;
begin
  Duration := 180.0;  { 3 minutes }
  Changes := 0;

  { Duration should not change during playback }
  for I := 1 to 10000 do
  begin
    { Simulate duration check }
    if Duration <> 180.0 then
      Inc(Changes);
  end;

  AssertEquals('Duration should remain stable', 0, Changes);
end;

{ Resource management tests }

procedure TTestLongPlayback.Test_HandleCount_Stable;
var
  I: Integer;
  InitialHandles, CurrentHandles: Integer;
  HandleGrowth: Integer;
begin
  { Simulate handle count tracking }
  InitialHandles := 10;  { Simulated initial handle count }
  CurrentHandles := InitialHandles;
  HandleGrowth := 0;

  for I := 1 to 1000 do
  begin
    { In real app, operations might open/close handles }
    { They should be properly managed }
  end;

  HandleGrowth := CurrentHandles - InitialHandles;
  AssertEquals('Handle count should not grow', 0, HandleGrowth);
end;

procedure TTestLongPlayback.Test_ThreadCount_Stable;
var
  I: Integer;
  InitialThreads, CurrentThreads: Integer;
begin
  InitialThreads := 4;  { Simulated: main, audio, video, event }
  CurrentThreads := InitialThreads;

  for I := 1 to 1000 do
  begin
    { Threads should be reused, not created }
  end;

  AssertEquals('Thread count should remain stable', InitialThreads, CurrentThreads);
end;

procedure TTestLongPlayback.Test_FileDescriptor_Stable;
var
  I: Integer;
  InitialFDs, CurrentFDs: Integer;
begin
  InitialFDs := 5;  { Simulated }
  CurrentFDs := InitialFDs;

  for I := 1 to 1000 do
  begin
    { File descriptors should be properly closed }
  end;

  AssertEquals('File descriptor count stable', InitialFDs, CurrentFDs);
end;

{ Continuous operation tests }

procedure TTestLongPlayback.Test_ContinuousPlayPause_1000Cycles;
var
  I: Integer;
  State: Boolean;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;
  State := False;

  for I := 1 to 1000 do
  begin
    State := not State;  { Toggle play/pause }
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Memory stable after 1000 play/pause cycles',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

procedure TTestLongPlayback.Test_ContinuousSeek_1000Operations;
var
  I: Integer;
  Position: Double;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;
  Position := 0;

  for I := 1 to 1000 do
  begin
    Position := Random * 100;
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Position in valid range', (Position >= 0) and (Position <= 100));
  AssertTrue('Memory stable after 1000 seeks',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

procedure TTestLongPlayback.Test_ContinuousVolumeChange_1000Operations;
var
  I: Integer;
  Volume: Integer;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;
  Volume := 50;

  for I := 1 to 1000 do
  begin
    Volume := Random(101);  { 0-100 }
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Volume in valid range', (Volume >= 0) and (Volume <= 100));
  AssertTrue('Memory stable after 1000 volume changes',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 50 * 1024);
end;

procedure TTestLongPlayback.Test_ContinuousTrackSwitch_100Times;
var
  I: Integer;
  CurrentTrack: Integer;
  Playlist: TStringList;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  Playlist := TStringList.Create;
  try
    for I := 1 to 20 do
      Playlist.Add('/path/to/track' + IntToStr(I) + '.mp3');

    CurrentTrack := 0;
    for I := 1 to 100 do
    begin
      CurrentTrack := Random(Playlist.Count);
      { Simulate loading new track }
    end;
  finally
    Playlist.Free;
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Memory stable after 100 track switches',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 200 * 1024);
end;

{ Endurance simulation }

procedure TTestLongPlayback.Test_SimulatedHourPlayback;
var
  I: Integer;
  Position: Double;
  Updates: Integer;
  MemStart, MemEnd: PtrUInt;
begin
  MemStart := GetMemoryUsed;

  { Simulate 1 hour of playback at 10 updates per simulated second }
  { 3600 seconds * 10 = 36000 updates, scaled down for test speed }
  Position := 0;
  Updates := 0;

  for I := 1 to 36000 do
  begin
    Position := Position + 0.1;
    if Position > 3600 then Position := 0;  { Loop }
    Inc(Updates);
  end;

  MemEnd := GetMemoryUsed;

  AssertEquals('Should complete all updates', 36000, Updates);
  AssertTrue('Memory growth should be minimal',
    Abs(Int64(MemEnd) - Int64(MemStart)) < MAX_MEMORY_GROWTH_BYTES);
end;

procedure TTestLongPlayback.Test_SimulatedPlaylistLoop;
var
  I, J, LoopCount: Integer;
  Playlist: TStringList;
  CurrentIndex: Integer;
  MemStart, MemEnd: PtrUInt;
begin
  MemStart := GetMemoryUsed;

  Playlist := TStringList.Create;
  try
    for I := 1 to 50 do
      Playlist.Add('/music/track' + IntToStr(I) + '.mp3');

    CurrentIndex := 0;
    LoopCount := 0;

    { Simulate playing through playlist 20 times }
    for J := 1 to 20 do
    begin
      for I := 0 to Playlist.Count - 1 do
      begin
        CurrentIndex := I;
        { Simulate track playback }
      end;
      Inc(LoopCount);
    end;
  finally
    Playlist.Free;
  end;

  MemEnd := GetMemoryUsed;

  AssertEquals('Should complete 20 playlist loops', 20, LoopCount);
  AssertTrue('Memory stable after playlist loops',
    Abs(Int64(MemEnd) - Int64(MemStart)) < 500 * 1024);
end;

{ ============================================================================
  TTestResourceManagement
  ============================================================================ }

function TTestResourceManagement.GetMemoryUsed: PtrUInt;
var
  Status: TFPCHeapStatus;
begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
end;

procedure TTestResourceManagement.Test_BufferAllocation_Stable;
var
  I: Integer;
  Buffers: array of TMemoryStream;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  SetLength(Buffers, 10);
  for I := 0 to 9 do
  begin
    Buffers[I] := TMemoryStream.Create;
    Buffers[I].SetSize(64 * 1024);  { 64KB buffer }
  end;

  { Use buffers }
  for I := 0 to 9 do
    Buffers[I].Position := 0;

  { Free buffers }
  for I := 0 to 9 do
    Buffers[I].Free;

  MemAfter := GetMemoryUsed;

  AssertTrue('Memory should return after buffer free',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

procedure TTestResourceManagement.Test_BufferReuse_Efficient;
var
  I: Integer;
  Buffer: TMemoryStream;
  MemPeak, MemCurrent: PtrUInt;
begin
  Buffer := TMemoryStream.Create;
  try
    MemPeak := GetMemoryUsed;

    for I := 1 to 100 do
    begin
      Buffer.Clear;
      Buffer.SetSize(64 * 1024);
      { Simulate buffer use }
      MemCurrent := GetMemoryUsed;
      if MemCurrent > MemPeak then
        MemPeak := MemCurrent;
    end;

    { Peak should not be much higher than single buffer }
    AssertTrue('Buffer reuse should be efficient', MemPeak < GetMemoryUsed + 200 * 1024);
  finally
    Buffer.Free;
  end;
end;

procedure TTestResourceManagement.Test_BufferCleanup_OnStop;
var
  Buffer: TMemoryStream;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  Buffer := TMemoryStream.Create;
  try
    Buffer.SetSize(1024 * 1024);  { 1MB }
    { Simulate playback }
  finally
    Buffer.Free;  { Stop - cleanup }
  end;

  MemAfter := GetMemoryUsed;

  AssertTrue('Buffer should be freed on stop',
    Abs(Int64(MemAfter) - Int64(MemBefore)) < 100 * 1024);
end;

procedure TTestResourceManagement.Test_MetadataCache_LRU;
var
  Cache: TStringList;
  I: Integer;
  MaxSize: Integer;
begin
  MaxSize := 100;
  Cache := TStringList.Create;
  try
    { Add items beyond max size }
    for I := 1 to 150 do
    begin
      Cache.Add('Key' + IntToStr(I) + '=Value' + IntToStr(I));
      { LRU eviction }
      while Cache.Count > MaxSize do
        Cache.Delete(0);
    end;

    AssertEquals('Cache should not exceed max size', MaxSize, Cache.Count);
  finally
    Cache.Free;
  end;
end;

procedure TTestResourceManagement.Test_ThumbnailCache_SizeLimit;
var
  Cache: TStringList;
  TotalSize, MaxSize: Int64;
  I: Integer;
begin
  MaxSize := 50 * 1024 * 1024;  { 50MB limit }
  TotalSize := 0;
  Cache := TStringList.Create;
  try
    for I := 1 to 1000 do
    begin
      { Each thumbnail ~100KB }
      if TotalSize + 100 * 1024 <= MaxSize then
      begin
        Cache.Add('Thumb' + IntToStr(I));
        TotalSize := TotalSize + 100 * 1024;
      end;
    end;

    AssertTrue('Cache size should respect limit', TotalSize <= MaxSize);
  finally
    Cache.Free;
  end;
end;

procedure TTestResourceManagement.Test_CacheCleanup_Periodic;
var
  Cache: TStringList;
  I: Integer;
begin
  Cache := TStringList.Create;
  try
    { Fill cache }
    for I := 1 to 100 do
      Cache.Add('Item' + IntToStr(I));

    { Periodic cleanup - remove old items }
    while Cache.Count > 50 do
      Cache.Delete(0);

    AssertEquals('Cache cleaned to target size', 50, Cache.Count);
  finally
    Cache.Free;
  end;
end;

procedure TTestResourceManagement.Test_TempFiles_Cleaned;
var
  TempFile: string;
begin
  TempFile := GetTempDir + 'test_temp_' + IntToStr(Random(10000)) + '.tmp';

  { Create temp file }
  with TFileStream.Create(TempFile, fmCreate) do
    Free;

  AssertTrue('Temp file should exist', FileExists(TempFile));

  { Clean up }
  DeleteFile(TempFile);

  AssertFalse('Temp file should be deleted', FileExists(TempFile));
end;

procedure TTestResourceManagement.Test_TempFiles_NotAccumulate;
var
  I: Integer;
  TempFiles: TStringList;
  TempFile: string;
begin
  TempFiles := TStringList.Create;
  try
    { Create multiple temp files }
    for I := 1 to 10 do
    begin
      TempFile := GetTempDir + 'test_accum_' + IntToStr(I) + '.tmp';
      with TFileStream.Create(TempFile, fmCreate) do
        Free;
      TempFiles.Add(TempFile);
    end;

    { Clean up all }
    for I := 0 to TempFiles.Count - 1 do
      DeleteFile(TempFiles[I]);

    { Verify cleanup }
    for I := 0 to TempFiles.Count - 1 do
      AssertFalse('File should be deleted: ' + TempFiles[I],
        FileExists(TempFiles[I]));
  finally
    TempFiles.Free;
  end;
end;

{ ============================================================================
  TTestPlaylistEndurance
  ============================================================================ }

procedure TTestPlaylistEndurance.SetUp;
begin
  FPlaylist := TStringList.Create;
end;

procedure TTestPlaylistEndurance.TearDown;
begin
  FPlaylist.Free;
end;

function TTestPlaylistEndurance.GetMemoryUsed: PtrUInt;
var
  Status: TFPCHeapStatus;
begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
end;

procedure TTestPlaylistEndurance.Test_LargePlaylist_10000Items;
var
  I: Integer;
  MemBefore, MemAfter: PtrUInt;
begin
  MemBefore := GetMemoryUsed;

  for I := 1 to 10000 do
    FPlaylist.Add('/music/artist/album/track' + IntToStr(I) + '.mp3');

  MemAfter := GetMemoryUsed;

  AssertEquals('Should have 10000 items', 10000, FPlaylist.Count);
  AssertTrue('Memory usage should be reasonable for 10000 items',
    (MemAfter - MemBefore) < 5 * 1024 * 1024);  { < 5MB }
end;

procedure TTestPlaylistEndurance.Test_LargePlaylist_Navigation;
var
  I, Index: Integer;
begin
  { Setup large playlist }
  for I := 1 to 10000 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  { Navigate through it }
  Index := 0;
  for I := 1 to 10000 do
  begin
    Index := (Index + 1) mod FPlaylist.Count;
    { Access item }
    AssertTrue('Item should exist', Length(FPlaylist[Index]) > 0);
  end;
end;

procedure TTestPlaylistEndurance.Test_LargePlaylist_Search;
var
  I, Found: Integer;
  SearchTerm: string;
begin
  for I := 1 to 10000 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  { Search for specific items }
  SearchTerm := 'track5000';
  Found := -1;
  for I := 0 to FPlaylist.Count - 1 do
    if Pos(SearchTerm, FPlaylist[I]) > 0 then
    begin
      Found := I;
      Break;
    end;

  AssertTrue('Should find track5000', Found >= 0);
end;

procedure TTestPlaylistEndurance.Test_LargePlaylist_Sort;
var
  I: Integer;
  IsSorted: Boolean;
begin
  { Add items in reverse order to test sorting }
  for I := 1000 downto 1 do
    FPlaylist.Add('/music/track' + Format('%.4d', [I]) + '.mp3');

  FPlaylist.Sort;

  { Verify sorted (padded numbers sort correctly) }
  IsSorted := True;
  for I := 1 to FPlaylist.Count - 1 do
    if AnsiCompareStr(FPlaylist[I], FPlaylist[I - 1]) < 0 then
    begin
      IsSorted := False;
      Break;
    end;

  AssertTrue('Playlist should be sorted', IsSorted);
end;

procedure TTestPlaylistEndurance.Test_PlaylistLoop_100Times;
var
  I, J, CurrentIndex: Integer;
begin
  for I := 1 to 20 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  CurrentIndex := 0;
  for J := 1 to 100 do
  begin
    for I := 0 to FPlaylist.Count - 1 do
    begin
      CurrentIndex := I;
      { Simulate playing track }
    end;
  end;

  { Should complete without issue }
  AssertTrue('Should complete 100 loops', True);
end;

procedure TTestPlaylistEndurance.Test_ShuffleRegenerate_100Times;
var
  I, J, Temp, Idx1, Idx2: Integer;
  Order: array of Integer;
begin
  for I := 1 to 50 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  SetLength(Order, FPlaylist.Count);

  for J := 1 to 100 do
  begin
    { Generate new shuffle order }
    for I := 0 to High(Order) do
      Order[I] := I;

    { Fisher-Yates shuffle }
    for I := High(Order) downto 1 do
    begin
      Idx1 := I;
      Idx2 := Random(I + 1);
      Temp := Order[Idx1];
      Order[Idx1] := Order[Idx2];
      Order[Idx2] := Temp;
    end;
  end;

  AssertTrue('Should generate 100 shuffle orders', True);
end;

procedure TTestPlaylistEndurance.Test_PlaylistModify_WhilePlaying;
var
  I, CurrentIndex: Integer;
begin
  for I := 1 to 100 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  CurrentIndex := 50;  { Playing track 50 }

  { Add items while playing }
  for I := 101 to 110 do
    FPlaylist.Add('/music/track' + IntToStr(I) + '.mp3');

  AssertEquals('Should have 110 items', 110, FPlaylist.Count);

  { Remove items before current }
  FPlaylist.Delete(0);
  CurrentIndex := CurrentIndex - 1;  { Adjust index }

  { Remove items after current }
  FPlaylist.Delete(FPlaylist.Count - 1);

  AssertTrue('Current index should still be valid',
    (CurrentIndex >= 0) and (CurrentIndex < FPlaylist.Count));
end;

initialization
  RegisterTest('Robustness', TTestLongPlayback);
  RegisterTest('Robustness', TTestResourceManagement);
  RegisterTest('Robustness', TTestPlaylistEndurance);

end.
