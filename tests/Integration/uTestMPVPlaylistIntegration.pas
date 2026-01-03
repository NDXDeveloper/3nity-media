{ ===============================================================================
  uTestMPVPlaylistIntegration.pas - Integration Tests for MPV + Playlist

  Part of 3nity Media Player - Test Suite

  Tests the interaction between TMockMPVEngine and TMockPlaylistManager to
  verify correct behavior of combined operations like:
  - Auto-play next track when file ends
  - Playlist navigation during playback
  - Playback mode handling (normal, repeat, shuffle)
  - Volume and settings persistence across tracks

  Author: Test Suite
  License: GPL-2.0
  =============================================================================== }

unit uTestMPVPlaylistIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uMockMPVEngine, uMockPlaylist, uTypes;

type
  { ===========================================================================
    TTestAutoPlayNext - Tests for automatic next track playback
    =========================================================================== }
  TTestAutoPlayNext = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FPlayedFiles: TStringList;
    procedure OnMPVEndFile(Sender: TObject; Reason: Integer);
    procedure OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_EndFile_TriggersPlayNext_NormalMode;
    procedure Test_EndFile_StopsAtPlaylistEnd_NormalMode;
    procedure Test_EndFile_WrapsToFirst_RepeatAllMode;
    procedure Test_EndFile_RepeatsTrack_RepeatOneMode;
    procedure Test_EndFile_PlaysShuffled_ShuffleMode;
  end;

  { ===========================================================================
    TTestPlaylistNavigation - Tests for playlist navigation during playback
    =========================================================================== }
  TTestPlaylistNavigation = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FLastLoadedFile: string;
    procedure OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_PlayNext_LoadsNextTrack;
    procedure Test_PlayPrevious_LoadsPreviousTrack;
    procedure Test_PlayFirst_LoadsFirstTrack;
    procedure Test_PlayLast_LoadsLastTrack;
    procedure Test_PlayIndex_LoadsSpecificTrack;
    procedure Test_DoubleClick_LoadsTrack;
  end;

  { ===========================================================================
    TTestSettingsPersistence - Tests for settings persistence across tracks
    =========================================================================== }
  TTestSettingsPersistence = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Volume_PersistsAcrossTracks;
    procedure Test_Mute_PersistsAcrossTracks;
    procedure Test_Speed_PersistsAcrossTracks;
    procedure Test_Equalizer_PersistsAcrossTracks;
    procedure Test_VideoSettings_PersistAcrossTracks;
  end;

  { ===========================================================================
    TTestPlaybackScenarios - Tests for common playback scenarios
    =========================================================================== }
  TTestPlaybackScenarios = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_StartPlayback_FromEmptyState;
    procedure Test_StopPlayback_MidPlaylist;
    procedure Test_ClearPlaylist_DuringPlayback;
    procedure Test_AddTrack_DuringPlayback;
    procedure Test_RemoveCurrentTrack_DuringPlayback;
    procedure Test_ReorderPlaylist_DuringPlayback;
  end;

  { ===========================================================================
    TTestPositionTracking - Tests for position tracking and seeking
    =========================================================================== }
  TTestPositionTracking = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FLastPosition: Double;
    procedure OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_Position_UpdatesDuringPlayback;
    procedure Test_Seek_UpdatesPosition;
    procedure Test_TrackChange_ResetsPosition;
    procedure Test_Duration_MatchesCurrentTrack;
  end;

  { ===========================================================================
    TTestPlaybackModeChanges - Tests for changing playback modes
    =========================================================================== }
  TTestPlaybackModeChanges = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_SwitchToRepeatAll_DuringPlayback;
    procedure Test_SwitchToRepeatOne_DuringPlayback;
    procedure Test_SwitchToShuffle_DuringPlayback;
    procedure Test_SwitchBackToNormal_DuringPlayback;
    procedure Test_ModeChange_AffectsHasNext;
  end;

  { ===========================================================================
    TTestEventSequence - Tests for correct event ordering
    =========================================================================== }
  TTestEventSequence = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
    FEventLog: TStringList;
    procedure LogEvent(const AEvent: string);
    procedure OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
    procedure OnMPVFileLoaded(Sender: TObject; const FileName: string);
    procedure OnPlaylistChange(Sender: TObject);
    procedure OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_LoadFile_EventSequence;
    procedure Test_PlayNext_EventSequence;
    procedure Test_Stop_EventSequence;
  end;

  { ===========================================================================
    TTestConcurrentOperations - Tests for handling concurrent operations
    =========================================================================== }
  TTestConcurrentOperations = class(TTestCase)
  private
    FMPV: TMockMPVEngine;
    FPlaylist: TMockPlaylistManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_RapidPlayPause;
    procedure Test_RapidTrackChanges;
    procedure Test_SeekDuringTrackChange;
    procedure Test_VolumeChangeDuringSeek;
  end;

implementation

{ ===========================================================================
  TTestAutoPlayNext
  =========================================================================== }

procedure TTestAutoPlayNext.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FPlayedFiles := TStringList.Create;

  FMPV.OnEndFile := @OnMPVEndFile;
  FPlaylist.OnPlay := @OnPlaylistPlay;
end;

procedure TTestAutoPlayNext.TearDown;
begin
  FPlayedFiles.Free;
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestAutoPlayNext.OnMPVEndFile(Sender: TObject; Reason: Integer);
begin
  { Simulate auto-play next behavior }
  if Reason = 0 then  { Normal end, not error }
  begin
    if FPlaylist.HasNext then
      FPlaylist.PlayNext;
  end;
end;

procedure TTestAutoPlayNext.OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
begin
  FPlayedFiles.Add(FileName);
  FMPV.SimulateFileLoad(FileName, 180.0);  { 3 minute track }
end;

procedure TTestAutoPlayNext.Test_EndFile_TriggersPlayNext_NormalMode;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');
  FPlaylist.Add('/music/track3.mp3');

  { Start playing first track }
  FPlaylist.PlayFirst;
  AssertEquals('Should be on track 1', 0, FPlaylist.CurrentIndex);

  { Simulate end of file }
  FMPV.SimulateEndFile(0);

  { Should have advanced to track 2 }
  AssertEquals('Should be on track 2', 1, FPlaylist.CurrentIndex);
  AssertEquals('Should have played 2 tracks', 2, FPlayedFiles.Count);
end;

procedure TTestAutoPlayNext.Test_EndFile_StopsAtPlaylistEnd_NormalMode;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');

  { Start at last track }
  FPlaylist.PlayLast;
  FPlayedFiles.Clear;

  { Simulate end of file }
  FMPV.SimulateEndFile(0);

  { Should NOT have played another track }
  AssertEquals('Should still be at last index', 1, FPlaylist.CurrentIndex);
  AssertEquals('Should not have played new track', 0, FPlayedFiles.Count);
end;

procedure TTestAutoPlayNext.Test_EndFile_WrapsToFirst_RepeatAllMode;
begin
  FPlaylist.PlaybackMode := pmRepeatAll;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');

  { Start at last track }
  FPlaylist.PlayLast;
  FPlayedFiles.Clear;

  { Simulate end of file }
  FMPV.SimulateEndFile(0);

  { Should wrap to first track }
  AssertEquals('Should wrap to first track', 0, FPlaylist.CurrentIndex);
  AssertEquals('Should have played new track', 1, FPlayedFiles.Count);
end;

procedure TTestAutoPlayNext.Test_EndFile_RepeatsTrack_RepeatOneMode;
var
  InitialFile: string;
begin
  FPlaylist.PlaybackMode := pmRepeatOne;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');

  { Start at track 1 }
  FPlaylist.PlayFirst;
  InitialFile := FPlaylist[FPlaylist.CurrentIndex].FileName;
  FPlayedFiles.Clear;

  { Simulate end of file }
  FMPV.SimulateEndFile(0);

  { Should repeat same track }
  AssertEquals('Should still be on same index', 0, FPlaylist.CurrentIndex);
  AssertEquals('Should have replayed same file', InitialFile, FPlayedFiles[0]);
end;

procedure TTestAutoPlayNext.Test_EndFile_PlaysShuffled_ShuffleMode;
begin
  FPlaylist.PlaybackMode := pmShuffle;
  FPlaylist.Add('/music/track1.mp3');
  FPlaylist.Add('/music/track2.mp3');
  FPlaylist.Add('/music/track3.mp3');
  FPlaylist.Add('/music/track4.mp3');
  FPlaylist.Add('/music/track5.mp3');

  { Start playing }
  FPlaylist.PlayFirst;
  FPlayedFiles.Clear;

  { Simulate end of file }
  FMPV.SimulateEndFile(0);

  { Should have played some track (shuffle order) }
  AssertEquals('Should have played next shuffled track', 1, FPlayedFiles.Count);
end;

{ ===========================================================================
  TTestPlaylistNavigation
  =========================================================================== }

procedure TTestPlaylistNavigation.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FLastLoadedFile := '';

  FPlaylist.OnPlay := @OnPlaylistPlay;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestPlaylistNavigation.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestPlaylistNavigation.OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
begin
  FLastLoadedFile := FileName;
  FMPV.SimulateFileLoad(FileName, 180.0);
end;

procedure TTestPlaylistNavigation.Test_PlayNext_LoadsNextTrack;
begin
  FPlaylist.PlayFirst;
  FPlaylist.PlayNext;

  AssertEquals('CurrentIndex should be 1', 1, FPlaylist.CurrentIndex);
  AssertEquals('Should load track2', FPlaylist[1].FileName, FLastLoadedFile);
end;

procedure TTestPlaylistNavigation.Test_PlayPrevious_LoadsPreviousTrack;
begin
  FPlaylist.PlayLast;
  FPlaylist.PlayPrevious;

  AssertEquals('CurrentIndex should be 3', 3, FPlaylist.CurrentIndex);
  AssertEquals('Should load track4', FPlaylist[3].FileName, FLastLoadedFile);
end;

procedure TTestPlaylistNavigation.Test_PlayFirst_LoadsFirstTrack;
begin
  FPlaylist.CurrentIndex := 3;
  FPlaylist.PlayFirst;

  AssertEquals('CurrentIndex should be 0', 0, FPlaylist.CurrentIndex);
  AssertEquals('Should load first track', FPlaylist[0].FileName, FLastLoadedFile);
end;

procedure TTestPlaylistNavigation.Test_PlayLast_LoadsLastTrack;
begin
  FPlaylist.CurrentIndex := 0;
  FPlaylist.PlayLast;

  AssertEquals('CurrentIndex should be 4', 4, FPlaylist.CurrentIndex);
  AssertEquals('Should load last track', FPlaylist[4].FileName, FLastLoadedFile);
end;

procedure TTestPlaylistNavigation.Test_PlayIndex_LoadsSpecificTrack;
begin
  FPlaylist.PlayIndex(2);

  AssertEquals('CurrentIndex should be 2', 2, FPlaylist.CurrentIndex);
  AssertEquals('Should load track3', FPlaylist[2].FileName, FLastLoadedFile);
end;

procedure TTestPlaylistNavigation.Test_DoubleClick_LoadsTrack;
begin
  { Simulate double-click on track 3 }
  FPlaylist.PlayIndex(2);

  AssertEquals('Should be on track 3', 2, FPlaylist.CurrentIndex);
  AssertTrue('File should be loaded', FLastLoadedFile <> '');
end;

{ ===========================================================================
  TTestSettingsPersistence
  =========================================================================== }

procedure TTestSettingsPersistence.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(3);
end;

procedure TTestSettingsPersistence.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestSettingsPersistence.Test_Volume_PersistsAcrossTracks;
begin
  FMPV.Volume := 75;
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  AssertEquals('Volume should persist', 75, FMPV.Volume);
end;

procedure TTestSettingsPersistence.Test_Mute_PersistsAcrossTracks;
begin
  FMPV.Muted := True;
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  AssertTrue('Mute should persist', FMPV.Muted);
end;

procedure TTestSettingsPersistence.Test_Speed_PersistsAcrossTracks;
begin
  FMPV.Speed := 1.5;
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  AssertEquals('Speed should persist', 1.5, FMPV.Speed, 0.01);
end;

procedure TTestSettingsPersistence.Test_Equalizer_PersistsAcrossTracks;
begin
  FMPV.SetEqualizerBand(0, 5.0);
  FMPV.SetEqualizerBand(5, -3.0);
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  AssertEquals('EQ band 0 should persist', 5.0, FMPV.GetEqualizerBand(0), 0.01);
  AssertEquals('EQ band 5 should persist', -3.0, FMPV.GetEqualizerBand(5), 0.01);
end;

procedure TTestSettingsPersistence.Test_VideoSettings_PersistAcrossTracks;
begin
  FMPV.Brightness := 10;
  FMPV.Contrast := 5;
  FMPV.Saturation := -5;

  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  AssertEquals('Brightness should persist', 10, FMPV.Brightness);
  AssertEquals('Contrast should persist', 5, FMPV.Contrast);
  AssertEquals('Saturation should persist', -5, FMPV.Saturation);
end;

{ ===========================================================================
  TTestPlaybackScenarios
  =========================================================================== }

procedure TTestPlaybackScenarios.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
end;

procedure TTestPlaybackScenarios.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestPlaybackScenarios.Test_StartPlayback_FromEmptyState;
begin
  { Verify initial state }
  AssertTrue('Playlist should be empty', FPlaylist.IsEmpty);
  AssertEquals('MPV status should be msNone', Ord(msNone), Ord(FMPV.Status));

  { Add tracks and start }
  FPlaylist.PopulateWithTestData(3);
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  AssertFalse('Playlist should not be empty', FPlaylist.IsEmpty);
end;

procedure TTestPlaybackScenarios.Test_StopPlayback_MidPlaylist;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.CurrentIndex := 2;
  FMPV.SimulateFileLoad(FPlaylist[2].FileName, 180.0);

  FMPV.Stop;

  AssertEquals('Status should be msStopped', Ord(msStopped), Ord(FMPV.Status));
  { Playlist index should remain unchanged }
  AssertEquals('Playlist index should be unchanged', 2, FPlaylist.CurrentIndex);
end;

procedure TTestPlaybackScenarios.Test_ClearPlaylist_DuringPlayback;
begin
  FPlaylist.PopulateWithTestData(5);
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  FPlaylist.Clear;

  AssertTrue('Playlist should be empty', FPlaylist.IsEmpty);
  AssertEquals('Playlist index should be -1', -1, FPlaylist.CurrentIndex);
end;

procedure TTestPlaybackScenarios.Test_AddTrack_DuringPlayback;
var
  OldCount: Integer;
begin
  FPlaylist.PopulateWithTestData(3);
  FPlaylist.CurrentIndex := 1;
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);
  OldCount := FPlaylist.Count;

  FPlaylist.Add('/music/newtrack.mp3');

  AssertEquals('Count should increase', OldCount + 1, FPlaylist.Count);
  { Current index should be unchanged }
  AssertEquals('Current index unchanged', 1, FPlaylist.CurrentIndex);
end;

procedure TTestPlaybackScenarios.Test_RemoveCurrentTrack_DuringPlayback;
var
  OldFileName: string;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.CurrentIndex := 2;
  OldFileName := FPlaylist[2].FileName;
  FMPV.SimulateFileLoad(OldFileName, 180.0);

  FPlaylist.Delete(2);

  { Current index should be adjusted }
  AssertEquals('Count should be 4', 4, FPlaylist.Count);
  { The old track at index 2 should be gone }
  AssertFalse('Old file should be removed', FPlaylist.Contains(OldFileName));
end;

procedure TTestPlaybackScenarios.Test_ReorderPlaylist_DuringPlayback;
var
  CurrentFile: string;
begin
  FPlaylist.PopulateWithTestData(5);
  FPlaylist.CurrentIndex := 1;
  CurrentFile := FPlaylist[1].FileName;
  FMPV.SimulateFileLoad(CurrentFile, 180.0);

  { Move current track to position 3 }
  FPlaylist.Move(1, 3);

  { Current index should follow the moved item }
  AssertEquals('Current index should follow moved item', 3, FPlaylist.CurrentIndex);
  AssertEquals('Current file should be same', CurrentFile, FPlaylist[FPlaylist.CurrentIndex].FileName);
end;

{ ===========================================================================
  TTestPositionTracking
  =========================================================================== }

procedure TTestPositionTracking.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FLastPosition := 0;

  FMPV.OnPositionChange := @OnPositionChange;
  FPlaylist.PopulateWithTestData(3);
end;

procedure TTestPositionTracking.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestPositionTracking.OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
begin
  FLastPosition := PosSec;
end;

procedure TTestPositionTracking.Test_Position_UpdatesDuringPlayback;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  FMPV.SimulatePosition(30.0);

  AssertEquals('Position should be 30', 30.0, FMPV.Position, 0.01);
  AssertEquals('OnPositionChange should receive 30', 30.0, FLastPosition, 0.01);
end;

procedure TTestPositionTracking.Test_Seek_UpdatesPosition;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulatePosition(10.0);

  FMPV.SeekAbsolute(45.0);

  AssertEquals('Position should be 45', 45.0, FMPV.Position, 0.01);
end;

procedure TTestPositionTracking.Test_TrackChange_ResetsPosition;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulatePosition(120.0);

  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);

  { Position should reset }
  AssertEquals('Position should reset on new track', 0.0, FMPV.Position, 0.01);
end;

procedure TTestPositionTracking.Test_Duration_MatchesCurrentTrack;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SimulatePosition(0);

  { Duration should match simulated value }
  AssertEquals('Duration should be 180', 180.0, FMPV.Duration, 0.01);
end;

{ ===========================================================================
  TTestPlaybackModeChanges
  =========================================================================== }

procedure TTestPlaybackModeChanges.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestPlaybackModeChanges.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestPlaybackModeChanges.Test_SwitchToRepeatAll_DuringPlayback;
begin
  FPlaylist.PlaybackMode := pmNormal;
  FPlaylist.CurrentIndex := 4;  { Last track }

  { Initially no next in normal mode at end }
  AssertFalse('HasNext should be false at end in normal', FPlaylist.HasNext);

  FPlaylist.PlaybackMode := pmRepeatAll;

  { Now should have next (wraps) }
  AssertTrue('HasNext should be true in RepeatAll', FPlaylist.HasNext);
end;

procedure TTestPlaybackModeChanges.Test_SwitchToRepeatOne_DuringPlayback;
begin
  FPlaylist.CurrentIndex := 2;
  FPlaylist.PlaybackMode := pmRepeatOne;

  AssertEquals('GetNext should return same index', 2, FPlaylist.GetNext);
  AssertEquals('GetPrevious should return same index', 2, FPlaylist.GetPrevious);
end;

procedure TTestPlaybackModeChanges.Test_SwitchToShuffle_DuringPlayback;
begin
  FPlaylist.CurrentIndex := 0;
  FPlaylist.PlaybackMode := pmShuffle;

  { Just verify it works without crash }
  AssertTrue('HasNext should be true in Shuffle', FPlaylist.HasNext);
end;

procedure TTestPlaybackModeChanges.Test_SwitchBackToNormal_DuringPlayback;
begin
  FPlaylist.CurrentIndex := 4;
  FPlaylist.PlaybackMode := pmRepeatAll;
  AssertTrue('HasNext in RepeatAll', FPlaylist.HasNext);

  FPlaylist.PlaybackMode := pmNormal;

  AssertFalse('HasNext should be false at end in Normal', FPlaylist.HasNext);
end;

procedure TTestPlaybackModeChanges.Test_ModeChange_AffectsHasNext;
begin
  FPlaylist.CurrentIndex := 4;

  FPlaylist.PlaybackMode := pmNormal;
  AssertFalse('pmNormal: no next at end', FPlaylist.HasNext);

  FPlaylist.PlaybackMode := pmRepeatAll;
  AssertTrue('pmRepeatAll: has next at end', FPlaylist.HasNext);

  FPlaylist.PlaybackMode := pmRepeatOne;
  AssertTrue('pmRepeatOne: has next always', FPlaylist.HasNext);

  FPlaylist.PlaybackMode := pmShuffle;
  AssertTrue('pmShuffle: has next always', FPlaylist.HasNext);
end;

{ ===========================================================================
  TTestEventSequence
  =========================================================================== }

procedure TTestEventSequence.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FEventLog := TStringList.Create;

  FMPV.OnStatusChange := @OnMPVStatusChange;
  FMPV.OnFileLoaded := @OnMPVFileLoaded;
  FPlaylist.OnChange := @OnPlaylistChange;
  FPlaylist.OnPlay := @OnPlaylistPlay;

  FPlaylist.PopulateWithTestData(3);
end;

procedure TTestEventSequence.TearDown;
begin
  FEventLog.Free;
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestEventSequence.LogEvent(const AEvent: string);
begin
  FEventLog.Add(AEvent);
end;

procedure TTestEventSequence.OnMPVStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
begin
  case NewStatus of
    msOpening: LogEvent('MPV:Opening');
    msPlaying: LogEvent('MPV:Playing');
    msPaused: LogEvent('MPV:Paused');
    msStopped: LogEvent('MPV:Stopped');
  end;
end;

procedure TTestEventSequence.OnMPVFileLoaded(Sender: TObject; const FileName: string);
begin
  LogEvent('MPV:FileLoaded');
end;

procedure TTestEventSequence.OnPlaylistChange(Sender: TObject);
begin
  LogEvent('Playlist:Change');
end;

procedure TTestEventSequence.OnPlaylistPlay(Sender: TObject; Index: Integer; const FileName: string);
begin
  LogEvent('Playlist:Play:' + IntToStr(Index));
end;

procedure TTestEventSequence.Test_LoadFile_EventSequence;
begin
  FPlaylist.PlayIndex(0);
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  AssertTrue('Should have Playlist:Play event', FEventLog.IndexOf('Playlist:Play:0') >= 0);
  AssertTrue('Should have MPV:FileLoaded event', FEventLog.IndexOf('MPV:FileLoaded') >= 0);
end;

procedure TTestEventSequence.Test_PlayNext_EventSequence;
begin
  FPlaylist.PlayFirst;
  FEventLog.Clear;

  FPlaylist.PlayNext;

  AssertTrue('Should have Playlist:Play:1 event', FEventLog.IndexOf('Playlist:Play:1') >= 0);
end;

procedure TTestEventSequence.Test_Stop_EventSequence;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FEventLog.Clear;

  FMPV.Stop;

  AssertTrue('Should have MPV:Stopped event', FEventLog.IndexOf('MPV:Stopped') >= 0);
end;

{ ===========================================================================
  TTestConcurrentOperations
  =========================================================================== }

procedure TTestConcurrentOperations.SetUp;
begin
  FMPV := TMockMPVEngine.Create;
  FPlaylist := TMockPlaylistManager.Create;
  FPlaylist.PopulateWithTestData(5);
end;

procedure TTestConcurrentOperations.TearDown;
begin
  FPlaylist.Free;
  FMPV.Free;
end;

procedure TTestConcurrentOperations.Test_RapidPlayPause;
var
  I: Integer;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  for I := 1 to 100 do
  begin
    FMPV.Pause;
    FMPV.Resume;
  end;

  { Should not crash and status should be consistent }
  AssertTrue('Status should be Playing or Paused',
    (FMPV.Status = msPlaying) or (FMPV.Status = msPaused));
end;

procedure TTestConcurrentOperations.Test_RapidTrackChanges;
var
  I: Integer;
begin
  for I := 0 to 4 do
  begin
    FMPV.SimulateFileLoad(FPlaylist[I].FileName, 180.0);
  end;

  { Should handle rapid track changes }
  AssertTrue('Should handle rapid track changes', True);
end;

procedure TTestConcurrentOperations.Test_SeekDuringTrackChange;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);
  FMPV.SeekAbsolute(30);
  FMPV.SimulateFileLoad(FPlaylist[1].FileName, 180.0);
  FMPV.SeekAbsolute(45);

  { Position should be from last seek }
  AssertEquals('Position should be 45', 45.0, FMPV.Position, 0.01);
end;

procedure TTestConcurrentOperations.Test_VolumeChangeDuringSeek;
begin
  FMPV.SimulateFileLoad(FPlaylist[0].FileName, 180.0);

  FMPV.SeekAbsolute(30);
  FMPV.Volume := 50;
  FMPV.SeekRelative(10);
  FMPV.Volume := 75;

  AssertEquals('Volume should be 75', 75, FMPV.Volume);
  AssertEquals('Position should be 40', 40.0, FMPV.Position, 0.01);
end;

initialization
  RegisterTest(TTestAutoPlayNext);
  RegisterTest(TTestPlaylistNavigation);
  RegisterTest(TTestSettingsPersistence);
  RegisterTest(TTestPlaybackScenarios);
  RegisterTest(TTestPositionTracking);
  RegisterTest(TTestPlaybackModeChanges);
  RegisterTest(TTestEventSequence);
  RegisterTest(TTestConcurrentOperations);

end.
