{ ===============================================================================
  uTestMockMPVBehavior.pas - MPV Engine Behavior Tests using Mock

  Part of 3nity Media - Test Suite

  Tests MPV engine behavior (play, pause, seek, etc.) using TMockMPVEngine.
  These tests validate playback logic that would be impossible to test
  without libmpv loaded.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  =============================================================================== }

unit uTestMockMPVBehavior;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  uMockMPVEngine;

type
  { ===========================================================================
    TTestMPVPlayback - Playback control tests
    =========================================================================== }
  TTestMPVPlayback = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
    FStatusChanges: array of TMPVStatus;
    FPositionChanges: array of Double;
    FFileLoadedCount: Integer;
    FEndFileCount: Integer;
    procedure OnStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
    procedure OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
    procedure OnFileLoaded(Sender: TObject; const FileName: string);
    procedure OnEndFile(Sender: TObject; Reason: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Initialization tests }
    procedure Test_Create_StatusIsNone;
    procedure Test_Initialize_SetsInitialized;
    procedure Test_Initialize_StatusStaysNone;
    procedure Test_Shutdown_ClearsState;

    { PlayMedia tests }
    procedure Test_PlayMedia_ChangesStatusToPlaying;
    procedure Test_PlayMedia_StoresMediaFile;
    procedure Test_PlayMedia_FiresFileLoadedEvent;
    procedure Test_PlayMedia_RequiresInitialization;
    procedure Test_PlayMedia_LogsCall;

    { Pause/Resume tests }
    procedure Test_Pause_ChangesStatusToPaused;
    procedure Test_Pause_OnlyWorksWhenPlaying;
    procedure Test_Resume_ChangesStatusToPlaying;
    procedure Test_Resume_OnlyWorksWhenPaused;
    procedure Test_TogglePause_TogglesState;
    procedure Test_TogglePause_MultipleTimes;

    { Stop tests }
    procedure Test_Stop_ChangesStatusToStopped;
    procedure Test_Stop_ResetsPosition;

    { CloseMedia tests }
    procedure Test_CloseMedia_ClearsMediaFile;
    procedure Test_CloseMedia_ChangesStatusToNone;
    procedure Test_CloseMedia_ClearsTracks;

    { Event tests }
    procedure Test_StatusChange_FiresEvent;
    procedure Test_StatusChange_MultipleTransitions;
  end;

  { ===========================================================================
    TTestMPVSeeking - Seeking tests
    =========================================================================== }
  TTestMPVSeeking = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
    FSeekCount: Integer;
    FLastPosition: Double;
    procedure OnSeek(Sender: TObject);
    procedure OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Absolute seeking }
    procedure Test_SeekAbsolute_SetsPosition;
    procedure Test_SeekAbsolute_ClampsToZero;
    procedure Test_SeekAbsolute_ClampsToDuration;
    procedure Test_SeekAbsolute_FiresEvent;
    procedure Test_SeekAbsolute_LogsCall;

    { Relative seeking }
    procedure Test_SeekRelative_Forward;
    procedure Test_SeekRelative_Backward;
    procedure Test_SeekRelative_ClampsAtBoundaries;

    { Percent seeking }
    procedure Test_SeekPercent_SetsCorrectPosition;
    procedure Test_SeekPercent_AtZero;
    procedure Test_SeekPercent_AtHundred;
    procedure Test_SeekPercent_AtFifty;

    { Position updates }
    procedure Test_SimulatePosition_UpdatesPosition;
    procedure Test_SimulatePosition_CalculatesPercent;
    procedure Test_SimulatePosition_FiresEvent;
  end;

  { ===========================================================================
    TTestMPVVolume - Volume and audio tests
    =========================================================================== }
  TTestMPVVolume = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Volume tests }
    procedure Test_Volume_DefaultIs100;
    procedure Test_Volume_SetValue;
    procedure Test_Volume_ClampsMin;
    procedure Test_Volume_ClampsMax;
    procedure Test_Volume_LogsCall;

    { Mute tests }
    procedure Test_Muted_DefaultIsFalse;
    procedure Test_Muted_SetTrue;
    procedure Test_Muted_SetFalse;
    procedure Test_Muted_LogsCall;

    { Speed tests }
    procedure Test_Speed_DefaultIsOne;
    procedure Test_Speed_SetValue;
    procedure Test_Speed_ClampsMin;
    procedure Test_Speed_ClampsMax;

    { Audio delay tests }
    procedure Test_AudioDelay_DefaultIsZero;
    procedure Test_AudioDelay_SetPositive;
    procedure Test_AudioDelay_SetNegative;
  end;

  { ===========================================================================
    TTestMPVVideo - Video settings tests
    =========================================================================== }
  TTestMPVVideo = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Brightness tests }
    procedure Test_Brightness_DefaultIsZero;
    procedure Test_Brightness_SetValue;
    procedure Test_Brightness_ClampsMin;
    procedure Test_Brightness_ClampsMax;

    { Contrast tests }
    procedure Test_Contrast_DefaultIsZero;
    procedure Test_Contrast_SetValue;
    procedure Test_Contrast_Range;

    { Saturation tests }
    procedure Test_Saturation_DefaultIsZero;
    procedure Test_Saturation_SetValue;
    procedure Test_Saturation_Range;

    { Hue tests }
    procedure Test_Hue_DefaultIsZero;
    procedure Test_Hue_SetValue;

    { Gamma tests }
    procedure Test_Gamma_DefaultIsZero;
    procedure Test_Gamma_SetValue;
  end;

  { ===========================================================================
    TTestMPVEqualizer - Equalizer tests
    =========================================================================== }
  TTestMPVEqualizer = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Band tests }
    procedure Test_Equalizer_DefaultBandsAreZero;
    procedure Test_Equalizer_SetBand;
    procedure Test_Equalizer_BandClampsMin;
    procedure Test_Equalizer_BandClampsMax;
    procedure Test_Equalizer_InvalidBandIgnored;

    { Preset tests }
    procedure Test_Equalizer_SetPreset;
    procedure Test_Equalizer_GetPreset;

    { Reset tests }
    procedure Test_Equalizer_Reset;
    procedure Test_Equalizer_ResetClearsPreamp;

    { Enable tests }
    procedure Test_Equalizer_Enable;
    procedure Test_Equalizer_Disable;
    procedure Test_Equalizer_DefaultDisabled;
  end;

  { ===========================================================================
    TTestMPVTracks - Track management tests
    =========================================================================== }
  TTestMPVTracks = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
    FLastAudioTrackID: Integer;
    FLastSubtitleTrackID: Integer;
    procedure OnAudioTrackChange(Sender: TObject; TrackID: Integer);
    procedure OnSubtitleTrackChange(Sender: TObject; TrackID: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { Audio track tests }
    procedure Test_AudioTracks_InitiallyEmpty;
    procedure Test_AudioTracks_AddTrack;
    procedure Test_AudioTracks_SelectTrack;
    procedure Test_AudioTracks_SelectFiresEvent;

    { Subtitle track tests }
    procedure Test_SubtitleTracks_InitiallyEmpty;
    procedure Test_SubtitleTracks_AddTrack;
    procedure Test_SubtitleTracks_SelectTrack;
    procedure Test_SubtitleTracks_LoadExternal;

    { Video track tests }
    procedure Test_VideoTracks_InitiallyEmpty;
    procedure Test_VideoTracks_AddTrack;

    { Clear tracks tests }
    procedure Test_ClearTracks_ClearsAll;

    { Subtitle settings }
    procedure Test_SubScale_DefaultIsOne;
    procedure Test_SubScale_SetValue;
    procedure Test_SubDelay_DefaultIsZero;
    procedure Test_SubVisible_DefaultIsTrue;
  end;

  { ===========================================================================
    TTestMPVSimulation - Simulation methods tests
    =========================================================================== }
  TTestMPVSimulation = class(TTestCase)
  private
    FEngine: TMockMPVEngine;
    FMetadataKeys: TStringList;
    FMetadataValues: TStringList;
    FEndFileReason: Integer;
    procedure OnMetadata(Sender: TObject; const Key, Value: string);
    procedure OnEndFile(Sender: TObject; Reason: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { SimulateFileLoad tests }
    procedure Test_SimulateFileLoad_SetsMediaFile;
    procedure Test_SimulateFileLoad_SetsDuration;
    procedure Test_SimulateFileLoad_SetsStatusPlaying;
    procedure Test_SimulateFileLoad_WithVideo;
    procedure Test_SimulateFileLoad_AudioOnly;

    { SimulateError tests }
    procedure Test_SimulateError_SetsStatusError;
    procedure Test_SimulateError_SetsLastError;

    { SimulateMetadata tests }
    procedure Test_SimulateMetadata_FiresEvents;
    procedure Test_SimulateMetadata_UpdatesStreamInfo;

    { SimulateEndFile tests }
    procedure Test_SimulateEndFile_SetsStatusStopped;
    procedure Test_SimulateEndFile_FiresEvent;
    procedure Test_SimulateEndFile_WithReason;

    { CallLog tests }
    procedure Test_CallLog_RecordsCalls;
    procedure Test_CallLog_ClearWorks;
    procedure Test_CallLog_MultipleOperations;
  end;

implementation

{ ===========================================================================
  TTestMPVPlayback
  =========================================================================== }

procedure TTestMPVPlayback.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
  FEngine.OnStatusChange := @OnStatusChange;
  FEngine.OnPositionChange := @OnPositionChange;
  FEngine.OnFileLoaded := @OnFileLoaded;
  FEngine.OnEndFile := @OnEndFile;
  SetLength(FStatusChanges, 0);
  SetLength(FPositionChanges, 0);
  FFileLoadedCount := 0;
  FEndFileCount := 0;
end;

procedure TTestMPVPlayback.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVPlayback.OnStatusChange(Sender: TObject; OldStatus, NewStatus: TMPVStatus);
begin
  SetLength(FStatusChanges, Length(FStatusChanges) + 1);
  FStatusChanges[High(FStatusChanges)] := NewStatus;
end;

procedure TTestMPVPlayback.OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
begin
  SetLength(FPositionChanges, Length(FPositionChanges) + 1);
  FPositionChanges[High(FPositionChanges)] := PosSec;
end;

procedure TTestMPVPlayback.OnFileLoaded(Sender: TObject; const FileName: string);
begin
  Inc(FFileLoadedCount);
end;

procedure TTestMPVPlayback.OnEndFile(Sender: TObject; Reason: Integer);
begin
  Inc(FEndFileCount);
end;

procedure TTestMPVPlayback.Test_Create_StatusIsNone;
begin
  AssertEquals('Initial status should be msNone', Ord(msNone), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Initialize_SetsInitialized;
begin
  FEngine.Initialize(0);
  AssertTrue('Should be initialized', FEngine.Initialized);
end;

procedure TTestMPVPlayback.Test_Initialize_StatusStaysNone;
begin
  FEngine.Initialize(0);
  AssertEquals('Status should stay msNone after init', Ord(msNone), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Shutdown_ClearsState;
begin
  FEngine.Initialize(0);
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.Shutdown;
  AssertFalse('Should not be initialized', FEngine.Initialized);
  AssertEquals('MediaFile should be empty', '', FEngine.MediaFile);
end;

procedure TTestMPVPlayback.Test_PlayMedia_ChangesStatusToPlaying;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  AssertEquals('Status should be msPlaying', Ord(msPlaying), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_PlayMedia_StoresMediaFile;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/path/to/video.mkv');
  AssertEquals('MediaFile should be stored', '/path/to/video.mkv', FEngine.MediaFile);
end;

procedure TTestMPVPlayback.Test_PlayMedia_FiresFileLoadedEvent;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  AssertEquals('FileLoaded event should fire once', 1, FFileLoadedCount);
end;

procedure TTestMPVPlayback.Test_PlayMedia_RequiresInitialization;
begin
  FEngine.PlayMedia('/test.mp4');
  AssertEquals('Status should be msError without init', Ord(msError), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_PlayMedia_LogsCall;
begin
  FEngine.Initialize(0);
  FEngine.ClearCallLog;
  FEngine.PlayMedia('/test.mp4');
  AssertTrue('CallLog should contain PlayMedia',
    Pos('PlayMedia', FEngine.GetCallLog.Text) > 0);
end;

procedure TTestMPVPlayback.Test_Pause_ChangesStatusToPaused;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  FEngine.Pause;
  AssertEquals('Status should be msPaused', Ord(msPaused), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Pause_OnlyWorksWhenPlaying;
begin
  FEngine.Initialize(0);
  FEngine.Pause;
  AssertEquals('Status should stay msNone', Ord(msNone), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Resume_ChangesStatusToPlaying;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  FEngine.Pause;
  FEngine.Resume;
  AssertEquals('Status should be msPlaying', Ord(msPlaying), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Resume_OnlyWorksWhenPaused;
begin
  FEngine.Initialize(0);
  FEngine.Resume;
  AssertEquals('Status should stay msNone', Ord(msNone), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_TogglePause_TogglesState;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  AssertEquals('Should be playing', Ord(msPlaying), Ord(FEngine.Status));
  FEngine.TogglePause;
  AssertEquals('Should be paused', Ord(msPaused), Ord(FEngine.Status));
  FEngine.TogglePause;
  AssertEquals('Should be playing again', Ord(msPlaying), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_TogglePause_MultipleTimes;
var
  I: Integer;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  for I := 1 to 10 do
  begin
    FEngine.TogglePause;
    if I mod 2 = 1 then
      AssertEquals('Should be paused on odd toggles', Ord(msPaused), Ord(FEngine.Status))
    else
      AssertEquals('Should be playing on even toggles', Ord(msPlaying), Ord(FEngine.Status));
  end;
end;

procedure TTestMPVPlayback.Test_Stop_ChangesStatusToStopped;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  FEngine.Stop;
  AssertEquals('Status should be msStopped', Ord(msStopped), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_Stop_ResetsPosition;
begin
  FEngine.Initialize(0);
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.SimulatePosition(60);
  FEngine.Stop;
  AssertEquals('Position should be 0', 0, FEngine.Position, 0.01);
end;

procedure TTestMPVPlayback.Test_CloseMedia_ClearsMediaFile;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  FEngine.CloseMedia;
  AssertEquals('MediaFile should be empty', '', FEngine.MediaFile);
end;

procedure TTestMPVPlayback.Test_CloseMedia_ChangesStatusToNone;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  FEngine.CloseMedia;
  AssertEquals('Status should be msNone', Ord(msNone), Ord(FEngine.Status));
end;

procedure TTestMPVPlayback.Test_CloseMedia_ClearsTracks;
begin
  FEngine.Initialize(0);
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.AddAudioTrack(1, 'English', 'eng', 'aac');
  FEngine.CloseMedia;
  AssertEquals('AudioTracks should be empty', 0, FEngine.AudioTracks.Count);
end;

procedure TTestMPVPlayback.Test_StatusChange_FiresEvent;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');
  AssertTrue('Should have status changes', Length(FStatusChanges) > 0);
  AssertEquals('Last status should be msPlaying', Ord(msPlaying), Ord(FStatusChanges[High(FStatusChanges)]));
end;

procedure TTestMPVPlayback.Test_StatusChange_MultipleTransitions;
begin
  FEngine.Initialize(0);
  FEngine.PlayMedia('/test.mp4');  { -> msPlaying }
  FEngine.Pause;                    { -> msPaused }
  FEngine.Resume;                   { -> msPlaying }
  FEngine.Stop;                     { -> msStopped }
  AssertTrue('Should have multiple status changes', Length(FStatusChanges) >= 4);
end;

{ ===========================================================================
  TTestMPVSeeking
  =========================================================================== }

procedure TTestMPVSeeking.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
  FEngine.Initialize(0);
  FEngine.SimulateFileLoad('/test.mp4', 120);  { 2 minute video }
  FEngine.OnSeek := @OnSeek;
  FEngine.OnPositionChange := @OnPositionChange;
  FSeekCount := 0;
  FLastPosition := 0;
end;

procedure TTestMPVSeeking.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVSeeking.OnSeek(Sender: TObject);
begin
  Inc(FSeekCount);
end;

procedure TTestMPVSeeking.OnPositionChange(Sender: TObject; PosSec, PosPct: Double);
begin
  FLastPosition := PosSec;
end;

procedure TTestMPVSeeking.Test_SeekAbsolute_SetsPosition;
begin
  FEngine.SeekAbsolute(30);
  AssertEquals('Position should be 30', 30, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekAbsolute_ClampsToZero;
begin
  FEngine.SeekAbsolute(-10);
  AssertEquals('Position should be 0', 0, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekAbsolute_ClampsToDuration;
begin
  FEngine.SeekAbsolute(200);
  AssertEquals('Position should be clamped to duration', 120, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekAbsolute_FiresEvent;
begin
  FEngine.SeekAbsolute(30);
  AssertEquals('OnSeek should fire', 1, FSeekCount);
end;

procedure TTestMPVSeeking.Test_SeekAbsolute_LogsCall;
begin
  FEngine.ClearCallLog;
  FEngine.SeekAbsolute(30);
  AssertTrue('CallLog should contain SeekAbsolute',
    Pos('SeekAbsolute', FEngine.GetCallLog.Text) > 0);
end;

procedure TTestMPVSeeking.Test_SeekRelative_Forward;
begin
  FEngine.SimulatePosition(30);
  FEngine.SeekRelative(10);
  AssertEquals('Position should be 40', 40, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekRelative_Backward;
begin
  FEngine.SimulatePosition(30);
  FEngine.SeekRelative(-10);
  AssertEquals('Position should be 20', 20, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekRelative_ClampsAtBoundaries;
begin
  FEngine.SimulatePosition(10);
  FEngine.SeekRelative(-20);
  AssertEquals('Position should be clamped to 0', 0, FEngine.Position, 0.01);

  FEngine.SimulatePosition(110);
  FEngine.SeekRelative(20);
  AssertEquals('Position should be clamped to 120', 120, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekPercent_SetsCorrectPosition;
begin
  FEngine.SeekPercent(25);
  AssertEquals('Position should be 30 (25% of 120)', 30, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekPercent_AtZero;
begin
  FEngine.SimulatePosition(60);
  FEngine.SeekPercent(0);
  AssertEquals('Position should be 0', 0, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekPercent_AtHundred;
begin
  FEngine.SeekPercent(100);
  AssertEquals('Position should be 120', 120, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SeekPercent_AtFifty;
begin
  FEngine.SeekPercent(50);
  AssertEquals('Position should be 60', 60, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SimulatePosition_UpdatesPosition;
begin
  FEngine.SimulatePosition(45);
  AssertEquals('Position should be 45', 45, FEngine.Position, 0.01);
end;

procedure TTestMPVSeeking.Test_SimulatePosition_CalculatesPercent;
begin
  FEngine.SimulatePosition(60);
  AssertEquals('PercentPos should be 50', 50, FEngine.PercentPos, 0.01);
end;

procedure TTestMPVSeeking.Test_SimulatePosition_FiresEvent;
begin
  FEngine.SimulatePosition(30);
  AssertEquals('OnPositionChange should receive position', 30, FLastPosition, 0.01);
end;

{ ===========================================================================
  TTestMPVVolume
  =========================================================================== }

procedure TTestMPVVolume.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
end;

procedure TTestMPVVolume.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVVolume.Test_Volume_DefaultIs100;
begin
  AssertEquals('Default volume should be 100', 100, FEngine.Volume);
end;

procedure TTestMPVVolume.Test_Volume_SetValue;
begin
  FEngine.Volume := 75;
  AssertEquals('Volume should be 75', 75, FEngine.Volume);
end;

procedure TTestMPVVolume.Test_Volume_ClampsMin;
begin
  FEngine.Volume := -10;
  AssertEquals('Volume should be clamped to 0', 0, FEngine.Volume);
end;

procedure TTestMPVVolume.Test_Volume_ClampsMax;
begin
  FEngine.Volume := 200;
  AssertEquals('Volume should be clamped to 150', 150, FEngine.Volume);
end;

procedure TTestMPVVolume.Test_Volume_LogsCall;
begin
  FEngine.ClearCallLog;
  FEngine.Volume := 50;
  AssertTrue('CallLog should contain SetVolume',
    Pos('SetVolume', FEngine.GetCallLog.Text) > 0);
end;

procedure TTestMPVVolume.Test_Muted_DefaultIsFalse;
begin
  AssertFalse('Default muted should be False', FEngine.Muted);
end;

procedure TTestMPVVolume.Test_Muted_SetTrue;
begin
  FEngine.Muted := True;
  AssertTrue('Muted should be True', FEngine.Muted);
end;

procedure TTestMPVVolume.Test_Muted_SetFalse;
begin
  FEngine.Muted := True;
  FEngine.Muted := False;
  AssertFalse('Muted should be False', FEngine.Muted);
end;

procedure TTestMPVVolume.Test_Muted_LogsCall;
begin
  FEngine.ClearCallLog;
  FEngine.Muted := True;
  AssertTrue('CallLog should contain SetMuted',
    Pos('SetMuted', FEngine.GetCallLog.Text) > 0);
end;

procedure TTestMPVVolume.Test_Speed_DefaultIsOne;
begin
  AssertEquals('Default speed should be 1.0', 1.0, FEngine.Speed, 0.01);
end;

procedure TTestMPVVolume.Test_Speed_SetValue;
begin
  FEngine.Speed := 1.5;
  AssertEquals('Speed should be 1.5', 1.5, FEngine.Speed, 0.01);
end;

procedure TTestMPVVolume.Test_Speed_ClampsMin;
begin
  FEngine.Speed := 0.01;
  AssertEquals('Speed should be clamped to 0.1', 0.1, FEngine.Speed, 0.01);
end;

procedure TTestMPVVolume.Test_Speed_ClampsMax;
begin
  FEngine.Speed := 10;
  AssertEquals('Speed should be clamped to 4.0', 4.0, FEngine.Speed, 0.01);
end;

procedure TTestMPVVolume.Test_AudioDelay_DefaultIsZero;
begin
  AssertEquals('Default audio delay should be 0', 0, FEngine.AudioDelay, 0.01);
end;

procedure TTestMPVVolume.Test_AudioDelay_SetPositive;
begin
  FEngine.AudioDelay := 0.5;
  AssertEquals('Audio delay should be 0.5', 0.5, FEngine.AudioDelay, 0.01);
end;

procedure TTestMPVVolume.Test_AudioDelay_SetNegative;
begin
  FEngine.AudioDelay := -0.3;
  AssertEquals('Audio delay should be -0.3', -0.3, FEngine.AudioDelay, 0.01);
end;

{ ===========================================================================
  TTestMPVVideo
  =========================================================================== }

procedure TTestMPVVideo.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
end;

procedure TTestMPVVideo.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVVideo.Test_Brightness_DefaultIsZero;
begin
  AssertEquals('Default brightness should be 0', 0, FEngine.Brightness);
end;

procedure TTestMPVVideo.Test_Brightness_SetValue;
begin
  FEngine.Brightness := 25;
  AssertEquals('Brightness should be 25', 25, FEngine.Brightness);
end;

procedure TTestMPVVideo.Test_Brightness_ClampsMin;
begin
  FEngine.Brightness := -150;
  AssertEquals('Brightness should be clamped to -100', -100, FEngine.Brightness);
end;

procedure TTestMPVVideo.Test_Brightness_ClampsMax;
begin
  FEngine.Brightness := 150;
  AssertEquals('Brightness should be clamped to 100', 100, FEngine.Brightness);
end;

procedure TTestMPVVideo.Test_Contrast_DefaultIsZero;
begin
  AssertEquals('Default contrast should be 0', 0, FEngine.Contrast);
end;

procedure TTestMPVVideo.Test_Contrast_SetValue;
begin
  FEngine.Contrast := 30;
  AssertEquals('Contrast should be 30', 30, FEngine.Contrast);
end;

procedure TTestMPVVideo.Test_Contrast_Range;
begin
  FEngine.Contrast := -100;
  AssertEquals('Contrast min should be -100', -100, FEngine.Contrast);
  FEngine.Contrast := 100;
  AssertEquals('Contrast max should be 100', 100, FEngine.Contrast);
end;

procedure TTestMPVVideo.Test_Saturation_DefaultIsZero;
begin
  AssertEquals('Default saturation should be 0', 0, FEngine.Saturation);
end;

procedure TTestMPVVideo.Test_Saturation_SetValue;
begin
  FEngine.Saturation := -20;
  AssertEquals('Saturation should be -20', -20, FEngine.Saturation);
end;

procedure TTestMPVVideo.Test_Saturation_Range;
begin
  FEngine.Saturation := -100;
  AssertEquals('Saturation min', -100, FEngine.Saturation);
  FEngine.Saturation := 100;
  AssertEquals('Saturation max', 100, FEngine.Saturation);
end;

procedure TTestMPVVideo.Test_Hue_DefaultIsZero;
begin
  AssertEquals('Default hue should be 0', 0, FEngine.Hue);
end;

procedure TTestMPVVideo.Test_Hue_SetValue;
begin
  FEngine.Hue := 50;
  AssertEquals('Hue should be 50', 50, FEngine.Hue);
end;

procedure TTestMPVVideo.Test_Gamma_DefaultIsZero;
begin
  AssertEquals('Default gamma should be 0', 0, FEngine.Gamma);
end;

procedure TTestMPVVideo.Test_Gamma_SetValue;
begin
  FEngine.Gamma := -30;
  AssertEquals('Gamma should be -30', -30, FEngine.Gamma);
end;

{ ===========================================================================
  TTestMPVEqualizer
  =========================================================================== }

procedure TTestMPVEqualizer.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
end;

procedure TTestMPVEqualizer.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVEqualizer.Test_Equalizer_DefaultBandsAreZero;
var
  I: Integer;
begin
  for I := 0 to 9 do
    AssertEquals('Band ' + IntToStr(I) + ' should be 0', 0, FEngine.GetEqualizerBand(I), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_SetBand;
begin
  FEngine.SetEqualizerBand(3, 6);
  AssertEquals('Band 3 should be 6', 6, FEngine.GetEqualizerBand(3), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_BandClampsMin;
begin
  FEngine.SetEqualizerBand(0, -20);
  AssertEquals('Band should be clamped to -12', -12, FEngine.GetEqualizerBand(0), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_BandClampsMax;
begin
  FEngine.SetEqualizerBand(0, 20);
  AssertEquals('Band should be clamped to 12', 12, FEngine.GetEqualizerBand(0), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_InvalidBandIgnored;
begin
  FEngine.SetEqualizerBand(15, 5);
  AssertEquals('Invalid band should return 0', 0, FEngine.GetEqualizerBand(15), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_SetPreset;
begin
  FEngine.SetEqualizerPreset('1:2:3:4:5:6:7:8:9:10');
  AssertEquals('Band 0 should be 1', 1, FEngine.GetEqualizerBand(0), 0.01);
  AssertEquals('Band 9 should be 10', 10, FEngine.GetEqualizerBand(9), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_GetPreset;
var
  Preset: string;
begin
  FEngine.SetEqualizerBand(0, 5);
  FEngine.SetEqualizerBand(5, -3);
  Preset := FEngine.GetEqualizerPreset;
  AssertTrue('Preset should contain values', Pos('5', Preset) > 0);
end;

procedure TTestMPVEqualizer.Test_Equalizer_Reset;
var
  I: Integer;
begin
  FEngine.SetEqualizerBand(3, 6);
  FEngine.SetEqualizerBand(7, -4);
  FEngine.ResetEqualizer;
  for I := 0 to 9 do
    AssertEquals('Band ' + IntToStr(I) + ' should be reset to 0',
      0, FEngine.GetEqualizerBand(I), 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_ResetClearsPreamp;
begin
  FEngine.Preamp := 5;
  FEngine.ResetEqualizer;
  AssertEquals('Preamp should be 0', 0, FEngine.Preamp, 0.01);
end;

procedure TTestMPVEqualizer.Test_Equalizer_Enable;
begin
  FEngine.EnableEqualizer(True);
  AssertTrue('Equalizer should be enabled', FEngine.EqualizerEnabled);
end;

procedure TTestMPVEqualizer.Test_Equalizer_Disable;
begin
  FEngine.EnableEqualizer(True);
  FEngine.EnableEqualizer(False);
  AssertFalse('Equalizer should be disabled', FEngine.EqualizerEnabled);
end;

procedure TTestMPVEqualizer.Test_Equalizer_DefaultDisabled;
begin
  AssertFalse('Equalizer should be disabled by default', FEngine.EqualizerEnabled);
end;

{ ===========================================================================
  TTestMPVTracks
  =========================================================================== }

procedure TTestMPVTracks.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
  FEngine.OnAudioTrackChange := @OnAudioTrackChange;
  FEngine.OnSubtitleTrackChange := @OnSubtitleTrackChange;
  FLastAudioTrackID := -1;
  FLastSubtitleTrackID := -1;
end;

procedure TTestMPVTracks.TearDown;
begin
  FEngine.Free;
end;

procedure TTestMPVTracks.OnAudioTrackChange(Sender: TObject; TrackID: Integer);
begin
  FLastAudioTrackID := TrackID;
end;

procedure TTestMPVTracks.OnSubtitleTrackChange(Sender: TObject; TrackID: Integer);
begin
  FLastSubtitleTrackID := TrackID;
end;

procedure TTestMPVTracks.Test_AudioTracks_InitiallyEmpty;
begin
  AssertEquals('AudioTracks should be empty', 0, FEngine.AudioTracks.Count);
end;

procedure TTestMPVTracks.Test_AudioTracks_AddTrack;
begin
  FEngine.AddAudioTrack(1, 'English', 'eng', 'aac');
  AssertEquals('AudioTracks should have 1 item', 1, FEngine.AudioTracks.Count);
end;

procedure TTestMPVTracks.Test_AudioTracks_SelectTrack;
begin
  FEngine.AddAudioTrack(1, 'English', 'eng', 'aac');
  FEngine.AddAudioTrack(2, 'French', 'fra', 'ac3');
  FEngine.SetAudioTrack(2);
  AssertEquals('Selected audio track should be 2', 2, FEngine.AudioTracks.SelectedID);
end;

procedure TTestMPVTracks.Test_AudioTracks_SelectFiresEvent;
begin
  FEngine.AddAudioTrack(1, 'English', 'eng', 'aac');
  FEngine.SetAudioTrack(1);
  AssertEquals('Event should fire with track ID', 1, FLastAudioTrackID);
end;

procedure TTestMPVTracks.Test_SubtitleTracks_InitiallyEmpty;
begin
  AssertEquals('SubtitleTracks should be empty', 0, FEngine.SubtitleTracks.Count);
end;

procedure TTestMPVTracks.Test_SubtitleTracks_AddTrack;
begin
  FEngine.AddSubtitleTrack(1, 'English', 'eng');
  AssertEquals('SubtitleTracks should have 1 item', 1, FEngine.SubtitleTracks.Count);
end;

procedure TTestMPVTracks.Test_SubtitleTracks_SelectTrack;
begin
  FEngine.AddSubtitleTrack(1, 'English', 'eng');
  FEngine.AddSubtitleTrack(2, 'French', 'fra');
  FEngine.SetSubtitleTrack(2);
  AssertEquals('Selected subtitle track should be 2', 2, FEngine.SubtitleTracks.SelectedID);
end;

procedure TTestMPVTracks.Test_SubtitleTracks_LoadExternal;
begin
  FEngine.LoadSubtitle('/path/to/subtitle.srt');
  AssertEquals('Should have 1 external subtitle', 1, FEngine.SubtitleTracks.Count);
  AssertTrue('External subtitle should be marked', FEngine.SubtitleTracks[0].IsExternal);
end;

procedure TTestMPVTracks.Test_VideoTracks_InitiallyEmpty;
begin
  AssertEquals('VideoTracks should be empty', 0, FEngine.VideoTracks.Count);
end;

procedure TTestMPVTracks.Test_VideoTracks_AddTrack;
begin
  FEngine.AddVideoTrack(1, 'Main', 'h264');
  AssertEquals('VideoTracks should have 1 item', 1, FEngine.VideoTracks.Count);
end;

procedure TTestMPVTracks.Test_ClearTracks_ClearsAll;
begin
  FEngine.AddAudioTrack(1, 'Audio', 'eng', 'aac');
  FEngine.AddSubtitleTrack(1, 'Sub', 'eng');
  FEngine.AddVideoTrack(1, 'Video', 'h264');
  FEngine.ClearTracks;
  AssertEquals('AudioTracks should be empty', 0, FEngine.AudioTracks.Count);
  AssertEquals('SubtitleTracks should be empty', 0, FEngine.SubtitleTracks.Count);
  AssertEquals('VideoTracks should be empty', 0, FEngine.VideoTracks.Count);
end;

procedure TTestMPVTracks.Test_SubScale_DefaultIsOne;
begin
  AssertEquals('Default SubScale should be 1.0', 1.0, FEngine.SubScale, 0.01);
end;

procedure TTestMPVTracks.Test_SubScale_SetValue;
begin
  FEngine.SubScale := 1.5;
  AssertEquals('SubScale should be 1.5', 1.5, FEngine.SubScale, 0.01);
end;

procedure TTestMPVTracks.Test_SubDelay_DefaultIsZero;
begin
  AssertEquals('Default SubDelay should be 0', 0, FEngine.SubDelay, 0.01);
end;

procedure TTestMPVTracks.Test_SubVisible_DefaultIsTrue;
begin
  AssertTrue('Default SubVisible should be True', FEngine.SubVisible);
end;

{ ===========================================================================
  TTestMPVSimulation
  =========================================================================== }

procedure TTestMPVSimulation.SetUp;
begin
  FEngine := TMockMPVEngine.Create;
  FEngine.Initialize(0);
  FEngine.OnMetadata := @OnMetadata;
  FEngine.OnEndFile := @OnEndFile;
  FMetadataKeys := TStringList.Create;
  FMetadataValues := TStringList.Create;
  FEndFileReason := -1;
end;

procedure TTestMPVSimulation.TearDown;
begin
  FMetadataKeys.Free;
  FMetadataValues.Free;
  FEngine.Free;
end;

procedure TTestMPVSimulation.OnMetadata(Sender: TObject; const Key, Value: string);
begin
  FMetadataKeys.Add(Key);
  FMetadataValues.Add(Value);
end;

procedure TTestMPVSimulation.OnEndFile(Sender: TObject; Reason: Integer);
begin
  FEndFileReason := Reason;
end;

procedure TTestMPVSimulation.Test_SimulateFileLoad_SetsMediaFile;
begin
  FEngine.SimulateFileLoad('/path/to/movie.mp4', 7200);
  AssertEquals('MediaFile should be set', '/path/to/movie.mp4', FEngine.MediaFile);
end;

procedure TTestMPVSimulation.Test_SimulateFileLoad_SetsDuration;
begin
  FEngine.SimulateFileLoad('/path/to/movie.mp4', 7200);
  AssertEquals('Duration should be 7200', 7200, FEngine.Duration, 0.01);
end;

procedure TTestMPVSimulation.Test_SimulateFileLoad_SetsStatusPlaying;
begin
  FEngine.SimulateFileLoad('/path/to/movie.mp4', 7200);
  AssertEquals('Status should be msPlaying', Ord(msPlaying), Ord(FEngine.Status));
end;

procedure TTestMPVSimulation.Test_SimulateFileLoad_WithVideo;
begin
  FEngine.SimulateFileLoad('/path/to/movie.mp4', 7200, True);
  AssertTrue('HasVideo should be True', FEngine.StreamInfo.HasVideo);
end;

procedure TTestMPVSimulation.Test_SimulateFileLoad_AudioOnly;
begin
  FEngine.SimulateFileLoad('/path/to/song.mp3', 240, False);
  AssertFalse('HasVideo should be False', FEngine.StreamInfo.HasVideo);
  AssertTrue('HasAudio should be True', FEngine.StreamInfo.HasAudio);
end;

procedure TTestMPVSimulation.Test_SimulateError_SetsStatusError;
begin
  FEngine.SimulateError('Test error');
  AssertEquals('Status should be msError', Ord(msError), Ord(FEngine.Status));
end;

procedure TTestMPVSimulation.Test_SimulateError_SetsLastError;
begin
  FEngine.SimulateError('File not found');
  AssertEquals('LastError should be set', 'File not found', FEngine.LastError);
end;

procedure TTestMPVSimulation.Test_SimulateMetadata_FiresEvents;
begin
  FEngine.SimulateMetadata('Test Song', 'Test Artist');
  AssertTrue('Should have metadata events', FMetadataKeys.Count >= 2);
  AssertTrue('Should have title key', FMetadataKeys.IndexOf('title') >= 0);
  AssertTrue('Should have artist key', FMetadataKeys.IndexOf('artist') >= 0);
end;

procedure TTestMPVSimulation.Test_SimulateMetadata_UpdatesStreamInfo;
begin
  FEngine.SimulateMetadata('My Song', 'My Artist');
  AssertEquals('StreamInfo.Title should be set', 'My Song', FEngine.StreamInfo.Title);
  AssertEquals('StreamInfo.Artist should be set', 'My Artist', FEngine.StreamInfo.Artist);
end;

procedure TTestMPVSimulation.Test_SimulateEndFile_SetsStatusStopped;
begin
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.SimulateEndFile;
  AssertEquals('Status should be msStopped', Ord(msStopped), Ord(FEngine.Status));
end;

procedure TTestMPVSimulation.Test_SimulateEndFile_FiresEvent;
begin
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.SimulateEndFile;
  AssertTrue('OnEndFile should have fired', FEndFileReason >= 0);
end;

procedure TTestMPVSimulation.Test_SimulateEndFile_WithReason;
begin
  FEngine.SimulateFileLoad('/test.mp4', 120);
  FEngine.SimulateEndFile(5);
  AssertEquals('EndFile reason should be 5', 5, FEndFileReason);
end;

procedure TTestMPVSimulation.Test_CallLog_RecordsCalls;
begin
  FEngine.ClearCallLog;
  FEngine.PlayMedia('/test.mp4');
  AssertTrue('CallLog should not be empty', FEngine.GetCallLog.Count > 0);
end;

procedure TTestMPVSimulation.Test_CallLog_ClearWorks;
begin
  FEngine.PlayMedia('/test.mp4');
  FEngine.ClearCallLog;
  AssertEquals('CallLog should be empty after clear', 0, FEngine.GetCallLog.Count);
end;

procedure TTestMPVSimulation.Test_CallLog_MultipleOperations;
begin
  FEngine.ClearCallLog;
  FEngine.PlayMedia('/test.mp4');
  FEngine.Pause;
  FEngine.Resume;
  FEngine.Stop;
  AssertTrue('CallLog should have multiple entries', FEngine.GetCallLog.Count >= 4);
end;

{ Registration }
initialization
  RegisterTest('Unit', TTestMPVPlayback);
  RegisterTest('Unit', TTestMPVSeeking);
  RegisterTest('Unit', TTestMPVVolume);
  RegisterTest('Unit', TTestMPVVideo);
  RegisterTest('Unit', TTestMPVEqualizer);
  RegisterTest('Unit', TTestMPVTracks);
  RegisterTest('Unit', TTestMPVSimulation);

end.
