{ ═══════════════════════════════════════════════════════════════════════════════
  uTestStreamRecorder.pas - Unit Tests for Stream Recorder

  Part of 3nity Media - Test Suite

  Tests for the uStreamRecorder unit which handles stream recording functionality,
  file naming, format conversion, and recording state management.

  Author: Nicolas DEOUX (NDXDev@gmail.com)
  License: GPL-2.0
  ═══════════════════════════════════════════════════════════════════════════════ }

unit uTestStreamRecorder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry,
  uStreamRecorder;

type
  { TTestStreamRecorder }
  TTestStreamRecorder = class(TTestCase)
  private
    FRecorder: TStreamRecorder;
    FStateChangeCount: Integer;
    FLastOldState: TRecordingState;
    FLastNewState: TRecordingState;
    FTrackChangeCount: Integer;
    FLastArtist: string;
    FLastTitle: string;
    procedure OnStateChangeHandler(Sender: TObject; OldState, NewState: TRecordingState);
    procedure OnTrackChangeHandler(Sender: TObject; const Artist, Title: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    { AudioFormatToString tests }
    procedure Test_AudioFormatToString_MP3;
    procedure Test_AudioFormatToString_OGG;
    procedure Test_AudioFormatToString_FLAC;
    procedure Test_AudioFormatToString_WAV;
    procedure Test_AudioFormatToString_AAC;
    procedure Test_AudioFormatToString_Opus;

    { StringToAudioFormat tests }
    procedure Test_StringToAudioFormat_MP3;
    procedure Test_StringToAudioFormat_OGG;
    procedure Test_StringToAudioFormat_FLAC;
    procedure Test_StringToAudioFormat_WAV;
    procedure Test_StringToAudioFormat_AAC;
    procedure Test_StringToAudioFormat_Opus;
    procedure Test_StringToAudioFormat_CaseInsensitive;
    procedure Test_StringToAudioFormat_Unknown_DefaultsMP3;

    { VideoFormatToString tests }
    procedure Test_VideoFormatToString_MP4;
    procedure Test_VideoFormatToString_MKV;
    procedure Test_VideoFormatToString_WebM;
    procedure Test_VideoFormatToString_TS;
    procedure Test_VideoFormatToString_AVI;

    { StringToVideoFormat tests }
    procedure Test_StringToVideoFormat_MP4;
    procedure Test_StringToVideoFormat_MKV;
    procedure Test_StringToVideoFormat_WebM;
    procedure Test_StringToVideoFormat_TS;
    procedure Test_StringToVideoFormat_AVI;
    procedure Test_StringToVideoFormat_CaseInsensitive;
    procedure Test_StringToVideoFormat_Unknown_DefaultsMP4;

    { RecordingStateToString tests }
    procedure Test_RecordingStateToString_Idle;
    procedure Test_RecordingStateToString_Recording;
    procedure Test_RecordingStateToString_Paused;
    procedure Test_RecordingStateToString_Stopping;
    procedure Test_RecordingStateToString_Error;

    { FormatRecordingDuration tests }
    procedure Test_FormatRecordingDuration_MinutesOnly;
    procedure Test_FormatRecordingDuration_WithHours;
    procedure Test_FormatRecordingDuration_Zero;

    { FormatRecordingSize tests }
    procedure Test_FormatRecordingSize_Bytes;
    procedure Test_FormatRecordingSize_KB;
    procedure Test_FormatRecordingSize_MB;
    procedure Test_FormatRecordingSize_GB;

    { TRecordingState enum tests }
    procedure Test_RecordingState_EnumValues;

    { TAudioRecordFormat enum tests }
    procedure Test_AudioRecordFormat_EnumValues;

    { TVideoRecordFormat enum tests }
    procedure Test_VideoRecordFormat_EnumValues;

    { TStreamRecorder initial state tests }
    procedure Test_Create_StateIsIdle;
    procedure Test_Create_IsRecordingFalse;
    procedure Test_Create_EmptyFileName;
    procedure Test_Create_EmptyRecordingPath;
    procedure Test_Create_ZeroFileSize;
    procedure Test_Create_ZeroTrackCount;

    { Default settings tests }
    procedure Test_DefaultSettings_AudioFormatMP3;
    procedure Test_DefaultSettings_VideoFormatMP4;
    procedure Test_DefaultSettings_AppendTimestampTrue;
    procedure Test_DefaultSettings_CreateSubfoldersTrue;
    procedure Test_DefaultSettings_OverwriteExistingFalse;
    procedure Test_DefaultSettings_AutoSplitFalse;
    procedure Test_DefaultSettings_SplitSizeMB;
    procedure Test_DefaultSettings_SplitTimeMinutes;
    procedure Test_DefaultSettings_UseMetadataTrue;
    procedure Test_DefaultSettings_OutputFolderNotEmpty;
    procedure Test_DefaultSettings_OutputFolderVideoNotEmpty;

    { StartRecording tests }
    procedure Test_StartRecording_SetsStateRecording;
    procedure Test_StartRecording_SetsSourceURL;
    procedure Test_StartRecording_SetsSourceName;
    procedure Test_StartRecording_SetsIsAudioOnly;
    procedure Test_StartRecording_ReturnsTrue;
    procedure Test_StartRecording_SetsStartTime;
    procedure Test_StartRecording_WhenAlreadyRecording_ReturnsFalse;

    { StopRecording tests }
    procedure Test_StopRecording_SetsStateIdle;
    procedure Test_StopRecording_ClearsRecordingPath;
    procedure Test_StopRecording_WhenNotRecording_NoEffect;

    { PauseRecording / ResumeRecording tests }
    procedure Test_PauseRecording_SetsStatePaused;
    procedure Test_ResumeRecording_SetsStateRecording;
    procedure Test_PauseRecording_WhenNotRecording_NoEffect;

    { GetMPVRecordPath tests }
    procedure Test_GetMPVRecordPath_WhenRecording;
    procedure Test_GetMPVRecordPath_WhenNotRecording;

    { NotifyTrackChange tests }
    procedure Test_NotifyTrackChange_IncrementsTrackCount;
    procedure Test_NotifyTrackChange_SetsCurrentTitle;
    procedure Test_NotifyTrackChange_FormatsArtistTitle;
    procedure Test_NotifyTrackChange_TitleOnlyIfNoArtist;
    procedure Test_NotifyTrackChange_NoDuplicates;

    { ShouldSplitFile tests }
    procedure Test_ShouldSplitFile_FalseWhenAutoSplitDisabled;
    procedure Test_ShouldSplitFile_FalseWhenBelowLimits;

    { Events tests }
    procedure Test_OnStateChange_TriggeredOnStart;
    procedure Test_OnStateChange_TriggeredOnStop;
    procedure Test_OnStateChange_ReceivesCorrectStates;
    procedure Test_OnTrackChange_Triggered;
    procedure Test_OnTrackChange_ReceivesArtistTitle;

    { Initialize tests }
    procedure Test_Initialize_ResetsState;
    procedure Test_Initialize_ResetsInfo;
  end;

implementation

{ TTestStreamRecorder }

procedure TTestStreamRecorder.SetUp;
begin
  FRecorder := TStreamRecorder.Create;
  FStateChangeCount := 0;
  FTrackChangeCount := 0;
  FLastArtist := '';
  FLastTitle := '';
end;

procedure TTestStreamRecorder.TearDown;
begin
  FRecorder.Free;
end;

procedure TTestStreamRecorder.OnStateChangeHandler(Sender: TObject;
  OldState, NewState: TRecordingState);
begin
  Inc(FStateChangeCount);
  FLastOldState := OldState;
  FLastNewState := NewState;
end;

procedure TTestStreamRecorder.OnTrackChangeHandler(Sender: TObject;
  const Artist, Title: string);
begin
  Inc(FTrackChangeCount);
  FLastArtist := Artist;
  FLastTitle := Title;
end;

{ ─────────────────────────────────────────────────────────────────────────────
  AudioFormatToString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_AudioFormatToString_MP3;
begin
  AssertEquals('MP3', 'MP3', AudioFormatToString(arfMP3));
end;

procedure TTestStreamRecorder.Test_AudioFormatToString_OGG;
begin
  AssertEquals('OGG', 'OGG', AudioFormatToString(arfOGG));
end;

procedure TTestStreamRecorder.Test_AudioFormatToString_FLAC;
begin
  AssertEquals('FLAC', 'FLAC', AudioFormatToString(arfFLAC));
end;

procedure TTestStreamRecorder.Test_AudioFormatToString_WAV;
begin
  AssertEquals('WAV', 'WAV', AudioFormatToString(arfWAV));
end;

procedure TTestStreamRecorder.Test_AudioFormatToString_AAC;
begin
  AssertEquals('AAC', 'AAC', AudioFormatToString(arfAAC));
end;

procedure TTestStreamRecorder.Test_AudioFormatToString_Opus;
begin
  AssertEquals('Opus', 'Opus', AudioFormatToString(arfOpus));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StringToAudioFormat Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_StringToAudioFormat_MP3;
begin
  AssertEquals('MP3', Ord(arfMP3), Ord(StringToAudioFormat('mp3')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_OGG;
begin
  AssertEquals('OGG', Ord(arfOGG), Ord(StringToAudioFormat('ogg')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_FLAC;
begin
  AssertEquals('FLAC', Ord(arfFLAC), Ord(StringToAudioFormat('flac')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_WAV;
begin
  AssertEquals('WAV', Ord(arfWAV), Ord(StringToAudioFormat('wav')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_AAC;
begin
  AssertEquals('AAC', Ord(arfAAC), Ord(StringToAudioFormat('aac')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_Opus;
begin
  AssertEquals('Opus', Ord(arfOpus), Ord(StringToAudioFormat('opus')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_CaseInsensitive;
begin
  AssertEquals('FLAC uppercase', Ord(arfFLAC), Ord(StringToAudioFormat('FLAC')));
  AssertEquals('Ogg mixed', Ord(arfOGG), Ord(StringToAudioFormat('Ogg')));
end;

procedure TTestStreamRecorder.Test_StringToAudioFormat_Unknown_DefaultsMP3;
begin
  AssertEquals('Unknown defaults to MP3', Ord(arfMP3), Ord(StringToAudioFormat('xyz')));
  AssertEquals('Empty defaults to MP3', Ord(arfMP3), Ord(StringToAudioFormat('')));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  VideoFormatToString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_VideoFormatToString_MP4;
begin
  AssertEquals('MP4', 'MP4', VideoFormatToString(vrfMP4));
end;

procedure TTestStreamRecorder.Test_VideoFormatToString_MKV;
begin
  AssertEquals('MKV', 'MKV', VideoFormatToString(vrfMKV));
end;

procedure TTestStreamRecorder.Test_VideoFormatToString_WebM;
begin
  AssertEquals('WebM', 'WebM', VideoFormatToString(vrfWebM));
end;

procedure TTestStreamRecorder.Test_VideoFormatToString_TS;
begin
  AssertEquals('TS', 'TS', VideoFormatToString(vrfTS));
end;

procedure TTestStreamRecorder.Test_VideoFormatToString_AVI;
begin
  AssertEquals('AVI', 'AVI', VideoFormatToString(vrfAVI));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StringToVideoFormat Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_StringToVideoFormat_MP4;
begin
  AssertEquals('MP4', Ord(vrfMP4), Ord(StringToVideoFormat('mp4')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_MKV;
begin
  AssertEquals('MKV', Ord(vrfMKV), Ord(StringToVideoFormat('mkv')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_WebM;
begin
  AssertEquals('WebM', Ord(vrfWebM), Ord(StringToVideoFormat('webm')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_TS;
begin
  AssertEquals('TS', Ord(vrfTS), Ord(StringToVideoFormat('ts')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_AVI;
begin
  AssertEquals('AVI', Ord(vrfAVI), Ord(StringToVideoFormat('avi')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_CaseInsensitive;
begin
  AssertEquals('MKV uppercase', Ord(vrfMKV), Ord(StringToVideoFormat('MKV')));
  AssertEquals('WebM mixed', Ord(vrfWebM), Ord(StringToVideoFormat('WebM')));
end;

procedure TTestStreamRecorder.Test_StringToVideoFormat_Unknown_DefaultsMP4;
begin
  AssertEquals('Unknown defaults to MP4', Ord(vrfMP4), Ord(StringToVideoFormat('xyz')));
  AssertEquals('Empty defaults to MP4', Ord(vrfMP4), Ord(StringToVideoFormat('')));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  RecordingStateToString Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_RecordingStateToString_Idle;
begin
  AssertEquals('Idle', 'Idle', RecordingStateToString(rsIdle));
end;

procedure TTestStreamRecorder.Test_RecordingStateToString_Recording;
begin
  AssertEquals('Recording', 'Recording', RecordingStateToString(rsRecording));
end;

procedure TTestStreamRecorder.Test_RecordingStateToString_Paused;
begin
  AssertEquals('Paused', 'Paused', RecordingStateToString(rsPaused));
end;

procedure TTestStreamRecorder.Test_RecordingStateToString_Stopping;
begin
  AssertEquals('Stopping', 'Stopping', RecordingStateToString(rsStopping));
end;

procedure TTestStreamRecorder.Test_RecordingStateToString_Error;
begin
  AssertEquals('Error', 'Error', RecordingStateToString(rsError));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  FormatRecordingDuration Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_FormatRecordingDuration_MinutesOnly;
var
  T: TTime;
  Result: string;
begin
  T := EncodeTime(0, 5, 30, 0);
  Result := FormatRecordingDuration(T);
  { Format output may vary - check meaningful content }
  AssertTrue('Should contain 5', Pos('5', Result) > 0);
  AssertTrue('Should contain 30', Pos('30', Result) > 0);
  AssertTrue('Should contain colon', Pos(':', Result) > 0);
end;

procedure TTestStreamRecorder.Test_FormatRecordingDuration_WithHours;
var
  T: TTime;
begin
  T := EncodeTime(2, 15, 45, 0);
  AssertEquals('2:15:45', '2:15:45', FormatRecordingDuration(T));
end;

procedure TTestStreamRecorder.Test_FormatRecordingDuration_Zero;
var
  T: TTime;
  Result: string;
begin
  T := EncodeTime(0, 0, 0, 0);
  Result := FormatRecordingDuration(T);
  { Format output may vary - check meaningful content }
  AssertTrue('Should contain 0', Pos('0', Result) > 0);
  AssertTrue('Should contain colon', Pos(':', Result) > 0);
  AssertTrue('Should be short format', Length(Trim(Result)) <= 5);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  FormatRecordingSize Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_FormatRecordingSize_Bytes;
begin
  AssertEquals('500 B', '500 B', FormatRecordingSize(500));
end;

procedure TTestStreamRecorder.Test_FormatRecordingSize_KB;
var
  S: string;
begin
  S := FormatRecordingSize(2048);
  AssertTrue('Contains KB', Pos('KB', S) > 0);
  AssertTrue('Contains 2', Pos('2', S) > 0);
end;

procedure TTestStreamRecorder.Test_FormatRecordingSize_MB;
var
  S: string;
begin
  S := FormatRecordingSize(5 * 1024 * 1024);
  AssertTrue('Contains MB', Pos('MB', S) > 0);
  AssertTrue('Contains 5', Pos('5', S) > 0);
end;

procedure TTestStreamRecorder.Test_FormatRecordingSize_GB;
var
  S: string;
begin
  S := FormatRecordingSize(Int64(2) * 1024 * 1024 * 1024);
  AssertTrue('Contains GB', Pos('GB', S) > 0);
  AssertTrue('Contains 2', Pos('2', S) > 0);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Enum Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_RecordingState_EnumValues;
begin
  AssertEquals('rsIdle = 0', 0, Ord(rsIdle));
  AssertEquals('rsRecording = 1', 1, Ord(rsRecording));
  AssertEquals('rsPaused = 2', 2, Ord(rsPaused));
  AssertEquals('rsStopping = 3', 3, Ord(rsStopping));
  AssertEquals('rsError = 4', 4, Ord(rsError));
end;

procedure TTestStreamRecorder.Test_AudioRecordFormat_EnumValues;
begin
  AssertEquals('arfMP3 = 0', 0, Ord(arfMP3));
  AssertEquals('arfOGG = 1', 1, Ord(arfOGG));
  AssertEquals('arfFLAC = 2', 2, Ord(arfFLAC));
  AssertEquals('arfWAV = 3', 3, Ord(arfWAV));
  AssertEquals('arfAAC = 4', 4, Ord(arfAAC));
  AssertEquals('arfOpus = 5', 5, Ord(arfOpus));
end;

procedure TTestStreamRecorder.Test_VideoRecordFormat_EnumValues;
begin
  AssertEquals('vrfMP4 = 0', 0, Ord(vrfMP4));
  AssertEquals('vrfMKV = 1', 1, Ord(vrfMKV));
  AssertEquals('vrfWebM = 2', 2, Ord(vrfWebM));
  AssertEquals('vrfTS = 3', 3, Ord(vrfTS));
  AssertEquals('vrfAVI = 4', 4, Ord(vrfAVI));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  TStreamRecorder Initial State Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_Create_StateIsIdle;
begin
  AssertEquals('Initial state is Idle', Ord(rsIdle), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_Create_IsRecordingFalse;
begin
  AssertFalse('Not recording initially', FRecorder.IsRecording);
end;

procedure TTestStreamRecorder.Test_Create_EmptyFileName;
begin
  AssertEquals('Empty filename', '', FRecorder.Info.FileName);
end;

procedure TTestStreamRecorder.Test_Create_EmptyRecordingPath;
begin
  AssertEquals('Empty recording path', '', FRecorder.RecordingPath);
end;

procedure TTestStreamRecorder.Test_Create_ZeroFileSize;
begin
  AssertEquals('Zero file size', 0, FRecorder.Info.FileSize);
end;

procedure TTestStreamRecorder.Test_Create_ZeroTrackCount;
begin
  AssertEquals('Zero track count', 0, FRecorder.Info.TrackCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Default Settings Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_DefaultSettings_AudioFormatMP3;
begin
  AssertEquals('Default audio MP3', Ord(arfMP3), Ord(FRecorder.Settings.AudioFormat));
end;

procedure TTestStreamRecorder.Test_DefaultSettings_VideoFormatMP4;
begin
  AssertEquals('Default video MP4', Ord(vrfMP4), Ord(FRecorder.Settings.VideoFormat));
end;

procedure TTestStreamRecorder.Test_DefaultSettings_AppendTimestampTrue;
begin
  AssertTrue('AppendTimestamp true', FRecorder.Settings.AppendTimestamp);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_CreateSubfoldersTrue;
begin
  AssertTrue('CreateSubfolders true', FRecorder.Settings.CreateSubfolders);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_OverwriteExistingFalse;
begin
  AssertFalse('OverwriteExisting false', FRecorder.Settings.OverwriteExisting);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_AutoSplitFalse;
begin
  AssertFalse('AutoSplit false', FRecorder.Settings.AutoSplit);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_SplitSizeMB;
begin
  AssertEquals('SplitSizeMB is 100', 100, FRecorder.Settings.SplitSizeMB);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_SplitTimeMinutes;
begin
  AssertEquals('SplitTimeMinutes is 60', 60, FRecorder.Settings.SplitTimeMinutes);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_UseMetadataTrue;
begin
  AssertTrue('UseMetadataInFilename true', FRecorder.Settings.UseMetadataInFilename);
end;

procedure TTestStreamRecorder.Test_DefaultSettings_OutputFolderNotEmpty;
begin
  AssertTrue('OutputFolder not empty', FRecorder.Settings.OutputFolder <> '');
end;

procedure TTestStreamRecorder.Test_DefaultSettings_OutputFolderVideoNotEmpty;
begin
  AssertTrue('OutputFolderVideo not empty', FRecorder.Settings.OutputFolderVideo <> '');
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StartRecording Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_StartRecording_SetsStateRecording;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertEquals('State is Recording', Ord(rsRecording), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_StartRecording_SetsSourceURL;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertEquals('SourceURL set', 'http://test.com/stream', FRecorder.Info.SourceURL);
end;

procedure TTestStreamRecorder.Test_StartRecording_SetsSourceName;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertEquals('SourceName set', 'Test Radio', FRecorder.Info.SourceName);
end;

procedure TTestStreamRecorder.Test_StartRecording_SetsIsAudioOnly;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertTrue('IsAudioOnly true', FRecorder.Info.IsAudioOnly);

  FRecorder.StopRecording;
  FRecorder.StartRecording('http://test.com/video', 'Test Video', False);
  AssertFalse('IsAudioOnly false', FRecorder.Info.IsAudioOnly);
end;

procedure TTestStreamRecorder.Test_StartRecording_ReturnsTrue;
var
  Res: Boolean;
begin
  Res := FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertTrue('StartRecording returns true', Res);
end;

procedure TTestStreamRecorder.Test_StartRecording_SetsStartTime;
var
  Before, After: TDateTime;
begin
  Before := Now;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  After := Now;
  AssertTrue('StartTime >= Before', FRecorder.Info.StartTime >= Before);
  AssertTrue('StartTime <= After', FRecorder.Info.StartTime <= After);
end;

procedure TTestStreamRecorder.Test_StartRecording_WhenAlreadyRecording_ReturnsFalse;
var
  Res: Boolean;
begin
  FRecorder.StartRecording('http://test1.com/stream', 'Radio 1', True);
  Res := FRecorder.StartRecording('http://test2.com/stream', 'Radio 2', True);
  AssertFalse('Second start returns false', Res);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  StopRecording Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_StopRecording_SetsStateIdle;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.StopRecording;
  AssertEquals('State is Idle', Ord(rsIdle), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_StopRecording_ClearsRecordingPath;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.StopRecording;
  AssertEquals('RecordingPath cleared', '', FRecorder.RecordingPath);
end;

procedure TTestStreamRecorder.Test_StopRecording_WhenNotRecording_NoEffect;
begin
  FRecorder.StopRecording;
  AssertEquals('State still Idle', Ord(rsIdle), Ord(FRecorder.Info.State));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  PauseRecording / ResumeRecording Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_PauseRecording_SetsStatePaused;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.PauseRecording;
  AssertEquals('State is Paused', Ord(rsPaused), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_ResumeRecording_SetsStateRecording;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.PauseRecording;
  FRecorder.ResumeRecording;
  AssertEquals('State is Recording', Ord(rsRecording), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_PauseRecording_WhenNotRecording_NoEffect;
begin
  FRecorder.PauseRecording;
  AssertEquals('State still Idle', Ord(rsIdle), Ord(FRecorder.Info.State));
end;

{ ─────────────────────────────────────────────────────────────────────────────
  GetMPVRecordPath Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_GetMPVRecordPath_WhenRecording;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertTrue('Path not empty when recording', FRecorder.GetMPVRecordPath <> '');
end;

procedure TTestStreamRecorder.Test_GetMPVRecordPath_WhenNotRecording;
begin
  AssertEquals('Path empty when not recording', '', FRecorder.GetMPVRecordPath);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  NotifyTrackChange Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_NotifyTrackChange_IncrementsTrackCount;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Artist', 'Title');
  AssertEquals('TrackCount is 1', 1, FRecorder.Info.TrackCount);
  FRecorder.NotifyTrackChange('Artist2', 'Title2');
  AssertEquals('TrackCount is 2', 2, FRecorder.Info.TrackCount);
end;

procedure TTestStreamRecorder.Test_NotifyTrackChange_SetsCurrentTitle;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('The Beatles', 'Yesterday');
  AssertEquals('CurrentTitle set', 'The Beatles - Yesterday', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamRecorder.Test_NotifyTrackChange_FormatsArtistTitle;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Queen', 'Bohemian Rhapsody');
  AssertEquals('Format Artist - Title', 'Queen - Bohemian Rhapsody', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamRecorder.Test_NotifyTrackChange_TitleOnlyIfNoArtist;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('', 'Unknown Track');
  AssertEquals('Title only', 'Unknown Track', FRecorder.Info.CurrentTitle);
end;

procedure TTestStreamRecorder.Test_NotifyTrackChange_NoDuplicates;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Artist', 'Title');
  FRecorder.NotifyTrackChange('Artist', 'Title');
  FRecorder.NotifyTrackChange('Artist', 'Title');
  AssertEquals('No duplicate count', 1, FRecorder.Info.TrackCount);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  ShouldSplitFile Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_ShouldSplitFile_FalseWhenAutoSplitDisabled;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.AutoSplit := False;
  FRecorder.Settings := Settings;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertFalse('No split when disabled', FRecorder.ShouldSplitFile);
end;

procedure TTestStreamRecorder.Test_ShouldSplitFile_FalseWhenBelowLimits;
var
  Settings: TRecordingSettings;
begin
  Settings := FRecorder.Settings;
  Settings.AutoSplit := True;
  Settings.SplitSizeMB := 1000;
  Settings.SplitTimeMinutes := 120;
  FRecorder.Settings := Settings;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertFalse('No split below limits', FRecorder.ShouldSplitFile);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Events Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_OnStateChange_TriggeredOnStart;
begin
  FRecorder.OnStateChange := @OnStateChangeHandler;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertTrue('OnStateChange triggered', FStateChangeCount > 0);
end;

procedure TTestStreamRecorder.Test_OnStateChange_TriggeredOnStop;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FStateChangeCount := 0;
  FRecorder.OnStateChange := @OnStateChangeHandler;
  FRecorder.StopRecording;
  AssertTrue('OnStateChange on stop', FStateChangeCount > 0);
end;

procedure TTestStreamRecorder.Test_OnStateChange_ReceivesCorrectStates;
begin
  FRecorder.OnStateChange := @OnStateChangeHandler;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  AssertEquals('OldState is Idle', Ord(rsIdle), Ord(FLastOldState));
  AssertEquals('NewState is Recording', Ord(rsRecording), Ord(FLastNewState));
end;

procedure TTestStreamRecorder.Test_OnTrackChange_Triggered;
begin
  FRecorder.OnTrackChange := @OnTrackChangeHandler;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Artist', 'Title');
  AssertEquals('OnTrackChange triggered', 1, FTrackChangeCount);
end;

procedure TTestStreamRecorder.Test_OnTrackChange_ReceivesArtistTitle;
begin
  FRecorder.OnTrackChange := @OnTrackChangeHandler;
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Test Artist', 'Test Title');
  AssertEquals('Artist received', 'Test Artist', FLastArtist);
  AssertEquals('Title received', 'Test Title', FLastTitle);
end;

{ ─────────────────────────────────────────────────────────────────────────────
  Initialize Tests
  ───────────────────────────────────────────────────────────────────────────── }

procedure TTestStreamRecorder.Test_Initialize_ResetsState;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.Initialize;
  AssertEquals('State reset to Idle', Ord(rsIdle), Ord(FRecorder.Info.State));
end;

procedure TTestStreamRecorder.Test_Initialize_ResetsInfo;
begin
  FRecorder.StartRecording('http://test.com/stream', 'Test Radio', True);
  FRecorder.NotifyTrackChange('Artist', 'Title');
  FRecorder.Initialize;
  AssertEquals('FileName reset', '', FRecorder.Info.FileName);
  AssertEquals('TrackCount reset', 0, FRecorder.Info.TrackCount);
  AssertEquals('SourceURL reset', '', FRecorder.Info.SourceURL);
end;

initialization
  RegisterTest(TTestStreamRecorder);

end.
